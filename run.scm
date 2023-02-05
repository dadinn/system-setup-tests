#!/usr/bin/env sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((srfi srfi-1) #:prefix srfi1:)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 readline) #:select (readline))
 ((ice-9 format) #:select (format))
 ((ice-9 ftw) #:select (scandir))
 ((ice-9 threads) #:select
  (par-for-each call-with-new-thread thread-exited?))
 ((ice-9 exceptions))
 ((ice-9 expect)
  #:select
  ((expect . expect-old)
   expect-strings expect-regexec
   expect-strings-compile-flags
   expect-timeout expect-timeout-proc
   expect-select expect-eof-proc)))

(define-macro (expect . clauses)
  (let ((binding (gensym "matcher"))
        (matcher (caar clauses))
        (body (cdar clauses)))
    `(let ((,binding ,matcher))
       (expect-old ,(cons binding body)))))

(define-macro (comment . args)
  `(if #f #f))

(define-macro (interact port)
  "Scheme procedure: interact PORT

Gives interactive control to the user by reading lines of commands and forwarding them
to the VM process. The serial output of the VM process are then printed to the current
standard output. PORT argument has to be an input-output pipe to communicate with the
current process.

To exit the interactive mode enter \"continue!\" as command."
  `(let ((interaction
          (call-with-new-thread
           (lambda ()
             (let loop ((line (readline)))
               (cond
                ((equal? line "continue!")
                 (newline ,port))
                (else
                 (display line ,port)
                 (newline ,port)
                 (loop (readline)))))))))
     (let loop ()
       (expect-old
        ((const #t)
         (cond
          ((thread-exited? interaction)
           (format #t "\nCONTINUING...\n"))
          (else (loop))))))))

(define-syntax prompt
  (lambda (stx)
    (syntax-case stx ()
      ((_ message invalid-message (pattern body ...) ...)
       #'(let loop ((resp (readline message)))
           (cond
            ((regex:string-match pattern resp)
             body ...)
            ...
            (else (loop (readline invalid-message)))))))))

(define (printable-char? c)
  (or (eqv? c #\newline) (not (eqv? 'Cc (char-general-category c)))))

(define (fetch-pid pipe)
  ((@@ (ice-9 popen) pipe-info-pid)
   ((@@ (ice-9 popen) fetch-pipe-info) pipe)))

(define* (run-qemu name
          #:key memory network? uefi?
          ovmf-code-file ovmf-vars-file
          sources-path mirrors-path
          cdrom-path drives-path
          drive-specs)
  (when (not (utils:directory? drives-path))
    (utils:mkdir-p drives-path))
  (for-each
   (lambda (spec)
     (let* ((filename (assoc-ref spec "name"))
            (filename (string-append filename ".img"))
            (path (utils:path drives-path filename))
            (size (assoc-ref spec "size")))
       (when (not (file-exists? path))
         (system* "qemu-img" "create" "-f" "qcow2" path size))))
   (or drive-specs '()))
  (cond
   ((and uefi? ovmf-vars-file (file-exists? ovmf-vars-file))
    (let ((target-file (utils:path drives-path "OVMF_VARS.fd")))
      (unless (file-exists? target-file)
        (copy-file ovmf-vars-file target-file))))
   ((and uefi? ovmf-vars-file)
    (error "OVMF_VARS file doesn't exist!" ovmf-vars-file))
   ((and uefi? (not ovmf-vars-file))
    (error "OVMF_VARS file must be specified for UEFI support!"))
   (uefi? (error "OVMF_VARS file doesn't exist!" ovmf-vars-file)))
  (let ((port
         (apply popen:open-pipe* OPEN_BOTH
          `("qemu-system-x86_64"
            "-name" ,name
            "-enable-kvm"
            "-nographic"
            ,@(if (not network?)  (list "-nic" "none") '())
            "-m" ,(or memory "4096")
            ,@(if cdrom-path ; boot from CD-ROM the first time
               (list "-boot" "once=d" "-cdrom" cdrom-path)
               (list "-boot" "order=c"))
            ,@(srfi1:append-map
               (lambda (spec)
                 (list
                  "-drive"
                  (utils:emit-arg-alist
                   `(("file" . ,(utils:path drives-path (string-append (assoc-ref spec "name") ".img")))
                     ("format" . "qcow2")
                     ("if" . "none")
                     ("id" . ,(assoc-ref spec "name"))))
                  "-device"
                  (utils:emit-arg-alist
                   `("virtio-blk-pci"
                     ("drive" . ,(assoc-ref spec "name"))
                     ("serial" . ,(assoc-ref spec "name"))))))
               drive-specs)
            ,@(if uefi?
               (list
                "-drive"
                (utils:emit-arg-alist
                 `("readonly"
                   ("if" . "pflash")
                   ("format" . "raw")
                   ("file" . ,ovmf-code-file)))
                "-drive"
                (utils:emit-arg-alist
                 `(("if" . "pflash")
                   ("format" . "raw")
                   ("file" . ,(utils:path drives-path "OVMF_VARS.fd")))))
               '())
            ,@(if sources-path
               (list
                "-virtfs"
                (utils:emit-arg-alist
                 `("local" "readonly"
                   ("path" . ,sources-path)
                   ("mount_tag" . "sources")
                   ("security_model" . "mapped"))))
               '())
            ,@(if mirrors-path
               (list
                "-virtfs"
                (utils:emit-arg-alist
                 `("local"
                   ,@(if (not network?) '("readonly") '())
                   ("path" . ,mirrors-path)
                   ("mount_tag" . "mirrors")
                   ("security_model" . "mapped"))))
               '())))))
    (setvbuf port 'none)
    (set-port-encoding! port "UTF-8")
    (set-port-conversion-strategy! port 'substitute)
    port))

(define (options-spec project-path)
  `((sync-mirror
     (value #t)
     (value-arg "type")
     (single-char #\M)
     (description "Synchronise local apt-mirror via internet, and then exit."))
    (use-network
     (single-char #\N)
     (description "Use network for all package dependencies, and disable local apt-mirror."))
    (data-path
     (single-char #\d)
     (description
      "Path to store temporary mirrors, isos, etc.")
     (value #t)
     (value-arg "path")
     (default "/var/tmp/system-setup"))
    (temp-path
     (single-char #\t)
     (description
      "Path to store temporary test files, including drives, logs, etc.")
     (value #t)
     (value-arg "path")
     (default "/tmp/system-setup"))
    (specs-path
     (single-char #\s)
     (description
      ,(format #f "Path to test specifications."))
     (value #t)
     (value-arg "path")
     (predicate utils:directory?)
     (default ,(utils:path project-path "tests" "resources" "specs")))
    (ovmf-code-file
     (description "Path to OVMF code file")
     (value #t)
     (value-arg "path")
     (default "/usr/share/OVMF/OVMF_CODE.fd"))
    (ovmf-vars-file
     (description "Path to OVMF vars file")
     (value #t)
     (value-arg "path")
     (default "/usr/share/OVMF/OVMF_VARS.fd"))
    (parallel
     (single-char #\P)
     (description "Execute multiple tests in parallel in separate VMs. Only prints output into log files."))
    (verify
     (single-char #\V)
     (description "Run verification process only on existing test results for specific run (by RUNID timestamp).")
     (value #t)
     (value-arg "RUNID"))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define (init-matcher log-path default-pattern)
 (when (not (file-exists? log-path))
  (utils:mkdir-p log-path))
 (lambda* (id #:rest args)
   (let* ((pattern
           (apply format #f
            (if (null? args) default-pattern (car args))
            (if (null? args) '() (cdr args))))
          (rx (make-regexp pattern))
          (log-file (utils:path log-path (string-append id ".log")))
          (log-port (open-output-file log-file)))
     (format log-port "EXPECTING: ~A\nMATCHING AGAINST:\n" pattern)
     (lambda (s eof?)
       (cond
        ((not eof?)
         (let* ((content-length (string-length s))
                (current-char (string-ref s (- content-length 1))))
           (when (< 0 content-length)
             (display current-char log-port))
           (cond
            ((regexp-exec rx s)
             (newline log-port)
             (display "MATCHED!!!" log-port)
             (close log-port)
             #t)
            (else #f))))
        (else
         (close log-port)
         (error "matcher encountered EOF!")))))))

(define (call-init-zpool port spec)
  (let ((args (utils:assoc-get spec "zpool"))
        (rootdev (utils:assoc-get spec "instroot" "rootdev")))
    (cond
     ((and args rootdev)
      (format port
       "/mnt/sources/init-instroot/init-instroot.scm -A --without-zfs-native-encryption -Z ~A\n"
       (string-join args " ")))
     (args
      (format port
       "/mnt/sources/init-instroot/init-instroot.scm -A -p ~A -Z ~A\n"
       (or (utils:assoc-get spec "instroot" "passphrase") "IGNORE_THIS")
       (string-join args " ")))
     (else
      (newline port)))))

(define (call-init-instroot port spec)
  (format port
   "/mnt/sources/init-instroot/init-instroot.scm -A ~A\n"
   (srfi1:fold
    (lambda (entry acc)
      (let ((key (car entry))
            (val (cdr entry)))
        (string-append
         acc
         (cond
          ((string=? "passphrase" key)
           (string-append " -p " (or val "IGNORED")))
          ((string=? "rootdev" key)
           (string-append " -r " val))
          ((string=? "luks-label" key)
           (string-append " -l " val))
          ((string=? "uefi" key) " -E")
          ((string=? "bootdev" key)
           (string-append " -b " val))
          ((string=? "zpool" key)
           (string-append " -z " val))
          ((string=? "swapsize" key)
           (string-append " -s " val))
          ((string=? "swapfiles" key)
           (string-append " -S " val))
          ((string=? "luks-devs" key)
           (string-append " -v "
            (utils:emit-arg-alist val
             #:pair-separator #\:
             #:list-separator #\,)))
          ((string=? "luksv2" key)
           (if val " -L" ""))
          ((string=? "uefi" key)
           (if val " -E" ""))
          (else "")))))
    "" (assoc-ref spec "instroot"))))

(define* (call-debian-setup port spec #:optional use-network?)
  (format port
   "/mnt/sources/debian-setup/install.scm -AS ~A ~A\n"
   (srfi1:fold
    (lambda (entry acc)
      (let ((key (car entry))
            (val (cdr entry)))
        (string-append acc
         (cond
          ((string=? "hostname" key)
           (string-append " -n " val))
          ((string=? "sudouser" key)
           (string-append " -u " val))
          ((string=? "password" key)
           (string-append " -p " val))
          ((string=? "release" key)
           (string-append " -r " val))
          (else "")))))
    "" (assoc-ref spec "install"))
   (if (not use-network?)
    "-m http://localhost:8080/debian --no-check-gpg"
    "")))

(define guest-guile-package
  '(("debian" .
      (("bullseye" . "guile-3.0")
       ("buster" . "guile-2.2")
       ("stretch" . "guile-2.0")))))

(define os-mirror-type
  '(("debian" . "apt")))

(define mirror-specs
  '(("apt" .
     (("os" . "debian")
      ("release" . "bullseye")
      ("memory" . "1024")
      ("username" . "user")
      ("password" . "live")))))

(define live-iso-specs
  '(("debian" .
     (("stretch" .
       (("http"
         (("filename" . "debian-live-9.2.0-amd64-gnome.iso")))))
      ("buster" .
       (("torrent" .
         (("filename" . "debian-live-10.3.0-amd64-standard.iso")
          ("url" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")))))
      ("bullseye" .
       (("http" .
         (("filename" . "debian-live-11.6.0-amd64-standard.iso")
          ("url" . "https://cdimage.debian.org/debian-cd/current-live/amd64/iso-hybrid/debian-live-11.6.0-amd64-standard.iso")))
        ("torrent" .
         (("filename" . "debian-live-11.6.0-amd64-standard.iso")
          ("url" . "magnet:?xt=urn:btih:4365ce1cbb930a7c018e70073fdb2877bd4da852&dn=debian-live-11.6.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")))))))
    ("archlinux" .
     (("2020.01.01" .
       (("http" .
         (("filename" ."archlinux-2020.01.01-x86_64.iso")
          ("url" ."https://archive.archlinux.org/iso/2020.01.01/archlinux-2020.01.01-x86_64.iso")))))))))

(define (download-http iso-dir filename url)
  (cond
   ((not (utils:which* "curl"))
    (format #t "\nCould not find executable curl on PATH to download Live ISO image:\n~A\n\n" url)
    #f)
   (else
    (format #t "\nDownloading Live ISO image over HTTP...\n\n")
    (guard (ex (else (format #t "\n\nFailed to download Live ISO image over HTTP:\n~A\n\n" url) #f))
      (cond
       ((zero? (system* "curl" "--fail" "--location" "--output" (utils:path iso-dir filename) url))
        (format #t "\nDownloaded Live ISO image: ~A\n" (utils:path iso-dir filename))
        (utils:path iso-dir filename))
       (else (error "Failed to Download Live ISO image!")))))))

(define (download-torrent iso-dir filename url)
  (cond
   ((not (utils:which* "transmission-cli"))
    (format #t "\nCould not find transmission-cli executable on PATH to download Live ISO image:\n~A\n\n" url)
    #f)
   (else
    (format #t "\nDownloading Live ISO image using BitTorrent Magnet link...\n\n")
    (guard (ex (else (format #t "\n\nFailed to download Live ISO image over BitTorrent:\n~A\n\n" url) #f))
      (let ((expect-port (popen:open-pipe* OPEN_BOTH "transmission-cli" "--download-dir" iso-dir url))
            (expect-char-proc display))
        (set-port-conversion-strategy! expect-port 'substitute)
        (set-port-encoding! expect-port "UTF-8")
        (setvbuf expect-port 'none)
        (dynamic-wind
          (const #t)
          (lambda ()
            (expect-strings
             ("Seeding, "
              (sleep 3)
              (format #t "\nDownloaded Live ISO image: ~A\n" (utils:path iso-dir filename))
              (utils:path iso-dir filename))))
          (lambda ()
            (kill (fetch-pid expect-port) SIGTERM)
            (popen:close-pipe expect-port))))))))

(define (find-iso-path download? iso-dir spec)
  (srfi1:any
   (lambda (pair)
     (let ((type (car pair))
           (args (cdr pair)))
       (cond
        ((equal? type "http")
         (let* ((filename (assoc-ref args "filename"))
                (iso-path (utils:path iso-dir filename))
                (url (assoc-ref args "url")))
           (if (not download?)
               (and (file-exists? iso-path) iso-path)
               (prompt
                "\nWould you like to download Live ISO image via HTTP request? [Y/n]:\n"
                "\nInvalid response! Please try again [Y/n]:\n"
                ("^[yY]?$" (download-http iso-dir filename url))
                ("^[nN]$" #f)))))
        ((equal? type "torrent")
         (let* ((filename (assoc-ref args "filename"))
                (iso-path (utils:path iso-dir filename))
                (url (assoc-ref args "url")))
           (if (not download?)
               (and (file-exists? iso-path) iso-path)
               (prompt
                "\nWould you like to download Live ISO image via BitTorrent? [Y/n]:\n"
                "\nInvalid response! Please try again [Y/n]:\n"
                ("^[yY]?$" (download-torrent iso-dir filename url))
                ("^[nN]$" #f)))))
        (else #f))))
   spec))

(define* (resolve-iso-path data-path os release)
  (let* ((iso-dir (utils:path data-path "isos"))
         (spec
          (utils:assoc-get
           live-iso-specs
           os release))
         (iso-path (find-iso-path #f iso-dir spec)))
    (if iso-path iso-path
        (find-iso-path #t iso-dir spec))))

(define (open-log-port logs-path filename)
 (when (not (utils:directory? logs-path))
  (utils:mkdir-p logs-path))
 (let ((log-port (open-output-file (utils:path logs-path filename))))
  (setvbuf log-port 'none)
  log-port))

(define* (run-test
          name spec run-id temp-path data-path sources-path
          #:key ovmf-code-file ovmf-vars-file use-network? parallel? verify-only?)
  (let* ((use-network? (or use-network? (utils:assoc-get spec "guest" "network")))
         (memory (utils:assoc-get spec "guest" "memory"))
         (uefi? (utils:assoc-get spec "guest" "uefi"))
         (run-path (utils:path temp-path run-id))
         (mirror-path
          (utils:path
           data-path "mirrors"
           (assoc-ref
            os-mirror-type
            (utils:assoc-get spec "guest" "os"))))
         (os (utils:assoc-get spec "guest" "os"))
         (release (utils:assoc-get spec "guest" "release"))
         (cdrom-path (resolve-iso-path data-path os release))
         (test-path (utils:path run-path name))
         (logs-path (utils:path test-path "logs"))
         (log-port (open-log-port logs-path "output.log"))
         (expect-char-proc
          (if (not parallel?)
           (lambda (c)
             (display c log-port)
             (display c))
           (lambda (c) (display c log-port))))
         (matcher (init-matcher (utils:path logs-path "expect") ":~~# "))
         (drives-path (utils:path test-path "drives"))
         (drive-specs (utils:assoc-get spec "guest" "drives"))
         (live-username (utils:assoc-get spec "guest" "username"))
         (live-password (utils:assoc-get spec "guest" "password")))
    (dynamic-wind
      (const #t)
      (lambda ()
    ;; START RUN
    (when (not verify-only?)
      (when (not (or use-network? (utils:directory? mirror-path)))
        (error "Not using network, yet local mirror directory doesn't exist!.
Either run with networking enabled, or synchronise apt-mirror first!"))
      (unless cdrom-path
        (error "Could not find Live ISO image!"))
    (let* ((expect-port
            (run-qemu name
             #:memory memory
             #:network? use-network? #:uefi? uefi?
             #:ovmf-code-file ovmf-code-file
             #:ovmf-vars-file ovmf-vars-file
             #:cdrom-path cdrom-path
             #:sources-path sources-path
             #:mirrors-path mirror-path
             #:drives-path drives-path
             #:drive-specs drive-specs)))
      (dynamic-wind
        (const #t)
        (lambda ()
          (if uefi?
           (expect
            ((matcher "step01" "Welcome to GRUB!")
             (sleep 1)
             (format expect-port "e")
             (format expect-port "\x1b[B") ;; down key
             (format expect-port "\x1b[B") ;; down key
             (format expect-port "\x1b[F") ;; END key
             (format expect-port " console=ttyS0")
             (format expect-port "\x18"))) ;; Ctrl-X key
           (expect
            ((matcher "step01" "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
             (sleep 1)
             (format expect-port "\t")
             (sleep 1)
             (format expect-port " console=ttyS0\n"))))
          (expect
           ((matcher "step02" "debian login:")
            (format expect-port "~A\n" live-username)))
          (expect
           ((matcher "step03" "Password:")
            (format expect-port "~A\n" live-password)))
          (expect
           ((matcher "step04" "\\$ ")
            (format expect-port "sudo -i\n")))
          (expect
           ((matcher "step05")
            (format expect-port "export LC_ALL=C\n")))
          (expect
           ((matcher "step06")
            (format expect-port "mkdir /mnt/sources\n")))
          (expect
           ((matcher "step07")
            (format expect-port
             "mount -t 9p -o ~A sources /mnt/sources\n"
             (utils:emit-arg-alist
              '(("trans" . "virtio")
                ("msize" . "104857600")
                "ro")))))
            (when (not use-network?)
             (expect
              ((matcher "step08")
               (format expect-port "mkdir -p /var/spool/apt-mirror\n")))
             (expect
              ((matcher "step09")
               (format expect-port
                "mount -t 9p -o ~A mirrors /var/spool/apt-mirror\n"
                (utils:emit-arg-alist
                 '(("trans" . "virtio")
                   ("msize" . "104857600")
                   "ro")))))
             (expect
              ((matcher "step10")
               (format expect-port "if [ -e /etc/apt/sources.list.d/base.list ]; then echo updating /etc/apt/sources.list; mv /etc/apt/sources.list.d/base.list /etc/apt/sources.list; fi\n")))
             (expect
              ((matcher "step11")
               (format expect-port "sed -i -E 's;^deb ([^ ]+) ([^ ]+) main.*$;deb file:///var/spool/apt-mirror/mirror/deb.debian.org/debian/ \\2 main;g' /etc/apt/sources.list\n"))))
            (expect
             ((matcher "step12")
              (format expect-port "apt update\n")))
            (expect
             ((matcher "step13")
              (format expect-port "apt install -y ~A"
               (utils:assoc-get guest-guile-package
                (utils:assoc-get spec "guest" "os")
                (utils:assoc-get spec "guest" "release")))
              (newline expect-port)))
            (expect
             ((matcher "step14")
              (call-init-zpool expect-port spec)))
            (expect
             ((matcher "step15")
              (call-init-instroot expect-port spec)))
            (when (not use-network?)
              (expect
               ((matcher "step16")
                (format expect-port "apt install -y nginx\n")))
              (expect
               ((matcher "step17")
                (format expect-port "cp /mnt/sources/tests/resources/nginx/apt-mirror.conf /etc/nginx/conf.d/\n")))
              (expect
               ((matcher "step18")
                (format expect-port "systemctl restart nginx\n")
                (sleep 10))))
            (expect
             ((matcher "step19")
              (call-debian-setup expect-port spec use-network?)))
            (expect
             ((matcher "step20" "FINISHED INSTALLING NEW DEBIAN SYSTEM!")
              (format expect-port "systemctl poweroff\n")
              (sleep 5))))
        (lambda ()
          (kill (fetch-pid expect-port) SIGTERM)
          (popen:close-pipe expect-port)
          (format #t "\nTerminated QEMU process for test ~A, run ~A!\n" name run-id)))))
    ;; VERIFY RUN
      (format #t "\nVerifying results for ~A\n" name)
      (let* ((expect-port
              (run-qemu name
               #:memory memory
               #:network? #f #:uefi? uefi?
               #:ovmf-code-file ovmf-code-file
               #:ovmf-vars-file ovmf-vars-file
               #:drives-path drives-path
               #:drive-specs drive-specs))
             (passphrase (utils:assoc-get spec "instroot" "passphrase"))
             (hostname (utils:assoc-get spec "install" "hostname"))
             (sudouser (utils:assoc-get spec "install" "sudouser"))
             (password (utils:assoc-get spec "install" "password"))
             (luks-label (utils:assoc-get spec "instroot" "luks-label"))
             (rootdev (utils:assoc-get spec "instroot" "rootdev"))
             (rootdev (and rootdev (caddr (string-split rootdev #\/))))
             (bootdev (utils:assoc-get spec "instroot" "bootdev"))
             (zpool (utils:assoc-get spec "instroot" "zpool")))
        (dynamic-wind
          (const #t)
          (lambda ()
            (expect
             ((matcher "verify01" "The highlighted entry will be executed automatically in [0-9]+s.")
              (newline expect-port)))
            (cond
             (rootdev
              (expect
               ((matcher "verify02" "Please unlock disk ~A:"
                 (or luks-label (string-append rootdev (if bootdev "1" "3") "_crypt")))
                (format expect-port "~A\n" passphrase))))
             ((and zpool passphrase)
              (expect
               ((matcher "verify02" "Enter passphrase for '~A':" zpool)
                (format expect-port "~A\n" passphrase)))))
            (expect
             ((matcher "verify03" "login: ")
              (format expect-port "~A\n" sudouser)))
            (expect
             ((matcher "verify04" "Password: ")
              (format expect-port "~A\n" password)))
            (expect
             ((matcher "verify05" "~A@~A:~~\\$ " sudouser hostname)
              (format expect-port "sudo -i\n")))
            (expect
             ((matcher "verify06" "\\[sudo\\] password for ~A: " sudouser)
              (format expect-port "~A\n" password)))
            (expect
             ((matcher "verify07" "root@~A:~~# " hostname)
              (format expect-port "export LC_ALL=C\n")))
            (expect
             ((matcher "verify08" "root@~A:~~# " hostname)
              (format expect-port "lsblk -f\n")
              (newline expect-port)))
            (expect
             ((matcher "verify08" "root@~A:~~# " hostname)
              (format expect-port "swapon --output-all\n")
              (newline expect-port)))
            (when zpool
              (expect
               ((matcher "verify09" "root@~A:~~# " hostname)
                (format expect-port "zpool status -P\n")))
              (expect
               ((matcher "verify10" "root@~A:~~# " hostname)
                (format expect-port "zfs list -t all\n"))))
            (expect
               ((matcher "verify11" "root@~A:~~# " hostname)
                (format expect-port "uname -r\n")))
            (expect
             ((matcher "verify12" "root@~A:~~# " hostname)
              (format expect-port "systemctl poweroff\n")))
            (expect
             ((matcher "verify13" "reboot: Power down")
              (sleep 5))))
          (lambda ()
            (kill (fetch-pid expect-port) SIGTERM)
            (popen:close-pipe expect-port)
            (format #t "\nTerminated QEMU process verifying test ~A, run ~A!\n" name run-id)))))
      (lambda ()
        (close-port log-port)))))

(define* (run-mirror-sync guest-spec run-id sources-path temp-path data-path)
  (let* ((name "mirror-sync")
         (os (utils:assoc-get guest-spec "os"))
         (release (utils:assoc-get guest-spec "release"))
         (memory (utils:assoc-get guest-spec "memory"))
         (run-path (utils:path temp-path run-id))
         (mirror-path
          (utils:path
           data-path "mirrors"
           (assoc-ref
            os-mirror-type os)))
         (cdrom-path (resolve-iso-path data-path os release))
         (test-path (utils:path run-path name))
         (logs-path (utils:path test-path "logs"))
         (log-port (open-log-port logs-path "output.log"))
         (expect-port
          (run-qemu name
           #:memory memory
           #:network? #t
           #:cdrom-path cdrom-path
           #:mirrors-path mirror-path
           #:sources-path sources-path))
         (expect-char-proc
          (lambda (c)
            (display c log-port)
            (display c)))
         (matcher (init-matcher (utils:path logs-path "expect") ":~~# "))
         (live-username (utils:assoc-get guest-spec "username"))
         (live-password (utils:assoc-get guest-spec "password")))
    (when (not (utils:directory? mirror-path))
      (utils:mkdir-p mirror-path))
    (dynamic-wind
      (const #t)
      (lambda ()
              (expect
               ((matcher "step01" "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
                (sleep 1)
                (format expect-port "\t")
                (sleep 1)
                (format expect-port " console=ttyS0\n")))
              (expect
               ((matcher "step02" "debian login:")
                (format expect-port "~A\n" live-username)))
              (expect
               ((matcher "step03" "Password:")
                (format expect-port "~A\n" live-password)))
              (expect
               ((matcher "step04" "\\$ ")
                (format expect-port "sudo -i\n")))
              (expect
               ((matcher "step05")
                (format expect-port "export LC_ALL=C\n")))
              (expect
               ((matcher "step06")
                (format expect-port "mkdir /mnt/sources\n")))
              (expect
               ((matcher "step07")
                (format expect-port
                 "mount -t 9p -o ~A sources /mnt/sources\n"
                 (utils:emit-arg-alist
                  '(("trans" . "virtio")
                    ("msize" . "104857600")
                    "ro")))))
              (expect
               ((matcher "step08")
                (format expect-port "mkdir -p /var/spool/apt-mirror\n")))
              (expect
               ((matcher "step09")
                (format expect-port
                 "mount -t 9p -o ~A mirrors /var/spool/apt-mirror\n"
                 (utils:emit-arg-alist
                  '(("trans" . "virtio")
                    ("msize" . "104857600"))))))
              (expect
               ((matcher "step10")
                (format expect-port "apt update\n")))
              (expect
               ((matcher "step11")
                (format expect-port "apt install -y apt-mirror\n")))
              (expect
               ((matcher "step12")
                (format expect-port "cp /mnt/sources/tests/resources/apt/mirror.list /etc/apt/\n")))
              (expect
               ((matcher "step13")
                (format expect-port "apt-mirror\n")))
              (expect
               ((matcher "step14")
                (format #t "\nFinished synchronising apt-mirror!\n"))))
      (lambda ()
        (kill (fetch-pid expect-port) SIGTERM)
        (popen:close-pipe expect-port)
        (close-port log-port)
        (format #t "\nTerminated QEMU process test ~A, run ~A!\n" name run-id)
        (format #t "Updating read permissions for ~A..." mirror-path)
        (system (format #f "find ~A -exec chmod a+r" mirror-path))
        (format #t "Updated read permissions for ~A!" mirror-path)))))

(define (find-test-names specs-path)
  (let ((pattern "([a-zA-Z_-]+).scm$"))
    (map
     (lambda (f)
       (regex:match:substring
        (regex:string-match pattern f) 1))
     (scandir specs-path
      (lambda (f)
        (and (not (utils:directory? (utils:path specs-path f)))
         (regex:string-match pattern f)))))))

(define (load-test-specs specs-path test-name)
  (let* ((dir-path (utils:path specs-path test-name))
         (file-path (string-append dir-path ".scm")))
    (cond
     ((utils:directory? dir-path)
      (srfi1:append-map
       (lambda (t) (load-test-specs specs-path t))
       (find-test-names dir-path)))
     ((file-exists? file-path)
      (let* ((port (open-input-file file-path))
             (spec (read port)))
        (close port)
        (list (cons test-name spec))))
     (else (list (cons test-name #f))))))

(define (main args)
  (let* ((project-path (dirname (dirname (current-filename))))
         (options (utils:getopt-extra args (options-spec project-path)))
         (start-time (current-time))
         (verify-run-id (hash-ref options 'verify))
         (run-id
          (or verify-run-id
           (strftime "%Y%m%d_%H%M%S"
             (localtime start-time))))
         (data-path (hash-ref options 'data-path))
         (temp-path (hash-ref options 'temp-path))
         (specs-path (hash-ref options 'specs-path))
         (test-spec-names (find-test-names specs-path))
         (alias-names
          (scandir specs-path
           (lambda (f) (utils:directory? (utils:path specs-path f)))))
         (alias-names (cddr alias-names))
         (mirror-type (hash-ref options 'sync-mirror))
         (use-network? (hash-ref options 'use-network))
         (ovmf-code-file (hash-ref options 'ovmf-code-file))
         (ovmf-vars-file (hash-ref options 'ovmf-vars-file))
         (test-names (hash-ref options '()))
         (parallel? (hash-ref options 'parallel))
         (help? (hash-ref options 'help)))
    (unless (utils:directory? data-path)
      (utils:mkdir-p data-path))
    (unless (utils:directory? temp-path)
      (utils:mkdir-p temp-path))
    (cond
     ((or help? (and (not mirror-type) (null? test-names)))
      (format
       #t "
Start up Qemu/KVM machine to run test specs based on specified IDs.

USAGE:

~A [OPTION...] [ID...]

Valid OPTION value are:

~A

"
       (basename (car args))
       (utils:usage (options-spec project-path)))
      (if (null? test-spec-names)
          (display "No test specificications have been defined yet!\n\n")
          (format
           #t "The following test specification names are defined:\n\n~A\n\n"
           (string-join test-spec-names ",\n")))
      (if (null? alias-names)
          (display "No test aliases have been defined yet!\n\n")
          (format
           #t "The following test aliases are defined:\n\n~A\n\n"
           (string-join alias-names ",\n"))))
     (mirror-type
      (let ((spec (assoc-ref mirror-specs mirror-type)))
        (run-mirror-sync spec run-id project-path temp-path data-path)))
     (else
      (let ((test-specs
             (srfi1:append-map
              (lambda (test-name)
                (load-test-specs specs-path test-name))
              test-names)))
        (cond
         ((srfi1:any (negate cdr) test-specs)
          (for-each
           (lambda (pair)
             (let ((test-name (car pair))
                   (spec (cdr pair)))
               (unless spec
                 (format #t "\nCould not find test specification for ~A!\n" test-name))))
           test-specs)
          (exit 1))
         (else
          (let ((test-runner
                 (lambda (pair)
                   (let ((test-name (car pair))
                         (spec (cdr pair)))
                     (run-test test-name spec run-id
                      temp-path data-path project-path
                      #:ovmf-code-file ovmf-code-file
                      #:ovmf-vars-file ovmf-vars-file
                      #:use-network? use-network?
                      #:parallel? parallel?
                      #:verify-only? (not (not verify-run-id)))))))
            (if parallel?
                (par-for-each test-runner test-specs)
                (for-each test-runner test-specs))))))))))
