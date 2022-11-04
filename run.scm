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
 ((ice-9 expect)
  #:select
  ((expect . expect-old)
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

(define-macro (interact . args)
  "Scheme procedure: interact [expect-port] [lock-filename]

Forks the current process and gives control to the user reading lines of commands
and forwarding them to the VM process. The stdout and stderr of the VM process
are then printed to the current standard output.

Takes optional arguments `expect-port' and `lock-filename'.

The `expect-port' should be an input-output pipe to communicate with the current process.
By default it expect `expect-port' binding to be defined in the current lexical context.
The `lock-filename' is used to create a temporary empty file (by default `interact.LCK')
to signal to the current process when the user has decided to exit the interactive mode.
This temporary file is removed afterwards.

Quiting interactive mode is done by typing the `quit' command."
  (let ((expect-port (or (and (pair? args) (car args)) 'expect-port))
        (lock-filename (or (and (pair? args) (cadr args)) "interact.LCK")))
    `(let ((pid (primitive-fork)))
       (cond
        ((zero? pid)
         (let loop ((line (readline)))
           (cond
            ((equal? line "quit")
             (let ((port (open-output-file ,lock-filename)))
               (newline port)
               (close port))
             (newline ,expect-port)
             (primitive-exit 0))
            (else
             (display line ,expect-port)
             (newline ,expect-port)
             (loop (readline))))))
        (else
         (let loop ()
           (expect-old
            ((const #t)
             (cond
              ((file-exists? ,lock-filename)
               (delete-file ,lock-filename))
              (else (loop))))))
         (waitpid pid))))))

(define (printable-char? c)
  (or (eqv? c #\newline) (not (eqv? 'Cc (char-general-category c)))))

(define (fetch-pid pipe)
  ((@@ (ice-9 popen) pipe-info-pid)
   ((@@ (ice-9 popen) fetch-pipe-info) pipe)))

(define* (run-qemu
	  #:key name memory network? sources-path mirrors-path cdrom-path drives-path (drive-specs '()) uefi?
	  (ovmf-code-file "/usr/share/OVMF/OVMF_CODE.fd")
	  (ovmf-vars-file "/usr/share/OVMF/OVMF_VARS.fd"))
  (when uefi?
    (cond
     ((file-exists? ovmf-vars-file)
      (copy-file ovmf-vars-file (utils:path drives-path (basename ovmf-vars-file))))
     (else (error "OVMF_VARS file doesn't exist!" ovmf-vars-file))))
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
		   ("file" . ,(utils:path drives-path (basename ovmf-vars-file))))))
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
	    ,@(if (and mirrors-path (not network?))
	       (list
		"-virtfs"
		(utils:emit-arg-alist
		 `("local"
		   ("path" . ,mirrors-path)
		   ("mount_tag" . "mirrors")
		   ("security_model" . "mapped"))))
	       '())))))
    (setvbuf port 'none)
    (set-port-encoding! port "UTF-8")
    port))

(define options-spec
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
     (predicate ,utils:directory?)
     (default "/var/tmp/system-setup"))
    (temp-path
     (single-char #\t)
     (description
      "Path to store temporary test files, including drives, logs, etc.")
     (value #t)
     (value-arg "path")
     (predicate ,utils:directory?)
     (default "/tmp/system-setup"))
    (verify
     (single-char #\V)
     (description "Run verification process only on existing test results for specific run (by RUNID timestamp).")
     (value #t)
     (value-arg "RUNID"))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define test-specs
 '(("debian-stretch-luks"
    ("guest"
     ("os" . "debian")
     ("release" . "stretch")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "luks")
       ("size" . "4G")
       ("interface" . "virtio"))))
    ("instroot"
     ("rootdev" . "/dev/disk/by-id/virtio-luks")
     ("luks-label" . "crypt_root")
     ("swapsize" . "100M")
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "stretch")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-stretch-luks-zfs"
    ("guest"
     ("os" . "debian")
     ("release" . "stretch")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "luks")
       ("size" . "4G")
       ("interface" . "virtio"))
      (("name" . "zfs1")
       ("size" . "1G")
       ("interface" . "virtio"))
      (("name" . "zfs2")
       ("size" . "1G")
       ("interface" . "virtio"))))
    ("zpool" "storage" "mirror" "/dev/disk/by-id/virtio-zfs1" "/dev/disk/by-id/virtio-zfs2")
    ("instroot"
     ("rootdev" . "/dev/disk/by-id/virtio-luks")
     ("luks-label" . "crypt_root")
     ("zpool" . "storage")
     ("swapsize" . "100M")
     ;; passphrase must be set for unattended mode even
     ;; though ZFS v0.6.5 does not support native encryption
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "stretch")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-stretch-zfs"
    ("guest"
     ("os" . "debian")
     ("release" . "stretch")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "boot")
       ("size" . "1G")
       ("interface" . "virtio"))
      (("name" . "zfs1")
       ("size" . "2G")
       ("interface" . "virtio"))
      (("name" . "zfs2")
       ("size" . "2G")
       ("interface" . "virtio"))))
    ("zpool" "storage" "mirror" "/dev/disk/by-id/virtio-zfs1" "/dev/disk/by-id/virtio-zfs2")
    ("instroot"
     ("bootdev" . "/dev/disk/by-id/virtio-boot")
     ("zpool" . "storage")
     ("swapsize" . "100M"))
    ("install"
     ("os" . "debian")
     ("release" . "stretch")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-buster-luks"
    ("guest"
     ("os" . "debian")
     ("release" . "buster")
     ("username" . "user")
     ("password" . "live")
     ("drives" .
      ((("name" . "main")
	("size" . "4G")
	("if" . "virtio")))))
    ("instroot"
     ("rootdev" . "/dev/disk/by-id/virtio-main")
     ("luks-label" . "crypt_root")
     ("swapsize" . "100M")
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "buster")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-buster-zfs"
    ("guest"
     ("network" . #t)
     ("os" . "debian")
     ("release" . "buster")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "boot")
       ("size" . "1G")
       ("interface" . "virtio"))
      (("name" . "zfs1")
       ("size" . "3G")
       ("interface" . "virtio"))
      (("name" . "zfs2")
       ("size" . "3G")
       ("interface" . "virtio"))))
    ("zpool" "storage" "mirror" "/dev/disk/by-id/virtio-zfs1" "/dev/disk/by-id/virtio-zfs2")
    ("instroot"
     ("zpool" . "storage")
     ("bootdev" . "/dev/disk/by-id/virtio-boot")
     ("swapsize" . "100M")
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "buster")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-bullseye-luks"
    ("enabled" . #t)
    ("guest"
     ("os" . "debian")
     ("release" . "bullseye")
     ("username" . "user")
     ("password" . "live")
     ("drives" .
      ((("name" . "main")
	("size" . "4G")
	("interface" . "virtio")))))
    ("instroot" .
     (("rootdev" . "/dev/disk/by-id/virtio-main")
      ("luks-label" . "crypt_root")
      ("swapsize" . "100M")
      ("passphrase" . "asonetuh")))
    ("install" .
     (("os" . "debian")
      ("release" . "bullseye")
      ("hostname" . "besenczy")
      ("sudouser" . "dadinn")
      ("password" . "asonetuh"))))
   ("debian-bullseye-zfs"
    ("enabled" . #t)
    ("guest"
     ("os" . "debian")
     ("release" . "bullseye")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "boot")
       ("size" . "1G")
       ("interface" . "virtio"))
      (("name" . "zfs1")
       ("size" . "3G")
       ("interface" . "virtio"))
      (("name" . "zfs2")
       ("size" . "3G")
       ("interface" . "virtio"))))
    ("zpool" "storage" "mirror" "/dev/disk/by-id/virtio-zfs1" "/dev/disk/by-id/virtio-zfs2")
    ("instroot"
     ("zpool" . "storage")
     ("bootdev" . "/dev/disk/by-id/virtio-boot")
     ("swapsize" . "100M")
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "bullseye")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))
   ("debian-bullseye-luks-zfs"
    ("guest"
     ("os" . "debian")
     ("release" . "bullseye")
     ("username" . "user")
     ("password" . "live")
     ("drives"
      (("name" . "main")
       ("size" . "3G")
       ("interface" . "virtio"))
      (("name" . "zfs1")
       ("size" . "3G")
       ("interface" . "virtio"))
      (("name" . "zfs2")
       ("size" . "3G")
       ("interface" . "virtio"))))
    ("zpool" "storage" "mirror" "/dev/disk/by-id/virtio-zfs1" "/dev/disk/by-id/virtio-zfs2")
    ("instroot"
     ("rootdev" . "/dev/disk/by-id/virtio-main")
     ("luks-label" . "vda3_crypt")
     ("zpool" . "storage")
     ("swapsize" . "100M")
     ("passphrase" . "asonetuh"))
    ("install"
     ("os" . "debian")
     ("release" . "bullseye")
     ("hostname" . "besenczy")
     ("sudouser" . "dadinn")
     ("password" . "asonetuh")))))

(define (init-matcher log-path)
 (when (not (file-exists? log-path))
  (utils:mkdir-p log-path))
 (lambda* (id pattern #:rest args)
   (let* ((pattern (apply format #f pattern args))
	  (pattern-length (string-length pattern))
	  (log-file (utils:path log-path (string-append id ".log")))
	  (log-port (open-output-file log-file)))
     (format log-port "EXPECTING: ~A\nMATCHING AGAINST:\n" pattern)
     (lambda (s eof?)
       (cond
        ((not eof?)
	 (let ((content (substring s (max 0 (- (string-length s) pattern-length 1)))))
	   (when (< 0 (string-length s))
	     (display (string-ref s (- (string-length s) 1)) log-port))
	   (cond
	    ((regex:string-match pattern content)
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
	  ((eq? "passphrase" key)
	   (string-append " -p " (or val "IGNORED")))
	  ((eq? "rootdev" key)
	   (string-append " -r " val))
	  ((eq? "luks-label" key)
	   (string-append " -l " val))
	  ((eq? "uefi" key) " -E")
	  ((eq? "bootdev" key)
	   (string-append " -b " val))
	  ((eq? "zpool" key)
	   (string-append " -z " val))
	  ((eq? "swapsize" key)
	   (string-append " -s " val))
	  ((eq? "swapfiles" key)
	   (string-append " -S " val))
	  ((eq? "luks-devs" key)
	   (string-append " -v "
	    (utils:emit-arg-alist val
	     #:pair-separator #\:
	     #:list-separator #\,)))
	  ((eq? "luksv2" key)
	   (if val " -L" ""))
	  ((eq? "uefi" key)
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
	  ((eq? "hostname" key)
	   (string-append " -n " val))
	  ((eq? "sudouser" key)
	   (string-append " -u " val))
	  ((eq? "password" key)
	   (string-append " -p " val))
	  ((eq? "release" key)
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
      ("username" . "user")
      ("password" . "live")))))

(define live-iso-specs
  '(("debian" .
     (("stretch" .
       (("filename" . "debian-live-9.2.0-amd64-gnome.iso")))
      ("buster" .
       (("torrent" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	("filename" . "debian-live-10.3.0-amd64-standard.iso")))
      ("bullseye" .
       (("torrent" . "magnet:?xt=urn:btih:e5f75ea3ff72e4aea39caecd870e8e43a9ed14b5&dn=debian-live-11.2.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	("filename" . "debian-live-11.2.0-amd64-standard.iso")))
      ("archlinux" .
       (("2020.01.01" .
	 (("curl" ."https://archive.archlinux.org/iso/2020.01.01/archlinux-2020.01.01-x86_64.iso")
	  ("filename" ."archlinux-2020.01.01-x86_64.iso")))))))))

(define (resolve-iso-path data-path os release)
  (let* ((iso-dir (utils:path data-path "isos"))
	 (iso-path
	  (utils:path iso-dir
	   (utils:assoc-get live-iso-specs os release "filename"))))
    (when (not (utils:directory? iso-dir))
      (utils:mkdir-p iso-dir))
    (cond
     ((file-exists? iso-path) iso-path)
     (else (error "Cannot find ISO image!" iso-path)))))

(define (open-log-port logs-path filename)
 (when (not (utils:directory? logs-path))
  (utils:mkdir-p logs-path))
 (let ((log-port (open-output-file (utils:path logs-path filename))))
  (setvbuf log-port 'none)
  log-port))

(define* (run-test #:key name run-id temp-path data-path sources-path use-network? verify-run)
  (when (not (assoc-ref test-specs name))
    (error "No spec exists for test name!" name))
  (let* ((spec (assoc-ref test-specs name))
	 (use-network? (or use-network? (utils:assoc-get spec "guest" "network")))
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
	  (lambda (c)
	    (display c log-port)
	    (display c)))
	 (matcher (init-matcher (utils:path logs-path "expect")))
	 (drives-path (utils:path test-path "drives"))
	 (drive-specs (utils:assoc-get spec "guest" "drives"))
	 (live-username (utils:assoc-get spec "guest" "username"))
	 (live-password (utils:assoc-get spec "guest" "password")))
    (dynamic-wind
      (const #t)
      (lambda ()
    ;; START RUN
    (when (not verify-run)
      (when (not (or use-network? (utils:directory? mirror-path)))
	(error "Not using network, yet local mirror directory doesn't exist!.
Either run with networking enabled, or synchronise apt-mirror first!"))
    (when (not (utils:directory? drives-path))
      (utils:mkdir-p drives-path))
    (for-each
     (lambda (drive-spec)
       (let* ((filename (assoc-ref drive-spec "name"))
	      (filename (string-append filename ".img"))
	      (path (utils:path drives-path filename))
	      (size (assoc-ref drive-spec "size")))
	 (when (not (file-exists? path))
	   (system* "qemu-img" "create" "-f" "qcow2" path size))))
     drive-specs)
    (let* ((expect-port
	    (run-qemu
	     #:name name
	     #:memory "4096"
	     #:network? use-network?
	     #:uefi? uefi?
	     #:sources-path sources-path
	     #:mirrors-path mirror-path
	     #:cdrom-path cdrom-path
	     #:drives-path drives-path
	     #:drive-specs drive-specs)))
      (dynamic-wind
	(const #t)
	(lambda ()
	  (cond
	   (uefi?
	    ;; Generate Live CDs (at least for Debian) which can boot in UEFI from serial console.
	    ;; Maybe something along these lines:
	    ;; https://p5r.uk/blog/2020/instaling-debian-over-serial-console.html
	    (error "Booting does not work from Live CD when using UEFI!!!"))
	   (else
	    (expect
	     ((matcher "boot01" "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
	      (sleep 1)
              (format expect-port "\t")
	      (sleep 1)
              (format expect-port " console=ttyS0\n")))))
	  (expect
	   ((matcher "boot02" "debian login:")
            (format expect-port "~A\n" live-username)))
	  (expect
	   ((matcher "boot03" "Password:")
            (format expect-port "~A\n" live-password)))
	  (expect
	   ((matcher "boot04" "\\$ ")
            (format expect-port "sudo -i\n")))
	  (expect
	   ((matcher "boot05" "# ")
	    (format expect-port "export LC_ALL=C\n")))
	  (expect
	   ((matcher "boot06" "# ")
	    (format expect-port "mkdir /mnt/sources\n")))
	  (expect
	   ((matcher "boot07" "# ")
            (format expect-port
             "mount -t 9p -o ~A sources /mnt/sources\n"
             (utils:emit-arg-alist
	      '(("trans" . "virtio")
		("msize" . "104857600")
		"ro")))))
	    (when (not use-network?)
	     (expect
	      ((matcher "mirror01" "# ")
	       (format expect-port "mkdir -p /var/spool/apt-mirror\n")))
	     (expect
	      ((matcher "mirror02" "# ")
               (format expect-port
                "mount -t 9p -o ~A mirrors /var/spool/apt-mirror\n"
                (utils:emit-arg-alist
		 '(("trans" . "virtio")
		   ("msize" . "104857600")
		   "ro")))))
	     (expect
	      ((matcher "mirror03" "# ")
	       (format expect-port "if [ -e /etc/apt/sources.list.d/base.list ]; then echo updating /etc/apt/sources.list; mv /etc/apt/sources.list.d/base.list /etc/apt/sources.list; fi\n")))
	     (expect
	      ((matcher "mirror04" "# ")
	       (format expect-port "sed -i -E 's;^deb ([^ ]+) ([^ ]+) main.*$;deb file:///var/spool/apt-mirror/mirror/deb.debian.org/debian/ \\2 main;g' /etc/apt/sources.list\n"))))
	    (expect
	     ((matcher "test00" "# ")
	      (format expect-port "apt update\n")))
	    (expect
	     ((matcher "test01" "# ")
	      (format expect-port "apt install -y ~A"
	       (utils:assoc-get guest-guile-package
		(utils:assoc-get spec "guest" "os")
		(utils:assoc-get spec "guest" "release")))
	      (newline expect-port)))
	    (expect
	     ((matcher "test02" "# ")
	      (call-init-zpool expect-port spec)))
	    (expect
	     ((matcher "test03" "# ")
	      (call-init-instroot expect-port spec)))
	    (when (not use-network?)
	      (expect
	       ((matcher "test04" "# ")
		(format expect-port "apt install -y nginx\n")))
	      (expect
	       ((matcher "test05"  "# ")
		(format expect-port "cp /mnt/sources/tests/mirrors/apt/apt-mirror.conf /etc/nginx/conf.d/\n")))
	      (expect
	       ((matcher "test06" "# ")
		(format expect-port "systemctl restart nginx\n")
		(sleep 10))))
	    (expect
	     ((matcher "test07" "# ")
	      (call-debian-setup expect-port spec use-network?)))
	    (expect
	     ((matcher "test08" "FINISHED INSTALLING NEW DEBIAN SYSTEM!")
	      (format expect-port "systemctl poweroff\n")
	      (sleep 5))))
	(lambda ()
	  (kill (fetch-pid expect-port) SIGTERM)
	  (popen:close-pipe expect-port)
	  (format #t "\nTerminated QEMU process for test ~A, run ~A!\n" name run-id)))))
    ;; VERIFY RUN
      (format #t "\nVerifying results for ~A\n" name)
      (let* ((expect-port
	      (run-qemu
	       #:name (string-append name "_verify")
	       #:memory "4096"
	       #:network? #f
	       #:drives-path drives-path
	       #:drive-specs drive-specs))
	     (matcher (init-matcher logs-path))
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
	      (format expect-port "swapon -s\n")
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
	      (format expect-port "systemctl poweroff\n")))
	    (expect
	     ((matcher "verify12" "reboot: Power down")
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
         (expect-char-proc
          (lambda (c)
            (display c log-port)
            (display c)))
         (matcher (init-matcher (utils:path logs-path "expect")))
         (live-username (utils:assoc-get guest-spec "username"))
         (live-password (utils:assoc-get guest-spec "password")))
    (dynamic-wind
      (const #t)
      (lambda ()
        (when (not (utils:directory? mirror-path))
          (utils:mkdir-p mirror-path))
        (let* ((expect-port
                (run-qemu
                 #:name name
                 #:memory "1024"
                 #:network? #t
                 #:cdrom-path cdrom-path
                 #:mirrors-path mirror-path
                 #:sources-path sources-path)))
          (dynamic-wind
            (const #t)
            (lambda ()
              (expect
               ((matcher "boot01" "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
                (sleep 1)
                (format expect-port "\t")
                (sleep 1)
                (format expect-port " console=ttyS0\n")))
              (expect
               ((matcher "boot02" "debian login:")
                (format expect-port "~A\n" live-username)))
              (expect
               ((matcher "boot03" "Password:")
                (format expect-port "~A\n" live-password)))
              (expect
               ((matcher "boot04" "\\$ ")
                (format expect-port "sudo -i\n")))
              (expect
               ((matcher "boot05" "# ")
                (format expect-port "export LC_ALL=C\n")))
              (expect
               ((matcher "mount01" "# ")
                (format expect-port "mkdir /mnt/sources\n")))
              (expect
               ((matcher "mount02" "# ")
                (format expect-port
                 "mount -t 9p -o ~A sources /mnt/sources\n"
                 (utils:emit-arg-alist
                  '(("trans" . "virtio")
                    ("msize" . "104857600")
                    "ro")))))
              (expect
               ((matcher "mount03" "# ")
                (format expect-port "mkdir -p /var/spool/apt-mirror\n")))
              (expect
               ((matcher "mount04" "# ")
                (format expect-port
                 "mount -t 9p -o ~A mirrors /var/spool/apt-mirror\n"
                 (utils:emit-arg-alist
                  '(("trans" . "virtio")
                    ("msize" . "104857600"))))))
              (expect
               ((matcher "mirror01" "# ")
                (format expect-port "apt update\n")))
              (expect
               ((matcher "mirror02" "# ")
                (format expect-port "apt install -y apt-mirror\n")))
              (expect
               ((matcher "mirror03" "# ")
                (format expect-port "cp /mnt/sources/tests/mirrors/apt/mirror.list /etc/apt/\n")))
              (expect
               ((matcher "mirror04" "# ")
                (format expect-port "apt-mirror\n")))
              (expect
               ((matcher "mirror05" "# ")
                (format #t "\nFinished synchronising apt-mirror!\n"))))
            (lambda ()
              (kill (fetch-pid expect-port) SIGTERM)
              (popen:close-pipe expect-port)
              (format #t "\nTerminated QEMU process test ~A, run ~A!\n" name run-id)))))
      (lambda () (close-port log-port)))))

(define (main args)
  (let* ((project-path (dirname (dirname (current-filename))))
	 (options (utils:getopt-extra args options-spec))
	 (start-time (current-time))
	 (verify-run (hash-ref options 'verify))
	 (run-id
	  (or verify-run
	   (strftime "%Y%m%d_%H%M%S"
	     (localtime start-time))))
	 (data-path (hash-ref options 'data-path))
	 (temp-path (hash-ref options 'temp-path))
	 (mirror-type (hash-ref options 'sync-mirror))
	 (use-network? (hash-ref options 'use-network))
	 (test-names (hash-ref options '()))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (format #t "
Start up Qemu/KVM machine to run test specs based on specified IDs.

USAGE:

~A [OPTION...] [ID...]

Valid OPTION value are:

~A

The following test specification ID values are avaible:

~A

When no test spec ID is specified, only the enabled tests (ones marked with *) are run.

"
       (basename (car args))
       (utils:usage options-spec)
       (string-join
	(map
	 (lambda (spec)
	   (if (assoc-ref spec "enabled")
	    (string-append (car spec) "*")
	    (car spec)))
	 test-specs)
	",\n")))
     (mirror-type
      (let ((spec (assoc-ref mirror-specs mirror-type)))
        (run-mirror-sync spec run-id project-path temp-path data-path)))
     (else
      (unless (utils:directory? data-path)
	(utils:mkdir-p data-path))
      (unless (utils:directory? temp-path)
	(utils:mkdir-p temp-path))
      (for-each
       (lambda (test-name)
	(run-test
	 #:run-id run-id
	 #:name test-name
	 #:sources-path project-path
	 #:data-path data-path
	 #:temp-path temp-path
	 #:use-network? use-network?
	 #:verify-run verify-run))
       (if (null? test-names)
	(map (lambda (spec) (car spec))
	 (filter
	  (lambda (spec) (assoc-ref spec "enabled"))
	  test-specs))
	(map
	 (lambda (name)
	  (if (not (assoc-ref test-specs name))
	   (error "No spec exists for test name!" name)
	   name))
	 test-names)))))))

