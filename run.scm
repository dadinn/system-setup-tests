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

(define* (run-qemu
	  #:key name memory network? sources-path mirrors-path cdrom-path drives-path drive-specs uefi?
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
     (description "Run verification process only on existing test results for specific run TIMESTAMP.")
     (value #t)
     (value-arg "TIMESTAMP"))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define test-specs
 '(("debian-buster-luks"
    ("guest"
     ("os" . "debian")
     ("release" . "buster")
     ("username" . "user")
     ("password" . "live")
     ("iso" .
      (("torrent" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
       ("filename" . "debian-live-10.3.0-amd64-standard.iso")))
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
     ("iso" .
      (("torrent" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
       ("filename" . "debian-live-10.3.0-amd64-standard.iso")))
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
     ("iso" .
      (("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
       ("filename" . "debian-live-11.1.0-amd64-standard.iso")))
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
     ("iso"
      ("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
      ("filename" . "debian-live-11.1.0-amd64-standard.iso"))
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
   ("archlinux-luks" .
    (("os" . "archlinux")
     ("iso" .
      (("curl" ."https://archive.archlinux.org/iso/2020.01.01/archlinux-2020.01.01-x86_64.iso")
       ("filename" ."archlinux-2020.01.01-x86_64.iso")))))))

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

(define (call-init-zpool spec port)
  (let ((args (assoc-ref spec "zpool")))
    (if args
      (format port
       "/mnt/sources/init-instroot/init-instroot.scm -A -p ~A -Z ~A\n"
       (utils:assoc-get spec "instroot" "passphrase")
       (string-join args " "))
      (newline port))))

(define (call-init-instroot spec port)
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
	   (string-append " -p " val))
	  ((eq? "rootdev" key)
	   (string-append " -r " val))
	  ((eq? "luks-label" key)
	   (string-append " -l " val))
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

(define* (call-debian-setup spec port #:optional use-network?)
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
    "-m http://localhost:8080/debian"
    "")))

(define debian-guile-versions
  '(("bullseye" . "guile-3.0")
    ("buster" . "guile-2.2")
    ("stretch" . "guile-2.0")))

(define os-mirror-type
  '(("debian" . "apt")))

(define live-isos-spec
 '(("debian"
    ("bullseye"
     ("iso"
      ("filename" . "debian-live-11.1.0-amd64-standard.iso")
      ("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce"))
     ("username" . "user")
     ("password" . "live"))
    ("buster"
     ("iso"
      ("filename" . "debian-live-10.10.0-amd64-standard.iso")
      ("torrent" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce"))
     ("username" . "user")
     ("password" . "live")))
   ("archlinux"
    ("2020.01.01"
     ("iso"
      ("curl" ."https://archive.archlinux.org/iso/2020.01.01/archlinux-2020.01.01-x86_64.iso")
      ("filename" ."archlinux-2020.01.01-x86_64.iso"))))))

(define (resolve-iso-path data-path spec)
  (let ((iso-path (utils:path data-path "isos" (assoc-ref spec "filename"))))
    (cond
     ((file-exists? iso-path) iso-path)
     (else (error "Cannot find ISO image!" iso-path)))))

(define (open-log-port logs-path filename)
 (when (not (utils:directory? logs-path))
  (utils:mkdir-p logs-path))
 (let ((log-port (open-output-file (utils:path logs-path filename))))
  (setvbuf log-port 'none)
  log-port))

(define* (run-test #:key name temp-path data-path sources-path use-network? sync-mirror? verify-run)
  (let* ((spec (assoc-ref test-specs name))
	 (use-network? (or use-network? (utils:assoc-get spec "guest" "network")))
	 (mirror-path
	  (utils:path
	   data-path "mirrors"
	   (assoc-ref
	    os-mirror-type
	    (utils:assoc-get spec "guest" "os"))))
	 (cdrom-path
	  (resolve-iso-path
	   data-path
	   (utils:assoc-get spec "guest" "iso")))
	 (test-path (utils:path temp-path name))
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
      (when (not (utils:directory? mirror-path))
	(cond
	 (sync-mirror?
	  (utils:mkdir-p mirror-path))
	 ((not use-network?)
	  (error "Not using network, yet local mirror directory doesn't exist!.
Either run with networking enabled, or synchronise apt-mirror first!"))))
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
     drive-specs)
    (let* ((expect-port
	    (run-qemu
	     #:name name
	     #:memory "4096"
	     #:network? (or use-network? sync-mirror?)
	     #:sources-path sources-path
	     #:mirrors-path mirror-path
	     #:cdrom-path cdrom-path
	     #:drives-path drives-path
	     #:drive-specs drive-specs)))
      (dynamic-wind
	(const #t)
	(lambda ()
	  (expect
	   ((matcher "boot01" "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
	    (sleep 1)
	    (display "\t" expect-port)
	    (sleep 1)
	    (display " console=ttyS0" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot02" "debian login:")
	    (display live-username expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot03" "Password:")
	    (display live-password expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot04" "\\$ ")
	    (display "sudo -i" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot05" "# ")
	    (display "export LC_ALL=C" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot06" "# ")
	    (display "mkdir /mnt/sources" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "boot07" "# ")
	    (display
	     (string-join
	      (list
	       "mount" "-t" "9p" "-o"
	       (utils:emit-arg-alist
		'(("trans" . "virtio")
		  ("msize" . "104857600")
		  "ro"))
	       "sources" "/mnt/sources")
	      " ") expect-port)
	    (newline expect-port)))
	  (cond
	   (sync-mirror?
	    (expect
	     ((matcher "mirror01" "# ")
	      (display "mkdir -p /var/spool/apt-mirror" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror02" "# ")
	      (display
	       (string-join
		(list
		 "mount" "-t" "9p" "-o"
		 (utils:emit-arg-alist
		  '(("trans" . "virtio")
		    ("msize" . "104857600")))
		 "mirrors" "/var/spool/apt-mirror")
		" ") expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror03" "# ")
	      (display "apt update" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror04" "# ")
	      (display "apt install -y apt-mirror" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror05" "# ")
	      (display "cp /mnt/sources/tests/mirrors/apt/mirror.list /etc/apt/" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror06" "# ")
	      (display "apt-mirror" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "mirror07" "# ")
	      (newline)
	      (utils:println "Finished synchronising apt-mirror!")
	      (newline))))
	   (else
	    (when (and (not use-network?))
	     (expect
	      ((matcher "mirror08" "# ")
	       (display "mkdir -p /var/spool/apt-mirror" expect-port)
	       (newline expect-port)))
	     (expect
	      ((matcher "mirror09" "# ")
	       (display
		(string-join
		 (list
		  "mount" "-t" "9p" "-o"
		  (utils:emit-arg-alist
		   '(("trans" . "virtio")
		     ("msize" . "104857600")
		     "ro"))
		  "mirrors" "/var/spool/apt-mirror")
		 " ")
		expect-port)
	       (newline expect-port)))
	     (expect
	      ((matcher "mirror10" "# ")
	       (display "sed -i -E 's;^deb ([^ ]+) ([^ ]+) main.*$;deb file:///var/spool/apt-mirror/mirror/deb.debian.org/debian/ \\2 main;g' /etc/apt/sources.list" expect-port)
	       (newline expect-port))))
	    (expect
	     ((matcher "mirror11" "# ")
	      (display "apt update" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "test01" "# ")
	      (format
	       expect-port "apt install -y ~A"
	       (assoc-ref debian-guile-versions
		(utils:assoc-get spec "guest" "release")))
	      (newline expect-port)))
	    (expect
	     ((matcher "test02" "# ")
	      (call-init-zpool spec expect-port)))
	    (expect
	     ((matcher "test03" "# ")
	      (call-init-instroot spec expect-port)))
	    (when (not use-network?)
	      (expect
	       ((matcher "test04" "# ")
		(display "apt install -y nginx" expect-port)
		(newline expect-port)))
	      (expect
	       ((matcher "test05"  "# ")
		(display "cp /mnt/sources/tests/mirrors/apt/apt-mirror.conf /etc/nginx/conf.d/" expect-port)
		(newline expect-port)))
	      (expect
	       ((matcher "test06" "# ")
		(display "systemctl restart nginx" expect-port)
		(newline expect-port)
		(sleep 10))))
	    (expect
	     ((matcher "test07" "# ")
	      (call-debian-setup spec expect-port use-network?)))
	    (expect
	     ((matcher "test08" "Shutting down the system...")
	      (sleep 5))))))
	(lambda ()
	  (popen:close-pipe expect-port)
	  (newline)
	  (display "Terminated QEMU process!")
	  (newline)))))
    ;; VERIFY RUN
    (when (not sync-mirror?)
      (newline)
      (format #t "Verifying results for ~A" name)
      (newline)
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
		(display passphrase expect-port)
		(newline expect-port))))
	     (zpool
	      (expect
	       ((matcher "verify02" "Enter passphrase for '~A':" zpool)
		(display passphrase expect-port)
		(newline expect-port)))))
	    (expect
	     ((matcher "verify03" "login: ")
	      (display sudouser expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify04" "Password: ")
	      (display password expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify05" "~A@~A:~~\\$ " sudouser hostname)
	      (display "sudo -i" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify06" "\\[sudo\\] password for ~A: " sudouser)
	      (display password expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify07" "root@~A:~~# " hostname)
	      (display "export LC_ALL=C" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify08" "root@~A:~~# " hostname)
	      (display "lsblk" expect-port)
	      (newline expect-port)))
	    (when zpool
	      (expect
	       ((matcher "verify09" "root@~A:~~# " hostname)
		(display "zpool status -P" expect-port)
		(newline expect-port)))
	      (expect
	       ((matcher "verify10" "root@~A:~~# " hostname)
		(display "zfs list -t all" expect-port)
		(newline expect-port))))
	    (expect
	     ((matcher "verify11" "root@~A:~~# " hostname)
	      (display "systemctl poweroff" expect-port)
	      (newline expect-port)))
	    (expect
	     ((matcher "verify12" "reboot: Power down")
	      (sleep 5))))
	  (lambda ()
	    (popen:close-pipe expect-port)
	    (newline)
	    (format #t "Finished verification for ~A!\n" name)
	    (newline))))))
      (lambda ()
	(close-port log-port)))))

(define (main args)
  (let* ((project-path (dirname (dirname (current-filename))))
	 (options (utils:getopt-extra args options-spec))
	 (start-time (current-time))
	 (data-path (hash-ref options 'data-path))
	 (temp-path (hash-ref options 'temp-path))
	 (sync-mirror? (hash-ref options 'sync-mirror))
	 (use-network? (hash-ref options 'use-network))
	 (verify-run (hash-ref options 'verify))
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
     (else
      (for-each
       (lambda (test-name)
	(if (not (assoc-ref test-specs test-name))
	 (error "No spec exists for test name!" test-name)
	 (run-test
	  #:name test-name
	  #:sources-path project-path
	  #:data-path data-path
	  #:temp-path
	  (utils:path
	   temp-path
	   (or verify-run
	    (strftime "%Y%m%d_%H%M%S"
	     (localtime start-time))))
	  #:use-network? use-network?
	  #:sync-mirror? sync-mirror?
	  #:verify-run verify-run)))
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


;; Matenak mukodott Archlinux-szal:
;;==================================
;; Ujabb image nem mukodott, de ezzel igen:
;; https://archive.archlinux.org/iso/2020.01.01/
;;
;; a parancs amivel futtatta:
;;
;; qemu-system-x86_64 --enable-kvm -cdrom archlinux-2020.01.01-x86_64.iso -m 512 -serial stdio -display none
;; amikor grub menu feljon TAB majd a kernel argumentumok vegere "console=ttyS0'
;; loginnal root user password nelkul bejelentkezik


;; ISO image manipullalas:
;;=========================
;; mkdir /mnt/debiso
;; mount -o loop debian-xyz.iso /mnt/debiso
;; rsync -av /mnt/debiso /tmp/debisobackup





  ;; OLD SHIT

(comment

 (comment
 (let ((port (open-pipe* OPEN_BOTH "cat")))
   (setvbuf port 'line 10)
   (display "FUCK\n" port)  
   (let ((res (read-string port))) (display res) (close port) res)))


 (system* "qemu-img" "create" "-f" "qcow" name "4G")
 
 (let ((port (popen:open-pipe* OPEN_BOTH
			       "qemu-system-x86_64"
			       "-enable-kvm"
			       "-m" "4096"
			       ;; "-drive" (string-append "file=" cdrom-path)
			       "-cdrom" cdrom-path
			       "-serial" "stdio"
			       ;;"-append" "console=ttyS0"
			       ;;"-nographic"
			       "-display" "none"
			       ;;"-smbios" "uefi=on"
			       )))
   (display "Hello world!!!")
   (while #t (display (get-line port)))

   (with-input-from-port port
     (lambda ()
       (with-output-to-port port
	 (lambda ()
	   (expect-strings
	    ("Press [Tab] to edit options"
	     (display "\t console=ttyS0")
	     (newline))))))))

 ;; this seems to work!!!
 (execlp
  "qemu-system-x86_64"
  "--enable-kvm"
  "-name" "TEST"
  "-cdrom" cdrom-path
  "-m" "512"
  "-serial" "stdio"
  ;;"-append" "console=ttyS0"
  ;;"-nographic"
  "-display" "none"
  )
 ;; After this, press tab in the GRUB ncurses choice window, and enter console=ttyS0, and press Enter
 ;; When asked for the "archiso login:" prompt type root, and Enter



 ;;these below don't work
 (execlp
  "qemu-system-x86_64"
  "-enable-kvm"
  "-m" "4096"
  ;; "-drive" (string-append "file=" cdrom-path)
  "-cdrom" cdrom-path
  "-serial" "stdio"
  "-append" "console=ttyS0" ;; THIS CAN'T work
  "-nographic"
  ;;"-smbios" "uefi=on"
  )

 (system*
  "virt-install"
  ;;"--connect" "qemu:///system"
  "--connect" "qemu:///session"
  "--name" name
  "--description" "testing system installation"
  "--ram" "4096"
  "--vcpu" "2"
  ;;"--os-type" "linux"
  ;;"--os-variant" "Debian10"
  "--cdrom" "/home/dadinn/Downloads/isos/debian-live-10.3.0-amd64-standard.iso"
  ;; ISO image doesn't seem to work with location
  ;;"--location" "/home/dadinn/Downloads/isos/debian-live-10.3.0-amd64-standard.iso"
  ;;"--location" "http://ftp.us.debian.org/debian/dists/buster/main/installer-amd64/"
  ;;"--extra-args" "console=tty0,115200n8,serial,systemd.unit=multi-user.target"
  ;;"--extra-args" "console=ttyS0"
  "--disk" (string-append "path=disks/" name ".img,bus=virtio,size=4")
  "--filesystem" (string-append sources-path ",sources")
  ;;"--boot" "uefi"
  "--graphics" "none"
  "--serial" "stdio"
  "--debug"
  ;; this is not working with forced poweroff
  ;;"--events" "on_poweroff=destroy"
  ))
