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
 ((ice-9 expect)))

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
		   `(("file" . ,(utils:path drives-path (assoc-ref spec "name")))
		     ("format" . "qcow2")
		     ("if" . ,(assoc-ref spec "if"))
		     ("media" . "disk")))))
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
	    "-virtfs"
	    ,(utils:emit-arg-alist
	      `("local" "readonly"
		("path" . ,sources-path)
		("mount_tag" . "sources")
		("security_model" . "mapped")))
	    "-virtfs"
	    ,(utils:emit-arg-alist
	     `("local"
	       ("path" . ,mirrors-path)
	       ("mount_tag" . "mirrors")
	       ("security_model" . "mapped")))))))
    (setvbuf port 'none)
    (set-port-encoding! port "UTF-8")
    port))

(define options-spec
  `((name
     (single-char #\n)
     (description "Name to identify test execution.")
     (value #t)
     (value-arg "TEXT")
     (default "debian-bullseye-luks"))
    (cdrom
     (description
      "Path to Live ISO file")
     (value #t)
     (value-arg "PATH")
     (predicate ,(lambda (path) (file-exists? path)))
     (default "/home/dadinn/Downloads/isos/debian-live-11.1.0-amd64-standard.iso"))
    (sync-mirror
     (single-char #\M)
     (description "Synchronise local apt-mirror via internet, and then exit."))
    (use-network
     (single-char #\N)
     (description "Use network for all package dependencies, and disable local apt-mirror."))
    (data
     (single-char #\d)
     (description
      "Path to store temporary mirrors, isos, etc.")
     (value #t)
     (value-arg "path")
     (predicate ,utils:directory?)
     (default "/var/tmp/system-setup"))
    (temp
     (single-char #\t)
     (description
      "Path to store temporary test files, including drives, logs, etc.")
     (value #t)
     (value-arg "path")
     (predicate ,utils:directory?)
     (default "/tmp/system-setup"))
    (verify-only
     (single-char #\V)
     (description "Run verification process only on existing test results."))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define tests-spec
  '(("debian-buster-luks" .
     (("guest" .
       (("os" . "debian")
	("release" . "buster")
	("username" . "user")
	("password" . "live")
	("iso" .
	 (("torrent" . "magnet:?xt=urn:btih:7bf9f33a7cc577b7829a4b9db8fe89dacd6eabd9&dn=debian-live-10.10.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	  ("filename" . "debian-live-10.3.0-amd64-standard.iso")))
	("drives" .
	 ((("name" . "main.img")
	   ("size" . "4G")
	   ("if" . "virtio"))))
	("uefi" . #f)))
      ("instroot" .
       (("rootdev" . "/dev/vda")
	("swapsize" . "100M")
	("passphrase" . "asonetuh")))
      ("install" .
       (("os" . "debian")
	("hostname" . "besenczy")
	("sudouser" . "dadinn")
	("password" . "asonetuh")))))
    ("debian-bullseye-luks" .
     (("guest" .
       (("os" . "debian")
	("release" . "bullseye")
	("username" . "user")
	("password" . "live")
	("iso" .
	 (("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	  ("filename" . "debian-live-11.1.0-amd64-standard.iso")))
	("drives" .
	 ((("name" . "main.img")
	   ("size" . "4G")
	   ("if" . "virtio"))))
	("uefi" . #f)))
      ("instroot" .
       (("rootdev" . "/dev/vda")
	("swapsize" . "100M")
	("passphrase" . "asonetuh")))
      ("install" .
       (("os" . "debian")
	("hostname" . "besenczy")
	("sudouser" . "dadinn")
	("password" . "asonetuh")))))
    ("debian-bullseye-zfs" .
     (("guest" .
       (("os" . "debian")
	("release" . "bullseye")
	("username" . "user")
	("password" . "live")
	("iso" .
	 (("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	  ("filename" . "debian-live-11.1.0-amd64-standard.iso")))
	("drives" .
	 ((("name" . "boot.img")
	   ("size" . "1G")
	   ("if" . "virtio"))
	  (("name" . "zfs1.img")
	   ("size" . "3G")
	   ("if" . "virtio"))
	  (("name" . "zfs2.img")
	   ("size" . "3G")
	   ("if" . "virtio"))))
	("uefi" . #f)))
      ("zpool" "storage" "mirror" "/dev/vdb" "/dev/vdc")
      ("instroot" .
       (("zpool" . "storage")
	("bootdev" . "/dev/vda")
	("swapsize" . "100M")
	("passphrase" . "asonetuh")))
      ("install" .
       (("os" . "debian")
	("hostname" . "besenczy")
	("sudouser" . "dadinn")
	("password" . "asonetuh")))))
    ("debian-bullseye-luks" .
     (("guest" .
       (("os" . "debian")
	("release" . "bullseye")
	("iso" .
	 (("torrent" . "magnet:?xt=urn:btih:f3d7a863cc4eadce466a7aa3194e14ce9179d907&dn=debian-live-11.1.0-amd64-standard.iso&tr=http%3A%2F%2Fbttracker.debian.org%3A6969%2Fannounce")
	  ("filename" . "debian-live-11.1.0-amd64-standard.iso")))))))
    ("archlinux-luks" .
     (("os" . "archlinux")
      ("iso" .
       (("curl" ."https://archive.archlinux.org/iso/2020.01.01/archlinux-2020.01.01-x86_64.iso")
	("filename" ."archlinux-2020.01.01-x86_64.iso")))))))

(define (resolve-iso-path spec data-path)
  (let ((iso-path (utils:path data-path "isos" (utils:assoc-get spec "guest" "iso" "filename"))))
    (cond
     ((file-exists? iso-path) iso-path)
     (else (error "Cannot find ISO image!" iso-path)))))

(define (init-matcher logs-path)
  (lambda (pattern)
    (let ((start-time (current-time)))
      (lambda (s eof?)
	(if (not eof?)
	    ;;This is needed to support matching against output with null characters
	    (let ((stuff (string-filter printable-char? s)))
	      (with-output-to-file
		  (utils:path
		   logs-path
		   (string-append
		    "expect_"
 		    (strftime "%y%m%d_%H%M%S" (localtime start-time))
		    ".log"))
		(lambda ()
		  (display (string-append "EXPECTING: " pattern))
		  (newline)
		  (display "MATCHING AGAINST:")
		  (newline)
		  (display stuff)))
	      (regex:string-match pattern stuff))
	    #f)))))

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
	  (else "")))))
    "" (assoc-ref spec "install"))
   (if (not use-network?)
    "-m http://localhost:8080/debian"
    "")))

(define os-mirror-type
  '(("debian" . "apt")))

(define (main args)
  (let* ((project-path (dirname (dirname (current-filename))))
	 (options (utils:getopt-extra args options-spec))
	 (test-name (hash-ref options 'name))
	 (test-spec (assoc-ref tests-spec test-name))
	 (data-path (hash-ref options 'data))
	 (mirror-path
	  (utils:path
	   data-path "mirrors"
	   (assoc-ref
	    os-mirror-type
	    (utils:assoc-get test-spec "guest" "os"))))
	 (cdrom-path (resolve-iso-path test-spec data-path))
	 (temp-path (hash-ref options 'temp))
	 (test-path (utils:path temp-path test-name))
	 (logs-path (utils:path test-path "logs"))
	 (drives-path (utils:path test-path "drives"))
	 (drive-specs (utils:assoc-get test-spec "guest" "drives"))
	 (sync-mirror? (hash-ref options 'sync-mirror))
	 (use-network? (hash-ref options 'use-network))
	 (verify-only? (hash-ref options 'verify-only))
	 ;; spec stuff
	 (live-username (utils:assoc-get test-spec "guest" "username"))
	 (live-password (utils:assoc-get test-spec "guest" "password"))
	 (help? (hash-ref options 'help)))
    (cond
     (help?
      (display
       (string-append "USAGE:

" (basename (car args)) " [OPTION...] [COMMAND...]

Start up KVM/Qemu machine to test out shit

Valid options are:

" (utils:usage options-spec)))
      (newline))
     ((not cdrom-path)
      (error "Live ISO image must be specified!"))
     ((and (not use-network?) (not (utils:directory? mirror-path)))
      (error "Not using network, yet local mirror directory doesn't exist!.
Either run with networking enabled, or synchronise apt-mirror first!"))
     (else
      (when (not (utils:directory? drives-path))
	(utils:mkdir-p drives-path))
      (when (not (utils:directory? logs-path))
	(utils:mkdir-p logs-path))
      (when (and sync-mirror? (not (utils:directory? mirror-path)))
	(utils:mkdir-p mirror-path))
      (for-each
       (lambda (spec)
	 (let* ((filename (assoc-ref spec "name"))
		(path (utils:path drives-path filename))
		(size (assoc-ref spec "size")))
	   (when (not (file-exists? path))
	     (system* "qemu-img" "create" "-f" "qcow2" path size))))
       drive-specs)
      (let* ((start-time (current-time))
	     (log-port
	      (open-output-file
	       (utils:path
		logs-path
		(string-append
		 (strftime "%y%m%d_%H%M%S" (localtime start-time))
		 "_" test-name
		 ".log"))))
	     (expect-char-proc
	      (lambda (c)
		(display c log-port)
		(display c)))
	     (expect-port
	      (run-qemu
	       #:name test-name
	       #:memory "4096"
	       #:network? (or use-network? sync-mirror?)
	       #:sources-path project-path
	       #:mirrors-path mirror-path
	       #:cdrom-path cdrom-path
	       #:drives-path drives-path
	       #:drive-specs drive-specs))
	     (matcher (init-matcher logs-path)))
	(dynamic-wind
	  (const #t)
	  (lambda ()
	(expect
	 ((matcher "\"Booting .* Installer with Speech Synthesis\\.\\.\\.\"")
	  (sleep 1)
	  (display "\t" expect-port)
	  (sleep 1)
	  (display " console=ttyS0" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "debian login:")
	  (display live-username expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Password:")
	  (display live-password expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "\\$ ")
	  (display "sudo -i" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "export LC_ALL=C" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "mkdir /mnt/sources" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
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
	   ((matcher "# ")
	    (display "mkdir -p /var/spool/apt-mirror" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
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
	   ((matcher "# ")
	    (display "apt update" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "apt install -y apt-mirror" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "cp /mnt/sources/tests/mirrors/apt/mirror.list /etc/apt/" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "apt-mirror" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (utils:println "Finished synchronising apt-mirror!"))))
	 (else
	  (when (and (not use-network?))
	  (expect
	   ((matcher "# ")
	    (display "mkdir -p /var/spool/apt-mirror" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
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
	    (newline expect-port)
	    (expect
	     ((matcher "# ")
	      (display "sed -i -E 's;^deb ([^ ]+) ([^ ]+) main.*$;deb file:///var/spool/apt-mirror/mirror/deb.debian.org/debian/ \\2 main contrib;g' /etc/apt/sources.list" expect-port)
	      (newline expect-port))))))
	(expect
	 ((matcher "# ")
	  (display "apt update" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "apt install -y guile-2.2" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (call-init-zpool test-spec expect-port)))
	(expect
	 ((matcher "# ")
	  (call-init-instroot test-spec expect-port)))
	(when (not use-network?)
	  (expect
	   ((matcher "# ")
	    (display "apt install -y nginx" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "cp /mnt/sources/tests/mirrors/apt/apt-mirror.conf /etc/nginx/conf.d/" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "systemctl restart nginx" expect-port)
	    (newline expect-port)
	    (sleep 10))))
	(expect
	 ((matcher "# ")
	  (call-debian-setup test-spec expect-port use-network?)))
	(expect
	 ((matcher "Shutting down the system...")
	  (sleep 10))))))
	  (lambda ()
	    (popen:close-pipe expect-port)
	    (close-port log-port)
	    (newline)
	    (display "Terminated QEMU process!")
	    (newline))))))))


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
