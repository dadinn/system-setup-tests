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

(define* (run-qemu #:key name memory network? cdrom sources mirrors drives)
  (let ((port
	 (apply popen:open-pipe* OPEN_BOTH
	  `("qemu-system-x86_64"
	    "-enable-kvm"
	    "-nographic"
	    ,@(if (not network?)  (list "-nic" "none") '())
	    "-m" ,(or memory "4096")
	    ;;"-smbios" "uefi=on")
	    ,@(if cdrom ; boot from CD-ROM the first time
	       (list "-boot" "once=d" "-cdrom" cdrom))
	    "-virtfs"
	    ,(utils:emit-arg-alist
	      `("local" "readonly"
		("path" . ,sources)
		("mount_tag" . "sources")
		("security_model" . "mapped")))
	    "-virtfs"
	    ,(utils:emit-arg-alist
	     `("local"
	       ("path" . ,mirrors)
	       ("mount_tag" . "mirrors")
	       ("security_model" . "mapped")))
	    ,@(srfi1:append-map
	       (lambda (conf)
		 (list
		  "-drive"
		  (utils:emit-arg-alist
		   `(("file" . ,(car conf))
		     ("format" . "qcow2")
		     ("if" . "virtio")
		     ("media" . "disk")))))
	       drives)))))
    (setvbuf port 'none)
    (set-port-encoding! port "UTF-8")
    port))

(define options-spec
  `((name
     (single-char #\n)
     (description "Name to identify test execution.")
     (value #t)
     (value-arg "TEXT")
     (default "testing"))
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
    (temp
     (single-char #\t)
     (description
      "Path to store temporary files, logs, mirrors, etc.")
     (value #t)
     (value-arg "path")
     (predicate ,utils:directory?)
     (default "/var/tmp/system-setup"))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define (main args)
  (let* ((project-path (dirname (dirname (current-filename))))
	 (options (utils:getopt-extra args options-spec))
	 (name (hash-ref options 'name))
	 (cdrom-path (hash-ref options 'cdrom))
	 (temp-path (hash-ref options 'temp))
	 (logs-path (utils:path temp-path "logs"))
	 (mirror-path (utils:path temp-path "mirror"))
	 (drives-path
	  (utils:path temp-path "drives"))
	 (drive-configs
	  (list
	   (cons (utils:path drives-path "main.img") "4G")))
	 (sync-mirror? (hash-ref options 'sync-mirror))
	 (use-network? (hash-ref options 'use-network))
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
Please either run with networking enabled, or synchronise apt-mirror first!"))
     (else
      (when (not (utils:directory? drives-path))
	(utils:mkdir-p drives-path))
      (when (not (utils:directory? logs-path))
	(utils:mkdir-p logs-path))
      (when (and sync-mirror? (not (utils:directory? mirror-path)))
	(utils:mkdir-p mirror-path))
      (for-each
       (lambda (config)
	 (let ((path (car config))
	       (size (cdr config)))
	   (when (not (file-exists? path))
	     (system* "qemu-img" "create" "-f" "qcow2" path size))))
       drive-configs)
      (let* ((start-time (current-time))
	     (log-port
	      (open-output-file
	       (utils:path
		logs-path
		(string-append
		 (strftime "%y%m%d_%H%M%S" (localtime start-time))
		 "_" name
		 ".log"))))
	     (expect-char-proc
	      (lambda (c)
		(display c log-port)
		(display c)))
	     (expect-port
	      (run-qemu
	       #:name name
	       #:memory "4096"
	       #:cdrom cdrom-path
	       #:network? (or use-network? sync-mirror?)
	       #:mirrors mirror-path
	       #:sources project-path
	       #:drives drive-configs))
	     (matcher (init-matcher logs-path))
	     (live-username "user")
	     (live-password "live")
	     (hostname "shitfuck")
	     (sudo-username "fuckshit")
	     (sudo-password "fuckshit")
	     (root-dev "/dev/vda")
	     (luks-passhprase "fuckshit"))
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
	  (display "user" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Password:")
	  (display "live" expect-port)
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
	    (display "cp /mnt/sources/tests/apt-mirror.list /etc/apt/mirror.list" expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display "apt-mirror" expect-port)
	    (newline expect-port)))
	  (utils:println "Finished synchronising apt-mirror!")
	  (exit 0))
	 ((not use-network?)
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
	      (newline expect-port)))))))
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
	  (display "/mnt/sources/init-instroot/init-instroot.scm -r /dev/vda -s 100M" expect-port)
	  (newline expect-port)))
	(while
	    ;; When the drive devices are reused between runs an additional confirmation is required
	    ;; during partitioning, which requires an optional check.
	    (expect
	     ((matcher "Proceed anyway\\? \\(y,N\\)")
	      (display "y" expect-port)
	      (newline expect-port)
	      #t)
	     ((matcher "Are you sure\\? \\(Type uppercase yes\\): ")
	      (display "YES" expect-port)
	      (newline expect-port)
	      #f)))
	(expect
	 ((matcher (string-append "Enter passphrase for " root-dev "[^:]*: "))
	  (display luks-passhprase expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Verify passphrase:")
	  (display luks-passhprase expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher (string-append "Enter passphrase for " root-dev "[^:]*: "))
	  (display luks-passhprase expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Would you like to overwrite LUKS device with random data\\? \\[y/N\\]")
	  (newline expect-port)))
	(when (not use-network?)
	  (expect
	   ((matcher "# ")
	    (display
	     "mkdir -p /mnt/instroot/var/spool/apt-mirror"
	     expect-port)
	    (newline expect-port)))
	  (expect
	   ((matcher "# ")
	    (display
	     "mount --bind /var/spool/apt-mirror /mnt/instroot/var/spool/apt-mirror"
	     expect-port)
	    (newline expect-port))))
	(expect
	 ((matcher "# ")
	  (display
	   (string-join
	    `("/mnt/sources/debian-setup/install.scm"
	      "-n" ,hostname "-s" ,sudo-username
	      ,@(if (not use-network?)
		    (list "-m" "file:///var/spool/apt-mirror/mirror/deb.debian.org/debian/")
		    #nil))
	    " ")
	   expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Please type \"Hello#123\" here: ")
	  (display "Hello#123" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "New password: ")
	  (display sudo-password expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Retype new password: ")
	  (display sudo-password expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "Remove configuration script and temporary files\\? \\[y/N\\]")
	  (newline expect-port)))
	(expect
	 ((matcher "Ready to finish installation and reboot the system\\? \\[Y/n\\]")
	  (newline expect-port))))
	  (lambda ()
	    (popen:close-pipe expect-port)
	    (utils:println "Terminated QEMU process!"))))))))


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
