#!/usr/bin/env sh
# -*- scheme -*-
exec guile -e main -s "$0" "$@"
!#

(add-to-load-path
 (dirname (current-filename)))

(use-modules
 ((common utils) #:prefix utils:)
 ((ice-9 popen) #:prefix popen:)
 ((ice-9 regex) #:prefix regex:)
 ((ice-9 rdelim) #:prefix rdelim:)
 ((ice-9 expect)))

(define-macro (comment . args)
  `(if #f #f))

(define (matcher pattern)
  (lambda (s eof?)
    (if (not eof?)
	;;This is needed to support matching against output with null characters
	(let ((stuff (string-filter (lambda (c) (not (eqv? c #\nul))) s)))
	  (regex:string-match pattern stuff))
	#f)))

(define* (run-qemu #:key name memory cdrom sources)
  (let ((port
	 (popen:open-pipe*
	  OPEN_BOTH
	  "qemu-system-x86_64"
	  "-enable-kvm"
	  "-serial" "stdio"
	  "-display" "none"
	  "-m" (or memory "4096")
	  ;;"-smbios" "uefi=on")
	  "-cdrom" cdrom
	  "-virtfs"
	  (utils:emit-arg-alist
	   `("local" "readonly"
	     ("path" . ,sources)
	     ("mount_tag" . "sources")
	     ("security_model" . "passthrough")))
	  "-drive"
	  (utils:emit-arg-alist
	   `(("file" . ,(utils:path "disks" (string-append name ".img")))
	     ("format" . "qcow2")
	     ("if" . "virtio")
	     ("media" . "disk"))))))
    (setvbuf port 'none)
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
      "Path to cdrom ISO file")
     (value #t)
     (value-arg "PATH")
     (predicate ,(lambda (path) (file-exists? path)))
     (default
       ;; ArchLinux Syslinux bootloader is configured to write to serial port,
       ;; unlike other distro (like Debian)
       "/home/dadinn/Downloads/isos/archlinux-2020.01.01-x86_64.iso"))
    (sources
     (description
      "Path to sources directory containing init-instroot and bootstrap scripts (defaults to parent directory)")
     (value #t)
     (value-arg "PATH")
     (predicate
      ,(lambda (path) (utils:directory? path)))
     (default ,(dirname (dirname (current-filename)))))
    (help
     (single-char #\h)
     (description
      "This usage help..."))))

(define (main args)
  (let* ((options (utils:getopt-extra args options-spec))
	 (name (hash-ref options 'name))
	 (cdrom-path (hash-ref options 'cdrom))
	 (sources-path (hash-ref options 'sources))
	 (sources-path (or sources-path (dirname (dirname (current-filename)))))
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
     ;; ((not cdrom-path) (error "cdrom ISO image must be specified!"))
     (else
      (when (not (file-exists? "disks"))
	(mkdir "disks"))
      (system* "qemu-img" "create" "-f" "qcow2" (string-append "disks/" name ".img") "4G")
      (let ((expect-char-proc
	     (lambda (c) (display c)))
	    (expect-port
	     (run-qemu
	      #:name name
	      #:memory "4096"
	      #:cdrom cdrom-path
	      #:sources sources-path)))
	(expect-strings
	 ("Press \\[Tab\\] to edit options"
	  (display "\t console=ttyS0" expect-port)
	  (newline expect-port)))
	(expect-strings
	 ("archiso login:"
	  (display "root" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "export LC_ALL=C" expect-port)
	  (newline expect-port)
	  (display "mkdir /mnt/sources" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "mount -t 9p -o ro sources /mnt/sources" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "pacman -Syy guile --noconfirm" expect-port)
	  (newline expect-port)))
	(expect
	 ((matcher "# ")
	  (display "LC_ALL=C lsblk" expect-port)
	  (newline expect-port)
	  (display "ls -la /mnt/sources/" expect-port)
	  (newline expect-port))))))))


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
