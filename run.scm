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
     (predicate ,(lambda (path) (file-exists? path))))
    (sources
     (description
      "Path to sources directory containing init-instroot and bootstrap scripts (defaults to parent directory)")
     (value #t)
     (value-arg "PATH")
     (predicate
      ,(lambda (path) (utils:directory? path)))
     ;;(default ,(dirname (dirname (current-filename))))
     )
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
      (system*
       "virt-install"
       "--connect" "qemu:///system"
       ;; this is not working with forced poweroff
       ;;"--events" "on_poweroff=destroy"
       "--name" name
       "--description" "testing system installation"
       "--ram" "4096"
       "--graphics" "none"
       "--vcpu" "2"


       ;;"--os-type" "linux"
       ;;"--os-variant" "Debian10"
       "--location" "/home/dadinn/Downloads/isos/debian-live-10.3.0-amd64-standard.iso"
       ;;"--location" "http://ftp.us.debian.org/debian/dists/buster/main/installer-amd64/"
       "--extra-args" "console=tty0,console=ttyS0,115200n8,serial"
       ;;"--extra-args" "console=ttyS0"
       "--disk" (string-append "path=disks/" name ".img,bus=virtio,size=4")
       "--filesystem" (string-append sources-path ",sources")
       "--debug"
					;"--boot" "uefi"
       )))))
