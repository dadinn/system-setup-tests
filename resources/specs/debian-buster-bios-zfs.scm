(("guest"
  ("network" . #t)
  ("distro" . "debian")
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
  ("passphrase" . "password01"))
 ("install"
  ("distro" . "debian")
  ("release" . "buster")
  ("hostname" . "besenczy")
  ("sudouser" . "testuser")
  ("password" . "password02")))
