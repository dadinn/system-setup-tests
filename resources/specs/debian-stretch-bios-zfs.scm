(("guest"
  ("distro" . "debian")
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
  ("distro" . "debian")
  ("release" . "stretch")
  ("hostname" . "testing")
  ("sudouser" . "testuser")
  ("password" . "password")))
