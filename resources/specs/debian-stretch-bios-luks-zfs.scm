(("guest"
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
  ("passphrase" . "password01"))
 ("install"
  ("os" . "debian")
  ("release" . "stretch")
  ("hostname" . "besenczy")
  ("sudouser" . "testuser")
  ("password" . "password02")))
