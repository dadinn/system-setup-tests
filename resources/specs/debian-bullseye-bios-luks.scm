(("guest"
  ("distro" . "debian")
  ("release" . "bullseye")
  ("username" . "user")
  ("password" . "live")
  ("drives"
   (("name" . "main")
    ("size" . "4G")
    ("interface" . "virtio"))))
 ("instroot"
  ("rootdev" . "/dev/disk/by-id/virtio-main")
  ("luks-label" . "crypt_root")
  ("swapsize" . "100M")
  ("passphrase" . "password01"))
 ("install"
  ("distro" . "debian")
  ("release" . "bullseye")
  ("hostname" . "besenczy")
  ("sudouser" . "testuser")
  ("password" . "password02")))
