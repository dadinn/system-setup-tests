(("guest"
  ("os" . "debian")
  ("release" . "bullseye")
  ("username" . "user")
  ("password" . "live")
  ("drives"
   (("name" . "main")
    ("size" . "4G")
    ("interface" . "virtio")))
  ("uefi" . #t))
 ("instroot"
  ("rootdev" . "/dev/disk/by-id/virtio-main")
  ("luks-label" . "crypt_root")
  ("swapsize" . "100M")
  ("passphrase" . "password01")
  ("uefi" . #t))
 ("install"
  ("os" . "debian")
  ("release" . "bullseye")
  ("hostname" . "besenczy")
  ("sudouser" . "testuser")
  ("password" . "password02")))
