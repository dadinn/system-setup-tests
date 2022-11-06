(("guest"
  ("os" . "debian")
  ("release" . "stretch")
  ("username" . "user")
  ("password" . "live")
  ("drives"
   (("name" . "luks")
    ("size" . "4G")
    ("interface" . "virtio"))))
 ("instroot"
  ("rootdev" . "/dev/disk/by-id/virtio-luks")
  ("luks-label" . "crypt_root")
  ("swapsize" . "100M")
  ("passphrase" . "password"))
 ("install"
  ("os" . "debian")
  ("release" . "stretch")
  ("hostname" . "besenczy")
  ("sudouser" . "testuser")
  ("password" . "password")))
("guest"
 ("os" . "debian")
 ("release" . "stretch")
 ("username" . "user")
 ("password" . "live")
 ("drives"
  (("name" . "luks")
   ("size" . "4G")
   ("interface" . "virtio"))))
("instroot"
 ("rootdev" . "/dev/disk/by-id/virtio-luks")
 ("luks-label" . "crypt_root")
 ("swapsize" . "100M")
 ("passphrase" . "password01"))
("install"
 ("os" . "debian")
 ("release" . "stretch")
 ("hostname" . "besenczy")
 ("sudouser" . "testuser")
 ("password" . "password02"))
