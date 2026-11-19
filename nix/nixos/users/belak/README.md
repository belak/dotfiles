# belak user

The login password is not in this repo. `hashedPasswordFile` points at
`/etc/secrets/belak-password`, which has to exist on each machine before
the first boot or the user is locked out (`users.mutableUsers` is false).

To set or change the password, as root:

``` shell
install -d -m 0700 /etc/secrets
mkpasswd -m yescrypt > /etc/secrets/belak-password
chmod 0600 /etc/secrets/belak-password
```

On a fresh install, run the same commands against the target from the
installer (`nixos-enter`, or prefix the paths with `/mnt`) before
rebooting. Changes take effect on the next `nixos-rebuild switch`.
