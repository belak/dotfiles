# belak's dotfiles

## Nix

For setup, run the following:

``` shell
# NixOS setup
sudo ln -sf ~/.dotfiles/nix/flake.nix /etc/nixos/flake.nix
sudo nixos-rebuild switch --flake ~/.dotfiles

# nix-darwin setup
nix run nix-darwin -- switch --flake ~/.dotfiles

# home-manager setup
nix run github:nix-community/home-manager -- switch --flake ~/.dotfiles
```

### Vultr bootstrap snapshot

Build the minimal x86-64 image on an x86-64 Linux builder:

``` shell
nix build .#nixosConfigurations.vultr-bootstrap.config.system.build.image
```

The output is `result/nixos-vultr.img`, a raw UEFI disk image. Building it
requires Linux; macOS needs an x86-64 Linux remote builder. The image is sized
for its contents, and the root partition and filesystem grow to fill the
instance's disk on boot.

1. Host `nixos-vultr.img` at a URL Vultr can download without authentication.
2. In Vultr's Snapshots page, import that URL with UEFI enabled. With
   `vultr-cli`, use:

   ``` shell
   vultr-cli snapshot create-url --url https://example.com/nixos-vultr.img --uefi
   ```

3. Deploy an instance from the snapshot with a disk at least as large as the
   image. Allow inbound TCP port 22 in any Vultr firewall attached to it.
4. Connect with `ssh root@INSTANCE_IP` using a key listed in `secrets/keys.nix`.

The image uses DHCP and allows only key-based SSH login. It includes the public
keys in `secrets/keys.nix`'s `users` list, not private keys or agenix secrets.
SSH host keys and the machine ID are generated on boot. Cloud-init is not
included, so Vultr-selected SSH keys, passwords, user-data, and startup scripts
are not applied.

After connecting, deploy a host-specific NixOS configuration. Preserve the QEMU
guest support, UEFI boot, and the `nixos` root / `ESP` boot filesystem labels
unless deliberately changing the disk layout. Remove the hosted image after
Vultr finishes importing it.

See [Vultr's snapshot import requirements][vultr-snapshot-import] for platform
limits and the upload workflow.

[vultr-snapshot-import]: https://docs.vultr.com/best-practices-when-migrating-to-vultr

### Concepts

Hosts are configured with a user-level nix target (`home-manager`) and often a
host-level nix target (`nix-darwin`, `nixos`, etc).

In general, packages should be installed at the user-level unless they need to
be installed at a system level to work properly. This includes things like
system daemons, programs needed to set up the system, and shells.

### Work Computers

Note that the Nix flake in this repo is also used as a dependency in a private
work flake. This allows me to keep most common configuration here, in modules,
and have any work-specific config in a private repo.
