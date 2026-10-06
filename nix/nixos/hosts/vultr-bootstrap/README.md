# Vultr bootstrap image
Or: A new instance awakens

Build the secret-free image on an x86-64 Linux machine:

```fish
nix build .#vultr-bootstrap-image
```

The output is `result/nixos-vultr.img`, a raw UEFI disk image sized for
its contents. Building requires an x86-64 Linux builder; it cannot run
natively on macOS. The reusable [Vultr profile](../../profiles/vultr.nix)
provides VirtIO support, serial console access, and automatic root disk
expansion.

Root SSH accepts the public keys in `secrets/keys.nix`'s `users` list at
build time. Password login is disabled, and SSH host keys are generated
on boot. The image contains no agenix secrets, blog, or mail services.
It uses DHCP without cloud-init, so Vultr-selected SSH keys, passwords,
and user-data are not applied.

Host the image at a URL Vultr can download, then import it as a UEFI
snapshot:

```fish
vultr-cli snapshot create-url --url https://example.com/nixos-vultr.img --uefi
```

Launch an instance from the snapshot with a disk at least as large as
the image and inbound TCP port 22 allowed. Remove the hosted copy after
Vultr finishes importing it. Import the Vultr profile in the eventual
host configuration and preserve the `nixos` root and `ESP` boot
filesystem labels. Do not run Disko or repartition the instance when
deploying that configuration. The bootstrap profile has no swap;
configure it in the eventual host if needed.

Use this image only for new instances. Update an existing instance
through normal NixOS deployments, not by replacing its disk image.
