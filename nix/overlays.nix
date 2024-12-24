{
  nixpkgs-unstable,
  agenix,
  deploy-rs,
  nix-vscode-extensions,
  ...
}:
{
  additions = final: _prev: {
    my = final.lib.packagesFromDirectoryRecursive {
      callPackage = final.callPackage;
      directory = ./pkgs;
    };
  };

  agenix = final: _prev: { agenix = agenix.packages.${final.system}.default; };

  deploy-rs-overlay = deploy-rs.overlays.default;

  # This is a modified version of what's in the deploy-rs readme. For some
  # reason overriding deploy-rs entirely seems to clobber the needed `lib` attr,
  # and oddly enough the `deploy-rs` binary is accessed through
  # deploy-rs.reploy-rs, so we need to provide that as well.
  #
  # Note that this depends on our "unstable" overlay because it's the easiest
  # way to make sure we get a package from nixpkgs and not from the deploy-rs
  # source. Another option would be creating a new nixpkgs instance.
  deploy-rs-pkg-override = final: prev: {
    deploy-rs = final.unstable.deploy-rs // {
      inherit (final.unstable) deploy-rs;
      inherit (prev.deploy-rs) lib;
    };
  };

  unstable = final: _prev: { unstable = import nixpkgs-unstable { inherit (final) config system; }; };

  vscode = final: _prev: { community-vscode-extensions = nix-vscode-extensions.extensions.${final.system}; };
}
