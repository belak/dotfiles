{
  nixpkgs-unstable,
  agenix,
  claude-code,
  deploy-rs,
  emacs-overlay,
  nix-vscode-extensions,
  belak-blog,
  ...
}:
{
  additions = final: _prev: {
    my = final.lib.packagesFromDirectoryRecursive {
      callPackage = final.callPackage;
      directory = ./pkgs;
    };

    belak-blog = belak-blog.packages.${final.stdenv.hostPlatform.system}.default;
  };

  agenix = agenix.overlays.default;

  claude-code = claude-code.overlays.default;

  deploy-rs-overlay = deploy-rs.overlays.default;

  emacs = emacs-overlay.overlays.default;

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

  unstable = final: _prev: {
    unstable = import nixpkgs-unstable {
      inherit (final) config;
      inherit (final.stdenv.hostPlatform) system;
    };
  };

  vscode = final: _prev: {
    community-vscode-extensions = nix-vscode-extensions.extensions.${final.stdenv.hostPlatform.system};
  };

  # NixOS/nixpkgs#507531 - direnv test-fish gets Killed: 9 on darwin after
  # libarchive 3.8.6 update.
  direnv-darwin-fix = _final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
    direnv = prev.direnv.overrideAttrs (_: { doCheck = false; });
  };
}
