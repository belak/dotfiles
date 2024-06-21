{
  self,
  nixpkgs-unstable,
  deploy-rs,
  ...
}:
{
  additions = final: _prev: { my = import ./pkgs { pkgs = final; }; };

  deploy-rs-overlay = deploy-rs.overlays.default;

  # This is a modified version of what's in the deploy-rs readme. For some
  # reason overriding deploy-rs entirely seems to clobber the needed `lib` attr.
  # Note that this depends on our "unstable" overlay.
  deploy-rs-pkg-override = final: prev: {
    deploy-rs = final.unstable.deploy-rs // {
      inherit (prev.deploy-rs) lib;
    };
  };

  unstable = final: _prev: { unstable = self.lib.mkPkgs final.system nixpkgs-unstable [ ]; };
}
