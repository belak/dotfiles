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
  deploy-rs-pkg-override = final: prev: {
    deploy-rs = prev.deploy-rs // {
      deploy-rs = nixpkgs-unstable.legacyPackages.${final.system}.deploy-rs;
    };
  };

  unstable = final: _prev: { unstable = self.lib.mkPkgs final.system nixpkgs-unstable [ ]; };
}
