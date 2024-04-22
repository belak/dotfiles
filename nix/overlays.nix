{
  self,
  nixpkgs-unstable,
  deploy-rs,
  ...
}:
{
  additions = final: _prev: { my = import ./pkgs { pkgs = final; }; };

  #deploy-rs = deploy-rs.overlays.default;

  unstable = final: _prev: { unstable = self.lib.mkPkgs final.system nixpkgs-unstable [ ]; };
}
