{ self, nixpkgs-unstable, ... }:
{
  additions = final: _prev: { my = import ./pkgs { pkgs = final; }; };

  unstable = final: _prev: { unstable = self.lib.mkPkgs final.system nixpkgs-unstable [ ]; };
}
