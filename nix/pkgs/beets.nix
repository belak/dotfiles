{ python3Packages }:
# Beets with the third-party plugins this setup relies on. All builtin plugins
# are already enabled by the nixpkgs derivation, so this only wires up the ones
# that ship separately.
python3Packages.beets.override {
  pluginOverrides = {
    alternatives = {
      enable = true;
      propagatedBuildInputs = [ python3Packages.beets-alternatives ];
    };
    filetote = {
      enable = true;
      propagatedBuildInputs = [ python3Packages.beets-filetote ];
    };
  };
}
