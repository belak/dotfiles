{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.dev;
in
{
  options.belak.dev = {
    enable = lib.mkEnableOption "devtools";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # Various tools
      android-tools
      cloc
      crane
      dos2unix
      editorconfig-core-c
      exiftool
      gnumake
      imagemagick
      picotool
      pkg-config
      protobuf

      # Python
      python311
      virtualenv
      my.pyenv
      my.pyenv-virtualenv

      # Ruby
      my.rbenv
      my.ruby-build
    ];
  };
}
