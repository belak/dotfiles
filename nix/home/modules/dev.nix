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
    # Anything language- or project-specific belongs in a devshell rather than
    # here. This list is for tools worth having on PATH in every checkout.
    home.packages = with pkgs; [
      dos2unix
      editorconfig-core-c
      fswatch
      gnumake
      grpcurl
      just
      sops
      woodpecker-cli

      # Nix
      nix-update

      # Python
      ruff
      uv
    ];
  };
}
