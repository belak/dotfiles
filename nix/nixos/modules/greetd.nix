{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.belak.greetd;
in
{
  options.belak.greetd = {
    enable = lib.mkEnableOption "greetd";

    command = lib.mkOption {
      type = lib.types.str;
      default = "niri-session";
      description = "Session command to launch after login.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings.default_session.command =
        "${pkgs.greetd.greetd}/bin/agreety --cmd ${cfg.command}";
    };
  };
}
