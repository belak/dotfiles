{
  self,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.services.buildbot-worker;
in
{
  options.belak.services.buildbot-worker = {
    enable = lib.mkEnableOption "buildbot-worker";
  };
  config = lib.mkIf cfg.enable {
    services.buildbot-nix.worker = {
      enable = true;
      masterUrl = "tcp:host=artemicion.elwert.dev:port=9989";
      workerPasswordFile = config.age.secrets.buildbot-worker-password.path;
    };

    age.secrets.buildbot-worker-password.file = ../../../../secrets/buildbot-worker-${config.networking.hostName}.age;
  };
}
