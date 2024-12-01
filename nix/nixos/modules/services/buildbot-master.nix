{
  self,
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.services.buildbot-master;
in
{
  options.belak.services.buildbot-master = {
    enable = lib.mkEnableOption "buildbot-master";
    domain = lib.mkOption { default = "ci.seabird.chat"; };
  };

  config = lib.mkIf cfg.enable {
    services.buildbot-nix.master = {
      enable = true;
      domain = cfg.domain;
      useHTTPS = true;

      workersFile = config.age.secrets.buildbot-workers.path;

      github = {
        authType.app = {
          id = 1074372;
          secretKeyFile = config.age.secrets.buildbot-app-secret.path;
        };
        webhookSecretFile = config.age.secrets.buildbot-webhook-secret.path;
        oauthId = "Iv23lirtHDIAmyTbzgzh";
        oauthSecretFile = config.age.secrets.buildbot-oauth-secret.path;
        topic = "buildbot-seabird";
      };
    };

    networking.firewall.allowedTCPPorts = [
      9989
    ];

    age.secrets.buildbot-workers.file = ../../../../secrets/buildbot-workers.age;
    age.secrets.buildbot-app-secret.file = ../../../../secrets/buildbot-app-secret.age;
    age.secrets.buildbot-webhook-secret.file = ../../../../secrets/buildbot-webhook-secret.age;
    age.secrets.buildbot-oauth-secret.file = ../../../../secrets/buildbot-oauth-secret.age;
  };
}
