{
  config,
  lib,
  pkgs,
  buildbot-nix,
  ...
}:
let
  cfg = config.belak.services.buildbot-nix;
in
{
  imports = [
    buildbot-nix.nixosModules.buildbot-master
    buildbot-nix.nixosModules.buildbot-worker
  ];

  options.belak.services.buildbot-nix = {
    enable = lib.mkEnableOption "buildbot-nix master + worker";

    domain = lib.mkOption { default = "buildbot.elwert.cloud"; };

    forgejoDomain = lib.mkOption { default = "git.elwert.cloud"; };

    oidcDiscoveryUrl = lib.mkOption {
      type = lib.types.str;
      default = "https://pocket-id.elwert.cloud/.well-known/openid-configuration";
    };

    oidcClientId = lib.mkOption {
      type = lib.types.str;
      description = "pocket-id OIDC client ID for buildbot login.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.buildbot-nix.master = {
      enable = true;
      domain = cfg.domain;
      # TLS terminates at zidane, so the local vhost is plain HTTP
      # but the public URL is still https://.
      useHTTPS = true;
      admins = [ "belak" ];
      authBackend = "oidc";
      workersFile = config.age.secrets."buildbot-worker-${config.networking.hostName}".path;

      oidc = {
        name = "pocket-id";
        discoveryUrl = cfg.oidcDiscoveryUrl;
        clientId = cfg.oidcClientId;
        clientSecretFile = config.age.secrets.buildbot-oidc-secret.path;
        # Map a custom `roles` claim to buildbot group roles.
        # Configure pocket-id to emit `"roles": ["admin"]` for members
        # of the `buildbot_admins` group.
        mapping.groups = "roles";
      };

      gitea = {
        enable = true;
        instanceUrl = "https://${cfg.forgejoDomain}";
        tokenFile = config.age.secrets.buildbot-forgejo-token.path;
        webhookSecretFile = config.age.secrets.buildbot-forgejo-webhook-secret.path;
        topic = "build-with-buildbot";
      };
    };

    services.buildbot-nix.worker = {
      enable = true;
      workerPasswordFile = config.age.secrets.buildbot-worker-password.path;
    };

    # Push every new store path produced on this host to attic.
    systemd.services.attic-watch-store = {
      description = "Push new nix store paths to attic";
      wantedBy = [ "multi-user.target" ];
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      path = [ pkgs.attic-client ];
      environment.HOME = "/var/lib/attic-watch-store";
      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "attic-watch-store";
        LoadCredential = "token:${config.age.secrets.buildbot-attic-token.path}";
        Restart = "on-failure";
        RestartSec = 10;
      };
      script = ''
        set -eu
        ATTIC_TOKEN=$(< "$CREDENTIALS_DIRECTORY/token")
        attic login elwert https://attic.elwert.cloud "$ATTIC_TOKEN"
        attic use elwert:buildbot
        exec attic watch-store elwert:buildbot
      '';
    };

    age.secrets."buildbot-worker-${config.networking.hostName}" = {
      file = ../../../../secrets/buildbot-worker-${config.networking.hostName}.age;
      owner = "buildbot";
    };
    age.secrets.buildbot-worker-password = {
      file = ../../../../secrets/buildbot-worker-password.age;
      owner = "buildbot-worker";
    };
    age.secrets.buildbot-forgejo-token = {
      file = ../../../../secrets/buildbot-forgejo-token.age;
      owner = "buildbot";
    };
    age.secrets.buildbot-forgejo-webhook-secret = {
      file = ../../../../secrets/buildbot-forgejo-webhook-secret.age;
      owner = "buildbot";
    };
    age.secrets.buildbot-oidc-secret = {
      file = ../../../../secrets/buildbot-oidc-secret.age;
      owner = "buildbot";
    };
    age.secrets.buildbot-attic-token = {
      file = ../../../../secrets/buildbot-attic-token.age;
    };
  };
}
