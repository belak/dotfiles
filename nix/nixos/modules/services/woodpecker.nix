{ config, lib, ... }:
let
  cfg = config.belak.services.woodpecker;
in
{
  options.belak.services.woodpecker = {
    enable = lib.mkEnableOption "woodpecker CI server";

    domain = lib.mkOption { default = "woodpecker.elwert.cloud"; };
    forgejoUrl = lib.mkOption { default = "https://forgejo.elwert.cloud"; };

    admins = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Forgejo usernames granted Woodpecker instance-admin rights";
    };

    open = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Allow any Forgejo user to log in and register with Woodpecker";
    };

    repoOwners = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Forgejo users/orgs whose repos may be synced into Woodpecker";
    };
  };

  config = lib.mkIf cfg.enable {
    services.woodpecker-server = {
      enable = true;

      environment = {
        WOODPECKER_HOST = "https://${cfg.domain}";
        WOODPECKER_SERVER_ADDR = "127.0.0.1:8000";
        # Agents may run on other hosts, so the gRPC port needs to be
        # reachable over the network rather than just localhost.
        WOODPECKER_GRPC_ADDR = "0.0.0.0:9000";
        WOODPECKER_GITEA = "true";
        WOODPECKER_GITEA_URL = cfg.forgejoUrl;
        WOODPECKER_ADMIN = builtins.concatStringsSep "," cfg.admins;
        WOODPECKER_OPEN = lib.boolToString cfg.open;
        WOODPECKER_REPO_OWNERS = builtins.concatStringsSep "," cfg.repoOwners;
      };

      environmentFile = [
        config.age.secrets.woodpecker-agent-secret.path
        config.age.secrets.woodpecker-forgejo-client-id.path
        config.age.secrets.woodpecker-forgejo-client-secret.path
      ];
    };

    # EnvironmentFile points at a stable /run/agenix path, so switch-to-configuration
    # won't restart the service on its own when a secret's content changes.
    #
    # The trigger hashes the file rather than pointing at it: inside a flake a
    # path is a subpath of the whole source tree, so its store hash moves on any
    # repo change, not just this secret.
    systemd.services.woodpecker-server.restartTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.woodpecker-agent-secret.file)
      (builtins.hashFile "sha256" config.age.secrets.woodpecker-forgejo-client-id.file)
      (builtins.hashFile "sha256" config.age.secrets.woodpecker-forgejo-client-secret.file)
    ];

    age.secrets = {
      woodpecker-agent-secret = {
        file = ../../../../secrets/woodpecker-agent-secret.age;
      };

      woodpecker-forgejo-client-id = {
        file = ../../../../secrets/woodpecker-forgejo-client-id.age;
      };

      woodpecker-forgejo-client-secret = {
        file = ../../../../secrets/woodpecker-forgejo-client-secret.age;
      };
    };

    services.nginx.virtualHosts."${cfg.domain}" = {
      locations."/".proxyPass = "http://127.0.0.1:8000";
    };

    # Agents on other hosts connect to the gRPC port over the internal
    # elwert.dev network.
    networking.firewall.allowedTCPPorts = [ 9000 ];
  };
}
