{ config, lib, ... }:
let
  cfg = config.belak.services.woodpecker-agent;
in
{
  options.belak.services.woodpecker-agent = {
    enable = lib.mkEnableOption "woodpecker CI agent";

    server = lib.mkOption {
      type = lib.types.str;
      default = "vivi.elwert.dev:9000";
      description = "host:port of the woodpecker-server gRPC endpoint";
    };

    maxWorkflows = lib.mkOption {
      type = lib.types.ints.positive;
      default = 1;
      description = "Number of workflows this agent can run in parallel";
    };
  };

  config = lib.mkIf cfg.enable {
    services.woodpecker-agents.agents.podman = {
      enable = true;

      environment = {
        WOODPECKER_SERVER = cfg.server;
        WOODPECKER_BACKEND = "docker";
        DOCKER_HOST = "unix:///run/podman/podman.sock";
        WOODPECKER_MAX_WORKFLOWS = builtins.toString cfg.maxWorkflows;
      };

      extraGroups = [ "podman" ];

      environmentFile = [ config.age.secrets.woodpecker-agent-secret.path ];
    };

    belak.services.podman.enable = true;

    # EnvironmentFile points at a stable /run/agenix path, so switch-to-configuration
    # won't restart the service on its own when the secret's content changes.
    systemd.services.woodpecker-agent-podman.restartTriggers = [
      config.age.secrets.woodpecker-agent-secret.file
    ];

    age.secrets.woodpecker-agent-secret = {
      file = ../../../../secrets/woodpecker-agent-secret.age;
    };
  };
}
