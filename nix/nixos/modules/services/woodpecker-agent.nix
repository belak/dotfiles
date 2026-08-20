{ config, lib, ... }:
let
  cfg = config.belak.services.woodpecker-agent;

  # Named for the backend, the way upstream's own example does it, so that a
  # second agent on a different backend has an obvious name to take.
  agentName = "podman";
  stateName = "woodpecker-agent/${agentName}";
  stateDir = "/var/lib/${stateName}";
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
    services.woodpecker-agents.agents.${agentName} = {
      enable = true;

      environment = {
        WOODPECKER_SERVER = cfg.server;
        WOODPECKER_BACKEND = "docker";
        DOCKER_HOST = "unix:///run/podman/podman.sock";
        WOODPECKER_MAX_WORKFLOWS = builtins.toString cfg.maxWorkflows;

        # The UI only ever shows what the agent reports, and that defaults to
        # the short hostname, so we make it more specific.
        WOODPECKER_HOSTNAME = config.networking.fqdnOrHostName;

        # The agent id lives in this file. Without it the agent registers as a
        # new agent on every start, so an unclean disconnect leaves the old
        # entry behind in the UI forever.
        WOODPECKER_AGENT_CONFIG_FILE = "${stateDir}/agent.conf";
      };

      extraGroups = [ "podman" ];

      environmentFile = [ config.age.secrets.woodpecker-agent-secret.path ];
    };

    belak.services.podman.enable = true;

    # The upstream module runs the agent with DynamicUser and ProtectSystem =
    # strict, so the config file needs a writable directory of its own.
    systemd.services."woodpecker-agent-${agentName}" = {
      serviceConfig.StateDirectory = stateName;

      # EnvironmentFile points at a stable /run/agenix path, so
      # switch-to-configuration won't restart the service on its own when the
      # secret's content changes.
      restartTriggers = [ config.age.secrets.woodpecker-agent-secret.file ];
    };

    age.secrets.woodpecker-agent-secret = {
      file = ../../../../secrets/woodpecker-agent-secret.age;
    };
  };
}
