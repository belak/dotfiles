{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.belak.services.taskchampion;
in
{
  options.belak.services.taskchampion = {
    enable = lib.mkEnableOption "taskchampion sync server";
    domain = lib.mkOption {
      type = lib.types.str;
      default = "taskwarrior.elwert.cloud";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 10222;
    };
  };

  config = lib.mkIf cfg.enable {
    services.taskchampion-sync-server = {
      enable = true;
      port = cfg.port;
      # We intentionally do not use allowClientIds so we don't leak it in the public NixOS config.
      # Instead, we rely on Caddy basic auth to restrict access to the server.
    };

    belak.services.caddy.virtualHosts.${cfg.domain} = {
      backend = "http://127.0.0.1:${toString cfg.port}";
      # Caddy's access log records the authenticated user, which is how
      # devices are told apart.
      extraConfig = ''
        basic_auth {
          import ${config.age.secrets.taskchampion-basic-auth-accounts.path}
        }
      '';
    };

    # One "<device> <bcrypt hash>" line per account, with hashes from
    # `caddy hash-password`.
    age.secrets.taskchampion-basic-auth-accounts = {
      file = ../../../../secrets/taskchampion-basic-auth-accounts.age;
      owner = "caddy";
    };

    # Caddy reads the accounts file when it loads its config, so a change to
    # the secret only needs a reload. The trigger hashes the file rather than
    # pointing at it: inside a flake a path is a subpath of the whole source
    # tree, so its store hash moves on any repo change, not just this secret.
    systemd.services.caddy.reloadTriggers = [
      (builtins.hashFile "sha256" config.age.secrets.taskchampion-basic-auth-accounts.file)
    ];
  };
}
