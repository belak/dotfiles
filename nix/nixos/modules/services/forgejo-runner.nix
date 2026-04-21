{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.services.forgejo-runner;
in
{
  options.belak.services.forgejo-runner = {
    enable = lib.mkEnableOption "forgejo actions runner";

    url = lib.mkOption {
      type = lib.types.str;
      default = "https://git.elwert.cloud";
    };
  };

  config = lib.mkIf cfg.enable {
    services.gitea-actions-runner = {
      package = pkgs.forgejo-runner;
      instances.main = {
        enable = true;
        name = config.networking.hostName;
        url = cfg.url;
        tokenFile = config.age.secrets.forgejo-runner-token.path;
        labels = [
          "ubuntu-latest:docker://node:22-bookworm"
          "nix:docker://nixos/nix"
        ];
      };
    };

    # When the runner and Forgejo are on the same host, ensure the runner
    # starts after Forgejo to avoid 502s during registration.
    systemd.services."gitea-runner-main" = lib.mkIf config.belak.services.forgejo.enable {
      after = [ "forgejo.service" ];
      wants = [ "forgejo.service" ];
    };

    virtualisation.podman.enable = true;

    age.secrets.forgejo-runner-token = {
      file = ../../../../secrets/forgejo-runner-token.age;
      owner = "gitea-runner";
    };
  };
}
