{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.belak.services.forgejo-runner;

  # Custom image: nix + node in one place so actions/checkout (JS) and
  # nix builds can coexist in the same job.
  nixCiImage = pkgs.dockerTools.buildLayeredImage {
    name = "localhost/nix-ci";
    tag = "latest";
    contents = with pkgs; [
      nix
      nodejs
      attic-client
      bashInteractive
      coreutils
      gnutar
      gzip
      gitMinimal
      cacert
      (dockerTools.fakeNss.override {
        extraPasswdLines =
          [ "ci:x:1001:1001:CI user:/home/ci:/bin/bash" ]
          ++ map (
            n: "nixbld${toString n}:x:${toString (30000 + n)}:30000:Nix build user ${toString n}:/var/empty:/run/current-system/sw/bin/nologin"
          ) (lib.range 1 32);
        extraGroupLines = [
          "ci:!:1001:"
          "nixbld:!:30000:${
            lib.concatStringsSep "," (map (n: "nixbld${toString n}") (lib.range 1 32))
          }"
        ];
      })
      (writeTextDir "etc/nix/nix.conf" ''
        experimental-features = nix-command flakes
        build-users-group = nixbld
        sandbox = true
      '')
      (writeTextDir "home/ci/.keep" "")
    ];
    # Jobs run as non-root `ci` user so tools that refuse root
    # (initdb, elasticsearch, etc.) work out of the box.
    fakeRootCommands = ''
      chown -R 1001:1001 home/ci
      mkdir -p tmp
      chmod 1777 tmp
    '';
    enableFakechroot = true;
    config.User = "ci";
    config.Env = [
      "PATH=/bin"
      "USER=ci"
      "HOME=/home/ci"
      "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
  };
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
          "nix:docker://localhost/nix-ci:latest"
        ];
        # Give nix's user-namespace sandbox what it needs inside jobs.
        settings.container.options = "--privileged";
      };
    };

    # Static user so agenix can chown the token secret. Upstream module
    # defaults to DynamicUser, which doesn't exist at activation time.
    users.users.gitea-runner = {
      isSystemUser = true;
      group = "gitea-runner";
    };
    users.groups.gitea-runner = { };

    systemd.services."gitea-runner-main" = {
      # Runner needs node on PATH so act can bind-mount it into job
      # containers for JS-based actions.
      path = [ pkgs.nodejs ];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "gitea-runner";
        Group = "gitea-runner";
      };
    } // lib.optionalAttrs config.belak.services.forgejo.enable {
      # When the runner and Forgejo are on the same host, ensure the runner
      # starts after Forgejo to avoid 502s during registration.
      after = [ "forgejo.service" ];
      wants = [ "forgejo.service" ];
    };

    virtualisation.podman.enable = true;

    # Load the nix-ci image into the system (rootful) podman storage
    # that the runner talks to via /run/podman/podman.sock.
    systemd.services.load-nix-ci-image = {
      description = "Load nix-ci container image into rootful podman storage";
      wantedBy = [ "gitea-runner-main.service" ];
      before = [ "gitea-runner-main.service" ];
      after = [ "podman.service" ];
      requires = [ "podman.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.podman}/bin/podman load -i ${nixCiImage}";
      };
    };

    age.secrets.forgejo-runner-token = {
      file = ../../../../secrets/forgejo-runner-token.age;
      owner = "gitea-runner";
    };
  };
}
