# Runs Linkding behind Caddy with SQLite and Pocket ID authentication.
# The Pocket ID client's callback URL is https://<domain>/oidc/callback/ (with
# the trailing slash). linkding-env.age holds OIDC_RP_CLIENT_ID and
# OIDC_RP_CLIENT_SECRET. Run `linkding-manage createsuperuser` with the email
# Pocket ID returns before the first OIDC login; OIDC does not grant admin.
# Back up /var/lib/linkding.
{ config, lib, ... }:
let
  cfg = config.belak.services.linkding;
  issuer = "https://pocket-id.elwert.cloud";
in
{
  options.belak.services.linkding = {
    enable = lib.mkEnableOption "Linkding";

    domain = lib.mkOption {
      type = lib.types.str;
      default = "bookmarks.elwert.cloud";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 9090;
    };
  };

  config = lib.mkIf cfg.enable {
    services.linkding = {
      enable = true;
      address = "127.0.0.1";
      port = cfg.port;
      database.type = "sqlite";
      environmentFile = config.age.secrets.linkding-env.path;
      settings = {
        LD_CSRF_TRUSTED_ORIGINS = "https://${cfg.domain}";
        LD_ENABLE_OIDC = "True";
        OIDC_OP_AUTHORIZATION_ENDPOINT = "${issuer}/authorize";
        OIDC_OP_TOKEN_ENDPOINT = "${issuer}/api/oidc/token";
        OIDC_OP_USER_ENDPOINT = "${issuer}/api/oidc/userinfo";
        OIDC_OP_JWKS_ENDPOINT = "${issuer}/.well-known/jwks.json";
        OIDC_RP_SIGN_ALGO = "RS256";
      };
    };

    age.secrets.linkding-env.file = ../../../../secrets/linkding-env.age;

    systemd.services =
      lib.genAttrs
        [
          "linkding"
          "linkding-setup"
          "linkding-background-tasks"
        ]
        (_: {
          restartTriggers = [
            (builtins.hashFile "sha256" config.age.secrets.linkding-env.file)
          ];
        });

    belak.services.caddy.virtualHosts.${cfg.domain}.backend = "http://127.0.0.1:${toString cfg.port}";
  };
}
