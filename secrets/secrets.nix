let
  keys = import ./keys.nix;
  inherit (keys) users systems;

  service-atticd = [ keys.system-quina ];
  service-forgejo = [ keys.system-baku ];
  service-immich = [ keys.system-freya ];
  service-kavita = [ keys.system-freya ];
  service-miniflux = [ keys.system-freya ];
  service-pocket-id = [ keys.system-zidane ];
  service-syncthing = [ keys.system-freya ];
  service-woodpecker = [ keys.system-baku ];
  service-woodpecker-agent = [ keys.system-quina ];
in
{
  "atticd-env.age".publicKeys = service-atticd ++ users;

  "acme-cloudflare-env.age".publicKeys = users ++ [ keys.system-zidane ];
  "belak-password.age".publicKeys = users ++ systems;

  "forgejo-oidc-client-id.age".publicKeys = service-forgejo ++ users;
  "forgejo-oidc-client-secret.age".publicKeys = service-forgejo ++ users;
  "forgejo-smtp-password.age".publicKeys = service-forgejo ++ users;

  "immich-smtp-password.age".publicKeys = service-immich ++ users;
  "immich-oidc-client-id.age".publicKeys = service-immich ++ users;
  "immich-oidc-client-secret.age".publicKeys = service-immich ++ users;

  "kavita-token-key.age".publicKeys = service-kavita ++ users;
  "kavita-oidc-client-secret.age".publicKeys = service-kavita ++ users;

  "miniflux-admin-credentials.age".publicKeys = service-miniflux ++ users;
  "miniflux-oidc-client-id.age".publicKeys = service-miniflux ++ users;
  "miniflux-oidc-client-secret.age".publicKeys = service-miniflux ++ users;

  "pocket-id-encryption-key.age".publicKeys = service-pocket-id ++ users;
  "pocket-id-smtp-password.age".publicKeys = service-pocket-id ++ users;

  "syncthing-gui-password.age".publicKeys = service-syncthing ++ users;

  "woodpecker-agent-secret.age".publicKeys = service-woodpecker ++ service-woodpecker-agent ++ users;
  "woodpecker-forgejo-client-id.age".publicKeys = service-woodpecker ++ users;
  "woodpecker-forgejo-client-secret.age".publicKeys = service-woodpecker ++ users;
}
