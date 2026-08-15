let
  keys = import ./keys.nix;
  inherit (keys) users systems;

  # atticd, forgejo, and the woodpecker server are moving to vivi. baku keeps
  # access until it is decommissioned, so it can still be deployed to during
  # the cutover. quina stays on as the arm64 woodpecker agent and needs nothing
  # but the agent secret.
  service-atticd = [ keys.system-vivi ];
  service-forgejo = [
    keys.system-vivi
    keys.system-baku
  ];
  service-immich = [ keys.system-freya ];
  service-kavita = [ keys.system-freya ];
  service-miniflux = [ keys.system-freya ];
  service-pocket-id = [ keys.system-zidane ];
  service-syncthing = [ keys.system-freya ];
  service-woodpecker = [
    keys.system-vivi
    keys.system-baku
  ];
  service-woodpecker-agent = [
    keys.system-vivi
    keys.system-quina
  ];

  # The agent secret is shared between the server and its agents, and vivi runs
  # both, so it is listed once here rather than as the union of the two service
  # lists above. Duplicate recipients are a warning from age, not an error, but
  # they do write a redundant stanza.
  service-woodpecker-any = [
    keys.system-vivi
    keys.system-baku
    keys.system-quina
  ];
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

  "woodpecker-agent-secret.age".publicKeys = service-woodpecker-any ++ users;
  "woodpecker-forgejo-client-id.age".publicKeys = service-woodpecker ++ users;
  "woodpecker-forgejo-client-secret.age".publicKeys = service-woodpecker ++ users;
}
