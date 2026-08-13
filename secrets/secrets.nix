let
  user-belak-beatrix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6gzteIYFVEhBmz+bA8SqIPKuYXk0HjLJJnw9dDTZwd";
  user-belak-hades = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK69hivmEYShurplMVlBfRanBi4St0pbnbRXSP0n7Qnm";
  user-belak-melinoe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzuXboQDv2VCig0+A780O0+sKs1euw+3OafnRA6z14P";
  user-belak-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINjxw57nR3VIhpVt9zYipzLqZ0ecHhDBjyP8dNhxL5mP";
  user-belak-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvdWkVKcV087KDa9e2fdaubwW8SztSo+k+lYaeEKILC";

  user-belak-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUSx9TTTHUq4GOkeBU4Ga03QombEBiZLqqa8KIqnnUy";

  users = [
    user-belak-beatrix
    user-belak-hades
    user-belak-melinoe
    user-belak-quina
    user-belak-zorn

    user-belak-work
  ];

  system-baku = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUhTjBux/Puqhpa4TgphZYsXIClhMWF0iOTZugc0k6a";
  system-beatrix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ8DedpQ6Q+OqlMeiQydzu89Q2xIGGAIIl4+tyXy584v";
  system-freya = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDU1rGovd901nTi60c/WTDtTrkWSJ8V2lDMJr6MusKWS";
  system-hades = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINB84SBcMThfhBWlPiW1ySels6Ri17TDoDSjuuoX4tfF";
  system-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBZ06jS8rephEg8IZgqkwBJ7QRPH7Osh+HE0LU6q2YvC";
  system-zidane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8pkaoi6ASLpjFP+9v/frMX6wAiWrM3LTMvkdnU8Rd0";
  system-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIl/rte7VsiKLVGhRLz3eSYh4Ln3YO9h/CQEOrib4xKe";

  systems = [
    system-baku
    system-beatrix
    system-freya
    system-hades
    system-quina
    system-zidane
    system-zorn
  ];

  service-atticd = [ system-quina ];
  service-forgejo = [ system-baku ];
  service-immich = [ system-freya ];
  service-kavita = [ system-freya ];
  service-miniflux = [ system-freya ];
  service-pocket-id = [ system-zidane ];
  service-syncthing = [ system-freya ];
  service-woodpecker = [ system-baku ];
  service-woodpecker-agent = [ system-quina ];
in
{
  "atticd-env.age".publicKeys = service-atticd ++ users;

  "acme-cloudflare-env.age".publicKeys = users ++ [ system-zidane ];
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
