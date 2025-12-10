let
  user-belak-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUSx9TTTHUq4GOkeBU4Ga03QombEBiZLqqa8KIqnnUy";
  user-belak-melinoe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzuXboQDv2VCig0+A780O0+sKs1euw+3OafnRA6z14P";
  user-belak-hades = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK69hivmEYShurplMVlBfRanBi4St0pbnbRXSP0n7Qnm";
  user-belak-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINjxw57nR3VIhpVt9zYipzLqZ0ecHhDBjyP8dNhxL5mP";
  user-belak-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvdWkVKcV087KDa9e2fdaubwW8SztSo+k+lYaeEKILC";

  users = [
    user-belak-work
    user-belak-melinoe
    user-belak-hades
    user-belak-quina
    user-belak-zorn
  ];

  system-eiko = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGFpH5p7ODkUq0kLqda1/fghcCo+MxvCZLdKOfhZCtK+";
  system-freya = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDU1rGovd901nTi60c/WTDtTrkWSJ8V2lDMJr6MusKWS";
  system-hades = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINB84SBcMThfhBWlPiW1ySels6Ri17TDoDSjuuoX4tfF";
  system-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBZ06jS8rephEg8IZgqkwBJ7QRPH7Osh+HE0LU6q2YvC";
  system-zidane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8pkaoi6ASLpjFP+9v/frMX6wAiWrM3LTMvkdnU8Rd0";
  system-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIl/rte7VsiKLVGhRLz3eSYh4Ln3YO9h/CQEOrib4xKe";

  systems = [
    system-eiko
    system-freya
    system-hades
    system-quina
    system-zidane
    system-zorn
  ];

  service-forgejo = [ system-eiko ];
  service-miniflux = [ system-eiko ];
  service-pocket-id = [ system-eiko ];
in
{
  "acme-cloudflare-env.age".publicKeys = users ++ [
    system-eiko
    system-zidane
  ];
  "belak-password.age".publicKeys = users ++ systems;

  "miniflux-admin-credentials.age".publicKeys = service-miniflux ++ users;

  "miniflux-oidc-client-id.age".publicKeys = service-miniflux ++ users;
  "miniflux-oidc-client-secret.age".publicKeys = service-miniflux ++ users;

  "pocket-id-encryption-key.age".publicKeys = service-pocket-id ++ users;

  "forgejo-oidc-client-id.age".publicKeys = service-forgejo ++ users;
  "forgejo-oidc-client-secret.age".publicKeys = service-forgejo ++ users;
}
