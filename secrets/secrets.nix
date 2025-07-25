let
  user-belak-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUSx9TTTHUq4GOkeBU4Ga03QombEBiZLqqa8KIqnnUy";
  user-belak-melinoe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzuXboQDv2VCig0+A780O0+sKs1euw+3OafnRA6z14P";
  user-belak-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBdD1/ti5UMpCjCJbwTGplZZmVNwCT+c6rYqdKa9cN3t";
  user-belak-zagreus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNHMEfjGg5ek6OtbFytZ/zCSZosT8aHqHRfnufb3gIi";
  user-belak-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvdWkVKcV087KDa9e2fdaubwW8SztSo+k+lYaeEKILC";

  users = [
    user-belak-work
    user-belak-melinoe
    user-belak-quina
    user-belak-zagreus
    user-belak-zorn
  ];

  system-artemicion = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG+HQwhrRk/ipckUwCLPCkujJe5Uzl/HmdY2iX+PGqd1";
  system-kupo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPwQnnGikM0ko50JG6l7WPosQNgtnjqpR9i6BvYbkRO2";
  system-stiltzkin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCdp7P84mEIR2+TwvqjXlqye92bCOSD1uaf+kuBl2ex";
  system-moguo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILR5itUnf60cPaUMbwe2O+pPr99zXyCy6S1oGg2bOub3";
  system-monty = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+wnueT1cqvNDi5INmEsFSHHW5/Chhko8LGEz9zQOPx";
  system-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIyYqXHUJt+EZhWhWH2WlErat+CQQ/iVfsZNkaJLzEBu";
  system-eiko = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGFpH5p7ODkUq0kLqda1/fghcCo+MxvCZLdKOfhZCtK+";
  system-vivi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDx5Y7VvA9CUdrsiVpNbRufBdJdvJZEfRQXIGnPgqynH";
  system-zagreus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID4UJL+NcFXmr678LkhV92sqIoM4gpKic0lc8DGH0Mib";
  system-zidane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8pkaoi6ASLpjFP+9v/frMX6wAiWrM3LTMvkdnU8Rd0";
  system-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIl/rte7VsiKLVGhRLz3eSYh4Ln3YO9h/CQEOrib4xKe";

  systems = [
    system-artemicion
    system-kupo
    system-stiltzkin
    system-moguo
    system-monty
    system-quina
    system-eiko
    system-vivi
    system-zagreus
    system-zidane
    system-zorn
  ];

  service-authelia = [ system-eiko ];
  service-forgejo = [ system-eiko ];
  service-miniflux = [ system-eiko ];
  service-lldap = [ system-eiko ];
in
{
  "acme-cloudflare-env.age".publicKeys = users ++ [
    system-eiko
    system-zidane
  ];
  "belak-password.age".publicKeys = users ++ systems;

  "authelia-ldap-password.age".publicKeys = service-authelia ++ users;
  "authelia-jwt-secret.age".publicKeys = service-authelia ++ users;
  "authelia-storage-encryption-key.age".publicKeys = service-authelia ++ users;
  "authelia-oidc-hmac-secret.age".publicKeys = service-authelia ++ users;
  "authelia-oidc-rs256-key.age".publicKeys = service-authelia ++ users;

  "lldap-admin-password.age".publicKeys = service-lldap ++ users;
  "lldap-jwt-secret.age".publicKeys = service-lldap ++ users;

  "miniflux-admin-credentials.age".publicKeys = service-miniflux ++ users;

  "miniflux-oidc-client-id.age".publicKeys = service-authelia ++ service-miniflux ++ users;
  "miniflux-oidc-client-secret.age".publicKeys = service-miniflux ++ users;
  "miniflux-oidc-client-secret-hashed.age".publicKeys = service-authelia ++ users;

  "forgejo-oidc-client-id.age".publicKeys = service-authelia ++ service-forgejo ++ users;
  "forgejo-oidc-client-secret.age".publicKeys = service-forgejo ++ users;
  "forgejo-oidc-client-secret-hashed.age".publicKeys = service-authelia ++ users;

  "testing-oidc-client-id.age".publicKeys = service-authelia ++ users;
  "testing-oidc-client-secret.age".publicKeys = users;
  "testing-oidc-client-secret-hashed.age".publicKeys = service-authelia ++ users;
}
