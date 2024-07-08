let
  user-belak-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUSx9TTTHUq4GOkeBU4Ga03QombEBiZLqqa8KIqnnUy";
  user-belak-zagreus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGNHMEfjGg5ek6OtbFytZ/zCSZosT8aHqHRfnufb3gIi";

  users-admins = [
    user-belak-work
    user-belak-zagreus
  ];
  users = [
    user-belak-work
    user-belak-zagreus
  ];

  system-kupo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPwQnnGikM0ko50JG6l7WPosQNgtnjqpR9i6BvYbkRO2";
  system-stiltzkin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCdp7P84mEIR2+TwvqjXlqye92bCOSD1uaf+kuBl2ex";
  system-moguo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILR5itUnf60cPaUMbwe2O+pPr99zXyCy6S1oGg2bOub3";
  system-monty = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+wnueT1cqvNDi5INmEsFSHHW5/Chhko8LGEz9zQOPx";
  system-eiko = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGFpH5p7ODkUq0kLqda1/fghcCo+MxvCZLdKOfhZCtK+";
  system-vivi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDx5Y7VvA9CUdrsiVpNbRufBdJdvJZEfRQXIGnPgqynH";
  system-zagreus = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID4UJL+NcFXmr678LkhV92sqIoM4gpKic0lc8DGH0Mib";
  system-zidane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8pkaoi6ASLpjFP+9v/frMX6wAiWrM3LTMvkdnU8Rd0";

  systems = [
    system-kupo
    system-stiltzkin
    system-moguo
    system-monty
    system-eiko
    system-vivi
    system-zagreus
    system-zidane
  ];
in
{
  "belak-password.age".publicKeys = users-admins ++ systems;
  "traefik-env.age".publicKeys = users-admins ++ [ system-eiko ];
}
