# Public SSH keys, used as both agenix recipients and the root authorizedKeys in
# the server module.
rec {
  user-belak-melinoe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMzuXboQDv2VCig0+A780O0+sKs1euw+3OafnRA6z14P";
  user-belak-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINjxw57nR3VIhpVt9zYipzLqZ0ecHhDBjyP8dNhxL5mP";
  user-belak-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOvdWkVKcV087KDa9e2fdaubwW8SztSo+k+lYaeEKILC";

  user-belak-work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUSx9TTTHUq4GOkeBU4Ga03QombEBiZLqqa8KIqnnUy";

  users = [
    user-belak-melinoe
    user-belak-quina
    user-belak-zorn

    user-belak-work
  ];

  system-baku = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFUhTjBux/Puqhpa4TgphZYsXIClhMWF0iOTZugc0k6a";
  system-freya = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDU1rGovd901nTi60c/WTDtTrkWSJ8V2lDMJr6MusKWS";
  system-quina = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBZ06jS8rephEg8IZgqkwBJ7QRPH7Osh+HE0LU6q2YvC";
  system-vivi = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDx5Y7VvA9CUdrsiVpNbRufBdJdvJZEfRQXIGnPgqynH";
  system-zidane = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8pkaoi6ASLpjFP+9v/frMX6wAiWrM3LTMvkdnU8Rd0";
  system-zorn = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIl/rte7VsiKLVGhRLz3eSYh4Ln3YO9h/CQEOrib4xKe";

  systems = [
    system-baku
    system-freya
    system-quina
    system-vivi
    system-zidane
    system-zorn
  ];
}
