{
  imports = [
    ./buildbot-master.nix
    ./buildbot-worker.nix
    ./gitea.nix
    ./nginx.nix
    ./postgres.nix
    ./soju.nix
  ];
}
