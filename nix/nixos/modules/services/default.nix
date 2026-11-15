# Imports the self-hosted service modules.
{
  imports = [
    ./atticd.nix
    ./audiobookshelf.nix
    ./btta.nix
    ./caddy.nix
    ./filebrowser-quantum.nix
    ./forgejo.nix
    ./immich.nix
    ./inkheart.nix
    ./jellyfin.nix
    ./kavita.nix
    ./linkding.nix
    ./miniflux.nix
    ./oauth2-proxy.nix
    ./plex.nix
    ./pocket-id.nix
    ./podman.nix
    ./postgres.nix
    ./soju.nix
    ./syncthing.nix
    ./taskchampion.nix
    ./woodpecker-agent.nix
    ./woodpecker.nix
  ];
}
