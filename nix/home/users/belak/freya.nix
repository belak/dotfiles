{ ... }:
{
  belak = {
    dotfiles.enable = true;

    # Beets runs here rather than on a workstation so moves and renames stay
    # local to the NFS server instead of crossing the network. These are the
    # storage paths: /mnt/media/* is the read-only view Plex and Jellyfin
    # index, and /mnt/remote-thorn is a sandbox for another machine.
    beets = {
      enable = true;
      directory = "/mnt/amarant/media/Music Lossless";
      derivedDirectory = "/mnt/amarant/media/Music";
    };
  };

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.
}
