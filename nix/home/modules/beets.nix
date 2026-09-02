{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.beets;
in
{
  options.belak.beets = {
    enable = lib.mkEnableOption "beets";

    directory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Music/master";
      description = ''
        Root of the master tree, holding the best copy of each album. Beets
        moves and renames within this constantly, so it must be writable.
      '';
    };

    derivedDirectory = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Music/derived";
      description = ''
        Root of the derived MP3 tree, regenerated from the master with
        `beet alt update derived`. Never edited directly.
      '';
    };

    library = lib.mkOption {
      type = lib.types.str;
      default = "${config.xdg.dataHome}/beets/library.db";
      description = ''
        Path to the beets database. This is SQLite and does not behave over
        NFS, so keep it on local disk even when the music itself is remote.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # base.yaml is installed by the dotfiles module alongside every other
    # config file. Without it the include below resolves to nothing and beets
    # silently runs on defaults, so make the dependency fail loudly instead.
    assertions = [
      {
        assertion = config.belak.dotfiles.enable;
        message = "belak.beets requires belak.dotfiles for ~/.config/beets/base.yaml";
      }
    ];

    home.packages = [ pkgs.my.beets ];

    # Only the per-host paths are generated; everything else lives in the
    # checked-in base.yaml. Beets gives the including file higher priority than
    # anything it includes, so these win wherever the two overlap.
    #
    # Generating config.yaml rather than symlinking it also leaves
    # ~/.config/beets writable, which is where beets puts the Discogs token and
    # the import state file.
    xdg.configFile."beets/config.yaml".text = ''
      # Generated from belak.beets. Edit config/beets/config.yaml in the
      # dotfiles repo for anything that is not a per-host path.
      include:
        - ${config.home.homeDirectory}/.config/beets/base.yaml

      directory: ${cfg.directory}
      library: ${cfg.library}

      alternatives:
        derived:
          directory: ${cfg.derivedDirectory}
    '';
  };
}
