{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = lib.mkIf pkgs.stdenv.isDarwin {
    home.packages = with pkgs; [
      coreutils-prefixed
      gnugrep
      mkalias
    ];

    # This is a hack adapted from https://github.com/LnL7/nix-darwin/issues/214#issuecomment-2048491929
    # which creates aliases for home-manager applications, allowing them
    # to be found by Alfred and spotlight, as long as com.apple.alias-file
    # is specified under additional file types.
    home.activation = {
      makeTrampolines = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        app_folder="${config.home.homeDirectory}/Applications/Home Manager Trampolines"
        rm -rf "$app_folder"
        mkdir -p "$app_folder"
        for app in $genProfilePath/home-path/Applications/*.app; do
          app_target="$app_folder/$(basename "$app")"
          real_app=$(readlink -f "$app")
          echo "mkalias \"$real_app\" \"$app_target\"" >&2
          $DRY_RUN_CMD ${pkgs.mkalias}/bin/mkalias "$real_app" "$app_target"
        done
      '';
    };
  };
}
