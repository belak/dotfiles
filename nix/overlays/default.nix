{
  packages = (final: prev: {
    # Overrides
    my = {
      wezterm = final.darwin.apple_sdk_11_0.callPackage ./packages/wezterm.nix {
        inherit (final.darwin.apple_sdk_11_0.frameworks) Cocoa CoreGraphics Foundation UserNotifications System;
      };
    };

    # Additional packages
    wezterm-bin = final.callPackage ./packages/wezterm-bin.nix { };
  });
}
