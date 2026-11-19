{
  nixpkgs-unstable,
  agenix,
  llm-agents,
  emacs-overlay,
  nix-vscode-extensions,
  ...
}:
{
  additions = final: _prev: {
    my = final.lib.packagesFromDirectoryRecursive {
      callPackage = final.callPackage;
      directory = ./pkgs;
    };
  };

  llm-agents = llm-agents.overlays.shared-nixpkgs;

  emacs = emacs-overlay.overlays.default;

  unstable = final: _prev: {
    unstable = import nixpkgs-unstable {
      inherit (final) config;
      inherit (final.stdenv.hostPlatform) system;
    };
  };

  vscode = final: _prev: {
    community-vscode-extensions = nix-vscode-extensions.extensions.${final.stdenv.hostPlatform.system};
  };
}
