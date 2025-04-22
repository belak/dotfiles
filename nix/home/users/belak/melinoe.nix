{ pkgs, ... }:
{
  belak = {
    dotfiles.enable = true;
    dotfiles.symlink = true;
    dev.enable = true;
    emacs.enable = true;
  };

  nixpkgs.allowedUnfree = [
    "discord"
    "obsidian"
    "spotify"
    "vscode"
  ];

  home.packages = with pkgs; [
    deploy-rs
    discord
    libation
    nix-init
    typst
    typstfmt
    tinymist
    senpai
    spotify
    templ
  ];

  programs.vscode = {
    enable = true;

    mutableExtensionsDir = false;

    extensions =
      with pkgs.vscode-extensions;
      [
        editorconfig.editorconfig
        golang.go
        jnoortheen.nix-ide
        stkb.rewrap
        rust-lang.rust-analyzer
      ]
      ++ (with pkgs.community-vscode-extensions.vscode-marketplace; [
        a-h.templ
        monokai.theme-monokai-pro-vscode
      ]);

    userSettings = {
      "editor.acceptSuggestionOnCommitCharacter" = false;
      "editor.cursorBlinking" = "solid";
      "editor.find.seedSearchStringFromSelection" = false;
      "editor.formatOnSave" = true;
      "editor.minimap.enabled" = false;
      "editor.renderWhitespace" = "selection";
      "editor.scrollBeyondLastLine" = false;
      "explorer.fileNesting.expand" = false;
      "extensions.ignoreRecommendations" = true;
      "files.hotExit" = "off";
      "workbench.colorTheme" = "Monokai Pro";
      "workbench.iconTheme" = "Monokai Pro Icons";
      "workbench.settings.editor" = "json";
      "workbench.settings.useSplitJSON" = true;
      "workbench.startupEditor" = "none";

      "emmet.includeLanguages" = {
        "templ" = "html";
      };

      "[nix]" = {
        "editor.formatOnSave" = false;
      };

      "[templ]" = {
        "editor.defaultFormatter" = "a-h.templ";
      };
    };

    keybindings = [
      {
        "key" = "cmd+1";
        "command" = "workbench.action.openEditorAtIndex1";
      }
      {
        "key" = "cmd+2";
        "command" = "workbench.action.openEditorAtIndex2";
      }
      {
        "key" = "cmd+3";
        "command" = "workbench.action.openEditorAtIndex3";
      }
      {
        "key" = "cmd+4";
        "command" = "workbench.action.openEditorAtIndex4";
      }
      {
        "key" = "cmd+5";
        "command" = "workbench.action.openEditorAtIndex5";
      }
      {
        "key" = "cmd+6";
        "command" = "workbench.action.openEditorAtIndex6";
      }
      {
        "key" = "cmd+7";
        "command" = "workbench.action.openEditorAtIndex7";
      }
      {
        "key" = "cmd+8";
        "command" = "workbench.action.openEditorAtIndex8";
      }
      {
        "key" = "cmd+9";
        "command" = "workbench.action.lastEditorInGroup";
      }
      {
        "key" = "shift+cmd+1";
        "command" = "workbench.action.focusFirstEditorGroup";
      }
      {
        "key" = "shift+cmd+2";
        "command" = "workbench.action.focusSecondEditorGroup";
      }
      {
        "key" = "shift+cmd+3";
        "command" = "workbench.action.focusThirdEditorGroup";
      }
      {
        "key" = "ctrl+tab";
        "command" = "workbench.action.nextEditorInGroup";
      }
      {
        "key" = "ctrl+shift+tab";
        "command" = "workbench.action.previousEditorInGroup";
      }
    ];
  };
}
