{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.belak.vscode;
  jsonFormat = pkgs.formats.json { };
in
{
  options.belak.vscode = {
    enable = lib.mkEnableOption "vscode";
    extraExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
    };
    userSettings = lib.mkOption {
      type = lib.types.either lib.types.path jsonFormat.type;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    nixpkgs.allowedUnfree = [
      "vscode"
    ];

    programs.vscode = {
      enable = true;

      mutableExtensionsDir = false;

      profiles.default = {
        extensions =
          with pkgs.vscode-extensions;
          [
            editorconfig.editorconfig
            golang.go
            jnoortheen.nix-ide
            mathiasfrohlich.kotlin
            rust-lang.rust-analyzer
            stkb.rewrap
            svelte.svelte-vscode
          ]
          ++ (with pkgs.community-vscode-extensions.vscode-marketplace; [
            a-h.templ
            monokai.theme-monokai-pro-vscode
          ])
          ++ cfg.extraExtensions;

        userSettings = lib.mkMerge [
          ({
            # Fix a number of nits I have with VSCode, mostly disabling features
            # and hiding things.
            "editor.acceptSuggestionOnCommitCharacter" = false;
            "editor.find.seedSearchStringFromSelection" = false;
            "editor.scrollBeyondLastLine" = false;
            "explorer.fileNesting.expand" = false;
            "extensions.ignoreRecommendations" = true;
            "workbench.startupEditor" = "none";
            "files.hotExit" = "off";
            "search.showLineNumbers" = true;
            "editor.inlayHints.enabled" = "offUnlessPressed";

            # Theme and other UI settings
            "workbench.colorTheme" = "Monokai Pro";
            "workbench.iconTheme" = "Monokai Pro Icons";
            "workbench.editor.highlightModifiedTabs" = true;
            "editor.cursorBlinking" = "solid";
            "editor.minimap.enabled" = false;
            "editor.renderWhitespace" = "selection";
            "window.menuBarVisibility" = "toggle";

            # Use JSON in the settings editor so we can more easily see the exact
            # keys we need when configuring via nix.
            "workbench.settings.editor" = "json";
            "workbench.settings.useSplitJSON" = true;

            # Disable formatOnSave globally so we can enable it for specific
            # languages.
            "editor.formatOnSave" = true;

            # Plugin specific settings
            "emmet.includeLanguages" = {
              "templ" = "html";
            };

            # Language specific settings
            "[templ]" = {
              "editor.defaultFormatter" = "a-h.templ";
            };
          })
          cfg.userSettings
        ];

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

    };
  };
}
