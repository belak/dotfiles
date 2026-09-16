{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.belak.taskwarrior;
in
{
  options.belak.taskwarrior = {
    enable = lib.mkEnableOption "taskwarrior";
  };

  config = lib.mkIf cfg.enable {
    programs.taskwarrior = {
      enable = true;
      package = pkgs.taskwarrior3;
      colorTheme = "dark-16";

      # We specify as much config as possible here, but add an include for
      # taskrc.local to allow configuring the sync server separately.
      config = {
        # Context definitions
        context.code.read = "project:code";
        context.code.write = "project:code";
        context.fun.read = "project:fun or +fun";
        context.fun.write = "+fun";
        context.work.read = "project:work";
        context.work.write = "project:work";

        # Tag priority tweaks
        urgency.user.tag.costly.coefficient = 3.0;
        urgency.user.tag.fun.coefficient = 0.0;

        # Change priorities to be High, Medium, None (default), Low rather than
        # the default of H, M, L, None.
        uda.priority.values = "H,M,,L";
        urgency.uda.priority.L.coefficient = 0.0;
        urgency.uda.priority."".coefficient = 1.8;

        # Allow parent urgency to cascade into children.
        urgency."inherit" = "on";

        # Various behavior tweaks for usability and consistency
        weekstart = "Monday";
        search.case.sensitive = "no";

        # Sync in the background every minute when tui is open.
        uda.taskwarrior-tui.background_process = "task sync";
        uda.taskwarrior-tui.background_process_period = 60;

        # Press `t` in tui to toggle the +next tag on the selected task.
        uda.taskwarrior-tui.quick-tag.name = "next";
      };

      # Includes can't be specified by config above, so we set them up here.
      extraConfig = ''
        include holidays.en-US.rc
        include ~/.config/task/taskrc.local
      '';
    };

    home.packages = with pkgs; [
      python314Packages.bugwarrior
      taskopen
      taskwarrior-tui
      vit
    ];
  };
}
