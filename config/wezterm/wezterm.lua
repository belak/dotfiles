local wezterm = require 'wezterm';

return {
  -- Override a number of keybinds to make them spawn in the home directory
  -- rather than the current one.
  keys = {
    {key="t", mods="CMD", action=wezterm.action{SpawnCommandInNewTab={
      cwd = wezterm.home_dir,
    }}},
    {key="t", mods="CTRL|SHIFT", action=wezterm.action{SpawnCommandInNewTab={
      cwd = wezterm.home_dir,
    }}},
    {key="\"", mods="CTRL|ALT", action=wezterm.action{SplitHorizontal={
      cwd = wezterm.home_dir,
    }}},
    {key="%", mods="CTRL|ALT", action=wezterm.action{SplitVertical={
      cwd = wezterm.home_dir,
    }}},
  },

  -- Set the font information. We use the ttf version of Terminus because for
  -- some reason the bitmap font doesn't work properly on Linux and we can use
  -- the same font on macOS.
  font = wezterm.font("Ubuntu Mono"),
  font_size = 12.0,

  exit_behavior = "Close",

  -- None of the color schemes quite match the default Linux colors, so we
  -- define our own.
  color_scheme = "Linux",
  color_schemes = {
    ["Linux"] = {
      background = "#000000",
      foreground = "#ffffff",

      -- NOTE: these aren't quite right - I should probably tweak them at some
      -- point.
      cursor_bg = "#53ae71",
      cursor_border = "#53ae71",
      cursor_fg = "#000000",
      selection_bg = "#4d52f8",
      selection_fg = "#000000",

      ansi = {
        "#000000",
        "#aa0000",
        "#00aa00",
        "#aa5500",
        "#0000aa",
        "#aa00aa",
        "#00aaaa",
        "#aaaaaa",
      },
      brights = {
        "#555555",
        "#ff5555",
        "#55ff55",
        "#ffff55",
        "#5555ff",
        "#ff55ff",
        "#55ffff",
        "#ffffff"
      },
    }
  }
}
