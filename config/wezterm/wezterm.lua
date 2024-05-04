local wezterm = require 'wezterm';

local is_macos = wezterm.target_triple == "x86_64-apple-darwin" or wezterm.target_triple == "aarch64-apple-darwin"

-- Set the font information. We use the ttf version of Terminus because for
-- some reason the bitmap font doesn't work properly on Linux.
local font = "Terminus"
local font_size = 12.0

local extra_keys = {};

if is_macos then
  -- When I'm using MacOS, I'm generally on a larger monitor, so a larger font
  -- size is important for readability.
  font = "Terminus (TTF)"
  font_size = 14.0

  extra_keys = {
    {key="LeftArrow", mods="CMD", action=wezterm.action.ActivateTabRelative(-1)},
    {key="RightArrow", mods="CMD", action=wezterm.action.ActivateTabRelative(1)},
  }
end


local config = {
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
    table.unpack(extra_keys)
  },

  font = wezterm.font(font),
  font_size = font_size,

  bold_brightens_ansi_colors = "BrightOnly",

  exit_behavior = "Close",

  -- Disable all the ligatures we can. For some reason many fonts have a
  -- ligature for "fi" which looks really weird when in the middle of a
  -- word like config. It may be petty but I find it easier to disable
  -- all ligatures than put up with it.
  harfbuzz_features = {"calt=0", "clig=0", "liga=0"},

  -- Revert to the old tab bar style
  use_fancy_tab_bar = false,

  -- Hide the tab-bar if there's only one application. Sometimes I use terminal
  -- applications, and it works well to hide the tab-bar if I'm using wezterm
  -- as an application window.
  hide_tab_bar_if_only_one_tab = true,

  -- None of the color schemes quite match the default Linux colors, so we
  -- define our own.
  color_scheme = "Linux",
  color_schemes = {
    ["Linux"] = {
      background = "#000000",
      foreground = "#ffffff",

      -- NOTE: these aren't quite right - I should probably tweak them at some
      -- point.
      cursor_bg = "#ffffff",
      cursor_border = "#ffffff",
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

local wayland_gnome = require 'wayland_gnome'
wayland_gnome.apply_to_config(config)

return config
