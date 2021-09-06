local wezterm = require 'wezterm';

return {
  font = wezterm.font("Terminus (TTF)"),
  font_size = 12.0,

  close_behavior = "Close",

  -- None of the color schemes quite match the default Linux colors, so we
  -- define our own.
  color_scheme = "Linux",
  color_schemes = {
    ["Linux"] = {
      background = "#000000",
      foreground = "#ffffff",

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
