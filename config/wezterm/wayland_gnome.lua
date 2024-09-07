-- Originally copied from this comment on GitHub:
--
-- https://github.com/wez/wezterm/issues/3334#issuecomment-1510393277

local wezterm = require 'wezterm'
local mod = {}

local function gsettings(key)
  return wezterm.run_child_process({"gsettings", "get", "org.gnome.desktop.interface", key})
end

function mod.apply_to_config(config)
  if wezterm.target_triple ~= "x86_64-unknown-linux-gnu" then
    -- skip if not running on linux
    return
  end

  local success, stdout, stderr = gsettings("cursor-theme")
  if success then
    config.xcursor_theme = stdout:gsub("'(.+)'\n", "%1")
  end

  local success, stdout, stderr = gsettings("cursor-size")
  if success then
    config.xcursor_size = tonumber(stdout)
  end

  if os.getenv("WAYLAND_DISPLAY") then
    config.enable_wayland = true

    local success, stdout, stderr = gsettings("text-scaling-factor")
    if success then
      config.font_size = (config.font_size or 10.0) * tonumber(stdout)
    end
  end
end

return mod
