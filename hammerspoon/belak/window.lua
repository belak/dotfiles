-- Useful constants - these are positions I use in addition to the hs.layout
-- constants.
local left30top50 = hs.geometry.rect(0, 0, 0.3, 0.5)
local left30bot50 = hs.geometry.rect(0, 0.5, 0.3, 0.5)
local middle40 = hs.geometry.rect(0.3, 0, 0.4, 1)
local middle40top50 = hs.geometry.rect(0.3, 0, 0.4, 0.5)
local middle40bot50 = hs.geometry.rect(0.3, 0.5, 0.4, 0.5)
local right30top50 = hs.geometry.rect(0.7, 0, 0.3, 0.5)
local right30bot50 = hs.geometry.rect(0.7, 0.5, 0.3, 0.5)

-- Note that this is a special case because of how pop-out video players work on
-- macOS.
local right30top59 = hs.geometry.rect(0.7, 0, 0.3, 0.59)
local right30bot41 = hs.geometry.rect(0.7, 0.59, 0.3, 0.41)

-- Disable window animations to make things a bit snappier
hs.window.animationDuration = 0

-- axHotfix is a workaround for windows which set AXEnhancedUserInterface not
-- respecting the set animationDuration.
local function axHotfix(action)
  local window = hs.window.focusedWindow()

  if not window then return end

  local app = window:application()
  local ax_app = hs.axuielement.applicationElement(app)

  -- original settings
  local was_enhanced = ax_app.AXEnhancedUserInterface
  local original_animation_duration = hs.window.animationDuration

  -- Override AXEnhancedUserInterface and animationDuration
  ax_app.AXEnhancedUserInterface = false
  hs.window.animationDuration = 0

  -- Run our action
  action(window)

  -- restore original settings
  hs.window.animationDuration = original_animation_duration
  ax_app.AXEnhancedUserInterface = was_enhanced
end

-- Left side shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
  axHotfix(function (win) win:moveToUnit(hs.layout.left50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "i", function()
  axHotfix(function (win) win:moveToUnit(left30top50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "j", function()
  axHotfix(function (win) win:moveToUnit(hs.layout.left30) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "n", function()
  axHotfix(function (win) win:moveToUnit(left30bot50) end)
end)

-- Middle shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "o", function()
  axHotfix(function (win) win:moveToUnit(middle40top50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "m", function()
  axHotfix(function (win) win:moveToUnit(middle40bot50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Return", function()
  axHotfix(function (win) win:moveToUnit(middle40) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "Return", function()
  axHotfix(function (win) win:moveToUnit(hs.layout.maximized) end)
end)

-- Right side shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
  axHotfix(function (win) win:moveToUnit(hs.layout.right50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "p", function()
  axHotfix(function (win) win:moveToUnit(right30top50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "l", function()
  axHotfix(function (win) win:moveToUnit(hs.layout.right30) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, ",", function()
  axHotfix(function (win) win:moveToUnit(right30bot50) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, ";", function()
  axHotfix(function (win) win:moveToUnit(right30top59) end)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, ".", function()
  axHotfix(function (win) win:moveToUnit(right30bot41) end)
end)
