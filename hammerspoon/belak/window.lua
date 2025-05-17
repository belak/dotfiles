-- Useful constants - these are positions I use in addition to the hs.layout
-- constants.
local left30top50 = hs.geometry.rect(0, 0, 0.3, 0.5)
local left30bot50 = hs.geometry.rect(0, 0.5, 0.3, 0.5)
local middle40 = hs.geometry.rect(0.3, 0, 0.4, 1)
local right30top50 = hs.geometry.rect(0.7, 0, 0.3, 0.5)
local right30bot50 = hs.geometry.rect(0.7, 0.5, 0.3, 0.5)

-- Disable window animations to make things a bit snappier
hs.window.animationDuration = 0

-- Left side shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(hs.layout.left30)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "u", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(left30top50)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "j", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(hs.layout.left30)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "n", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(left30bot50)
end)

-- Middle shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Return", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(middle40)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "Return", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(hs.layout.maximized)
end)

-- Right side shortcuts

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(hs.layout.right30)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "o", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(right30top50)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "l", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(hs.layout.right30)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, ",", function()
  local win = hs.window.focusedWindow()
  win:moveToUnit(right30bot50)
end)
