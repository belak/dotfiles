-- We actually want to load "reload" first, just in case any of the other
-- modules prevent the config from fully loading.
require("belak.reload")

require("belak.window")

hs.alert.show("Config loaded")
