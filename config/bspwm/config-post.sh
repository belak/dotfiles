#!/bin/sh

# This needs to pick up the space names from previous bspc commands. Thankfully
# it's optional so if something early in the config fails, it's not a big deal
# if this fails to start... which is why it's in config-post rather than
# config-pre
killall polybar
polybar -r mybar &

# Try to restore our saved backlight brightness state.
brightnessctl -r
brightnessctl -r --device tpacpi::kbd_backlight

if [[ -f ~/.fehbg ]]; then ~/.fehbg; fi
