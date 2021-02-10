#!/bin/sh

killall sxhkd
sxhkd &

killall xss-lock
xss-lock -- \
  i3lock \
    --blur 1 \
    --color 000000ff \
    --pass-media-keys \
    --pass-screen-keys &

