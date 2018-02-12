#!/usr/bin/env bash

# Setup the keyboard map based on what keyboard I have plugged in.

echo "xauth file: $XAUTHORITY" >> /tmp/trying-to-setup-keyboard

setxkbmap us

if [ -f "$HOME/.Xmodmap_swap_ctrl_caps" ]; then
	xmodmap "$HOME/.Xmodmap_swap_ctrl_caps"
fi

# change keys on Japanese keyboard if we are using a Japanese keyboard
japanese_keyboard_xmodmap="$HOME/.Xmodmap_japanese_keyboard"

if [ -f "$japanese_keyboard_xmodmap" ]; then
	grep_output="$(lsusb | grep '045e:00dc Microsoft Corp.')"
	if [ -n "$grep_output" ] ; then
		xmodmap "$japanese_keyboard_xmodmap"
	fi
fi
