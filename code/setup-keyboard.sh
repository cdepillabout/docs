#!/usr/bin/env bash
#
# Setup the keyboard map based on what keyboard I have plugged in.
#
# We need to use a lock to make sure that this doesn't get run twice at once.
# This is because setxkbmap needs to be called before xmodmap, and xmodmap can
# only be called ONCE after setxkbmap.  This is because it switches the current
# values of the keys, instead of making sure they are the exact same everytime.

LOCKFILE=/tmp/setup-keyboard.lock

# Create the lockfile if it does not exist.
if [ ! -f "${LOCKFILE}" ] ; then
  touch "${LOCKFILE}"
fi

(
  flock -n 200 || exit 1

  sleep 0.5

  setxkbmap -display "${DISPLAY}" us

  if [ -f "$HOME/.Xmodmap_swap_ctrl_caps" ]; then
    xmodmap -display "${DISPLAY}" "$HOME/.Xmodmap_swap_ctrl_caps"
  fi

  # change keys on Japanese keyboard if we are using a Japanese keyboard
  japanese_keyboard_xmodmap="$HOME/.Xmodmap_japanese_keyboard"

  if [ -f "$japanese_keyboard_xmodmap" ]; then
    lsusb_output="$(lsusb -d "045e:00dc")"
    if [ -n "$lsusb_output" ] ; then
      xmodmap "$japanese_keyboard_xmodmap"
    fi
  fi

) 200<"${LOCKFILE}"
