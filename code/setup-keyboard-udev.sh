#!/usr/bin/env bash

# This is called from udev.  It basically just calls the ./setup-keyboard.sh
# script.

CURDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# XXX: It would be nice to figure out a way not to have to hardcode these.
export HOME="/home/illabout"
export DISPLAY=":0.0"
export XAUTHORITY="${HOME}/.Xauthority"

if ps auxw | grep -i [x]org-server >/dev/null; then
	echo "`date`: running setup-keyboard-udev.sh..." >> /tmp/setup-keyboard-udev.debug
	$CURDIR/setup-keyboard.sh & disown -h
else
	echo "`date`: not running setup-keyboard-udev.sh because the X server is not running" >> /tmp/setup-keyboard-udev.debug
fi
