#!/usr/bin/env bash

# This is called from udev.  It basically just calls the ./setup-keyboard.sh
# script.

CURDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# XXX: It would be nice to figure out a way not to have to hardcode these.
export HOME="/home/illabout"
export DISPLAY=":0"
export XAUTHORITY="${HOME}/.Xauthority"

(sleep 2 && $CURDIR/setup-keyboard.sh) & disown -h
