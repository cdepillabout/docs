#!/usr/bin/env bash
#
# Set config values used for building and running xmonad and taffybar.

# The directory in $HOME with all the XMonad stuff.
export XMONAD_DIR="${HOME}/.xmonad"

# A directory to use to install the taffybar and xmonad binaries.
export XMONAD_LOCAL_BIN_PATH="${XMONAD_DIR}/local-bin"

# The lts resolver we will use for building with stack.
export XMONAD_STACK_RESOLVER="lts-9.14"
