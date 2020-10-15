#!/usr/bin/env bash
#
# This is a script to set the SSH_AUTH_SOCK environment variable
# before running git.  See the documentation in get-ssh-auth-sock
# for why this is necessary.
#
# This command is called from the emacs plugin magit.

auth_sock="$(get-ssh-auth-sock -q)"
ret="$?"

if [ "$ret" = 0 ] ; then
	export SSH_AUTH_SOCK="$auth_sock"
fi

git "$@"
