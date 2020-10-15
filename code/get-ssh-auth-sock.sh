#!/usr/bin/env bash
#
# SSH Get Auth Sock
#
# This is a command to automatically figure out the corrrct value of the
# SSH_AUTH_SOCK env var.
#
# If "-q" is passed, then nothing will be output on error.
# If "-v" is passed, then the auth sock that is currently being used
# will be output.
#
# This is needed when connecting to a previously running
# screen session, since the SSH_AUTH_SOCK variable will be the
# previous one we were using.  We need a new updated one
# for this new connection.

QUIET=""
VERBOSE=""

for arg in "$@" ; do
	if [[ "$arg" == '-h' || "$arg" == "--help" ]] ; then
		echo "Usage: ${FUNCNAME[0]} [-h] [-v]"
		echo "Figure out the correct SSH_AUTH_SOCK environment variable."
		exit 0
	elif [[ "$arg" == '-v' || "$arg" == "--verbose" ]] ; then
		VERBOSE=1
	elif [[ "$arg" == '-q' || "$arg" == "--quiet" ]] ; then
		QUIET=1
	else
		echo "Unknown argument \"${arg}\"" >&2
	fi
done

# get all the existing agent files
shopt -s extglob
agent_files="$(ls -rt /tmp/ssh-+([A-Za-z0-9 ])/agent.+([0-9]) 2>/dev/null)"

# make sure there actually are auth socks to get
if [ -z "$agent_files" ] ; then
	[ ! "$QUIET" ] && echo "ERROR: no auth sock available"
	exit 1
fi

# get the most recently added ssh auth sock
auth_sock="$(echo "$agent_files" | tail -1)"
[ "$VERBOSE" ] && echo "using auth sock: $auth_sock" >&2

echo "$auth_sock"
exit 0
