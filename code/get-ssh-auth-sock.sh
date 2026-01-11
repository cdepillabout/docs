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

set -euo pipefail

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
shopt -s extglob nullglob

potential_agent_file_locations=(
  /tmp/ssh-+([A-Za-z0-9 ])/agent.+([0-9])
  "$HOME/.ssh/agent"/s.+([A-Za-z0-9]).agent.+([A-Za-z0-9])
)

# pick the most recently created/modified *socket*
auth_sock=""
best_mtime=-1
for f in "${potential_agent_file_locations[@]}"; do
  # make sure it is a socket
  [[ -S "$f" ]] || continue

  mtime=$(stat -c %Y -- "$f" 2>/dev/null) || continue
  if (( mtime > best_mtime )); then
    best_mtime=$mtime
    auth_sock=$f
  fi
done

# make sure there is an auth sock
if [ -z "$auth_sock" ] ; then
  [ ! "$QUIET" ] && echo "ERROR: no auth sock available"
  exit 1
fi

[ "$VERBOSE" ] && echo "using auth sock: $auth_sock" >&2

echo "$auth_sock"
exit 0
