#!/bin/bash
#
# Fetch all remotes from command line.
# git fetch-all <REMOTE> [<REMOTE>...]

if [ $# -lt 1 ] ; then
	echo "Usage: $0 <REMOTE> [<REMOTE>...]"
	exit 1
fi

for remote in "$@" ; do
	git fetch "$remote"
done

