#!/bin/bash

# This program creates a tmp file in /tmp/ that has one line like:
# SSH_AUTH_SOCK="/tmp/ssh-LLALALA/agent.9999"
# This program echos the name of that file on stdout.
# The file that is produced can be sourced, in order to get
# the correct ssh auth sock.
# (This is generally used only when using screen.  When disconnecting 
# from a screen session and picking it up on another computer,
# the new ssh session will have a different auth sock, so it 
# needs to be set in the environment somehow.  This program 
# provides the means for doing that.)



# create our tempfile
TMPFILE="$(mktemp /tmp/tempfile.XXXXXXXXXX)"
if [ "$?" -ne "0" ] ; then
	exit 1
fi

# get all the existing agent files
shopt -s extglob
agent_files="$(ls -Qrt /tmp/ssh-+([A-Za-z0-9 ])/agent.+([0-9]) 2>/dev/null)"
shopt -u extglob

# make sure there actually are auth socks to get
if [ -z "$agent_files" ] ; then
	exit 1
fi

# get the most recently added ssh auth sock
most_recent_auth_sock="$(echo "$agent_files" | tail -1)"
echo "SSH_AUTH_SOCK=$most_recent_auth_sock" >> "$TMPFILE"

echo "$TMPFILE"
