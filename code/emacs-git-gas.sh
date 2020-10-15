#!/usr/bin/env bash
#
# This is a script to easily call the bash function git-gas.

if type git-gas >/dev/null 2>/dev/null ; then
	git-gas "$@"
else
	echo "WARNING: emacs-git-gas could not find the git-gas bash function. This "
	echo "means that emacs-git-gas was probably not run from a shell where ~/.bashrc "
	echo "has been sourced. Running normal git executable instead..."
	git "$@"
fi
