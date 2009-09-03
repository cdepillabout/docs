#!/bin/bash
#
# This file sources .bashrc and runs its arguments.
# This is used to run files from cron that require specific
# envvars to be set.

# This is actually really confusing.  My .bashrc tests if the shell is
# interactive, and if it is not, it just exits.  So we cannot simply do 
# source ~/.bashrc, but we must run it under an interactive bash process.
# Then we run the command passed in on the command line.

# WARNING: this will probably not work if passed -e, -E, or -N
# because echo reads them.
echo "source ~/.bashrc; $@ " | bash -i -s

