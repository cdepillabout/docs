#!/bin/bash 
#these are functions and vars for bash scripts


# these are foreground colors
BLACK_TEXT='\E[30m'
RED_TEXT='\E[31m'
GREEN_TEXT='\E[32m'
YELLOW_TEXT='\E[33m'
BLUE_TEXT='\E[34m'
MAGENTA_TEXT='\E[35m'
CYAN_TEXT='\E[36m'
WHITE_TEXT='\E[37m'

# bold color output 
BOLD_TEXT='\E[01m'
BOLD_BLACK_TEXT="${BOLD_TEXT}${BLACK_TEXT}"
BOLD_RED_TEXT="${BOLD_TEXT}${RED_TEXT}"
BOLD_GREEN_TEXT="${BOLD_TEXT}${GREEN_TEXT}"
BOLD_YELLOW_TEXT="${BOLD_TEXT}${YELLOW_TEXT}"
BOLD_BLUE_TEXT="${BOLD_TEXT}${BLUE_TEXT}"
BOLD_MAGENTA_TEXT="${BOLD_TEXT}${MAGENTA_TEXT}"
BOLD_CYAN_TEXT="${BOLD_TEXT}${CYAN_TEXT}"
BOLD_WHITE_TEXT="${BOLD_TEXT}${WHITE_TEXT}"

RESET_TEXT='\E[00m'

# exit after printing first argument to this function
function die {
	# echo the first argument
	echo $1

	exit 1
}

function is_a_num {
	if [ -n "$(echo $1 | egrep '^[0-9]+$')" ]
	then
		echo "yes"
	else
		echo "no"
	fi
}


# This can be used for debugging output.
# To turn on debugging, use this command:
# _DEBUG="on"
# To turn off debuggin, use this command:
# _DEBUG="off"
# It is used like so:
# DEBUG echo "here is some debugging output."
function DEBUG()
{
	# The && and || have the same precedence, and are evaluated from
	# right to left. $@ expands to all the args passed to this function, 
	# so namely the command you want to run. : always evaluates to 0 (true).  
	# I'm not sure why you need this?
	[[ "$_DEBUG" == "on" || "$_DEBUG" == "yes" ]] &&  $@ || :
}
