#!/bin/sh these are functions for bash scripts

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
