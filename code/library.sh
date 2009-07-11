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
