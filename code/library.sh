#!/bin/bash 
#these are functions and vars for bash scripts


# To use these colors, use a command like this:
# echo -e "${BOLD_RED_TEXT}hello${RESET_TEXT}"

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


# this will get the location of $1 (from the $PATH)
function get_location_of_file()
{
	tmp_filename=$1
	tmp_fullpath=`which ${tmp_filename} 2>/dev/null`

	if [ -n "${tmp_fullpath}" ]
	then
		echo ${tmp_fullpath}
	fi
}

# this takes an argument list of filenames and outputs
# the first one that can be found on $PATH.
# It outputs a full path to the file.
function get_correct_filename_from_choices()
{
	while ((1))
	do
		# break if there are no more arguments
		if [ -z $1 ]
		then
			break
		fi

		# get the full path of the filename
		tmp_filename=$1
		shift
		tmp_fullpath=`get_location_of_file ${tmp_filename}`

		# if this program is on $PATH, echo it's full path
		# and get out of this loop
		if [ -n "${tmp_fullpath}" ]
		then
			echo ${tmp_fullpath}
			break
		fi
	done
}

# Add a path to a $*PATH enviroment variable.  This works for $PATH,
# $PYTHONPATH, $CLASSPATH, etc.  It only adds the path if it is not already
# present.  Also, it checks to make sure this is a valid path before it
# is added.
#
# $1 is the path enviroment variable to use, "PYTHONPATH", "PATH", etc.
# $2 is the path to add.
# $3 is "back" if you want to add this path to the end, and "front" if you want to
#	 add this path to the beginning.
# $4 whether or not to check if the directory exists
function add_to_path_side()
{
	envvar="$1"
	currentval="$(eval "echo $(echo '$'$envvar)")"
	path="${2}"
	side="$3"
	checkifexists="$4"

	# remove the trailing '/' from $path, if it exists.
	path="${path%'/'}"

	if [[ "$checkifexists" = "yes" && ! -d "$path" ]] ; then
		#echo "not adding ${path} to "'$'"${envvar} because it is not a valid path" >&2
		return
	fi

	if [[ ":${currentval}:" = *":${path}:"* ]] ; then
		#echo "not adding ${path} to "'$'"${envvar} because it already is in "'$'"${envvar}" >&2
		return
	fi

	if [[ "$side" = "front" || "$side" = "beginning" ]] ; then
		export "$envvar"="${path}:${currentval}"
	elif [[ "$side" = "back" || "$side" = "end" ]] ; then
		export "$envvar"="${currentval}:${path}"
	fi
}

function add_to_path()
{
	add_to_path_side "$1" "$2" "back" "yes"
}

function add_to_path_nocheck()
{
	add_to_path_side "$1" "$2" "back" "no"
}

function prepend_to_path()
{
	add_to_path_side "$1" "$2" "front" "yes"
}

function prepend_to_path_nocheck()
{
	add_to_path_side "$1" "$2" "front" "no"
}
