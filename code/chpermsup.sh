#!/usr/bin/env bash



function usage () {
	echo "USAGE: $0 <set | check> <MODE> <PATH>"
	echo ""
	echo "\"set\" will set the mode."
	echo "\"check\" will return 0 if the mode matches and 1 if the mode does not match."
	echo ""
	echo "MODE should be in the form of [ugo]=rwx."
	echo "For now, you must use \"=\".  You cannot use \"+\" or \"-\"."
	echo "Here are some examples:"
	echo "  u=r--	the user can read, but can't write or execute"
	echo "  g=---   group has no permisions"
	echo "  o=rwx   other users have all permissions"
}

function die () {
	message="$1"

	echo "ERROR: ${message}"
	exit 2
}

####
# HERE IS A DIRTY REGEX THAT CHECKS IF MODE IS A LEGIT MODE
# TODO: Make this work well. Allow modes like g+r or a-x
####
function checkmode () {
	mode_to_check="$1"
	match_amount=$(expr match "${mode_to_check}" "[ugo]=[r-][w-][x-]")

	if [ "${match_amount}" -ne 5 ] ; then
		die "${mode_to_check} is not a valid mode."
	fi
}

function get_specific_perms () {
	complete_mode="$1"
	full_perms="$2"

	# read the first character of the mode to get whether we are
	# looking at the user, group, or other perms
	perms_type="${complete_mode:0:1}"

	# return the part of full_perms that we are interested in
	if [ "${perms_type}" == "u" ] ; then
		echo "${full_perms:1:3}"
	elif [ "${perms_type}" == "g" ] ; then
		echo "${full_perms:4:3}"
	elif [ "${perms_type}" == "o" ] ; then
		echo "${full_perms:7:3}"
	else
		die "Internal error, couldn't get the correct perms_type: $perms_type"
	fi
}

# Check that 3 arguments have been passed in.
if [ "$#" -ne 3 ]; then
	usage
	exit 1
fi

# get our variables
operation="$1"
mode="$2"
path="$3"

# this checks that the operation is set correctly
if [ "${operation}" == "-c" -o "${operation}" == "--check" -o "${operation}" == "check" ]; then
	operation="check"
elif [ "${operation}" == "-s" -o "${operation}" == "--set" -o "${operation}" == "set" ]; then
	operation="set"
else
	die "Must specify either 'set' or 'check'."
fi

# check that mode is a legitimate mode
checkmode "${mode}"

# check that path is a legitimate path
orig_path="${path}"
path=$(readlink -e "${path}")
if [ "$?" -ne 0 ]; then
	die "File ${orig_path} does not exist. (Could not "\`"readlink"\`" it.)"
fi

# check to make sure that the path returned by readlink is actually an absolute path.
# (I don't think this check is really necessary.)
if [[ "${path}" != /* ]]
then
	die "Could not determine absolute path of ${path}."
fi


while [[ "${path}" != "/" ]]
do
	path_full_perms=$(stat -c "%A" "${path}")
	path_specific_perms=$(get_specific_perms "${mode}" "${path_full_perms}")

	# check to make sure that the specific perms of this path are
	# equal to the perms we are trying to set.
	if [ "${path_specific_perms}" != "${mode:2:3}" ] ; then
		if [ "${operation}" == "set" ] ; then
			#echo "setting path ($path) to mode ($mode)"

			# mode can't have the "-" characters in it
			# (TODO: chmod can throw an error #130 if it doesn't have perms
			# to change the perms on a directory.  Should this be caught?)
			chmod "${mode//-/}" "${path}"
		elif [ "${operation}" == "check" ] ; then
			exit 1
		fi
	fi

	path=$(dirname -- "${path}")
done

exit 0
