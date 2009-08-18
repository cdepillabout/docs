#!/bin/bash
#
# Use the expand command to expand a bunch of files in place, without
# having to redirect to a seperate file.

source ~/docs/code/lib/library.sh

function usage {
	echo "Usage: $0: same as \`expand\` except the FILE ... part is required."
	echo "This program will expand FILE ... in place."
	echo "For the -t option, only numbers are supported, not lists."
	echo
	expand --help
} 

# use getopts to get all of our variables
GETOPT_TEMP=`getopt -o it:hv --long initial,tabs:,help,version \
     -n $0 -- "$@"`

if [ $? != 0 ]
then 
	echo
	usage
	exit 1
fi

# Note the quotes around `$TEMP': they are essential!
eval set -- "$GETOPT_TEMP"

I_OPTION=""
T_OPTION=""

while true ; do
	case "$1" in
		-i|--initial) 
			I_OPTION="-i"
			shift 
			;;

		-t|--tabs) 
			T_OPTION="-t ${2}"
			shift 2 
			;;

		-h|--help)
			usage
			exit 1
			;;

		-v|--version)
			expand --version
			exit 1
			;;

		--) 
			shift
			break 
			;;

		*) 
			echo "Error!"
			usage
			exit 1 
			;;
	esac
done

# expand all the files from the command line
for arg do 
	echo "Expanding ${arg}"
	[[ -f "$arg" ]] || die "Error: no file named ${arg}"
	temp_file=`tempfile`
	expand $I_OPTION $T_OPTION ${arg} > ${temp_file}
	mv ${temp_file} ${arg}
done
