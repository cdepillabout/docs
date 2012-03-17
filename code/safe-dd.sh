#!/bin/bash
# A safe version of dd that will not overwrite the output file if it already exists.

source ~/docs/code/lib/library.sh

basename="$(basename $0)"

# print usage
function usage {
	beg="$(dd --help | head -2)"
	end="$(dd --help | tail -n +3)"
	newbeg="${beg//dd/$basename}"

	echo "This is a safe version of dd that will not overwrite an output file. Below is dd's help."
	echo
	echo "$newbeg"
	echo "$end"
}

#echo "all options: $@"

# make sure the output file doesn't already exist.
for arg in "$@" ; do
	#echo "arg: \"$arg\""
	if [ "${arg:0:3}" == "of=" ] ; then
		filename="${arg:3}"
		if [ -e "${filename}" ] ; then
			die "ERROR! \"$filename\" already exists."
		fi
	fi
done

dd "$@"
