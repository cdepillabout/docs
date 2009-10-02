#!/bin/bash
# Clean up the extraneous files that pdflatex and latex leave behind.
# These include the .aux, .log, and .out files.
# Only clean them up if there is also a tex file with the name
# we pass in.

source ~/docs/code/lib/library.sh

# print usage
function usage {
	echo "Usage: $0 FILE.TEX"
	echo
	echo "Clean up the extraneous files that TeX leaves behind"
	echo "when compileing FILE.TEX. This includes FILE.AUX,"
	echo "FILE.LOG, and FILE.OUT."
}

# make sure there is only one argument
if [ "$#" != "1" ] 
then
	echo "ERROR! Only takes one argument."
	echo
	usage
	exit 1
fi

directory=$(dirname $1)
basefilename=$(basename $1)
extension=${basefilename##*.}
filename=${basefilename%.*}

#echo $directory
#echo $basefilename
#echo $extension
#echo $filename

[ "$extension" = "tex" ] || die "ERROR! $basefilename does not end in \".tex\"."

function delete_file_if_exists {
	full_file_name=$1
	[ -f $full_file_name ] && rm $full_file_name
}

delete_file_if_exists $directory/${filename}.aux
delete_file_if_exists $directory/${filename}.log
delete_file_if_exists $directory/${filename}.out
