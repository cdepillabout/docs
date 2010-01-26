#!/bin/bash
# This runs a sas program and takes different steps if there is an error or if no error
# This can be called like this:
# run-sas PROGRAM.sas

SAS="/usr/local/SAS/SASFoundation/9.2/bin/sas_en"
SASFLAGS="-nonews -pagesize 32767"

# make sure we get the die function and color variables
if [[ -e "${HOME}/docs/code/library.sh" ]] ; then
	source "${HOME}/docs/code/library.sh"
else
	echo "ERROR! Could not soure the shell library at \"${HOME}/docs/code/library.sh\"!"
	exit 1
fi

# make sure they give an argument of the sas program
[[ "$#" -ne "1" ]] && die "ERROR! Needs one argument, a SAS program (a .sas file)."

# make sure the file exists
[[ ! -f "$1" ]] && die "ERROR! File \"$1\" does not exist!" 

sas_file="$1"

# make sure the file ends in .sas
[[ "${sas_file##*.}" != "sas" ]] && die "ERROR! File \"${sas_file}\" does not end in .sas!"

# make sure we can find our sas executable
[[ ! -e "$SAS" ]] && die "ERROR! Could not find the SAS executable in ${SAS}"

# try running sas on our file, and see if we get any errors
${SAS} -nonews -pagesize 32767 "${sas_file}"

sas_return_code="$?"

output_log="${sas_file%.*}.log"

# make sure there is an output log
[[ ! -e "$output_log" ]] && die "WEIRD ERROR! No output log \"$output_log\" created."

# if there is an error with the program, print warning
if [[ "${sas_return_code}" > "0" ]] ; then
	echo
	echo -e "${BOLD_RED_TEXT}Error in Program${RESET_TEXT}"
fi

# print the output log
echo
echo -e "${BOLD_BLUE_TEXT}$output_log${RESET_TEXT}"
cat "$output_log"

# if there is the output list, print it and back it up
output_lst="${sas_file%.*}.lst"
if [[ -e "$output_lst" ]] ; then
	echo
	echo -e "${BOLD_BLUE_TEXT}$output_lst${RESET_TEXT}"
	cat "$output_lst"

	# back it up so that it won't show up on the next invocation of this program unless 
	# it is created by sas again.
	mv "${output_lst}" "${output_lst}.bak"
fi

