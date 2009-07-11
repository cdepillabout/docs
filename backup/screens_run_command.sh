#!/bin/sh
#
# This will run a command on multiple windows in the current screen session.
#
# Warning: you need to escape strings that you are using in commands.
# For instance, if you have a file "hello world" and you wanted to list it
# on screen 4, the correct command would be
# 
# $ screens_run_command.sh 4 ls \"hello world\"


source ~/docs/backup/library.sh


# print usage
function usage {
	echo "Usage: $0 STARTING_NUM [ENDING_NUM] COMMAND ..."
}


# get the starting number
starting_window=$1
[ `is_a_num ${starting_window}` = "yes" ] || die "First argument ${1} is not a number."
shift

# get the final number
final_window=$starting_window
if [ `is_a_num ${1}` = "yes" ] 
then
	final_window=${1}
	shift
fi

# escape " and \ 
# (I use this later when I use "$my_args" instead of "$@")
my_args=""
for argument in "$@"
do
	argument=${argument//\\/\\\\}
	argument=${argument//\"/\\\"}
	my_args+="\"$argument\" "
done
echo "my_string: $my_args"
#eval $my_string

for num in `seq $starting_window $final_window`
do
	screen -X at $num stuff "$my_args"\ $'\n'
done
