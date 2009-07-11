#!/bin/sh
# This is designed to list all the items from every table in an sqlite3 database.

source ~/docs/code/lib/library.sh

# color output 
TXT_BOLD='\E[01m'

# these are foreground colors
TXT_BLACK='\E[30m'
TXT_RED='\E[31m'
TXT_GREEN='\E[32m'
TXT_YELLOW='\E[33m'
TXT_BLUE='\E[34m'
TXT_MAGENTA='\E[35m'
TXT_CYAN='\E[36m'
TXT_WHITE='\E[37m'

TXT_RESET='\E[00m'


# print usage
function usage {
	echo "Usage: $0 [-column|-csv|-line|-list] [-header|-noheader] [databasename]"
}


database_name="sqlite3.db"
listing_method="-column"
header="-header"
color=1

for arg; 
do
	if [[ $arg == "-"* ]]
	then
		# get the options
		case $arg in
			"-column" ) listing_method="-column" ;;
			"-csv" ) listing_method="-csv" ;;
			"-html" ) listing_method="-html" ;;
			"-line" ) listing_method="-line" ;;
			"-list" ) listing_method="-list" ;;

			"-header" ) header="-header" ;;
			"-noheader" ) header="-noheader" ;;

			"-color" ) color=1 ;;
			"-nocolor" ) color=0 ;;

			* )
				usage
				exit 1
			;;
		esac
	else
		# get the database name
		database_name=$arg
	fi
done

#echo "listing_method = $listing_method"
#echo "header = $header"
#echo "database_name = $database_name"
#echo "color = $color"

# make sure the database exists
[[ -f $database_name ]] || die "ERROR! File \"$database_name\" does not exist."

# makes sure it's a database
file_output=`file -b $database_name | grep "SQLite 3.x database"`
[ -z "$file_output" ] && die "ERROR File \"$database_name\" is not an SQLite 3.x database."


# get a list of all the tables
tables=`sqlite3 $database_name .tables | sed -r -e 's/\W+/\n/g' | grep -v "^$"`

for line in $tables
do 
	# output color
	if (( $color ))
	then
		echo -ne "${TXT_RED}${TXT_BOLD}"
	fi

	echo "${line}:"

	# if there is color, go back to normal
	if (( $color )) 
	then
		echo -ne "${TXT_RESET}"
	fi

	sqlite3 $header $listing_method $database_name "select * from $line"

	echo
	echo

done
