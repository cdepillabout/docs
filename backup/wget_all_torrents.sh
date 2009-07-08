#!/bin/sh
#
# This will wget a series of torrent files, incrementing the
# number on each successive wget.  It infers the first number
# and will get all of the torrents in the set.
# It takes a URL such as
# http://d-addicts.net/torrents/atashinchi%20no%20danshi%20ep10%20%28704x396%20xvid%201.2%29.torrent

#set -x

# make sure there is only one argument
if [ "$#" != "1" ]
then
	echo "ERROR! Only takes one argument--a url to a torrent file."
	exit 1
fi

example_url=$1

function wget_url {
	example_url=$1
	replace_pattern=$2

	for num in `seq 1 20`
	do
		formatted_num=`printf %02d ${num}`
		# get rid of place holder
		evaled_replace_pattern=`eval echo ${replace_pattern}`
		url=`echo ${example_url} | sed -e "s/%20[Ee][Pp][0-9][0-9]/%20${evaled_replace_pattern}/"`
		#url=`echo ${example_url} | sed -e "s/ep[0-9][0-9]/ep${formatted_num}/"`
		wget $url
	done
}

wget_url ${example_url} 'ep${formatted_num}'
wget_url ${example_url} 'ep${formatted_num}%20finale'

wget_url ${example_url} 'Ep${formatted_num}'
wget_url ${example_url} 'Ep${formatted_num}%20finale'

#wget_url ${example_url} 'eP${formatted_num}'
#wget_url ${example_url} 'eP${formatted_num}%20finale'

wget_url ${example_url} 'EP${formatted_num}'
wget_url ${example_url} 'EP${formatted_num}%20finale'

