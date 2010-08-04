#!/bin/bash
# mencoder_704x396
# Convert a movie to a 704x396 avi.

source ~/docs/code/library.sh

# takes two numbers as arguments and says whether they are close
# or not (based on some allowed difference).  So a sample function
# call could be `close_to 1.1 1.2 0.1`, which would return "yes"
function close_to()
{
	num_a=$1
	num_b=$2
	allowed_difference=$3

	difference=`echo "x = $num_a - $num_b ; if (0 > x) x *= -1; x" | bc -l`
	echo $difference $allowed_difference | awk '{if ($1 <= $2) print "yes";}'
}


[[ "$#" -ne "1" ]] && die "Error! Please supply the input movie."
input_movie="$1"

# change the ratio of the movie
# Ratio comes in on $1.
function change_ratio 
{

	local input_movie="$1"

	local RATIO_4_3="$(echo "4 / 3" | bc -l)"
	local RATIO_16_9="$(echo "16 / 9" | bc -l)"

	# try to parse the filename to find the video size.
	echo "$input_movie" | grep -P "\d{3,}(x|X)\d{3,}" 1>/dev/null 2>&1
	local return_code="$?"
	# if we can't find the video size, then we can just 
	# exit this function and return the original filename.
	if [[ "$return_code" -ne "0" ]] 
	then
		DEBUG echo "ERROR! Could not find ratio for movie..." >&2
		echo "$input_movie"
		return
	fi

	# get the old video ratio
	local full_ratio_string="$(echo "$input_movie" | grep -Po '\d{3,}(x|X)\d{3,}')"
	local input_movie_width="$(echo "$full_ratio_string" | grep -Po '\d{3,}(x|X)' | grep -Po '\d{3,}')"
	local input_movie_height="$(echo "$full_ratio_string" | grep -Po '(x|X)\d{3,}' | grep -Po '\d{3,}')"
	local input_movie_ratio="$(echo "${input_movie_width} / ${input_movie_height}" | bc -l)"


	# get the new reduced video ratio (based on the old one)
	if [[ `close_to $input_movie_ratio $RATIO_4_3 0.1` ]] ; then
		new_file_width="640"
		new_file_height="480"
	elif [[ `close_to $input_movie_ratio $RATIO_16_9 0.1` ]] ; then
		new_file_width="704"
		new_file_height="396"
	else
		DEBUG echo "movie ratio is ${input_movie_width}/${input_movie_height}:" \
			" ${input_movie_ratio} " >&2
		DEBUG echo "Error! Could not find the correct ratio for the input file $input_movie" >&2
		exit 1
	fi

	echo "$input_movie" | \
		sed -e 's/[0-9]\{3,\}\([Xx]\)[0-9]\{3,\}/'${new_file_width}'\1'${new_file_height}'/'

}

# get the old codec and replace with the new codec.
# Take the input filename on $1 and the new codec on $2
function change_codec 
{
	local input_movie="$1"
	local new_codec="$2"

	# get the old codec
	local old_codec_paren="$(echo "$input_movie" | grep -Po '\(\d{3,}(x|X)\d{3,} .*?\)')"
	local old_codec="$(echo "$old_codec_paren" | sed -r 's/\([0-9]{3,}(x|X)[0-9]{3,} (.*?)\)/\2/')"

	if [ -n "$old_codec" ]
	then
		echo "$input_movie" | \
			sed -r 's/\(([0-9]{3,}(X|x)[0-9]{3,}) '"$old_codec"'\)/\(\1 '"$new_codec"'\)/'
	else
		DEBUG echo "ERROR! Could not find codec!" >&2
		echo "$input_movie"
	fi

}

# change the extension of a file. take the input file on $1 and the
# new extension on $2.
function change_extension
{
	local input_movie="$1"
	local extensionless_input_movie="${input_movie%.*}"
	local new_extension="$2"

	echo "${extensionless_input_movie}.${new_extension}"
}

# replace the ratio with the correct one
new_file_name="$(change_ratio "$input_movie")"
DEBUG echo "new file name after ratio change: $new_file_name"

# replace the codec with FMP4
new_file_name="$(change_codec "$new_file_name" "FMP4")"
DEBUG echo "new file name after codec change: $new_file_name"

# replace the extension with .avi
new_file_name="$(change_extension "$new_file_name" "avi")"
DEBUG echo "new file name after extension change: $new_file_name"

# if we haven't made any changes, we have to append something to the
# end of the file so that the old file is not overwritten
if [ "$new_file_name" == "$input_movie" ]
then
	new_file_name="${new_file_name}-smaller"
fi

DEBUG echo "final new file name is ${new_file_name}. starting encoding..."

# TODO: rename subtitle file as well


#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts preset=standard -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy "$new_file_width" 
#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts preset=standard -ovc lavc -lavcopts vcodec=mpeg4 -vf scale="$new_file_width":"$new_file_height" -ofps 25

#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts mode=2:cbr:br=128 -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale="$new_file_width":"$new_file_height" -ofps 25
# I was getting weird errors when I tried to use "-ofps 25"
mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts mode=2:cbr:br=128 -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale="$new_file_width":"$new_file_height" 

#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts mode=2:cbr:br=128 -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale="$new_file_width":"$new_file_height" -ffourcc XVID

#mencoder dvd://$TITLE -alang en -oac mp3lame -lameopts br=320:cbr -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy 800 -o $FILE.avi
