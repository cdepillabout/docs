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

RATIO_4_3="$(echo "4 / 3" | bc -l)"
RATIO_16_9="$(echo "16 / 9" | bc -l)"

[[ "$#" -ne "1" ]] && die "Error! Please supply the input movie."
input_movie="$1"

# try to parse the filename to find the video size.
echo "$input_movie" | grep -P "\d{3,}(x|X)\d{3,}" 1>/dev/null 2>&1
return_code="$?"
[[ "$return_code" -ne "0" ]] && die "Error! Could not find old video size in file name."

# get the old video ratio
full_ratio_string="$(echo "$input_movie" | grep -Po "\d{3,}(x|X)\d{3,}")"
input_movie_width="$(echo "$full_ratio_string" | grep -Po "\d{3,}(x|X)" | grep -Po "\d{3,}")"
input_movie_height="$(echo "$full_ratio_string" | grep -Po "(x|X)\d{3,}" | grep -Po "\d{3,}")"
input_movie_ratio="$(echo "${input_movie_width} / ${input_movie_height}" | bc -l)"


#close_to $input_movie_ratio $RATIO_4_3 0.1
#close_to $input_movie_ratio $RATIO_16_9 0.1

# get the new reduced video ratio (based on the old one)
if [[ `close_to $input_movie_ratio $RATIO_4_3 0.1` ]] ; then
	new_file_width="640"
	new_file_height="480"
elif [[ `close_to $input_movie_ratio $RATIO_16_9 0.1` ]] ; then
	new_file_width="704"
	new_file_height="396"
else
	DEBUG echo "movie ratio is ${input_movie_width}/${input_movie_height}:" \
		" ${input_movie_ratio} "
	die "Error! Could not find the correct ratio for the input file $input_movie"
fi

new_file_name="$(echo "$input_movie" | \
	sed -e 's/[0-9]\{3,\}\([Xx]\)[0-9]\{3,\}/'${new_file_width}'\1'${new_file_height}'/')"

function get_codec 
{
	input_movie="$1"

	echo "$input_movie" | \
		grep -Po "\(\d{3,}(x|X)\d{3,} .*?\)" | \
		sed -r 's/\([0-9]{3,}(x|X)[0-9]{3,} (.*?)\)/\2/'
}

# get the old codec and replace with the new codec
function change_codec 
{
	input_movie="$1"
	new_codec="$2"

	old_codec="$(get_codec "$input_movie")"

	if [ -n "$old_codec" ]
	then
		echo "$input_movie" | \
			sed -r 's/\(([0-9]{3,}(X|x)[0-9]{3,}) '"$old_codec"'\)/\(\1 '"$new_codec"'\)/'
	else
		echo "$input_movie"
	fi

}

new_file_name="$(change_codec "$new_file_name" "FMP4")"

function change_extension
{
	input_movie="$1"
	extensionless_input_movie="${input_movie%.*}"
	new_extension="$2"

	echo "${extensionless_input_movie}.${new_extension}"
}

new_file_name="$(change_extension "$new_file_name" "avi")"

# replace the extension with .avi

DEBUG echo "new file name is ${new_file_name}. starting encoding..."

#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts preset=standard -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy "$new_file_width" 
#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts preset=standard -ovc lavc -lavcopts vcodec=mpeg4 -vf scale="$new_file_width":"$new_file_height" -ofps 25
mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts mode=2:cbr:br=128 -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale="$new_file_width":"$new_file_height" -ofps 25
#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts mode=2:cbr:br=128 -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale="$new_file_width":"$new_file_height" -ffourcc XVID

#mencoder dvd://$TITLE -alang en -oac mp3lame -lameopts br=320:cbr -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy 800 -o $FILE.avi
