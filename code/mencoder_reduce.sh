#!/bin/bash
# mencoder_704x396
# Convert a movie to a 704x396 avi.

source ~/docs/code/library.sh

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

# get the new reduced video ratio (based on the old one)
if [[ "$input_movie_ratio" == "$RATIO_4_3" ]] ; then
	new_file_width="640"
	new_file_height="480"
elif [[ "$input_movie_ratio" == "$RATIO_16_9" ]] ; then
	new_file_width="704"
	new_file_height="396"
else
	die "Error! Could not find the correct ratio for the input file $input_movie"
fi

new_file_name="$(echo "$input_movie" | sed -e 's/[0-9]\{3,\}\([Xx]\)[0-9]\{3,\}/'${new_file_width}'\1'${new_file_height}'/')"

DEBUG echo "new file name is ${new_file_name}. starting encoding..."

#mencoder "$input_movie" -o "$new_file_name" -oac mp3lame -lameopts preset=standard -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy "$new_file_width" 
mencoder "$input_movie" -o "$new_file_name" -oac copy -ovc lavc -lavcopts vcodec=mpeg4 -vf scale="$new_file_width":"$new_file_height"

#mencoder dvd://$TITLE -alang en -oac mp3lame -lameopts br=320:cbr -ovc lavc -lavcopts vcodec=mpeg4:vhq -vf scale -zoom -xy 800 -o $FILE.avi
