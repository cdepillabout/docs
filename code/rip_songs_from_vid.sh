#!/bin/bash
#
# This program takes videos on command line,
# rips the audio, then encodes them to ogg files.

source ~/docs/code/lib/library.sh

function usage {
	echo "Usage: $0 VIDEO ..."
}

[ -n "$1" ] || die "Error! Need at least one argument."


for my_file in "$@"
do
	mplayer -ao pcm -vo null $my_file
	wav_my_file=${my_file%.*}.wav
	mv audiodump.wav $wav_my_file
	oggenc --quality 5 $wav_my_file
	rm $wav_my_file
done

