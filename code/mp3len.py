#!/usr/bin/env python2

import sys
import subprocess
import re

def usage():
	print "Usage: " + sys.argv[0] + " AUDIO_FILE ... "
	print "Print the length of audio files."

def print_file_info(length, file):
	new_length = format_time(0, length)
	print new_length,
	for i in range (8 - (len(str(new_length)))):
		print " ",
	print file

def format_time(mins, secs):
	remaining_secs = secs % 60
	extra_secs = int(secs / 60)
	mins += extra_secs
	return "%01d:%02d" % (mins, remaining_secs) 


def main():

	if len(sys.argv) <= 1:
		usage()
		sys.exit()

	total_length = float(0)

	for f in sys.argv[1:]:
		cmd = ["mplayer", "-vo", "null", "-ao", "null", "-frames", "0", "-identify", f]
		proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=open('/dev/null', 'w'))
		midentify_output = proc.communicate()[0]
		id_length_string = re.search("ID_LENGTH=(\d|\.)+", midentify_output).group()
		length = float(re.search("(\d|\.)+", id_length_string).group())
		print_file_info(length, f)
		total_length += length

	print "\ntotal: %s" % format_time(0, total_length)

if __name__ == '__main__':
	main()
