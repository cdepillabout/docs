#!/usr/bin/env python

import sys
import subprocess
import re

def usage():
	print "Usage: " + sys.argv[0] + " AUDIO_FILE ... "
	print "Print the length of audio files."

def print_file_info(length, file):
	print length,
	for i in range (8 - (len(str(length)))):
		print " ",
	print file

def main():

	if len(sys.argv) <= 1:
		usage()
		sys.exit()
	
	total_length = float(0)

	for file in sys.argv[1:]:
		midentify_output = \
			subprocess.Popen(["midentify", file], stdout=subprocess.PIPE).communicate()[0]
		id_length_string = re.search("ID_LENGTH=(\d|\.)+", midentify_output).group()
		length = float(re.search("(\d|\.)+", id_length_string).group())
		#print_file_info(length, file)
		total_length += length

	secs = total_length % 60
	mins = int(total_length / 60)

	print str(mins) + ":" + str(secs)

if __name__ == '__main__':
	main()
