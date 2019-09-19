#!/usr/bin/env bash
#
# Print out the time I've been using the computer today by calling arbtt-stats.

# output everything with the active tag today
arbtt-stats --filter='$date >= '`date '+%Y-%m-%d'` --output-only=active --output-format=csv 2>/dev/null | \
	# get only the line with the active tag
	grep active | \
	# get only the column with the total time
	cut -d',' -f2 | \
		# remove the seconds (leaving just the hours and minutes)
	sed -e 's/:00$//'
