#!/usr/bin/env bash
# Take a screenshot using imagemagik and then copy it
# to the clipboard.

# make a temp directory to store our image in
scriptbasename=$(basename "$0")
tempdir=$(mktemp -dt "${scriptbasename}.XXXXXXXXXXXX")
imagepath="${tempdir}/image.png"
# Remove the temporary directory when the script finishes
trap '[ -n "$tempdir" ] && rm -rf "$tempdir"' EXIT

# create the screen shot
import "${imagepath}"

# copy the screen shot to the clipboard
copy-image-to-clipboard "${imagepath}"
