#!/bin/bash
# Create the proper links from ~/ to my dot files.

for dot_file in dot_* 
do
	ln -sf `pwd`/${dot_file} ~/.${dot_file#dot_}
done

