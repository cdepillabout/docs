#!/usr/bin/env bash
# Create the proper links from ~/ to my dot files.

source $HOME/docs/code/lib/library.sh

function super_link {
	file_name=$1
	link_target=$2

	[ ! -e $link_target -o -L $link_target ] || \
		die "ERROR! $link_target is not a link, so exiting."

	ln -sf $file_name $link_target
}

function make_directory {
	directory=$1

	if [ ! -e $directory ]
	then
		mkdir $directory
	else
		[ -d $directory ] || die "Error! $directory is not a directory"
	fi
}


super_link $HOME/docs/dot_files/dot_bashrc $HOME/.bashrc
super_link $HOME/docs/dot_files/dot_emacs $HOME/.emacs
super_link $HOME/docs/dot_files/dot_screenrc $HOME/.screenrc
super_link $HOME/docs/dot_files/dot_vimrc $HOME/.vimrc

make_directory $HOME/.xmonad
super_link $HOME/docs/dot_files/dot_xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
super_link $HOME/docs/dot_files/dot_xmonad/conky-rc $HOME/.xmonad/conky-rc

make_directory $HOME/.mplayer
super_link $HOME/docs/dot_files/dot_mplayer/config $HOME/.mplayer/config

make_directory $HOME/.fluxbox
super_link $HOME/docs/dot_files/dot_fluxbox/keys $HOME/.fluxbox/keys

#for dot_file in dot_* 
#do
#	ln -sf `pwd`/${dot_file} ~/.${dot_file#dot_}
#done

