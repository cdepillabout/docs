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


# bashrc files
super_link $HOME/docs/dot_files/dot_bashrc $HOME/.bashrc

# screen files
super_link $HOME/docs/dot_files/dot_screenrc $HOME/.screenrc

# emacs files
super_link $HOME/docs/dot_files/dot_emacs $HOME/.emacs
make_directory $HOME/.emacs-backups/
make_directory $HOME/.emacs.d/
super_link $HOME/docs/dot_files/dot_emacs.d/local.el $HOME/.emacs.d/local.el

# xmonad files
make_directory $HOME/.xmonad
super_link $HOME/docs/dot_files/dot_xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
super_link $HOME/docs/dot_files/dot_xmonad/conky-rc $HOME/.xmonad/conky-rc

# mplayer files
make_directory $HOME/.mplayer
super_link $HOME/docs/dot_files/dot_mplayer/config $HOME/.mplayer/config
super_link $HOME/docs/dot_files/dot_mplayer/input.conf $HOME/.mplayer/input.conf

# fluxbox files
make_directory $HOME/.fluxbox
super_link $HOME/docs/dot_files/dot_fluxbox/keys $HOME/.fluxbox/keys

# vim files
super_link $HOME/docs/dot_files/dot_vimrc $HOME/.vimrc
make_directory $HOME/.vim-tmp
make_directory $HOME/.vim
make_directory $HOME/.vim/after
make_directory $HOME/.vim/after/syntax
super_link $HOME/docs/dot_files/dot_vim/after/syntax/sas.vim $HOME/.vim/after/syntax/sas.vim
super_link $HOME/docs/dot_files/dot_vim/after/syntax/java.vim $HOME/.vim/after/syntax/java.vim
make_directory $HOME/.vim/after/indent
super_link $HOME/docs/dot_files/dot_vim/after/indent/java.vim $HOME/.vim/after/indent/java.vim

# vimperator
super_link $HOME/docs/dot_files/dot_vimperatorrc $HOME/.vimperatorrc

# pentadact
super_link $HOME/docs/dot_files/dot_pentadactylrc $HOME/.pentadactylrc

# eclim
super_link $HOME/docs/dot_files/dot_eclimrc $HOME/.eclimrc

# git
super_link $HOME/docs/dot_files/dot_gitconfig $HOME/.gitconfig

# mercurial
super_link $HOME/docs/dot_files/dot_hgrc $HOME/.hgrc

# Xmodmaps
super_link $HOME/docs/dot_files/dot_Xmodmap_japanese_keyboard $HOME/.Xmodmap_japanese_keyboard
super_link $HOME/docs/dot_files/dot_Xmodmap_swap_ctrl_caps $HOME/.Xmodmap_swap_ctrl_caps

# install vim vundles if it doesn't already exist
if [ ! -d "$HOME/.vim/bundle/vundle/.git" ] ; then
	echo "cloning vundle..."
	echo
	mkdir -p "$HOME/.vim/bundle"
	git clone https://github.com/gmarik/vundle.git $HOME/.vim/bundle/vundle
	echo
	echo "installing all bundles by opening vim and running :BundleInstall"
	vim +BundleInstall +qall
	echo
	echo "There may be extra files that need to be installed by hand in order"
	echo "to get all the vim bundles working correctly.  Read the top of .vimrc"
	echo "and install everything that is needed (this is stuff like ghc-mod, hlint, etc)."
	echo "You might want to install things with a cabal command like this:"
	echo "(Remember, you might need to source your .bashrc to get the ~/.cabal/bin directory"
	echo "added to the path)"
	echo
	echo "\`cabal install happy -j8 && source ~/.bashrc && cabal install ghc-mod hoogle hlint hdevtools -j8\`"
fi
