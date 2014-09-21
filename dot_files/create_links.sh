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

# xmobar files
super_link $HOME/docs/dot_files/dot_xmobarrc $HOME/.xmobarrc

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

# gdbinit
super_link $HOME/docs/dot_files/dot_gdbinit $HOME/.gdbinit

# hirc (This creates a haskell project based on a template)
super_link $HOME/docs/dot_files/dot_hirc $HOME/.hirc

# ctags
super_link $HOME/docs/dot_files/dot_ctags $HOME/.ctags



# create a ~/.bashrc_local
[ ! -f "$HOME/.bashrc-local" ] && touch "$HOME/.bashrc-local"

# ask if we will add umask 022 to bashrc
grep umask "$HOME/.bashrc-local" 2>/dev/null 1>&2
does_umask_exist_in_bashrc_local="$?"
[ "${does_umask_exist_in_bashrc_local}" != 0 ] && \
	read -r -p "Do you want to add \`umask 022\` to $HOME/.bashrc-local? [y/N] " response
if [ "${does_umask_exist_in_bashrc_local}" == "0" ] ; then
	# umask is already in bashrc_local, so do nothing...
	:
elif [[ "${response}" =~ ^([yY][eE][sS]|[yY])$ ]] ; then
	# adding valid umask to bashrc-local
	echo -e '\numask 0022' >> $HOME/.bashrc-local
else
	# adding a commented out umask to bashrc-local
	echo -e '\n#umask 0022' >> $HOME/.bashrc-local
fi


# install vim vundles if it doesn't already exist and we are not root
if [ ! -d "$HOME/.vim/bundle/vundle/.git" -a "$UID" != "0" ] ; then
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
	echo "\`cabal install happy -j8 && source ~/.bashrc && cabal install ghc-mod hoogle hlint -j8\`"
fi
