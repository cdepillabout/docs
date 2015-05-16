#!/usr/bin/env bash
# Create the proper links from ~/ to my dot files.

# change directory to this file
cd "$( dirname "${BASH_SOURCE[0]}" )"
ret="$?"
if [ "$ret" -ne "0" ] ; then
	echo "ERROR! could not cd to the script's directory!"
	exit 1
fi
SCRIPT_DIR="$( pwd )"

source ../code/lib/library.sh

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
super_link $SCRIPT_DIR/dot_bashrc $HOME/.bashrc

# screen files
super_link $SCRIPT_DIR/dot_screenrc $HOME/.screenrc

# emacs files
super_link $SCRIPT_DIR/dot_emacs $HOME/.emacs
make_directory $HOME/.emacs-backups/
make_directory $HOME/.emacs.d/
super_link $SCRIPT_DIR/dot_emacs.d/local.el $HOME/.emacs.d/local.el

# xmonad files
make_directory $HOME/.xmonad
super_link $SCRIPT_DIR/dot_xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
super_link $SCRIPT_DIR/dot_xmonad/conky-rc $HOME/.xmonad/conky-rc

# xmobar files
#super_link $SCRIPT_DIR/dot_xmobarrc $HOME/.xmobarrc

# mplayer files
make_directory $HOME/.mplayer
super_link $SCRIPT_DIR/dot_mplayer/config $HOME/.mplayer/config
super_link $SCRIPT_DIR/dot_mplayer/input.conf $HOME/.mplayer/input.conf

# fluxbox files
make_directory $HOME/.fluxbox
super_link $SCRIPT_DIR/dot_fluxbox/keys $HOME/.fluxbox/keys

# vim files
super_link $SCRIPT_DIR/dot_vimrc $HOME/.vimrc
make_directory $HOME/.vim-tmp
make_directory $HOME/.vim-undo
make_directory $HOME/.vim
make_directory $HOME/.vim/after
make_directory $HOME/.vim/after/syntax
super_link $SCRIPT_DIR/dot_vim/after/syntax/sas.vim $HOME/.vim/after/syntax/sas.vim
super_link $SCRIPT_DIR/dot_vim/after/syntax/java.vim $HOME/.vim/after/syntax/java.vim
make_directory $HOME/.vim/after/indent
super_link $SCRIPT_DIR/dot_vim/after/indent/java.vim $HOME/.vim/after/indent/java.vim
make_directory $HOME/.vim/spell
super_link $SCRIPT_DIR/dot_vim/spell/en.utf-8.add $HOME/.vim/spell/en.utf-8.add

# vimperator
super_link $SCRIPT_DIR/dot_vimperatorrc $HOME/.vimperatorrc

# muttator
super_link $SCRIPT_DIR/dot_muttatorrc $HOME/.muttatorrc

# pentadact
super_link $SCRIPT_DIR/dot_pentadactylrc $HOME/.pentadactylrc

# eclim
super_link $SCRIPT_DIR/dot_eclimrc $HOME/.eclimrc

# git
super_link $SCRIPT_DIR/dot_gitconfig $HOME/.gitconfig

# mercurial
super_link $SCRIPT_DIR/dot_hgrc $HOME/.hgrc

# Xmodmaps
super_link $SCRIPT_DIR/dot_Xmodmap_japanese_keyboard $HOME/.Xmodmap_japanese_keyboard
super_link $SCRIPT_DIR/dot_Xmodmap_swap_ctrl_caps $HOME/.Xmodmap_swap_ctrl_caps

# gdbinit
super_link $SCRIPT_DIR/dot_gdbinit $HOME/.gdbinit

# hirc (This creates a haskell project based on a template)
super_link $SCRIPT_DIR/dot_hirc $HOME/.hirc

# ctags
super_link $SCRIPT_DIR/dot_ctags $HOME/.ctags

# radare2
super_link $SCRIPT_DIR/dot_radare2rc $HOME/.radare2rc




# create a ~/.bashrc_local
[ ! -f "$HOME/.bashrc-local" ] && touch "$HOME/.bashrc-local"


# ask if we will add umask 022 to bashrc-local
grep "umask" "$HOME/.bashrc-local" 2>/dev/null 1>&2
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


# Make sure either a ~/.bash_profile or ~/.profile exists.  Usually
# this file will source ~/.bashrc, so all my settings will get loaded.
if [ ! \( -e "$HOME/.bash_profile" -o -e "$HOME/.profile" \) ] ; then
	echo "Creating ~/.profile and making it source ~/.bashrc."
	echo -e '\n[ -e "$HOME/.bashrc" ] && source "$HOME/.bashrc"' >> $HOME/.profile
fi

# check to make sure git is installed
command -v "git" &>/dev/null
git_check_ret="$?"

# check to make sure vim is installed
command -v "vim" &>/dev/null
vim_check_ret="$?"

if [ "${git_check_ret}" = "0" -a "${vim_check_ret}" = 0 ] ; then
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
		echo "There may be extra files that need to be installed by hand"
		echo "in order to get all the vim bundles working correctly."
		echo "Read the top of .vimrc and install everything that is needed"
		echo "(this is stuff like ghc-mod, hlint, etc)."
		echo
		echo "You might want to install things with a cabal command like this:"
		echo "(Remember, you might need to source your .bashrc to get the"
		echo "~/.cabal/bin directory added to the path)"
		echo
		echo "\`cabal install happy -j8 && source ~/.bashrc && cabal install ghc-mod hoogle hlint -j8\`"
	fi
else
	echo "ERROR! \`git\` or \`vim\` does not exist. Not installing vim bundles."
fi
