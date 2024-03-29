#!/usr/bin/env bash
# Create the proper links from ~/ to my dot files.

######################
## Helper functions ##
######################

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

	# TODO: This function should also check to make sure that the $link_target
	# isn't pointing somewhere else (before overwriting it).

	[ ! -e "${link_target}" -o -L "${link_target}" ] || \
		die "ERROR! $link_target is not a link, so exiting."

	# if the link target is a symbolic link and a directory, we should delete
	# it first to make sure the new link doesn't get created INSIDE of it.
	if [ -L "${link_target}" -a -d "${link_target}" ] ; then
		rm "${link_target}"
	fi

	[ -e "${file_name}" ] || \
		die "ERROR! The file we are trying to link from doesn't exist: ${file_name}."

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

###################
## Files under ~ ##
###################

# bashrc files
super_link $SCRIPT_DIR/dot_bashrc $HOME/.bashrc

# screen files
super_link $SCRIPT_DIR/dot_screenrc $HOME/.screenrc

# emacs files
super_link $SCRIPT_DIR/dot_emacs $HOME/.emacs
make_directory $HOME/.emacs.d/
make_directory $HOME/.emacs.d/backups/
# super_link $SCRIPT_DIR/dot_emacs.d/local.el $HOME/.emacs.d/local.el
# super_link $SCRIPT_DIR/dot_spacemacs $HOME/.spacemacs


# xmonad files
make_directory $HOME/.xmonad
super_link $SCRIPT_DIR/dot_xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs

# xmobar files
super_link $SCRIPT_DIR/dot_xmobarrc $HOME/.xmobarrc

# stalonetray files
super_link $SCRIPT_DIR/dot_stalonetrayrc $HOME/.stalonetrayrc

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
make_directory $HOME/.vim/spell
super_link $SCRIPT_DIR/dot_vim/spell/en.utf-8.add $HOME/.vim/spell/en.utf-8.add
super_link $SCRIPT_DIR/dot_vim/after $HOME/.vim/after
super_link $SCRIPT_DIR/dot_vim/syntax $HOME/.vim/syntax
super_link $SCRIPT_DIR/dot_vim/UltiSnips $HOME/.vim/UltiSnips

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

# gpg
make_directory $HOME/.gnupg
chmod 0700 $HOME/.gnupg
super_link $SCRIPT_DIR/dot_gnupg/gpg.conf $HOME/.gnupg/gpg.conf

# ghci.conf
make_directory $HOME/.ghc
super_link $SCRIPT_DIR/dot_ghc/ghci.conf $HOME/.ghc/ghci.conf

# .ocamlinit and .lambda-term-inputrc (for utop)
super_link $SCRIPT_DIR/dot_ocamlinit $HOME/.ocamlinit
super_link $SCRIPT_DIR/dot_lambda-term-inputrc $HOME/.lambda-term-inputrc

# local .nix environment files
make_directory $HOME/.nix-local
super_link $SCRIPT_DIR/dot_nix-local/ocaml.nix $HOME/.nix-local/ocaml.nix

# postgres rc files
super_link $SCRIPT_DIR/dot_psqlrc $HOME/.psqlrc

#########################
## files under .config ##
#########################

make_directory $HOME/.config

# neovim (nvim)
make_directory $HOME/.config/nvim
super_link $SCRIPT_DIR/dot_config/nvim/init.vim $HOME/.config/nvim/init.vim
super_link $SCRIPT_DIR/dot_config/nvim/coc-settings.json $HOME/.config/nvim/coc-settings.json

# taffybar
make_directory $HOME/.config/taffybar
super_link $SCRIPT_DIR/dot_config/taffybar/taffybar.hs $HOME/.config/taffybar/taffybar.hs

# termonad
make_directory $HOME/.config/termonad
super_link $SCRIPT_DIR/dot_config/termonad/termonad.hs $HOME/.config/termonad/termonad.hs

# xdg user-dirs file
super_link $SCRIPT_DIR/dot_config/user-dirs.dirs $HOME/.config/user-dirs.dirs

###########################################
## Additional install and setup commands ##
###########################################

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
	fi
else
	echo "ERROR! \`git\` or \`vim\` does not exist. Not installing vim bundles."
fi
