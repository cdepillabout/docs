
# no formatting is done.  useful for trying to paste in comments.
# they auto format the next line (as a comment), so it is really annoying. 
# both of these commands are needed (maybe its just the first one...?)
:set paste
:set fo+="paste"

# match an arbitrary pattern
:match Search /pattern/

# turn off the highlighting
:match none


# CSCOPE
# I have a file that makes cscope easy to use in ~/.vim/plugin/
# Put the cursor over something I want to search for and hit CTRL-\ s.
# Then, to go back, hit CTRL-t.
#
# To make the cscope.out database file (which I need to be able to use cscope
# in vim), run the command 
# `cscope -b -q -R -I/usr/include/glib-2.0 -I/usr/include/gtk-2.0`
# The -b and -q just build the database,
# -R makes it go recursive
# and -I adds more directories to search for header files (in case 
# the files I am using don't directly include those headers)

# create file database to access java files
find . -name "*.java" > cscope.files
# then just run `cscope -b -q`
#
#
# make a cscope database of the linux kernel
find . -path "./arch/*" ! -path "./arch/i386*" -prune -o -path "./include/asm-*" ! -path "./include/asm-i386*" -prune -o -path "./tmp*" -prune -o -path "./Documentation*" -prune -o -path "./scripts*" -prune -o -name "*.[chxsS]" -print | tee cscope.files



# SPELLING

# from within vim, turn on spell check
:set spell

# when over a mispelled word, get corrections
z=


# format documents so that it word wraps, but it doesn't
# insert newlines unless you hit the enter button
:set formatoptions=l
:set lbr 



# CTAGS
# from http://articles.techrepublic.com.com/5100-22-5054618.html
# add `set tags=~/.tags` to your .vimrc and take out that cscope 
# plugin that I'm using or ctags won't work.
# also, there are more things on that website.
# When browsing source, use Ctrl-] to jump to the tag under the
# cursor.  Or, if it is the wrong tag, use g] to get a list of
# tags with the same name.  Look at:
# Or, use g Ctrl-] to jump if there is just one or bring up a list
# if there is multiple
# Use gf to go to the filename under the cursor
# http://vimdoc.sourceforge.net/htmldoc/usr_29.html
# http://vimdoc.sourceforge.net/htmldoc/tagsrch.html
ctags -f ~/.tags -R ~/myprojects/src $JAVA_HOME/src


# open file in remote vim session
# (or open it in current terminal it there
# is no remote session)
# [i made rvim an alias]
rvim file.txt

# switch to next tab
gt
gT

# move a tab
:tabmove <number>

# to switch back to your last location after a movement command
ctrl-o

# to then go back to your original location
ctrl-i

# pgup
ctrl-b

# pgdown
ctrl-f

# jump to first line being shown in the window ("highest")
H

# jump to the middle line being shown in the window
M

# jump to the last line being shown in the window
L

# when in visual mode, you can use these commands to select
# 'a' 'p'aragraph, 'a' '['block...
# this also works with the 'y'ank commands...
ap
a(
a[
as

# add abbreviation for "hello"
:ab hh hello

# if you want to type hh without it being expanded to hello
hh<CTRL-v> 

# remove abbreviation for "hello"
:una hh 

# when in :help, to just to a new topic (enclosed in pipes: |new-topic|)
ctrl-]
# to jump back
ctrl-o

# when you are programming, the % key jumps to the next ( [ or {. Then,
# when you are on that character, it jumps the corresponding closing ) ] or }
%

# to jump to the definition of a local variable
gd
# definition of global variable
gD

# display all instances of keyword under the cursor
[I

# to list all buffers
:ls

#to open buffer 1
:b 1
# to open buffer 2
:b 2

# to split the window and create a new buffer
:new
# just split the window
:sp
# vertically
:vsp

# to move to the new window 
Ctrl-w <motion key>
# as a short cut, to cycle through all open windows
ctrl-w ctrl-w

# reverse windows (make the bottom one go to the top and the top one go to the bottom)
ctrl-w r

# make current window as big as possible (think of other windows as being made flat '_')
ctrl-w _

# make all window equal size
ctrl-w =


# delete to end of page (everything visible on the screen)
dL

# chop off end of line and go into insert mode
C



# view the registers
:reg

# put the 6th register item
"6p


# line complete 
ctrl-x ctrl-l


# just like 'p', but it automatically adjusts the indent level of the 
# pasted code to match that of the code you paste into.
]p

# EDIT A MACRO
# I typically issue a :new to open a new window, move my cursor to that 
# window and then "ap. The contents of the macro are printed to the window. 
# From there, you can edit the macro as needed and then yank the macro back 
# in to the desired register by executing something like 0"ay$. Once yanked, 
# the new macro will execute as you would expect it would. 


# open the file under the cursor in a new tab
ctrl-w gf
# or you can do a tab split and then gf
:tab split
gf


# open all folds in a file
zR

# close all folds in a file
zM

# There are a couple different kinds of vim files.
# The major ones are global plugins, filetype plugins, syntax files, and indentation files.
# filetype plugins are loaded when files of a specific filetype are loaded.
# This is an example of a line that could go into a filetype plugin file:
#autocmd FileType sas set tabstop=3			" 3-space tab indent width
# A filetype plugin is like a global plugin, except that it sets options defines mappings
# for the current buffer only.


# this sets up format options that may help you when writing txt files or emails
:set formatoptions+=aw

# use this command to format a paragraph
gqip


# get a list of files to edit from the command line
vim $(ls some_dir/*)
# note that this doesn't work:
ls some_dir/* | vim   # THIS DOESN'T WORK!!

# start eclimd
/usr/share/eclipse/eclimd


# make vim write out a file using a different encoding
# (in this case it is utf8)
:set fileencoding=utf8
