#!/usr/bin/env python

import sys, re, os, getopt

ESCAPE="\033"

# colored text
BLACK_TEXT= ESCAPE + '[30m'
RED_TEXT= ESCAPE + '[31m'
GREEN_TEXT= ESCAPE + '[32m'
YELLOW_TEXT= ESCAPE + '[33m'
BLUE_TEXT= ESCAPE + '[34m'
MAGENTA_TEXT= ESCAPE + '[35m'
CYAN_TEXT= ESCAPE + '[36m'
WHITE_TEXT= ESCAPE + '[37m'

# bold color output 
BOLD_TEXT= ESCAPE + '[01m'

BOLD_BLACK_TEXT= BOLD_TEXT + BLACK_TEXT
BOLD_RED_TEXT= BOLD_TEXT + RED_TEXT
BOLD_GREEN_TEXT= BOLD_TEXT + GREEN_TEXT
BOLD_YELLOW_TEXT= BOLD_TEXT + YELLOW_TEXT
BOLD_BLUE_TEXT= BOLD_TEXT + BLUE_TEXT
BOLD_MAGENTA_TEXT= BOLD_TEXT + MAGENTA_TEXT
BOLD_CYAN_TEXT= BOLD_TEXT + CYAN_TEXT
BOLD_WHITE_TEXT= BOLD_TEXT + WHITE_TEXT

# normal text
RESET_TEXT= ESCAPE + '[00m'


# global variables
regex_flags = 0 # flags to pass to our regular expression compiler
highlight_color = BOLD_RED_TEXT # default highlight color
highlight_filenames = True # highlight_filenames by default
stripe_filenames = True # stripe filenames with different colors
from_grep = False # the input is coming from grep


def usage():
    "Print program usage."
    return """Usage: %(command_name)s [OPTION]... PATTERN [FILE]...
Highlight all occurances of PATTERN in FILEs.
If no FILE specified, read from stdin.
Example: %(command_name)s -i 'hello' helloworld.c helloworld.h

Options:
    -c, --color=COLOR       Print matches in color. Valid colors are
                            "black", "red", "green", "yellow", "blue",
                            "magenta", "cyan", "white".
    -g, --from-grep         input is coming from grep, so highlight filenames
    -h, --help              print this usage
    -i, --ignore-case       ignore case when searching for matches 
    -k, --regular-filename  don't colorize filenames
    -s, --no-stripe         don't stripe filenames with color
""" % {'command_name': sys.argv[0]}

def get_color(color_string):
    "Return a color escape sequence based on color_string."
    if color_string.lower() == "black":
        return BOLD_BLACK_TEXT
    elif color_string.lower() == "red":
        return BOLD_RED_TEXT
    elif color_string.lower() == "green":
        return BOLD_GREEN_TEXT
    elif color_string.lower() == "yellow":
        return BOLD_YELLOW_TEXT
    elif color_string.lower() == "blue":
        return BOLD_BLUE_TEXT
    elif color_string.lower() == "magenta":
        return BOLD_MAGENTA_TEXT
    elif color_string.lower() == "cyan":
        return BOLD_CYAN_TEXT
    elif color_string.lower() == "white":
        return BOLD_WHITE_TEXT
    else:
        print "ERROR! Incorrect color string.  Valid color strings are:"
        print "\"black\", \"red\", \"green\", \"yellow\", \"blue\", \"magenta\", ",
        print "\"cyan\", \"white\""
        print
        print usage()
        sys.exit(1)

def try_to_shorten_filename(original_filename):
    "Try to shorten the original_filename by replacing /home/username with just ~"
    if not os.environ.has_key("HOME"):
        return original_filename
    
    home_dir = os.environ.get("HOME")
    if original_filename.startswith(home_dir):
        return original_filename.replace(home_dir, "~", 1)

    return original_filename

def create_highlight_replace_func(color):
    "The returned method gets passed into re.sub to replace part of a matching line."
    return lambda matchobj: color + matchobj.group() + RESET_TEXT

def safety_print(string):
    "Print a string without throwing any IOErrors"
    try:
        print string,
    except IOError:
        pass

prev_file_name = ''
cur_color = ''
def get_grep_file_name(line):
    "Higlights filenames we get as input from grep."
    global prev_file_name
    global cur_color

    f, search_line = line.split(":", 1)
    file_name = try_to_shorten_filename(f)

    if prev_file_name != file_name:
        cur_color = grep_rotate_current_color()
        prev_file_name = file_name

    if highlight_filenames:
        file_name = cur_color + file_name + RESET_TEXT + \
            BOLD_TEXT + ": " + RESET_TEXT
    else:
        file_name = file_name + ": "

    return file_name, search_line

def highlight_lines(regex, highlight_replace_func, files):
    """Takes a compiled regex, function to highlight matches
    in files for the regex, and a list of files to search through.
    Just highlightes the matches on each line of files."""

    if not files:
        files.append(sys.stdin)

    for f in files:
        if hasattr(f, "readline"):
            file = f
        else:
            try:
                file = open(f, 'r')
            except IOError:
                print >>sys.stderr, sys.argv[0] + ": \"" + f + \
                        "\": no such file or directory"
                continue

        if len(files) <= 1:
            file_name = ""
        else:
            file_name = try_to_shorten_filename(f)
            if highlight_filenames:
                file_name = rotate_current_color() + file_name + RESET_TEXT + \
                    BOLD_TEXT + ": " + RESET_TEXT
            else:
                file_name = file_name + ": "

        for line in file:
            if from_grep:
                file_name, line = get_grep_file_name(line)
            safety_print(file_name + re.sub(regex, highlight_replace_func, line))

def rotate_current_color():
    "This is used for highlighting filenames in stripes of different color."
    global stripe_filenames
    if not stripe_filenames:
        while True:
            yield MAGENTA_TEXT

    possible_colors = [MAGENTA_TEXT, GREEN_TEXT, YELLOW_TEXT, CYAN_TEXT]
    color_index = -1
    while True:
        color_index += 1
        yield possible_colors[color_index % len(possible_colors)]
grep_rotate_current_color = rotate_current_color().next
rotate_current_color = rotate_current_color().next

def main():
    global regex_flags 
    global highlight_color 
    global highlight_filenames 
    global stripe_filenames 
    global from_grep 

    # deal with flags
    try:
        opts, args = getopt.getopt(sys.argv[1:], "c:ghiks", 
                ["color=", "from-grep", "help", "ignore-case", 
                    "regular-filename", "no-stripe"])
    except getopt.error, msg:
        print "ERROR! " + msg
        print "for help use --help"
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-c", "--color"):
            if not arg:
                print "ERROR! Must supply COLOR to --color.\n"
                print usage()
                sys.exit(1)
            highlight_color = get_color(arg)
        if opt in ("-g", "--from-grep"):
            from_grep = True   
        if opt in ("-h", "--help"):
            print usage()
            sys.exit(0)
        if opt in ("-i", "--ignore-case"):
            regex_flags |= re.IGNORECASE
        if opt in ("-k", "--regular-filename"):
            highlight_filenames = False
        if opt in ("-s", "--no-stripe"):
            stripe_filenames = False

    # get the highlight_replace function
    highlight_replace_func = create_highlight_replace_func(highlight_color)

    # make sure that the regex is specified on the command line
    if not args:
        print "ERROR! Must supply PATTERN.\n"
        print usage()
        sys.exit(1)

    # our regular expression to match is the first argument after the flags
    regex_string = args[0]
    regex = re.compile(regex_string, regex_flags)

    # The rest of the arguments are files to match.
    # If there are no files passed in, we default to searching sys.stdin
    files = args[1:]

    highlight_lines(regex, highlight_replace_func, files)

            

    return 0

if __name__ == '__main__':
    main()
