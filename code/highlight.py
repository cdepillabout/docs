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
    -i, --ignore-case       ignore case when searching for matches 
    -h, --help              print this usage
    -k, --regular-filename  don't colorize filenames
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
    "Try to shorten the original_filename by replacing /home/user with just ~"
    if not os.environ.has_key("HOME"):
        return original_filename
    
    home_dir = os.environ.get("HOME")
    if original_filename.startswith(home_dir):
        return original_filename.replace(home_dir, "~", 1)

    return original_filename


def create_highlight_replace_func(color):
    "This method gets passed into re.sub to replace part of a matching line."
    return lambda matchobj: color + matchobj.group() + RESET_TEXT

def highlight_lines(regex, highlight_replace_func, file=sys.stdin, line_prefix=''):
    """Highlights all lines in file matching regex.
    line_prefix is used to put something on the beginning of a line,
    so for example, you can put the file name on the beginning of the line."""
    for line in file:
        try:
            print line_prefix + re.sub(regex, highlight_replace_func, line),
        except IOError:
            pass

def main():


    regex_flags = 0 # flags to pass to our regular expression compiler
    color = BOLD_RED_TEXT # default color
    highlight_filenames = True # highlight_filenames by default

    # deal with flags
    try:
        opts, args = getopt.getopt(sys.argv[1:], "c:ghik", 
                ["color=", "help", "ignore-case", "regular-filename"])
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
            color = get_color(arg)
        if opt in ("-h", "--help"):
            print usage()
            sys.exit(0)
        if opt in ("-i", "--ignore-case"):
            regex_flags |= re.IGNORECASE
        if opt in ("-k", "--regular-filename"):
            highlight_filenames = False

    # get the highlight_replace function
    highlight_replace_func = create_highlight_replace_func(color)

    # make sure that the regex is specified on the command line
    if not args:
        print "ERROR! Must supply PATTERN.\n"
        print usage()
        sys.exit(1)

    # our regular expression to match is the first argument after the flags
    print "args = " + str(args)
    regex_string = args[0]
    regex = re.compile(regex_string, regex_flags)

    # The rest of the arguments are files to match.
    # If there are no files passed in, we default to searching sys.stdin
    
    files = args[1:]
    print "files = " + str(files)


    if not files:
        highlight_lines(regex, highlight_replace_func)
    else:
        # this allows for stripped, alternating color
        def rotate_current_color():
            color = MAGENTA_TEXT
            while True:
                if color == MAGENTA_TEXT:
                    color = GREEN_TEXT
                    yield GREEN_TEXT
                if color == GREEN_TEXT:
                    color = YELLOW_TEXT
                    yield YELLOW_TEXT
                if color == YELLOW_TEXT:
                    color = CYAN_TEXT
                    yield CYAN_TEXT
                if color == CYAN_TEXT:
                    color = MAGENTA_TEXT
                    yield MAGENTA_TEXT

        current_color = rotate_current_color().next

        for f in files:
            file = open(f, 'r')
            file_name = try_to_shorten_filename(f)

            if highlight_filenames:
                file_name = current_color() + file_name + RESET_TEXT + BOLD_TEXT + ": " + RESET_TEXT
            else:
                file_name = file_name + ": "

            highlight_lines(regex, highlight_replace_func, file, 
                "" if len(files) == 1 else file_name)
            

    return 0

if __name__ == '__main__':
    main()
