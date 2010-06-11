#!/usr/bin/env python

import re
import sys
import subprocess

def usage(): 
	return """Usage: %(command_name)s [OPTION]... PATTERN [FILE]...
Search for PATTERN in FILE and print colorful output.
PATTERN is a regular expression.

    -f, --from      permission we are copying from 
    -t, --to        permission we are copying to (overwriting)

Example:
%(command_name)s --from u --to g SOME_FILE\tSet group perms equal to the user perms""" \
        % {'command_name': sys.argv[0]}

def get_i_flag(args):
    for a in args:
        print a

def parse_grep_help_option_line(line):
    "Parse a line of grep --help output and return valid commands on that line"
    line = line.strip()
    result = re.match("--?\w+(-?\w*)*(\[=\w+\]|=\w+)?", line)
    if result:

        end_index = result.end()
        if line[result.end()] == ",":
            end_index += 1

        tmp = parse_grep_help_option_line(line[end_index:])
        return [result.group()] + tmp
    else:
        return list()


def get_grep_options():
    grep_help_output = \
            subprocess.Popen(["grep", "--help"], 
                    stdout=subprocess.PIPE).communicate()[0]

    grep_options = list()
    for line in grep_help_output.split('\n'):
        if line.startswith("  "):
            grep_options.extend(parse_grep_help_option_line(line))
    
    return grep_options


######
def interpret_grep_opts(grep_opts):
    if not grep_opts:
        return NULL
    
    
    if grep_opts[0].find('=') != -1:


def main():


    grep_opts = get_grep_options()
    print str(grep_opts)
    interpreted_grep_opts = interpret_grep_opts(grep_opts)

    # basically we just need to see if the -i flag is passed, and then find the pattern
    #get_i_flag(sys.argv)




if __name__ == '__main__':
    main()
