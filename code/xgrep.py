#!/usr/bin/env python

"""Excludes some directories from grep searches."""

import sys
import os
import subprocess


dirs_to_exclude = [
                    ".git",
                    ".svn", 
                    "build", 
                  ]

def main():

    exclude_opts = ['--exclude-dir=' + x for x in dirs_to_exclude]

    #print "executing `grep " + str(exclude_opts + sys.argv[1:])

    os.execvp('grep', sys.argv[:1] + exclude_opts + sys.argv[1:])

if __name__ == '__main__':
    main()
