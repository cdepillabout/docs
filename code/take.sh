#!/usr/bin/env bash

# This moves a file to a directory and then subsequently cd's to that directory.
# The `cd` must be done from a shell function.

command_basename=$(basename "$0")
_DEBUG=0

function usage () {
cat <<END
Usage: ${command_basename} [-d] <FILE>... <DIR>

${command_basename} will move all FILEs to DIR, and then cd to DIR.

Arguments:

  -d, --debug      Run in debug mode.

Example:

- Run the command \`foo bar baz\`
\$ ${command_basename} file1 file2 file3 dir
END
}

function debug () {
	[ "${_DEBUG}" -eq "1" ] && echo "DEBUG: $@" >&2
}

function die () {
	echo "ERROR: $@" >&2
	exit 1
}

# Parse out the flags given to this flip command
for arg in "$@" ; do
	case "$arg" in
		-d|--debug)
			shift
			_DEBUG=1
			;;
		-h|--help)
			usage
			exit 100
			;;
		*)
			break
			;;
	esac
done

debug "args to ${command_basename}: $@"

# Make sure we are given at least two arguments.
[ "$#" -lt "2" ] && die "Must be given at least two arguments. FILE and DIR."

last_arg="${@:$#:1}"
debug "last arugment to this command: ${last_arg}"

# make sure the last argument is a directory
[ ! -d "$last_arg" ] && die "The last argument to this comamnd (\"${last_arg}\") must be a directory."

# mv files
mv "$@"
ret="$?"
if [ "${ret}" != "0" ] ; then
	debug "mv failed with return code: ${ret}"
	exit $ret
fi

# If this suceeds, then a shell function must subsequently do the `cd` to $last_dir.
