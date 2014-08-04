#!/usr/bin/env bash

# This flips the command line arguments around, much like the `flip` function
# in haskell.

command_basename=$(basename "$0")
debug=0

function usage_flipf2l () {
cat <<END
Usage: ${command_basename} [FLIP_ARGS] <COMMAND> [COMMAND_ARGS...] <ARGS_TO_FLIP>...

${command_basename} will flip the first argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the *LAST* argument.  COMMAND_ARGS are arguments beginning with '-'.  ${command_basename} will
ignore these and pass them to COMMAND unchanged. End of COMMAND_ARGS is
signaled by '--'.

${command_basename} is most helpful when used with commands like \`mv\` and \`cp\`.

Arguments:

  -d, --debug      Run in debug mode.

Example:

- Run the command \`foo bar baz\`
\$ ${command_basename} foo baz bar

- Run the command \`foo bar1 bar2 bar3 baz\`
\$ ${command_basename} foo baz bar1 bar2 bar3

- Run the command \`mv file1 file2 dir/\`
\$ ${command_basename} mv dir/ file1 file2

- Run the command \`cp -i file1 file2 dir/\`
\$ ${command_basename} cp -i dir/ file1 file2

- Run the command \`cp -- -file-with-leading-dash dir/\`
\$ ${command_basename} cp -- dir/ -file-with-leading-dash
END
}

function usage_flipl2f () {
cat <<END
Usage: ${command_basename} [FLIP_ARGS] <COMMAND> [COMMAND_ARGS...] <ARGS_TO_FLIP>...

${command_basename} will flip the last argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the *FIRST* argument.  COMMAND_ARGS are arguments beginning with '-'.  ${command_basename} will
ignore these and pass them to COMMAND unchanged. End of COMMAND_ARGS is
signaled by '--'.

${command_basename} is most helpful when used with commands like \`grep\` and \`ack\`.

Arguments:

  -d, --debug      Run in debug mode.

Example:

- Run the command \`foo bar baz\`
\$ ${command_basename} foo baz bar

- Run the command \`foo bar baz1 baz2 baz3\`
\$ ${command_basename} foo baz1 baz2 baz3 bar

- Run the command \`grep "string" file1 file2\`
\$ ${command_basename} grep file1 file2 "string"

- Run the command \`grep -i "string" file1 file2/\`
\$ ${command_basename} grep -i file1 file2 "string"

- Run the command \`grep -- "-search-string-with-leading-dash" file1\`
\$ ${command_basename} grep -- file1 -file-with-leading-dash
END
}

# look at the filename we are being called with to figure out whether to
# run flipf2l or flipl2f.
case "$command_basename" in
	flipf2l)
		command_to_run=flipf2l
		;;
	flipl2f)
		command_to_run=flipl2f
		;;
	*)
		command_to_run=flipf2l
		;;
esac

# Parse out the flags given to this flip command
for arg in "$@" ; do
	case "$arg" in
		-d|--debug)
			shift
			debug=1
			;;
		*)
			break
			;;
	esac
done

echo "$@"

# Make sure we are given at least one argument.


# find the last of the COMMAND_ARGS.  We operate on the arguments after this.


