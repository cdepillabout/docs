#!/usr/bin/env bash

# This flips the command line arguments around, much like the `flip` function
# in haskell.

function usage_flipf () {
cat <<END
Usage: $0 <COMMAND> [COMMAND_ARGS...] <ARGS_TO_FLIP>...

$0 will flip the frist argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the last argument.  COMMAND_ARGS are arguments beginning with '-'.  $0 will
ignore these and pass them to COMMAND unchanged. End of COMMAND_ARGS is
signaled by '--'.

This is most helpful when used with commands like \`mv\` and \`cp\`.

Example:

- Run the command \`foo bar baz\`
	\$ $0 foo baz bar

- Run the command \`foo bar1 bar2 bar3 baz\`
	\$ $0 foo baz bar1 bar2 bar3

- Run the command \`mv file1 file2 dir/\`
	\$ $0 mv dir/ file1 file2

- Run the command \`cp -i file1 file2 dir/\`
	\$ $0 cp -i dir/ file1 file2

- Run the command \`cp -- -file-with-leading-dash dir/\`
	\$ $0 cp -- dir/ -file-with-leading-dash
END
}

function usage_flipl () {
cat <<END
Usage: $0 <COMMAND> [COMMAND_ARGS...] <ARGS_TO_FLIP>...

$0 will flip the last argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the first argument.  COMMAND_ARGS are arguments beginning with '-'.  $0 will
ignore these and pass them to COMMAND unchanged. End of COMMAND_ARGS is
signaled by '--'.

This is most helpful when used with commands like \`grep\` and \`ack\`.

Example:

- Run the command \`foo bar baz\`
	\$ $0 foo baz bar

- Run the command \`foo bar baz1 baz2 baz3\`
	\$ $0 foo baz1 baz2 baz3 bar

- Run the command \`grep "string" file1 file2\`
	\$ $0 grep file1 file2 "string"

- Run the command \`grep "string" file1 file2 -i file1 file2 dir/\`
	\$ $0 cp -i dir/ file1 file2

- Run the command \`cp -- -file-with-leading-dash dir/\`
	\$ $0 cp -- dir/ -file-with-leading-dash
END
}

usage_flipl
