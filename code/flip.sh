#!/usr/bin/env bash

# This flips the command line arguments around, much like the `flip` function
# in haskell.

command_basename=$(basename "$0")
_DEBUG=0

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

function debug () {
	[ "${_DEBUG}" -eq "1" ] && echo "$@"
}

function die () {
	echo "ERROR: $@"
	exit 1
}

# look at the filename we are being called with to figure out whether to
# run flipf2l or flipl2f.
case "$command_basename" in
	flipf2l)
		flip_type=flipf2l
		;;
	flipl2f)
		flip_type=flipl2f
		;;
	*)
		flip_type=flipf2l
		;;
esac

# Parse out the flags given to this flip command
for arg in "$@" ; do
	case "$arg" in
		-d|--debug)
			shift
			_DEBUG=1
			;;
		*)
			break
			;;
	esac
done

debug "args to ${flip_type}: $@"

# Make sure we are given at least one argument.
[ "$#" -lt "1" ] && die "Must be given at least one argument as the COMMAND."

# Use the first argument as the command to run.
command_to_run="$1" && shift
debug "command to run: $command_to_run"

# find the last of the COMMAND_ARGS.  We operate on the arguments after this.
command_flags_end_index=0
for command_arg in "$@" ; do
	case "$command_arg" in
		--)
			((command_flags_end_index++))
			break
			;;
		-*)
			((command_flags_end_index++))
			;;
		*)
			break
			;;
	esac
done
debug "command flags end index: $command_flags_end_index"

# The flags for the COMMAND.  For example, with `grep -i -r "something" file1`,
# command_flags would be -i and -r.
command_flags="${@:1:$command_flags_end_index}"
debug "command flags: $command_flags"

# The non-flag arguments for the COMMAND.  For example, with `grep -i -r "something" file1`,
# command_args would be "something" and file1.
((command_args_start_index=command_flags_end_index+1))
command_args="${@:$command_args_start_index}"
debug "command args start index: $command_args_start_index"
debug "command args: $command_args"

# The index of the last argument.
command_args_end_index=0
for i in "$@" ; do
	((command_args_end_index++))
done
debug "command args end index: $command_args_end_index"
debug "\$@: $@"

case "$flip_type" in
	flipl2f)
		"$command_to_run" \
			"${@:1:$command_flags_end_index}" \
			"${@:$command_args_end_index}" \
			"${@:$command_args_start_index:(($command_args_end_index-$command_args_start_index))}"
		;;
	flipf2l)
		"$command_to_run" \
			"${@:1:$command_flags_end_index}" \
			"${@:((command_args_start_index+1))}" \
			"${@:$command_args_start_index:1}"
		;;
	*)
		die "action for flip type \"$flip_type\" has not been defined."
		;;
esac

