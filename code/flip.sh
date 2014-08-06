#!/usr/bin/env bash

# This flips the command line arguments around, much like the `flip` function
# in haskell.

command_basename=$(basename "$0")
_DEBUG=0

function usage_flipf2l () {
cat <<END
Usage: ${command_basename} [FLIP_FLAGS] <COMMAND> [COMMAND_FLAGS...] <ARGS_TO_FLIP>...

${command_basename} will flip the first argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the *LAST* argument.  COMMAND_FLAGS are arguments beginning with '-'.  ${command_basename} will
ignore these and pass them to COMMAND unchanged. End of COMMAND_FLAGS is
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
Usage: ${command_basename} [FLIP_FLAGS] <COMMAND> [COMMAND_FLAGS...] <ARGS_TO_FLIP>...

${command_basename} will flip the last argument in ARGUMENTS_TO_FLIP and pass it to COMMAND as
the *FIRST* argument.  COMMAND_FLAGS are arguments beginning with '-'.  ${command_basename} will
ignore these and pass them to COMMAND unchanged. End of COMMAND_FLAGS is
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

function usage_flip () {
cat <<END
Usage: ${command_basename} [FLIP_FLAGS] <COMMAND> [COMMAND_FLAGS...] <ARGS_TO_FLIP>...

flip will run either flipf2l or flipl2f based on the COMMAND it is passed.

flipf2l:
  - mv
  - cp

flipl2f:
  - grep
  - ack

Run \`flipf2l --help\` or \`flipl2f --help\` for more info.
END
}

function debug () {
	[ "${_DEBUG}" -eq "1" ] && echo "DEBUG: $@" >&2
}

function die () {
	echo "ERROR: $@" >&2
	exit 1
}

# look at the filename we are being called with to figure out whether to
# run flipf2l or flipl2f.
case "$command_basename" in
	flip)
		flip_type=flip
		;;
	flipf2l)
		flip_type=flipf2l
		;;
	flipl2f)
		flip_type=flipl2f
		;;
	*)
		echo "WARNING: no flip type know for \"$command_basename\", so running flipf2l." >&2
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
		-h|--help)
			case "$flip_type" in
				flip)
					usage_flip
					;;
				flipl2f)
					usage_flipl2f
					;;
				*)
					usage_flipf2l
					;;
			esac
			exit 0
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

# If the flip command we are running is just "flip", then we need to decide whether
# to run flipl2f or flipf2l based on the command name.
if [ "$flip_type" = "flip" ] ; then
	case "$command_to_run" in
		mv|cp)
			flip_type=flipf2l
			debug "\$flip_type was \"flip\", but the COMMAND is $command_to_run, so we are now running as $flip_type"
			;;
		grep|ack)
			flip_type=flipl2f
			debug "\$flip_type was \"flip\", but the COMMAND is $command_to_run, so we are now running as $flip_type"
			;;
	esac
fi

# The total number of arguments and flags to COMMAND.
command_total_args_and_flags=0
for command_arg in "$@" ; do
	((command_total_args_and_flags++))
done
debug "command total args: $command_total_args_and_flags"

# find the last of the COMMAND_FLAGS. We operate on the arguments after this.
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
if [ "$command_flags_end_index" -gt 0 ] ; then
	command_flags="${@:1:$command_flags_end_index}"
else
	command_flags=""
fi
debug "command flags: $command_flags"

# The non-flag arguments for the COMMAND.  For example, with `grep -i -r "something" file1`,
# command_args would be "something" and file1.
if [ "$command_total_args_and_flags" -gt "$command_flags_end_index" ] ; then
	((command_args_start_index=command_flags_end_index + 1))
	((command_args_total=command_total_args_and_flags - command_flags_end_index))
	command_args="${@:$command_args_start_index}"
else
	command_args_start_index=0
	command_args_total=0
	command_args=""
fi
debug "command args start index: $command_args_start_index"
debug "command args total: $command_args_total"
debug "command args: $command_args"

debug "\$@: $@"

# These evals are pretty awful.
case "$flip_type" in
	flipl2f)
		eval "\"\${command_to_run}\"" \
			`if [ "$command_flags_end_index" -gt 0 ] ; then echo " \"\\\${@:1:$command_flags_end_index}\"" ; fi ` \
			`if [ "$command_args_total" -gt 0 ] ; then echo " \"\\\${@:$command_total_args_and_flags}\"" ; fi ` \
			`if [ "$command_args_total" -gt 1 ] ; then echo " \"\\\${@:$command_args_start_index:$(expr $command_total_args_and_flags-$command_args_start_index)}\"" ; fi `
		;;
	flipf2l)
		eval "\"\${command_to_run}\"" \
			`if [ "$command_flags_end_index" -gt 0 ] ; then echo " \"\\\${@:1:$command_flags_end_index}\"" ; fi ` \
			`if [ "$command_args_total" -gt 1 ] ; then echo " \"\\\${@:$((command_args_start_index + 1))}\"" ; fi ` \
			`if [ "$command_args_total" -gt 0 ] ; then echo " \"\\\${@:$command_args_start_index:1}\"" ; fi `
		;;
	flip)
		case "$command_args_total" in
			0)
				"$command_to_run"
				;;
			1)
				eval "\"\${command_to_run}\"" \
					`if [ "$command_flags_end_index" -gt 0 ] ; then echo " \"\\\${@:1:$command_flags_end_index}\"" ; fi ` \
					"${@:$command_args_start_index}"
				;;
			2)
				eval "\"\${command_to_run}\"" \
					`if [ "$command_flags_end_index" -gt 0 ] ; then echo " \"\\\${@:1:$command_flags_end_index}\"" ; fi ` \
					"${@:$((command_args_start_index + 1))}" \
					"${@:$command_args_start_index:1}"
				;;
			*)
				die "Too many args to $command_basename with unknown COMMAND (i.e. it's not grep, mv, cp, ack, etc)."
				;;
		esac
		;;
	*)
		die "action for flip type \"$flip_type\" has not been defined."
		;;
esac

