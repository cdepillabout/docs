#!/bin/bash
#
# Diff the changed files between two commits.
# git diff-changed-files <commit> <commit>

if [ $# -ne 2 ] ; then
	echo "Usage: $0 <commit> <commit>"
	exit 2
fi

commit1="$1"
commit2="$2"

# get changed files in both commits
changed_files_commit1="$(git show --pretty="format:" --name-only "$commit1" | grep -v "^$")"
changed_files_commit2="$(git show --pretty="format:" --name-only "$commit2" | grep -v "^$")"

# get differences between the two file lists
# (this just could have been a bash compare, I didn't need to actually use diff)
changed_files_diff="$(diff -u <(echo "$changed_files_commit1") <(echo "$changed_files_commit2"))"
if [ -n "$changed_files_diff" ] ; then
	echo "Commits affect different files, so they are not the same."
	exit 1
fi

# the git root directory
git_root="$(git rev-parse --show-toplevel)"

output="$(echo "$changed_files_commit1" | while read file ; do
	full_file="${git_root}/${file}"
	diff_commit1="$(git diff "${commit1}^" "${commit1}" -- "$full_file")"
	diff_commit2="$(git diff "${commit2}^" "${commit2}" -- "$full_file")"

	# now diff my two diffs, and check if they are different
	changes_diff="$(diff -u <(echo "$diff_commit1") <(echo "$diff_commit2"))"

	# if they are different, print out the differences
	if [ -n "$changes_diff" ] ; then
		echo "There are differences in ${file}."
		echo "${commit1}:"
		git diff --color "${commit1}^" "${commit1}" -- "$full_file" | sed -e "s/^/\t/"
		echo "${commit2}:"
		git diff --color "${commit2}^" "${commit2}" -- "$full_file" | sed -e "s/^/\t/"

		is_different=true
		export is_different
	fi

done)"


if [ -z "$output" ] ; then
	# there was no output, so the files are not different
	exit 0
else
	# there was output, so that means files are different
	echo "$output"
	exit 1
fi
