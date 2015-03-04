#!/usr/bin/env bash
# Add transactions to hledger, and then commit the new ledger file
# using `git commit`.

if [ ! -f "$HLEDGER_DEFAULT_LEDGER" ] ; then
	echo "ERROR! Define \$HLEDGER_DEFAULT_LEDGER to point to your default ledger."
	exit 1
fi

ledger_directory="$(dirname "$HLEDGER_DEFAULT_LEDGER")"

cd "$ledger_directory"

echo "Running \`hledger add -f \"${HLEDGER_DEFAULT_LEDGER}\"\`."
echo "Press Ctrl-C to cancel transaction."
echo "Press Ctrl-D to accept transaction and commit into git."
echo

# ignore ctrl-c (but send it to the child processes, i.e. the hledger process below)
trap "" SIGINT

hledger add -f "${HLEDGER_DEFAULT_LEDGER}"
ret=$?

# reset the ctrl-c signal back to it's default value
trap SIGINT

if [ "$ret" -eq "0" ]; then
	echo
	echo "Commiting and pushing..."
	echo
	git add "${HLEDGER_DEFAULT_LEDGER}"
	git commit -m "Ledger update for $(date '+%Y-%m-%d')."
	git push
else
	echo
	echo "hledger ended with return code \"$ret\", so not commiting or pushing."
fi


