#!/usr/bin/env bash
#
# Output everything on stdin, but indented by the given number of characters.

set -eEuo pipefail

# character to use for indenting by default
char=' '

# amount to indent by default
amount=4

usage() {
    cat <<EOF
Usage: $0 [-c CHARACTER] [-a AMOUNT]

Options:
  -c, --char   CHARACTER  Character to indent with (default: '${char}')
  -a, --amount AMOUNT     Number of characters to indent (default: ${amount})
  -h, --help              Show this help message and exit
EOF
}


# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -c|--char)
            char="$2"
            shift 2
            ;;
        -a|--amount)
            amount="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Get the indent characters
indent="$(printf "%${amount}s" ' ' | tr ' ' "${char}")"

# Read from stdin and pass to sed to prepend indentation
#
# TODO: Should escape indent characters
sed -e "s/^/${indent}/"

