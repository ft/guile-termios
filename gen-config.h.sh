#!/bin/sh

# Copyright (c) 2014 Frank Terbeck <ft@bewatermyfriend.org>
# All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

available=' '
members='c_iflag c_oflag c_cflag c_lflag c_line c_cc c_ispeed c_ospeed'

C_COMPILER=${C_COMPILER:-cc}
export C_COMPILER

for member in $members; do
    printf 'Checking whether struct termios has %s member... ' "$member"
    if sh ./termios-struct-has.sh "$member"; then
        printf 'yes.\n'
        name=$(printf '%s' "$member" | tr a-z A-Z)
        available="$available$name "
    else
        printf 'no.\n'
    fi
done

print2config () {
    fmt=$1
    shift
    printf "$fmt" "$@" >> config.h || exit 1
}

printf 'Generating "config.h" from "config.h.in"... '
rm -f config.h
while IFS='' read -r line; do
    case "$line" in
    '#define-maybe '*)
        token=${line##* }
        case "$available" in
        *\ "$token"\ *)
            print2config '#ifndef GUILE_TERMIOS_HAS_%s\n' "$token"
            print2config '#define GUILE_TERMIOS_HAS_%s\n' "$token"
            print2config '#endif /* GUILE_TERMIOS_HAS_%s */\n' "$token"
            ;;
        *)
            print2config '#ifdef GUILE_TERMIOS_HAS_%s\n' "$token"
            print2config '#undef GUILE_TERMIOS_HAS_%s\n' "$token"
            print2config '#endif /* GUILE_TERMIOS_HAS_%s */\n' "$token"
            ;;
        esac
        ;;
    *)
        print2config '%s\n' "$line"
    esac
done < config.h.in

[ "$?" != 0 ] && exit 1

printf 'done.\n'
exit 0
