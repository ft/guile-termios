#!/bin/sh

# Copyright (c) 2014-2021 Frank Terbeck <ft@bewatermyfriend.org>
# All rights reserved.
#
# Terms for redistribution and use can be found in LICENCE.

${C_COMPILER:-cc} -o /dev/null -x c -c - 2> /dev/null <<EOF
#include <stddef.h>
#include <stdlib.h>

#include <termios.h>

int
main(void)
{
    long int offset;

    offset = (long int) offsetof(struct termios, $1);
    return EXIT_SUCCESS;
}
EOF
