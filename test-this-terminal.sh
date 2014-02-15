#!/bin/sh

GUILE_LOAD_PATH=$PWD/scheme
export GUILE_LOAD_PATH

GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

exec ${GUILE_BINARY:-guile} -s ./test-this-terminal.scm
