#!/bin/sh

GUILE_LOAD_PATH=$PWD/scheme
export GUILE_LOAD_PATH

GUILE_LOAD_COMPILED_PATH=$PWD/scheme
export GUILE_LOAD_COMPILED_PATH

GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

for i in test-this-terminal*.scm; do
    ${GUILE_BINARY:-guile} -s "$i"
done
