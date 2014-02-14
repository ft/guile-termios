#!/bin/sh

GUILE_AUTO_COMPILE=0
export GUILE_AUTO_COMPILE

exec guile ./gen-gps.scm
