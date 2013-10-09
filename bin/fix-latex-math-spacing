#!/bin/bash

# Converts blank lines adjacent to display-style environments to comments.
# This preserves the visual space in the TeX source but removes excessive 
# space from the typeset output.

tr '\n' '\0' \
  | sed 's/\x0\x0\(\\\(begin{\(displaymath\|align\|description\)\|\[\)\)/\x0%\x0\1/g' \
  | sed 's/\(\\\(end{\(displaymath\|align\|description\).*\|\]\)\)\x0\x0/\1\x0%\x0/g' \
  | tr '\0' '\n'
