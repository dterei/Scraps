#!/bin/sh
gcc sections.c -O0 -S -o sections.s; perl -pe 's/^\s*\.section\s+\.text\s+(\d+)\s*,\s*"?[bnwdrxsay]*"?\s*$/.text $1\n/g' sections.s > sections-2.s; as sections-2.s; nm -n a.out
