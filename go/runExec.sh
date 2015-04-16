#!/bin/sh

X=$(./Exec)

P=$(echo $X | cut -d' ' -f3 | cut -d':' -f3)
C=$(echo $X | cut -d' ' -f8 | cut -d':' -f3)

DIFF=$(calc "($C - $P) * 1000000")

echo $P
echo $C
echo -n ${DIFF}
echo ns
