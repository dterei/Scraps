#!/bin/sh

cat $1 | sed '1d' | tr 'yeqdkroawjtsulgcvhbmpnfxiz' 'aozsitkyfuwnjgvepxhlrbcmdq' | awk -F, '{print "Case #" NR ": " $1}'

