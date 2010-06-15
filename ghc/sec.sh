#!/bin/sh -xe

CC=gcc
CC_OPTS="-O3 -Wall"

rm -f sec.x a.out

# Generate linker script.
# Assumes gnu ld. Use gcc's driver to find ld and select emulation (e.g.
# elf_i386) for given options (e.g. -m32).
$CC $CC_OPTS -nostdlib -Wl,--verbose 2>&1 | \
    sed -e '1,/======/d;/======/,$d;/^ *\*(\.text /a*(SORT(.ghc.*))' > sec.x

# Compile and run.
$CC $CC_OPTS -Wl,--script,sec.x sec.c
./a.out
