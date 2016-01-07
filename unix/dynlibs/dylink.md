# Dynamic Linking / Shared Libraries Notes

http://www.bottomupcs.com/chapter08.html

## printf orig

r1  = GOT         ; invariant
r14 = r1          ; setup GOT
r16 = [r1 + 0x50] ; 0x40000000000004f0 -- function -- printf@PLT (will fix)
r1  = [r1 + 0x58] ; 0x6000000000000ec0 -- new GOT / gp
jump r16          ; call PLT

## printf PLT setup

r15 = 0           ; relocation index -- what we will fixup

## PLT

r2  = r14         ; backup old GOT (r14 = r1)
r16 = [r14+0x00]  ; r14 = GOT -- module unique id
r17 = [r14+0x08]  ; dynlinker function
r1  = [r14+0x16]  ; new GOT / gp for dynlinker
jump r17          ; call dynlinker -- `_dl_runtime_resolve`

## Dynamic Linker

Fixes up [r1 + 0x50] and [r1 + 0x58] so that next time around, printf original
will shortcut straight to the actual printf function and bi-pass the printf@PLT
stub.

