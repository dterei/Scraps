# C Memory Alignment

This is a test of memory alignment on X86. Only tested on 64bit for
now.

## Outcome

 * X64 deals fine with unaligned memory. The only exception is SSE
   instructions, they require 16 byte alignment.

 * On Nehalem and more recent procesors there is no penalty to
   unligned memory accesses. Even cross cache-line boundaries don't
   have a penalty anymore (however still want to watch for cache-line
   contention between cores when writing parallel code).

 * ARM & Itanium don't like unaligned memory and will through an
   exception when it occurs. Compilers can break the access up into
   multiple 1 byte accesses to avoid this when told.

## Resources

 * http://lemire.me/blog/archives/2012/05/31/data-alignment-for-speed-myth-or-reality/

## Assembly

Generally looks something like (AT&T syntax):

~~~~ {.asm}
	# load a byte from address 3 and store into %eax register,
        # sign extending to a word (32bit).
	movsbl	3, %eax

	# store a byte to the pointer (%rax + 1) from the %dil
        # register.
	movb	%dil, 1(%rax)
~~~~

