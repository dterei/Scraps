ifneq ($(KERNELRELEASE),)
# kbuild part of makefile
obj-m := tsc_kmod.o

else
# normal makefile
KDIR ?= /lib/modules/`uname -r`/build

all: tsc1 tsc2 tsc3

tsc3: tsc3.c tsc.h clock.h
	$(CC)     tsc3.c	-o tsc3

kmod:
	$(MAKE) -C $(KDIR) M=$$PWD

.PHONY: clean
clean:
	rm -f tsc1 tsc2 tsc3

endif
