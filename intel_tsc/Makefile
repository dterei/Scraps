ifneq ($(KERNELRELEASE),)
# kbuild part of makefile
obj-m := tsc_kmod.o

else
# normal makefile
KDIR ?= /lib/modules/`uname -r`/build
CFLAGS = -O3

all: tsc_user clock_bench

kmod:
	$(MAKE) -C $(KDIR) M=$$PWD

.PHONY: clean
clean:
	@rm -f tsc_user clock_bench
	@rm -f *.o

endif
