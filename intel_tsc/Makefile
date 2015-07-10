ifneq ($(KERNELRELEASE),)
# kbuild part of makefile
obj-m := tsc_kmod.o

else
# normal makefile
KDIR ?= /lib/modules/`uname -r`/build
CFLAGS = -O3

all: using_tsc eval_clocks benchmarking

kmod:
	$(MAKE) -C $(KDIR) M=$$PWD

.PHONY: clean
clean:
	@rm -f using_tsc eval_clocks benchmarking
	@rm -f *.o

endif
