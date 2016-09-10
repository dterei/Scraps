TARGETS= \
	beg1 beg2 beg3 beg4 beg5 \
	int1 int2 int3 int4 \
	adv1 adv2 adv3

.PHONY: all
all: $(TARGETS)

beg%: beg%.rs
	rustc $^

int%: int%.rs
	rustc --test $^

beg5: beg5.rs
	rustc --test $^

adv%: adv%.rs
	rustc --test $^

.PHONY: clean
clean:
	rm -f $(TARGETS)
