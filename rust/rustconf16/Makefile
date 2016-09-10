TARGETS= \
	beg1 beg2 beg3 beg4 beg5 \
	int1 int2 int3 int4 \
	adv1 adv2 adv3 adv4 \
	test1

.PHONY: all
all: $(TARGETS)

beg%: beg%.rs
	rustc $^

int%: int%.rs
	rustc --test $^

int3: int3.rs
	rustc $^

int4: int4.rs
	rustc $^

beg5: beg5.rs
	rustc --test $^

adv%: adv%.rs
	rustc --test $^

test%: test%.rs
	rustc $^

.PHONY: clean
clean:
	rm -f $(TARGETS)
