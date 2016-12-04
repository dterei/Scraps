EXECS:=ApproachIO ApproachFree ApproachMTL Scraps

all: $(EXECS)

.PHONY: clean
clean:
	rm *.o
	rm *.hi
	rm -f $(EXECS)

%: %.hs
	ghc --make $^
