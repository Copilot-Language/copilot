OUTS=copilot_fib.out fib.out copilot_maj.out fastmaj.out 
RM=rm -f 
CC=gcc

.PHONY: all, clean

%.out: %.c
	$(CC) -O3 -o $@ $<

all: $(OUTS)

clean: 
	$(RM) $(OUTS)


