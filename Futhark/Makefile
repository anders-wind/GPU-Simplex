SRC = $(wildcard *.fut)

default: $(SRC:.fut=-c)

opencl: $(SRC:.fut=-opencl)

all: default opencl

%-c: %.fut
	futhark-c $< -o $@

%-opencl: %.fut
	futhark-opencl $< -o $@

test: $(SRC)
	./runtests.py

clean:
	rm -f *-c *-opencl *.c *expected* *actual* *.bin

.PHONY=all default opencl test clean
