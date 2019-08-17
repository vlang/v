CC ?= cc
CFLAGS ?= -O2 -fPIC
PREFIX ?= /usr/local

all: 
	curl -o v.c -LsSf https://raw.githubusercontent.com/vlang/vc/master/v.c
	${CC} -std=gnu11 -w -o v v.c -lm
	./v -o v compiler
	rm v.c
	@echo "V has been successfully built"

symlink: v
	ln -sf `pwd`/v ${PREFIX}/bin/v
