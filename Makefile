CC ?= cc
PREFIX ?= /usr/local

all: 
	git clone --depth 1 --quiet https://github.com/vlang/vc
	${CC} -std=gnu11 -w -o v vc/v.c -lm
	./v -o v compiler
	rm -rf vc
	@echo "V has been successfully built"

symlink: v
	ln -sf `pwd`/v ${PREFIX}/bin/v
