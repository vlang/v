CC ?= cc
CFLAGS ?= -O2 -fPIC
PREFIX ?= /usr/local

all: 
	curl -o v.c -LsSf https://raw.githubusercontent.com/vlang/vc/master/v.c
	${CC} -std=gnu11 -w -o v v.c -lm
	v -o v compiler
	rm v.c
	@echo "V has been successfully built"

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -print0 | xargs -0 -n1 ./v
	bash ./compiler/tests/repl/repl.sh

symlink: v
	ln -sf `pwd`/v ${PREFIX}/bin/v
