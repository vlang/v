CC ?= cc
CFLAGS ?= -O2 -fPIC
PREFIX ?= /usr/local

all: v
	$(info V has been successfully built)

v: v.c
	${CC} -std=gnu11 -w -o v v.c -lm
	./v -o v compiler

v-release: v.c
	./v -cflags '${CFLAGS}' -o v compiler
	strip v

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c .v.c v vprod thirdparty/**/*.o
	find . -name '.*.c' -print0 | xargs -0 -n1 rm -f

SOURCES = $(wildcard thirdparty/**/*.c)
OBJECTS := ${SOURCES:.c=.o} 

thirdparty: ${OBJECTS}

thirdparty-release: ${OBJECTS}
	strip ${OBJECTS}

debug: clean v thirdparty

release: CFLAGS += -pie
release: clean v-release thirdparty-release

install: uninstall
	mkdir -p ${PREFIX}/lib/vlang
	cp -r {v,vlib,thirdparty} ${PREFIX}/lib/vlang
	ln -s ${PREFIX}/lib/vlang/v ${PREFIX}/bin/v

uninstall:
	rm -rf ${PREFIX}/{bin/v,lib/vlang}

symlink:
	ln -sf `pwd`/v ${PREFIX}/bin/v
