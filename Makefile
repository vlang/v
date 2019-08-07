CC ?= cc
CFLAGS ?= -O2 -fPIC
PREFIX ?= /usr/local
VC ?= 0.1.17

all: v
	$(info V has been successfully built)

v: v.c.out
	./v.c.out -o v compiler

v-release: v.c
	./v -cflags '${CFLAGS}' -o v compiler
	strip v

v.c.out: v.${VC}.c
	${CC} -std=gnu11 -w -o v.c.out v.${VC}.c -lm

v.${VC}.c:
	curl -o v.${VC}.c -Ls https://github.com/vlang/vc/raw/${VC}/v.c

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c v*.c v.c.out v vprod thirdparty/**/*.o
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
	mkdir -p ${PREFIX}/lib/vlang ${PREFIX}/bin
	cp -r {v,vlib,thirdparty} ${PREFIX}/lib/vlang
	ln -s ${PREFIX}/lib/vlang/v ${PREFIX}/bin/v

uninstall:
	rm -rf ${PREFIX}/{bin/v,lib/vlang}

symlink:
	ln -sf `pwd`/v ${PREFIX}/bin/v
