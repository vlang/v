CC ?= cc
CFLAGS ?= -fPIC -O2

all: clean v
	$(info V has been successfully built)

v: v.c
	./v -o v compiler

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c
	${CC} -std=gnu11 -w -o v v.c -lm 

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -not -path "examples/hot_code_reloading/*" -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c .v.c v vprod thirdparty/**/*.o

SOURCES = $(wildcard thirdparty/**/*.c)
OBJECTS := ${SOURCES:.c=.o} 

thirdparty: ${OBJECTS}
	strip ${OBJECTS}
