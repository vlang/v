CFLAGS := --std=c11

# Set std to gnu11 on Linux, fixes segfault.
ifneq ($(OS),Windows_NT)
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		CFLAGS := --std=gnu11
	endif
endif

all: v vc

v: vc
	./vc -o v .

vc: v.c
	cc $(CFLAGS) -w -o vc v.c

v.c:
	wget https://vlang.io/v.c

clean:
	-rm v vc v.c
