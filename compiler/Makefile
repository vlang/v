
ifeq ($(OS),Windows_NT)
    CCFLAGS += -Dwindows
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
        CCFLAGS += -Dlinux
    endif
    ifeq ($(UNAME_S),Darwin)
        CCFLAGS += -Dmac
    endif
endif


all: v vc

v: vc
	./vc -o v .

vc: v.c
	cc $(CCFLAGS) -std=c11 -w -o vc v.c

v.c:
	wget https://vlang.io/v.c

clean:
	-rm v vc v.c
