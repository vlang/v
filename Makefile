CC ?= cc

_SYS := $(shell uname 2>/dev/null || echo Unknown)
_SYS := $(patsubst MSYS%,MSYS,$(_SYS))
_SYS := $(patsubst MINGW%,MinGW,$(_SYS))

ifneq ($(filter $(_SYS),MSYS MinGW),)
WIN32:=1
endif

all:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc
ifdef WIN32
	$(CC) -std=gnu11 -w -o v0.exe vc/v_win.c
else
	$(CC) -std=gnu11 -w -o v0 vc/v.c -lm
endif
	./v0 -o v compiler
	rm -rf v0 vc/
	@echo "V has been successfully built"

