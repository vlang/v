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
	$(CC) -std=c99 -w -o v0.exe vc/v_win.c
	./v0.exe -o v.exe compiler
	rm -f v0.exe
else
	$(CC) -std=gnu11 -w -o v vc/v.c -lm && \
	./v -o v2 compiler && \
	mv -f v2 v
endif
	rm -rf vc/
	@echo "V has been successfully built"

