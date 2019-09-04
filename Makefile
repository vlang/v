CC ?= cc

_SYS := $(shell uname)
_SYS := $(patsubst MSYS%,MSYS,$(_SYS))
_SYS := $(patsubst MINGW%,MinGW,$(_SYS))

ifeq (,$(findstring _SYS,MSYS,MinGW))
WIN32:=1
endif

all:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc
ifdef WIN32
	$(CC) -std=gnu11 -DUNICODE -D_UNICODE -w -o v0.exe vc/v_win.c
	./v0.exe -o v.exe compiler
else
	$(CC) -std=gnu11 -w -o v vc/v.c -lm
	./v -o v compiler
endif
	rm -rf vc/
	@echo "V has been successfully built"

