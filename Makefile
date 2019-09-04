CC ?= cc

_SYS:=$(shell uname -o)
ifeq ($(_SYS),Msys)
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
	./v v compiler
endif
	rm -rf vc/
	@echo "V has been successfully built"

