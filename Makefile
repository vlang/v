CC ?= cc

_SYS := $(shell uname 2>/dev/null || echo Unknown)
_SYS := $(patsubst MSYS%,MSYS,$(_SYS))
_SYS := $(patsubst MINGW%,MinGW,$(_SYS))

ifneq ($(filter $(_SYS),MSYS MinGW),)
WIN32 := 1
endif

ifeq ($(_SYS),Linux)
LINUX := 1
endif

ifeq ($(_SYS),Darwin)
MAC := 1
endif

ifdef ANDROID_ROOT
ANDROID := 1
undefine LINUX
endif

all: fresh_vc fresh_tcc
ifdef WIN32
	$(CC) -std=c99 -w -o v0.exe vc/v_win.c $(LDFLAGS)
	./v0.exe -o v.exe v.v
	rm -f v0.exe
else
	$(CC) -std=gnu11 -w -o v vc/v.c $(LDFLAGS) -lm
ifdef ANDROID
	chmod 755 v
endif  
	@(VC_V=`./v version | cut -f 3 -d " "`; \
	V_V=`git rev-parse --short HEAD`; \
	if [ $$VC_V != $$V_V ]; then \
		echo "Self rebuild ($$VC_V => $$V_V)"; \
		./v -o v v.v; \
	fi)
ifndef ANDROID
	./v build module vlib/builtin > /dev/null
	./v build module vlib/strings > /dev/null
	./v build module vlib/strconv > /dev/null
endif  
endif
	rm -rf vc/
	@echo "V has been successfully built"


fresh_vc:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc
	#cp fns.h vc/fns.h

fresh_tcc:
ifdef WIN32
	rm -rf /var/tmp/tcc/
	git clone --depth 1 --quiet https://github.com/vlang/tccbin_win /var/tmp/tcc
endif
ifdef LINUX
	rm -rf /var/tmp/tcc/
	git clone --depth 1 --quiet https://github.com/vlang/tccbin /var/tmp/tcc
endif

selfcompile:
	./v -o v v.v
