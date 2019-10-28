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

all: fresh_vc fresh_tcc
ifdef WIN32
	$(CC) -std=c99 -w -o v0.exe vc/v_win.c $(LDFLAGS)
	./v0.exe -o v.exe v.v
	rm -f v0.exe
else
	$(CC) -std=gnu11 -w -o v vc/v.c $(LDFLAGS) -lm
	@(VC_V=`./v version | cut -f 3 -d " "`; \
	V_V=`git rev-parse --short HEAD`; \
	if [ $$VC_V != $$V_V ]; then \
		echo "Self rebuild ($$VC_V => $$V_V)"; \
		./v -o v v.v; \
	fi)
endif
	rm -rf vc/
	@echo "V has been successfully built"


fresh_vc:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc

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
