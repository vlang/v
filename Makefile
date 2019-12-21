CC ?= cc
TMPVC ?= /tmp/vc

VCFILE := v.c
TMPTCC := /var/tmp/tcc
VCREPO := https://github.com/vlang/vc
TCCREPO := https://github.com/vlang/tccbin
GITCLEANPULL := git clean -xf && git pull --quiet
GITFASTCLONE := git clone --depth 1 --quiet

#### Platform detections and overrides:
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
#####

ALL_TARGETS = latest_vc
ifndef ANDROID
ALL_TARGETS += latest_tcc
endif

ifdef WIN32
TCCREPO := https://github.com/vlang/tccbin_win
VCFILE := v_win.c
endif

all: $(ALL_TARGETS)
ifdef WIN32
	$(CC) -std=c99 -w -o v0.exe $(TMPVC)/$(VCFILE) $(LDFLAGS)
	./v0.exe -o v.exe v.v
	rm -f v0.exe
else
	$(CC) -std=gnu11 -w -o v $(TMPVC)/$(VCFILE) $(LDFLAGS) -lm
ifdef ANDROID
	chmod 755 v
endif  
	@(VC_V=`./v version | cut -f 3 -d " "`; \
	V_V=`git rev-parse --short HEAD`; \
	if [ $$VC_V != $$V_V ]; then \
		echo "Self rebuild ($$VC_V => $$V_V)"; \
		make selfcompile; \
	fi)
ifndef ANDROID
	make modules
endif  
endif
	@echo "V has been successfully built"

clean:
	git clean -xf

latest_tcc: $(TMPTCC)/.git/config
	cd $(TMPTCC) && $(GITCLEANPULL)

latest_vc: $(TMPVC)/.git/config
	cd $(TMPVC) && $(GITCLEANPULL)

fresh_vc:
	rm -rf $(TMPVC)
	$(GITFASTCLONE) $(VCREPO) $(TMPVC)

fresh_tcc:
	rm -rf $(TMPTCC)/  
	$(GITFASTCLONE) $(TCCREPO) $(TMPTCC)  

$(TMPTCC)/.git/config:
	make fresh_tcc

$(TMPVC)/.git/config:
	make fresh_vc

selfcompile:
	./v -o v v.v

modules:
	./v build module vlib/builtin > /dev/null
	./v build module vlib/strings > /dev/null
	./v build module vlib/strconv > /dev/null
