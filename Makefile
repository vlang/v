CC ?= cc
CFLAGS ?=
LDFLAGS ?=
TMPDIR ?= /tmp

VCFILE := v.c
TMPVC  := $(TMPDIR)/vc
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

ifeq ($(_SYS),FreeBSD)
LDFLAGS += -lexecinfo
endif

ifdef ANDROID_ROOT
ANDROID := 1
undefine LINUX
endif
#####

ifdef WIN32
TCCREPO := https://github.com/vlang/tccbin_win
VCFILE := v_win.c
endif

all: latest_vc latest_tcc
ifdef WIN32
	$(CC) $(CFLAGS) -std=c99 -municode -w -o v.exe $(TMPVC)/$(VCFILE) $(LDFLAGS)
	./v.exe self
else
	$(CC) $(CFLAGS) -std=gnu11 -w -o v $(TMPVC)/$(VCFILE) $(LDFLAGS) -lm
ifdef ANDROID
	chmod 755 v
endif
	./v self
ifndef ANDROID
	$(MAKE) modules
endif
endif
	@echo "V has been successfully built"

clean:
	rm -rf $(TMPTCC)
	rm -rf $(TMPVC)
	git clean -xf

latest_vc: $(TMPVC)/.git/config
	cd $(TMPVC) && $(GITCLEANPULL)

fresh_vc:
	rm -rf $(TMPVC)
	$(GITFASTCLONE) $(VCREPO) $(TMPVC)

latest_tcc: $(TMPTCC)/.git/config
ifndef ANDROID
	cd $(TMPTCC) && $(GITCLEANPULL)
endif

fresh_tcc:
ifndef ANDROID
	rm -rf $(TMPTCC)
	$(GITFASTCLONE) $(TCCREPO) $(TMPTCC)
endif

$(TMPTCC)/.git/config:
	$(MAKE) fresh_tcc

$(TMPVC)/.git/config:
	$(MAKE) fresh_vc

selfcompile:
	./v -keep_c -cg -o v cmd/v

selfcompile-static:
	./v -keep_c -cg -cflags '--static' -o v-static cmd/v

modules: module_builtin module_strings module_strconv
module_builtin:
	#./v build module vlib/builtin > /dev/null
module_strings:
	#./v build module vlib/strings > /dev/null
module_strconv:
	#./v build module vlib/strconv > /dev/null
