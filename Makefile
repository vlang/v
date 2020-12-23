CC ?= cc
CFLAGS ?=
LDFLAGS ?=
TMPDIR ?= /tmp
VROOT  ?= .
VC     ?= ./vc
V      ?= ./v
VCREPO ?= https://github.com/vlang/vc
TCCREPO ?= https://github.com/vlang/tccbin

VCFILE := v.c
TMPTCC := $(VROOT)/thirdparty/tcc
TCCOS := unknown
TCCARCH := unknown
GITCLEANPULL := git clean -xf && git pull --quiet
GITFASTCLONE := git clone --depth 1 --quiet --single-branch

#### Platform detections and overrides:
_SYS := $(shell uname 2>/dev/null || echo Unknown)
_SYS := $(patsubst MSYS%,MSYS,$(_SYS))
_SYS := $(patsubst MINGW%,MinGW,$(_SYS))

ifneq ($(filter $(_SYS),MSYS MinGW),)
WIN32 := 1
V:=./v.exe
endif

ifeq ($(_SYS),Linux)
LINUX := 1
TCCOS := linux
endif

ifeq ($(_SYS),Darwin)
MAC := 1
TCCOS := macos
endif

ifeq ($(_SYS),FreeBSD)
TCCOS := freebsd
LDFLAGS += -lexecinfo
endif

ifdef ANDROID_ROOT
ANDROID := 1
undefine LINUX
TCCOS := android
endif
#####

ifdef WIN32
TCCOS := windows
VCFILE := v_win.c
endif

TCCARCH := $(shell uname -m 2>/dev/null || echo unknown)

ifeq ($(TCCARCH),x86_64)
	TCCARCH := amd64
else
ifneq ($(filter x86%,$(TCCARCH)),)
	TCCARCH := i386
else
ifeq ($(TCCARCH),aarch64)
	TCCARCH := arm64
else
ifneq ($(filter arm%,$(TCCARCH)),)
	TCCARCH := arm
# otherwise, just use the arch name
endif
endif
endif
endif

# note that a branch may not exist yet for the user's system configuration,
# in that case they'll get an error from git while cloning it.
# TODO: print a pretty error ourselves in that case, and ask the user to open a feature request
TCCBRANCH := thirdparty-$(TCCOS)-$(TCCARCH)

all: latest_vc latest_tcc
ifdef WIN32
	$(CC) $(CFLAGS) -g -std=c99 -municode -w -o $(V) $(VC)/$(VCFILE) $(LDFLAGS)
ifdef prod
	$(V) -prod self
else
	$(V) self
endif
else
	$(CC) $(CFLAGS) -g -std=gnu99 -w -o $(V) $(VC)/$(VCFILE) -lm -lpthread $(LDFLAGS)
ifdef ANDROID
	chmod 755 v
endif

ifdef prod
	$(V) -prod self
else
	$(V) self
endif

ifndef ANDROID
	$(MAKE) modules
endif
endif
	@echo "V has been successfully built"
	@$(V) -version

#clean: clean_tmp
#git clean -xf

clean:
	rm -rf $(TMPTCC)
	rm -rf $(VC)

latest_vc: $(VC)/.git/config
ifndef local
	cd $(VC) && $(GITCLEANPULL)
else
	@echo "Using local vc"
endif

fresh_vc:
	$(GITFASTCLONE) $(VCREPO) $(VC)

latest_tcc: $(TMPTCC)/.git/config
ifndef ANDROID
ifndef local
	cd $(TMPTCC) && $(GITCLEANPULL)
else
	@echo "Using local tcc"
endif
endif

fresh_tcc:
	rm -rf $(TMPTCC)
	$(GITFASTCLONE) --branch $(TCCBRANCH) $(TCCREPO) $(TMPTCC)

$(TMPTCC)/.git/config:
	$(MAKE) fresh_tcc

$(VC)/.git/config:
	$(MAKE) fresh_vc

selfcompile:
	$(V) -cg -o v cmd/v

selfcompile-static:
	$(V) -cg -cflags '--static' -o v-static cmd/v

modules: module_builtin module_strings module_strconv
module_builtin:
	#$(V) build module vlib/builtin > /dev/null
module_strings:
	#$(V) build module vlib/strings > /dev/null
module_strconv:
	#$(V) build module vlib/strconv > /dev/null
