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

.PHONY: all clean fresh_vc fresh_tcc

ifdef prod
VFLAGS+=-prod
endif

all: latest_vc latest_tcc
ifdef WIN32
	$(CC) $(CFLAGS) -g -std=c99 -municode -w -o $(V) $(VC)/$(VCFILE) $(LDFLAGS)
	$(V) -o v2.exe $(VFLAGS) cmd/v
	move /y v2.exe v.exe
else
	$(CC) $(CFLAGS) -g -std=gnu99 -w -o $(V) $(VC)/$(VCFILE) -lm -lpthread $(LDFLAGS)
	$(V) -o v2.exe $(VFLAGS) cmd/v
	mv -f v2.exe v  
endif
	@echo "V has been successfully built"
	@$(V) -version

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
	rm -rf $(VC)
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
# Check wether a TCC branch exists for the user's system configuration.
ifneq (,$(findstring thirdparty-$(TCCOS)-$(TCCARCH), $(shell git ls-remote --heads $(TCCREPO) | sed 's/^[a-z0-9]*\trefs.heads.//')))
	$(GITFASTCLONE) --branch thirdparty-$(TCCOS)-$(TCCARCH) $(TCCREPO) $(TMPTCC)
else
	@echo 'Pre-built TCC not available for thirdparty-$(TCCOS)-$(TCCARCH) at $(TCCREPO), will use the system compiler: $(CC)'
	$(GITFASTCLONE) --branch thirdparty-unknown-unknown $(TCCREPO) $(TMPTCC)
endif

$(TMPTCC)/.git/config:
	$(MAKE) fresh_tcc

$(VC)/.git/config:
	$(MAKE) fresh_vc

asan:
	$(MAKE) all CFLAGS='-fsanitize=address,undefined'

selfcompile:
	$(V) -cg -o v cmd/v

selfcompile-static:
	$(V) -cg -cflags '--static' -o v-static cmd/v

### NB: Please keep this Makefile and make.bat simple.
install:
	@echo 'Please use `sudo v symlink` instead.'
    
