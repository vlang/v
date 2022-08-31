CC ?= cc
CFLAGS ?=
LDFLAGS ?=
TMPDIR ?= /tmp
VROOT  ?= .
VC     ?= ./vc
VEXE   ?= ./v
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
VEXE := ./v.exe
endif

ifeq ($(_SYS),Linux)
LINUX := 1
TCCOS := linux
ifneq ($(shell ldd /bin/ls | grep musl),)
TCCOS := linuxmusl
endif
endif

ifeq ($(_SYS),Darwin)
MAC := 1
TCCOS := macos
endif

ifeq ($(_SYS),FreeBSD)
TCCOS := freebsd
LDFLAGS += -lexecinfo
endif

ifeq ($(_SYS),NetBSD)
TCCOS := netbsd
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
ifeq ($(TCCARCH),arm64)
	TCCARCH := arm64
else
ifneq ($(filter arm%,$(TCCARCH)),)
	TCCARCH := arm
# otherwise, just use the arch name
endif
endif
endif
endif

.PHONY: all clean check fresh_vc fresh_tcc check_for_working_tcc

ifdef prod
VFLAGS+=-prod
endif

all: latest_vc latest_tcc
ifdef WIN32
	$(CC) $(CFLAGS) -std=c99 -municode -w -o v1.exe $(VC)/$(VCFILE) $(LDFLAGS)
	v1.exe -no-parallel -o v2.exe $(VFLAGS) cmd/v
	v2.exe -o $(VEXE) $(VFLAGS) cmd/v
	del v1.exe
	del v2.exe
else
	$(CC) $(CFLAGS) -std=gnu99 -w -o v1.exe $(VC)/$(VCFILE) -lm -lpthread $(LDFLAGS)
	./v1.exe -no-parallel -o v2.exe $(VFLAGS) cmd/v
	./v2.exe -o $(VEXE) $(VFLAGS) cmd/v
	rm -rf v1.exe v2.exe
endif
	@$(VEXE) run cmd/tools/detect_tcc.v
	@echo "V has been successfully built"
	@$(VEXE) -version

clean:
	rm -rf $(TMPTCC)
	rm -rf $(VC)

ifndef local
latest_vc: $(VC)/.git/config
	cd $(VC) && $(GITCLEANPULL)
else
latest_vc:
	@echo "Using local vc"
endif

check_for_working_tcc:
	@$(TMPTCC)/tcc.exe --version > /dev/null 2> /dev/null || echo "The executable '$(TMPTCC)/tcc.exe' does not work."

fresh_vc:
	rm -rf $(VC)
	$(GITFASTCLONE) $(VCREPO) $(VC)

ifndef local
latest_tcc: $(TMPTCC)/.git/config
	cd $(TMPTCC) && $(GITCLEANPULL)
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
else
latest_tcc:
	@echo "Using local tcc"
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif

fresh_tcc:
	rm -rf $(TMPTCC)
ifndef local
# Check wether a TCC branch exists for the user's system configuration.
ifneq (,$(findstring thirdparty-$(TCCOS)-$(TCCARCH), $(shell git ls-remote --heads $(TCCREPO) | sed 's/^[a-z0-9]*\trefs.heads.//')))
	$(GITFASTCLONE) --branch thirdparty-$(TCCOS)-$(TCCARCH) $(TCCREPO) $(TMPTCC)
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
else
	@echo 'Pre-built TCC not available for thirdparty-$(TCCOS)-$(TCCARCH) at $(TCCREPO), will use the system compiler: $(CC)'
	$(GITFASTCLONE) --branch thirdparty-unknown-unknown $(TCCREPO) $(TMPTCC)
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif
else
	@echo "Using local tccbin"
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif

$(TMPTCC)/.git/config:
	$(MAKE) fresh_tcc

$(VC)/.git/config:
	$(MAKE) fresh_vc

asan:
	$(MAKE) all CFLAGS='-fsanitize=address,undefined'

selfcompile:
	$(VEXE) -cg -o v cmd/v

selfcompile-static:
	$(VEXE) -cg -cflags '--static' -o v-static cmd/v

### NB: Please keep this Makefile and make.bat simple.
install:
	@echo 'Please use `sudo ./v symlink` instead.'

check:
	$(VEXE) test-all
