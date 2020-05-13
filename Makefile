CC ?= cc
CFLAGS ?=
LDFLAGS ?=
TMPDIR ?= /tmp

VCFILE := v.c
TMPVC  := $(TMPDIR)/v.c

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
VCFILE := v_win.c
endif

all: latest_vc
ifdef WIN32
	$(CC) $(CFLAGS) -g -std=c99 -municode -w -o v.exe $(TMPVC) $(LDFLAGS)
	./v.exe self
else
	$(CC) $(CFLAGS) -g -std=gnu11 -w -o v $(TMPVC) $(LDFLAGS) -lm
ifdef ANDROID
	chmod 755 v
endif
	./v self
ifndef ANDROID
	$(MAKE) modules
endif
endif
ifdef V_ALWAYS_CLEAN_TMP
	$(MAKE) clean_tmp
endif
	@echo "V has been successfully built"
	@./v -version

clean: clean_tmp
	git clean -xf

clean_tmp:
	rm $(TMPVC)

latest_vc:
	curl "https://raw.githubusercontent.com/vlang/vc/master/v.c" -o $(TMPVC) -s

selfcompile:
	./v -keepc -cg -o v cmd/v

selfcompile-static:
	./v -keepc -cg -cflags '--static' -o v-static cmd/v

modules: module_builtin module_strings module_strconv
module_builtin:
	#./v build module vlib/builtin > /dev/null
module_strings:
	#./v build module vlib/strings > /dev/null
module_strconv:
	#./v build module vlib/strconv > /dev/null
