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

all: latest_vc latest_tcc
	$(CC) $(CFLAGS) -std=gnu11 -w -o v $(TMPVC)/$(VCFILE) $(LDFLAGS) -lm
ifdef ANDROID
	chmod 755 v
endif
	@./v self

ifdef V_ALWAYS_CLEAN_TMP
	$(MAKE) clean_tmp
endif
	@echo "V has been successfully built"
	@./v -version

clean: clean_tmp
	@git clean -xf

clean_tmp:
	@rm -rf $(TMPTCC)
	@rm -rf $(TMPVC)

latest_vc: $(TMPVC)/.git/config
	@echo "Building V"
	@git version
	@echo "Downloading v.c..."
	@cd $(TMPVC) $(GITCLEANPULL)

fresh_vc:
	@rm -rf $(TMPVC)
	$(GITFASTCLONE) $(VCREPO) $(TMPVC)

latest_tcc: $(TMPTCC)/.git/config
ifndef ANDROID
	@cd $(TMPTCC) && $(GITCLEANPULL)
endif

fresh_tcc:
ifndef ANDROID
	@rm -rf $(TMPTCC)
	$(GITFASTCLONE) $(TCCREPO) $(TMPTCC)
endif

$(TMPTCC)/.git/config:
	$(MAKE) fresh_tcc

$(TMPVC)/.git/config:
	$(MAKE) fresh_vc

selfcompile:
	./v -keepc -cg -o v cmd/v

selfcompile-static:
	./v -keepc -cg -cflags '--static' -o v-static cmd/v
