CC ?= cc
CPPFLAGS ?=
CFLAGS ?=
LDFLAGS ?=
TMPDIR ?= /tmp
VROOT  ?= .
VC     ?= ./vc
VEXE   ?= ./v
VCREPO ?= https://github.com/vlang/vc
TCCREPO ?= https://github.com/vlang/tccbin
LEGACYREPO ?= https://github.com/macports/macports-legacy-support
GIT ?= git

VCFILE := v.c
TMPTCC := $(VROOT)/thirdparty/tcc
LEGACYLIBS := $(VROOT)/thirdparty/legacy
TMPLEGACY := $(LEGACYLIBS)/source
TCCOS := unknown
TCCARCH := unknown
HAS_GIT := $(shell command -v $(GIT) >/dev/null 2>&1 && echo 1 || echo 0)
GITCLEANPULL := $(GIT) clean -xf && $(GIT) pull --quiet
GITFASTCLONE := $(GIT) clone --filter=blob:none --quiet

#### Platform detections and overrides:
_SYS := $(shell uname 2>/dev/null || echo Unknown)
_SYS := $(patsubst MSYS%,MSYS,$(_SYS))
_SYS := $(patsubst MINGW%,MinGW,$(_SYS))

ifneq ($(filter $(_SYS),MSYS MinGW),)
WIN32 := 1
EXE_EXT := .exe
# GNU make defaults CC to `cc`, but mingw32-make installations often only
# provide `gcc`. Switch only the implicit default and preserve explicit CC=...
ifneq ($(filter $(origin CC),default file),)
ifeq ($(CC),cc)
CC := gcc
endif
endif
endif

ifeq ($(_SYS),Linux)
LINUX := 1
TCCOS := linux
ifneq ($(shell ldd --version 2>&1 | grep -i musl),)
TCCOS := linuxmusl
endif
endif

ifeq ($(_SYS),Darwin)
MAC := 1
TCCOS := macos
ifeq ($(shell expr $(shell uname -r | cut -d. -f1) \<= 16), 1)
LEGACY := 1
CPPFLAGS += -I$(LEGACYLIBS)/include/LegacySupport
LDFLAGS += -L$(LEGACYLIBS)/lib
LDFLAGS += -lMacportsLegacySupport
VFLAGS += -cc $(CC)
VFLAGS += -cflags "$(strip $(CPPFLAGS) $(CFLAGS))"
VFLAGS += -ldflags -L$(LEGACYLIBS)/lib
VFLAGS += -cflags $(LEGACYLIBS)/lib/libMacportsLegacySupport.a
endif
endif

ifeq ($(_SYS),FreeBSD)
TCCOS := freebsd
LDFLAGS += -lexecinfo
endif

ifeq ($(_SYS),NetBSD)
NETBSD := 1
TCCOS := netbsd
LDFLAGS += -lexecinfo
endif

ifeq ($(_SYS),OpenBSD)
TCCOS := openbsd
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

TCCBUILDSCRIPT = $(VROOT)/thirdparty/build_scripts/thirdparty-$(TCCOS)-$(TCCARCH)_tcc.sh

.PHONY: all clean rebuild check fresh_vc fresh_tcc fresh_legacy latest_tcc_source check_for_working_tcc etags ctags

ifdef prod
VFLAGS+=-prod
endif

# Keep bootstrap C compiler/linker flags aligned with the initial `v1` build.
BOOTSTRAP_CFLAGS := $(strip $(CPPFLAGS) $(CFLAGS))
BOOTSTRAP_LDFLAGS := $(strip $(LDFLAGS))
ifeq ($(LINUX),1)
ifeq ($(TCCARCH),arm)
BOOTSTRAP_LDFLAGS := $(strip $(BOOTSTRAP_LDFLAGS) -latomic)
endif
endif
BOOTSTRAP_CCOMPILER_VFLAG :=
ifeq ($(LINUX),1)
ifneq ($(filter $(TCCARCH),arm64 aarch64),)
ifeq ($(filter -cc,$(VFLAGS)),)
ifeq ($(findstring -cc=,$(VFLAGS)),)
	# Bundled TCC can hang when bootstrapping V on Linux ARM64, so use the
	# same system compiler as the initial `v1` build unless the user overrode it.
	BOOTSTRAP_CCOMPILER_VFLAG := -cc "$(CC)"
endif
endif
endif
endif
BOOTSTRAP_VFLAGS := $(BOOTSTRAP_CCOMPILER_VFLAG) $(if $(strip $(BOOTSTRAP_CFLAGS)),-cflags "$(BOOTSTRAP_CFLAGS)") $(if $(strip $(BOOTSTRAP_LDFLAGS)),-ldflags "$(BOOTSTRAP_LDFLAGS)")

all: latest_vc latest_tcc latest_legacy
ifdef WIN32
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c99 -municode -w -o v1$(EXE_EXT) $(VC)/$(VCFILE) $(LDFLAGS) -lws2_32 || cmd/tools/cc_compilation_failed_windows.sh
	./v1$(EXE_EXT) -no-parallel -o v2$(EXE_EXT) $(VFLAGS) $(BOOTSTRAP_VFLAGS) cmd/v
	./v2$(EXE_EXT) -o $(VEXE)$(EXE_EXT) $(VFLAGS) $(BOOTSTRAP_VFLAGS) cmd/v
	$(RM) v1$(EXE_EXT)
	$(RM) v2$(EXE_EXT)
else
ifdef LEGACY
	$(MAKE) -C $(TMPLEGACY) CPPFLAGS='$(CPPFLAGS)' CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)'
	$(MAKE) -C $(TMPLEGACY) PREFIX=$(realpath $(LEGACYLIBS)) CPPFLAGS='$(CPPFLAGS)' CFLAGS='$(CFLAGS)' LDFLAGS='$(LDFLAGS)' install
	rm -rf $(TMPLEGACY)
	$(eval override LDFLAGS+=-L$(realpath $(LEGACYLIBS))/lib -lMacportsLegacySupport)
endif
	$(CC) $(CPPFLAGS) $(CFLAGS) -std=c99 -w -o v1$(EXE_EXT) $(VC)/$(VCFILE) -lm -lpthread $(BOOTSTRAP_LDFLAGS) || cmd/tools/cc_compilation_failed_non_windows.sh
ifdef NETBSD
	paxctl +m v1$(EXE_EXT)
endif
	./v1$(EXE_EXT) -no-parallel -o v2$(EXE_EXT) $(VFLAGS) $(BOOTSTRAP_VFLAGS) cmd/v
ifdef NETBSD
	paxctl +m v2$(EXE_EXT)
endif
	./v2$(EXE_EXT) -nocache -o $(VEXE)$(EXE_EXT) $(VFLAGS) $(BOOTSTRAP_VFLAGS) cmd/v
ifdef NETBSD
	paxctl +m $(VEXE)$(EXE_EXT)
endif
	rm -rf v1$(EXE_EXT) v2$(EXE_EXT)
endif
	@$(VEXE)$(EXE_EXT) run cmd/tools/detect_tcc.v
	@echo "V has been successfully built"
	@$(VEXE)$(EXE_EXT) -version
	@$(VEXE)$(EXE_EXT) run .github/problem-matchers/register_all.vsh

clean:
	rm -rf $(TMPTCC)
	rm -rf $(LEGACYLIBS)
	rm -rf $(VC)

rebuild: clean all

ifndef local
latest_vc: $(VC)/.git/config
ifeq ($(HAS_GIT),1)
	cd $(VC) && $(GITCLEANPULL)
else
	@echo "git not found; using existing $(VC)/$(VCFILE)"
endif
else
latest_vc:
	@echo "Using local vc"
endif

check_for_working_tcc:
	@$(TMPTCC)/tcc.exe --version > /dev/null 2> /dev/null || echo "The executable '$(TMPTCC)/tcc.exe' does not work."

fresh_vc:
	rm -rf $(VC)
ifeq ($(HAS_GIT),1)
	$(GITFASTCLONE) $(VCREPO) $(VC)
else
	@echo "git is required to clone $(VCREPO) into $(VC)"
	@exit 1
endif

ifndef local
latest_tcc: $(TMPTCC)/.git/config
ifeq ($(HAS_GIT),1)
ifdef WIN32
	@if [ -f "$(TMPTCC)/lib/advapi32.def" ]; then \
		cd "$(TMPTCC)" && $(GIT) checkout -- lib/advapi32.def > /dev/null 2> /dev/null || true; \
	fi
endif
	cd $(TMPTCC) && $(GITCLEANPULL)
ifdef WIN32
	@if [ -f "$(TMPTCC)/lib/advapi32.def" ]; then \
		for sym in RegEnumKeyExW RegEnumValueW RegQueryInfoKeyW; do \
			grep -qx "$$sym" "$(TMPTCC)/lib/advapi32.def" || printf '%s\n' "$$sym" >> "$(TMPTCC)/lib/advapi32.def"; \
		done; \
	fi
endif
else
	@echo "git not found; skipping update of $(TMPTCC)"
endif
ifneq (,$(wildcard ./tcc.exe))
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif

else
latest_tcc:
	@echo "Using local tcc"
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif

# Rebuild the bundled TCC in-place from upstream tinycc, while preserving the
# V-specific libgc/openlibm files already stored in $(TMPTCC).
latest_tcc_source: $(TMPTCC)/.git/config
ifeq ($(HAS_GIT),1)
ifneq (,$(wildcard $(TCCBUILDSCRIPT)))
	@TCC_FOLDER='$(TMPTCC)' $(if $(strip $(TCC_COMMIT)),TCC_COMMIT='$(TCC_COMMIT)') CC='$(CC)' bash '$(TCCBUILDSCRIPT)'
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
else
	@echo 'No upstream TinyCC build script is available for thirdparty-$(TCCOS)-$(TCCARCH).'
	@echo 'Use `make latest_tcc` to refresh the prebuilt bundle from $(TCCREPO).'
	@exit 1
endif
else
	@echo "git is required to bootstrap $(TMPTCC) before rebuilding it from source"
	@exit 1
endif

fresh_tcc:
	rm -rf $(TMPTCC)
ifndef local
ifeq ($(HAS_GIT),1)
	@set -e; \
	branches="$$( $(GIT) ls-remote --heads $(TCCREPO) 2> /dev/null | awk '{sub("refs/heads/","",$$2); print $$2}' || true )"; \
	preferred_branch='thirdparty-$(TCCOS)-$(TCCARCH)'; \
	fallback_branch=''; \
	if [ "$(LINUX)" = "1" ]; then \
		fallback_branch='thirdparty-linuxmusl-$(TCCARCH)'; \
	fi; \
	selected_branch=''; \
	if printf '%s\n' "$$branches" | grep -Fx "$$preferred_branch" > /dev/null; then \
		selected_branch="$$preferred_branch"; \
	elif [ "$$fallback_branch" != '' ] && [ "$$fallback_branch" != "$$preferred_branch" ] \
		&& printf '%s\n' "$$branches" | grep -Fx "$$fallback_branch" > /dev/null; then \
		selected_branch="$$fallback_branch"; \
	fi; \
	if [ "$$selected_branch" = '' ]; then \
		echo "Pre-built TCC not available for $$preferred_branch at $(TCCREPO), will use the system compiler: $(CC)"; \
		$(GITFASTCLONE) --branch thirdparty-unknown-unknown $(TCCREPO) "$(TMPTCC)"; \
	else \
		$(GITFASTCLONE) --branch "$$selected_branch" $(TCCREPO) "$(TMPTCC)"; \
		if [ -f "$(TMPTCC)/lib/advapi32.def" ]; then \
			for sym in RegEnumKeyExW RegEnumValueW RegQueryInfoKeyW; do \
				grep -qx "$$sym" "$(TMPTCC)/lib/advapi32.def" || printf '%s\n' "$$sym" >> "$(TMPTCC)/lib/advapi32.def"; \
			done; \
		fi; \
		if ! "$(TMPTCC)/tcc.exe" --version > /dev/null 2> /dev/null; then \
			if [ "$$fallback_branch" != '' ] && [ "$$fallback_branch" != "$$selected_branch" ] \
				&& printf '%s\n' "$$branches" | grep -Fx "$$fallback_branch" > /dev/null; then \
				echo "Pre-built TCC bundle $$selected_branch did not run; retrying with $$fallback_branch."; \
				rm -rf "$(TMPTCC)"; \
				$(GITFASTCLONE) --branch "$$fallback_branch" $(TCCREPO) "$(TMPTCC)"; \
			fi; \
			$(MAKE) --quiet check_for_working_tcc 2> /dev/null; \
		else \
			$(MAKE) --quiet check_for_working_tcc 2> /dev/null; \
		fi; \
	fi
else
	@echo "git is required to clone $(TCCREPO)"
	@exit 1
endif
else
	@echo "Using local tccbin"
	@$(MAKE) --quiet check_for_working_tcc 2> /dev/null
endif

ifndef local
latest_legacy: $(TMPLEGACY)/.git/config
ifdef LEGACY
ifeq ($(HAS_GIT),1)
	cd $(TMPLEGACY) && $(GITCLEANPULL)
else
	@echo "git not found; using existing $(TMPLEGACY)"
endif
endif
else
latest_legacy:
ifdef LEGACY
	@echo "Using local legacysupport"
endif
endif

fresh_legacy:
	rm -rf $(LEGACYLIBS)
ifeq ($(HAS_GIT),1)
	$(GITFASTCLONE) $(LEGACYREPO) $(TMPLEGACY)
else
	@echo "git is required to clone $(LEGACYREPO)"
	@exit 1
endif

$(TMPTCC)/.git/config:
ifeq ($(HAS_GIT),1)
	$(MAKE) fresh_tcc
else
	@echo "git not found; skipping bootstrap of $(TMPTCC), system compiler $(CC) will be used"
endif

$(VC)/.git/config:
ifeq ($(HAS_GIT),1)
	$(MAKE) fresh_vc
else
	@if [ -f "$(VC)/$(VCFILE)" ]; then \
		echo "git not found; using existing $(VC)/$(VCFILE)"; \
	else \
		echo "git is required to download $(VC)/$(VCFILE). Install git or provide the file manually."; \
		exit 1; \
	fi
endif

$(TMPLEGACY)/.git/config:
ifdef LEGACY
ifeq ($(HAS_GIT),1)
	$(MAKE) fresh_legacy
else
	@if [ -d "$(TMPLEGACY)" ]; then \
		echo "git not found; using existing $(TMPLEGACY)"; \
	else \
		echo "git is required to download legacy support sources ($(LEGACYREPO))"; \
		exit 1; \
	fi
endif
endif

asan:
	$(MAKE) all CFLAGS='-fsanitize=address,undefined'

selfcompile:
	$(VEXE)$(EXE_EXT) -cg -o v cmd/v

selfcompile-static:
	$(VEXE)$(EXE_EXT) -cg -cflags '--static' -o v-static cmd/v

### NB: Please keep this Makefile and makev.bat simple.
install:
	@echo 'Please use `sudo ./v symlink` instead, or manually add the current directory to your PATH.'

check:
	$(VEXE)$(EXE_EXT) test-all

etags:
	./v$(EXE_EXT) -print-v-files cmd/v | grep -v :parse_text| etags -L -

ctags:
	./v$(EXE_EXT) -print-v-files cmd/v | grep -v :parse_text| ctags -L -
