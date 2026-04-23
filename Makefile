CC ?= cc
VFLAGS ?=
CFLAGS ?=
LDFLAGS ?=
BOOTSTRAP_CC_CFLAGS := $(strip $(CFLAGS))
BOOTSTRAP_VC_CC_CFLAGS := $(BOOTSTRAP_CC_CFLAGS)
BOOTSTRAP_VC_CFLAGS := $(BOOTSTRAP_CC_CFLAGS)
BOOTSTRAP_CFLAGS := $(BOOTSTRAP_CC_CFLAGS)
BOOTSTRAP_LDFLAGS := $(strip $(LDFLAGS))

ifeq ($(shell uname -s 2>/dev/null),Linux)
ifneq ($(filter arm%,$(shell uname -m 2>/dev/null)),)
BOOTSTRAP_LDFLAGS := $(strip $(BOOTSTRAP_LDFLAGS) -latomic)
endif
ifneq ($(filter $(shell uname -m 2>/dev/null),arm64 aarch64),)
BOOTSTRAP_VC_UNSAFE_OPTFLAGS := $(filter-out -O -O0 -O1,$(filter -O%,$(BOOTSTRAP_CC_CFLAGS)))
ifneq ($(BOOTSTRAP_VC_UNSAFE_OPTFLAGS),)
	# Some Linux ARM64 system compilers miscompile the external vc bootstrap
	# snapshot at -O2/-O3, making `v1` segfault before it can build `v2`.
	# Keep the bootstrap stages at -O1, but preserve the requested flags for
	# the final `v` build.
	BOOTSTRAP_VC_SAFE_CFLAGS := $(strip $(filter-out -O%,$(BOOTSTRAP_CC_CFLAGS)) -O1)
	BOOTSTRAP_VC_CC_CFLAGS := $(BOOTSTRAP_VC_SAFE_CFLAGS)
	BOOTSTRAP_VC_CFLAGS := $(BOOTSTRAP_VC_SAFE_CFLAGS)
endif
endif
endif

ifneq ($(filter $(shell uname -s 2>/dev/null),FreeBSD NetBSD OpenBSD),)
BOOTSTRAP_LDFLAGS := $(strip $(BOOTSTRAP_LDFLAGS) -lexecinfo)
endif

BOOTSTRAP_VC_VFLAGS := $(if $(strip $(BOOTSTRAP_VC_CFLAGS)),-cflags "$(BOOTSTRAP_VC_CFLAGS)") $(if $(strip $(BOOTSTRAP_LDFLAGS)),-ldflags "$(BOOTSTRAP_LDFLAGS)")
BOOTSTRAP_VFLAGS := $(if $(strip $(BOOTSTRAP_CFLAGS)),-cflags "$(BOOTSTRAP_CFLAGS)") $(if $(strip $(BOOTSTRAP_LDFLAGS)),-ldflags "$(BOOTSTRAP_LDFLAGS)")

.PHONY: all check download_vc v

all: download_vc v

download_vc:
	if [ -f vc/v.c ] ; then \
		if command -v git >/dev/null 2>&1 ; then \
			git -C vc/ pull; \
		else \
			echo "git not found; using existing vc/v.c"; \
		fi; \
	else \
		if command -v git >/dev/null 2>&1 ; then \
			git clone --filter=blob:none https://github.com/vlang/vc vc/; \
		else \
			echo "git is required to download vc/. Please install git or provide vc/v.c."; \
			exit 1; \
		fi; \
	fi

v:
	$(CC) $(BOOTSTRAP_VC_CC_CFLAGS) -std=gnu11 -w -o v1 vc/v.c -lm -lpthread $(BOOTSTRAP_LDFLAGS) || cmd/tools/cc_compilation_failed_non_windows.sh
	./v1 -no-parallel -o v2 $(VFLAGS) $(BOOTSTRAP_VC_VFLAGS) cmd/v
	./v2 -o v $(VFLAGS) $(BOOTSTRAP_VFLAGS) cmd/v
	rm -rf v1 v2
	./v run ./cmd/tools/detect_tcc.v
	@echo "V has been successfully built"
	./v version
	./v run .github/problem-matchers/register_all.vsh

check:
	./v test-all

install:
	@echo 'Please use `sudo ./v symlink` instead, or manually add the current directory to your PATH.'
