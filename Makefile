CC ?= cc
VFLAGS ?=
CFLAGS ?=
LDFLAGS ?=

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
	$(CC) $(CFLAGS) -std=gnu11 -w -o v1 vc/v.c -lm -lexecinfo -lpthread $(LDFLAGS) || cmd/tools/cc_compilation_failed_non_windows.sh
	./v1 -no-parallel -o v2 $(VFLAGS) cmd/v
	./v2 -o v $(VFLAGS) cmd/v
	rm -rf v1 v2
	./v run ./cmd/tools/detect_tcc.v
	@echo "V has been successfully built"
	./v version
	./v run .github/problem-matchers/register_all.vsh

check:
	./v test-all

install:
	@echo 'Please use `sudo ./v symlink` instead, or manually add the current directory to your PATH.'
