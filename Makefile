CC ?= cc
VFLAGS ?=
CFLAGS ?=
LDFLAGS ?=

all: download_vc v

.PHONY: all check download_vc install v

download_vc:
	@set -e; \
	if [ -f vc/v.c ]; then \
		if command -v git >/dev/null 2>&1; then \
			git -C vc/ pull --rebase; \
		else \
			echo "git not found; using existing vc/v.c"; \
		fi; \
	else \
		if command -v git >/dev/null 2>&1; then \
			git clone --filter=blob:none https://github.com/vlang/vc vc/; \
		else \
			echo "git is required to download vc/. Please install git or provide vc/v.c."; \
			exit 1; \
		fi; \
	fi

v:
	@set -e; \
	sys=`uname -s 2>/dev/null || echo unknown`; \
	arch=`uname -m 2>/dev/null || echo unknown`; \
	set -- $(CFLAGS); \
	ccflags=; \
	unsafe_o=0; \
	for arg do \
		case "$$arg" in \
			-O|-O0|-O1) \
				ccflags="$$ccflags $$arg"; \
				;; \
			-O*) \
				ccflags="$$ccflags $$arg"; \
				unsafe_o=1; \
				;; \
			*) \
				ccflags="$$ccflags $$arg"; \
				;; \
		esac; \
	done; \
	ccflags=$${ccflags# }; \
	bootstrap_ccflags=$$ccflags; \
	set -- $(LDFLAGS); \
	ldflags=; \
	for arg do \
		ldflags="$$ldflags $$arg"; \
	done; \
	ldflags=$${ldflags# }; \
	case "$$sys" in \
		Linux) \
			case "$$arch" in \
				arm*) \
					ldflags="$$ldflags -latomic"; \
					;; \
			esac; \
			;; \
		FreeBSD|NetBSD|OpenBSD) \
			ldflags="$$ldflags -lexecinfo"; \
			;; \
	esac; \
	ccflags=$${ccflags# }; \
	ldflags=$${ldflags# }; \
	if [ "$$sys" = Linux ]; then \
		case "$$arch" in \
			arm64|aarch64) \
				if [ $$unsafe_o -eq 1 ]; then \
					set -- $$ccflags; \
					bootstrap_ccflags=; \
					for arg do \
						case "$$arg" in \
							-O|-O0|-O1) \
								bootstrap_ccflags="$$bootstrap_ccflags $$arg"; \
								;; \
							-O*) \
								bootstrap_ccflags="$$bootstrap_ccflags -O1"; \
								;; \
							*) \
								bootstrap_ccflags="$$bootstrap_ccflags $$arg"; \
								;; \
						esac; \
					done; \
					bootstrap_ccflags=$${bootstrap_ccflags# }; \
				fi; \
				;; \
		esac; \
	fi; \
	$(CC) $$bootstrap_ccflags -std=gnu11 -w -o v1 vc/v.c -lm -lpthread $$ldflags || cmd/tools/cc_compilation_failed_non_windows.sh; \
	set -- ./v1 -no-parallel -o v2 $(VFLAGS); \
	if [ -n "$$bootstrap_ccflags" ]; then \
		set -- "$$@" -cflags "$$bootstrap_ccflags"; \
	fi; \
	if [ -n "$$ldflags" ]; then \
		set -- "$$@" -ldflags "$$ldflags"; \
	fi; \
	set -- "$$@" cmd/v; \
	"$$@"; \
	set -- ./v2 -o v $(VFLAGS); \
	if [ -n "$$ccflags" ]; then \
		set -- "$$@" -cflags "$$ccflags"; \
	fi; \
	if [ -n "$$ldflags" ]; then \
		set -- "$$@" -ldflags "$$ldflags"; \
	fi; \
	set -- "$$@" cmd/v; \
	"$$@"; \
	rm -rf v1 v2; \
	./v run ./cmd/tools/detect_tcc.v; \
	echo "V has been successfully built"; \
	./v version; \
	./v run .github/problem-matchers/register_all.vsh

check:
	./v test-all

install:
	@echo 'Please use `sudo ./v symlink` instead, or manually add the current directory to your PATH.'
