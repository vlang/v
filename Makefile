CC ?= cc
VFLAGS ?=
CFLAGS ?=
LDFLAGS ?=

.PHONY: all check download_vc v

all: download_vc v

download_vc:
	if [ -f vc/v.c ] ; then git -C vc/ pull; else git clone --filter=blob:none https://github.com/vlang/vc vc/; fi

v:
	$(CC) $(CFLAGS) -std=gnu11 -w -o v1 vc/v.c -lm -lexecinfo -lpthread $(LDFLAGS)
	./v1 -no-parallel -o v2 $(VFLAGS) cmd/v
	./v2 -o v $(VFLAGS) cmd/v
	rm -rf v1 v2
	@echo "V has been successfully built"
	./v run ./cmd/tools/detect_tcc.v

check:
	./v test-all
