CC ?= cc
VFLAGS ?=

all:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc
	$(CC) -std=gnu11 -w -I ./thirdparty/stdatomic/nix -o v1 vc/v.c -lm -lexecinfo -lpthread
	./v1 -no-parallel -o v2 $(VFLAGS) cmd/v
	./v2 -o v $(VFLAGS) cmd/v
	rm -rf v1 v2 vc/
	@echo "V has been successfully built"
	./v run ./cmd/tools/detect_tcc.v
