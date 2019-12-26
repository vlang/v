CC ?= cc

all:
	rm -rf vc/
	git clone --depth 1 --quiet https://github.com/vlang/vc
	$(CC) -std=gnu11 -w -o v vc/v.c -lm -lexecinfo
	rm -rf vc/
	@echo "V has been successfully built"
