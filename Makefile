all: clean v
	$(info V has been successfully built)

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o v compiler
	rm v.c

v.c: Makefile
	curl -Os https://github.com/vlang/vc/raw/23b4932859a91849d7401f3568a16dc97248c55f/v.c

test: v
	./v -prod -o vprod compiler # Test prod build
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c v vprod
