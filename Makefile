all: build-debug
	$(info V has been successfully built)

clean:
	-rm -f v.c v vprod

URL = https://raw.githubusercontent.com/vlang/vc/master/v.c

get_v.c:
	curl --remote-name --progress-bar $(URL) || \
	wget --timestamping --show-progress --quiet $(URL)

bootstrap: get_v.c
	cc -std=gnu11 -w -o v v.c -lm 

build-debug: bootstrap
	./v -o v compiler

build-release: bootstrap
	./v -prod -o v compiler

build-obf-release: bootstrap
	./v -prod -obf -o v compiler

build-test: bootstrap
	./v -prod -o vprod compiler # Test prod build

test: build-test
	echo "Running V tests..."
	find . -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..."
	find examples -name '*.v' -not -path "examples/hot_code_reloading/*" -print0 | xargs -0 -n1 ./v

strip:
	strip v

compact:
	upx -qqq --lzma v

debug: clean build-debug
release: clean build-release strip
obf-release: clean build-obf-release strip
