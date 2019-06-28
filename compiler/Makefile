all: v

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o v .

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

test: v
	find .. -name '*_test.v' -print0 | xargs -0 -n1 ./v
	echo "Building V examples..." 
	find ../examples -name '*.v' -print0 | xargs -0 -n1 ./v

clean:
	-rm -f v.c v
