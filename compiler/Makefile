all: v

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o v .

v.c:
	curl -Os https://raw.githubusercontent.com/vlang/vc/master/v.c

test:
	find .. -name '*_test.v' | xargs v {}

clean:
	-rm v.c
