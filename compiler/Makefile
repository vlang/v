all: v

v: v.c
	cc -std=gnu11 -w -o v v.c
	./v -o v .

v.c:
	wget https://vlang.io/v.c

clean:
	-rm vc v.c
