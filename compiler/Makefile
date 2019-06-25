all: v vc rm

v: vc
	./bin/vc -o bin/v .

vc: v.c
	mkdir -p bin && cc -std=gnu11 -w -o bin/vc v.c

v.c:
	wget https://vlang.io/v.c

rm:
	-rm bin/vc v.c

clean:
	-rm bin v.c
