all: v vc

v: vc
	./vc -o v .

vc: v.c
	cc -std=gnu11 -w -o vc v.c

clean:
	-rm v vc
