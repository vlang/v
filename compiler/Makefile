all: v vc

v: vc
	./vc -o v .

vc: v.c
	cc -std=c11 -w -o vc vc_repository/v.c

v.c:
	git clone git@github.com:vlang/vc.git vc_repository

clean:
	-rm -rf v vc vc_repository
