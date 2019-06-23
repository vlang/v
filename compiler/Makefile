# Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
# Use of this source code is governed by an MIT license
# that can be found in the LICENSE file.

all: v vc

v: vc
	./vc -o v .

vc: v.c
	cc -w -o vc v.c

v.c:
	wget https://vlang.io/v.c

clean:
	-rm v vc v.c
