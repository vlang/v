V_PATH := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))../

all: v vc

v: vc
	V_PATH=$(V_PATH) ./vc -o v .

vc: bootstrap/v.c
	cc -std=gnu11 -w -o vc ./bootstrap/v.c

clean:
	-rm v vc v.c
