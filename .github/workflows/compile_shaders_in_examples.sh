#!/usr/bin/env bash

for f in examples/sokol/*/ ; do
	echo "compiling shaders for $f ...";
	time ./v shader $f;
	echo "done";
done;
