#!/bin/zsh

for f in **/*_test.v ; do
        echo "Testing $f..."
        v $f || echo "fail"
done

for f in examples/*.v ; do
        echo "Building $f..."
        v $f || echo "fail"
done
