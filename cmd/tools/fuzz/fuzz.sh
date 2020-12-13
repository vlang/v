#!/bin/sh

cores=$(nproc --all)

echo Number of cores: $cores
echo Compiling...
./v -cc clang -o cmd/tools/fuzz/map_fuzz cmd/tools/fuzz/map_fuzz.v -prod -cflags "-fsanitize=memory"

echo Fuzzing:
while true
do
  for ((i=1;i<=cores;++i))
  do
    sleep 0.001
    ./cmd/tools/fuzz/map_fuzz &
  done
  wait
done
