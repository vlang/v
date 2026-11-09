#!/bin/sh
# Generate first so V's frontend memory is released before native optimization.
set -eu
compiler=$1
output=$2
cc=$3
case "$compiler" in /*) ;; *) compiler="$PWD/$compiler" ;; esac
case "$output" in /*) ;; *) output="$PWD/$output" ;; esac
project=$(mktemp -d "${output}.build.XXXXXX")
trap 'rm -rf "$project"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# These are vself's options; putting the export option before `self` would
# export the vself helper instead of the compiler it is meant to build.
"$compiler" self -prod -cc "$cc" -parallel-cc -generate-c-project "$project" -o "$output"
make --no-print-directory -C "$project"
binary="$project/v"
[ -f "$binary" ] || binary="$project/v.exe"
chmod 755 "$binary"
mv -f "$binary" "$output"
