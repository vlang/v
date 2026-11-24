#!/bin/sh
# Generate first so V's frontend memory is released before native optimization.
# With a fourth argument, only generate the C project into that directory: another
# machine (such as the x86 host of an emulated CI job) compiles it with `make -C`.
set -eu
compiler=$1
output=$2
cc=$3
keep_project=${4:-}
case "$compiler" in /*) ;; *) compiler="$PWD/$compiler" ;; esac
case "$output" in /*) ;; *) output="$PWD/$output" ;; esac
if [ -n "$keep_project" ]; then
  case "$keep_project" in /*) ;; *) keep_project="$PWD/$keep_project" ;; esac
  rm -rf "$keep_project"
  mkdir -p "$keep_project"
  "$compiler" self -prod -cc "$cc" -parallel-cc -generate-c-project "$keep_project" -o "$output"
  exit 0
fi
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
