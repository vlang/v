#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 /absolute/install/prefix" >&2
  exit 2
fi
prefix=$1
case "$prefix" in
  /*) ;;
  *) echo 'SQLite install prefix must be absolute' >&2; exit 2 ;;
esac

# Match the amalgamation version used by db.sqlite's fallback installer, and
# verify the upstream source archive before unpacking it.
version=3530400
checksum=0e9483900e92cd5de8fd48d16bf9200145a61f7fd5be542a5ac81d8a9516eb9c
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/v-msan-sqlite.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
archive="sqlite-autoconf-$version.tar.gz"
curl --fail --location --output "$build_dir/$archive" \
  "https://www.sqlite.org/2026/$archive"
printf '%s  %s\n' "$checksum" "$build_dir/$archive" | sha256sum --check
tar -xzf "$build_dir/$archive" -C "$build_dir"
cd "$build_dir/sqlite-autoconf-$version"

# Configure probes describe the host ABI. Instrument only the library so that
# configure does not need to execute MemorySanitizer probe programs.
cc=${CC:-clang}
CC="$cc" CFLAGS='-O1 -g -fno-omit-frame-pointer' ./configure \
  --prefix="$prefix" --disable-readline --disable-static --soname=legacy
msan_cflags='-O1 -g -fno-omit-frame-pointer -fsanitize=memory'
make -j2 libsqlite3.so CFLAGS="$msan_cflags" LDFLAGS='-fsanitize=memory'
make install-dll install-headers install-pc \
  CFLAGS="$msan_cflags" LDFLAGS='-fsanitize=memory'

# Select the instrumented library only for programs linked through pkg-config.
# The system sqlite3 CLI must keep using its uninstrumented library so that
# test feature detection can still run sqlite3 --version.
sed -i '/^Libs:/s|$| -Wl,-rpath,${libdir}|' "$prefix/lib/pkgconfig/sqlite3.pc"

# Verify the installed library is instrumented.
nm -D "$prefix/lib/libsqlite3.so" > "$build_dir/sqlite-symbols.txt"
grep -q '__msan_' "$build_dir/sqlite-symbols.txt"
