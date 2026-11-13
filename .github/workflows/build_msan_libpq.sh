#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 /absolute/install/prefix" >&2
  exit 2
fi
prefix=$1
case "$prefix" in
  /*) ;;
  *) echo 'libpq install prefix must be absolute' >&2; exit 2 ;;
esac

# Match the libpq major version installed in the sanitizer runner, and verify
# the upstream source archive before unpacking it.
version=18.6
checksum=555610c24d53e4316da5b7d3fc25c279d96856d5e0e23ee308c328c5fa881d9f
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/v-msan-libpq.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
archive="postgresql-$version.tar.bz2"
curl --fail --location --output "$build_dir/$archive" \
  "https://ftp.postgresql.org/pub/source/v$version/$archive"
printf '%s  %s\n' "$checksum" "$build_dir/$archive" | sha256sum --check
tar -xjf "$build_dir/$archive" -C "$build_dir"
cd "$build_dir/postgresql-$version"

# Configure probes describe the host ABI; only the library and its bundled
# support objects need instrumentation. Running configure probes with MSan
# would make intentional uninitialized configure tests unreliable.
cc=${CC:-clang}
CC="$cc" CFLAGS='-O1 -g -fno-omit-frame-pointer' ./configure \
  --prefix="$prefix" --with-ssl=openssl --without-icu --without-readline --without-zlib
msan_cflags='-O1 -g -fno-omit-frame-pointer -fsanitize=memory'
msan_ldflags='$(LDFLAGS_INTERNAL) -fsanitize=memory'
make -j2 -C src/interfaces/libpq CFLAGS="$msan_cflags" LDFLAGS="$msan_ldflags"
make -C src/interfaces/libpq install CFLAGS="$msan_cflags" LDFLAGS="$msan_ldflags"
make -C src/include install

# Verify the installed library is instrumented.
nm -D "$prefix/lib/libpq.so" > "$build_dir/libpq-symbols.txt"
grep -q '__msan_' "$build_dir/libpq-symbols.txt"
