#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 /absolute/install/prefix" >&2
  exit 2
fi
prefix=$1
case "$prefix" in
  /*) ;;
  *) echo 'FreeType install prefix must be absolute' >&2; exit 2 ;;
esac

# Match Ubuntu 24.04's FreeType version. Instrument its internal allocations as
# well as the V caller so that MSan can follow font parsing and glyph output.
version=2.13.2
checksum=12991c4e55c506dd7f9b765933e62fd2be2e06d421505d7950a132e4f1bb484d
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/v-msan-freetype.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
archive="freetype-$version.tar.xz"
# Retry transport failures such as curl's empty-reply error (52), as well as
# HTTP failures. Bound each attempt and the overall retry window.
curl --fail --location --retry 3 --retry-all-errors --retry-delay 1 \
  --connect-timeout 15 --max-time 60 --retry-max-time 180 \
  --output "$build_dir/$archive" \
  "https://download.savannah.gnu.org/releases/freetype/$archive"
printf '%s  %s\n' "$checksum" "$build_dir/$archive" | sha256sum --check
tar -xJf "$build_dir/$archive" -C "$build_dir"
cd "$build_dir/freetype-$version"

# Run configure probes without MSan, and keep optional uninstrumented libraries
# out of this private build. The canvas fixtures use uncompressed TrueType data.
cc=${CC:-clang}
CC="$cc" CFLAGS='-O1 -g -fno-omit-frame-pointer' ./configure \
  --prefix="$prefix" --disable-static --with-zlib=no --with-bzip2=no \
  --with-png=no --with-harfbuzz=no --with-brotli=no
# FreeType's generated CFLAGS contain required configuration defines and -c;
# select instrumentation through CC to preserve those flags.
make -j2 CC="$cc -fsanitize=memory" LDFLAGS='-fsanitize=memory'
make install CC="$cc -fsanitize=memory" LDFLAGS='-fsanitize=memory'

sed -i '/^Libs:/s|$| -Wl,-rpath,${libdir}|' "$prefix/lib/pkgconfig/freetype2.pc"
nm -D "$prefix/lib/libfreetype.so" > "$build_dir/freetype-symbols.txt"
grep -q '__msan_' "$build_dir/freetype-symbols.txt"
