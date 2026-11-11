#!/bin/sh
# Install a compiler optimized with -prod without changing the flags of its tests.
set -eu

platform() {
  arch=$(uname -m)
  host_os=$(uname -s)
  termux_prefix=
  case "${PREFIX:-}" in *com.termux*) termux_prefix=1 ;; esac
  if [ "$host_os" = Linux ] && [ -n "${TERMUX_VERSION:-}$termux_prefix" ]; then
    # Termux uses Android's bionic libc, so it cannot reuse glibc binaries.
    # A 64-bit kernel can host 32-bit Termux; require a known 64-bit userland.
    [ "$(getconf LONG_BIT 2>/dev/null)" = 64 ] || return 0
    case "$arch" in
      aarch64) echo termux-arm64 ;;
      x86_64) echo termux-x64 ;;
    esac
    return
  fi
  case "$host_os:$arch" in
    Linux:x86_64)
      # The shared Linux compiler is built on Ubuntu 22.04. musl, non-Termux
      # Android and older glibc installations must build their own compiler.
      if getconf GNU_LIBC_VERSION 2>/dev/null | awk '
        { split($2, v, "."); if ($1 == "glibc" && (v[1] > 2 || (v[1] == 2 && v[2] >= 35))) ok = 1 }
        END { exit !ok }'; then
        echo linux-x64
      fi
      ;;
    Darwin:arm64|Darwin:x86_64)
      major=$(sw_vers -productVersion | cut -d . -f 1)
      if [ "$major" -ge 15 ]; then
        case "$arch" in arm64) echo macos-arm64 ;; *) echo macos-x64 ;; esac
      fi
      ;;
    MINGW*:x86_64|MSYS*:x86_64) echo windows-x64 ;;
  esac
}

if [ "${1:-}" = --platform ]; then
  platform
  exit 0
fi
case "${V_CI_SOURCE_BUILD:-0}" in
  0|1) ;;
  *) echo 'V_CI_SOURCE_BUILD must be 0 or 1' >&2; exit 1 ;;
esac

sha=$(git rev-parse 'HEAD^{commit}')
host_platform=$(platform)
prebuilt=${V_CI_PREBUILT:-}

# Only reuse the verified artifact supplied by this workflow run. Standalone
# jobs and other commits build an optimized compiler from their own checkout.
if [ "${V_CI_SOURCE_BUILD:-0}" = 1 ]; then
  prebuilt=
fi

if [ "$#" -eq 0 ]; then
  set -- -j4
fi

if [ -n "$prebuilt" ]; then
  asset="$sha-$host_platform"
  if [ -z "$host_platform" ] || [ "$(basename "$prebuilt")" != "$asset" ]; then
    echo 'Prebuilt compiler does not match this checkout and platform' >&2
    exit 1
  fi
  # Check the digest directly, rather than trusting paths in a checksum file.
  expected=$(awk 'NR == 1 {print $1}' "$prebuilt.sha256")
  case "$expected" in ''|*[!0-9a-f]*) echo 'Invalid compiler checksum' >&2; exit 1 ;; esac
  if [ "${#expected}" -ne 64 ]; then
    echo 'Invalid compiler checksum length' >&2
    exit 1
  fi
  if command -v sha256sum >/dev/null 2>&1; then
    actual=$(sha256sum "$prebuilt" | cut -d ' ' -f 1)
  else
    actual=$(shasum -a 256 "$prebuilt" | cut -d ' ' -f 1)
  fi
  if [ "$actual" != "$expected" ]; then
    echo 'Prebuilt compiler checksum mismatch' >&2
    exit 1
  fi
  # A binary alone is insufficient: the tests also need the platform's TCC,
  # GC library, bootstrap sources and (on older macOS) legacy support files.
  make "$@" latest_vc latest_tcc latest_legacy
  cp "$prebuilt" ./vprod
  chmod 755 ./vprod
  echo "Using optimized compiler $asset"
else
  if [ ! -x ./v ]; then
    make "$@"
  fi
  compiler=${V_CI_CC:-}
  if [ -z "$compiler" ]; then
    if command -v clang >/dev/null 2>&1; then
      compiler=clang
    elif command -v gcc >/dev/null 2>&1; then
      compiler=gcc
    else
      compiler=cc
    fi
  fi
  case "$compiler" in *tcc*) echo 'A production compiler needs an optimizing C compiler' >&2; exit 1 ;; esac
  # Export the C project before native optimization to keep peak memory down.
  # Clear inherited test/backend flags and recursive make settings only here.
  (
    unset VFLAGS VEXE VPROD MAKEFLAGS MFLAGS
    sh cmd/tools/build_vprod.sh ./v ./vprod "$compiler"
  )
fi

(
  unset VFLAGS VEXE
  ./vprod -version
)
mv -f ./vprod ./v
./v -new-compiler -gc none run cmd/tools/detect_tcc.v
./v -new-compiler -gc none run .github/problem-matchers/register_all.vsh
make "$@" cache-basic-modules
