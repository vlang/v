#!/bin/sh

# Install the V 0.5.2 release compiler used by cmd/v as its V1 fallback.
# If GitHub does not provide a usable binary, preserve hermetic `make local=1`
# builds by compiling from this checkout before trying the existing oldv tool.

set -u

release_version=0.5.2
release_base_url=https://github.com/vlang/v/releases/download/$release_version

if [ "$#" -ne 2 ]; then
	echo "usage: $0 <bootstrap-v> <v1-fallback-output>" >&2
	exit 2
fi

bootstrap_v=$1
fallback_output=$2
system=$(uname -s 2>/dev/null || echo unknown)
architecture=$(uname -m 2>/dev/null || echo unknown)

fallback_dir=$(dirname "$fallback_output")
mkdir -p "$fallback_dir" || exit 1
fallback_dir=$(cd "$fallback_dir" && pwd) || exit 1
fallback_output=$fallback_dir/$(basename "$fallback_output")

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/v1-fallback.XXXXXX") || exit 1
archive=$work_dir/release.zip
candidate=$work_dir/v1_fallback
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

asset=
member=v/v
expected_sha256=
case "$system:$architecture" in
	Linux:x86_64|Linux:amd64)
		asset=v_linux.zip
		expected_sha256=86caf9e70c3342d48ef19eb4f6c47b709f18c90ae86255520d5c29df6b482e23
		;;
	Linux:arm64|Linux:aarch64)
		asset=v_linux_arm64.zip
		expected_sha256=7e102f0ecc722bc59fea83ab1c99ae49c2f7be8f30abee9443220e452a439ed3
		;;
	Darwin:arm64|Darwin:aarch64)
		asset=v_macos_arm64.zip
		expected_sha256=e539a8dc3aeea47267f3cf00c25c4f0a364d8037fb13f5379d2a574a7abac8ee
		;;
	Darwin:x86_64|Darwin:amd64)
		asset=v_macos_x86_64.zip
		expected_sha256=de19ef02874aec502f091b75e504e4836da38f627ddf7f7f9ecf6e8cf262f9d0
		;;
	MSYS*:x86_64|MSYS*:amd64|MINGW*:x86_64|MINGW*:amd64)
		asset=v_windows.zip
		member=v/v.exe
		expected_sha256=5f1d619b6b04a2b54b4ad21826a25bdcba2acf75a941c8f72fc95672b6b064ca
		;;
esac

sha256_of() {
	if command -v sha256sum >/dev/null 2>&1; then
		sha256sum "$1" | awk '{print $1}'
	elif command -v shasum >/dev/null 2>&1; then
		shasum -a 256 "$1" | awk '{print $1}'
	elif command -v sha256 >/dev/null 2>&1; then
		sha256 -q "$1"
	elif command -v openssl >/dev/null 2>&1; then
		openssl dgst -sha256 "$1" | awk '{print $NF}'
	else
		return 1
	fi
}

candidate_has_expected_version() {
	candidate_version=$("$candidate" version 2>/dev/null) || return 1
	case "$candidate_version" in
		"V $release_version "*) return 0 ;;
		*) return 1 ;;
	esac
}

download_release() {
	[ -n "$asset" ] || return 1
	url=${V1_FALLBACK_RELEASE_URL:-$release_base_url/$asset}
	if command -v curl >/dev/null 2>&1; then
		curl -fL --retry 2 --connect-timeout 15 -o "$archive" "$url" || return 1
	elif command -v wget >/dev/null 2>&1; then
		wget -O "$archive" "$url" || return 1
	else
		return 1
	fi
	actual_sha256=$(sha256_of "$archive") || return 1
	if [ "$actual_sha256" != "${V1_FALLBACK_RELEASE_SHA256:-$expected_sha256}" ]; then
		echo "V $release_version fallback archive checksum mismatch" >&2
		return 1
	fi
	if command -v unzip >/dev/null 2>&1; then
		unzip -p "$archive" "$member" > "$candidate" || return 1
	elif command -v bsdtar >/dev/null 2>&1; then
		bsdtar -xOf "$archive" "$member" > "$candidate" || return 1
	elif command -v tar >/dev/null 2>&1; then
		tar -xOf "$archive" "$member" > "$candidate" || return 1
	else
		return 1
	fi
	chmod +x "$candidate" || return 1
	candidate_has_expected_version || {
		echo "The V $release_version release fallback cannot run on this host." >&2
		return 1
	}
}

build_with_oldv() {
	echo "Building the V $release_version fallback with oldv..."
	oldv_target=$candidate
	oldv_copy='cp ./v "$V1_FALLBACK_TARGET"'
	case "$system" in
		MSYS*|MINGW*)
			oldv_copy='copy /Y .\v.exe "%V1_FALLBACK_TARGET%" >NUL'
			if command -v cygpath >/dev/null 2>&1; then
				oldv_target=$(cygpath -w "$candidate")
			fi
			;;
	esac
	V1_FALLBACK_TARGET=$oldv_target "$bootstrap_v" -no-parallel -gc none run \
		cmd/tools/oldv.v --cache=false --command "$oldv_copy" "$release_version" || return 1
	chmod +x "$candidate" || return 1
}

build_from_local_sources() {
	echo "Building the V $release_version fallback from local sources..."
	set -- "$bootstrap_v" -no-parallel -gc none -d v1_fallback -o "$candidate"
	if [ -n "${CC:-}" ]; then
		set -- "$@" -cc "$CC"
	fi
	if [ -n "${OLDV_CCOPTIONS:-}" ]; then
		set -- "$@" -cflags "$OLDV_CCOPTIONS"
	fi
	if [ -n "${OLDV_LDFLAGS:-}" ]; then
		set -- "$@" -ldflags "$OLDV_LDFLAGS"
	fi
	set -- "$@" cmd/v
	"$@" || return 1
	chmod +x "$candidate" || return 1
	candidate_has_expected_version || return 1
}

if download_release; then
	echo "Installed the V $release_version release fallback from $asset"
else
	echo "Could not install a V $release_version release asset for $system/$architecture." >&2
	if [ -n "${V1_FALLBACK_LOCAL:-}" ]; then
		build_from_local_sources && local_build_succeeded=1
	fi
	if [ "${local_build_succeeded:-0}" -ne 1 ]; then
		build_with_oldv || {
			echo "Could not build the V $release_version fallback with oldv." >&2
			exit 1
		}
	fi
fi

candidate_has_expected_version || {
	if [ -n "${candidate_version:-}" ]; then
		echo "Expected V $release_version fallback, got: $candidate_version" >&2
	else
		echo "The staged V1 fallback is not executable." >&2
	fi
	exit 1
}

rm -f "$fallback_output"
mv "$candidate" "$fallback_output" || exit 1
