#!/bin/sh

# Install the V 0.5.2 release compiler used by cmd/v as its V1 fallback.
# If GitHub does not provide a usable binary, oldv checks out the 0.5.2 V
# sources and their matching vc snapshot, then builds the fallback there.

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
candidate_root_file=$work_dir/v1_fallback.vroot
release_tree=$work_dir/release
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

cache_parent=${V1_FALLBACK_CACHE_DIR:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/v/v1-fallback}
cache_root=$cache_parent/$release_version
cached_candidate=$cache_root/$(basename "$member")

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
	mkdir -p "$release_tree" || return 1
	if command -v unzip >/dev/null 2>&1; then
		unzip -q "$archive" -d "$release_tree" || return 1
	elif command -v bsdtar >/dev/null 2>&1; then
		bsdtar -xf "$archive" -C "$release_tree" || return 1
	elif command -v tar >/dev/null 2>&1; then
		tar -xf "$archive" -C "$release_tree" || return 1
	else
		return 1
	fi
	candidate=$release_tree/$member
	chmod +x "$candidate" || return 1
	candidate_has_expected_version || {
		echo "The V $release_version release fallback cannot run on this host." >&2
		return 1
	}
}

build_with_oldv() {
	echo "Building the V $release_version fallback with oldv..."
	oldv_target=$candidate
	oldv_copy='cp ./v "$V1_FALLBACK_TARGET" && pwd > "$V1_FALLBACK_ROOT_TARGET"'
	case "$system" in
		MSYS*|MINGW*)
			oldv_copy='copy /Y .\v.exe "%V1_FALLBACK_TARGET%" >NUL && cd > "%V1_FALLBACK_ROOT_TARGET%"'
			if command -v cygpath >/dev/null 2>&1; then
				oldv_target=$(cygpath -w "$candidate")
			fi
			;;
	esac
	V1_FALLBACK_TARGET=$oldv_target V1_FALLBACK_ROOT_TARGET=$candidate_root_file \
		"$bootstrap_v" -no-parallel -gc none run \
		cmd/tools/oldv.v --cache=false --command "$oldv_copy" "$release_version" || return 1
	chmod +x "$candidate" || return 1
}

use_cached_release() {
	[ -d "$cache_root/vlib" ] || return 1
	[ -x "$cached_candidate" ] || return 1
	candidate=$cached_candidate
	candidate_has_expected_version || return 1
	printf '%s\n' "$cache_root" > "$candidate_root_file" || return 1
}

install_downloaded_release() {
	mkdir -p "$cache_parent" || return 1
	staged_cache=$cache_root.tmp.$$
	rm -rf "$staged_cache"
	mv "$release_tree/v" "$staged_cache" || return 1
	if [ -e "$cache_root" ]; then
		rm -rf "$cache_root" || return 1
	fi
	mv "$staged_cache" "$cache_root" || return 1
	candidate=$cached_candidate
	printf '%s\n' "$cache_root" > "$candidate_root_file" || return 1
}

if use_cached_release; then
	echo "Using the cached V $release_version release fallback"
elif download_release; then
	echo "Installed the V $release_version release fallback from $asset"
	install_downloaded_release || exit 1
else
	echo "Could not install a V $release_version release asset for $system/$architecture." >&2
	build_with_oldv || {
		echo "Could not build the V $release_version fallback with oldv." >&2
		exit 1
	}
fi

candidate_has_expected_version || {
	if [ -n "${candidate_version:-}" ]; then
		echo "Expected V $release_version fallback, got: $candidate_version" >&2
	else
		echo "The staged V1 fallback is not executable." >&2
	fi
	exit 1
}

staged_output=$fallback_output.tmp.$$
cp "$candidate" "$staged_output" || exit 1
chmod +x "$staged_output" || exit 1
mv -f "$staged_output" "$fallback_output" || exit 1
mv -f "$candidate_root_file" "$fallback_output.vroot" || exit 1
