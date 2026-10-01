#!/bin/sh

# Install the V 0.5.2 release compiler used by cmd/v as its V1 fallback.
# If GitHub does not provide a usable binary, oldv checks out the 0.5.2 V
# sources and their matching vc snapshot, then builds the fallback there.
# The release vlib is supplemented with modules whose public paths changed
# after 0.5.2, while their source was still compatible with V1.

set -u

release_version=0.5.2
release_base_url=https://github.com/vlang/v/releases/download/$release_version
compatibility_marker=.v1-fallback-complete

if [ "$#" -ne 2 ]; then
	echo "usage: $0 <bootstrap-v> <v1-fallback-output>" >&2
	exit 2
fi

# Automatic provisioning passes paths through the environment so make and its
# recipe shell never parse valid path characters such as apostrophes or '$'.
bootstrap_v=${V1_FALLBACK_BOOTSTRAP:-$1}
fallback_output=${V1_FALLBACK_OUTPUT:-$2}
system=$(uname -s 2>/dev/null || echo unknown)
architecture=$(uname -m 2>/dev/null || echo unknown)

path_owner() {
	stat -c %u "$1" 2>/dev/null || stat -f %u "$1" 2>/dev/null
}

path_permissions() {
	stat -c %a "$1" 2>/dev/null || stat -f %Lp "$1" 2>/dev/null
}

validate_trusted_temp_root() {
	root=$1
	[ -d "$root" ] || {
		echo "Temporary directory $root is not a directory." >&2
		return 1
	}
	case "$system" in
		MSYS*|MINGW*) return 0 ;;
	esac
	owner=$(path_owner "$root") || return 1
	permissions=$(path_permissions "$root") || return 1
	case "$owner" in
		''|*[!0-9]*) return 1 ;;
	esac
	case "$permissions" in
		''|*[!0-7]*) return 1 ;;
	esac
	if [ "$owner" != 0 ] && [ "$owner" != "$effective_uid" ]; then
		echo "Temporary directory $root is not owned by the current user or root." >&2
		return 1
	fi
	permissions_value=$((0$permissions))
	if [ $((permissions_value & 0022)) -ne 0 ] \
		&& [ $((permissions_value & 01000)) -eq 0 ]; then
		echo "Temporary directory $root is writable by other users without the sticky bit." >&2
		return 1
	fi
}

validate_private_directory() {
	directory=$1
	[ -d "$directory" ] && [ ! -L "$directory" ] || {
		echo "Refusing unsafe V1 fallback path $directory: expected a real directory." >&2
		return 1
	}
	case "$system" in
		MSYS*|MINGW*) return 0 ;;
	esac
	owner=$(path_owner "$directory") || return 1
	permissions=$(path_permissions "$directory") || return 1
	case "$owner" in
		''|*[!0-9]*) return 1 ;;
	esac
	case "$permissions" in
		''|*[!0-7]*) return 1 ;;
	esac
	if [ "$owner" != "$effective_uid" ] || [ $((0$permissions)) -ne $((0700)) ]; then
		echo "Refusing unsafe V1 fallback path $directory: expected user-owned mode 0700." >&2
		return 1
	fi
}

private_temp_cache_parent() {
	root=$1
	case "$system" in
		MSYS*|MINGW*)
			candidate=$(mktemp -d "$root/v1-fallback-cache.XXXXXX") || return 1
			;;
		*)
			candidate=$root/v1-fallback-cache-$effective_uid
			(umask 077 && mkdir "$candidate") 2>/dev/null || true
			;;
	esac
	validate_private_directory "$candidate" || return 1
	printf '%s\n' "$candidate"
}

effective_uid=
case "$system" in
	MSYS*|MINGW*) ;;
	*)
		effective_uid=$(id -u 2>/dev/null) || exit 1
		case "$effective_uid" in
			''|*[!0-9]*) exit 1 ;;
		esac
		;;
esac
temp_root=${TMPDIR:-/tmp}
temp_root=$(cd "$temp_root" 2>/dev/null && pwd -P) || exit 1
validate_trusted_temp_root "$temp_root" || exit 1

fallback_dir=$(dirname "$fallback_output")
mkdir -p "$fallback_dir" || exit 1
fallback_dir=$(cd "$fallback_dir" && pwd) || exit 1
fallback_output=$fallback_dir/$(basename "$fallback_output")

work_dir=$(mktemp -d "$temp_root/v1-fallback.XXXXXX") || exit 1
validate_private_directory "$work_dir" || exit 1
archive=$work_dir/release.zip
candidate=$work_dir/v1_fallback
candidate_root_file=$work_dir/v1_fallback.vroot
release_tree=$work_dir/release

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

if [ -n "${V1_FALLBACK_CACHE_DIR:-}" ]; then
	cache_parent=$V1_FALLBACK_CACHE_DIR
elif [ -n "${XDG_CACHE_HOME:-}" ]; then
	cache_parent=$XDG_CACHE_HOME/v/v1-fallback
elif [ -n "${HOME:-}" ]; then
	cache_parent=$HOME/.cache/v/v1-fallback
else
	cache_parent=$(private_temp_cache_parent "$temp_root") || exit 1
fi
cache_root=$cache_parent/$release_version
cached_candidate=$cache_root/$(basename "$member")
cache_lock=$cache_parent/.install-$release_version.lock
cache_lock_owner=
cache_lock_probe=
cache_lock_acquired=

cleanup() {
	if [ -n "$cache_lock_acquired" ]; then
		owner_pid=$(sed -n '1p' "$cache_lock" 2>/dev/null || true)
		if [ "$owner_pid" = "$$" ]; then
			rm -f "$cache_lock"
		fi
	fi
	if [ -n "$cache_lock_probe" ]; then
		rm -f "$cache_lock_probe"
	fi
	if [ -n "$cache_lock_owner" ]; then
		rm -f "$cache_lock_owner"
	fi
	rm -rf "$work_dir"
}

trap cleanup EXIT
trap 'exit 1' HUP INT TERM

process_identity() {
	identity_pid=$1
	if [ -r "/proc/$identity_pid/stat" ]; then
		identity_stat=$(cat "/proc/$identity_pid/stat") || return 1
		identity_fields=${identity_stat##*) }
		identity_start=$(printf '%s\n' "$identity_fields" | awk '{print $20}')
		identity_boot=$(cat /proc/sys/kernel/random/boot_id 2>/dev/null || true)
		[ -n "$identity_start" ] || return 1
		printf '%s:%s\n' "$identity_boot" "$identity_start"
		return
	fi
	ps -p "$identity_pid" -o lstart= 2>/dev/null | awk '{$1=$1; print}'
}

acquire_cache_lock() {
	mkdir -p "$cache_parent" || return 1
	cache_lock_owner=$(mktemp "$cache_parent/.install-$release_version.owner.XXXXXX") || return 1
	cache_lock_probe=$cache_lock_owner.probe
	owner_identity=$(process_identity "$$")
	owner_name=$(basename "$cache_lock_owner")
	printf '%s\n%s\n%s\n' "$$" "$owner_identity" "$owner_name" > "$cache_lock_owner" || return 1
	if ! ln "$cache_lock_owner" "$cache_lock_probe" 2>/dev/null; then
		echo "Could not create the V $release_version fallback cache lock (hard links are unsupported)." >&2
		return 1
	fi
	rm -f "$cache_lock_probe"
	wait_count=0
	while :; do
		if ln "$cache_lock_owner" "$cache_lock" 2>/dev/null; then
			cache_lock_acquired=1
			return
		fi
		if [ ! -e "$cache_lock" ]; then
			if ln "$cache_lock_owner" "$cache_lock" 2>/dev/null; then
				cache_lock_acquired=1
				return
			fi
			if [ ! -e "$cache_lock" ]; then
				echo "Could not create the V $release_version fallback cache lock (hard links may be unsupported)." >&2
				return 1
			fi
		fi
		existing_pid=$(sed -n '1p' "$cache_lock" 2>/dev/null || true)
		existing_identity=$(sed -n '2p' "$cache_lock" 2>/dev/null || true)
		existing_owner=$(sed -n '3p' "$cache_lock" 2>/dev/null || true)
		case "$existing_pid" in
			''|*[!0-9]*) stale_owner=invalid ;;
			*)
				if kill -0 "$existing_pid" 2>/dev/null; then
					current_identity=$(process_identity "$existing_pid")
					if [ -n "$existing_identity" ] && [ "$current_identity" = "$existing_identity" ]; then
						sleep 1
						continue
					fi
					if [ -z "$existing_identity" ] || [ -z "$current_identity" ]; then
						wait_count=$((wait_count + 1))
						if [ "$wait_count" -ge 120 ]; then
							echo "Timed out waiting for the V $release_version fallback cache lock." >&2
							return 1
						fi
						sleep 1
						continue
					fi
				fi
				stale_owner=$existing_pid
				;;
		esac
		reclaim=$cache_lock.reclaim-$stale_owner
		if ln "$cache_lock_owner" "$reclaim" 2>/dev/null; then
			current_pid=$(sed -n '1p' "$cache_lock" 2>/dev/null || true)
			current_identity=$(sed -n '2p' "$cache_lock" 2>/dev/null || true)
			current_owner=$(sed -n '3p' "$cache_lock" 2>/dev/null || true)
			if [ "$current_pid" = "$existing_pid" ] && [ "$current_identity" = "$existing_identity" ] \
				&& [ "$current_owner" = "$existing_owner" ]; then
				rm -f "$cache_lock"
				case "$existing_owner" in
					.install-$release_version.owner.*) rm -f "$cache_parent/$existing_owner" ;;
				esac
			fi
			rm -f "$reclaim"
			continue
		fi
		reclaim_pid=$(sed -n '1p' "$reclaim" 2>/dev/null || true)
		reclaim_identity=$(sed -n '2p' "$reclaim" 2>/dev/null || true)
		case "$reclaim_pid" in
			''|*[!0-9]*) ;;
			*)
				if kill -0 "$reclaim_pid" 2>/dev/null; then
					current_identity=$(process_identity "$reclaim_pid")
					if [ -n "$reclaim_identity" ] && [ "$current_identity" = "$reclaim_identity" ]; then
						sleep 1
						continue
					fi
					if [ -z "$reclaim_identity" ] || [ -z "$current_identity" ]; then
						wait_count=$((wait_count + 1))
						if [ "$wait_count" -ge 120 ]; then
							echo "Timed out waiting for recovery of the V $release_version fallback cache lock." >&2
							return 1
						fi
						sleep 1
						continue
					fi
				fi
				;;
		esac
		rm -f "$reclaim"
		sleep 1
	done
}

write_candidate_root() {
	root=$cache_root
	case "$system" in
		MSYS*|MINGW*)
			if command -v cygpath >/dev/null 2>&1; then
				root=$(cygpath -w "$cache_root") || return 1
			else
				root=$(cd "$cache_root" && pwd -W) || return 1
			fi
			;;
	esac
	printf '%s\n' "$root" > "$candidate_root_file"
}

install_crypto_subtle_compatibility() {
	source=$1/vlib/crypto/internal/subtle
	target=$1/vlib/crypto/subtle
	[ -f "$source/aliasing.v" ] || return 1
	[ -f "$source/comparison.v" ] || return 1
	mkdir -p "$target" || return 1
	cp "$source/aliasing.v" "$source/comparison.v" "$target/" || return 1
}

install_moved_module_compatibility() {
	source=$1/vlib/x/json2
	target=$1/vlib/json2
	staged=$target.tmp.$$
	marker=$target/$compatibility_marker
	[ -f "$source/json2.v" ] || return 1
	rm -f "$marker" || return 1
	rm -rf "$staged" || return 1
	cp -R "$source" "$staged" || {
		rm -rf "$staged"
		return 1
	}
	rm -rf "$target" || {
		rm -rf "$staged"
		return 1
	}
	mv "$staged" "$target" || {
		rm -rf "$staged"
		return 1
	}
	printf '%s\n' "$release_version" > "$marker" || return 1
}

install_fallback_compatibility() {
	install_crypto_subtle_compatibility "$1" || return 1
	install_moved_module_compatibility "$1" || return 1
}

fallback_compatibility_is_installed() {
	root=$1
	[ -f "$root/vlib/crypto/subtle/aliasing.v" ] || return 1
	[ -f "$root/vlib/crypto/subtle/comparison.v" ] || return 1
	[ -f "$root/vlib/json2/json2.v" ] || return 1
	[ "$(cat "$root/vlib/json2/$compatibility_marker" 2>/dev/null)" = "$release_version" ] || return 1
}

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
	oldv_copy='mkdir -p ./vlib/crypto/subtle && cp ./vlib/crypto/internal/subtle/aliasing.v ./vlib/crypto/internal/subtle/comparison.v ./vlib/crypto/subtle/ && rm -rf ./vlib/json2 && cp -R ./vlib/x/json2 ./vlib/json2 && echo 0.5.2 > ./vlib/json2/.v1-fallback-complete && cp ./v "$V1_FALLBACK_TARGET" && pwd > "$V1_FALLBACK_ROOT_TARGET"'
	case "$system" in
		MSYS*|MINGW*)
			oldv_copy='if not exist .\vlib\crypto\subtle mkdir .\vlib\crypto\subtle && copy /Y .\vlib\crypto\internal\subtle\aliasing.v .\vlib\crypto\subtle\ >NUL && copy /Y .\vlib\crypto\internal\subtle\comparison.v .\vlib\crypto\subtle\ >NUL && xcopy /E /I /Y .\vlib\x\json2 .\vlib\json2 >NUL && echo 0.5.2> .\vlib\json2\.v1-fallback-complete && copy /Y .\v.exe "%V1_FALLBACK_TARGET%" >NUL && cd > "%V1_FALLBACK_ROOT_TARGET%"'
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
	fallback_compatibility_is_installed "$cache_root" ||
		install_fallback_compatibility "$cache_root" || return 1
	write_candidate_root || return 1
}

install_downloaded_release() {
	mkdir -p "$cache_parent" || return 1
	staged_cache=$cache_root.tmp.$$
	rm -rf "$staged_cache"
	mv "$release_tree/v" "$staged_cache" || return 1
	install_fallback_compatibility "$staged_cache" || return 1
	if [ -e "$cache_root" ]; then
		rm -rf "$cache_root" || return 1
	fi
	mv "$staged_cache" "$cache_root" || return 1
	candidate=$cached_candidate
	write_candidate_root || return 1
}

acquire_cache_lock || exit 1

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
