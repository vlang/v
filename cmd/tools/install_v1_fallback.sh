#!/bin/sh

# Install the last V 0.5.2 compiler snapshot that still contains V1.
# oldv checks out the pinned V sources and their matching vc snapshot, then
# builds the fallback there. Its vlib is kept beside the cached compiler so
# moved modules, such as json2, resolve at their current locations.

set -u

release_version=0.5.2
# This is the parent of the commit that removed V1. Unlike the 0.5.2 release
# archive, this snapshot still has the top-level vlib/json2 module.
fallback_revision=0613e1f6fc68573f5b679e406ce58a00d6ebeb30
fallback_short_revision=0613e1f
fallback_vc_revision=e658629fc4bd59826bd7637cde498d0c6236b14b
fallback_vc_short_revision=e658629

if [ "$#" -ne 2 ]; then
	echo "usage: $0 <bootstrap-v> <v1-fallback-output>" >&2
	exit 2
fi

bootstrap_v=$1
fallback_output=$2
selected_cc=${CC:-cc}
system=$(uname -s 2>/dev/null || echo unknown)
source_root=$(cd "$(dirname "$0")/../.." && pwd) || exit 1
cache_parent=${V1_FALLBACK_CACHE_DIR:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/v/v1-fallback}
oldv_workdir=$cache_parent/sources/${fallback_short_revision}_${fallback_vc_short_revision}
oldv_source_dir=$oldv_workdir/v_at_${fallback_revision}_vc_${fallback_vc_revision}
lock_dir=$oldv_workdir.lock
lock_owner_file=$lock_dir/owner
lock_timeout_seconds=600

fallback_dir=$(dirname "$fallback_output")
mkdir -p "$fallback_dir" || exit 1
fallback_dir=$(cd "$fallback_dir" && pwd) || exit 1
fallback_output=$fallback_dir/$(basename "$fallback_output")

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/v1-fallback.XXXXXX") || exit 1
candidate=$work_dir/v1_fallback
candidate_root_file=$work_dir/v1_fallback.vroot
lock_acquired=0

cleanup() {
	rm -rf "$work_dir"
	if [ "$lock_acquired" -eq 1 ]; then
		lock_acquired=0
		rm -f "$lock_owner_file"
		rmdir "$lock_dir" 2>/dev/null || true
	fi
}

trap cleanup EXIT
trap 'exit 1' HUP INT TERM

acquire_cache_lock() {
	mkdir -p "$cache_parent/sources" || return 1
	waiting=0
	waited=0
	while ! mkdir "$lock_dir" 2>/dev/null; do
		lock_owner_pid=$(sed -n '1p' "$lock_owner_file" 2>/dev/null || true)
		case "$lock_owner_pid" in
			''|*[!0-9]*) ;;
			*)
				if ! kill -0 "$lock_owner_pid" 2>/dev/null; then
					stale_lock=$lock_dir.reclaim.$$.$waited
					if [ ! -e "$stale_lock" ] && mv "$lock_dir" "$stale_lock" 2>/dev/null; then
						rm -rf "$stale_lock"
						continue
					fi
				fi
				;;
		esac
		if [ "$waited" -ge "$lock_timeout_seconds" ]; then
			echo "Timed out waiting for the V1 fallback cache lock at $lock_dir." >&2
			echo "If no other V1 fallback installation is running, remove that directory and retry." >&2
			return 1
		fi
		if [ "$waiting" -eq 0 ]; then
			echo "Waiting for another V1 fallback installation..."
			waiting=1
		fi
		sleep 1 || return 1
		waited=$((waited + 1))
	done
	printf '%s\n' "$$" > "$lock_owner_file" || {
		rm -f "$lock_owner_file"
		rmdir "$lock_dir" 2>/dev/null || true
		return 1
	}
	lock_acquired=1
}

acquire_cache_lock || {
	echo "Could not lock the V1 fallback cache at $lock_dir." >&2
	exit 1
}

write_candidate_root() {
	root=$1
	case "$system" in
		MSYS*|MINGW*)
			if command -v cygpath >/dev/null 2>&1; then
				root=$(cygpath -w "$root") || return 1
			else
				root=$(cd "$root" && pwd -W) || return 1
			fi
			;;
	esac
	printf '%s\n' "$root" > "$candidate_root_file"
}

local_git_repo() {
	repository=$1
	revision=$2
	promisor_config=$(git -C "$repository" config --get-regexp '^remote\..*\.promisor$' 2>/dev/null) \
		|| promisor_config=
	[ -z "$promisor_config" ] || return 1
	is_shallow=$(git -C "$repository" rev-parse --is-shallow-repository 2>/dev/null) || return 1
	[ "$is_shallow" = false ] || return 1
	git -C "$repository" show-ref --verify --quiet refs/heads/master || return 1
	git -C "$repository" cat-file -e "$revision^{commit}" 2>/dev/null || return 1
	git -C "$repository" archive --format=tar "$revision" >/dev/null 2>&1 || return 1
	common_dir=$(git -C "$repository" rev-parse --git-common-dir 2>/dev/null) || return 1
	case "$common_dir" in
		/*) printf '%s\n' "$common_dir" ;;
		*) (cd "$repository/$common_dir" && pwd) ;;
	esac
}

candidate_has_expected_version() {
	candidate_version=$("$candidate" version 2>/dev/null) || return 1
	case "$candidate_version" in
		"V $release_version $fallback_short_revision"*) return 0 ;;
		*) return 1 ;;
	esac
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
	mkdir -p "$oldv_workdir" || return 1
	set -- "$bootstrap_v" -no-parallel -gc none -cc "$selected_cc" run cmd/tools/oldv.v \
		--cache=false --workdir "$oldv_workdir" --command "$oldv_copy"
	if local_v_repo=$(local_git_repo "$source_root" "$fallback_revision"); then
		set -- "$@" --vrepo "$local_v_repo"
	fi
	if local_vc_repo=$(local_git_repo "$source_root/vc" "$fallback_vc_revision"); then
		set -- "$@" --vcrepo "$local_vc_repo"
	fi
	set -- "$@" --vccommit "$fallback_vc_revision"
	set -- "$@" "$fallback_revision"
	VFLAGS= CC=$selected_cc OLDV_VFLAGS="-d v1_fallback -cc \"$selected_cc\"" \
		V1_FALLBACK_TARGET=$oldv_target \
		"$@" || return 1
	candidate_root=$(cd "$oldv_source_dir" && pwd) || return 1
	write_candidate_root "$candidate_root" || return 1
	chmod +x "$candidate" || return 1
}

build_with_oldv || {
	echo "Could not build the V $release_version fallback with oldv." >&2
	exit 1
}

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
