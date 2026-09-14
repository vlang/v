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

if [ "$#" -ne 2 ]; then
	echo "usage: $0 <bootstrap-v> <v1-fallback-output>" >&2
	exit 2
fi

bootstrap_v=$1
fallback_output=$2
system=$(uname -s 2>/dev/null || echo unknown)
source_root=$(cd "$(dirname "$0")/../.." && pwd) || exit 1
cache_parent=${V1_FALLBACK_CACHE_DIR:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/v/v1-fallback}
oldv_workdir=$cache_parent/sources/$fallback_short_revision

fallback_dir=$(dirname "$fallback_output")
mkdir -p "$fallback_dir" || exit 1
fallback_dir=$(cd "$fallback_dir" && pwd) || exit 1
fallback_output=$fallback_dir/$(basename "$fallback_output")

work_dir=$(mktemp -d "${TMPDIR:-/tmp}/v1-fallback.XXXXXX") || exit 1
candidate=$work_dir/v1_fallback
candidate_root_file=$work_dir/v1_fallback.vroot
trap 'rm -rf "$work_dir"' EXIT HUP INT TERM

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
	oldv_copy='cp ./v "$V1_FALLBACK_TARGET" && pwd > "$V1_FALLBACK_ROOT_TARGET"'
	case "$system" in
		MSYS*|MINGW*)
			oldv_copy='copy /Y .\v.exe "%V1_FALLBACK_TARGET%" >NUL && cd > "%V1_FALLBACK_ROOT_TARGET%"'
			if command -v cygpath >/dev/null 2>&1; then
				oldv_target=$(cygpath -w "$candidate")
			fi
		;;
	esac
	mkdir -p "$oldv_workdir" || return 1
	set -- "$bootstrap_v" -no-parallel -gc none run cmd/tools/oldv.v \
		--cache=false --workdir "$oldv_workdir" --command "$oldv_copy"
	if local_v_repo=$(local_git_repo "$source_root" "$fallback_revision"); then
		set -- "$@" --vrepo "$local_v_repo"
	fi
	if local_vc_repo=$(local_git_repo "$source_root/vc" "$fallback_vc_revision"); then
		set -- "$@" --vcrepo "$local_vc_repo"
	fi
	set -- "$@" "$fallback_revision"
	OLDV_VFLAGS='-d v1_fallback' V1_FALLBACK_TARGET=$oldv_target \
		V1_FALLBACK_ROOT_TARGET=$candidate_root_file \
		"$@" || return 1
	candidate_root=$(sed -n '1p' "$candidate_root_file") || return 1
	[ -n "$candidate_root" ] || return 1
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
