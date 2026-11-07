#!/usr/bin/env bash

set -euo pipefail

mode="${1:-}"
tcc_dir="${2:-}"
tcc_repo="${3:-}"
tcc_arch="${4:-}"
vroot="${5:-}"
tmp_root="${TMPDIR:-/tmp}"
expected_branch="thirdparty-linux-${tcc_arch}"
unknown_branch='thirdparty-unknown-unknown'
marker_name='vlang-compatible-tcc'
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

# shellcheck source=git_argv.sh
source "$script_dir/git_argv.sh"
parse_git_argv

tcc_is_required() {
	case " ${VFLAGS:-} " in
		*" -cc tcc "* | *" -cc=tcc "*) return 0 ;;
	esac
	return 1
}

case "$mode" in
	fresh | latest) ;;
	*)
			echo "usage: $0 <fresh|latest> <tcc-dir> <tcc-repo> <arch> <vroot>" >&2
		exit 2
		;;
esac

case "$tcc_dir" in
	'' | / | . | ..)
		echo "refusing unsafe TCC directory: '$tcc_dir'" >&2
		exit 2
		;;
esac

if [ -z "$tcc_repo" ] || [ -z "$tcc_arch" ] || [ -z "$vroot" ]; then
	echo 'the TCC repository, architecture, and V root must be non-empty' >&2
	exit 2
fi

if [ ! -d "$vroot" ] || [ ! -d "$(dirname "$tcc_dir")" ]; then
	echo 'the V root and TCC parent directory must already exist' >&2
	exit 2
fi
vroot="$(cd "$vroot" && pwd -P)"
tcc_dir="$(cd "$(dirname "$tcc_dir")" && pwd -P)/$(basename "$tcc_dir")"
if [ "$tcc_dir" != "$vroot/thirdparty/tcc" ]; then
	echo "the Linux TCC bundle must be located at ${vroot}/thirdparty/tcc, got ${tcc_dir}" >&2
	exit 2
fi

git_available=1
if ! git_executable_is_available; then
	git_available=0
	if [ "$mode" = 'fresh' ]; then
		echo "the Git executable was not found: ${git_cmd[0]}" >&2
		exit 2
	fi
	if ! tcc_is_required; then
		echo "the Git executable was not found: ${git_cmd[0]}; skipping the Linux TCC refresh." >&2
		exit 0
	fi
fi

if [ "$git_available" != 1 ] && [ ! -d "$tmp_root" ]; then
	echo "the Git executable was not found: ${git_cmd[0]}" >&2
	echo "the temporary directory required to validate the existing TCC bundle is unavailable: ${tmp_root}" >&2
	exit 1
fi

probe_dir="$(mktemp -d "${tmp_root%/}/v-tcc-host-probe.XXXXXX")"
trap 'rm -rf "$probe_dir"' EXIT HUP INT TERM
probe_source="$probe_dir/probe.c"
probe_exe="$probe_dir/probe"
probe_log="$probe_dir/probe.log"
search_dirs_log="$probe_dir/search-dirs.log"

# Include Boehm's header the way V's own C does with TinyCC on Linux (see
# vlib/builtin/gc_boehm_helpers.c). The compatibility <gc.h> also defines a weak
# GC_noop1_ptr, which TinyCC does not keep weak after the plain prototype in
# <gc/gc.h>: since TinyCC e23dabed, linking it against a libgc.a that defines the
# function fails with "link symbol 'GC_noop1_ptr' defined twice".
cat > "$probe_source" <<'EOF'
#include <gc/gc.h>
#include <stdio.h>
#include <string.h>

int main(void) {
	GC_INIT();
	char *value = GC_MALLOC(32);
	strcpy(value, "v-tcc-host-boehm-probe");
	puts(value);
	return 0;
}
EOF

# Mirror the TinyCC defines that V adds for Linux UAPI headers (see
# v3_tcc_linux_uapi_flags in vlib/v/driver/driver.v): arm64 kernel headers use
# `__u128` in <asm/sigcontext.h>, which <signal.h> includes through <gc/gc.h>.
probe_defines=()
case "$tcc_arch" in
	aarch64 | arm64) probe_defines+=('-D__u128=__uint128_t') ;;
esac

probe_bundle() {
	: > "$probe_log"
	: > "$search_dirs_log"
	if [ ! -f "$tcc_dir/lib/libgc.a" ] || [ ! -f "$vroot/thirdparty/libgc/include/gc.h" ]; then
		echo 'the TCC bundle libgc.a or V libgc headers are missing' >> "$probe_log"
		return 1
	fi
	if ! (cd "$vroot" && "$tcc_dir/tcc.exe" --version) >> "$probe_log" 2>&1; then
		return 1
	fi
	if ! (
		unset C_INCLUDE_PATH CPATH
		cd "$vroot"
		"$tcc_dir/tcc.exe" -print-search-dirs
	) > "$search_dirs_log" 2>&1; then
		echo 'the TCC bundle could not report its search directories' >> "$probe_log"
		cat "$search_dirs_log" >> "$probe_log"
		return 1
	fi
	{
		echo 'TCC search directories:'
		cat "$search_dirs_log"
	} >> "$probe_log"
	rm -f "$probe_exe"
	if ! (
		unset C_INCLUDE_PATH CPATH
		cd "$vroot"
		"$tcc_dir/tcc.exe" \
			-I"$vroot/thirdparty/libgc/include" \
			-DGC_THREADS=1 -DTHREAD_LOCAL_ALLOC=1 -DGC_BUILTIN_ATOMIC=1 \
			${probe_defines[@]+"${probe_defines[@]}"} \
			-o "$probe_exe" "$probe_source" "$tcc_dir/lib/libgc.a" -ldl -lpthread
	) \
		>> "$probe_log" 2>&1; then
		return 1
	fi
	if ! "$probe_exe" >> "$probe_log" 2>&1; then
		return 1
	fi
	tail -n 1 "$probe_log" | grep -Fqx 'v-tcc-host-boehm-probe'
}

remote_has_branch() {
	run_git ls-remote --exit-code --heads "$tcc_repo" "refs/heads/$1" >/dev/null 2>&1
}

# origin_branch_sha prints the commit that branch $1 points to on the origin of the
# existing TCC checkout, which is the repository latest_bundle fetches from; that can
# be a mirror chosen when the checkout was created. It fails when the branch is
# missing or the repository cannot be reached.
origin_branch_sha() {
	local refs
	refs="$(run_git -C "$tcc_dir" ls-remote --exit-code --heads origin "refs/heads/$1" 2>/dev/null)" \
		|| return 1
	refs="${refs%%[[:space:]]*}"
	[ -n "$refs" ] || return 1
	printf '%s\n' "$refs"
}

clone_branch() {
	local branch="$1"
	rm -rf "$tcc_dir"
	run_git clone --filter=blob:none --quiet --branch "$branch" "$tcc_repo" "$tcc_dir"
}

use_system_compiler_or_fail() {
	local failed_branch="$1"
	local failed_sha="$2"
	if ! remote_has_branch "$unknown_branch"; then
		echo "No host-compatible TCC commit was found on ${failed_branch}, and ${unknown_branch} is unavailable for the explicit system-compiler fallback." >&2
		return 1
	fi
	if tcc_is_required; then
		echo "No host-compatible TCC commit was found on ${failed_branch}; explicit '-cc tcc' cannot continue." >&2
		clone_branch "$unknown_branch"
		write_system_marker "$failed_sha"
		return 1
	fi
	echo "No host-compatible TCC commit was found on ${failed_branch}; using the system compiler."
	clone_branch "$unknown_branch"
	write_system_marker "$failed_sha"
}

write_marker() {
	local remote_head_sha="$1"
	local compatible_sha="$2"
	local marker_dir="$tcc_dir/.git/$marker_name"
	rm -rf "$marker_dir"
	mkdir -p "$marker_dir"
	{
		printf '%s\n' 'tccos=linux'
		printf '%s\n' "tccarch=$tcc_arch"
		printf '%s\n' 'abi=glibc'
		printf '%s\n' "branch=$expected_branch"
		printf '%s\n' "remote_head_sha=$remote_head_sha"
		printf '%s\n' "compatible_sha=$compatible_sha"
	} > "$marker_dir/metadata"
}

write_system_marker() {
	local remote_head_sha="$1"
	local marker_dir="$tcc_dir/.git/$marker_name"
	rm -rf "$marker_dir"
	mkdir -p "$marker_dir"
	{
		printf '%s\n' 'tccos=linux'
		printf '%s\n' "tccarch=$tcc_arch"
		printf '%s\n' 'abi=glibc'
		printf '%s\n' "branch=$expected_branch"
		printf '%s\n' "remote_head_sha=$remote_head_sha"
		printf '%s\n' 'mode=system'
	} > "$marker_dir/metadata"
}

find_compatible_ancestor() {
	local start_sha="$1"
	local skip_sha="$2"
	local candidate_sha
	for candidate_sha in $(run_git -C "$tcc_dir" rev-list "$start_sha"); do
		if [ "$candidate_sha" = "$skip_sha" ]; then
			continue
		fi
		run_git -C "$tcc_dir" checkout --detach --quiet "$candidate_sha"
		if probe_bundle; then
			printf '%s\n' "$candidate_sha"
			return 0
		fi
	done
	return 1
}

select_compatible_history() {
	local remote_head_sha="$1"
	local compatible_sha
	echo "TCC ${expected_branch}@${remote_head_sha} is not host-compatible:" >&2
	sed 's/^/  /' "$probe_log" >&2
	if compatible_sha="$(find_compatible_ancestor "$remote_head_sha" "$remote_head_sha")"; then
		run_git -C "$tcc_dir" checkout --detach --quiet "$compatible_sha"
		write_marker "$remote_head_sha" "$compatible_sha"
		echo "Using newest host-compatible TCC commit ${compatible_sha} from ${expected_branch}."
		return 0
	fi
	rm -rf "$tcc_dir/.git/$marker_name"
	use_system_compiler_or_fail "$expected_branch" "$remote_head_sha"
}

assert_clean_checkout() {
	local status
	status="$(run_git -C "$tcc_dir" status --porcelain --untracked-files=all)"
	if [ -n "$status" ]; then
		echo 'Refusing to refresh a dirty TCC checkout:' >&2
		printf '%s\n' "$status" >&2
		return 1
	fi
}

validate_detached_marker() {
	local metadata="$tcc_dir/.git/$marker_name/metadata"
	local current_sha
	local remote_head_sha
	[ -f "$metadata" ] || return 1
	current_sha="$(run_git -C "$tcc_dir" rev-parse HEAD)"
	remote_head_sha="$(sed -n 's/^remote_head_sha=//p' "$metadata" | head -n 1)"
	[ -n "$remote_head_sha" ] \
		&& run_git -C "$tcc_dir" cat-file -e "${remote_head_sha}^{commit}" \
		&& run_git -C "$tcc_dir" merge-base --is-ancestor "$current_sha" "$remote_head_sha" \
		&& grep -Fqx 'tccos=linux' "$metadata" \
		&& grep -Fqx "tccarch=$tcc_arch" "$metadata" \
		&& grep -Fqx 'abi=glibc' "$metadata" \
		&& grep -Fqx "branch=$expected_branch" "$metadata" \
		&& grep -Fqx "compatible_sha=$current_sha" "$metadata"
}

validate_system_marker() {
	local metadata="$tcc_dir/.git/$marker_name/metadata"
	local current_sha
	local remote_head_sha
	local system_remote_sha
	[ -f "$metadata" ] || return 1
	current_sha="$(run_git -C "$tcc_dir" rev-parse HEAD)"
	remote_head_sha="$(sed -n 's/^remote_head_sha=//p' "$metadata" | head -n 1)"
	system_remote_sha="$(run_git -C "$tcc_dir" rev-parse "refs/remotes/origin/${unknown_branch}" 2>/dev/null)" \
		|| return 1
	[ -n "$remote_head_sha" ] \
		&& run_git -C "$tcc_dir" merge-base --is-ancestor "$current_sha" "$system_remote_sha" \
		&& grep -Fqx 'tccos=linux' "$metadata" \
		&& grep -Fqx "tccarch=$tcc_arch" "$metadata" \
		&& grep -Fqx 'abi=glibc' "$metadata" \
		&& grep -Fqx "branch=$expected_branch" "$metadata" \
		&& grep -Fqx 'mode=system' "$metadata"
}

# keep_unchanged_bundle succeeds, without fetching or checking out anything, when
# the selection made for the upstream head $3 still holds. Rewriting unchanged
# files would also invalidate the cached objects that use them as native inputs.
keep_unchanged_bundle() {
	local current_branch="$1"
	local current_sha="$2"
	local remote_head_sha="$3"
	local metadata="$tcc_dir/.git/$marker_name/metadata"
	if [ "$current_branch" = "$expected_branch" ]; then
		if [ "$current_sha" != "$remote_head_sha" ] || ! probe_bundle; then
			return 1
		fi
		echo "TCC ${expected_branch}@${current_sha} is already up to date."
		return 0
	fi
	grep -Fqx "remote_head_sha=$remote_head_sha" "$metadata" 2> /dev/null || return 1
	if [ -z "$current_branch" ]; then
		# The host can change independently of the recorded revision, so a pinned
		# bundle is checked again before it is kept.
		probe_bundle || return 1
		echo "Using newest host-compatible TCC commit ${current_sha} from ${expected_branch}."
		return 0
	fi
	# The system-compiler fallback: every commit of the unchanged branch already
	# failed the host check, and checking them again downloads each bundle.
	if tcc_is_required; then
		return 1
	fi
	echo "No host-compatible TCC commit was found on ${expected_branch}@${remote_head_sha}; continuing with the system compiler. Run 'make fresh_tcc' to check the branch again."
}

fresh_bundle() {
	if ! remote_has_branch "$expected_branch"; then
		echo "Pre-built TCC not available for ${expected_branch} at ${tcc_repo}." >&2
		use_system_compiler_or_fail "$expected_branch" 'unavailable'
		return
	fi
	clone_branch "$expected_branch"
	local head_sha
	head_sha="$(run_git -C "$tcc_dir" rev-parse HEAD)"
	if probe_bundle; then
		rm -rf "$tcc_dir/.git/$marker_name"
		return
	fi
	select_compatible_history "$head_sha"
}

latest_bundle() {
	if [ ! -d "$tcc_dir/.git" ]; then
		fresh_bundle
		return
	fi
	assert_clean_checkout
	local current_branch
	local branch_status
	local current_sha
	if current_branch="$(run_git -C "$tcc_dir" symbolic-ref --quiet --short HEAD)"; then
		:
	else
		branch_status=$?
		if [ "$branch_status" -ne 1 ]; then
			return "$branch_status"
		fi
		current_branch=''
	fi
	current_sha="$(run_git -C "$tcc_dir" rev-parse HEAD)"
	if [ -n "$current_branch" ]; then
		if [ "$current_branch" = "$unknown_branch" ]; then
			if ! validate_system_marker; then
				echo "Refusing to refresh the system-compiler TCC fallback without an exact ${expected_branch}/${tcc_arch} marker or while it contains local commits; run 'make fresh_tcc' after preserving them." >&2
				return 1
			fi
		elif [ "$current_branch" != "$expected_branch" ]; then
			echo "Refusing to refresh TCC branch ${current_branch} as ${expected_branch}; run 'make fresh_tcc'." >&2
			return 1
		fi
	else
		if ! validate_detached_marker; then
			echo "Refusing to refresh detached TCC without an exact ${expected_branch}/${tcc_arch} compatibility marker; run 'make fresh_tcc'." >&2
			return 1
		fi
	fi

	local advertised_sha
	if ! advertised_sha="$(origin_branch_sha "$expected_branch")"; then
		if [ "$current_branch" = "$unknown_branch" ] && remote_has_branch "$unknown_branch"; then
			if tcc_is_required; then
				echo "Pre-built TCC is still unavailable for ${expected_branch}; explicit '-cc tcc' cannot continue." >&2
				return 1
			fi
			write_system_marker 'unavailable'
			echo "Pre-built TCC is still unavailable for ${expected_branch}; continuing with the system compiler."
			return 0
		fi
		echo "Unable to refresh ${expected_branch} from the origin of ${tcc_dir}; preserving the current TCC checkout." >&2
		return 1
	fi
	if keep_unchanged_bundle "$current_branch" "$current_sha" "$advertised_sha"; then
		return 0
	fi

	run_git -C "$tcc_dir" fetch --quiet origin "refs/heads/${expected_branch}:refs/remotes/origin/${expected_branch}"
	local remote_head_sha
	remote_head_sha="$(run_git -C "$tcc_dir" rev-parse "refs/remotes/origin/${expected_branch}")"
	if [ "$current_branch" = "$expected_branch" ] \
		&& ! run_git -C "$tcc_dir" merge-base --is-ancestor "$current_sha" "$remote_head_sha"; then
		echo "Refusing to overwrite local TCC commits on ${expected_branch}; run 'make fresh_tcc' after preserving them." >&2
		return 1
	fi
	run_git -C "$tcc_dir" checkout --detach --quiet "$remote_head_sha"
	if probe_bundle; then
		run_git -C "$tcc_dir" checkout --quiet -B "$expected_branch" "refs/remotes/origin/${expected_branch}"
		rm -rf "$tcc_dir/.git/$marker_name"
		return
	fi
	select_compatible_history "$remote_head_sha"
}

if [ "$git_available" != 1 ]; then
	if probe_bundle; then
		echo "the Git executable was not found: ${git_cmd[0]}; preserving the existing host-compatible TCC bundle." >&2
		exit 0
	fi
	echo "the Git executable was not found: ${git_cmd[0]}, and the existing TCC bundle failed its host-compatibility probe; explicit '-cc tcc' cannot continue." >&2
	sed 's/^/  /' "$probe_log" >&2
	exit 1
elif [ "$mode" = 'fresh' ]; then
	fresh_bundle
else
	latest_bundle
fi
