#!/bin/sh

# Compile the single vc snapshot with compatibility fixes for older versions.
set -eu

snapshot=$1
shift
script_dir=$(CDPATH= cd -P "$(dirname "$0")" && pwd)

# An optional -- separates linker inputs, which must follow the streamed source
# for linkers using --as-needed. Reset -x so object files keep their own language.
remaining=$#
source_added=0
while [ "$remaining" -gt 0 ]; do
	arg=$1
	shift
	remaining=$((remaining - 1))
	if [ "$source_added" -eq 0 ] && [ "$arg" = -- ]; then
		set -- "$@" -x c - -x none
		source_added=1
	else
		set -- "$@" "$arg"
	fi
done
if [ "$source_added" -eq 0 ]; then
	set -- "$@" -x c - -x none
fi

# A FIFO keeps the snapshot streaming while allowing a POSIX shell to wait for
# the filter separately and report its failure even if the compiler succeeds.
stream_dir=$(mktemp -d "${TMPDIR:-/tmp}/v-vc-compat.XXXXXX")
filter_pid=
trap 'rm -rf "$stream_dir"' 0
trap 'if [ -n "$filter_pid" ]; then kill "$filter_pid" 2>/dev/null || :; fi; exit 1' 1 2 3 15
mkfifo "$stream_dir/source"

# Generated string literals can contain bytes that are not valid UTF-8.
V_BOOTSTRAP_VROOT=$(CDPATH= cd -P "$script_dir/../.." && pwd) \
LC_ALL=C awk -f "$script_dir/vc_compat.awk" "$snapshot" > "$stream_dir/source" &
filter_pid=$!
if "$@" < "$stream_dir/source"; then
	compiler_status=0
else
	compiler_status=$?
fi
if wait "$filter_pid"; then
	filter_status=0
else
	filter_status=$?
fi
filter_pid=
if [ "$compiler_status" -ne 0 ]; then
	exit "$compiler_status"
fi
exit "$filter_status"
