#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
repo_root=$(CDPATH= cd "$script_dir/../../../.." && pwd)
cd "$repo_root"

if [ ! -x ./v ]; then
	echo "x.executor validation must be run from a V checkout with local ./v" >&2
	exit 1
fi

tmp_root=$(mktemp -d "${TMPDIR:-/tmp}/xexecutor-validate.XXXXXX")
cleanup() {
	rm -rf "$tmp_root"
}
trap cleanup EXIT INT TERM

vtmp="$tmp_root/vtmp"
vcache="$tmp_root/vcache"
mkdir -p "$vtmp" "$vcache"

run_v() {
	seconds=$1
	shift
	echo "+ ./v $*"
	if command -v timeout >/dev/null 2>&1; then
		timeout "$seconds" env VTMP="$vtmp" VCACHE="$vcache" ./v "$@"
	else
		env VTMP="$vtmp" VCACHE="$vcache" ./v "$@"
	fi
}

sh vlib/x/executor/tools/check_no_async_dependency.sh

v_files=$(find vlib/x/executor -type f -name '*.v' | sort)
example_files=$(find vlib/x/executor/examples -type f -name '*.v' | sort)

run_v 60 fmt -verify $v_files

for example in $example_files; do
	run_v 60 run "$example"
done

run_v 120 test vlib/x/executor
run_v 180 -prod test vlib/x/executor
