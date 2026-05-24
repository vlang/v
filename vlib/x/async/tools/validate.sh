#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
repo_root=$(CDPATH= cd "$script_dir/../../../.." && pwd)
cd "$repo_root"

if [ ! -x ./v ]; then
	echo "x.async validation must be run from a V checkout with local ./v" >&2
	exit 1
fi

tmp_root=$(mktemp -d "${TMPDIR:-/tmp}/xasync-validate.XXXXXX")
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

# Keep the official x.async validation serial. Running two V runners against the
# same checkout/cache can make build artefacts collide before x.async code runs.
v_files=$(find vlib/x/async -type f -name '*.v' | sort)
example_files=$(find vlib/x/async/examples -type f -name '*.v' | sort)

run_v 60 fmt -verify $v_files

for example in $example_files; do
	run_v 60 run "$example"
done

run_v 120 test vlib/x/async
run_v 180 -prod test vlib/x/async
