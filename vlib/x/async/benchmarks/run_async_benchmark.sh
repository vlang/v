#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd "$(dirname "$0")" && pwd)
repo_root=$(CDPATH= cd "$script_dir/../../../.." && pwd)
cd "$repo_root"

if [ ! -x ./v ]; then
	echo "x.async benchmarks must be run from a V checkout with local ./v" >&2
	exit 1
fi

tmp_root=$(mktemp -d "${TMPDIR:-/tmp}/xasync-benchmark.XXXXXX")
cleanup() {
	rm -rf "$tmp_root"
}
trap cleanup EXIT INT TERM

vtmp="$tmp_root/vtmp"
vcache="$tmp_root/vcache"
out="$tmp_root/out/async_benchmark"
mkdir -p "$vtmp" "$vcache" "$(dirname "$out")"

echo "Running x.async benchmark with isolated VTMP and VCACHE."
echo "Tune sizes with XASYNC_BENCH_GROUP_ROUNDS, XASYNC_BENCH_GROUP_JOBS,"
echo "XASYNC_BENCH_TASK_ROUNDS, XASYNC_BENCH_POOL_JOBS,"
echo "XASYNC_BENCH_POOL_WORKERS, XASYNC_BENCH_TIMEOUT_ROUNDS,"
echo "XASYNC_BENCH_EVERY_ITERATIONS, and XASYNC_BENCH_EVERY_INTERVAL_MS."

if command -v timeout >/dev/null 2>&1; then
	timeout 180s env VTMP="$vtmp" VCACHE="$vcache" ./v -prod -o "$out" run vlib/x/async/benchmarks/async_benchmark.v
else
	env VTMP="$vtmp" VCACHE="$vcache" ./v -prod -o "$out" run vlib/x/async/benchmarks/async_benchmark.v
fi
