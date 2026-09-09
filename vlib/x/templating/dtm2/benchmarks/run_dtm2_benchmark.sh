#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd "${script_dir}/../../../../.." && pwd)"
cd "${repo_root}"

timestamp="$(date +%Y-%m-%d_%H%M%S)"
results_dir="${DTM2_BENCH_RESULTS_DIR:-vlib/x/templating/dtm2/benchmarks/results/${timestamp}}"
bench_source="vlib/x/templating/dtm2/benchmarks/dtm2_benchmark.v"
bench_bin="/tmp/dtm2_benchmark_${timestamp}_$$"
bench_mode="${DTM2_BENCH_MODE:-prod}"

mkdir -p "${results_dir}"
trap 'rm -f "${bench_bin}"' EXIT

case "${bench_mode}" in
	dev)
		build_cmd=(./v -o "${bench_bin}" "${bench_source}")
		;;
	prod)
		build_cmd=(./v -prod -o "${bench_bin}" "${bench_source}")
		;;
	prod_o2)
		build_cmd=(./v -prod -no-prod-options -cflags -O2 -o "${bench_bin}" "${bench_source}")
		;;
	*)
		echo "unsupported DTM2_BENCH_MODE=${bench_mode}; use dev, prod, or prod_o2" >&2
		exit 2
		;;
esac

{
	echo "date: $(date -Iseconds)"
	echo "v_version: $(./v --version)"
	echo "kernel: $(uname -srmo)"
	echo "cpu_model: $(awk -F': ' '/model name/{print $2; exit}' /proc/cpuinfo)"
	echo "logical_cpu_count: $(nproc)"
	echo "memory_total: $(awk '/MemTotal/{print $2 " " $3}' /proc/meminfo)"
	echo "benchmark_mode: ${bench_mode}"
	echo "benchmark_source: ${bench_source}"
	echo "DTM2_BENCH_CASE: ${DTM2_BENCH_CASE:-all}"
	echo "DTM2_BENCH_ITERATIONS: ${DTM2_BENCH_ITERATIONS:-50000}"
	echo "DTM2_BENCH_COLD_ITERATIONS: ${DTM2_BENCH_COLD_ITERATIONS:-500}"
	echo "DTM2_BENCH_PLACEHOLDERS: ${DTM2_BENCH_PLACEHOLDERS:-50}"
	echo "DTM2_BENCH_COMPRESS_HTML: ${DTM2_BENCH_COMPRESS_HTML:-true}"
	echo "DTM2_BENCH_RELOAD_MODIFIED_TEMPLATES: ${DTM2_BENCH_RELOAD_MODIFIED_TEMPLATES:-false}"
	echo "DTM2_BENCH_VALIDATE_EACH_ITERATION: ${DTM2_BENCH_VALIDATE_EACH_ITERATION:-false}"
} > "${results_dir}/environment.txt"

"${build_cmd[@]}" > "${results_dir}/build.log" 2>&1

{
	cat "${results_dir}/environment.txt"
	echo
	/usr/bin/time -v "${bench_bin}"
} 2>&1 | tee "${results_dir}/benchmark.log"
