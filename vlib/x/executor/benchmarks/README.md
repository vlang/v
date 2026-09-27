# x.executor benchmarks

This folder contains cautious local benchmarks for the public `x.executor` API.
They are observation tools, not correctness tests and not portable performance
claims.

## Available benchmark

- `executor_benchmark.v`: measures short default runs for `try_post()` plus
  `drain_pending()`, `run_sync()`, bounded admission timeout, and small
  multi-producer submission.
- `run_executor_benchmark.sh`: builds and runs the benchmark with the local
  `./v`, isolated `VTMP`/`VCACHE`, and a temporary output binary.

## Run

From the repository root:

```sh
sh vlib/x/executor/benchmarks/run_executor_benchmark.sh
```

The script is serialized by design. Do not run multiple V benchmark or test
runners against the same checkout/cache without isolating `VTMP`, `VCACHE`, and
output paths.

## Parameters

The defaults are intentionally small. Tune them locally with:

- `XEXECUTOR_BENCH_DRAIN_ROUNDS`
- `XEXECUTOR_BENCH_DRAIN_JOBS`
- `XEXECUTOR_BENCH_SYNC_ROUNDS`
- `XEXECUTOR_BENCH_TIMEOUT_ROUNDS`
- `XEXECUTOR_BENCH_TIMEOUT_MS`
- `XEXECUTOR_BENCH_PRODUCERS`

All environment values are clamped. Do not commit machine-specific benchmark
results as permanent truth. Tests remain the authority for functional and
concurrency-safety validation.
