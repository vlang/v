# x.async benchmarks

This folder contains cautious local benchmarks for the public `x.async` API.
They are observation tools, not correctness tests and not portable performance
claims.

## Available benchmark

- `async_benchmark.v`: measures short default runs for `Group`, `Task[T]`,
  `Pool`, `with_timeout()`, and `every()`.
- `run_async_benchmark.sh`: builds and runs the benchmark with the local `./v`,
  isolated `VTMP`/`VCACHE`, and a temporary output binary.

## Run

From the repository root:

```sh
sh vlib/x/async/benchmarks/run_async_benchmark.sh
```

The script is serialized by design. Do not run multiple V benchmark or test
runners against the same checkout/cache without isolating `VTMP`, `VCACHE`, and
output paths.

## Parameters

The defaults are intentionally small. Tune them locally with:

- `XASYNC_BENCH_GROUP_ROUNDS`
- `XASYNC_BENCH_GROUP_JOBS`
- `XASYNC_BENCH_TASK_ROUNDS`
- `XASYNC_BENCH_POOL_JOBS`
- `XASYNC_BENCH_POOL_WORKERS`
- `XASYNC_BENCH_TIMEOUT_ROUNDS`
- `XASYNC_BENCH_EVERY_ITERATIONS`
- `XASYNC_BENCH_EVERY_INTERVAL_MS`

Do not commit machine-specific benchmark results as permanent truth. Tests
remain the authority for functional and concurrency-safety validation.
