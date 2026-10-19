`metrics.new_registry()` owns named counters, gauges, and histograms. Create instruments with
`counter(name)`, `gauge(name)`, or `histogram(name, bounds)`. `Counter.add` accepts nonnegative
increments; gauges support `set` and signed `inc`. Invalid names, negative or NaN counter
increments, conflicting instrument types or histogram sample names, and invalid histogram bounds
panic. Names use ASCII letters, underscores, and noninitial digits; metric names may also
contain colons.

Every operation, including registration and `to_prometheus`, synchronizes access. A scrape locks
each instrument while rendering it; values from different instruments are not a global snapshot.
Caller-owned label maps must not be modified concurrently with an operation that reads them.
Label order is irrelevant, and labels containing commas, equals signs, quotes, backslashes, or
newlines remain distinct and are escaped for Prometheus exposition.

Histograms use finite, strictly increasing bounds. Reusing a name requires identical bounds.
`record` maintains cumulative buckets, count, and sum. The exporter appends an implicit `+Inf`
bucket; the `le` label is reserved and cannot be supplied to `record`. `to_prometheus()` renders
HELP/TYPE lines and samples in Prometheus text format, including `NaN`, `+Inf`, and `-Inf` gauge
values. Construct instruments through the registry so they are exported.
