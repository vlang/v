# ratelimit

`new_token_bucket(rate_per_sec: 100.0, burst: 20)` creates a thread-safe token bucket.
The bucket starts full; `allow()` consumes one token or returns false when empty.
Tokens refill at the positive, finite `rate_per_sec`, capped at the positive `burst` capacity.
`stats()` returns the numbers of allowed and denied requests.

The default clock is monotonic with millisecond resolution.
`new_token_bucket_with_clock(config, now)` accepts a millisecond clock for deterministic tests.
Backward clock readings do not refill the bucket or move its last-refill timestamp backward.
The clock callback runs while the bucket mutex is locked; it must not reenter the same bucket.
