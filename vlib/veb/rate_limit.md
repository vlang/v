# Rate limit middleware

Register `veb.rate_limit[Context](requests_per_second: 5, burst: 10)` with `app.use` or
`app.route_use` to limit requests by client IP. Each key starts with `burst` tokens and
refills at `requests_per_second`, up to `burst` tokens.

The store holds at most `max_keys` client keys. Before admitting a new key to a full
store, the limiter removes buckets idle longer than `idle_timeout`, measured in
milliseconds. If every slot is still occupied, the new client receives HTTP 429.
Existing clients keep their buckets and token counts, so filling the store cannot
reset another client's rate limit. Allowed and denied requests keep a bucket active.

Denied requests use the configured `message` and include a `Retry-After` header in
whole seconds. Token exhaustion reports the wait for another token. A full store
reports the earliest possible idle bucket expiry, rounded up; continued activity
can keep that slot occupied longer. A nonpositive `requests_per_second` never
refills tokens and reports a 60-second retry after its initial burst is exhausted.
A nonpositive `max_keys` rejects every request without allocating buckets and also
reports a 60-second retry.
