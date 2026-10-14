# cache

`new_lru[K, V](capacity)` creates a least-recently-used cache with a positive capacity.
`put` inserts or replaces a value; `get` returns an option and promotes a hit.
`get_or_compute` calls its factory only on a miss. `contains` does not change recency.
`delete` reports whether a key was present, and `len` counts stored entries.
Recency maintenance is linear in the number of cached entries.

`new_ttl[K, V](ttl)` creates a cache whose entries expire after the given duration.
`set` restarts an entry's TTL; `get` removes an expired entry and returns none.
`purge_expired` removes all expired entries and returns their count.
`len` includes expired entries until they are accessed or purged.
Expiry has millisecond resolution; a TTL below one millisecond expires immediately.
The default clock is monotonic. `new_ttl_with_clock[K, V](ttl, now)` accepts a callback
returning millisecond timestamps; custom clocks should be monotonic too.

Both caches are single-threaded. Synchronize access before sharing a cache between threads.
Keys must be supported V map keys.
