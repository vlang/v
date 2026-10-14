module cache

import time

// Lru[K, V] is a capacity-bounded least-recently-used cache: reads promote
// entries and inserts beyond capacity evict the stalest entry first. It is
// single-threaded; share it across threads behind a mutex or channel.
// It covers the `functools.lru_cache` / .NET `MemoryCache` use case.
pub struct Lru[K, V] {
mut:
	cap   int
	items map[K]V
	order []K // least-recently-used first; most-recently-used last
}

// new_lru creates an Lru holding at most cap entries.
pub fn new_lru[K, V](cap int) Lru[K, V] {
	assert cap > 0, 'cache.new_lru: cap must be positive'
	return Lru[K, V]{
		cap: cap
	}
}

// get returns the value for key and promotes it to most-recently-used,
// or none if absent.
pub fn (mut c Lru[K, V]) get(key K) ?V {
	if key !in c.items {
		return none
	}
	c.touch(key)
	return c.items[key]
}

// get_or_compute returns the cached value, computing and storing it on a miss.
pub fn (mut c Lru[K, V]) get_or_compute(key K, f fn () V) V {
	if key in c.items {
		c.touch(key)
		return c.items[key]
	}
	v := f()
	c.put(key, v)
	return v
}

// put stores value, evicting least-recently-used entries while over capacity.
pub fn (mut c Lru[K, V]) put(key K, value V) {
	if key in c.items {
		c.items[key] = value
		c.touch(key)
		return
	}
	c.items[key] = value
	c.order << key
	for c.order.len > c.cap {
		lru := c.order[0]
		c.order.delete(0)
		c.items.delete(lru)
	}
}

// delete removes key, returning false if it was absent.
pub fn (mut c Lru[K, V]) delete(key K) bool {
	if key !in c.items {
		return false
	}
	c.items.delete(key)
	i := c.order.index(key)
	c.order.delete(i)
	return true
}

// len returns the number of cached entries.
pub fn (c &Lru[K, V]) len() int {
	return c.order.len
}

// contains reports whether key is cached, without affecting recency.
pub fn (c &Lru[K, V]) contains(key K) bool {
	return key in c.items
}

// touch moves key to the most-recently-used end. The key must be present.
fn (mut c Lru[K, V]) touch(key K) {
	i := c.order.index(key)
	c.order.delete(i)
	c.order << key
}

// TtlEntry is one cached value with its millisecond expiry timestamp.
struct TtlEntry[V] {
	value   V
	expires i64
}

// Ttl[K, V] is a time-to-live cache: entries disappear after ttl elapses.
// Expiry uses millisecond timestamps from now (a monotonic clock by default), so a
// fake clock can be injected for deterministic tests. Single-threaded.
pub struct Ttl[K, V] {
mut:
	items map[K]TtlEntry[V]
	ttl   time.Duration
	now   fn () i64
}

// new_ttl creates a Ttl expiring entries ttl after each set.
pub fn new_ttl[K, V](ttl time.Duration) Ttl[K, V] {
	return new_ttl_with_clock[K, V](ttl, fn () i64 {
		return i64(time.sys_mono_now() / u64(time.millisecond))
	})
}

// new_ttl_with_clock is new_ttl with an injectable millisecond clock,
// for tests and custom time sources.
pub fn new_ttl_with_clock[K, V](ttl time.Duration, now fn () i64) Ttl[K, V] {
	return Ttl[K, V]{
		ttl: ttl
		now: now
	}
}

// set stores value, (re)starting its ttl.
pub fn (mut t Ttl[K, V]) set(key K, value V) {
	t.items[key] = TtlEntry[V]{value, t.now() + i64(t.ttl / time.millisecond)}
}

// get returns the value for key, or none if absent or expired (expired
// entries are removed on access).
pub fn (mut t Ttl[K, V]) get(key K) ?V {
	if key !in t.items {
		return none
	}
	e := t.items[key]
	if t.now() >= e.expires {
		t.items.delete(key)
		return none
	}
	return e.value
}

// delete removes key, returning false if it was absent.
pub fn (mut t Ttl[K, V]) delete(key K) bool {
	if key !in t.items {
		return false
	}
	t.items.delete(key)
	return true
}

// purge_expired removes all expired entries, returning how many were removed.
pub fn (mut t Ttl[K, V]) purge_expired() int {
	now := t.now()
	mut dead := []K{}
	for k, e in t.items {
		if now >= e.expires {
			dead << k
		}
	}
	for k in dead {
		t.items.delete(k)
	}
	return dead.len
}

// len returns the number of stored entries, including not-yet-purged expired
// ones (use purge_expired for an exact live count).
pub fn (t &Ttl[K, V]) len() int {
	return t.items.len
}
