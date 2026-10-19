module cache

import time

// Count is a heap counter: closures capture plain values by copy, so shared
// mutable test state must live behind a reference (same reason the sync
// tests use atomics).
struct Count {
mut:
	n int
}

// ManualClock is a settable millisecond clock for deterministic TTL tests.
struct ManualClock {
mut:
	now i64
}

fn test_lru_basic_and_eviction() {
	mut c := new_lru[string, int](2)
	c.put('a', 1)
	c.put('b', 2)
	assert c.len() == 2
	assert c.get('a') or { -1 } == 1 // promotes a; b is now stalest
	c.put('c', 3) // evicts b
	assert c.len() == 2
	assert c.get('b') == none, 'least-recently-used entry must go first'
	assert c.get('a') or { -1 } == 1
	assert c.get('c') or { -1 } == 3
}

fn test_lru_get_or_compute_and_delete() {
	mut c := new_lru[string, int](4)
	mut calls := &Count{}
	first := c.get_or_compute('k', fn [mut calls] () int {
		calls.n++
		return 7
	})
	second := c.get_or_compute('k', fn [mut calls] () int {
		calls.n++
		return 8
	})
	assert first == 7 && second == 7, 'second call must hit the cache'
	assert calls.n == 1, 'factory must run once'
	assert c.delete('k') == true
	assert c.delete('k') == false
	assert c.get('k') == none
	assert c.contains('k') == false
}

fn test_ttl_expiry_with_fake_clock() {
	mut clk := &ManualClock{
		now: 1_000_000
	}
	clock := fn [clk] () i64 {
		return clk.now
	}
	mut t := new_ttl_with_clock[string, string](time.second, clock)
	t.set('k', 'v')
	assert t.get('k') or { '' } == 'v'
	clk.now += 999
	assert t.get('k') or { '' } == 'v', 'still fresh before ttl'
	clk.now += 1
	assert t.get('k') == none, 'expired exactly at ttl'
	assert t.len() == 0, 'expired entries are removed on access'
}

fn test_ttl_reset_and_purge() {
	mut clk := &ManualClock{}
	clock := fn [clk] () i64 {
		return clk.now
	}
	mut t := new_ttl_with_clock[string, int](time.second, clock)
	t.set('a', 1)
	t.set('b', 2)
	clk.now += 900
	t.set('a', 10) // re-set restarts a's ttl
	clk.now += 200
	assert t.get('a') or { -1 } == 10, 're-set must extend life'
	assert t.get('b') == none, 'b expired'
	assert t.purge_expired() == 0, 'access already removed b'
	t.set('c', 3)
	clk.now += 10_000
	assert t.purge_expired() == 2, 'a and c both expired'
	assert t.len() == 0
}

fn test_ttl_default_clock_constructor() {
	mut t := new_ttl[string, int](time.hour)
	t.set('key', 42)
	assert t.get('key') or { -1 } == 42
}
