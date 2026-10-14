import sync
import sync.stdatomic { new_atomic }

fn test_concurrent_map_basic() {
	mut m := sync.new_concurrent_map[string, int]()
	m.store('a', 1)
	m.store('b', 2)
	assert m.load('a') or { -1 } == 1
	assert m.load('missing') == none
	v, loaded := m.load_or_store('a', 99)
	assert v == 1 && loaded == true, 'existing value must win'
	v2, loaded2 := m.load_or_store('c', 3)
	assert v2 == 3 && loaded2 == false, 'missing key must store'
	assert m.len() == 3
	m.delete('b')
	m.delete('absent') // no-op
	assert m.len() == 2
	mut got_a := new_atomic(0)
	mut got_c := new_atomic(0)
	mut got_n := new_atomic(0)
	m.range(fn [mut got_a, mut got_c, mut got_n] (k string, v int) bool {
		if k == 'a' {
			got_a.store(v)
		} else if k == 'c' {
			got_c.store(v)
		}
		got_n.add(1)
		return true
	})
	assert got_a.load() == 1
	assert got_c.load() == 3
	assert got_n.load() == 2
}

fn test_concurrent_map_int_keys_and_early_stop() {
	mut m := sync.new_concurrent_map[int, string]()
	for i in 0 .. 100 {
		m.store(i, 'v${i}')
	}
	assert m.len() == 100
	assert m.load(42) or { '' } == 'v42'
	mut visits := new_atomic(0)
	m.range(fn [mut visits] (k int, v string) bool {
		_ = v
		_ = k
		visits.add(1)
		return visits.load() < 5
	})
	assert visits.load() == 5, 'returning false must stop iteration'
}

fn test_concurrent_map_hammer() {
	mut m := sync.new_concurrent_map[int, int]()
	done_ch := chan bool{cap: 8}
	defer {
		done_ch.close()
	}
	for w in 0 .. 8 {
		spawn fn [mut m, w, done_ch] () {
			for i in 0 .. 250 {
				k := w * 250 + i
				m.store(k, k)
				_ := m.load(k) or { -1 }
			}
			done_ch <- true
		}()
	}
	for _ in 0 .. 8 {
		_ := <-done_ch
	}
	assert m.len() == 2000, 'all hammered keys must be present'
	mut total := new_atomic(i64(0))
	m.range(fn [mut total] (k int, v int) bool {
		_ = k
		total.add(i64(v))
		return true
	})
	assert total.load() == i64(2000 * 1999 / 2), 'every value must be visited exactly once in sum'
}

struct ConcurrentMapMutableKey {
mut:
	n int
}

fn test_concurrent_map_pointer_keys_keep_identity_after_mutation() {
	mut key := &ConcurrentMapMutableKey{ n: 1 }
	mut other := &ConcurrentMapMutableKey{ n: 1 }
	m := sync.new_concurrent_map[&ConcurrentMapMutableKey, string]()
	m.store(key, 'first')
	m.store(other, 'second')
	key.n = 2026
	assert m.load(key)? == 'first'
	assert m.load(other)? == 'second'
	value, loaded := m.load_or_store(key, 'wrong')
	assert loaded
	assert value == 'first'
	m.delete(key)
	assert m.load(key) == none
	assert m.len() == 1
}

fn test_concurrent_map_callback_can_mutate_map() {
	m := sync.new_concurrent_map[int, int]()
	m.store(1, 2)
	m.range(fn [m] (key int, value int) bool {
		assert key == 1
		assert value == 2
		m.delete(key)
		return true
	})
	assert m.len() == 0
}
