module sync

// concurrent_map_shards is the default shard count: a power of two so the
// shard index is one mask operation.
const concurrent_map_shards = 32

// shard is one independently locked partition of a ConcurrentMap.
@[heap]
struct ConcurrentShard[K, V] {
mut:
	mutex &Mutex
	items map[K]V
}

// ConcurrentMap is a thread-safe map with store/load/delete and Go-style
// range iteration. It shards entries over 32 independently locked partitions,
// so disjoint keys proceed in parallel; only same-shard keys serialize. It is
// the V-idiomatic equivalent of Go's `sync.Map` / .NET's
// `ConcurrentDictionary`. Key types follow the same constraints as builtin
// `map[K]V`.
@[noinit]
pub struct ConcurrentMap[K, V] {
mut:
	shards []&ConcurrentShard[K, V]
}

// new_concurrent_map creates an empty ConcurrentMap.
pub fn new_concurrent_map[K, V]() &ConcurrentMap[K, V] {
	mut m := &ConcurrentMap[K, V]{}
	for _ in 0 .. concurrent_map_shards {
		m.shards << &ConcurrentShard[K, V]{
			mutex: new_mutex()
		}
	}
	return m
}

// shard_for maps a key to its partition. String keys use their first byte
// mixed with their length; pointer keys use their stable address, while
// other scalar key types hash through their string form.
fn (m &ConcurrentMap[K, V]) shard_for(key K) &ConcurrentShard[K, V] {
	$if K is $pointer {
		address := u64(usize(key))
		h := address ^ (address >> 5) ^ (address >> 17)
		return m.shards[h % u64(concurrent_map_shards)]
	} $else $if K is string {
		s := key
		mut h := u64(s.len)
		if s.len > 0 {
			h = h * 31 + u64(s[0])
			h = h * 31 + u64(s[s.len - 1])
		}
		return m.shards[h % u64(concurrent_map_shards)]
	} $else {
		s := key.str()
		mut h := u64(s.len * 31 + 7)
		for i := 0; i < s.len && i < 16; i++ {
			h = h * 31 + u64(s[i])
		}
		return m.shards[h % u64(concurrent_map_shards)]
	}
}

// store sets the value for key.
pub fn (m &ConcurrentMap[K, V]) store(key K, value V) {
	mut s := m.shard_for(key)
	s.mutex.lock()
	defer {
		s.mutex.unlock()
	}
	s.items[key] = value
}

// load returns the value for key, or none if absent.
pub fn (m &ConcurrentMap[K, V]) load(key K) ?V {
	mut s := m.shard_for(key)
	s.mutex.lock()
	defer {
		s.mutex.unlock()
	}
	if key !in s.items {
		return none
	}
	return s.items[key]
}

// load_or_store returns the existing value for key, or stores and returns
// value when absent. The second result reports whether the value was loaded
// (true) rather than stored (false).
pub fn (m &ConcurrentMap[K, V]) load_or_store(key K, value V) (V, bool) {
	mut s := m.shard_for(key)
	s.mutex.lock()
	defer {
		s.mutex.unlock()
	}
	if key in s.items {
		return s.items[key], true
	}
	s.items[key] = value
	return value, false
}

// delete removes key. It reports nothing: deleting an absent key is a no-op.
pub fn (m &ConcurrentMap[K, V]) delete(key K) {
	mut s := m.shard_for(key)
	s.mutex.lock()
	defer {
		s.mutex.unlock()
	}
	s.items.delete(key)
}

// range calls f for every entry. Returning false from f stops the iteration.
// Entries stored during iteration may or may not be visited.
pub fn (m &ConcurrentMap[K, V]) range(f fn (key K, val V) bool) {
	for i in 0 .. m.shards.len {
		mut s := m.shards[i]
		s.mutex.lock()
		keys := s.items.keys()
		vals := s.items.values()
		s.mutex.unlock()
		for j, k in keys {
			if !f(k, vals[j]) {
				return
			}
		}
	}
}

// len returns the total number of entries.
pub fn (m &ConcurrentMap[K, V]) len() int {
	mut n := 0
	for i in 0 .. m.shards.len {
		mut s := m.shards[i]
		s.mutex.lock()
		n += s.items.len
		s.mutex.unlock()
	}
	return n
}
