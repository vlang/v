module sync

// Map protects a typed map with a single mutex. Share one Map by reference between
// threads; do not copy it. The default value is an empty, initialized map.
// The mutex protects map operations, not later mutations of objects stored as values.
@[heap]
pub struct Map[K, V] {
mut:
	mu   &Mutex = new_mutex()
	data map[K]V
}

// new_map creates an empty concurrent map.
pub fn new_map[K, V]() Map[K, V] {
	return Map[K, V]{}
}

// load returns the value for key and whether it was present.
// Missing pointer values are nil; other missing values use their type's default.
pub fn (m &Map[K, V]) load(key K) (V, bool) {
	m.mu.lock()
	defer { m.mu.unlock() }
	if v := m.data[key] {
		return v, true
	}
	$if V is $pointer {
		return unsafe { V(nil) }, false
	} $else {
		return V{}, false
	}
}

// store sets the value for key.
pub fn (mut m Map[K, V]) store(key K, value V) {
	m.mu.lock()
	defer { m.mu.unlock() }
	m.data[key] = value
}

// delete removes key.
pub fn (mut m Map[K, V]) delete(key K) {
	m.mu.lock()
	defer { m.mu.unlock() }
	m.data.delete(key)
}

// has reports whether key is present.
pub fn (m &Map[K, V]) has(key K) bool {
	m.mu.lock()
	defer { m.mu.unlock() }
	return key in m.data
}

// len returns the number of entries.
pub fn (m &Map[K, V]) len() int {
	m.mu.lock()
	defer { m.mu.unlock() }
	return m.data.len
}

// clear removes all entries.
pub fn (mut m Map[K, V]) clear() {
	m.mu.lock()
	defer { m.mu.unlock() }
	m.data.clear()
}

// keys returns a snapshot of all keys, in unspecified order.
// Separate calls to keys and values may observe different concurrent changes.
pub fn (m &Map[K, V]) keys() []K {
	m.mu.lock()
	defer { m.mu.unlock() }
	mut out := []K{cap: m.data.len}
	for k, _ in m.data {
		out << k
	}
	return out
}

// values returns a shallow snapshot of all values, in unspecified order.
// Reference values still refer to their original objects.
pub fn (m &Map[K, V]) values() []V {
	m.mu.lock()
	defer { m.mu.unlock() }
	mut out := []V{cap: m.data.len}
	for _, v in m.data {
		out << v
	}
	return out
}
