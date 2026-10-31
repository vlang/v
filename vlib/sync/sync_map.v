module sync

// Map is a concurrent map, similar to Go's sync.Map. It is safe for
// concurrent use by multiple goroutines.
pub struct Map[K, V] {
mut:
	mu   Mutex
	data map[K]V
}

// new_map creates an empty concurrent map.
pub fn new_map[K, V]() Map[K, V] {
	return Map[K, V]{}
}

// load returns the value for key and whether it was present.
pub fn (m &Map[K, V]) load(key K) (V, bool) {
	m.mu.lock()
	defer { m.mu.unlock() }
	if v := m.data[key] {
		return v, true
	}
	return V{}, false
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

// keys returns a snapshot of all keys.
pub fn (m &Map[K, V]) keys() []K {
	m.mu.lock()
	defer { m.mu.unlock() }
	mut out := []K{cap: m.data.len}
	for k, _ in m.data {
		out << k
	}
	return out
}

// values returns a snapshot of all values.
pub fn (m &Map[K, V]) values() []V {
	m.mu.lock()
	defer { m.mu.unlock() }
	mut out := []V{cap: m.data.len}
	for _, v in m.data {
		out << v
	}
	return out
}
