import sync

struct Store {
mut:
	mu      sync.Mutex
	entries map[string]int @[guarded_by: 'mu']
}

@[requires_lock: 'mu']
fn (s &Store) get(key string) int {
	return s.entries[key]
}

@[requires_lock: 'mu']
fn (mut s Store) put(key string, val int) {
	s.entries[key] = val
}

@[requires_lock: 'mu']
fn (s &Store) helper(key string) int {
	return s.get(key)
}

fn test_guarded_by_requires_lock() {
	mut s := Store{
		entries: map[string]int{}
	}
	s.mu.lock()
	s.put('a', 42)
	assert s.get('a') == 42
	assert s.helper('a') == 42
	s.mu.unlock()
	s.mu.lock()
	assert s.entries['a'] == 42
	s.mu.unlock()
}
