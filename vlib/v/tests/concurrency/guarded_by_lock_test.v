import sync

struct Store {
mut:
	mu      sync.Mutex
	entries map[string]int @[guarded_by: 'mu']
}

fn new_store() Store {
	return Store{
		entries: map[string]int{}
	}
}

fn test_guarded_by_lock() {
	mut s := new_store()
	s.mu.lock()
	s.entries['a'] = 1
	s.entries['b'] = 2
	s.mu.unlock()
	s.mu.lock()
	assert s.entries['a'] == 1
	assert s.entries['b'] == 2
	s.entries['a'] = 10
	s.mu.unlock()
	s.mu.lock()
	assert s.entries['a'] == 10
	s.mu.unlock()
}
