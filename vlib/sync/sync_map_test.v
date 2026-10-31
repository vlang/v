module sync

fn test_sync_map_basic() {
	mut m := new_map[string, int]()
	m.store('a', 1)
	m.store('b', 2)
	v, ok := m.load('a')
	assert ok
	assert v == 1
	_, ok2 := m.load('zzz')
	assert !ok2
	assert m.has('b')
	assert m.len() == 2
}

fn test_sync_map_delete() {
	mut m := new_map[string, int]()
	m.store('a', 1)
	m.delete('a')
	assert !m.has('a')
	assert m.len() == 0
}

fn test_sync_map_clear() {
	mut m := new_map[int, int]()
	m.store(1, 10)
	m.store(2, 20)
	m.clear()
	assert m.len() == 0
}

fn test_sync_map_keys_values() {
	mut m := new_map[string, int]()
	m.store('a', 1)
	m.store('b', 2)
	ks := m.keys()
	vs := m.values()
	assert ks.len == 2
	assert vs.len == 2
}
