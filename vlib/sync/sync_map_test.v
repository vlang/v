module sync

import time

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

struct MapTestValue {
	name  string
	count int
}

fn test_sync_map_missing_pointer_is_nil() {
	mut m := new_map[string, &MapTestValue]()
	missing, found := m.load('missing')
	assert !found
	assert isnil(missing)
	value := &MapTestValue{'present', 7}
	m.store('present', value)
	loaded, present := m.load('present')
	assert present
	assert loaded == value
	assert loaded.count == 7
	m.store('nil', unsafe { nil })
	nil_value, nil_present := m.load('nil')
	assert nil_present
	assert isnil(nil_value)
}

fn test_sync_map_default_and_struct_values() {
	mut m := Map[int, MapTestValue]{}
	missing, found := m.load(7)
	assert !found
	assert missing == MapTestValue{}
	m.store(7, MapTestValue{'seven', 7})
	value, present := m.load(7)
	assert present
	assert value == MapTestValue{'seven', 7}
	m.store(7, MapTestValue{'replacement', 8})
	assert m.len() == 1
	m.delete(999)
	assert m.has(7)
}

fn test_sync_map_snapshots_survive_changes() {
	mut m := new_map[int, string]()
	m.store(1, 'one')
	m.store(2, 'two')
	mut keys := m.keys()
	mut values := m.values()
	m.store(1, 'changed')
	m.delete(2)
	m.clear()
	keys.sort()
	values.sort()
	assert keys == [1, 2]
	assert values == ['one', 'two']
	assert m.keys() == []int{}
	assert m.values() == []string{}
}

fn test_sync_map_shared_between_workers() {
	m := &Map[int, int]{}
	done := chan bool{cap: 4}
	for worker in 0 .. 4 {
		spawn fn (m &Map[int, int], worker int, done chan bool) {
			for i in 0 .. 200 {
				key := worker * 200 + i
				m.store(key, key * 3)
				value, found := m.load(key)
				assert found
				assert value == key * 3
				assert m.has(key)
				if i % 2 == 0 {
					m.delete(key)
				}
				_ = m.keys()
				_ = m.values()
			}
			done <- true
		}(m, worker, done)
	}
	for _ in 0 .. 4 {
		select {
			_ := <-done {
			}
			5 * time.second {
				panic('concurrent map worker did not finish')
			}
		}
	}
	assert m.len() == 400
	for key in 0 .. 800 {
		value, found := m.load(key)
		assert found == (key % 2 == 1)
		if found {
			assert value == key * 3
		}
	}
}

fn test_sync_map_clear_during_access() {
	m := &Map[int, int]{}
	done := chan bool{cap: 2}
	spawn fn (m &Map[int, int], done chan bool) {
		for i in 0 .. 200 {
			m.store(i, i)
			value, found := m.load(i)
			if found {
				assert value == i
			}
			_ = m.keys()
			_ = m.values()
		}
		done <- true
	}(m, done)
	spawn fn (m &Map[int, int], done chan bool) {
		for _ in 0 .. 200 {
			m.clear()
			_ = m.len()
		}
		done <- true
	}(m, done)
	for _ in 0 .. 2 {
		select {
			_ := <-done {
			}
			5 * time.second {
				panic('map clear worker did not finish')
			}
		}
	}
	m.clear()
	assert m.len() == 0
	m.store(42, 9)
	value, found := m.load(42)
	assert found && value == 9
}
