module datatypes

fn test_btree_string_basic() {
	mut m := new_btree[string, int]()
	assert m.is_empty()
	assert m.len() == 0
	m.set('pear', 3)
	m.set('apple', 1)
	m.set('plum', 4)
	assert m.len() == 3
	assert m.get('apple') or { -1 } == 1
	assert m.get('missing') or { -1 } == -1
	assert m.exists('plum')
	assert !m.exists('missing')
	m.set('apple', 10)
	assert m.len() == 3, 'replace must not grow the map'
	assert m.get('apple') or { -1 } == 10
	assert m.keys() == ['apple', 'pear', 'plum']
	assert m.delete('pear') == true
	assert m.delete('pear') == false
	assert m.keys() == ['apple', 'plum']
	assert (m.min_key() or { '' }) == 'apple'
	assert (m.max_key() or { '' }) == 'plum'
}

fn is_sorted(a []int) bool {
	for i in 1 .. a.len {
		if a[i - 1] >= a[i] {
			return false
		}
	}
	return true
}

fn test_btree_many_inserts_stay_sorted() {
	mut m := new_btree[int, int]()
	// deterministic pseudo-shuffle: 7919 is coprime to 1000
	for i in 0 .. 1000 {
		k := (i * 7919) % 1000
		m.set(k, k * 2)
	}
	assert m.len() == 1000
	keys := m.keys()
	assert keys.len == 1000
	assert is_sorted(keys)
	for k in 0 .. 1000 {
		assert m.get(k) or { -1 } == k * 2
	}
}

fn test_btree_delete_keeps_order() {
	mut m := new_btree[int, int]()
	for i in 0 .. 1000 {
		m.set((i * 7919) % 1000, i)
	}
	for k in 0 .. 1000 {
		if k % 2 == 0 {
			assert m.delete(k) == true
		}
	}
	assert m.len() == 500
	keys := m.keys()
	assert keys.len == 500
	assert is_sorted(keys)
	for k in keys {
		assert k % 2 == 1
	}
	assert m.delete(42) == false
	// delete everything, then reuse the tree (root shrink path)
	for k in keys {
		assert m.delete(k) == true
	}
	assert m.is_empty()
	assert (m.min_key() or { -1 }) == -1
	m.set(7, 7)
	assert m.get(7) or { -1 } == 7
}

fn test_btree_range_and_iterate() {
	mut m := new_btree[int, string]()
	for k in 0 .. 100 {
		m.set(k, 'v${k}')
	}
	assert m.range_keys(20, 29) == [20, 21, 22, 23, 24, 25, 26, 27, 28, 29]
	assert m.range_keys(0, 0) == [0]
	assert m.range_keys(99, 99) == [99]
	assert m.range_keys(50, 40) == []
	mut it := m.iterate(20, 22)
	mut got := []string{}
	for {
		e := it.next() or { break }
		got << '${e.key}=${e.value}'
	}
	assert got == ['20=v20', '21=v21', '22=v22']
}

fn test_btreeset_basic() {
	mut s := new_btree_set[string]()
	assert s.is_empty()
	assert s.add('b') == true
	assert s.add('a') == true
	assert s.add('c') == true
	assert s.add('a') == false, 'duplicate add must fail'
	assert s.len() == 3
	assert s.contains('b')
	assert !s.contains('z')
	assert (s.min() or { '' }) == 'a'
	assert (s.max() or { '' }) == 'c'
	assert s.range('a', 'b') == ['a', 'b']
	assert s.remove('b') == true
	assert s.remove('b') == false
	assert s.range('a', 'c') == ['a', 'c']
}
