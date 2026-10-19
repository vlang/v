module crdt

fn test_gset_lookup_and_add() {
	mut gset := new_gset[string]()
	assert gset.len() == 0
	assert gset.lookup('item1') == false

	gset.add('item1')
	assert gset.len() == 1
	assert gset.lookup('item1') == true
	assert gset.lookup('item2') == false

	// Adding duplicate does not increase length
	gset.add('item1')
	assert gset.len() == 1
}

fn test_gset_elements() {
	mut gset := new_gset[int]()
	gset.add(10)
	gset.add(20)
	elems := gset.elements()
	assert elems.len == 2
	assert 10 in elems
	assert 20 in elems
}

fn test_gset_equality_and_merge() {
	mut s1 := new_gset[string]()
	mut s2 := new_gset[string]()

	s1.add('apple')
	s2.add('banana')

	assert (s1 == s2) == false

	s1.merge(&s2)
	assert s1.lookup('apple')
	assert s1.lookup('banana')
	assert s1.len() == 2

	// Idempotent merge
	s1.merge(&s2)
	assert s1.len() == 2

	s2.add('apple')
	assert s1 == s2
}

