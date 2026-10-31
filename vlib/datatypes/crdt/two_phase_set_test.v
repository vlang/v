module crdt

fn test_two_phase_set_basic() {
	mut tps := new_two_phase_set[string]()
	assert tps.len() == 0
	assert tps.lookup('test') == false

	tps.add('test')
	assert tps.lookup('test') == true
	assert tps.len() == 1
	assert tps.elements() == ['test']

	tps.remove('test')
	assert tps.lookup('test') == false
	assert tps.len() == 0

	// In a 2P-Set, re-adding after removal does not restore the element
	tps.add('test')
	assert tps.lookup('test') == false
}

fn test_two_phase_set_merge() {
	mut s1 := new_two_phase_set[string]()
	mut s2 := new_two_phase_set[string]()

	s1.add('item1')
	s1.add('item2')

	s2.add('item2')
	s2.remove('item2')
	s2.add('item3')

	assert s1.lookup('item2') == true
	assert s2.lookup('item2') == false

	// Merge s2 into s1: item2 will be removed in s1 because removal wins
	s1.merge(&s2)
	assert s1.lookup('item1') == true
	assert s1.lookup('item2') == false
	assert s1.lookup('item3') == true

	// Commutative merge
	s2.merge(&s1)
	assert s1 == s2
}
