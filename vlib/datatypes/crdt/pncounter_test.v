module crdt

fn test_pncounter_basic() {
	mut pn := new_pncounter()
	assert pn.value() == 0
	pn.increment()
	assert pn.value() == 1
	pn.increment_value(10)
	assert pn.value() == 11
	pn.decrement()
	assert pn.value() == 10
	pn.decrement_value(15)
	assert pn.value() == -5
}

fn test_pncounter_merge() {
	mut pn1 := new_pncounter_with_id('node1')
	mut pn2 := new_pncounter_with_id('node2')

	pn1.increment_value(10)
	pn1.decrement_value(2)
	assert pn1.value() == 8

	pn2.increment_value(5)
	pn2.decrement_value(1)
	assert pn2.value() == 4

	// Merge pn2 into pn1: (10 + 5) - (2 + 1) = 12
	pn1.merge(&pn2)
	assert pn1.value() == 12

	// Idempotency: merging again changes nothing
	pn1.merge(&pn2)
	assert pn1.value() == 12

	// Commutativity: merging pn1 into pn2 gives same result
	pn2.merge(&pn1)
	assert pn2.value() == 12
}

