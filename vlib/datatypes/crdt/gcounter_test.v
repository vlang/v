module crdt

fn test_gcounter_increment() {
	mut counter := new_gcounter()
	assert counter.value() == 0
	counter.increment()
	assert counter.value() == 1
	counter.increment_value(5)
	assert counter.value() == 6
}

fn test_gcounter_merge() {
	mut c1 := new_gcounter_with_id('node_a')
	mut c2 := new_gcounter_with_id('node_b')

	c1.increment_value(3)
	c2.increment_value(4)

	assert c1.value() == 3
	assert c2.value() == 4

	// Merge c2 into c1
	c1.merge(&c2)
	assert c1.value() == 7

	// Idempotency: merging again should not change value
	c1.merge(&c2)
	assert c1.value() == 7

	// Commutativity: merging c1 into c2 yields same result
	c2.merge(&c1)
	assert c2.value() == 7
}

