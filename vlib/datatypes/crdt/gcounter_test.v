module crdt

fn test_increment() {
	mut counter := new_gcounter()
	counter.increment()
	assert counter.value() == 1
}

fn test_merge() {
	mut first_counter := new_gcounter()
	first_counter.increment()
	assert first_counter.value() == 1
	mut second_counter := new_gcounter()
	second_counter.merge(first_counter)
	assert second_counter.value() == 1
}
