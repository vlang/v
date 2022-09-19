import datatypes

fn test_push_and_pop() {
	mut r := datatypes.new_ringbuffer<int>(2)

	r.push(3)
	r.push(4)

	mut oldest_value := r.pop()

	assert oldest_value == 3

	r.push(5)

	oldest_value = r.pop()

	assert oldest_value == 4
}

fn test_clear_and_empty() {
	mut r := datatypes.new_ringbuffer<int>(4)
	r.push(3)
	r.push(4)

	oldest_value := r.pop()
	assert oldest_value == 3

	r.clear()

	assert r.is_empty() == true
}

fn test_capacity_and_is_full() {
	mut r := datatypes.new_ringbuffer<int>(4)

	assert r.capacity() == 4

	r.push(3)
	r.push(4)
	r.push(5)
	r.push(6)

	assert r.is_full() == true
}
