import datatypes

fn test_push_and_pop() {
	mut r := datatypes.new_ringbuffer<int>(2)

	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	mut oldest_value := r.pop() or { 0 }

	assert oldest_value == 3

	r.push(5) or { panic(err) }

	oldest_value = r.pop() or { 0 }

	assert oldest_value == 4
}

fn test_clear_and_empty() {
	mut r := datatypes.new_ringbuffer<int>(4)
	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	oldest_value := r.pop() or { 0 }
	assert oldest_value == 3

	r.clear()

	assert r.is_empty() == true
}

fn test_capacity_and_is_full() {
	mut r := datatypes.new_ringbuffer<int>(4)

	assert r.capacity() == 4

	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }
	r.push(5) or { panic(err) }
	r.push(6) or { panic(err) }

	assert r.is_full() == true
}

fn test_occupied_and_remaining() {
	mut r := datatypes.new_ringbuffer<int>(4)

	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	assert r.occupied() == r.remaining()
}

fn test_push_and_pop_many() {
	mut r := datatypes.new_ringbuffer<int>(4)
	a := [1, 2, 3, 4]
	r.push_many(a) or { panic(err) }

	assert r.is_full() == true

	b := r.pop_many(4) or { panic(err) }

	assert a == b
}
