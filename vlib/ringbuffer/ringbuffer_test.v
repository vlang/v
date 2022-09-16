import ringbuffer

fn test_push_and_pop() {
	mut r := ringbuffer.new<int>(2)

	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	mut oldest_value := r.pop() or { panic(err) }

	assert oldest_value == 3

	r.push(5) or { panic(err) }

	oldest_value = r.pop() or { panic(err) }

	assert oldest_value == 4
}

fn test_clear_and_empty() {
	mut r := ringbuffer.new<int>(4)
	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	oldest_value := r.pop() or { panic(err) }

	assert oldest_value == 3

	r.clear()

	assert r.is_empty() == true
}

fn test_capacity_remaining_is_full() {
	mut r := ringbuffer.new<int>(4)

	r.push(3) or { panic(err) }
	r.push(4) or { panic(err) }

	assert r.is_full() == false
	assert r.capacity() == 4
	assert r.remaining() == 2
}
