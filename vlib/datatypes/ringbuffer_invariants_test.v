module datatypes

fn test_a_fresh_buffer_is_empty_but_not_full() {
	mut rb := new_ringbuffer[int](4)
	assert rb.is_empty() == true
	assert rb.is_full() == false
	assert rb.occupied() == 0
	assert rb.remaining() == rb.capacity()
}

fn test_capacity_is_the_size_the_buffer_was_created_with() {
	assert new_ringbuffer[int](4).capacity() == 4
	assert new_ringbuffer[int](1).capacity() == 1
	assert new_ringbuffer[int](0).capacity() == 0
}

fn test_push_and_pop_preserve_fifo_order() {
	mut rb := new_ringbuffer[int](4)
	rb.push_many([1, 2, 3, 4]) or { panic(err) }
	assert rb.is_full() == true
	assert rb.occupied() == 4
	assert rb.remaining() == 0
	popped := rb.pop_many(4) or { panic(err) }
	assert popped == [1, 2, 3, 4]
	assert rb.is_empty() == true
	assert rb.occupied() == 0
	assert rb.remaining() == rb.capacity()
}

fn test_push_past_the_capacity_reports_an_overflow() ! {
	mut rb := new_ringbuffer[int](2)
	rb.push(1) or { panic(err) }
	rb.push(2) or { panic(err) }
	if _ := rb.push(3) {
		return error('expected a third push into a two-slot buffer to fail')
	} else {
		assert err.msg() == 'Buffer overflow'
	}
	assert rb.occupied() == 2
	assert rb.remaining() == 0
}

fn test_pop_from_an_empty_buffer_reports_an_error() ! {
	mut rb := new_ringbuffer[int](2)
	if _ := rb.pop() {
		return error('expected a pop from an empty buffer to fail')
	} else {
		assert err.msg() == 'Buffer is empty'
	}
	assert rb.is_empty() == true
	assert rb.remaining() == rb.capacity()
}

// push_many keeps whatever it wrote before the overflow: it does not roll the
// buffer back to the state it started in.
fn test_push_many_stops_at_the_first_overflow() ! {
	mut rb := new_ringbuffer[int](3)
	if _ := rb.push_many([1, 2, 3, 4, 5]) {
		return error('expected five values in a three-slot buffer to fail')
	} else {
		assert err.msg() == 'Buffer overflow'
	}
	assert rb.occupied() == 3
	kept := rb.pop_many(3) or { panic(err) }
	assert kept == [1, 2, 3]
}

// NOTE: the values pop_many already removed before the buffer ran dry are
// discarded with the error, so the caller cannot recover them.
fn test_pop_many_asking_for_more_than_it_holds_empties_the_buffer() ! {
	mut rb := new_ringbuffer[int](3)
	rb.push_many([1, 2, 3]) or { panic(err) }
	if _ := rb.pop_many(5) {
		return error('expected five pops from a three-slot buffer to fail')
	} else {
		assert err.msg() == 'Buffer is empty'
	}
	assert rb.is_empty() == true
	assert rb.occupied() == 0
	assert rb.remaining() == rb.capacity()
}

// The reader index has to wrap from the end of `content` back to the start
// while the writer is still moving forward, which the earlier tests never
// exercise because they drain the buffer before refilling it.
fn test_the_read_index_wraps_past_the_end_of_the_content_slice() {
	mut rb := new_ringbuffer[int](4)
	rb.push_many([1, 2, 3, 4]) or { panic(err) }
	rb.pop_many(3) or { panic(err) }
	assert rb.occupied() == 1
	assert rb.remaining() == 3
	rb.push_many([5, 6, 7]) or { panic(err) }
	assert rb.is_full() == true
	assert rb.occupied() == 4
	assert rb.remaining() == 0
	popped := rb.pop_many(4) or { panic(err) }
	assert popped == [4, 5, 6, 7]
	assert rb.is_empty() == true
	drained := rb.pop_many(3) or { []int{} }
	assert drained == []int{}
}

fn test_clear_resets_both_indices_and_zeroes_the_slots() {
	mut rb := new_ringbuffer[string](3)
	rb.push_many(['a', 'b', 'c']) or { panic(err) }
	rb.clear()
	assert rb.is_empty() == true
	assert rb.is_full() == false
	assert rb.occupied() == 0
	assert rb.remaining() == rb.capacity()
	assert rb.capacity() == 3
	rb.push('d') or { panic(err) }
	kept := rb.pop() or { panic(err) }
	assert kept == 'd'
	rb.pop() or { return }
	assert false, 'a cleared buffer should not yield a second value'
}

// NOTE: a buffer of size zero reports is_empty() and is_full() at the same
// time, and accepts neither a push nor a pop.
fn test_a_zero_sized_buffer_is_both_empty_and_full() ! {
	mut rb := new_ringbuffer[int](0)
	assert rb.is_empty() == true
	assert rb.is_full() == true
	assert rb.capacity() == 0
	if _ := rb.push(1) {
		return error('expected a push into a zero-sized buffer to fail')
	} else {
		assert err.msg() == 'Buffer overflow'
	}
	if _ := rb.pop() {
		return error('expected a pop from a zero-sized buffer to fail')
	} else {
		assert err.msg() == 'Buffer is empty'
	}
}

// A linear congruential generator rather than rand, so the sequence is the same
// on every run and a failure can be reproduced from the step number alone.
fn test_an_interleaved_sequence_matches_a_naive_reference() {
	for size in [1, 3, 7] {
		mut rb := new_ringbuffer[int](size)
		mut reference := []int{}
		mut seed := 1
		mut deepest := 0
		for i in 0 .. 200 {
			seed = (seed * 75 + 74) % 65537
			if seed % 3 != 0 {
				if rb.remaining() > 0 {
					rb.push(i) or { panic(err) }
					reference << i
				}
			} else if rb.occupied() > 0 {
				popped := rb.pop() or { panic(err) }
				assert popped == reference[0], 'size ${size}, step ${i}: got ${popped}, want ${reference[0]}'
				reference.delete(0)
			}
			assert rb.occupied() == reference.len, 'size ${size}, step ${i}'
			assert rb.remaining() == rb.capacity() - reference.len, 'size ${size}, step ${i}'
			assert rb.is_full() == (reference.len == size), 'size ${size}, step ${i}'
			assert rb.is_empty() == (reference.len == 0), 'size ${size}, step ${i}'
			if reference.len > deepest {
				deepest = reference.len
			}
		}
		// the reference and the buffer must have been driven to both extremes,
		// otherwise the loop proved nothing about wraparound
		assert deepest == size, 'size ${size} never filled up'
		assert reference.len == rb.occupied()
	}
}
