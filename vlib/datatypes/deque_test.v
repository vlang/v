module datatypes

fn test_deque_interleaved() {
	mut d := new_deque[int]()
	assert d.is_empty() && d.len() == 0
	d.push_back(1)
	d.push_front(0)
	d.push_back(2)
	assert d.len() == 3
	assert d.array() == [0, 1, 2]
	assert d.peek_front()! == 0
	assert d.peek_back()! == 2
	assert d.pop_front()! == 0
	assert d.pop_back()! == 2
	assert d.pop_front()! == 1
	assert d.is_empty()
}

fn test_deque_empty_pop_front_errors() {
	mut d := new_deque[int]()
	d.pop_front() or {
		assert err.msg() == 'datatypes: pop_front from empty Deque'
		return
	}
	assert false, 'pop_front on empty must error'
}

fn test_deque_empty_pop_back_errors() {
	mut d := new_deque[int]()
	d.pop_back() or {
		assert err.msg() == 'datatypes: pop_back from empty Deque'
		return
	}
	assert false, 'pop_back on empty must error'
}

fn test_deque_empty_peek_front_errors() {
	d := new_deque[int]()
	d.peek_front() or {
		return
	}
	assert false, 'peek_front on empty must error'
}

fn test_deque_empty_peek_back_errors() {
	d := new_deque[int]()
	d.peek_back() or {
		return
	}
	assert false, 'peek_back on empty must error'
}

fn test_deque_popped_values_are_not_retained() {
	mut d := new_deque[string]()
	d.push_back('front payload')
	d.push_back('back payload')
	assert d.pop_front()! == 'front payload'
	assert d.pop_back()! == 'back payload'
	for value in d.buf {
		assert value == ''
	}
	d.push_front('reused')
	assert d.array() == ['reused']
}

fn test_deque_array_snapshot_is_independent() {
	mut d := new_deque[int]()
	d.push_back(1)
	d.push_back(2)
	mut snapshot := d.array()
	snapshot[0] = 9
	d.push_front(0)
	assert snapshot == [9, 2]
	assert d.array() == [0, 1, 2]
}

struct DequeRefPayload {
	value int
}

fn test_deque_reference_and_array_values_survive_pop() {
	mut refs := new_deque[&DequeRefPayload]()
	refs.push_back(&DequeRefPayload{ value: 7 })
	assert refs.pop_back()!.value == 7
	assert isnil(refs.buf[0])
	mut arrays := new_deque[[]int]()
	arrays.push_front([1, 2, 3])
	assert arrays.pop_front()! == [1, 2, 3]
	for value in arrays.buf {
		assert value.len == 0
	}
}

fn test_deque_wrap_and_growth() {
	mut d := new_deque[int]()
	// fill, drain from the front to move head mid-ring, then keep pushing
	// so the ring wraps around before it grows
	for i in 0 .. 6 {
		d.push_back(i)
	}
	for _ in 0 .. 4 {
		d.pop_front()!
	}
	for i in 6 .. 20 {
		d.push_back(i)
	}
	d.push_front(-1)
	mut expected := [-1, 4, 5]
	for i in 6 .. 20 {
		expected << i
	}
	assert d.array() == expected
	assert d.len() == expected.len
	mut drained := []int{}
	for !d.is_empty() {
		drained << d.pop_back()!
	}
	mut rev := expected.clone()
	rev.reverse_in_place()
	assert drained == rev, 'pop_back must drain in reverse order'
}

fn test_deque_as_stack_and_queue() {
	mut s := new_deque[string]()
	s.push_back('a')
	s.push_back('b')
	assert s.pop_back()! == 'b'
	assert s.pop_back()! == 'a'
	mut q := new_deque[string]()
	q.push_back('a')
	q.push_back('b')
	assert q.pop_front()! == 'a'
	assert q.pop_front()! == 'b'
}
