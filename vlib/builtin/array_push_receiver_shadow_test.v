fn a() {}

fn test_array_push_keeps_its_receiver_type_with_a_same_named_function() {
	a()
	mut values := []int{cap: 1}
	values << 1
	values << 2
	assert values == [1, 2]
	// Lower only the header length: ordinary shrinking would detach the buffer.
	mut slice := unsafe { values[..] }
	mut header := unsafe { &array(&slice) }
	header.len = 1
	assert slice.flags.has(.is_slice)
	assert slice.cap > slice.len
	slice << 7
	assert slice == [1, 7]
	assert values == [1, 2]
}
