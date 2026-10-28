fn test_array_text_reads_updates_through_an_unsafe_shared_slice() {
	values := [1, 2]
	// The slice deliberately shares the immutable array's element storage.
	mut view := unsafe { values[..] }
	view[0] = 9
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_split_array_text_reads_updates_through_an_unsafe_shared_slice() {
	values := 'a,b'.split(',')
	// The slice deliberately shares the immutable array's element storage.
	mut view := unsafe { values[..] }
	view[0] = 'z'
	assert values[0] == 'z'
	assert '${values}' == "['z', 'b']"
	assert values.str() == "['z', 'b']"
}

fn test_bool_array_text_reads_updates_through_an_unsafe_shared_slice() {
	values := [true, false]
	// The slice deliberately shares the immutable array's element storage.
	mut view := unsafe { values[..] }
	view[0] = false
	assert !values[0]
	assert '${values}' == '[false, false]'
	assert values.str() == '[false, false]'
}

fn test_array_text_reads_updates_through_a_data_pointer() {
	values := [1, 2]
	unsafe {
		// The pointer deliberately aliases the immutable array's element storage.
		mut data := &int(values.data)
		data[0] = 9
	}
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_a_captured_shared_slice() {
	values := [1, 2]
	mutate := fn [values] () {
		// The captured header retains the original array's element storage.
		mut view := unsafe { values[..] }
		view[0] = 9
	}
	mutate()
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_a_header_pointer() {
	values := [1, 2]
	// The pointer deliberately retains the original array header and storage.
	mut alias := unsafe { &values }
	unsafe {
		(*alias)[0] = 9
	}
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn mutate_array_text_storage(values []int) {
	// The callee deliberately changes storage shared with an immutable argument.
	mut view := unsafe { values[..] }
	view[0] = 9
}

type ArrayTextInts = []int

fn (values ArrayTextInts) mutate_array_text_receiver() {
	mutate_array_text_storage(values)
}

fn test_array_text_reads_updates_through_an_immutable_argument() {
	values := [1, 2]
	mutate_array_text_storage(values)
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_an_alias_argument() {
	values := [1, 2]
	alias := values
	mutate_array_text_storage(alias)
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_a_slice_argument() {
	values := [1, 2]
	mutate_array_text_storage(values[..])
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_an_immutable_receiver() {
	values := [1, 2]
	ArrayTextInts(values).mutate_array_text_receiver()
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}

fn test_array_text_reads_updates_through_a_slice_alias_argument() {
	values := [1, 2]
	view := values[..]
	mutate_array_text_storage(view)
	assert values[0] == 9
	assert '${values}' == '[9, 2]'
	assert values.str() == '[9, 2]'
}
