module builtin

fn test_managed_array_alignment_does_not_depend_on_allocation_alignment() {
	assert sizeof(ArrayDataHeader) <= usize(array_data_header_size())
	// Model allocators with weaker alignment by testing every possible low nibble.
	allocation := unsafe { malloc(128) }
	for offset in 0 .. 16 {
		raw := unsafe { allocation + offset }
		data := init_array_data(raw)
		assert usize(data) % 16 == 0
		assert usize(data) - usize(raw) >= usize(array_data_header_size())
		assert usize(data) - usize(raw) <= usize(array_data_header_size()) + 15
		a := array{
			data:         data
			len:          1
			cap:          1
			element_size: 16
			flags:        .managed
		}
		header := unsafe { a.data_header() }
		assert header.allocation == raw
		assert !header.has_slices
	}
	unsafe { free(allocation) }
}

fn test_managed_array_header_survives_slice_offsets() {
	mut owner := []u64{len: 8, cap: 16, init: u64(index)}
	header := unsafe { owner.data_header() }
	allocation := header.allocation
	mut view := unsafe { owner[1..5] }
	assert view.offset == int(sizeof(u64))
	assert view.flags.has(.is_slice)
	assert unsafe { owner.data_header() } == header
	assert header.has_slices
	// Freeing a borrowed slice must leave its owner's allocation alive.
	unsafe { view.free() }
	assert owner[1] == 1
	view << u64(99)
	assert view.data != owner.data
	assert usize(view.data) % 16 == 0
	assert unsafe { view.data_header().allocation } != allocation
	assert view == [u64(1), 2, 3, 4, 99]
	assert owner[5] == 5
	unsafe {
		view.free()
		owner.free()
	}
}

fn test_managed_array_offset_and_noslices_growth_free_original_allocation() {
	mut values := []u64{len: 2, cap: 2, init: u64(index)}
	unsafe { values.flags.set(.noslices) }
	header := unsafe { values.data_header() }
	assert values.pop_left() == 0
	assert unsafe { values.data_header() } == header
	for n in 2 .. 128 {
		values << u64(n)
		assert usize(values.data) % 16 == 0
	}
	assert values[0] == 1
	assert values.last() == 127
	unsafe { values.free() }
	assert values.data == unsafe { nil }
	assert values.offset == 0
	assert values.len == 0
}

fn test_noscan_array_allocations_use_the_same_alignment_and_ownership() {
	$if gcboehm_opt ? {
		for initialized in [false, true] {
			data := if initialized {
				alloc_array_data_noscan(32)
			} else {
				alloc_array_data_noscan_uninit(32)
			}
			assert usize(data) % 16 == 0
			mut values := array{
				data:         data
				len:          2
				cap:          2
				element_size: 16
				flags:        .managed | .noscan_data
			}
			header := unsafe { values.data_header() }
			assert header.allocation != unsafe { nil }
			assert !header.has_slices
			values.ensure_cap_noscan(128)
			assert usize(values.data) % 16 == 0
			assert values.flags.has(.noscan_data)
			assert !values.buffer_has_slices()
			unsafe { values.free() }
		}
	}
}
