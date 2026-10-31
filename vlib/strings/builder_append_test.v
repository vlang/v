import strings

fn test_builder_append_copy_boundaries() {
	for initial_cap in [0, 4096] {
		mut b := strings.new_builder(initial_cap)
		mut expected := []u8{}
		for size in [0, 1, 2, 3, 4, 7, 8, 15, 16, 31, 32, 63, 64, 65] {
			mut payload := []u8{cap: size}
			for i in 0 .. size {
				payload << u8(i)
			}
			b.write_string(payload.bytestr())
			expected << payload
			unsafe { b.write_ptr(payload.data, payload.len) }
			expected << payload
		}
		assert b.str().bytes() == expected
	}
}

fn test_builder_append_exact_capacity_then_grows() {
	mut b := strings.new_builder(6)
	b.write_string('abc')
	unsafe { b.write_ptr(c'def', 3) }
	assert b.len == 6
	assert b.cap == 6
	b.write_string('g')
	assert b.cap >= 7
	assert b.str() == 'abcdefg'
}

fn test_builder_append_borrows_own_buffer() {
	for initial_cap in [6, 32] {
		mut b := strings.new_builder(initial_cap)
		b.write_string('abcdef')
		// Borrow the live bytes so both the in-capacity and growth paths append themselves.
		borrowed := unsafe { tos(&u8(b.data), b.len) }
		b.write_string(borrowed)
		assert b.len == 12
		b.ensure_cap(32)
		unsafe { b.write_ptr(&u8(b.data) + 2, 4) }
		assert b.str() == 'abcdefabcdefcdef'
	}
}

fn test_builder_append_overlapping_unused_capacity() {
	mut b := strings.new_builder(32)
	b.write_string('abcdefgh')
	// The source still points into initialized storage after reducing the live length.
	unsafe { b.len = 3 }
	unsafe { b.write_ptr(&u8(b.data) + 1, 5) }
	assert b.str() == 'abcbcdef'
}

fn test_builder_append_detaches_a_slice() {
	for use_ptr in [false, true] {
		mut backing := 'abcdef'.bytes()
		mut b := strings.Builder(backing[1..5])
		// A slice with spare capacity must detach before an append writes to shared storage.
		unsafe { b.len = 2 }
		if use_ptr {
			unsafe { b.write_ptr(c'XY', 2) }
		} else {
			b.write_string('XY')
		}
		assert backing.bytestr() == 'abcdef'
		assert b.str() == 'bcXY'
	}
}

fn test_builder_append_ignores_empty_or_nil_input() {
	mut b := strings.new_builder(8)
	b.write_string('a')
	b.write_string('')
	unsafe {
		b.write_ptr(nil, 0)
		b.write_ptr(nil, 3)
		b.write_ptr(c'skip', -1)
	}
	assert b.str() == 'a'
}
