module strings

fn assert_buffer_string(got string, expected []u8) {
	assert got.len == expected.len
	for i, byte in expected {
		assert got[i] == byte, 'byte ${i}'
	}
	assert unsafe { got.str[got.len] } == 0
}

fn test_buffer_byte_growth_and_terminator_at_capacity_boundaries() {
	for capacity in [0, 1, 2, 3, 4, 7, 8, 15, 16, 31, 32, 63, 64, 65, 127, 128, 129, 1024] {
		for length in [0, capacity, capacity + 1, capacity * 2 + 17] {
			mut b := new_buffer(capacity)
			mut expected := []u8{len: length}
			for i in 0 .. length {
				byte := u8(i % 256)
				expected[i] = byte
				b.write_u8(byte)
			}
			assert b.len() == length
			assert b.capacity() >= length
			got := b.take_string()
			assert_buffer_string(got, expected)
			assert b.len() == 0
			assert b.capacity() == 0
			unsafe { got.free() }
		}
	}
}

fn test_buffer_transfer_uses_the_original_base_allocation() {
	for capacity in [1, 16, 64, 1024] {
		for text in ['', 'a', 'a'.repeat(capacity), 'b'.repeat(capacity + 1)] {
			mut b := new_buffer(capacity)
			b.write_string(text)
			base := b.data
			allocated := b.cap
			got := b.take_string()
			assert got.str == base
			assert allocated >= got.len
			assert_buffer_string(got, text.bytes())
			assert b.data == unsafe { nil }
			b.write_string('different')
			assert got == text
			unsafe {
				b.free()
				got.free()
			}
		}
	}
}

fn test_buffer_bulk_growth_and_independent_transfers() {
	mut b := new_buffer(0)
	first_text := 'λ😀\x00\r\n'.repeat(100)
	b.write_string(first_text)
	first := b.take_string()
	second_text := 'other\x00text'.repeat(5000)
	b.write_string(second_text)
	second := b.take_string()
	gc_collect()
	assert_buffer_string(first, first_text.bytes())
	assert_buffer_string(second, second_text.bytes())
	unsafe { second.free() }
	b.write_string('third')
	third := b.take_string()
	assert third == 'third'
	assert_buffer_string(first, first_text.bytes())
	unsafe {
		first.free()
		third.free()
		b.free()
	}
	empty := b.take_string()
	assert empty == ''
}

fn test_buffer_default_value_empty_writes_and_repeated_transfers() {
	mut b := Buffer{}
	assert b.len() == 0
	assert b.capacity() == 0
	b.write_string('')
	written := b.write([]u8{}) or { panic(err) }
	assert written == 0
	unsafe { b.write_ptr(nil, 0) }
	b.reserve(0)
	assert b.data == unsafe { nil }
	first_empty := b.take_string()
	second_empty := b.take_string()
	assert first_empty == ''
	assert second_empty == ''
	b.write_string('usable')
	usable := b.take_string()
	assert usable == 'usable'
	unsafe {
		usable.free()
		b.free()
		b.free()
	}
}

fn test_buffer_reserve_does_not_change_initialized_bytes() {
	mut b := new_buffer(3)
	b.write_string('abc')
	b.reserve(1000)
	assert b.len() == 3
	assert b.capacity() >= 1003
	base := b.data
	b.reserve(0)
	assert b.data == base
	b.reserve(1)
	assert b.data == base
	got := b.take_string()
	assert got == 'abc'
	unsafe { got.free() }
}

fn test_buffer_mixed_byte_string_slice_and_pointer_writes() {
	for capacity in [0, 1, 8, 4096] {
		mut b := new_buffer(capacity)
		mut expected := []u8{}
		for size in [0, 1, 2, 3, 4, 7, 8, 15, 16, 31, 32, 63, 64, 65] {
			payload := []u8{len: size, init: u8(index % 256)}
			b.write_u8(0xff)
			expected << u8(0xff)
			b.write_string(payload.bytestr())
			expected << payload
			written := b.write(payload) or { panic(err) }
			assert written == size
			expected << payload
			unsafe { b.write_ptr(payload.data, payload.len) }
			expected << payload
		}
		got := b.take_string()
		assert_buffer_string(got, expected)
		unsafe { got.free() }
	}
}

fn test_buffer_set_byte_and_unchecked_set_byte() {
	mut b := new_buffer(5)
	b.write_string('ABCDE')
	b.set_byte(0, `a`)
	b.set_byte(b.len() - 1, `e`)
	unsafe { b.set_byte_unchecked(2, 0) }
	assert b.len() == 5
	got := b.take_string()
	assert got == 'aB\x00De'
	unsafe { got.free() }
}

fn test_buffer_free_keeps_transferred_strings_and_allows_reuse() {
	mut b := new_buffer(16)
	b.write_string('first')
	first := b.take_string()
	b.write_string('discard'.repeat(1000))
	unsafe { b.free() }
	assert b.len() == 0
	assert b.capacity() == 0
	assert b.data == unsafe { nil }
	assert first == 'first'
	b.write_string('second')
	second := b.take_string()
	unsafe { b.free() }
	gc_collect()
	assert first == 'first'
	assert second == 'second'
	unsafe {
		first.free()
		second.free()
	}
}
