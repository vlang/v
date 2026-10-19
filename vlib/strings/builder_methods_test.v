import strings

fn test_builder_byte_at_reads_the_buffer_at_an_index() {
	mut b := strings.new_builder(0)
	b.write_string('hello world')
	assert b.len == 11
	assert b.byte_at(0) == `h`
	assert b.byte_at(4) == `o`
	assert b.byte_at(b.len - 1) == `d`
	// The buffer is only a byte array, so an index past the end panics rather
	// than returning a zero byte.
}

fn test_builder_spart_returns_a_slice_of_the_buffer() {
	mut b := strings.new_builder(0)
	b.write_string('hello world')
	assert b.spart(0, 5) == 'hello'
	assert b.spart(6, 5) == 'world'
	assert b.spart(11, 0) == ''
}

fn test_builder_last_n_returns_the_trailing_bytes() {
	mut b := strings.new_builder(0)
	b.write_string('hello world')
	assert b.last_n(11) == 'hello world'
	assert b.last_n(5) == 'world'
	assert b.last_n(0) == ''
	// Asking for more than the buffer holds yields nothing, not a panic.
	assert b.last_n(99) == ''
}

fn test_builder_after_returns_the_bytes_from_a_position() {
	mut b := strings.new_builder(0)
	b.write_string('hello world')
	assert b.after(0) == 'hello world'
	assert b.after(6) == 'world'
	// A position at or past the end of the buffer yields nothing.
	assert b.after(11) == ''
	assert b.after(99) == ''
}

fn test_builder_write_string2_appends_both_arguments_in_order() {
	mut b := strings.new_builder(0)
	b.write_string('|')
	b.write_string2('left', 'right')
	assert b.str() == '|leftright'
}

fn test_builder_write_string2_skips_empty_arguments() {
	mut b := strings.new_builder(0)
	b.write_string2('', 'only-second')
	b.write_string2('only-first', '')
	assert b.str() == 'only-secondonly-first'
}

fn test_builder_go_back_discards_the_last_bytes() {
	mut b := strings.new_builder(0)
	b.write_string('abcdef')
	b.go_back(2)
	assert b.len == 4
	assert b.str() == 'abcd'
	// str() leaves the builder empty, so it can be filled again from scratch.
	b.write_string('again')
	assert b.str() == 'again'
}

fn test_builder_go_back_can_discard_the_whole_buffer() {
	mut b := strings.new_builder(0)
	b.write_string('abcd')
	b.go_back(b.len)
	assert b.len == 0
	b.write_string('xy')
	assert b.str() == 'xy'
}

fn test_builder_go_back_to_resets_the_buffer_to_a_position() {
	mut b := strings.new_builder(0)
	b.write_string('abcdef')
	b.go_back_to(3)
	assert b.len == 3
	assert b.str() == 'abc'

	mut c := strings.new_builder(0)
	c.write_string('abcdef')
	c.go_back_to(0)
	assert c.len == 0
	assert c.str() == ''
}

fn test_builder_writeln2_terminates_both_arguments_with_a_newline() {
	mut b := strings.new_builder(0)
	b.writeln2('one', 'two')
	assert b.str() == 'one\ntwo\n'
}

fn test_builder_write_byte_appends_a_single_byte() {
	mut b := strings.new_builder(0)
	b.write_string('xyz')
	b.write_byte(`!`)
	assert b.str() == 'xyz!'
	assert b.byte_at(3) == `!`
}

fn test_builder_write_ptr_appends_a_byte_range() {
	mut b := strings.new_builder(0)
	b.write_string('>>')
	payload := 'raw bytes'
	unsafe {
		// write_ptr takes a pointer, so the call needs `unsafe`; the block is
		// kept to just this one call.
		b.write_ptr(payload.str, payload.len)
	}
	// A zero length is a no-op, including for a nil pointer.
	unsafe {
		b.write_ptr(unsafe { nil }, 0)
	}
	assert b.len == '>>'.len + payload.len
	assert b.str() == '>>raw bytes'
}

fn test_builder_reuse_as_plain_u8_array_takes_over_the_buffer() {
	mut b := strings.new_builder(0)
	b.write_string('abcd')
	arr := unsafe { b.reuse_as_plain_u8_array() }
	assert arr.len == 4
	assert arr[0] == `a`
	assert arr[3] == `d`
	// The array now owns the buffer the builder allocated, so it has to be
	// released by the caller.
	unsafe {
		arr.free()
	}
}
