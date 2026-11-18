import strings

// Preserve intentionally borrowed headers under autofree; the tests free them explicitly.
@[manualfree]
fn borrow_transfer_builder(mut target strings.Builder, source []u8) {
	unsafe { vmemcpy(&target, &source, int(sizeof(strings.Builder))) }
}

fn test_transfer_to_empty_target_exchanges_storage() {
	mut source := strings.new_builder(32)
	mut target := strings.new_builder(16)
	source.write_string('a\x00b')
	source_data := source.data
	target_data := target.data
	source.transfer_to(mut target)
	assert source.len == 0
	assert source.data == target_data
	assert source.cap == 16
	assert target.data == source_data
	assert target.cap == 32
	assert target.bytestr() == 'a\x00b'
	source.write_string('next')
	target.write_string('!')
	assert source.str() == 'next'
	assert target.str() == 'a\x00b!'
	unsafe {
		source.free()
		target.free()
	}
}

fn test_transfer_appends_and_reuses_source_capacity() {
	for capacity in [2, 128] {
		mut source := strings.new_builder(32)
		mut target := strings.new_builder(capacity)
		source.write_string('a\x00b')
		target.write_string('xy')
		source_data := source.data
		target_data := target.data
		source.transfer_to(mut target)
		assert source.len == 0
		assert source.data == source_data
		assert source.cap == 32
		if capacity == 128 {
			assert target.data == target_data
		}
		source.write_string('next')
		assert target.str() == 'xya\x00b'
		assert source.str() == 'next'
		unsafe {
			source.free()
			target.free()
		}
	}
}

fn test_transfer_empty_source_and_self_leave_storage_unchanged() {
	mut source := strings.new_builder(32)
	mut target := strings.new_builder(16)
	source_data := source.data
	target_data := target.data
	for text in ['', 'hello'] {
		target.write_string(text)
		source.transfer_to(mut target)
		target.transfer_to(mut target)
		assert source.data == source_data
		assert source.cap == 32
		assert target.data == target_data
		assert target.cap == 16
		assert target.str() == text
	}
	unsafe {
		source.free()
		target.free()
	}
}

// Keep the intentional aliases even when these tests run with autofree.
@[manualfree]
fn test_transfer_alias_with_offset_detaches_before_growth() {
	for capacity in [6, 64] {
		mut target := strings.new_builder(capacity)
		target.write_string('abcdef')
		// Borrow the target header explicitly; drop also changes the data pointer.
		mut source := strings.new_builder(0)
		borrow_transfer_builder(mut source, target)
		source.drop(1)
		source.transfer_to(mut target)
		assert source.len == 0
		assert source.data != target.data
		assert target.flags.has(.noslices)
		source.write_string('new')
		assert target.str() == 'abcdefbcdef'
		assert source.str() == 'new'
		unsafe {
			source.free()
			target.free()
		}
	}
}

@[manualfree]
fn test_transfer_slice_aliases_preserve_other_views() {
	for source_is_slice in [false, true] {
		mut backing := 'abcdef'.bytes()
		mut source := strings.new_builder(0)
		mut target := strings.new_builder(0)
		// Keep both borrowed views even when compiling with autofree.
		borrow_transfer_builder(mut source, if source_is_slice { backing[1..4] } else { backing })
		borrow_transfer_builder(mut target, if source_is_slice { backing } else { backing[1..4] })
		source.transfer_to(mut target)
		assert source.len == 0
		assert target.bytestr() == if source_is_slice { 'abcdefbcd' } else { 'bcdabcdef' }
		source.write_string('new')
		assert backing.bytestr() == 'abcdef'
		unsafe {
			source.free()
			target.free()
			backing.free()
		}
	}
}

@[manualfree]
fn test_transfer_to_empty_slice_preserves_storage_metadata() {
	mut backing := 'other'.bytes()
	mut source := strings.new_builder(32)
	source.write_string('abcdef')
	mut target := strings.new_builder(0)
	// Borrow the empty slice header without autofree detaching it first.
	borrow_transfer_builder(mut target, backing[1..1])
	source_flags := source.flags
	target_flags := target.flags
	source.transfer_to(mut target)
	assert source.len == 0
	assert source.offset == 1
	assert source.flags == target_flags
	assert target.offset == 0
	assert target.flags == source_flags
	source.write_string('new')
	assert target.str() == 'abcdef'
	assert source.str() == 'new'
	unsafe {
		source.free()
		target.free()
		backing.free()
	}
}

@[manualfree]
fn test_transfer_to_empty_alias_does_not_reuse_shared_storage() {
	for use_slice in [false, true] {
		mut source := strings.new_builder(32)
		source.write_string('abcdef')
		mut target := strings.new_builder(0)
		// Borrow the source header or its empty slice, retaining shared storage.
		borrow_transfer_builder(mut target, if use_slice { source[1..1] } else { source })
		if !use_slice {
			target.clear()
		}
		source_data := source.data
		source.transfer_to(mut target)
		assert source.len == 0
		assert source.cap == 0
		assert target.data == source_data
		source.write_string('new')
		assert target.str() == 'abcdef'
		assert source.str() == 'new'
		unsafe {
			source.free()
			target.free()
		}
	}
}
