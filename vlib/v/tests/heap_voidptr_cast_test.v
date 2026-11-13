fn heap_voidptr_cast(bytes []u8) &voidptr {
	mut ptr := voidptr(bytes.data)
	// Returning the pointer slot promotes it to the heap. The cast must read
	// its stored pointer, while the return still addresses the slot itself.
	unsafe {
		*(&u8(ptr)) = 21
	}
	return &ptr
}

fn heap_voidptr_paren_cast(bytes []u8) &voidptr {
	mut ptr := voidptr(bytes.data)
	unsafe {
		*(&u8(ptr)) = 42
	}
	return &ptr
}

fn test_pointer_cast_reads_heap_promoted_voidptr() {
	mut bytes := [u8(0), 0, 0, 0]
	cell := heap_voidptr_cast(bytes)
	assert bytes == [u8(21), 0, 0, 0]
	assert unsafe { *cell } == bytes.data
}

fn test_parenthesized_pointer_cast_reads_heap_promoted_voidptr() {
	mut bytes := [u8(0), 0, 0, 0]
	cell := heap_voidptr_paren_cast(bytes)
	assert bytes == [u8(42), 0, 0, 0]
	assert unsafe { *cell } == bytes.data
}
