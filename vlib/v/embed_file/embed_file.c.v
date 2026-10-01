module embed_file

// join_chunks_raw_alloc reserves `size` bytes that do not come from V's
// allocator, and returns nil if the request cannot be met.
//
// It exists for the one case in join_chunks_buffer that must not go through the
// preallocator; see the comment there for why. The raw allocator is reached from
// here rather than from the plain `.v` file so that the module stays free of `C.`
// symbols outside a `.c.v` one.
@[inline; unsafe]
fn join_chunks_raw_alloc(size int) &u8 {
	return unsafe { &u8(C.malloc(usize(size))) }
}
