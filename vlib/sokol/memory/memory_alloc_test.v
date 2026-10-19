module memory

// salloc/sfree are plain malloc/free wrappers, so they can be exercised without
// any sokol library being initialised.
fn test_salloc_returns_a_writable_block_of_the_requested_size() {
	p := salloc(64, unsafe { nil })
	assert p != unsafe { nil }
	mut bp := unsafe { &u8(p) }
	unsafe {
		*bp = 42
		assert *bp == 42
	}
	sfree(p, unsafe { nil })
}

fn test_salloc_gives_distinct_blocks() {
	a := salloc(16, unsafe { nil })
	b := salloc(16, unsafe { nil })
	assert a != unsafe { nil }
	assert b != unsafe { nil }
	assert a != b
	sfree(a, unsafe { nil })
	sfree(b, unsafe { nil })
}

fn test_a_zero_sized_block_is_still_freeable() {
	p := salloc(0, unsafe { nil })
	// whether malloc(0) hands back a pointer is up to the allocator, but freeing
	// whatever it returned must not be a problem
	sfree(p, unsafe { nil })
}
