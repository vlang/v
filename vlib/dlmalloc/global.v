[has_globals]
module dlmalloc

__global global = new(get_system_allocator())

/// malloc allocates `size` bytes.
///
/// Returns a null pointer if allocation fails. Returns a valid pointer
/// otherwise.
[unsafe]
pub fn malloc(size usize) voidptr {
	unsafe {
		return global.malloc(size)
	}
}

// free deallocates a `ptr`.
[unsafe]
pub fn free(ptr voidptr) {
	unsafe {
		global.free_(ptr)
	}
}

// Same as `malloc`, except if the allocation succeeds it's guaranteed to
// point to `size` bytes of zeros.
[unsafe]
pub fn calloc(size usize) voidptr {
	unsafe {
		return global.calloc(size)
	}
}

// realloc reallocates `ptr`, a previous allocation with `old_size` and
// to have `new_size`.
//
//
// Returns a null pointer if the memory couldn't be reallocated, but `ptr`
// is still valid. Returns a valid pointer and frees `ptr` if the request
// is satisfied.
[unsafe]
pub fn realloc(ptr voidptr, oldsize usize, newsize usize) voidptr {
	unsafe {
		_ := oldsize

		return global.realloc(ptr, newsize)
	}
}

// memalign allocates `size` bytes with `align` align.
//
//
// Returns a null pointer if allocation fails. Returns a valid pointer otherwise.
[unsafe]
pub fn memalign(size usize, align usize) voidptr {
	unsafe {
		if align <= malloc_alignment {
			return global.malloc(size)
		} else {
			return global.memalign(align, size)
		}
	}
}
