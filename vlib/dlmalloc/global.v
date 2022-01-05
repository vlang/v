module dlmalloc

const (
	global = new(get_system_allocator())
)

/// malloc allocates `size` bytes.
///
/// Returns a null pointer if allocation fails. Returns a valid pointer
/// otherwise.
[unsafe]
pub fn malloc(size usize) voidptr {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		return glob.malloc(size)
	}
}

// free deallocates a `ptr`.
[unsafe]
pub fn free(ptr voidptr) {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		glob.free_(ptr)
	}
}

// Same as `malloc`, except if the allocation succeeds it's guaranteed to
// point to `size` bytes of zeros.
[unsafe]
pub fn calloc(size usize) voidptr {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		return glob.calloc(size)
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
		mut glob := &Dlmalloc(&dlmalloc.global)
		return glob.realloc(ptr, newsize)
	}
}

// memalign allocates `size` bytes with `align` align.
//
//
// Returns a null pointer if allocation fails. Returns a valid pointer otherwise.
[unsafe]
pub fn memalign(size usize, align usize) voidptr {
	unsafe {
		mut glob := &Dlmalloc(&dlmalloc.global)
		if align <= malloc_alignment {
			return glob.malloc(size)
		} else {
			return glob.memalign(align, size)
		}
	}
}
