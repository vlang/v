module wasm

// cabi_realloc implements the canonical ABI realloc used by component model
// string lowering. It uses V's bump allocator (malloc) and copies the old
// data. free is a no-op in the WASM backend, so this is a bump allocator.
@[export: 'cabi_realloc']
pub fn cabi_realloc(ptr voidptr, old_size int, align int, new_size int) voidptr {
	if new_size <= 0 {
		return unsafe { nil }
	}
	mut new_ptr := unsafe { malloc(new_size) }
	if ptr != unsafe { nil } && old_size > 0 {
		copy_size := if old_size < new_size { old_size } else { new_size }
		unsafe {
			vmemcpy(new_ptr, ptr, copy_size)
		}
	}
	_ = align
	return new_ptr
}