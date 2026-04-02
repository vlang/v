module builtin

// Stub declarations for VGC functions, so that V does not error
// because of the missing definitions in $if vgc ? { } blocks.
// Note: they will NOT be called, since calls to them are wrapped with `$if vgc ? { }`.

fn vgc_malloc(n usize) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_noscan(n usize) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_typed_opts(n usize, ptrmap u64, ptr_words u8, zero_fill bool) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_noscan_opts(n usize, zero_fill bool) voidptr {
	return unsafe { nil }
}

fn vgc_memdup(src voidptr, n isize) voidptr {
	return unsafe { nil }
}

fn vgc_memdup_noscan(src voidptr, n isize) voidptr {
	return unsafe { nil }
}

fn vgc_realloc(old_ptr voidptr, new_size usize) voidptr {
	return unsafe { nil }
}

fn vgc_calloc(n usize) voidptr {
	return unsafe { nil }
}

fn vgc_free(ptr voidptr) {
}

fn vgc_heap_usage() (usize, usize, usize, usize, usize) {
	return 0, 0, 0, 0, 0
}

fn vgc_memory_use() usize {
	return 0
}
