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
