module builtin

// Stub declarations for VGC functions, so that V does not error
// because of the missing definitions in $if vgc ? { } blocks.
// Note: they will NOT be called, since calls to them are wrapped with `$if vgc ? { }`.

fn vgc_malloc(n uint) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_noscan(n uint) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_typed_opts(n uint, ptrmap u64, ptr_words u8, zero_fill bool) voidptr {
	return unsafe { nil }
}

fn vgc_malloc_noscan_opts(n uint, zero_fill bool) voidptr {
	return unsafe { nil }
}

fn vgc_memdup(src voidptr, n int) voidptr {
	return unsafe { nil }
}

fn vgc_memdup_noscan(src voidptr, n int) voidptr {
	return unsafe { nil }
}

fn vgc_realloc(old_ptr voidptr, new_size uint) voidptr {
	return unsafe { nil }
}

fn vgc_calloc(n uint) voidptr {
	return unsafe { nil }
}

fn vgc_free(ptr voidptr) {
}

fn vgc_heap_usage() (uint, uint, uint, uint, uint) {
	return 0, 0, 0, 0, 0
}

fn vgc_memory_use() uint {
	return 0
}

fn vgc_collect() {
}

fn vgc_enable(on bool) {
}

fn vgc_is_enabled() bool {
	return false
}
