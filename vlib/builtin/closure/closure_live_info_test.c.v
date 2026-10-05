// vtest vflags: -enable-globals
module closure

fn test_closure_live_info_has_no_gc_scanned_padding() {
	// Every byte copied into the live map must belong to an initialized field.
	assert sizeof(ClosureLiveInfo) == 2 * sizeof(voidptr) + 2 * sizeof(u64)
	assert __offsetof(ClosureLiveInfo, ctx) == 0
	assert __offsetof(ClosureLiveInfo, drop_data) == sizeof(voidptr)
	assert __offsetof(ClosureLiveInfo, owns_data) == 2 * sizeof(voidptr)
	assert __offsetof(ClosureLiveInfo, generation) == 2 * sizeof(voidptr) + sizeof(u64)
}

fn test_closure_live_info_preserves_ownership_flags() {
	closure_ensure_initialized()
	closure_mtx_lock_platform()
	defer {
		closure_mtx_unlock_platform()
	}
	mut key := 0
	exec_ptr := voidptr(&key)
	closure_live_set(exec_ptr, unsafe { nil }, true, unsafe { nil })
	owned := closure_live_delete(exec_ptr)
	assert owned.owns_data == 1
	assert owned.generation != 0
	closure_live_set(exec_ptr, unsafe { nil }, false, unsafe { nil })
	borrowed := closure_live_delete(exec_ptr)
	assert borrowed.owns_data == 0
	assert borrowed.generation > owned.generation
	assert exec_ptr !in g_closure.live
}
