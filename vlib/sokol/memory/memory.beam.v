// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
module memory

// These provide placeholder type definitions and no-op allocator functions for the BEAM backend.
// The BEAM VM has its own memory management (garbage collection per process).

pub type FnAllocatorAlloc = fn (size usize, user_data voidptr) voidptr

pub type FnAllocatorFree = fn (ptr voidptr, user_data voidptr)

pub type FnLogCb = fn (const_tag &char, log_level u32, log_item_id u32, const_message_or_null &char, line_nr u32, const_filename_or_null &char, user_data voidptr)

pub fn salloc(size usize, user_data voidptr) voidptr {
	return unsafe { nil }
}

pub fn sfree(ptr voidptr, user_data voidptr) {
}

pub fn slog(const_tag &char, log_level u32, log_item_id u32, const_message_or_null &char, line_nr u32,
	const_filename_or_null &char, user_data voidptr) {
}
