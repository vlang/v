module sgl

import sokol.memory

@[typedef]
pub struct C.sgl_allocator_t {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

@[typedef]
pub struct C.sgl_logger_t {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}
