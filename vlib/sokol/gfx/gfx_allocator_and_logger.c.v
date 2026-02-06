module gfx

import sokol.memory

@[typedef]
pub struct C.sg_allocator {
pub mut:
	alloc_fn  memory.FnAllocatorAlloc = unsafe { nil }
	free_fn   memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

@[typedef]
pub struct C.sg_logger {
pub mut:
	func      memory.FnLogCb = unsafe { nil }
	user_data voidptr
}
