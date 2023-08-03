module gfx

import sokol.memory

[typedef]
pub struct C.sg_allocator {
pub mut:
	alloc     memory.FnAllocatorAlloc = unsafe { nil }
	free      memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

[typedef]
pub struct C.sg_logger {
pub mut:
	log_cb    memory.FnLogCb = unsafe { nil }
	user_data voidptr
}
