module gfx

import sokol.memory

[typedef]
pub struct C.sg_allocator {
pub mut:
	alloc     memory.FnAllocatorAlloc
	free      memory.FnAllocatorFree
	user_data voidptr
}

[typedef]
pub struct C.sg_logger {
pub mut:
	log_cb    memory.FnLogCb
	user_data voidptr
}
