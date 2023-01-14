module sgl

import sokol.memory

[typedef]
pub struct C.sgl_allocator_t {
pub mut:
	alloc     memory.FnAllocatorAlloc
	free      memory.FnAllocatorFree
	user_data voidptr
}

[typedef]
pub struct C.sgl_logger_t {
pub mut:
	log_cb    memory.FnLogCb
	user_data voidptr
}
