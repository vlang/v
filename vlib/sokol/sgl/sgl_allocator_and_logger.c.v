module sgl

import sokol.memory

[typedef]
pub struct C.sgl_allocator_t {
pub mut:
	alloc     memory.FnAllocatorAlloc = unsafe { nil }
	free      memory.FnAllocatorFree  = unsafe { nil }
	user_data voidptr
}

[typedef]
pub struct C.sgl_logger_t {
pub mut:
	log_cb    memory.FnLogCb = unsafe { nil }
	user_data voidptr
}
