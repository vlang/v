module sapp

import sokol.memory

[typedef]
pub struct C.sapp_allocator {
pub mut:
	alloc     memory.FnAllocatorAlloc
	free      memory.FnAllocatorFree
	user_data voidptr
}

[typedef]
pub struct C.sapp_logger {
pub mut:
	log_cb    memory.FnLogCb
	user_data voidptr
}
