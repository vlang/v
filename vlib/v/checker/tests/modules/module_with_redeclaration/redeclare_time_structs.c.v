module module_with_redeclaration

import sokol.memory

@[typedef]
pub struct C.saudio_allocator {
pub mut:
	alloc     memory.FnAllocatorAlloc
	free      memory.FnAllocatorFree
	user_data voidptr
}

@[typedef]
pub struct C.saudio_logger {
pub mut:
	log_cb    memory.FnLogCb
	user_data voidptr
}

pub struct C.saudio_desc {
pub mut:
	user_data voidptr
	allocator C.saudio_allocator
	logger    C.saudio_logger
}
