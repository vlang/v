module memory

pub type FnAllocatorAlloc = fn (size usize, user_data voidptr) voidptr

pub type FnAllocatorFree = fn (ptr voidptr, user_data voidptr)

pub type FnLogCb = fn (const_message &char, user_data voidptr)

// salloc - used in the allocator structs, that the SOKOL libraries use, for allocating new memory blocks
pub fn salloc(size usize, user_data voidptr) voidptr {
	res := unsafe { malloc(int(size)) }
	$if trace_sokol_memory ? {
		eprintln('sokol.memory.salloc | user_data: ${user_data:x} | size: ${size:10} | res: ${res:x}')
	}
	$if trace_sokol_memory_salloc_backtrace ? {
		print_backtrace()
	}
	return res
}

// sfree - used in the allocator structs, that the SOKOL libraries use, for freeing memory
pub fn sfree(ptr voidptr, user_data voidptr) {
	$if trace_sokol_memory ? {
		eprintln(' sokol.memory.sfree | user_data: ${user_data:x} |                    ptr: ${ptr:x}')
	}
	$if trace_sokol_memory_sfree_backtrace ? {
		print_backtrace()
	}
	unsafe { free(ptr) }
}

fn C.SOKOL_LOG(const_message &char)

pub fn slog(const_message &char, user_data voidptr) {
	$if trace_sokol_memory ? {
		C.fprintf(C.stderr, c'sokol.memory.slog | user_data: %p, message: %s\n', user_data,
			const_message)
	}
	C.fprintf(C.stderr, c'%s\n', const_message)
	/*
	$if msvc {
		C.fprintf(C.stderr, c'%s\n', const_message)
	} $else {
		$if !prod {
			C.SOKOL_LOG(const_message)
		}
	}
	*/
}
