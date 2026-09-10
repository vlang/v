module runtime

struct C.task_basic_info {
	resident_size u64
}

fn C.task_info(C.task_t, i32, &int, &u32) i32

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	mut info := C.task_basic_info{}
	mut count := u32(C.MACH_TASK_BASIC_INFO_COUNT)
	unsafe {
		if C.task_info(C.mach_task_self(), C.TASK_BASIC_INFO, &int(&info), &count) == C.KERN_SUCCESS {
			return info.resident_size
		}
	}
	return 0
}
