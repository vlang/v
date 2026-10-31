module runtime

struct C.task_basic_info {
	resident_size u64
}

fn C.task_info(C.task_t, i32, &int, &u32) i32

fn C.getrusage(who i32, usage &C.rusage) i32

struct C.rusage {
	ru_maxrss int
}

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	$if macos || ios {
		mut info := C.task_basic_info{}
		mut count := u32(C.MACH_TASK_BASIC_INFO_COUNT)
		unsafe {
			if C.task_info(C.mach_task_self(), C.TASK_BASIC_INFO, &int(&info), &count) == C.KERN_SUCCESS {
				return info.resident_size
			}
		}
		return 0
	} $else {
		// Portable snapshots generated on Darwin still report memory on their host.
		mut usage := C.rusage{}
		if C.getrusage(0, &usage) == -1 {
			return error('used_memory: C.getrusage() failed')
		}
		return u64(int_max(1, usage.ru_maxrss)) * 1024
	}
}
