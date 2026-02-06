module runtime

#include <sys/resource.h>

struct C.rusage {
	ru_maxrss int
}

fn C.getrusage(who int, usage &C.rusage) int

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	page_size := usize(C.sysconf(C._SC_PAGESIZE))
	c_errno_1 := C.errno
	if page_size == usize(-1) {
		return error('used_memory: C.sysconf() return error code = ${c_errno_1}')
	}

	mut usage := C.rusage{}
	ret := C.getrusage(C.RUSAGE_SELF, &usage)
	if ret == -1 {
		c_errno_2 := C.errno
		return error('used_memory: C.getrusage() return error code = ${c_errno_2}')
	}
	return u64(int_max(1, usage.ru_maxrss)) * 1024
}
