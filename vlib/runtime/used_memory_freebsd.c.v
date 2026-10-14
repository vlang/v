module runtime

$if tinyc {
	#include <sys/resource.h>
}

struct C.rusage {
	ru_maxrss int
	ru_idrss  int
}

fn C.getrusage(who i32, usage &C.rusage) i32

$if !tinyc {
	#flag -lprocstat
	#flag @VEXEROOT/vlib/runtime/used_memory_freebsd.o
}

fn C.v_runtime_freebsd_resident_pages() u64

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	page_size := usize(C.sysconf(C._SC_PAGESIZE))
	c_errno_1 := C.errno
	if page_size == usize(-1) {
		return error('used_memory: C.sysconf() return error code = ${c_errno_1}')
	}
	$if tinyc {
		mut usage := C.rusage{}
		x := C.getrusage(0, &usage)
		if x == -1 {
			c_errno_2 := C.errno
			return error('used_memory: C.getrusage() return error code = ${c_errno_2}')
		}
		return u64(int_max(1, usage.ru_maxrss)) * 1024
	} $else {
		return C.v_runtime_freebsd_resident_pages() * u64(page_size)
	}
}
