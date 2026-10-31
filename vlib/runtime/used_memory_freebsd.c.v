module runtime

$if tinyc && !v3_c_extern_declarations ? {
	#include <sys/resource.h>
}

struct C.rusage {
	ru_maxrss int
	ru_idrss  int
}

fn C.getrusage(who i32, usage &C.rusage) i32

const sysctl_ctl_kern = i32(1)
const sysctl_kern_proc = i32(14)
const sysctl_kern_proc_rlimit_usage = i32(46)
const rlimit_rss = i32(5)

// used_memory retrieves the current physical memory usage of the process.
// TinyCC and kernels without kern.proc.rlimit_usage report peak resident memory.
pub fn used_memory() !u64 {
	page_size := uint(C.sysconf(C._SC_PAGESIZE))
	c_errno_1 := C.errno
	if page_size == uint(-1) {
		return error('used_memory: C.sysconf() return error code = ${c_errno_1}')
	}
	$if tinyc {
		return used_memory_rusage()
	} $else {
		mib := [sysctl_ctl_kern, sysctl_kern_proc, sysctl_kern_proc_rlimit_usage, C.getpid(),
			rlimit_rss]!
		mut bytes := u64(0)
		mut length := uint(sizeof(u64))
		result := unsafe { C.sysctl(&mib[0], mib.len, &bytes, &length, nil, 0) }
		c_errno_2 := C.errno
		if result == 0 && length == sizeof(u64) && bytes != u64(-1) {
			return bytes
		}
		// FreeBSD 13 does not have the scalar RSS selector.
		if result == -1 && c_errno_2 == C.ENOENT {
			return used_memory_rusage() or { 0 }
		}
		return 0
	}
}

fn used_memory_rusage() !u64 {
	mut usage := C.rusage{}
	x := C.getrusage(0, &usage)
	if x == -1 {
		c_errno := C.errno
		return error('used_memory: C.getrusage() return error code = ${c_errno}')
	}
	return u64(int_max(1, usage.ru_maxrss)) * 1024
}
