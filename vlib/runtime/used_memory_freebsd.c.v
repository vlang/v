module runtime

import os

$if tinyc {
	#include <sys/resource.h>
}
struct C.rusage {
	ru_maxrss int
	ru_idrss  int
}

fn C.getrusage(who int, usage &C.rusage) int

$if !tinyc {
	#flag -lprocstat

	#include <sys/user.h>
	#include <libprocstat.h>
}
struct C.procstat {}

struct C.kinfo_proc {
	ki_rssize u64
}

fn C.procstat_open_sysctl() &C.procstat
fn C.procstat_close(&C.procstat)
fn C.procstat_getprocs(&C.procstat, int, int, &u32) &C.kinfo_proc

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
		mut proc_status := C.procstat_open_sysctl()
		defer {
			C.procstat_close(proc_status)
		}

		mut count := u32(0)

		kip := C.procstat_getprocs(proc_status, C.KERN_PROC_PID | C.KERN_PROC_INC_THREAD,
			os.getpid(), &count)

		if kip != 0 {
			return u64(kip.ki_rssize * page_size)
		}
	}
	return 0
}
