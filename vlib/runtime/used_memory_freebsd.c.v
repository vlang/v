module runtime

import os

#flag -lprocstat

#include <sys/user.h>
#include <libprocstat.h>

struct C.procstat {}

struct C.kinfo_proc {
	ki_rssize u64
}

fn C.procstat_open_sysctl() &C.procstat
fn C.procstat_close(&C.procstat)
fn C.procstat_getprocs(&C.procstat, int, int, &u32) &C.kinfo_proc

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	mut proc_status := C.procstat_open_sysctl()
	defer {
		C.procstat_close(proc_status)
	}

	mut count := u32(0)

	kip := C.procstat_getprocs(proc_status, C.KERN_PROC_PID | C.KERN_PROC_INC_THREAD,
		os.getpid(), &count)

	if kip != 0 {
		return u64(kip.ki_rssize)
	}

	return 0
}
