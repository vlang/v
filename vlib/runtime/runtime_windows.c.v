module runtime

import os

[typedef]
struct C.SYSTEM_INFO {
	dwNumberOfProcessors u32
}

[typedef]
struct C.MEMORYSTATUS {
	dwTotalPhys usize
	dwAvailPhys usize
}

fn C.GetSystemInfo(&C.SYSTEM_INFO)
fn C.GlobalMemoryStatus(&C.MEMORYSTATUS)

// nr_cpus returns the number of virtual CPU cores found on the system.
pub fn nr_cpus() int {
	sinfo := C.SYSTEM_INFO{}
	C.GetSystemInfo(&sinfo)
	mut nr := int(sinfo.dwNumberOfProcessors)
	if nr == 0 {
		nr = os.getenv('NUMBER_OF_PROCESSORS').int()
	}
	return nr
}

// physical_memory returns total/free physical memory found on the system.
pub fn physical_memory() (usize, usize) {
	memory_status := C.MEMORYSTATUS{}
	C.GlobalMemoryStatus(&memory_status)
	return memory_status.dwTotalPhys, memory_status.dwAvailPhys
}
