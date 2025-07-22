module runtime

import os

@[typedef]
pub struct C.MEMORYSTATUS {
	dwTotalPhys usize
	dwAvailPhys usize
}

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

// total_memory returns total physical memory found on the system.
pub fn total_memory() !usize {
	memory_status := C.MEMORYSTATUS{}
	C.GlobalMemoryStatus(&memory_status)
	return memory_status.dwTotalPhys
}

// free_memory returns free physical memory found on the system.
pub fn free_memory() !usize {
	memory_status := C.MEMORYSTATUS{}
	C.GlobalMemoryStatus(&memory_status)
	return memory_status.dwAvailPhys
}
