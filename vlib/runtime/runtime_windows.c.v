module runtime

import os

@[typedef]
pub struct C.MEMORYSTATUS {
	dwTotalPhys uint
	dwAvailPhys uint
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
pub fn total_memory() !uint {
	memory_status := C.MEMORYSTATUS{}
	C.GlobalMemoryStatus(&memory_status)
	return memory_status.dwTotalPhys
}

// free_memory returns free physical memory found on the system.
pub fn free_memory() !uint {
	memory_status := C.MEMORYSTATUS{}
	C.GlobalMemoryStatus(&memory_status)
	return memory_status.dwAvailPhys
}
