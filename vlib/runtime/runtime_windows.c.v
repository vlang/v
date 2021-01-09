module runtime

import os

[typedef]
struct C.SYSTEM_INFO {
    dwNumberOfProcessors u32
}
fn C.GetSystemInfo(&C.SYSTEM_INFO)

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
