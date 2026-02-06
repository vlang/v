module runtime

#flag -lpsapi
#include <psapi.h>

@[typedef]
struct C.PROCESS_MEMORY_COUNTERS {
	cb             u64
	WorkingSetSize isize
}

fn C.GetProcessMemoryInfo(int, &C.PROCESS_MEMORY_COUNTERS, u64) bool

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	mut pmc := C.PROCESS_MEMORY_COUNTERS{}
	pmc.cb = u64(sizeof(pmc))
	if C.GetProcessMemoryInfo(C.GetCurrentProcess(), &pmc, pmc.cb) {
		return u64(pmc.WorkingSetSize)
	}
	return 0
}
