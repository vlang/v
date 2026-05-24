module runtime

#define PSAPI_VERSION 2
#include <psapi.h>

// Windows 7+ exports K32GetProcessMemoryInfo from kernel32.dll, so avoid
// depending on psapi.lib during bootstrap builds.
@[typedef]
struct C.PROCESS_MEMORY_COUNTERS {
	cb                         u32
	PageFaultCount             u32
	PeakWorkingSetSize         usize
	WorkingSetSize             usize
	QuotaPeakPagedPoolUsage    usize
	QuotaPagedPoolUsage        usize
	QuotaPeakNonPagedPoolUsage usize
	QuotaNonPagedPoolUsage     usize
	PagefileUsage              usize
	PeakPagefileUsage          usize
}

fn C.K32GetProcessMemoryInfo(voidptr, &C.PROCESS_MEMORY_COUNTERS, u32) bool

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	mut pmc := C.PROCESS_MEMORY_COUNTERS{}
	pmc.cb = u32(sizeof(pmc))
	if C.K32GetProcessMemoryInfo(C.GetCurrentProcess(), &pmc, pmc.cb) {
		return u64(pmc.WorkingSetSize)
	}
	return 0
}
