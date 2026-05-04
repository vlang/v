module runtime

// Windows 7+ exports K32GetProcessMemoryInfo from kernel32.dll, so avoid
// depending on psapi.h/psapi.lib during bootstrap builds.
@[typedef]
struct C.V_PROCESS_MEMORY_COUNTERS {
	cb                              u32
	page_fault_count                u32
	peak_working_set_size           usize
	working_set_size                usize
	quota_peak_paged_pool_usage     usize
	quota_paged_pool_usage          usize
	quota_peak_non_paged_pool_usage usize
	quota_non_paged_pool_usage      usize
	pagefile_usage                  usize
	peak_pagefile_usage             usize
}

fn C.K32GetProcessMemoryInfo(voidptr, &C.V_PROCESS_MEMORY_COUNTERS, u32) bool

// used_memory retrieves the current physical memory usage of the process.
pub fn used_memory() !u64 {
	mut pmc := C.V_PROCESS_MEMORY_COUNTERS{}
	pmc.cb = u32(sizeof(pmc))
	if C.K32GetProcessMemoryInfo(C.GetCurrentProcess(), &pmc, pmc.cb) {
		return u64(pmc.working_set_size)
	}
	return 0
}
