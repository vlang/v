module runtime

#include <mach/mach.h>

[typedef]
struct C.vm_size_t {
}

struct C.host_basic_info {
	max_mem u64
}

[typedef]
struct C.vm_statistics64_data_t {
	free_count          u32
	purgeable_count     u32
	speculative_count   u32
	external_page_count u32
}

fn C.mach_host_self() C.host_t
fn C.host_info(host C.host_t, flavor int, host_info_out &int, host_info_outCnt &u32) int
fn C.host_statistics64(host C.host_t, flavor int, host_info_out &int, host_info_outCnt &u32) int
fn C.host_page_size(host C.host_t, out_page_size &C.vm_size_t) int

fn total_memory_macos() usize {
	mut hbi := C.host_basic_info{}
	mut memsz := u32(C.HOST_BASIC_INFO_COUNT)
	mut host := C.mach_host_self()
	unsafe {
		C.host_info(host, C.HOST_BASIC_INFO, &int(&hbi), &memsz)
	}

	return usize(hbi.max_mem)
}

fn free_memory_macos() usize {
	mut hs := C.vm_statistics64_data_t{}
	mut vmsz := u32(C.HOST_VM_INFO64_COUNT)
	mut hps := u32(0)
	mut host := C.mach_host_self()
	unsafe {
		C.host_statistics64(host, C.HOST_VM_INFO64, &int(&hs), &vmsz)
		C.host_page_size(host, &C.vm_size_t(&hps))
	}
	return usize(u64(hs.free_count) * u64(hps))
}
