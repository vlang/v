module runtime

#include <mach/mach.h>

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
fn C.host_info(host C.host_t, flavor int, host_info_out voidptr, host_info_outCnt voidptr) int
fn C.host_page_size(host C.host_t, out_page_size &u32) int
fn C.host_statistics64(host C.host_t, flavor int, host_info_out voidptr, host_info_outCnt voidptr) int

// physical_memory returns total/free physical memory found on the system.
pub fn physical_memory() (usize, usize) {
	mut hbi := C.host_basic_info{}
	mut hs := C.vm_statistics64_data_t{}
	mut memsz := C.HOST_BASIC_INFO_COUNT
	mut vmsz := C.HOST_VM_INFO64_COUNT
	mut hps := u32(0)
	mut host := C.mach_host_self()
	C.host_info(host, C.HOST_BASIC_INFO, &hbi, &memsz)
	C.host_statistics64(host, C.HOST_VM_INFO64, &hs, &vmsz)
	C.host_page_size(host, &hps)

	total := usize(hbi.max_mem)
	free := usize(u64(hs.free_count) * u64(hps))
	return total, free
}
