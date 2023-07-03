module runtime

#include <mach/mach.h>

[typedef]
struct C.vm_size_t {
}

[typedef]
struct C.vm_statistics64_data_t {
	free_count          u32
	purgeable_count     u32
	speculative_count   u32
	external_page_count u32
}

fn C.mach_host_self() C.host_t
fn C.host_page_size(host C.host_t, out_page_size &C.vm_size_t) int
fn C.host_statistics64(host C.host_t, flavor int, host_info_out &int, host_info_outCnt &u32) int

fn free_memory_impl() usize {
	$if macos {
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
	return 1
}
