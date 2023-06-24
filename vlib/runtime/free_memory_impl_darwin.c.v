module runtime

fn free_memory_impl() usize {
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
