module runtime

fn C.sysconf(name int) i64

// nr_cpus returns the number of virtual CPU cores found on the system.
pub fn nr_cpus() int {
	return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
}

// total_memory returns total physical memory found on the system.
pub fn total_memory() usize {
	$if macos {
		mut hbi := C.host_basic_info{}
		mut memsz := u32(C.HOST_BASIC_INFO_COUNT)
		mut host := C.mach_host_self()
		unsafe {
			C.host_info(host, C.HOST_BASIC_INFO, &int(&hbi), &memsz)
		}
		return usize(hbi.max_mem)
	}
	page_size := usize(C.sysconf(C._SC_PAGESIZE))
	phys_pages := usize(C.sysconf(C._SC_PHYS_PAGES))
	return page_size * phys_pages
}

// free_memory returns free physical memory found on the system.
pub fn free_memory() usize {
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
	$if linux {
		page_size := usize(C.sysconf(C._SC_PAGESIZE))
		av_phys_pages := usize(C.sysconf(C._SC_AVPHYS_PAGES))
		return page_size * av_phys_pages
	}
	return 1
}
