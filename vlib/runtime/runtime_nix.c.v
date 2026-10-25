module runtime

fn C.sysconf(name i32) i64

// nr_cpus returns the number of virtual CPU cores found on the system.
pub fn nr_cpus() int {
	mut cpus := int(C.sysconf(C._SC_NPROCESSORS_ONLN))
	if cpus < 1 {
		eprintln('Warning: sysconf(_SC_NPROCESSORS_ONLN) returned -1, returning CPU count as 1')
		cpus = 1
	}
	return cpus
}

// total_memory returns total physical memory found on the system.
pub fn total_memory() !uint {
	page_size := uint(C.sysconf(C._SC_PAGESIZE))
	c_errno_1 := C.errno
	if page_size == uint(-1) {
		return error('total_memory: `C.sysconf(C._SC_PAGESIZE)` return error code = ${c_errno_1}')
	}
	phys_pages := uint(C.sysconf(C._SC_PHYS_PAGES))
	c_errno_2 := C.errno
	if phys_pages == uint(-1) {
		return error('total_memory: `C.sysconf(C._SC_PHYS_PAGES)` return error code = ${c_errno_2}')
	}
	return page_size * phys_pages
}

// free_memory returns free physical memory found on the system.
// Note: implementation available only on Darwin, FreeBSD, Linux, OpenBSD and
// Windows. Otherwise, returns 'free_memory: not implemented'.
pub fn free_memory() !uint {
	return free_memory_impl()!
}
