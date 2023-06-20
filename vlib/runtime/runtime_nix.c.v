module runtime

fn C.sysconf(name int) i64

// nr_cpus returns the number of virtual CPU cores found on the system.
pub fn nr_cpus() int {
	return int(C.sysconf(C._SC_NPROCESSORS_ONLN))
}

// physical_memory returns total/free physical memory found on the system.
pub fn physical_memory() (usize, usize) {
	page_size := usize(C.sysconf(C._SC_PAGESIZE))
	phys_pages := usize(C.sysconf(C._SC_PHYS_PAGES))
	av_phys_pages := usize(C.sysconf(C._SC_AVPHYS_PAGES))
	return page_size * phys_pages, page_size * av_phys_pages
}
