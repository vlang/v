module runtime

fn free_memory_impl() usize {
	page_size := usize(C.sysconf(C._SC_PAGESIZE))
	av_phys_pages := usize(C.sysconf(C._SC_AVPHYS_PAGES))
	return page_size * av_phys_pages
}
