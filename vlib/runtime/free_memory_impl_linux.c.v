module runtime

fn free_memory_impl() !uint {
	$if cross ? {
		return error('free_memory: not implemented')
	}
	$if !cross ? {
		$if linux {
			page_size := uint(C.sysconf(C._SC_PAGESIZE))
			c_errno_1 := C.errno
			if page_size == uint(-1) {
				return error('free_memory: `C.sysconf(C._SC_PAGESIZE)` return error code = ${c_errno_1}')
			}
			av_phys_pages := uint(C.sysconf(C._SC_AVPHYS_PAGES))
			c_errno_2 := C.errno
			if av_phys_pages == uint(-1) {
				return error('free_memory: `C.sysconf(C._SC_AVPHYS_PAGES)` return error code = ${c_errno_2}')
			}
			return page_size * av_phys_pages
		}
	}
	return error('free_memory: not implemented')
}
