module runtime

fn free_memory_impl() usize {
	$if cross ? {
		return 1
	}
	$if !cross ? {
		$if freebsd {
			page_size := usize(C.sysconf(C._SC_PAGESIZE))
			// sysctlnametomib("vm.stats.vm.v_free_count") => mib :
			mib := [C.CTL_VM, 2147481600, 2147481598, 2147481557]!
			mut free_pages := int(0)
			bufsize := usize(4)
			unsafe {
				C.sysctl(&mib[0], mib.len, &free_pages, &bufsize, 0, 0)
			}
			return page_size * usize(free_pages)
		}
	}
	return 1
}
