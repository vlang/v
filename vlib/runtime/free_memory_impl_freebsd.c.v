module runtime

fn C.sysctlnametomib(name charptr, mib &int, len &usize) int

fn free_memory_impl() usize {
	$if cross ? {
		return 1
	}
	$if !cross ? {
		$if freebsd {
			page_size := usize(C.sysconf(C._SC_PAGESIZE))
			mut mib := [4]int{}
			mut len := usize(4)
			unsafe { C.sysctlnametomib(c'vm.stats.vm.v_free_count', &mib[0], &len) }
			mut free_pages := int(0)
			bufsize := usize(4)
			unsafe { C.sysctl(&mib[0], mib.len, &free_pages, &bufsize, 0, 0) }
			return page_size * usize(free_pages)
		}
	}
	return 1
}
