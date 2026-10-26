module runtime

fn C.sysctlnametomib(name charptr, mib &i32, len &uint) i32

fn free_memory_impl() !uint {
	$if cross ? {
		return error('free_memory: not implemented')
	}
	$if !cross ? {
		$if freebsd {
			page_size := uint(C.sysconf(C._SC_PAGESIZE))
			c_errno_1 := C.errno
			if page_size == uint(-1) {
				return error('free_memory: `C.sysconf()` return error code = ${c_errno_1}')
			}
			mut mib := [4]i32{} // C `int` mib buffer (sysctl name array)
			mut len := uint(4)
			retval_2 := unsafe {
				C.sysctlnametomib(c'vm.stats.vm.v_free_count', &mib[0], &len)
			}
			c_errno_2 := C.errno
			if retval_2 == -1 {
				return error('free_memory: `C.sysctlnametomib()` return error code = ${c_errno_2}')
			}
			mut free_pages := i32(0) // sysctl writes a 4-byte C int (bufsize 4)
			bufsize := uint(4)
			retval_3 := unsafe {
				C.sysctl(&mib[0], mib.len, &free_pages, &bufsize, 0, 0)
			}
			c_errno_3 := C.errno
			if retval_3 == -1 {
				return error('free_memory: `C.sysctl()` return error code = ${c_errno_3}')
			}
			return page_size * uint(free_pages)
		}
	}
	return error('free_memory: not implemented')
}
