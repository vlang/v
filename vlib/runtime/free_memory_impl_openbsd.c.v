module runtime

#flag @VEXEROOT/vlib/runtime/free_memory_openbsd.o

fn C.v_runtime_openbsd_free_memory(&uint) int

fn free_memory_impl() !uint {
	$if cross ? {
		return error('free_memory: not implemented')
	}
	$if !cross ? {
		$if openbsd {
			mut free_bytes := uint(0)
			retval := C.v_runtime_openbsd_free_memory(&free_bytes)
			c_errno := C.errno
			if retval == -1 {
				return error('free_memory: `C.sysctl()` return error code = ${c_errno}')
			}
			return free_bytes
		}
	}
	return error('free_memory: not implemented')
}
