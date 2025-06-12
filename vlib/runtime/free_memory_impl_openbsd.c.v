module runtime

#include <sys/sysctl.h>
#include <uvm/uvmexp.h>

struct C.uvmexp {
	pagesize int
	free     int
}

fn free_memory_impl() !usize {
	$if cross ? {
		return error('free_memory: not implemented')
	}
	$if !cross ? {
		$if openbsd {
			mib := [C.CTL_VM, C.VM_UVMEXP]!
			mut uvm := C.uvmexp{0, 0}
			mut len := usize(sizeof(C.uvmexp))
			retval := unsafe { C.sysctl(&mib[0], mib.len, &uvm, &len, C.NULL, 0) }
			c_errno := C.errno
			if retval == -1 {
				return error('free_memory: `C.sysctl()` return error code = ${c_errno}')
			}
			return usize(uvm.pagesize) * usize(uvm.free)
		}
	}
	return error('free_memory: not implemented')
}
