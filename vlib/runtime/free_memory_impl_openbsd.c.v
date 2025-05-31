module runtime

#include <sys/sysctl.h>
#include <uvm/uvmexp.h>

struct C.uvmexp {
	pagesize int
	free     int
}

fn free_memory_impl() usize {
	$if cross ? {
		return 1
	}
	$if !cross ? {
		$if openbsd {
			mib := [C.CTL_VM, C.VM_UVMEXP]!
			mut uvm := C.uvmexp{0, 0}
			mut len := sizeof(C.uvmexp)
			unsafe { C.sysctl(&mib[0], mib.len, &uvm, &len, C.NULL, 0) }
			return usize(uvm.pagesize) * usize(uvm.free)
		}
	}
	return 1
}
