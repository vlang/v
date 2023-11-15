module os

#include <sys/ptrace.h>

fn C.ptrace(u32, u32, voidptr, voidptr) u64

// debugger_present returns a bool indicating if the process is being debugged
@[inline]
pub fn debugger_present() bool {
	// check if the parent could trace its process,
	// if not a debugger must be present
	$if linux {
		return C.ptrace(C.PTRACE_TRACEME, 0, 1, 0) == -1
	}
	return false
}
