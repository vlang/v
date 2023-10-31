module os

#include <sys/ptrace.h>

fn C.ptrace(int, u32, voidptr, int) int

// debugger_present returns a bool indicating if the process is being debugged
[inline]
pub fn debugger_present() bool {
	// check if the parent could trace its process,
	// if not a debugger must be present
	$if freebsd {
		return C.ptrace(C.PT_TRACE_ME, 0, voidptr(1), 0) == -1
	}
	return false
}
