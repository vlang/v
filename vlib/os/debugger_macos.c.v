module os

fn C.ptrace(int, u32, voidptr, int) int

[inline]
pub fn debugger_present() bool {
	// check if the parent could trace its process,
	// if not a debugger must be present
	return C.ptrace(C.PT_TRACE_ME, 0, voidptr(1), 0) == -1
}