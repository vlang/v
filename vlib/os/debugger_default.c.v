module os

// debugger_present returns a bool indicating if the process is being debugged.
// Note: implementation available only on Darwin, FreeBSD, Linux, OpenBSD and
// Windows. Otherwise, returns false.
@[inline]
pub fn debugger_present() bool {
	return false
}
