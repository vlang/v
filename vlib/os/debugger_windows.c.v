module os

// this is defined in builtin_windows.c.v in builtin
// fn C.IsDebuggerPresent() bool

// debugger_present returns a bool indicating if the process is being debugged
@[inline]
pub fn debugger_present() bool {
	$if windows {
		return C.IsDebuggerPresent()
	}
	return false
}
