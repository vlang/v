module os

// this is defined in builtin_windows.c.v in builtin
// fn C.IsDebuggerPresent() bool

[inline]
pub fn debugger_present() bool {
	return C.IsDebuggerPresent()
}
