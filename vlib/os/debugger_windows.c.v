module os

#include <debugapi.h>

fn C.IsDebuggerPresent() bool

[inline]
pub fn debugger_present() bool {
	return C.IsDebuggerPresent()
}