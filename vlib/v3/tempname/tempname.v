module tempname

import os
import time

// unique_token returns a process-local suffix for temporary paths. The monotonic timestamp
// distinguishes sequential calls, while the stack address distinguishes concurrent workers.
pub fn unique_token() string {
	stack_marker := u8(0)
	stack_id := usize(voidptr(&stack_marker))
	return '${os.getpid()}.${time.sys_mono_now()}.${stack_id}'
}
