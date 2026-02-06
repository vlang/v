module sync

// Get current thread ID from Windows API (via C interface)
fn C.GetCurrentThreadId() u32

// thread_id returns a unique identifier for the caller thread.
pub fn thread_id() u64 {
	return u64(C.GetCurrentThreadId())
}
