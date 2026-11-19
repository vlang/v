@[has_globals]
module os

// g_v_os_execute_cs_storage backs a CRITICAL_SECTION that serialises the
// CreatePipe/CreateProcess sequence. CreateProcessW with bInheritHandles set
// copies every inheritable handle open in the parent at that instant into the
// child, so a spawn in another thread can capture a pipe write end belonging
// to a different call. That call then cannot see EOF until the unrelated child
// exits. Holding the lock from pipe creation until the parent closes its own
// inheritable write handles keeps the window private.
// 128 bytes is larger than CRITICAL_SECTION on both x86 and x64, and the u64
// array keeps the storage word aligned for its atomic operations.
__global g_v_os_execute_cs_storage = [16]u64{}

fn C.InitializeCriticalSection(voidptr)
fn C.EnterCriticalSection(voidptr)
fn C.LeaveCriticalSection(voidptr)

fn v_os_execute_lock_ptr() voidptr {
	return unsafe { voidptr(&g_v_os_execute_cs_storage[0]) }
}

fn init() {
	C.InitializeCriticalSection(v_os_execute_lock_ptr())
}

// v_os_execute_lock starts the pipe/process creation critical section.
fn v_os_execute_lock() {
	C.EnterCriticalSection(v_os_execute_lock_ptr())
}

// v_os_execute_unlock ends the pipe/process creation critical section.
fn v_os_execute_unlock() {
	C.LeaveCriticalSection(v_os_execute_lock_ptr())
}
