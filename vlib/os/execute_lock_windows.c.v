@[has_globals]
module os

// g_v_os_execute_lock serializes the creation of a child process, from the first
// handle made for it to the point where this process has closed its own copy of
// the child's ends.
//
// A child is created with handle inheritance on, so it gets a copy of every
// inheritable handle this process holds at that moment, including the write end of
// a pipe that another thread has just made for a child of its own. That other
// thread's read then sees no end of file until both children have exited: its
// short-lived child appears to run for as long as the longest-lived process that
// was started beside it.
//
// The Unix side serializes pipe()+spawn for a related reason, see
// execute_lock_nix.c.v. The lock is held for the creation only, never while the
// child runs.
@[cinit]
__global g_v_os_execute_lock C.SRWLOCK = C.SRWLOCK_INIT

fn v_os_execute_lock() {
	C.AcquireSRWLockExclusive(&g_v_os_execute_lock)
}

fn v_os_execute_unlock() {
	C.ReleaseSRWLockExclusive(&g_v_os_execute_lock)
}
