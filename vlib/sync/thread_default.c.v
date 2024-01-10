module sync

fn C.pthread_self() usize

// thread_id returns a unique identifier for the caller thread.
// All *currently* running threads in the same process, will have *different* thread identifiers.
// Note: if a thread finishes, and another starts, the identifier of the old thread may be
// reused for the newly started thread.
// In other words, thread IDs are guaranteed to be unique only within a process.
// A thread ID may be reused after a terminated thread has been joined (with `t.wait()`),
// or when the thread has terminated.

pub fn thread_id() u64 {
	return u64(C.pthread_self())
}
