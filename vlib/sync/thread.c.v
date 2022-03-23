module sync

pub fn thread_id() u64 {
	$if windows {
		return u64(C.GetCurrentThreadId())
	} $else {
		return u64(C.pthread_self())
	}
}
