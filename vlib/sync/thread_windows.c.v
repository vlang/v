module sync

fn C.GetCurrentThreadId() u32

pub fn thread_id() u64 {
	return u64(C.GetCurrentThreadId())
}
