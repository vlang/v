module sync

fn C.pthread_self() usize

pub fn thread_id() u64 {
	return u64(C.pthread_self())
}
