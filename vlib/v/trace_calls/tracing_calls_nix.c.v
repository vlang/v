module trace_calls

pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.gettid() u32
fn C.clock_gettime(int, &C.timespec) int
fn C.pthread_self() u64
