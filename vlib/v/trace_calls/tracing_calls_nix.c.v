module trace_calls

pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.gettid() u32
fn C.clock_gettime(i32, &C.timespec) i32

$if macos {
	fn C.pthread_self() voidptr
} $else {
	fn C.pthread_self() usize
}
