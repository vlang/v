module os

pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.nanosleep(req &C.timespec, rem &C.timespec) int

// sleep_ms provides a cross platform way to sleep, without having to `import time` for a time.sleep/1 call.
fn sleep_ms(ms i64) {
	$if windows {
		C.Sleep(u32(ms))
	} $else {
		mut req := C.timespec{ms / 1000, 1_000_000 * (ms % 1000)}
		rem := C.timespec{}
		for C.nanosleep(&req, &rem) < 0 {
			if C.errno == C.EINTR {
				req = rem
			} else {
				break
			}
		}
	}
}
