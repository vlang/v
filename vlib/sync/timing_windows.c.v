module sync

fn C.QueryPerformanceCounter(&u64) C.BOOL
fn C.QueryPerformanceFrequency(&u64) C.BOOL

fn sync_mono_now() i64 {
	mut counter := u64(0)
	mut frequency := u64(0)
	C.QueryPerformanceCounter(voidptr(&counter))
	C.QueryPerformanceFrequency(voidptr(&frequency))
	seconds := counter / frequency
	remainder := counter % frequency
	return i64(seconds * 1_000_000_000 + remainder * 1_000_000_000 / frequency)
}

fn sync_milliseconds(timeout i64) u32 {
	if timeout <= 0 {
		return 0
	}
	milliseconds := timeout / 1_000_000
	if milliseconds >= i64(C.INFINITE) {
		return u32(C.INFINITE - 1)
	}
	return u32(milliseconds)
}

fn sync_sleep_nanoseconds(duration i64) {
	C.Sleep(int(duration / 1_000_000))
}
