module time

pub fn sys_mono_now() u64 {
	$if js_browser {
		mut res := f64(0.0)
		#res = new builtin.u64(Number(window.performance.now()))

		return u64(res)
	} $else $if js_node {
		mut res := u64(0)
		#res = new builtin.u64(Number(process.hrtime.bigint()))

		return res
	} $else {
		// todo: we might have wrapper around `Date()` that will work as monotonic time.
		return 0
	}
}
