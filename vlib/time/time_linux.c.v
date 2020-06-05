module time

fn sys_mono_now_darwin() u64 {
	return 0
}

// dummy
pub fn darwin_now() Time {
	return Time{}
}