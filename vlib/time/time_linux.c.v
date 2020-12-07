module time

fn sys_mono_now_darwin() u64 {
	return 0
}

// dummy to compile with all compilers
pub fn darwin_now() Time {
	return Time{}
}

// dummy to compile with all compilers
pub fn solaris_now() Time {
	return Time{}
}

// dummy to compile with all compilers
pub fn darwin_utc() Time {
	return Time{}
}

// dummy to compile with all compilers
pub fn solaris_utc() Time {
	return Time{}
}
