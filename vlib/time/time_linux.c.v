module time

// sys_mono_now_darwin - dummy fn to compile on all platforms/compilers
fn sys_mono_now_darwin() u64 {
	return 0
}

// darwin_now - dummy fn to compile on all platforms/compilers
pub fn darwin_now() Time {
	return Time{}
}

// solaris_now - dummy fn to compile on all platforms/compilers
pub fn solaris_now() Time {
	return Time{}
}

// darwin_utc - dummy fn to compile on all platforms/compilers
pub fn darwin_utc() Time {
	return Time{}
}

// solaris_utc - dummy fn to compile on all platforms/compilers
pub fn solaris_utc() Time {
	return Time{}
}
