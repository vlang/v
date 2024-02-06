module os

import time

pub struct Stat {
pub:
	dev   u64
	inode u64
	mode  u32
	nlink u64
	uid   u32
	gid   u32
	rdev  u64
	size  u64
	atime i64
	mtime i64
	ctime i64
}

// get_atime returns the last accessed timestamp from the Stat struct
pub fn (st Stat) get_atime() time.Time {
	return time.unix(st.atime)
}

// get_atime returns the created timestamp from the Stat struct
pub fn (st Stat) get_ctime() time.Time {
	return time.unix(st.ctime)
}

// get_atime returns the last modified timestamp from the Stat struct
pub fn (st Stat) get_mtime() time.Time {
	return time.unix(st.mtime)
}
