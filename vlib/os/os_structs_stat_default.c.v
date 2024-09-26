module os

// Minimal stat struct as specified in
// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_stat.h.html
pub struct C.stat {
	st_dev   u64
	st_ino   u64
	st_mode  u32
	st_nlink u64
	st_uid   u32
	st_gid   u32
	st_rdev  u64
	st_size  u64
	st_atime int
	st_mtime int
	st_ctime int
}

pub struct C.__stat64 {
	st_size  u64
	st_mode  u32
	st_mtime int
}
