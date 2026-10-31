module os

pub struct C.stat {
	st_dev        u64    // 8
	st_ino        u64    // 8
	st_nlink      u64    // 8
	st_mode       u32    // 4
	st_uid        u32    // 4
	st_gid        u32    // 4
	st_rdev       u64    // 8
	st_size       u64    // 8
	st_blksize    u64    // 8
	st_blocks     u64    // 8
	st_atime      i64    // 8
	st_atime_nsec i64    // 8
	st_mtime      i64    // 8
	st_mtime_nsec i64    // 8
	st_ctime      i64    // 8
	st_ctime_nsec i64    // 8
	__unused      [3]i64 // 24
}

pub struct C.__stat64 {
	st_mode  u32
	st_size  u64
	st_mtime u64
}
