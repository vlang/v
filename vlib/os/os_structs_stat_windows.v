module os

pub struct C.__stat64 {
	st_dev   u32 // 4
	st_ino   u16 // 2
	st_mode  u16 // 2
	st_nlink u16 // 2	
	st_uid   u16 // 2
	st_gid   u16 // 2
	st_rdev  u32 // 4
	st_size  u64 // 8
	st_atime i64 // 8
	st_mtime i64 // 8
	st_ctime i64 // 8
}
