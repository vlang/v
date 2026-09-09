module os

pub struct C.stat {
	st_dev           i32
	st_mode          u16
	st_nlink         u16
	st_ino           u64
	st_uid           u32
	st_gid           u32
	st_rdev          i32
	st_atime         i64
	st_atimensec     i64
	st_mtime         i64
	st_mtimensec     i64
	st_ctime         i64
	st_ctimensec     i64
	st_birthtime     i64
	st_birthtimensec i64
	st_size          i64
	st_blocks        i64
	st_blksize       i32
	st_flags         u32
	st_gen           u32
	st_lspare        i32
	st_qspare        [2]i64
}

pub struct C.__stat64 {
	st_size  u64
	st_mode  u32
	st_mtime i64
}
