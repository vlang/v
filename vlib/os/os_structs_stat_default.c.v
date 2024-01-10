module os

pub struct C.stat {
	st_size  u64
	st_mode  u32
	st_mtime int
}

pub struct C.__stat64 {
	st_size  u64
	st_mode  u32
	st_mtime int
}
