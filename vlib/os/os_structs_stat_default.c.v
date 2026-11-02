module os

module os

$if !windows {
	#include <sys/stat.h>
} $else {
	// Minimal stat struct (Windows fallback)
	pub struct C.stat {
		st_dev   u64
		st_ino   u64
		st_mode  u32
		st_nlink u64
		st_uid   u32
		st_gid   u32
		st_rdev  u64
		st_size  u64
		st_atime i64
		st_mtime i64
		st_ctime i64
	}

	pub struct C.__stat64 {
		st_size  u64
		st_mode  u32
		st_mtime i64
	}
}
