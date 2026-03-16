module os

// Platform-specific dirent layout.
// Linux: d_ino(8) + d_off(8) + d_reclen(2) + d_type(1) + d_name  (offset 19)
// macOS: d_ino(8) + d_seekoff(8) + d_reclen(2) + d_namlen(2) + d_type(1) + d_name  (offset 21)
// The C backend always uses the real <dirent.h> header so field ordering here
// only matters for native backends (x64, arm64).
pub struct C.dirent {
	d_ino    u64
	d_off    i64
	d_reclen u16
	d_type   u8
	d_name   [256]char
}
