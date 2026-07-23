module modulecache

#include "@VEXEROOT/vlib/v3/modulecache/file_metadata.h"

fn C.v3_modulecache_file_metadata(&char, &u64, &u64, &u64, &u64, &u64, &u64, &u64) int

// file_metadata_signature returns a precise identity for an unchanged cache input.
// An empty result makes callers fall back to hashing the file contents.
pub fn file_metadata_signature(path string) string {
	mut device := u64(0)
	mut inode := u64(0)
	mut size := u64(0)
	mut mtime_seconds := u64(0)
	mut mtime_nanoseconds := u64(0)
	mut ctime_seconds := u64(0)
	mut ctime_nanoseconds := u64(0)
	result := C.v3_modulecache_file_metadata(&char(path.str), &device, &inode, &size,
		&mtime_seconds, &mtime_nanoseconds, &ctime_seconds, &ctime_nanoseconds)
	if result == 0 {
		return ''
	}
	return '${device}:${inode}:${size}:${mtime_seconds}:${mtime_nanoseconds}:${ctime_seconds}:${ctime_nanoseconds}'
}
