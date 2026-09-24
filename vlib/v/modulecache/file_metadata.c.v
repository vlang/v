module modulecache

import os

#include "@VMODROOT/vlib/v/modulecache/file_metadata.c"

fn C.v3_modulecache_file_metadata(&char, &u64, &u64, &u64, &u64, &u64, &u64, &u64) int

// file_metadata_signature returns a precise identity for an unchanged cache input.
// Include the running compiler build so a memoized source signature cannot survive
// a compiler update and make a new checker replay stale cached diagnostics.
// An empty result means the file system reports no usable identity for the path
// (FAT, exFAT and some network redirectors on Windows); callers must then compare
// the file contents instead, see file_change_signature.
pub fn file_metadata_signature(path string) string {
	if file_metadata_disabled_for(path) {
		return ''
	}
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
	return '${@VCURRENTHASH}:${device}:${inode}:${size}:${mtime_seconds}:${mtime_nanoseconds}:${ctime_seconds}:${ctime_nanoseconds}'
}

// file_metadata_disabled_for lets tests simulate a file system without file
// identities for the paths listed in V3_TEST_NO_FILE_METADATA.
fn file_metadata_disabled_for(path string) bool {
	listed := os.getenv('V3_TEST_NO_FILE_METADATA')
	if listed == '' {
		return false
	}
	resolved := os.real_path(path)
	for entry in listed.split(os.path_delimiter) {
		if entry != '' && os.real_path(entry) == resolved {
			return true
		}
	}
	return false
}
