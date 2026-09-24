module modulecache

import os
import time

#include "@VMODROOT/vlib/v/modulecache/file_metadata.c"

fn C.v3_modulecache_file_metadata(&char, &u64, &u64, &u64, &u64, &u64, &u64, &u64) int

// coarse_mtime_recent_seconds bounds how long a whole-second modification time
// counts as too recent to identify the file. FAT and exFAT keep modification
// times in 2 second steps and HFS+ in 1 second steps.
const coarse_mtime_recent_seconds = 3

// file_metadata_signature returns a precise identity for an unchanged cache input.
// Include the running compiler build so a memoized source signature cannot survive
// a compiler update and make a new checker replay stale cached diagnostics.
// An empty result means the metadata cannot identify the file: the file system
// reports no identity for it (some network redirectors on Windows), or it was
// modified so recently that another same-size edit could still leave identical
// metadata. Callers must then compare the file contents instead, see
// file_change_signature.
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
	if coarse_mtime_is_recent(mtime_seconds, mtime_nanoseconds, time.utc().unix()) {
		return ''
	}
	return '${@VCURRENTHASH}:${device}:${inode}:${size}:${mtime_seconds}:${mtime_nanoseconds}:${ctime_seconds}:${ctime_nanoseconds}'
}

// coarse_mtime_is_recent reports whether a modification time without a
// sub-second part is too recent, or in the future, for the metadata to tell a
// later same-size edit apart: on a file system with coarse timestamps such an
// edit can land in the same timestamp step. Once the step is over, any edit gets
// a strictly later timestamp.
fn coarse_mtime_is_recent(mtime_seconds u64, mtime_nanoseconds u64, now_seconds i64) bool {
	if mtime_nanoseconds != 0 {
		return false
	}
	return i64(mtime_seconds) > now_seconds - coarse_mtime_recent_seconds
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
