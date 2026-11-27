module modulecache

import os
import time

#flag @VMODROOT/vlib/v/modulecache/file_metadata.o

fn C.v3_modulecache_file_metadata(&char, int, &u64, &u64, &u64, &u64, &u64, &u64, &u64) int

// coarse_mtime_recent_seconds bounds how long a whole-second modification time
// counts as too recent to identify the file. FAT and exFAT keep modification
// times in 2 second steps and HFS+ in 1 second steps.
const coarse_mtime_recent_seconds = 3

// file_metadata_signature returns a precise identity for an unchanged cache input.
// Include the running compiler build so a memoized source signature cannot survive
// a compiler update and make a new checker replay stale cached diagnostics.
// An empty result means the metadata cannot identify the file: the file system
// reports no identity for it (some network redirectors on Windows) or no change
// time (FAT and exFAT on Windows), or it was modified so recently that another
// same-size edit could still leave identical metadata. Callers must then compare
// the file contents instead, see file_change_signature.
pub fn file_metadata_signature(path string) string {
	m := file_metadata_of(path) or { return '' }
	if coarse_mtime_is_recent(m.mtime_seconds, m.mtime_nanoseconds, time.utc().unix()) {
		return ''
	}
	return m.signature()
}

fn (m FileMetadata) signature() string {
	return '${@VCURRENTHASH}:${m.device}:${m.inode}:${m.size}:${m.mtime_seconds}:${m.mtime_nanoseconds}:${m.ctime_seconds}:${m.ctime_nanoseconds}'
}

// native_file_signatures_since identifies dependencies reported after a compile.
// The compiler may have opened an earlier target at any step of a path lookup.
// Check the change time of the directory entries and symlinks as well as the
// resolved file, then let the caller carry these identities through publication.
pub fn native_file_signatures_since(paths []string, started time.Time) ?map[string]string {
	mut signatures := map[string]string{}
	cutoff := started.unix_nano()
	for path in paths {
		m := file_metadata_of(path) or { return none }
		if m.changed_since(cutoff)
			|| coarse_mtime_is_recent(m.mtime_seconds, m.mtime_nanoseconds, started.unix()) {
			return none
		}
		signatures[path] = m.signature()
	}
	// Capture all the file identities first. If a directory was changed before
	// one of those reads, its later validation must see the change.
	mut checked := map[string]bool{}
	for path in paths {
		if !native_path_unchanged(path, cutoff, 0, mut checked) {
			return none
		}
	}
	return signatures
}

fn (m FileMetadata) changed_since(cutoff i64) bool {
	// With whole-second metadata, a change in the same tick is ambiguous.
	precision := if m.ctime_nanoseconds == 0 { i64(999_999_999) } else { i64(0) }
	return i64(m.ctime_seconds) * 1_000_000_000 + i64(m.ctime_nanoseconds) + precision >= cutoff
}

fn native_path_unchanged(path string, cutoff i64, depth int, mut checked map[string]bool) bool {
	if path in checked {
		return true
	}
	if depth >= 40 {
		return false
	}
	parent := os.dir(path)
	if parent != path && !native_path_unchanged(parent, cutoff, depth, mut checked) {
		return false
	}
	m := file_metadata_at(path, true) or { return false }
	if m.changed_since(cutoff) {
		return false
	}
	if m.is_link {
		link := os.readlink(path) or { return false }
		target := if os.is_abs_path(link) { link } else { '${parent}${os.path_separator}${link}' }
		if !native_path_unchanged(target, cutoff, depth + 1, mut checked) {
			return false
		}
	}
	checked[path] = true
	return true
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
