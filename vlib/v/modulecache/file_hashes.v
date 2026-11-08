@[has_globals]
module modulecache

import os
import sync
import time
import crypto.sha256

// FileHashMemo remembers the content hashes of files by their metadata, from
// one build to the next, as Zig's build cache trusts a file whose size, inode
// and modification time are unchanged (std/Build/Cache.zig,
// Manifest.hitWithCurrentLock). A build checks the same native headers, SDK
// headers among them, against several caches; with the memo it reads only the
// ones whose metadata changed since a build hashed them.
//
// A file modified at or after the start of the build is never remembered
// (isProblematicTimestamp in Cache.zig): an edit within the same timestamp step,
// which can be several milliseconds on file systems with nanosecond timestamps,
// could keep all of its metadata. Nor is a whole-second timestamp of a coarse
// file system while it is recent (see coarse_mtime_is_recent).
@[heap]
struct FileHashMemo {
mut:
	mu     &sync.Mutex = sync.new_mutex()
	path   string
	loaded bool
	// `kind\tpath` -> `metadata\thash`
	entries map[string]string
	lines   int
}

__global g_file_hash_memo &FileHashMemo

// The modification time of a file written when the memo was first used: the
// start of the build, as the timestamp file of a Zig cache manifest marks it.
__global g_build_start_mtime_seconds u64
__global g_build_start_mtime_nanoseconds u64
__global g_build_start_mtime_once &sync.Once

fn init() {
	g_build_start_mtime_once = sync.new_once()
}

fn init_build_start_mtime() {
	marker := os.join_path(os.vtmp_dir(), 'v3_build_start.${os.getpid()}')
	os.write_file(marker, '') or {}
	defer {
		os.rm(marker) or {}
	}
	stamp := file_metadata_of(marker) or {
		g_build_start_mtime_seconds = u64(time.utc().unix())
		return
	}
	g_build_start_mtime_seconds = stamp.mtime_seconds
	g_build_start_mtime_nanoseconds = stamp.mtime_nanoseconds
}

// build_start_mtime returns the modification time that marks the start of the
// build (see g_build_start_mtime_seconds).
fn build_start_mtime() (u64, u64) {
	g_build_start_mtime_once.do(init_build_start_mtime)
	return g_build_start_mtime_seconds, g_build_start_mtime_nanoseconds
}

const file_hash_memo_name = 'file_hashes.v2'

// enable_file_hash_memo makes file_signature and file_sha256 remember their
// hashes in `dir`, which holds them for the builds that use it.
pub fn enable_file_hash_memo(dir string) {
	if dir.len == 0 || !isnil(g_file_hash_memo) {
		return
	}
	os.mkdir_all(dir) or { return }
	g_file_hash_memo = &FileHashMemo{
		path: os.join_path(dir, file_hash_memo_name)
	}
}

struct FileMetadata {
	device            u64
	inode             u64
	size              u64
	mtime_seconds     u64
	mtime_nanoseconds u64
	ctime_seconds     u64
	ctime_nanoseconds u64
}

fn file_metadata_of(path string) ?FileMetadata {
	if file_metadata_disabled_for(path) {
		return none
	}
	mut m := FileMetadata{}
	if C.v3_modulecache_file_metadata(&char(path.str), &m.device, &m.inode, &m.size,
		&m.mtime_seconds, &m.mtime_nanoseconds, &m.ctime_seconds, &m.ctime_nanoseconds) == 0 {
		return none
	}
	return m
}

// file_sha256 returns the SHA-256 of the contents of the file at `path`, as hex.
pub fn file_sha256(path string) ?string {
	digest := memoized_file_hash(path, 'sha256', file_sha256_uncached)
	if digest.len == 0 {
		return none
	}
	return digest
}

fn file_sha256_uncached(path string) string {
	mut file := os.open(path) or { return '' }
	defer { file.close() }
	mut digest := sha256.new()
	mut buffer := []u8{len: 64 * 1024}
	for {
		read := file.read(mut buffer) or {
			if err is os.Eof {
				break
			}
			return ''
		}
		if read == 0 {
			break
		}
		digest.write(buffer[..read]) or { return '' }
	}
	return digest.sum([]).hex()
}

fn memoized_file_hash(path string, kind string, compute fn (string) string) string {
	if isnil(g_file_hash_memo) || path.contains('\t') || path.contains('\n') {
		return compute(path)
	}
	mut memo := g_file_hash_memo
	m := file_metadata_of(path) or { return compute(path) }
	start_seconds, start_nanoseconds := build_start_mtime()
	if m.mtime_seconds > start_seconds
		|| (m.mtime_seconds == start_seconds && m.mtime_nanoseconds >= start_nanoseconds)
		|| coarse_mtime_is_recent(m.mtime_seconds, m.mtime_nanoseconds, time.utc().unix()) {
		return compute(path)
	}
	key := '${kind}\t${path}'
	metadata := '${m.device}:${m.inode}:${m.size}:${m.mtime_seconds}:${m.mtime_nanoseconds}:${m.ctime_seconds}:${m.ctime_nanoseconds}'
	memo.mu.lock()
	memo.load()
	known := memo.entries[key] or { '' }
	memo.mu.unlock()
	if known.len > metadata.len && known[metadata.len] == `\t` && known.starts_with(metadata) {
		return known[metadata.len + 1..]
	}
	hash := compute(path)
	if file_hash_is_valid(kind, hash) {
		memo.mu.lock()
		memo.entries[key] = '${metadata}\t${hash}'
		memo.append('${key}\t${metadata}\t${hash}\n')
		memo.mu.unlock()
	}
	return hash
}

// file_hash_is_valid reports whether `hash` has the form of a hash of `kind`.
// It keeps out a line that another build was still writing.
fn file_hash_is_valid(kind string, hash string) bool {
	max_len := if kind == 'sha256' { 64 } else { 16 }
	if hash.len == 0 || hash.len > max_len || (kind == 'sha256' && hash.len != 64) {
		return false
	}
	for c in hash {
		if !((c >= `0` && c <= `9`) || (c >= `a` && c <= `f`)) {
			return false
		}
	}
	return true
}

// load reads the memo once; the holder of `mu` calls it. Only complete lines
// count: an append of another build can be read half written.
fn (mut memo FileHashMemo) load() {
	if memo.loaded {
		return
	}
	memo.loaded = true
	content := os.read_file(memo.path) or { return }
	mut start := 0
	for start < content.len {
		end := content.index_after_('\n', start)
		if end < 0 {
			break
		}
		line := content[start..end]
		start = end + 1
		memo.lines++
		fields := line.split('\t')
		if fields.len != 4 || !file_hash_is_valid(fields[0], fields[3]) {
			continue
		}
		memo.entries['${fields[0]}\t${fields[1]}'] = '${fields[2]}\t${fields[3]}'
	}
	// Later lines replace earlier ones. Rewrite a memo that grew mostly stale.
	if memo.lines > 1024 && memo.lines > 2 * memo.entries.len {
		mut out := []string{cap: memo.entries.len}
		for key, value in memo.entries {
			out << '${key}\t${value}\n'
		}
		temp := '${memo.path}.tmp.${os.getpid()}'
		os.write_file(temp, out.join('')) or { return }
		os.mv(temp, memo.path) or { os.rm(temp) or {} }
		memo.lines = memo.entries.len
	}
}

// append records one line. Short appends to the same file from builds that run
// at the same time do not interleave.
fn (mut memo FileHashMemo) append(line string) {
	mut file := os.open_append(memo.path) or { return }
	defer { file.close() }
	file.write_string(line) or {}
	memo.lines++
}
