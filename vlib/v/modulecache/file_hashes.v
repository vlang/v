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
//
// The memo is kept for every build of the user, so it grows with every file that
// any build hashed. It is split by the directory of each file into
// file_hash_memo_shards files, and a build reads only those of the directories
// of the files that it hashes: what it reads follows its own inputs, as a Zig
// manifest is one per compilation, not the size of all the memo.
@[heap]
struct FileHashMemo {
mut:
	mu  &sync.Mutex = sync.new_mutex()
	dir string
	// The shards that were read, with the number of their lines.
	loaded map[string]int
	// `kind\tpath` -> `metadata\thash`
	entries map[string]string
}

__global g_file_hash_memo &FileHashMemo

// The modification time of a file written when the memo was first used: the
// start of the build, as the timestamp file of a Zig cache manifest marks it.
__global g_build_start_mtime_seconds u64
__global g_build_start_mtime_nanoseconds u64
// 0 before the first use, 1 while one thread sets the time, 2 once it is set. It
// needs no setup, as a module `init` that the compiler's own build leaves out would.
__global g_build_start_mtime_state u32

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
	if C.atomic_load_u32(&g_build_start_mtime_state) != 2 {
		mut unset := u32(0)
		if C.atomic_compare_exchange_strong_u32(&g_build_start_mtime_state, &unset, 1) {
			init_build_start_mtime()
			C.atomic_store_u32(&g_build_start_mtime_state, 2)
		} else {
			for C.atomic_load_u32(&g_build_start_mtime_state) != 2 {
				C.cpu_relax()
			}
		}
	}
	return g_build_start_mtime_seconds, g_build_start_mtime_nanoseconds
}

// file_hash_memo_name is the directory of the shards of the memo in the memo
// directory of a build.
const file_hash_memo_name = 'file_hashes.v3'

// file_hash_memo_shards is the number of files that the memo is split into.
const file_hash_memo_shards = 256

// enable_file_hash_memo makes file_signature and file_sha256 remember their
// hashes in `dir`, which holds them for the builds that use it.
pub fn enable_file_hash_memo(dir string) {
	if dir.len == 0 || !isnil(g_file_hash_memo) {
		return
	}
	memo_dir := os.join_path(dir, file_hash_memo_name)
	if !os.is_dir(memo_dir) {
		// The memo of earlier compilers was one file.
		os.rm(os.join_path(dir, 'file_hashes.v2')) or {}
	}
	os.mkdir_all(memo_dir) or { return }
	g_file_hash_memo = &FileHashMemo{
		dir: memo_dir
	}
	// Mark the start of the build now, before any input is read.
	_, _ = build_start_mtime()
}

// file_changed_during_build reports whether the file or directory at `path` was
// modified at or after the start of the build, or so recently that its
// timestamp cannot tell a later edit apart (as for FileHashMemo). A record of
// the build's inputs must not vouch for such a file: the build may have read
// an earlier version of it.
pub fn file_changed_during_build(path string) bool {
	m := file_metadata_of(path) or { return true }
	start_seconds, start_nanoseconds := build_start_mtime()
	return m.mtime_seconds > start_seconds
		|| (m.mtime_seconds == start_seconds && m.mtime_nanoseconds >= start_nanoseconds)
		|| coarse_mtime_is_recent(m.mtime_seconds, m.mtime_nanoseconds, time.utc().unix())
}

// build_is_marked reports whether this process is a build that marked its start,
// which enable_file_hash_memo does before any input is read.
fn build_is_marked() bool {
	return !isnil(g_file_hash_memo)
}

// file_metadata_signature_from_before_build is file_metadata_signature for a
// stamp that records the file next to a content signature taken earlier. It is
// empty for a file changed during the build, so the next build compares the
// contents instead of trusting metadata that describe a later version of the file
// (isProblematicTimestamp and the zeroed mtime in Build/Cache.zig).
pub fn file_metadata_signature_from_before_build(path string) string {
	if build_is_marked() && file_changed_during_build(path) {
		return ''
	}
	return file_metadata_signature(path)
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
	shard := file_hash_memo_shard(path)
	memo.mu.lock()
	memo.load(shard)
	known := memo.entries[key] or { '' }
	memo.mu.unlock()
	if known.len > metadata.len && known[metadata.len] == `\t` && known.starts_with(metadata) {
		return known[metadata.len + 1..]
	}
	hash := compute(path)
	if file_hash_is_valid(kind, hash) {
		memo.mu.lock()
		memo.entries[key] = '${metadata}\t${hash}'
		memo.append(shard, '${key}\t${metadata}\t${hash}\n')
		memo.mu.unlock()
	}
	return hash
}

// memoized_file_fact returns what `compute` says of the file at `path`, remembered
// with the file hashes by its metadata (see FileHashMemo), so that an unchanged
// file is not read again. `kind` starts with `fact-` and names the fact; `compute`
// returns one line that starts with `f`, or '' when it cannot tell.
pub fn memoized_file_fact(path string, kind string, compute fn (string) string) string {
	return memoized_file_hash(path, kind, compute)
}

// file_hash_is_valid reports whether `hash` has the form of a value of `kind`.
// It keeps out a line that another build was still writing.
fn file_hash_is_valid(kind string, hash string) bool {
	if kind == source_file_facts_kind || kind.starts_with('fact-') {
		// Source facts are one line of their own encoding (source_file_facts).
		return hash.len > 0 && hash.len < 4096 && hash[0] == `f` && !hash.contains_any('\t\n\r')
	}
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

// file_hash_memo_shard returns the name of the shard of the memo that holds the
// hashes of `path`: the files of one directory are in one shard.
fn file_hash_memo_shard(path string) string {
	mut hash := u32(2166136261)
	for c in os.dir(path) {
		hash = (hash ^ u32(c)) * 16777619
	}
	return '${hash % file_hash_memo_shards:02x}'
}

// load reads the shard `shard` of the memo once; the holder of `mu` calls it.
// Only complete lines count: an append of another build can be read half written.
fn (mut memo FileHashMemo) load(shard string) {
	if shard in memo.loaded {
		return
	}
	path := os.join_path_single(memo.dir, shard)
	content := os.read_file(path) or {
		memo.loaded[shard] = 0
		return
	}
	mut lines := 0
	mut entries := 0
	mut start := 0
	for start < content.len {
		end := content.index_after_('\n', start)
		if end < 0 {
			break
		}
		line := content[start..end]
		start = end + 1
		lines++
		fields := line.split('\t')
		if fields.len != 4 || !file_hash_is_valid(fields[0], fields[3]) {
			continue
		}
		key := '${fields[0]}\t${fields[1]}'
		if key !in memo.entries {
			entries++
		}
		memo.entries[key] = '${fields[2]}\t${fields[3]}'
	}
	memo.loaded[shard] = lines
	// Later lines replace earlier ones. Rewrite a shard that grew mostly stale.
	if lines > 1024 && lines > 2 * entries {
		mut out := []string{cap: entries}
		for key, value in memo.entries {
			if file_hash_memo_shard(key.all_after('\t')) == shard {
				out << '${key}\t${value}\n'
			}
		}
		temp := '${path}.tmp.${os.getpid()}'
		os.write_file(temp, out.join('')) or { return }
		os.mv(temp, path) or { os.rm(temp) or {} }
		memo.loaded[shard] = out.len
	}
}

// append records one line in the shard `shard`. Short appends to the same file
// from builds that run at the same time do not interleave.
fn (mut memo FileHashMemo) append(shard string, line string) {
	mut file := os.open_append(os.join_path_single(memo.dir, shard)) or { return }
	defer { file.close() }
	file.write_string(line) or {}
	memo.loaded[shard]++
}
