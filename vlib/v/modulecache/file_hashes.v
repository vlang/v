@[has_globals]
module modulecache

import os
import sync
import crypto.sha256

// FileHashMemo remembers the content hashes of files by their metadata (see
// file_metadata_signature), from one build to the next. A build checks the
// same native headers, SDK headers among them, against several caches; with
// the memo it reads only the ones whose metadata changed since a build hashed
// them. A file without usable metadata, as one modified within the last
// moments on a file system without fine timestamps, is always read.
@[heap]
struct FileHashMemo {
mut:
	mu      &sync.Mutex = sync.new_mutex()
	path    string
	loaded  bool
	entries map[string]string
}

__global g_file_hash_memo &FileHashMemo

const file_hash_memo_name = 'file_hashes.v1'

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
	metadata := file_metadata_signature(path)
	if metadata.len == 0 {
		return compute(path)
	}
	key := '${kind}\t${path}\t${metadata}'
	mut memo := g_file_hash_memo
	memo.mu.lock()
	memo.load()
	known := memo.entries[key] or { '' }
	memo.mu.unlock()
	if known.len > 0 {
		return known
	}
	hash := compute(path)
	if hash.len > 0 {
		memo.mu.lock()
		memo.entries[key] = hash
		memo.append(key, hash)
		memo.mu.unlock()
	}
	return hash
}

// load reads the memo once; the holder of `mu` calls it.
fn (mut memo FileHashMemo) load() {
	if memo.loaded {
		return
	}
	memo.loaded = true
	content := os.read_file(memo.path) or { return }
	for line in content.split_into_lines() {
		tab := line.last_index('\t') or { continue }
		if tab > 0 && tab + 1 < line.len {
			memo.entries[line[..tab]] = line[tab + 1..]
		}
	}
	// Later lines replace earlier ones. Rewrite a memo that grew mostly stale.
	lines := content.count('\n')
	if lines > 4096 && lines > 2 * memo.entries.len {
		mut out := []string{cap: memo.entries.len}
		for key, hash in memo.entries {
			out << '${key}\t${hash}'
		}
		temp := '${memo.path}.tmp.${os.getpid()}'
		os.write_file(temp, out.join('\n') + '\n') or { return }
		os.mv(temp, memo.path) or { os.rm(temp) or {} }
	}
}

// append records one hash. Short appends to the same file from builds that run
// at the same time do not interleave.
fn (memo &FileHashMemo) append(key string, hash string) {
	mut file := os.open_append(memo.path) or { return }
	defer { file.close() }
	file.write_string('${key}\t${hash}\n') or {}
}
