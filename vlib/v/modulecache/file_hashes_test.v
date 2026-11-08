module modulecache

import os
import time

fn test_file_hash_memo_reuses_hashes_until_the_file_changes() {
	dir := os.join_path(os.vtmp_dir(), 'v3_modulecache_file_hashes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	enable_file_hash_memo(os.join_path(dir, 'memo'))
	path := os.join_path(dir, 'input.h')
	os.write_file(path, 'int first;\n')!
	old := time.utc().unix() - 600
	os.utime(path, old, old)!
	first := file_signature(path)
	assert first == file_signature_uncached(path)
	digest := file_sha256(path) or { panic('no digest') }
	// The memo keeps both kinds of hashes, and a later build reads them back.
	memo_text := os.read_file(os.join_path(dir, 'memo', file_hash_memo_name))!
	assert memo_text.contains('fnv\t') && memo_text.contains('${first}\n')
	assert memo_text.contains('${digest}\n')
	g_file_hash_memo.loaded = false
	g_file_hash_memo.entries = map[string]string{}
	assert file_signature(path) == first
	// An edit changes the metadata, so the file is read again.
	time.sleep(100 * time.millisecond)
	os.write_file(path, 'int other;\n')!
	assert file_signature(path) == file_signature_uncached(path)
	assert file_signature(path) != first
	assert (file_sha256(path) or { '' }) != digest
}

fn test_file_hash_memo_skips_files_modified_during_the_build() {
	dir := os.join_path(os.vtmp_dir(), 'v3_modulecache_recent_hashes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	enable_file_hash_memo(os.join_path(dir, 'memo'))
	path := os.join_path(dir, 'fresh.h')
	// Written after the memo started: its timestamp cannot tell a later
	// same-size edit apart, so it is hashed but not remembered.
	time.sleep(20 * time.millisecond)
	os.write_file(path, 'int fresh;\n')!
	first := file_signature(path)
	assert first == file_signature_uncached(path)
	assert 'fnv\t${path}' !in g_file_hash_memo.entries
}
