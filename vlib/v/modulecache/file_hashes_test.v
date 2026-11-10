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
	memo_text := os.read_file(os.join_path(dir, 'memo', file_hash_memo_name, file_hash_memo_shard(path)))!
	assert memo_text.contains('fnv\t') && memo_text.contains('${first}\n')
	assert memo_text.contains('${digest}\n')
	g_file_hash_memo.loaded = map[string]int{}
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

fn test_build_start_time_is_one_time_for_all_threads() {
	mut threads := []thread (u64, u64){}
	for _ in 0 .. 8 {
		threads << spawn build_start_mtime()
	}
	seconds, nanoseconds := build_start_mtime()
	assert seconds > 0
	for t in threads {
		other_seconds, other_nanoseconds := t.wait()
		assert other_seconds == seconds
		assert other_nanoseconds == nanoseconds
	}
}

fn test_file_hash_memo_reads_only_the_shards_of_the_files_it_hashes() {
	dir := os.join_path(os.vtmp_dir(), 'v3_modulecache_hash_shards_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'one'))!
	os.mkdir_all(os.join_path(dir, 'two'))!
	defer {
		os.rmdir_all(dir) or {}
	}
	enable_file_hash_memo(os.join_path(dir, 'memo'))
	g_file_hash_memo.loaded = map[string]int{}
	g_file_hash_memo.entries = map[string]string{}
	old := time.utc().unix() - 600
	mut paths := []string{}
	for name in ['one', 'two'] {
		path := os.join_path(dir, name, 'input.h')
		os.write_file(path, 'int ${name};\n')!
		os.utime(path, old, old)!
		paths << path
	}
	assert file_hash_memo_shard(os.join_path(dir, 'one', 'other.h')) == file_hash_memo_shard(paths[0])
	for path in paths {
		file_signature(path)
	}
	g_file_hash_memo.loaded = map[string]int{}
	g_file_hash_memo.entries = map[string]string{}
	// A later build that hashes the file of one directory reads that shard alone.
	assert file_signature(paths[0]) == file_signature_uncached(paths[0])
	assert g_file_hash_memo.loaded.keys() == [file_hash_memo_shard(paths[0])]
	assert 'fnv\t${paths[0]}' in g_file_hash_memo.entries
}
