module fastc

import os

fn test_resolve_memos_keep_the_newest_within_the_limit() {
	dir := os.join_path(os.vtmp_dir(), 'fastc_memo_prune_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	mut paths := []string{}
	for i in 0 .. 5 {
		path := os.join_path(dir, 'fastc_resolve_${i}.memo')
		os.write_file(path, 'memo')!
		os.write_file(fastc_memo_blob_path(path), 'blob')!
		os.utime(path, 1000 + i, 1000 + i)!
		paths << path
	}
	unrelated := os.join_path(dir, 'other.memo')
	os.write_file(unrelated, '')!
	// The memo just written is kept even when it is not the newest.
	fastc_prune_resolve_memos(paths[0], 3)
	assert os.exists(paths[0])
	assert !os.exists(paths[1]) && !os.exists(fastc_memo_blob_path(paths[1]))
	assert !os.exists(paths[2]) && !os.exists(fastc_memo_blob_path(paths[2]))
	assert os.exists(paths[3]) && os.exists(fastc_memo_blob_path(paths[3]))
	assert os.exists(paths[4]) && os.exists(fastc_memo_blob_path(paths[4]))
	assert os.exists(unrelated)
	// Below the limit nothing is removed.
	fastc_prune_resolve_memos(paths[0], 3)
	assert os.exists(paths[3]) && os.exists(paths[4])
}
