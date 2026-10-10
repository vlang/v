import os

// A private temp root, named with the pid so two runs on the same host cannot
// collide over the same directory.
fn file_meta_root() string {
	root := os.join_path(os.vtmp_dir(), 'os_file_meta_tests_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn test_posix_get_error_msg_renders_errno_codes() {
	assert os.posix_get_error_msg(0) != ''
	assert os.posix_get_error_msg(1) != ''
	assert os.posix_get_error_msg(2) == 'No such file or directory'
	// An unknown code must not call strerror(NULL) either.
	assert os.posix_get_error_msg(-1) != ''
}

fn test_stat_and_lstat_agree_on_a_regular_file() {
	root := file_meta_root()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'data.txt')
	content := 'a file with a few bytes'
	os.write_file(path, content)!

	st := os.stat(path)!
	lst := os.lstat(path)!
	assert st.size == u64(content.len)
	assert lst.size == u64(content.len)
	assert st.mtime > 0
	assert lst.mtime == st.mtime
	assert st.mode == lst.mode
	assert st.inode == lst.inode
}

fn test_utime_sets_access_and_modification_times() {
	root := file_meta_root()
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'timed.txt')
	os.write_file(path, 't')!

	os.utime(path, 1000000, 2000000)!
	assert os.lstat(path)!.atime == 1000000
	assert os.lstat(path)!.mtime == 2000000

	os.utime(os.join_path(root, 'missing.txt'), 1000, 1000) or {
		assert err.code() == 2 // ENOENT
		return
	}
	assert false, 'utime() on a missing file should have failed'
}

fn test_walk_ext_finds_matching_files_recursively() {
	root := file_meta_root()
	defer {
		os.rmdir_all(root) or {}
	}
	deep := os.join_path(root, 'nested', 'deeper')
	os.mkdir_all(deep) or { panic(err) }
	os.write_file(os.join_path(root, 'top.v'), 'a')!
	os.write_file(os.join_path(root, 'top.txt'), 'b')!
	os.write_file(os.join_path(deep, 'deep.v'), 'c')!
	os.write_file(os.join_path(root, '.hidden.v'), 'd')!

	assert os.walk_ext(root, '.zzz', os.WalkParams{}).len == 0

	names := os.walk_ext(root, '.v', os.WalkParams{}).map(os.file_name(it))
	assert 'top.v' in names
	assert 'deep.v' in names
	assert 'top.txt' !in names
	// Hidden entries are skipped unless the option asks for them.
	assert '.hidden.v' !in names

	with_hidden := os.walk_ext(root, '.v', hidden: true).map(os.file_name(it))
	assert '.hidden.v' in with_hidden
	assert 'top.v' in with_hidden

	// A path that is not a directory has nothing to walk.
	assert os.walk_ext(os.join_path(root, 'top.v'), '.v', os.WalkParams{}).len == 0
}

fn test_cp_all_copies_a_tree_and_rejects_a_missing_source() {
	root := file_meta_root()
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'src')
	src_sub := os.join_path(src, 'sub')
	os.mkdir_all(src_sub) or { panic(err) }
	os.write_file(os.join_path(src, 'x.txt'), 'X')!
	os.write_file(os.join_path(src_sub, 'y.txt'), 'Y')!

	dst := os.join_path(root, 'dst')
	os.cp_all(src, dst, true)!
	assert os.read_file(os.join_path(dst, 'x.txt'))! == 'X'
	assert os.read_file(os.join_path(dst, 'sub', 'y.txt'))! == 'Y'
	// Re-copying with overwrite is allowed and leaves the tree intact.
	os.cp_all(src, dst, true)!
	assert os.read_file(os.join_path(dst, 'sub', 'y.txt'))! == 'Y'

	os.cp_all(os.join_path(root, 'nope'), dst, true) or {
		assert err.msg() == "Source path doesn't exist"
		return
	}
	assert false, 'cp_all() on a missing source should have failed'
}

fn test_mv_by_cp_moves_files_and_folders() {
	root := file_meta_root()
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'mvdir')
	os.mkdir_all(src) or { panic(err) }
	os.write_file(os.join_path(src, 'z.txt'), 'Z')!

	dst := os.join_path(root, 'moved')
	os.mv_by_cp(src, dst, overwrite: true)!
	assert os.read_file(os.join_path(dst, 'z.txt'))! == 'Z'
	assert !os.exists(src)

	single := os.join_path(root, 'single.txt')
	os.write_file(single, 'S')!
	single_dst := os.join_path(root, 'single_moved.txt')
	os.mv_by_cp(single, single_dst, overwrite: true)!
	assert os.read_file(single_dst)! == 'S'
	assert !os.exists(single)
}

fn test_disk_usage_reports_free_and_total_space() {
	usage := os.disk_usage(os.vtmp_dir())!
	assert usage.total > 0
	assert usage.available > 0
	assert usage.available <= usage.total
	assert usage.used > 0

	os.disk_usage(os.join_path(os.vtmp_dir(), 'no-such-directory-${os.getpid()}')) or {
		assert err.msg() == 'cannot get disk usage of path'
		return
	}
	assert false, 'disk_usage() on a missing path should have failed'
}
