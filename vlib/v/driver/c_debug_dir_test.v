module driver

import os

// The directory a `-cg` build keeps is replaced by the next build of the same output only when
// that build succeeds. These are the three steps of that, on plain directories.

fn c_debug_dir_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), 'c_debug_dir_unit_${os.getpid()}_${name}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

fn write_c_debug_dir(dir string, text string) {
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'src.c'), text) or { panic(err) }
}

fn read_c_debug_dir(dir string) string {
	return os.read_file(os.join_path(dir, 'src.c')) or { '<none>' }
}

fn test_a_failed_build_gives_the_set_aside_directory_back() {
	root := c_debug_dir_fixture('failed')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	assert !os.exists(dir)
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	write_c_debug_dir(dir, 'new, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
	assert !os.exists(v3_c_debug_dir_set_aside(dir))
}

fn test_a_build_that_succeeds_drops_the_set_aside_directory() {
	root := c_debug_dir_fixture('succeeded')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new')
	v3_keep_new_c_debug_dir(dir)
	assert read_c_debug_dir(dir) == 'new'
	assert !os.exists(v3_c_debug_dir_set_aside(dir))
}

// A build that was killed leaves its unfinished directory beside the one it had set aside. The
// executable in place still belongs to the one set aside, whatever the next build does.
fn test_the_build_after_an_interrupted_one_keeps_what_was_set_aside() {
	root := c_debug_dir_fixture('interrupted')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'interrupted')
	// the next build starts, and fails
	v3_set_aside_c_debug_dir(dir)
	assert !os.exists(dir)
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	write_c_debug_dir(dir, 'next, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
}

fn test_a_first_build_and_a_build_without_c_debug_set_nothing_aside() {
	root := c_debug_dir_fixture('first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	v3_set_aside_c_debug_dir(dir)
	assert os.ls(root)! == []string{}
	plain := os.join_path(root, '.program.v3cc.token')
	write_c_debug_dir(plain, 'plain')
	cleanup_c_build_dir(plain)
	assert os.ls(root)! == []string{}
}

// A build has more ways to end than calls to cleanup_c_build_dir. The exit hook is what gives
// the set-aside directory back for the others: here it is run as the end of such a build.
fn test_the_exit_hook_gives_back_what_a_build_left_set_aside() {
	root := c_debug_dir_fixture('hook')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	assert v3_watched_c_debug_dir() == dir
	write_c_debug_dir(dir, 'new, unfinished')
	// the build ends here, without cleanup_c_build_dir
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(dir) == 'old'
	assert !os.exists(v3_c_debug_dir_set_aside(dir))
	assert v3_watched_c_debug_dir() == ''
}

// After a build that succeeded, or a failure that cleaned up, nothing is set aside: the hook
// leaves the directory that is there.
fn test_the_exit_hook_leaves_a_finished_build_alone() {
	root := c_debug_dir_fixture('hook_done')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new')
	v3_keep_new_c_debug_dir(dir)
	assert v3_watched_c_debug_dir() == ''
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(dir) == 'new'
	// a first build that fails without cleaning up has nothing to give back either
	first := os.join_path(root, '.other.v3cc.cg')
	v3_set_aside_c_debug_dir(first)
	write_c_debug_dir(first, 'first, unfinished')
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(first) == 'first, unfinished'
}

// A process forked from a build shares its memory, and may end while the build goes on: its
// hook must not take the directory the build is writing to.
fn test_the_exit_hook_of_another_process_touches_nothing() {
	root := c_debug_dir_fixture('hook_child')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new, being written')
	build_pid := v3_c_debug_dir_watched_pid
	v3_c_debug_dir_watched_pid = build_pid + 1
	v3_give_back_c_debug_dir_at_exit()
	v3_c_debug_dir_watched_pid = build_pid
	assert read_c_debug_dir(dir) == 'new, being written'
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	assert v3_watched_c_debug_dir() == dir
	v3_unwatch_c_debug_dir()
}
