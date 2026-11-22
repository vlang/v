module driver

import os
import sync.stdatomic

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

// An interrupted build leaves what it had written as `<dir>.interrupted` (its signal handler
// can do no more than rename). The next build of the output removes it.
fn test_the_next_build_removes_what_an_interrupted_one_left() {
	root := c_debug_dir_fixture('interrupted_left')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	write_c_debug_dir(v3_c_debug_dir_interrupted(dir), 'unfinished')
	v3_set_aside_c_debug_dir(dir)
	assert !os.exists(v3_c_debug_dir_interrupted(dir))
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
	// giving the directory back also ends the watch on it
	assert v3_watched_c_debug_dir() == ''
}

// While the build moves its executable into place, a signal neither gives the directory back
// nor ends the process: the handler only notes it. Called here as the signal would call it.
fn test_a_signal_during_publication_waits_and_gives_nothing_back() {
	root := c_debug_dir_fixture('publishing')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new')
	assert v3_c_debug_dir_state_is(c_debug_dir_armed)
	v3_begin_c_debug_dir_publication()
	assert v3_c_debug_dir_state_is(c_debug_dir_publishing)
	v3_c_debug_dir_signal(.int)
	assert v3_c_debug_dir_state_is(c_debug_dir_publishing)
	assert read_c_debug_dir(dir) == 'new'
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	// The test goes on, so the signal is taken out of the word here, as the end of the
	// publication takes it, and is not raised.
	assert v3_take_c_debug_dir_publication(c_debug_dir_idle) == int(os.Signal.int)
	v3_keep_new_c_debug_dir(dir)
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	assert read_c_debug_dir(dir) == 'new'
	assert os.ls(root)! == ['.program.v3cc.cg']
}

// The handler and the build can be on two threads, and each acts on what it read of the word
// a moment ago. The schedules that matter are played here by hand: a stale reading is given
// to the step that the other side has already overtaken.

// The handler read `publishing`; the build then ended the publication and found no signal;
// the handler then wrote its signal. Nobody would raise it. With one word, the handler's
// write is a swap from what it read, and fails: it looks again, and ends the process itself.
fn test_a_signal_is_not_lost_when_the_publication_ends_first() {
	root := c_debug_dir_fixture('handshake_build_first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	v3_begin_c_debug_dir_publication()
	read_by_the_handler := v3_c_debug_dir_load()
	assert v3_take_c_debug_dir_publication(c_debug_dir_idle) == 0
	assert v3_c_debug_dir_signal_step(read_by_the_handler, int(os.Signal.int)) == .again
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .end
}

// The same two, the other way round: the handler's signal is in the word before the build
// leaves the publication, and the build takes it out in the step that leaves.
fn test_a_signal_is_handed_over_when_it_is_noted_first() {
	root := c_debug_dir_fixture('handshake_signal_first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	v3_begin_c_debug_dir_publication()
	read_by_the_build := v3_c_debug_dir_load()
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.term)) == .wait
	// the build's reading is stale: leaving the publication from it must not succeed
	assert !v3_c_debug_dir_swap(read_by_the_build, c_debug_dir_idle)
	assert v3_take_c_debug_dir_publication(c_debug_dir_idle) == int(os.Signal.term)
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
}

// The handler read `armed`; the build then began its publication. The handler must neither
// give the directory back nor end the process inside the publication: its swap fails, it
// looks again, and waits.
fn test_a_handler_that_lost_the_directory_to_a_publication_waits() {
	root := c_debug_dir_fixture('handshake_lost')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new')
	read_by_the_handler := v3_c_debug_dir_load()
	assert read_by_the_handler == c_debug_dir_armed
	v3_begin_c_debug_dir_publication()
	assert v3_c_debug_dir_signal_step(read_by_the_handler, int(os.Signal.int)) == .again
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .wait
	assert read_c_debug_dir(dir) == 'new'
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	assert v3_take_c_debug_dir_publication(c_debug_dir_idle) == int(os.Signal.int)
}

// The handler has the directory; the build must not publish (it waits for the process to
// end), and a second signal stands by.
fn test_a_handler_that_has_the_directory_keeps_the_build_and_other_signals_out() {
	root := c_debug_dir_fixture('handshake_handler_first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .give_back
	assert v3_c_debug_dir_state_is(c_debug_dir_giving_back)
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.term)) == .stand_by
	assert !v3_c_debug_dir_swap(c_debug_dir_armed, c_debug_dir_publishing)
	assert v3_take_c_debug_dir_publication(c_debug_dir_idle) == -1
	// (the process ends here; the test puts the word back instead)
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
}

// A move that failed leaves the old executable in place: the directory is to be given back
// again, by a signal as by a failure.
fn test_a_publication_that_failed_is_armed_again() {
	root := c_debug_dir_fixture('publication_failed')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new, never linked')
	v3_begin_c_debug_dir_publication()
	v3_end_c_debug_dir_publication(dir, false)
	assert v3_c_debug_dir_state_is(c_debug_dir_armed)
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
}
