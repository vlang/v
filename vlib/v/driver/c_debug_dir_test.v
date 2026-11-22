module driver

import os
import sync.stdatomic
import time

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

// ── rollback: the build and a handler both want the directories ──────────────────────────

fn cleans_up_c_debug_dir(dir string, done chan bool) {
	cleanup_c_build_dir(dir)
	done <- true
}

// A handler has taken the directory and given it back, and is about to end the process. A
// cleanup of the build, on its own thread, must not touch the directories any more: the
// sources it would remove are the ones the handler has just put back, and nothing is set
// aside to replace them. (Before, the cleanup did not ask whose the directories were: it
// removed the old sources, for good.) It waits, as the publication does, for the end of the
// process.
fn test_a_cleanup_after_a_handler_took_the_directory_touches_nothing() {
	root := c_debug_dir_fixture('rollback_handler_first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new, unfinished')
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .give_back
	// what the handler does with the directory it has
	$if windows {
		v3_give_back_c_debug_dir_as_owner()
	} $else {
		v3_give_back_c_debug_dir_in_a_handler()
	}
	assert read_c_debug_dir(dir) == 'old'
	done := chan bool{cap: 1}
	spawn cleans_up_c_debug_dir(dir, done)
	mut finished := false
	select {
		_ := <-done {
			finished = true
		}
		300 * time.millisecond {
		}
	}
	assert read_c_debug_dir(dir) == 'old', 'the sources the handler gave back are still there'
	assert !finished, 'the cleanup waits for the process to end'
	// (the process ends here; the test lets the waiting thread go on a directory of no use)
	os.rename(dir, dir + '.kept')!
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
	_ := <-done
	assert read_c_debug_dir(dir + '.kept') == 'old'
}

// ── a directory that was replaced and could not be taken away ─────────────────────────────

struct CDebugDirBlock {
mut:
	file   os.File
	locked string
}

// block_c_debug_dir makes `set_aside` impossible to rename to `interrupted` and impossible
// to remove, the way a file that another program holds open does on Windows. On Unix: the
// name it would be renamed to is taken by a file (a directory there would take it IN:
// os.rename moves into a directory that exists), and it has a directory in it that
// nothing can be removed from.
fn block_c_debug_dir(set_aside string, interrupted string) CDebugDirBlock {
	mut block := CDebugDirBlock{}
	$if windows {
		block.file = os.open(os.join_path(set_aside, 'src.c')) or { panic(err) }
	} $else {
		os.write_file(interrupted, 'in the way') or { panic(err) }
		block.locked = os.join_path(set_aside, 'locked')
		write_c_debug_dir(block.locked, 'held')
		os.chmod(block.locked, 0o555) or { panic(err) }
	}
	return block
}

fn (mut block CDebugDirBlock) release() {
	$if windows {
		block.file.close()
	} $else {
		os.chmod(block.locked, 0o755) or {}
	}
}

// can_block_c_debug_dir is false where the block above does not hold: for root, on Unix.
fn can_block_c_debug_dir() bool {
	$if windows {
		return true
	} $else {
		return os.getuid() != 0
	}
}

// A build succeeded, and the directory it had set aside could be neither renamed nor
// removed. It is still there under the name of something to give back, beside a NEWER
// executable. The next build must not take it for the sources of the executable in place:
// it would remove the right ones, and give the wrong ones back when it fails.
fn test_a_replaced_directory_that_could_not_be_taken_away_is_not_given_back_later() {
	if !can_block_c_debug_dir() {
		eprintln('> skipped: a directory cannot be made impossible to remove for root')
		return
	}
	root := c_debug_dir_fixture('retired_released')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	// build 1 replaces the executable of sources 0 by the executable of sources 1
	write_c_debug_dir(dir, 'sources 0')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'sources 1')
	v3_begin_c_debug_dir_publication()
	mut block := block_c_debug_dir(set_aside, v3_c_debug_dir_interrupted(dir))
	v3_keep_new_c_debug_dir(dir)
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	assert os.is_dir(set_aside), 'the fixture: what was replaced could not be taken away'
	assert os.exists(v3_c_debug_dir_retired(dir)), 'it has the mark of a directory that was retired'
	assert !v3_c_debug_dir_is_set_aside(dir)
	block.release()
	// build 2 starts, writes, and fails
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'sources 2, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1', 'the executable in place is the one of sources 1'
	assert !os.exists(set_aside) && !os.exists(v3_c_debug_dir_retired(dir))
}

// The same, with the directory still impossible to remove when the next build runs. The name
// a set-aside directory needs is taken, so the sources of the executable in place could not be
// given back by a build that fails: that build is not started. It changes nothing, and nothing
// that ends this process changes anything either: not a cleanup, not the exit hook, not a
// signal. (Before, the build removed the sources in place and went on; a failure then left
// the executable without them.)
fn test_a_build_that_could_not_give_the_sources_back_is_not_started() {
	if !can_block_c_debug_dir() {
		eprintln('> skipped: a directory cannot be made impossible to remove for root')
		return
	}
	root := c_debug_dir_fixture('retired_held')
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 0')
	assert v3_set_aside_c_debug_dir(dir) == ''
	write_c_debug_dir(dir, 'sources 1')
	v3_begin_c_debug_dir_publication()
	mut block := block_c_debug_dir(set_aside, v3_c_debug_dir_interrupted(dir))
	defer {
		block.release()
		os.rmdir_all(root) or {}
	}
	v3_keep_new_c_debug_dir(dir)
	assert os.is_dir(set_aside), 'the fixture: what was replaced could not be taken away'
	// build 2
	refusal := v3_set_aside_c_debug_dir(dir)
	assert refusal.contains('did not start'), refusal
	assert refusal.contains(dir) && refusal.contains(set_aside), 'it names the directory in place and the one in the way'
	assert read_c_debug_dir(dir) == 'sources 1', 'the sources of the executable in place are where they were'
	assert v3_watched_c_debug_dir() == ''
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	// every way this process can end
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1', 'a cleanup of the refused build leaves them'
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(dir) == 'sources 1', 'the exit hook leaves them'
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .end, 'a signal only ends the process'
	$if !windows {
		v3_give_back_c_debug_dir_in_a_handler()
		assert read_c_debug_dir(dir) == 'sources 1', 'and the giving back of a handler, were it reached, leaves them'
	}
	assert read_c_debug_dir(set_aside) != 'sources 1'
	assert os.exists(v3_c_debug_dir_retired(dir)), 'the mark stays for as long as the directory does'
	// once what was in the way can go, the next build starts, and a failure gives back
	block.release()
	assert v3_set_aside_c_debug_dir(dir) == ''
	assert !os.exists(v3_c_debug_dir_retired(dir))
	write_c_debug_dir(dir, 'sources 2, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1'
}

// The directory in place cannot be moved aside (the name it would take is taken by something
// that is no directory; on Windows a file open in it does the same). Removing it instead is
// what the build did before. It is not started.
fn test_a_directory_that_cannot_be_moved_aside_is_not_removed_instead() {
	root := c_debug_dir_fixture('not_moved')
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 1')
	os.write_file(set_aside, 'in the way')!
	refusal := v3_set_aside_c_debug_dir(dir)
	assert refusal.contains('did not start') && refusal.contains('cannot be moved'), refusal
	assert read_c_debug_dir(dir) == 'sources 1'
	assert v3_watched_c_debug_dir() == ''
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	cleanup_c_build_dir(dir)
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(dir) == 'sources 1'
	// with the name free again the build starts
	os.rm(set_aside)!
	assert v3_set_aside_c_debug_dir(dir) == ''
	assert read_c_debug_dir(set_aside) == 'sources 1'
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1'
}

// With a retired directory that cannot go and NOTHING in place, there is nothing to lose: the
// build goes on, sets nothing aside, and a failure gives nothing back, least of all the
// retired directory.
fn test_a_build_with_nothing_in_place_goes_on_beside_a_retired_directory() {
	if !can_block_c_debug_dir() {
		eprintln('> skipped: a directory cannot be made impossible to remove for root')
		return
	}
	root := c_debug_dir_fixture('retired_nothing_in_place')
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(set_aside, 'sources 0, replaced')
	os.write_file(v3_c_debug_dir_retired(dir), '')!
	mut block := hold_c_debug_dir(set_aside)
	defer {
		block.release()
		os.rmdir_all(root) or {}
	}
	assert v3_set_aside_c_debug_dir(dir) == ''
	assert v3_watched_c_debug_dir() == dir
	write_c_debug_dir(dir, 'sources 1, unfinished')
	cleanup_c_build_dir(dir)
	assert !os.exists(dir), 'nothing was set aside, and the retired directory did not come back'
	assert os.exists(v3_c_debug_dir_retired(dir))
}

// The other way round: the build has the directories, and a signal arrives in the middle of
// its giving back. The handler must not touch them (the unfinished directory may be half
// removed, the one set aside not yet moved): it leaves its signal in the word and returns,
// and the build raises it when the directory is back. Played by hand, as the schedules above.
fn test_a_signal_during_a_rollback_of_the_build_waits_until_the_directory_is_back() {
	root := c_debug_dir_fixture('rollback_build_first')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new, unfinished')
	assert v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) == .mine
	assert v3_c_debug_dir_state_is(c_debug_dir_rolling_back)
	// a second thread of the build stays out too
	assert v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) == .busy
	// the signal, as the handler is called: it gives nothing back and ends nothing
	v3_c_debug_dir_signal(.term)
	assert v3_c_debug_dir_state_is(c_debug_dir_rolling_back)
	assert read_c_debug_dir(dir) == 'new, unfinished'
	assert read_c_debug_dir(v3_c_debug_dir_set_aside(dir)) == 'old'
	// the build gives back, and finds the signal in the word in the step that lets go of it
	assert v3_give_back_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
	assert v3_take_c_debug_dir_word(c_debug_dir_rolling_back, c_debug_dir_idle) == int(os.Signal.term)
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	// raised now, it only ends the process: nothing is set aside
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.term)) == .end
}

// The handler read `armed`; the build then took the directories for a rollback. The
// handler's swap fails: it looks again, and waits. And the build that read `armed` while a
// handler took the directory fails its swap the same way, and finds them the handler's.
fn test_a_rollback_and_a_handler_that_both_read_armed_do_not_both_get_the_directory() {
	root := c_debug_dir_fixture('rollback_both')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	read_by_both := v3_c_debug_dir_load()
	assert read_by_both == c_debug_dir_armed
	assert v3_c_debug_dir_rollback_step(read_by_both) == .mine
	assert v3_c_debug_dir_signal_step(read_by_both, int(os.Signal.int)) == .again
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .wait
	assert v3_take_c_debug_dir_word(c_debug_dir_rolling_back, c_debug_dir_armed) == int(os.Signal.int)
	// and the other order, from the same stale reading
	assert v3_c_debug_dir_signal_step(read_by_both, int(os.Signal.int)) == .give_back
	assert v3_c_debug_dir_rollback_step(read_by_both) == .again
	assert v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) == .theirs
	// (the process ends here; the test puts the word back instead)
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
}

// A cleanup of a directory that is not the watched one, or with nothing armed, asks nobody:
// no handler would touch those directories.
fn test_a_rollback_with_nothing_armed_is_nobodys_to_coordinate() {
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
	assert v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) == .nobodys
	assert !v3_begin_c_debug_dir_rollback()
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
}

// The mark by itself: a set-aside directory with the mark beside it is not set aside for
// anything. It is not given back, by a failure, by the exit hook or by the Unix handler's
// test of it; the next build removes it with its mark and sets aside the right one.
fn test_a_retired_directory_is_given_back_by_nothing() {
	root := c_debug_dir_fixture('retired_mark')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 1')
	write_c_debug_dir(set_aside, 'sources 0, replaced')
	os.write_file(v3_c_debug_dir_retired(dir), '')!
	assert !v3_c_debug_dir_is_set_aside(dir)
	// a failure of a build that had not set anything aside itself
	assert !v3_give_back_c_build_dir(dir + '.other')
	// the exit hook of a process that watches the directory
	v3_watch_c_debug_dir(dir)
	v3_give_back_c_debug_dir_at_exit()
	assert read_c_debug_dir(dir) == 'sources 1'
	// the giving back of the Unix handler, with the directory its own
	$if !windows {
		v3_give_back_c_debug_dir_in_a_handler()
		assert read_c_debug_dir(dir) == 'sources 1'
		assert read_c_debug_dir(set_aside) == 'sources 0, replaced'
		assert !os.exists(v3_c_debug_dir_interrupted(dir))
	}
	// the next build: the retired one goes, with its mark, and the right one is set aside
	v3_set_aside_c_debug_dir(dir)
	assert !os.exists(v3_c_debug_dir_retired(dir))
	assert read_c_debug_dir(set_aside) == 'sources 1'
	write_c_debug_dir(dir, 'sources 2, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1'
	assert os.ls(root)! == ['.program.v3cc.cg']
}

// A build that succeeds and can rename what it had set aside leaves no mark.
fn test_a_build_that_succeeds_leaves_no_mark() {
	root := c_debug_dir_fixture('retired_none')
	defer { os.rmdir_all(root) or {} }
	dir := os.join_path(root, '.program.v3cc.cg')
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new')
	v3_begin_c_debug_dir_publication()
	v3_keep_new_c_debug_dir(dir)
	assert os.ls(root)! == ['.program.v3cc.cg']
}

// hold_c_debug_dir makes `dir` impossible to remove: an open file in it on Windows, a
// directory in it that nothing can be removed from on Unix (not for root).
fn hold_c_debug_dir(dir string) CDebugDirBlock {
	mut block := CDebugDirBlock{}
	$if windows {
		block.file = os.open(os.join_path(dir, 'src.c')) or { panic(err) }
	} $else {
		block.locked = os.join_path(dir, 'locked')
		write_c_debug_dir(block.locked, 'held')
		os.chmod(block.locked, 0o555) or { panic(err) }
	}
	return block
}

// What a build had written cannot always be removed (a file in it is open). The directory
// that was set aside is then not moved back: os.rename moves INTO a directory that exists,
// and the old sources would end up inside what is left of the unfinished ones, found by
// nothing. It stays set aside, which is where the next build of the output looks for the
// sources of the executable in place.
fn test_a_directory_that_cannot_be_removed_does_not_swallow_the_one_set_aside() {
	if !can_block_c_debug_dir() {
		eprintln('> skipped: a directory cannot be made impossible to remove for root')
		return
	}
	root := c_debug_dir_fixture('not_removed')
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'old')
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'new, unfinished')
	mut block := hold_c_debug_dir(dir)
	defer {
		block.release()
		os.rmdir_all(root) or {}
	}
	cleanup_c_build_dir(dir)
	assert os.is_dir(dir), 'the fixture: what the build had written could not be removed'
	assert read_c_debug_dir(set_aside) == 'old', 'the old sources are still set aside'
	assert !os.exists(os.join_path(dir, os.file_name(set_aside))), 'and not inside what is left'
	// the watch goes on: the exit hook, or a signal, tries again
	assert v3_watched_c_debug_dir() == dir
	assert v3_c_debug_dir_state_is(c_debug_dir_armed)
	block.release()
	// the next build of the output keeps what is set aside, and a failure gives it back
	v3_set_aside_c_debug_dir(dir)
	write_c_debug_dir(dir, 'newer, unfinished')
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'old'
	assert os.ls(root)! == ['.program.v3cc.cg']
}

// The name of the set-aside directory is taken by something that is no directory, the
// directory in place is watched, and a signal comes: the schedule in which a build had armed
// the watch and had not yet found that its move failed. A handler gives back a DIRECTORY that
// was set aside, and nothing that merely has its name. (Before, the Unix handler asked only
// whether the name existed: it moved the sources in place to `.interrupted`, where the next
// build removes them, and put the thing in the way in their place.)
fn test_a_handler_gives_back_a_directory_and_not_whatever_has_its_name() {
	$if windows {
		// the Windows handler asks for a directory already (v3_c_debug_dir_is_set_aside)
		return
	}
	root := c_debug_dir_fixture('name_taken_signal')
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 1')
	os.write_file(set_aside, 'in the way')!
	v3_watch_c_debug_dir(dir)
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.int)) == .give_back
	v3_give_back_c_debug_dir_in_a_handler()
	assert read_c_debug_dir(dir) == 'sources 1', 'the sources in place are where they were'
	assert os.read_file(set_aside)! == 'in the way'
	assert !os.exists(v3_c_debug_dir_interrupted(dir))
	// (the process ends here; the test puts the word back instead)
	stdatomic.store_u64(&v3_c_debug_dir_word, c_debug_dir_idle)
	v3_unwatch_c_debug_dir()
}

// A signal while the build is moving the directory in place aside: it waits, as during a
// publication, and touches nothing. The build raises it when it knows the outcome. Played by
// hand: the state the build is in between its watch and its move, then the handler.
fn test_a_signal_during_the_setup_waits_until_the_move_is_known() {
	root := c_debug_dir_fixture('setup_signal')
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 1')
	os.write_file(set_aside, 'in the way')!
	v3_watch_c_debug_dir_as(dir, c_debug_dir_setting_aside)
	assert v3_c_debug_dir_state_is(c_debug_dir_setting_aside)
	// the signal, as the handler is called: it gives nothing back and ends nothing
	v3_c_debug_dir_signal(.term)
	assert v3_c_debug_dir_state_is(c_debug_dir_setting_aside)
	assert read_c_debug_dir(dir) == 'sources 1'
	assert os.read_file(set_aside)! == 'in the way'
	assert !os.exists(v3_c_debug_dir_interrupted(dir))
	// a cleanup on another thread of the build stays out as well
	assert v3_c_debug_dir_rollback_step(v3_c_debug_dir_load()) == .busy
	// the build finds its move failed, and lets the word go: the signal is in it
	assert v3_take_c_debug_dir_word(c_debug_dir_setting_aside, c_debug_dir_idle) == int(os.Signal.term)
	// raised now, it only ends the process
	assert v3_c_debug_dir_signal_step(v3_c_debug_dir_load(), int(os.Signal.term)) == .end
	v3_unwatch_c_debug_dir()
}

// The whole of it through the build's own function: with the name taken, the build is refused,
// the word was never `armed` on the way (it ends idle, with nothing watched), and the sources
// in place and the thing in the way are as they were. And a move that works leaves the word
// armed with a directory set aside.
fn test_the_setup_ends_idle_when_refused_and_armed_when_the_directory_was_moved() {
	root := c_debug_dir_fixture('setup_outcomes')
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(root, '.program.v3cc.cg')
	set_aside := v3_c_debug_dir_set_aside(dir)
	write_c_debug_dir(dir, 'sources 1')
	os.write_file(set_aside, 'in the way')!
	assert v3_set_aside_c_debug_dir(dir).contains('did not start')
	assert v3_c_debug_dir_state_is(c_debug_dir_idle)
	assert v3_watched_c_debug_dir() == ''
	assert read_c_debug_dir(dir) == 'sources 1'
	assert os.read_file(set_aside)! == 'in the way'
	os.rm(set_aside)!
	assert v3_set_aside_c_debug_dir(dir) == ''
	assert v3_c_debug_dir_state_is(c_debug_dir_armed)
	assert v3_watched_c_debug_dir() == dir
	assert read_c_debug_dir(set_aside) == 'sources 1'
	cleanup_c_build_dir(dir)
	assert read_c_debug_dir(dir) == 'sources 1'
}
