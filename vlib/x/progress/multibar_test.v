module progress

import os
import sync.stdatomic
import time

// These tests run real threads, so they never assume how fast anything is.
// Where a test needs the render thread to have drawn something, it waits for
// that to show up in the output (wait_for_output), up to a generous deadline,
// rather than sleeping a guessed amount of time.

fn ms(n int) time.Duration {
	return n * time.millisecond
}

// run_group_to_file runs `body` against a MultiBar that writes to a temporary
// file, and returns what was written. `body` gets the file's path so it can
// wait for output to appear.
fn run_group_to_file(name string, opts MultiBarOptions, body fn (mut mb MultiBar, path string)) !string {
	path := os.join_path(os.vtmp_dir(), 'x_progress_${name}_${os.getpid()}.log')
	mut f := os.create(path)!
	defer {
		os.rm(path) or {}
	}
	mut o := opts
	o.output_fd = f.fd
	mut mb := MultiBar.new(o)
	body(mut mb, path)
	f.close()
	return os.read_file(path)!
}

// wait_for_output blocks until `needle` shows up in the file at `path`.
// The 20 second deadline is only a safety net, so a bug fails the test rather
// than hanging the test run.
fn wait_for_output(path string, needle string) {
	deadline := time.sys_mono_now() + u64(20 * time.second)
	for time.sys_mono_now() < deadline {
		if (os.read_file(path) or { '' }).contains(needle) {
			return
		}
		time.sleep(ms(2))
	}
	assert false, 'timed out waiting for ${needle.bytes()} in the output'
}

// ---------------------------------------------------------------- bars

fn test_multibar_live_output() ! {
	out := run_group_to_file('live', MultiBarOptions{
		force: true
		width: 60
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut a := mb.add_bar(20, BarOptions{ text: 'a ', on_end: 'DONE-A' })
		mut b := mb.add_bar(40, BarOptions{ text: 'b ', on_end: 'DONE-B' })
		mut c := mb.add_bar(10, BarOptions{ text: 'c ', leave: true })
		mb.start()
		mb.println('hello from a log line')
		wait_for_output(path, 'c ') // the first frame: all three rows are drawn
		a.inc() // a row changes, so the block must be redrawn in place
		wait_for_output(path, '\x1b[2A')
		a.add(19)
		b.add(40)
		c.add(10)
		mb.wait()
	})!
	// each completion is reported exactly once (the old loop re-printed it forever)
	assert out.count('DONE-A') == 1
	assert out.count('DONE-B') == 1
	assert out.count('hello from a log line') == 1
	// `leave: true` keeps the final state of c
	assert out.contains('c 100.00%')
	// cursor: hidden once, shown once, and shown last
	assert out.count('\x1b[?25l') == 1
	assert out.count('\x1b[?25h') == 1
	assert out.ends_with('\x1b[?25h')
}

fn test_multibar_without_a_terminal_prints_plain_final_lines() ! {
	out := run_group_to_file('plain', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut a := mb.add_bar(5, BarOptions{ text: 'a ', on_end: 'DONE-A' })
		mut b := mb.add_bar(5, BarOptions{ text: 'b ', length: 10 })
		mb.start()
		mb.println('a log line')
		a.add(5)
		b.add(5)
		mb.wait()
	})!
	assert !out.contains('\x1b') // no escape codes in a log file
	assert !out.contains('\r')
	assert out.count('DONE-A') == 1
	assert out.contains('a log line')
	// b has no on_end; with no live display it still gets a summary line
	assert out.contains('b 100.00% [==========] 00:00')
}

fn test_nonterminal_output_strips_styles_from_summaries_and_logs() ! {
	out := run_group_to_file('plain_ansi', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mb.println('\x1b[31mbefore\x1b[0m')
		mut b := mb.add_bar(1, text: '\x1b[31mbar \x1b[0m', on_end: '\x1b[32mDONE\x1b[0m')
		mb.start()
		mb.println('\x1b]8;;https://example.test\x1b\\during\x1b]8;;\x1b\\')
		b.inc()
		mb.wait()
		mb.println('\x1b(Bafter')
	})!
	assert out == 'before\nduring\nDONE\nafter\n'
}

fn test_multibar_stop_finishes_unfinished_bars() ! {
	out := run_group_to_file('stop', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut a := mb.add_bar(100, BarOptions{ length: 10 })
		mb.start()
		a.add(10)
		mb.stop() // would block forever with wait(): a never reaches 100
	})!
	assert out.contains(' 10.00%')
}

fn test_bars_can_be_added_while_running() ! {
	out := run_group_to_file('dynamic', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut a := mb.add_bar(1, BarOptions{ on_end: 'DONE-A' })
		mb.start()
		a.inc()
		wait_for_output(path, 'DONE-A') // a has left the display; the group must stay alive
		mut b := mb.add_bar(1, BarOptions{ on_end: 'DONE-B' })
		b.inc()
		mb.wait()
	})!
	assert out.count('DONE-A') == 1
	assert out.count('DONE-B') == 1
}

fn test_a_lone_items_display_ends_by_itself() ! {
	out := run_group_to_file('lone', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mb.closing = true // what Bar.new() and Spinner.new() do for their private group
		mut b := mb.add_bar(3, BarOptions{ on_end: 'LONE-DONE' })
		b.start() // an item's start() starts its group
		b.add(3)
		wait_for_output(path, 'LONE-DONE') // no wait() yet: the display ended on its own
		b.wait() // regression: used to spin forever on the plain bar
	})!
	assert out.count('LONE-DONE') == 1
}

fn test_standalone_constructors_get_a_closing_group_and_draw_nothing_yet() {
	mut b := Bar.new(5)
	assert b.max == 5
	assert b.group.closing
	mut s := Spinner.new()
	assert s.group.closing
	// nothing was started, so nothing was written and no thread exists
	assert !b.group.started
	assert !s.group.started
}

fn test_many_bars_still_finish() ! {
	out := run_group_to_file('many', MultiBarOptions{
		force: true
		width: 50
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut bars := []&Bar{}
		for i in 0 .. 30 {
			bars << mb.add_bar(10, BarOptions{ text: 'b${i} ', on_end: 'END${i}' })
		}
		mb.start()
		for _ in 0 .. 10 {
			for mut b in bars {
				b.inc()
			}
		}
		mb.wait()
	})!
	for i in 0 .. 30 {
		assert out.count('END${i}\n') == 1
	}
}

fn test_bars_finishing_in_one_frame_print_in_completion_order() ! {
	out := run_group_to_file('order', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut first_registered := mb.add_bar(1, BarOptions{ on_end: 'REGISTERED-FIRST' })
		mut second_registered := mb.add_bar(1, BarOptions{ on_end: 'REGISTERED-SECOND' })
		// Both complete before the first frame is drawn, but the one
		// registered second completes first. Wait for the clock to tick so the
		// two completion times are certainly different.
		second_registered.inc()
		t := time.sys_mono_now()
		for time.sys_mono_now() == t {
			time.sleep(100 * time.microsecond)
		}
		first_registered.inc()
		mb.start()
		mb.wait()
	})!
	second := out.index('REGISTERED-SECOND') or { -1 }
	first := out.index('REGISTERED-FIRST') or { -1 }
	assert second >= 0
	assert second < first
}

// ------------------------------------------------------------------ spinners

fn test_spinner_is_driven_like_a_bar() ! {
	out := run_group_to_file('spin_life', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut sp := mb.add_spinner(SpinnerOptions{ on_end: 'SPIN-DONE' })
		assert !sp.is_done()
		sp.start()
		time.sleep(ms(30))
		assert !sp.is_done() // unlike a bar, nothing finishes it but finish()
		sp.finish()
		assert sp.is_done()
		sp.wait() // returns once the final frame is drawn
		e := sp.elapsed()
		time.sleep(ms(20))
		assert sp.elapsed() == e // frozen
		assert e > 0
	})!
	assert out.count('SPIN-DONE') == 1
}

fn waiter(mut mb MultiBar, mut flag stdatomic.AtomicVal[i64]) {
	mb.wait()
	flag.store(1)
}

fn test_wait_blocks_until_the_spinner_is_finished() ! {
	out := run_group_to_file('spin_wait', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut sp := mb.add_spinner(SpinnerOptions{ on_end: 'SPIN-DONE' })
		mut b := mb.add_bar(2, BarOptions{ on_end: 'BAR-DONE' })
		mb.start()
		mut flag := stdatomic.new_atomic(i64(0))
		t := spawn waiter(mut mb, mut flag)
		b.add(2) // the bar is complete, the spinner is not
		wait_for_output(path, 'BAR-DONE')
		time.sleep(ms(50))
		assert flag.load() == 0 // wait() is still blocked on the spinner
		sp.finish()
		t.wait()
		assert flag.load() == 1
	})!
	assert out.count('SPIN-DONE') == 1
	assert out.count('BAR-DONE') == 1
}

fn test_stop_finishes_spinners() ! {
	out := run_group_to_file('spin_stop', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		_ := mb.add_spinner(SpinnerOptions{
			frames: ['-']
			text:   'never finishes'
		})
		mb.start()
		mb.stop() // would hang with wait()
	})!
	// not a terminal: a spinner still reports one plain summary line
	assert out.contains('✓ never finishes')
	assert !out.contains('\x1b')
}

fn test_spinners_and_bars_mix_without_a_terminal() ! {
	out := run_group_to_file('mix_plain', MultiBarOptions{
		delay: ms(5)
	}, fn (mut mb MultiBar, _ string) {
		mut sp := mb.add_spinner(SpinnerOptions{ text: 'connecting ', on_end: 'SPIN-DONE' })
		mut b := mb.add_bar(4, BarOptions{ on_end: 'BAR-DONE' })
		mb.start()
		sp.finish()
		b.add(4)
		mb.wait()
	})!
	assert out.count('SPIN-DONE') == 1
	assert out.count('BAR-DONE') == 1
	assert !out.contains('\x1b')
	assert !out.contains('\r')
}

fn test_spinners_and_bars_mix_live() ! {
	out := run_group_to_file('mix_live', MultiBarOptions{
		force: true
		width: 60
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut sp := mb.add_spinner(SpinnerOptions{
			frames:   ['<A>', '<B>']
			interval: ms(10)
			text:     'spinning '
			on_end:   'SPIN-DONE'
		})
		mut b := mb.add_bar(10, BarOptions{ text: 'bar ', on_end: 'BAR-DONE' })
		mb.start()
		wait_for_output(path, '<A> spinning')
		wait_for_output(path, '<B> spinning') // the spinner really animates
		b.add(10)
		sp.finish()
		mb.wait()
	})!
	// two live rows, redrawn in place
	assert out.contains('\x1b[1A')
	assert out.count('SPIN-DONE') == 1
	assert out.count('BAR-DONE') == 1
	assert out.count('\x1b[?25l') == 1
	assert out.ends_with('\x1b[?25h')
}

fn test_leave_keeps_a_finished_spinner() ! {
	out := run_group_to_file('spin_leave', MultiBarOptions{
		force: true
		width: 60
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut sp := mb.add_spinner(SpinnerOptions{
			frames:       ['-']
			text:         'indexing '
			leave:        true
			show_elapsed: true
		})
		mb.start()
		wait_for_output(path, '- indexing')
		sp.finish()
		mb.wait()
	})!
	assert out.contains('✓ indexing ')
}

fn test_spinner_text_can_change_while_running() ! {
	out := run_group_to_file('spin_text', MultiBarOptions{
		force: true
		width: 60
		delay: ms(5)
	}, fn (mut mb MultiBar, path string) {
		mut sp := mb.add_spinner(SpinnerOptions{
			frames: ['-']
			text:   'first'
		})
		mb.start()
		wait_for_output(path, '- first')
		sp.set_text('second')
		wait_for_output(path, '- second')
		sp.finish()
		mb.wait()
	})!
	assert out.contains('- first')
	assert out.contains('- second')
}
