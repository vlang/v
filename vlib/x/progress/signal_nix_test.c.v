import os
import time
import x.progress

const progress_signal_probe = '--vtest-progress-signal'

#include <signal.h>

fn C.raise(i32) i32

fn app_handler(_ os.Signal) {
	exit(42)
}

fn run_progress_signal_probe(mode string) {
	sig := if mode.ends_with('term') { os.Signal.term } else { os.Signal.int }
	if mode.starts_with('handler') {
		os.signal_opt(sig, app_handler) or { panic(err) }
	} else if mode.starts_with('ignore') {
		os.signal_ignore(sig)
	}
	mut mb := progress.MultiBar.new(force: true, delay: 5 * time.millisecond)
	mut sp := mb.add_spinner(text: 'work')
	mb.start()
	time.sleep(30 * time.millisecond)
	C.raise(i32(sig))
	time.sleep(30 * time.millisecond)
	sp.finish()
	mb.wait()
}

fn testsuite_begin() {
	if os.args.len == 3 && os.args[1] == progress_signal_probe {
		run_progress_signal_probe(os.args[2])
		exit(0)
	}
}

// The display hides the cursor, so it must give it back however the program
// ends, while leaving the application's own signal handling alone (the same
// contract as term.show_cursor_on_exit).
fn test_cursor_is_restored_and_application_signal_handling_is_preserved() {
	binary := os.executable()
	hide := '\x1b[?25l'
	show := '\x1b[?25h'
	for mode in ['default-int', 'default-term', 'handler-int', 'handler-term', 'ignore-int',
		'ignore-term'] {
		result := os.exec([binary, progress_signal_probe, mode])
		assert result.output.contains(hide), '${mode}: ${result.output}'
		assert result.output.contains(show), '${mode}: ${result.output}'
		if mode.starts_with('default') {
			// no application handling: we restore the cursor, move to a fresh
			// line, and exit like a shell expects after the signal (128 + N)
			assert result.exit_code == if mode.ends_with('int') { 130 } else { 143 }, '${mode}: ${result.output}'
			assert result.output.contains('${show}\n'), '${mode}: ${result.output}'
		} else if mode.starts_with('handler') {
			// the application's handler ran, not ours; the cursor still came
			// back through the at-exit hook
			assert result.exit_code == 42, '${mode}: ${result.output}'
		} else {
			// the application ignores the signal, so the program ran to the end
			assert result.exit_code == 0, '${mode}: ${result.output}'
			assert result.output.count(show) == 1, '${mode}: ${result.output}'
		}
	}
}
