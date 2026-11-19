import os
import term

const cursor_exit_probe = '--vtest-cursor-exit'

#include <signal.h>
#include <unistd.h>

fn C.raise(i32) i32
fn C._exit(i32)

fn cursor_shutdown_handler(_ os.Signal) {
	C._exit(23)
}

fn run_cursor_exit_probe(mode string) {
	signal := if mode.ends_with('int') { os.Signal.int } else { os.Signal.term }
	if mode.starts_with('handler') {
		os.signal_opt(signal, cursor_shutdown_handler) or { panic(err) }
	} else if mode == 'ignore' {
		os.signal_ignore(.int, .term)
	}
	term.show_cursor_on_exit()
	term.hide_cursor()
	if mode == 'exit' {
		exit(7)
	}
	if mode == 'ignore' {
		C.raise(i32(os.Signal.int))
		C.raise(i32(os.Signal.term))
	} else if mode != 'normal' {
		C.raise(i32(signal))
	}
}

fn testsuite_begin() {
	if os.args.len == 3 && os.args[1] == cursor_exit_probe {
		run_cursor_exit_probe(os.args[2])
		exit(0)
	}
}

fn test_show_cursor_on_exit_preserves_exit_status_and_signal_handlers() {
	binary := os.executable()
	for mode in ['normal', 'exit', 'default-int', 'default-term', 'handler-int', 'handler-term',
		'ignore'] {
		result := os.exec([binary, cursor_exit_probe, mode])
		if mode in ['normal', 'exit', 'ignore'] {
			assert result.exit_code == if mode == 'exit' { 7 } else { 0 }, result.output
			assert result.output == '\x1b[?25l\x1b[?25h', result.output
		} else {
			assert result.exit_code != 0, mode
			if mode.starts_with('handler') {
				assert result.exit_code == 23, result.output
			}
			assert !result.output.contains('\x1b[?25h'), result.output
		}
	}
}
