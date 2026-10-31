import os
import term

#include <signal.h>
#include <unistd.h>

fn C.raise(i32) i32
fn C._exit(i32)

fn cursor_shutdown_handler(_ os.Signal) {
	C._exit(23)
}

fn main() {
	mode := os.args[1]
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
