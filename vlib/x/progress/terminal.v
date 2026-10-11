module progress

import os
import sync.stdatomic
import term

// The escape sequences are written by hand rather than calling
// term.hide_cursor() / term.show_cursor() because those always print to stdout
// and one call at a time. The bars draw on a configurable fd (stderr by
// default) and emit each frame as a single write.
const hide_cursor_seq = '\x1b[?25l'
const show_cursor_seq = '\x1b[?25h'

// The fd whose cursor we currently have hidden, or -1. Atomic because it is
// read from signal handlers and at-exit callbacks.
const hidden_cursor_fd = stdatomic.new_atomic(i64(-1))
const hooks_installed = stdatomic.new_atomic(i64(0))

// is_interactive reports whether escape sequences can be drawn on `fd`.
//
// For stdout and stderr this is the standard library's own check
// (term.can_show_color_on_*), so it follows the same rules as the rest of V:
// the VCOLORS override, TERM=dumb, and on Windows whether the console really
// has virtual-terminal processing enabled. (It is not about colour, and
// NO_COLOR does not apply.) Any other fd gets a plain terminal test.
fn is_interactive(fd int) bool {
	return match fd {
		1 { term.can_show_color_on_stdout() }
		2 { term.can_show_color_on_stderr() }
		else { os.is_atty(fd) > 0 && os.getenv('TERM') != 'dumb' }
	}
}

// restore_cursor_now shows the cursor again if (and only if) we hid it.
// Safe to call any number of times, from any thread.
fn restore_cursor_now() {
	mut h := hidden_cursor_fd
	fd := h.load()
	if fd >= 0 {
		os.fd_write(int(fd), show_cursor_seq)
	}
}

fn on_terminating_signal(sig os.Signal) {
	// The live block is still on screen with the cursor at the end of its
	// last line; move to a fresh line so the shell prompt does not land on it.
	mut h := hidden_cursor_fd
	fd := h.load()
	if fd >= 0 {
		os.fd_write(int(fd), show_cursor_seq + '\n')
		h.store(i64(-1))
	}
	// Conventional shell exit status for "killed by signal N": 128 + N
	// (130 for Ctrl+C). Exits at once: this runs inside a signal handler.
	exit_immediately(128 + int(sig))
}

// hook_signal makes `sig` restore the cursor and exit, unless the application
// already has its own disposition for it (a handler, or "ignore"). Like the
// standard library's term.show_cursor_on_exit, that is left alone: we put it
// back and rely on the at-exit hook, which still restores the cursor if the
// application's handler ends the program with exit().
fn hook_signal(sig os.Signal) {
	prev := os.signal_opt(sig, on_terminating_signal) or { return }
	if voidptr(prev) != unsafe { nil } {
		os.signal_opt(sig, prev) or {}
	}
}

// hide_cursor_on hides the cursor on `fd` and, the first time it is called,
// arranges for it to come back on normal exit, and on SIGINT / SIGTERM when
// the program has not set up handling for those itself.
fn hide_cursor_on(fd int) {
	mut h := hidden_cursor_fd
	h.store(i64(fd))
	os.fd_write(fd, hide_cursor_seq)
	mut installed := hooks_installed
	if installed.compare_and_swap(0, 1) {
		at_exit(fn () {
			restore_cursor_now()
		}) or {}
		hook_signal(.int)
		hook_signal(.term)
	}
}

fn show_cursor_on(fd int) {
	os.fd_write(fd, show_cursor_seq)
	mut h := hidden_cursor_fd
	h.store(i64(-1))
}
