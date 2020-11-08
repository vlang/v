module term_input

// import os
import time

#include <termios.h>

fn C.tcgetattr()
fn C.tcsetattr()
struct C.termios {
mut:
	c_lflag u32
	c_cc    [32]byte
}

const (
	termios_at_startup = get_termios()
)

[inline]
fn get_termios() &C.termios {
	mut t := &C.termios{}
	C.tcgetattr(C.STDIN_FILENO, t)
	return t
}

fn termios_setup() &C.termios {
	mut termios := get_termios()
	// Set raw input mode by unsetting ICANON and ECHO
	termios.c_lflag &= u32(~C.ICANON)
	termios.c_lflag &= u32(~C.ECHO)
	// Prevent stdin from blocking by making its read time 0
	termios.c_cc[C.VTIME] = 0
	termios.c_cc[C.VMIN] = 0
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, termios)
	println('\x1b[?1003h\x1b[?1015h\x1b[?1006h')

	return termios
}

fn termios_reset() {
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH /* C.TCSANOW ?? */, termios_at_startup)
	println('\x1b[?1003l\x1b[?1015l\x1b[?1006l\x1b[0J\x1b[?25h')
}

///////////////////////////////////////////

fn (mut ctx Context) termios_loop() {
	frame_time := 1_000_000 / ctx.cfg.frame_rate
	mut init_called := false
	mut sw := time.new_stopwatch(auto_start: false)
	mut sleep_len := 0
	for {
		if !init_called {
			if ctx.cfg.init_fn != voidptr(0) {
				ctx.cfg.init_fn(ctx.cfg.user_data)
			}
			init_called = true
		}
		// println('SLEEPING: $sleep_len')
		time.usleep(sleep_len)
		sw.restart()
		if ctx.cfg.event_fn != voidptr(0) {
			len := C.read(C.STDIN_FILENO, ctx.buf.data, ctx.cfg.buffer_size - ctx.buf.len)
			if len > 0 {
				ctx.resize_arr(len)
				ctx.parse_events()
			}
		}
		if ctx.cfg.frame_fn != voidptr(0) {
			ctx.cfg.frame_fn(ctx.cfg.user_data)
		}

		sw.pause()
		e := sw.elapsed().microseconds()
		sleep_len = frame_time - int(e)
	}
}

fn (mut ctx Context) parse_events() {

	// Stop this from getting stuck
	mut nr_iters := 0
	for ctx.buf.len > 0 {
		nr_iters++
		if nr_iters > 100 { ctx.shift(1) }
		mut event := &Event(0)

		if ctx.buf[0] == 0x1b {
			e, len := escape_sequence(ctx.buf.bytestr())
			event = e
			ctx.shift(len)
		} else {
			event = single_char(ctx.buf.bytestr())
			ctx.shift(1)
		}

		if event != 0 {
			ctx.cfg.event_fn(event, ctx.cfg.user_data)
			nr_iters = 0
		}
	}
}
