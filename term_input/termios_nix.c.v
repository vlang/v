module term_input

// import os
import time

#include <termios.h>

fn C.tcgetattr()
fn C.tcsetattr()
struct C.termios {
mut:
	c_iflag u32
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

fn (mut ctx Context) termios_setup() {
	mut termios := get_termios()

	if ctx.cfg.capture_events {
		// Set raw input mode by unsetting ICANON and ECHO,
		// as well as disable e.g. ctrl+c and ctrl.z
		termios.c_iflag &= ~u32(C.IGNBRK | C.BRKINT | C.PARMRK)
		termios.c_lflag &= ~u32(C.ICANON | C.ISIG | C.ECHO | C.IEXTEN | C.TOSTOP)
	} else {
		// Set raw input mode by unsetting ICANON and ECHO
		termios.c_lflag &= ~u32(C.ICANON | C.ECHO)	
	}
	// Prevent stdin from blocking by making its read time 0
	termios.c_cc[C.VTIME] = 0
	termios.c_cc[C.VMIN] = 0
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, termios)
	println('\x1b[?1003h\x1b[?1015h\x1b[?1006h')

	ctx.termios = termios
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
			ctx.init()
			init_called = true
		}
		// println('SLEEPING: $sleep_len')
		if sleep_len > 0 {
			time.usleep(sleep_len)
		}
		sw.restart()
		if ctx.cfg.event_fn != voidptr(0) {
			len := C.read(C.STDIN_FILENO, ctx.buf.data, ctx.buf.cap - ctx.buf.len)
			if len > 0 {
				ctx.resize_arr(len)
				ctx.parse_events()
			}
		}
		ctx.frame()
		sw.pause()
		e := sw.elapsed().microseconds()
		sleep_len = frame_time - int(e)

		ctx.frame_count++
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
			ctx.event(event)
			nr_iters = 0
		}
	}
}
