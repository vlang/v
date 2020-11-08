module term_input

import os
// import time

pub enum Direction {
	unknown
	up
	down
	left
	right
}

pub enum MouseButton {
	unknown
	primary
	secondary
}

pub enum EventType {
	unknown
	mouse_down
	mouse_up
	mouse_move
	mouse_drag
	mouse_scroll
	key_down
}

pub struct Event {
pub:
	typ       EventType

	// Mouse event info
	x         int
	y         int
	button    MouseButton
	direction Direction

	// Keyboard event info
	code      KeyCode
	modifiers u32
	ascii     byte
	utf8      string
}

pub struct Context {
pub:
	cfg 		 Config
mut:
	termios      &C.termios = 0
	buf          []byte
	// init_called  bool
	// quit_ordered bool
pub mut:
	frame_count  u64
}

pub struct Config {
	user_data      voidptr
	init_fn        fn(voidptr)
	frame_fn       fn(voidptr)
	event_fn       fn(&Event, voidptr)

	buffer_size    int = 256
	frame_rate     int = 30
	use_x11        bool

	capture_events bool
	// All kill signals
	reset          []int = [1, 2, 3, 4, 6, 7, 8, 9, 11, 13, 14, 15, 19]

}

pub fn init(cfg Config) &Context {
	mut res := &Context{
		cfg: cfg,
		buf: []byte{ cap: cfg.buffer_size }
	}

	// Reset console on exit
	C.atexit(termios_reset)
	for code in cfg.reset {
		os.signal(code, fn() {
			exit(0)
		})
	}

	return res
}

pub fn (mut ctx Context) run() {
	if ctx.cfg.use_x11 {
		eprintln('error: x11 backend not implemented yet')
		exit(1)
	} else {
		ctx.termios_setup()
		ctx.termios_loop()
	}
}

[inline]
// shifts the array left, to remove any data that was just read, and updates its len
// TODO: remove
fn (mut ctx Context) shift(len int) {
	unsafe {
		C.memmove(ctx.buf.data, ctx.buf.data + len, ctx.buf.cap - len)
		ctx.resize_arr(ctx.buf.len - len)
	}
}

// TODO: don't actually do this, lmao
[inline]
fn (mut ctx Context) resize_arr(size int) {
	mut l := &ctx.buf.len
	unsafe { *l = size }
}
