module input

import os

pub enum TerminalEvent {
	unknown
	empty // There was nothing in stdin
	mouse_down
	mouse_up
	mouse_move
	mouse_drag
	mouse_scroll
	text
}

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

pub struct EventData {
pub:
	x         int
	y         int
	button    MouseButton
	direction Direction
	key_code  int
	bytes     []byte
}

pub fn (d EventData) str() string {
	return '{ x: ${d.x:-3} | y: ${d.y:-3} | button: .${d.button:-9} | direction: .${d.direction:-9} | bytes: ${d.bytes.str()} }'
}

struct TermInput {
	buf_size  int
mut:
	buf       []byte
}

// TODO: Find a safer/better way of doing this
[inline]
fn (mut ti TermInput) resize_arr(size int) {
	mut l := &ti.buf.len
	unsafe { *l = size }
}

pub fn (mut ti TermInput) read() (TerminalEvent, EventData) {
	if ti.buf.len > 0 {
		if ti.buf.len < ti.buf_size - 1 {
			// The buffer had remaining data from the previous read, fill its remaining space
			unsafe {
				new_len := C.read(C.STDIN_FILENO, ti.buf.data + ti.buf.len, ti.buf_size - ti.buf.len)
				ti.resize_arr(ti.buf.len + new_len)
			}
		}
	} else {
		len := C.read(C.STDIN_FILENO, ti.buf.data, ti.buf_size)
		ti.resize_arr(len)
		if len == 0 { return TerminalEvent.empty, EventData{} }
		mut l := &ti.buf.len
		unsafe { *l = len }
		// println('NEW DATA - $len $ti.buf.len')		
	}

	if ti.buf.len > 2 && ti.buf[0] == 0x1b && ti.buf[1] == `[` {
		// return TerminalEvent.mouse_move, EventData{ bytes: ti.buf.clone() }
		e, data := ti.ansi()
		return e, data
	}
	ret_buf := ti.buf.clone()
	ti.resize_arr(0)
	return TerminalEvent.text, EventData{ bytes: ret_buf }
}

[inline]
// shifts the array left, to remove any data that was just read, and updates its len
fn (mut ti TermInput) shift(len int) {
	unsafe {
		C.memmove(ti.buf.data, ti.buf.data + len, ti.buf_size - len)
		ti.resize_arr(ti.buf.len - len)
	}
}

// [direct_array_access]
fn (mut ti TermInput) ansi() (TerminalEvent, EventData) {
	mut buf := ti.buf

	mut i := 2
	// Get all data up to the next letter (== ending of the escape sequence)
	for !buf[i++].is_letter() {
		if i >= buf.len {
			// TODO: possibility of infinite loop? clear / grow buffer if that's the case
			return TerminalEvent.empty, EventData{}
		}
	}

	data := buf[..i]

	if data.len > 3 && data[2] == `<` { // Mouse control
		str := data[3..].bytestr()
		split := str.split(';')
		typ, x, y := split[0].int(), split[1].int(), split[2].int()
		ti.shift(i)

		match typ {
			0, 2 {
				last := str[str.len - 1]
				button := if typ == 0 { MouseButton.primary } else { MouseButton.secondary }
				event := if last == `M` { TerminalEvent.mouse_down } else { TerminalEvent.mouse_up }
				return event, EventData{ x: x, y: y, button: button }
			}
			32, 34 {
				button := if typ == 32 { MouseButton.primary } else { MouseButton.secondary }
				return TerminalEvent.mouse_drag, EventData{ x: x, y: y, button: button }
			}
			35 {
				return TerminalEvent.mouse_move, EventData{ x: x, y: y }
			}
			64, 65 {
				direction := if typ == 64 { Direction.down } else { Direction.up }
				return TerminalEvent.mouse_scroll, EventData{ x: x, y: y, direction: direction }
			} else {}
		}
		unsafe {
			str.free()
			split.free()
		}
	}

	unsafe {
		data.free()
		buf.free()
	}

	return TerminalEvent.unknown, EventData{}
}

#include <termios.h>

fn C.tcgetattr()
fn C.tcsetattr()
struct C.termios {
mut:
	c_lflag u32
	c_cc    [32]byte
}

fn setup_console() {
	mut termios := C.termios{}
	C.tcgetattr(C.STDIN_FILENO, &termios)
	// Set raw input mode by unsetting ICANON and ECHO
	termios.c_lflag &= u32(~C.ICANON)
	termios.c_lflag &= u32(~C.ECHO)
	// Prevent stdin from blocking by making its read time 0
	termios.c_cc[C.VTIME] = 0
	termios.c_cc[C.VMIN] = 0
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)
	println('\x1b[?1003h\x1b[?1015h\x1b[?1006h')
}

fn reset_console() {
	mut termios := C.termios{}
	C.tcgetattr(C.STDIN_FILENO, &termios)
	// Set ICANON and ECHO back to normal on exit
	termios.c_lflag |= u32(C.ICANON)
	termios.c_lflag |= u32(C.ECHO)
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)
	println('\x1b[?1003l\x1b[?1015l\x1b[?1006l\x1b[0J\x1b[?25h')
}

pub struct SetupCfg {
	buf_size int = 32
	// All kill signals
	reset    []int = [1, 2, 3, 4, 6, 7, 8, 9, 11, 13, 14, 15, 19]
}
pub fn init(cfg SetupCfg) TermInput {
	mut ti := TermInput{
		buf_size: cfg.buf_size,
		buf: []byte { cap: cfg.buf_size }
	}
	C.atexit(reset_console)
	setup_console()
	for code in cfg.reset { 
		os.signal(code, fn() {
			exit(0)
		})
	}
	return ti
}
