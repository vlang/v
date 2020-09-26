module input

import os

pub enum TerminalEvent {
	unknown
	mouse_down
	mouse_up
	mouse_move
	mouse_drag
	mouse_scroll
	// The rest are unimplemented
	key_down
	key_up
	empty // There was nothing in stdin
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
}

pub fn (d EventData) str() string {
	return '{ x: ${d.x:-3} | y: ${d.y:-3} | button: .${d.button:-9} | direction: .${d.direction:-9} }'
}

pub fn read() (TerminalEvent, EventData) {
	mut buf := []byte{ len: 16 } // byteptr(malloc(16))
	nr_chars := C.read(C.STDIN_FILENO, buf.data, 16)
	if nr_chars < 3 { return TerminalEvent.unknown, EventData{} }

	str := buf[3..nr_chars].bytestr()
	split := str.split(';')
	if split.len < 3 { return TerminalEvent.unknown, EventData{} }

	x, y := split[1].int(), split[2].int()
	if split[0] == '0' {
		last := str[nr_chars - 4]
		if last == `M` { return TerminalEvent.mouse_down, EventData{ x: x, y: y, button: .primary } }
		if last == `m` { return TerminalEvent.mouse_up,   EventData{ x: x, y: y, button: .primary } }
	} else if split[0] == '2' {
		last := str[nr_chars - 4]
		if last == `M` { return TerminalEvent.mouse_down, EventData{ x: x, y: y, button: .secondary } }
		if last == `m` { return TerminalEvent.mouse_up,   EventData{ x: x, y: y, button: .secondary } }
	} else if split[0] == '32' {
		return TerminalEvent.mouse_drag, EventData{ x: x, y: y, button: .primary }
	} else if split[0] == '34' {
		return TerminalEvent.mouse_drag, EventData{ x: x, y: y, button: .secondary }
	} else if split[0] == '35' {
		return TerminalEvent.mouse_move, EventData{ x: x, y: y }
	} else if split[0] == '64' {
		return TerminalEvent.mouse_scroll, EventData{ x: x, y: y, direction: .up }
	} else if split[0] == '65' {
		return TerminalEvent.mouse_scroll, EventData{ x: x, y: y, direction: .down }
	}

	return TerminalEvent.unknown, EventData{}
}

fn range(start, stop int) []int {
	mut arr := []int{ cap: stop - start }
	for i in 0..stop-start { arr << i + start }
	return arr
}

pub struct SetupCfg {
	reset []int = range(1, 33)
}
pub fn setup(cfg SetupCfg) {
	setup_console()
	C.atexit(reset_console)
	for code in cfg.reset { os.signal(code, fn() { reset_console() exit(0) }) }
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
	// Prevent stdin from blocking by makking its read time 0
	termios.c_cc[C.VTIME] = 0
	termios.c_cc[C.VMIN] = 0
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)

	println('\x1b[?1003h\x1b[?1015h\x1b[?1006h')
}

fn reset_console() {
	mut termios := C.termios{}
	C.tcgetattr(C.STDIN_FILENO, &termios)
	// Set ICANON and ECHO back to normal on exit
	termios.c_lflag &= u32(C.ICANON)
	termios.c_lflag &= u32(C.ECHO)
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)

	println('\x1b[?1003l\x1b[?1015l\x1b[?1006l\x1b[0J')
}
