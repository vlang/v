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
	return '{ x: ${d.x:-3} | y: ${d.y:-3} | button: .${d.button:-9} | direction: .${d.direction:-9} }'
}

pub fn read() (TerminalEvent, EventData) {
	mut buf := []byte{ len: 255 }
	len := C.read(C.STDIN_FILENO, buf.data, 255)
	buf.trim(len)
	if len == 0 { return TerminalEvent.empty, EventData{} }

	if len > 2 && buf[0] == 0x1b && buf[1] == `[` {
		e, data := ansi(buf, len)
		return e, data
	}

	return TerminalEvent.text, EventData{ bytes: buf }
}

[inline]
fn ansi(buf []byte, len int) (TerminalEvent, EventData) {
	str := buf[3..len].bytestr()
	split := str.split(';')
	if split.len < 3 { return TerminalEvent.unknown, EventData{} }

	typ, x, y := split[0].int(), split[1].int(), split[2].int()
	match typ {
		0, 2 {
			last := str[len - 4]
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
		} else {
			return TerminalEvent.unknown, EventData{}
		}
	}
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

	println('\x1b[?1003l\x1b[?1015l\x1b[?1006l\x1b[0J\x1b[?25h')
}

// TODO Change this, this whole range thing is a hack
fn range(start, stop int) []int {
	mut arr := []int{ cap: stop - start }
	for i in 0..stop-start { arr << i + start }
	return arr
}
pub struct SetupCfg {
	reset []int = range(1, 33).filter(it !in [28])
}
pub fn setup(cfg SetupCfg) {
	setup_console()
	C.atexit(reset_console)
	for code in cfg.reset { os.signal(code, fn() { reset_console() exit(0) }) }
}
