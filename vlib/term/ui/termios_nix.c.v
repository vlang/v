// Copyright (c) 2020 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ui

import os
import time

#include <termios.h>
#include <sys/ioctl.h>
#include <signal.h>

fn C.tcgetattr()
fn C.tcsetattr()
fn C.ioctl(fd int, request u64, arg voidptr) int

struct C.termios {
mut:
	c_iflag u32
	c_lflag u32
	c_cc    [32]byte
}

struct C.winsize {
	ws_row u16
	ws_col u16
}

const (
	termios_at_startup = get_termios()
)

[inline]
fn get_termios() C.termios {
	mut t := C.termios{}
	C.tcgetattr(C.STDIN_FILENO, &t)
	return t
}

[inline]
fn get_terminal_size() (u16, u16) {
	winsz := C.winsize{}
	C.ioctl(0, C.TIOCGWINSZ, &winsz)
	return winsz.ws_row, winsz.ws_col
}

fn restore_terminal_state() {
	termios_reset()
	mut c := ctx_ptr
	if c != 0 {
		c.paused = true
		c.load_title()
	}
	os.flush()
}

fn (mut ctx Context) termios_setup() ? {
	// store the current title, so restore_terminal_state can get it back
	ctx.save_title()

	if !ctx.cfg.skip_init_checks && !(is_atty(C.STDIN_FILENO) != 0 && is_atty(C.STDOUT_FILENO) != 0) {
		return error('not running under a TTY')
	}

	mut termios := get_termios()

	if ctx.cfg.capture_events {
		// Set raw input mode by unsetting ICANON and ECHO,
		// as well as disable e.g. ctrl+c and ctrl.z
		termios.c_iflag &= ~u32(C.IGNBRK | C.BRKINT | C.PARMRK | C.IXON)
		termios.c_lflag &= ~u32(C.ICANON | C.ISIG | C.ECHO | C.IEXTEN | C.TOSTOP)
	} else {
		// Set raw input mode by unsetting ICANON and ECHO
		termios.c_lflag &= ~u32(C.ICANON | C.ECHO)
	}

	if ctx.cfg.hide_cursor {
		print('\x1b[?25l')
	}

	if ctx.cfg.window_title != '' {
		print('\x1b]0;$ctx.cfg.window_title\x07')
	}

	if !ctx.cfg.skip_init_checks {
		// prevent blocking during the feature detections, but allow enough time for the terminal
		// to send back the relevant input data
		termios.c_cc[C.VTIME] = 1
		termios.c_cc[C.VMIN] = 0
		C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)
		// feature-test the SU spec
		sx, sy := get_cursor_position()
		print('$bsu$esu')
		ex, ey := get_cursor_position()
		if sx == ex && sy == ey {
			// the terminal either ignored or handled the sequence properly, enable SU
			ctx.enable_su = true
		} else {
			ctx.draw_line(sx, sy, ex, ey)
			ctx.set_cursor_position(sx, sy)
			ctx.flush()
		}
		// feature-test rgb (truecolor) support
		ctx.enable_rgb = supports_truecolor()
	}

	// Prevent stdin from blocking by making its read time 0
	termios.c_cc[C.VTIME] = 0
	termios.c_cc[C.VMIN] = 0
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH, &termios)
	// enable mouse input
	print('\x1b[?1003h\x1b[?1006h')
	if ctx.cfg.use_alternate_buffer {
		// switch to the alternate buffer
		print('\x1b[?1049h')
		// clear the terminal and set the cursor to the origin
		print('\x1b[2J\x1b[3J\x1b[1;1H')
	}
	ctx.window_height, ctx.window_width = get_terminal_size()

	// Reset console on exit
	C.atexit(restore_terminal_state)
	os.signal(C.SIGTSTP, restore_terminal_state)
	os.signal(C.SIGCONT, fn () {
		mut c := ctx_ptr
		if c != 0 {
			c.termios_setup()
			c.window_height, c.window_width = get_terminal_size()
			mut event := &Event{
				typ: .resized
				width: c.window_width
				height: c.window_height
			}
			c.paused = false
			c.event(event)
		}
	})
	for code in ctx.cfg.reset {
		os.signal(code, fn() {
			mut c := ctx_ptr
			if c != 0 {
				c.cleanup()
			}
			exit(0)
		})
	}

	os.signal(C.SIGWINCH, fn() {
		mut c := ctx_ptr
		if c != 0 {
			c.window_height, c.window_width = get_terminal_size()

			mut event := &Event{
				typ: .resized
				width: c.window_width
				height: c.window_height
			}
			c.event(event)
		}
	})

	os.flush()
}

fn get_cursor_position() (int, int) {
	print('\033[6n')
	buf := malloc(25)
	len := C.read(C.STDIN_FILENO, buf, 24)
	unsafe { buf[len] = 0 }
	s := tos(buf, len)
	a := s[2..].split(';')
	if a.len != 2 { return -1, -1 }
	return a[0].int(), a[1].int()
}

fn supports_truecolor() bool {
	// faster/simpler, but less reliable, check
	if os.getenv('COLORTERM') in ['truecolor', '24bit'] {
		return true
	}
	// set the bg color to some arbirtrary value (#010203), assumed not to be the default
	print('\x1b[48:2:1:2:3m')
	// andquery the current color
	print('\x1bP\$qm\x1b\\')
	buf := malloc(25)
	len := C.read(C.STDIN_FILENO, buf, 24)
	unsafe { buf[len] = 0 }
	s := tos(buf, len)
	return '1:2:3' in s

}

fn termios_reset() {
	C.tcsetattr(C.STDIN_FILENO, C.TCSAFLUSH /* C.TCSANOW ?? */, &termios_at_startup)
	print('\x1b[?1003l\x1b[?1006l\x1b[?25h')
	c := ctx_ptr
	if c != 0 && c.cfg.use_alternate_buffer {
		print('\x1b[?1049l')
	}
	os.flush()
}

///////////////////////////////////////////

// TODO: do multiple sleep/read cycles, rather than one big one
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
		if !ctx.paused {
			sw.restart()
			if ctx.cfg.event_fn != voidptr(0) {
				unsafe {
					len := C.read(C.STDIN_FILENO, byteptr(ctx.read_buf.data) + ctx.read_buf.len, ctx.read_buf.cap - ctx.read_buf.len)
					ctx.resize_arr(ctx.read_buf.len + len)
				}
				if ctx.read_buf.len > 0 {
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
}

fn (mut ctx Context) parse_events() {
	// Stop this from getting stuck in rare cases where something isn't parsed correctly
	mut nr_iters := 0
	for ctx.read_buf.len > 0 {
		nr_iters++
		if nr_iters > 100 { ctx.shift(1) }
		mut event := &Event(0)
		if ctx.read_buf[0] == 0x1b {
			e, len := escape_sequence(ctx.read_buf.bytestr())
			event = e
			ctx.shift(len)
		} else {
			event = single_char(ctx.read_buf.bytestr())
			ctx.shift(1)
		}
		if event != 0 {
			ctx.event(event)
			nr_iters = 0
		}
	}
}

fn single_char(buf string) &Event {
	ch := buf[0]

	mut event := &Event{
		typ: .key_down
		ascii: ch
		code: KeyCode(ch)
		utf8: buf
	}

	match ch {
		// special handling for `ctrl + letter`

		// TODO: Fix assoc in V and remove this workaround :/
		// 1  ... 26 { event = { event | code: KeyCode(96 | ch), modifiers: ctrl  } }
		// 65 ... 90 { event = { event | code: KeyCode(32 | ch), modifiers: shift } }


		// The bit `or`s here are really just `+`'s, just written in this way for a tiny performance improvement
		// don't treat tab, enter as ctrl+i, ctrl+j
		1  ... 8, 11 ... 26 { event = &Event{ typ: event.typ, ascii: event.ascii, utf8: event.utf8, code: KeyCode(96 | ch), modifiers: ctrl } }
		65 ... 90 { event = &Event{ typ: event.typ, ascii: event.ascii, utf8: event.utf8, code: KeyCode(32 | ch), modifiers: shift } }

		else {}
	}

	return event
}

[inline]
// Gets an entire, independent escape sequence from the buffer
// Normally, this just means reading until the first letter, but there are some exceptions...
fn escape_end(buf string) int {
	mut i := 0
	for {
		if i + 1 == buf.len { return buf.len }

		if buf[i].is_letter() || buf[i] == `~` {
			if buf[i] == `O` && i + 2 <= buf.len {
				n := buf[i+1]
				if (n >= `A` && n <= `D`) || (n >= `P` && n <= `S`) || n == `F` || n == `H` {
					return i + 2
				}
			}
			return i + 1
		// escape hatch to avoid potential issues/crashes, although ideally this should never eval to true
		} else if buf[i + 1] == 0x1b { return i + 1 }
		i++
	}
	// this point should be unreachable
	assert false
	return 0
}

fn escape_sequence(buf_ string) (&Event, int) {
	end := escape_end(buf_)
	single := buf_[..end] // read until the end of the sequence
	buf := single[1..] // skip the escape character

	if buf.len == 0 {
		return &Event{
			typ: .key_down
			ascii: 27
			code: .escape
			utf8: single
		}, 1
	}

	if buf.len == 1 {
		c := single_char(buf)
		// return { c | modifiers: c.modifiers | alt }, 2

		return &Event{
			typ: c.typ
			ascii: c.ascii
			code: c.code
			utf8: single
			modifiers: c.modifiers | alt
		}, 2
	}

	// ----------------
	//   Mouse events
	// ----------------

	// Documentation: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking
	if buf.len > 2 && buf[1] == `<` {
		split := buf[2..].split(';')
		if split.len < 3 { return &Event(0), 0 }

		typ, x, y := split[0].int(), split[1].int(), split[2].int()
		lo := typ & 0b00011
		hi := typ & 0b11100

		mut modifiers := u32(0)
		if hi & 4  != 0 { modifiers |= shift }
		if hi & 8  != 0 { modifiers |= alt }
		if hi & 16 != 0 { modifiers |= ctrl }

		match typ {
			0...31 {
				last := buf[buf.len - 1]
				button := if lo < 3 { MouseButton(lo + 1) } else { MouseButton.unknown }
				event := if last == `m` || lo == 3 { EventType.mouse_up } else { EventType.mouse_down }

				return &Event{ typ: event, x: x, y: y, button: button, modifiers: modifiers utf8: single }, end
			}
			32...63 {
				button, event := if lo < 3 {
						MouseButton(lo + 1), EventType.mouse_drag
					} else {
						MouseButton.unknown, EventType.mouse_move
					}

				return &Event{ typ: event, x: x, y: y, button: button, modifiers: modifiers, utf8: single }, end
			}
			64...95 {
				direction := if typ & 1 == 0 { Direction.down } else { Direction.up }
				return &Event{ typ: .mouse_scroll, x: x, y: y, direction: direction, modifiers: modifiers, utf8: single }, end
			} else {
				return &Event{ typ: .unknown, utf8: single }, end
			}
		}
	}

	// ----------------------------
	//   Special key combinations
	// ----------------------------

	mut code := KeyCode.null
	mut modifiers := u32(0)
	match buf {
		'[A', 'OA'                { code = .up }
		'[B', 'OB'                { code = .down }
		'[C', 'OC'                { code = .right }
		'[D', 'OD'                { code = .left }
		'[5~', '[[5~'             { code = .page_up }
		'[6~', '[[6~'             { code = .page_down }
		'[F', 'OF', '[4~', '[[8~' { code = .end }
		'[H', 'OH', '[1~', '[[7~' { code = .home }
		'[2~'                     { code = .insert }
		'[3~'                     { code = .delete }
		'OP', '[11~'              { code = .f1 }
		'OQ', '[12~'              { code = .f2 }
		'OR', '[13~'              { code = .f3 }
		'OS', '[14~'              { code = .f4 }
		'[15~'                    { code = .f5 }
		'[17~'                    { code = .f6 }
		'[18~'                    { code = .f7 }
		'[19~'                    { code = .f8 }
		'[20~'                    { code = .f9 }
		'[21~'                    { code = .f10 }
		'[23~'                    { code = .f11 }
		'[24~'                    { code = .f12 }
		else                      {}
	}

	if buf == '[Z' {
		code = .tab
		modifiers |= shift
	}

	if buf.len == 5 && buf[0] == `[` && buf[1].is_digit() && buf[2] == `;` {
		// code = KeyCode(buf[4] + 197)
		modifiers = match buf[3] {
			`2` { shift }
			`3` { alt }
			`4` { shift | alt }
			`5` { ctrl }
			`6` { ctrl | shift }
			`7` { ctrl | alt }
			`8` { ctrl | alt | shift }
			else { modifiers } // probably unreachable? idk, terminal events are strange
		}

		if buf[1] == `1` {
			code = match buf[4] {
				`A` { KeyCode.up }
				`B` { KeyCode.down }
				`C` { KeyCode.right }
				`D` { KeyCode.left }
				`F` { KeyCode.end }
				`H` { KeyCode.home }
				`P` { KeyCode.f1 }
				`Q` { KeyCode.f2 }
				`R` { KeyCode.f3 }
				`S` { KeyCode.f4 }
				else { code }
			}
		} else if buf[1] == `5` {
			code = KeyCode.page_up
		} else if buf[1] == `6` {
			code = KeyCode.page_down
		}
	}

	return &Event{ typ: .key_down, code: code, utf8: single, modifiers: modifiers }, end
}
