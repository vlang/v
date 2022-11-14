// Copyright (c) 2020-2021 Raúl Hernández. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ui

import os
import time

const buf_size = 64

const ctx_ptr = &Context(0)

const stdin_at_startup = u32(0)

struct ExtraContext {
mut:
	stdin_handle  C.HANDLE
	stdout_handle C.HANDLE
	read_buf      [buf_size]C.INPUT_RECORD
	mouse_down    MouseButton
}

fn restore_terminal_state() {
	if unsafe { ui.ctx_ptr != 0 } {
		if ui.ctx_ptr.cfg.use_alternate_buffer {
			// clear the terminal and set the cursor to the origin
			print('\x1b[2J\x1b[3J')
			print('\x1b[?1049l')
			flush_stdout()
		}
		C.SetConsoleMode(ui.ctx_ptr.stdin_handle, ui.stdin_at_startup)
	}
	load_title()
	os.flush()
}

pub fn init(cfg Config) &Context {
	mut ctx := &Context{
		cfg: cfg
	}
	// get the standard input handle
	stdin_handle := C.GetStdHandle(C.STD_INPUT_HANDLE)
	stdout_handle := C.GetStdHandle(C.STD_OUTPUT_HANDLE)
	if stdin_handle == C.INVALID_HANDLE_VALUE {
		panic('could not get stdin handle')
	}
	// save the current input mode, to be restored on exit
	if !C.GetConsoleMode(stdin_handle, &ui.stdin_at_startup) {
		panic('could not get stdin console mode')
	}

	// enable extended input flags (see https://stackoverflow.com/a/46802726)
	// 0x80 == C.ENABLE_EXTENDED_FLAGS
	if !C.SetConsoleMode(stdin_handle, 0x80) {
		panic('could not set raw input mode')
	}
	// enable window and mouse input events.
	if !C.SetConsoleMode(stdin_handle, C.ENABLE_WINDOW_INPUT | C.ENABLE_MOUSE_INPUT) {
		panic('could not set raw input mode')
	}
	// store the current title, so restore_terminal_state can get it back
	save_title()

	if ctx.cfg.use_alternate_buffer {
		// switch to the alternate buffer
		print('\x1b[?1049h')
		// clear the terminal and set the cursor to the origin
		print('\x1b[2J\x1b[3J\x1b[1;1H')
		flush_stdout()
	}

	if ctx.cfg.hide_cursor {
		ctx.hide_cursor()
		ctx.flush()
	}

	if ctx.cfg.window_title != '' {
		print('\x1b]0;${ctx.cfg.window_title}\x07')
		flush_stdout()
	}

	unsafe {
		x := &ui.ctx_ptr
		*x = ctx
	}
	C.atexit(restore_terminal_state)
	for code in ctx.cfg.reset {
		os.signal_opt(code, fn (_ os.Signal) {
			mut c := unsafe { ui.ctx_ptr }
			if unsafe { c != 0 } {
				c.cleanup()
			}
			exit(0)
		}) or {}
	}

	ctx.stdin_handle = stdin_handle
	ctx.stdout_handle = stdout_handle
	return ctx
}

pub fn (mut ctx Context) run() ? {
	frame_time := 1_000_000 / ctx.cfg.frame_rate
	mut init_called := false
	mut sw := time.new_stopwatch(auto_start: false)
	mut sleep_len := 0
	for {
		if !init_called {
			ctx.init()
			init_called = true
		}
		if sleep_len > 0 {
			time.sleep(sleep_len * time.microsecond)
		}
		if !ctx.paused {
			sw.restart()
			if ctx.cfg.event_fn != unsafe { nil } {
				ctx.parse_events()
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
	nr_events := u32(0)
	if !C.GetNumberOfConsoleInputEvents(ctx.stdin_handle, &nr_events) {
		panic('could not get number of events in stdin')
	}
	if nr_events < 1 {
		return
	}

	// print('$nr_events | ')
	if !C.ReadConsoleInput(ctx.stdin_handle, &ctx.read_buf[0], ui.buf_size, &nr_events) {
		panic('could not read from stdin')
	}
	for i in 0 .. nr_events {
		// print('E ')
		match int(ctx.read_buf[i].EventType) {
			C.KEY_EVENT {
				e := unsafe { ctx.read_buf[i].Event.KeyEvent }
				ch := e.wVirtualKeyCode
				ascii := unsafe { e.uChar.AsciiChar }
				if e.bKeyDown == 0 {
					continue
				}
				// we don't handle key_up events because they don't exist on linux...
				// see: https://docs.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes
				code := match int(ch) {
					C.VK_BACK { KeyCode.backspace }
					C.VK_RETURN { KeyCode.enter }
					C.VK_PRIOR { KeyCode.page_up }
					14...20 { KeyCode.null }
					C.VK_NEXT { KeyCode.page_down }
					C.VK_END { KeyCode.end }
					C.VK_HOME { KeyCode.home }
					C.VK_LEFT { KeyCode.left }
					C.VK_UP { KeyCode.up }
					C.VK_RIGHT { KeyCode.right }
					C.VK_DOWN { KeyCode.down }
					C.VK_INSERT { KeyCode.insert }
					C.VK_DELETE { KeyCode.delete }
					65...90 { unsafe { KeyCode(ch + 32) } } // letters
					91...93 { KeyCode.null } // special keys
					96...105 { unsafe { KeyCode(ch - 48) } } // numpad numbers
					112...135 { unsafe { KeyCode(ch + 178) } } // f1 - f24
					else { unsafe { KeyCode(ascii) } }
				}

				mut modifiers := Modifiers{}
				if e.dwControlKeyState & (0x1 | 0x2) != 0 {
					modifiers.set(.alt)
				}
				if e.dwControlKeyState & (0x4 | 0x8) != 0 {
					modifiers.set(.ctrl)
				}
				if e.dwControlKeyState & 0x10 != 0 {
					modifiers.set(.shift)
				}

				mut event := &Event{
					typ: .key_down
					modifiers: modifiers
					code: code
					ascii: ascii
					width: int(e.dwControlKeyState)
					height: int(e.wVirtualKeyCode)
					utf8: unsafe { e.uChar.UnicodeChar.str() }
				}
				ctx.event(event)
			}
			C.MOUSE_EVENT {
				e := unsafe { ctx.read_buf[i].Event.MouseEvent }
				sb_info := C.CONSOLE_SCREEN_BUFFER_INFO{}
				if !C.GetConsoleScreenBufferInfo(ctx.stdout_handle, &sb_info) {
					panic('could not get screenbuffer info')
				}
				x := e.dwMousePosition.X + 1
				y := int(e.dwMousePosition.Y) - sb_info.srWindow.Top + 1
				mut modifiers := Modifiers{}
				if e.dwControlKeyState & (0x1 | 0x2) != 0 {
					modifiers.set(.alt)
				}
				if e.dwControlKeyState & (0x4 | 0x8) != 0 {
					modifiers.set(.ctrl)
				}
				if e.dwControlKeyState & 0x10 != 0 {
					modifiers.set(.shift)
				}
				// TODO: handle capslock/numlock/etc?? events exist for those keys
				match int(e.dwEventFlags) {
					C.MOUSE_MOVED {
						mut button := match int(e.dwButtonState) {
							0 { MouseButton.unknown }
							1 { MouseButton.left }
							2 { MouseButton.right }
							else { MouseButton.middle }
						}
						typ := if e.dwButtonState == 0 {
							if ctx.mouse_down != .unknown {
								button = ctx.mouse_down
								ctx.mouse_down = .unknown
								EventType.mouse_up
							} else {
								EventType.mouse_move
							}
						} else {
							EventType.mouse_drag
						}
						ctx.event(&Event{
							typ: typ
							x: x
							y: y
							button: button
							modifiers: modifiers
						})
					}
					C.MOUSE_WHEELED {
						ctx.event(&Event{
							typ: .mouse_scroll
							direction: if i16(e.dwButtonState >> 16) < 0 {
								Direction.up
							} else {
								Direction.down
							}
							x: x
							y: y
							modifiers: modifiers
						})
					}
					0x0008 /* C.MOUSE_HWHEELED */ {
						ctx.event(&Event{
							typ: .mouse_scroll
							direction: if i16(e.dwButtonState >> 16) < 0 {
								Direction.right
							} else {
								Direction.left
							}
							x: x
							y: y
							modifiers: modifiers
						})
					}
					0 /* CLICK */, C.DOUBLE_CLICK {
						button := match int(e.dwButtonState) {
							0 { ctx.mouse_down }
							1 { MouseButton.left }
							2 { MouseButton.right }
							else { MouseButton.middle }
						}
						ctx.mouse_down = button
						ctx.event(&Event{
							typ: .mouse_down
							x: x
							y: y
							button: button
							modifiers: modifiers
						})
					}
					else {}
				}
			}
			C.WINDOW_BUFFER_SIZE_EVENT {
				// e := unsafe { ctx.read_buf[i].Event.WindowBufferSizeEvent }
				sb := C.CONSOLE_SCREEN_BUFFER_INFO{}
				if !C.GetConsoleScreenBufferInfo(ctx.stdout_handle, &sb) {
					panic('could not get screenbuffer info')
				}
				w := sb.srWindow.Right - sb.srWindow.Left + 1
				h := sb.srWindow.Bottom - sb.srWindow.Top + 1
				utf8 := '(${ctx.window_width}, ${ctx.window_height}) -> (${w}, ${h})'
				if w != ctx.window_width || h != ctx.window_height {
					ctx.window_width, ctx.window_height = w, h
					mut event := &Event{
						typ: .resized
						width: ctx.window_width
						height: ctx.window_height
						utf8: utf8
					}
					ctx.event(event)
				}
			}
			// C.MENU_EVENT {
			// 	e := unsafe { ctx.read_buf[i].Event.MenuEvent }
			// }
			// C.FOCUS_EVENT {
			// 	e := unsafe { ctx.read_buf[i].Event.FocusEvent }
			// }
			else {}
		}
	}
}

[inline]
fn save_title() {
	// restore the previously saved terminal title
	print('\x1b[22;0t')
	flush_stdout()
}

[inline]
fn load_title() {
	// restore the previously saved terminal title
	print('\x1b[23;0t')
	flush_stdout()
}
