// Copyright (c) 2020-2024 Raul Hernandez. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// BEAM backend for term.ui
// Provides stub implementations for terminal UI on BEAM/Erlang

@[has_globals]
module ui

import strings

// Color represents an RGB color.
pub struct Color {
pub:
	r u8
	g u8
	b u8
}

// hex returns the color in hex format.
pub fn (c Color) hex() string {
	return '#${c.r.hex()}${c.g.hex()}${c.b.hex()}'
}

// ExtraContext contains fields specific to the BEAM implementation.
// On BEAM, terminal I/O is handled differently than on C/POSIX systems.
struct ExtraContext {
mut:
	read_buf []u8
	// read_all_bytes causes all the raw bytes to be read as one event unit.
	// This is crucial for UTF-8 support since Unicode codepoints can span several bytes.
	read_all_bytes bool = true
}

__global ctx_ptr = &Context(unsafe { nil })

// init initializes the terminal console with Config `cfg`.
// On BEAM: Creates a context but terminal features may be limited.
pub fn init(cfg Config) &Context {
	mut ctx := &Context{
		cfg: cfg
	}
	ctx.read_buf = []u8{cap: cfg.buffer_size}
	ctx_ptr = ctx
	return ctx
}

// run sets up and starts the terminal.
// On BEAM: Stub implementation - full terminal UI not supported.
pub fn (mut ctx Context) run() ! {
	if ctx.cfg.use_x11 {
		ctx.fail('error: x11 backend not implemented on BEAM')
		return error('x11 not supported on BEAM')
	}
	// BEAM doesn't have termios, so terminal UI is limited
	ctx.fail('error: full terminal UI not supported on BEAM backend')
	return error('terminal UI not supported on BEAM')
}

// flush displays the accumulated print buffer to the screen.
// On BEAM: Uses print() instead of C.write()
@[inline]
pub fn (mut ctx Context) flush() {
	if ctx.print_buf.len > 0 {
		// Convert buffer to string and print
		s := unsafe { tos(ctx.print_buf.data, ctx.print_buf.len) }
		print(s)
		flush_stdout()
		ctx.print_buf.clear()
	}
}

// flush_stdout flushes the standard output.
// On BEAM: Uses Erlang's io:fflush()
@[inline]
fn flush_stdout() {
	// BEAM codegen handles this - translates to io:fflush() or similar
}

// Synchronized Updates spec constants
const bsu = '\x1bP=1s\x1b\\'
const esu = '\x1bP=2s\x1b\\'

// write puts the string `s` into the print buffer.
@[inline]
pub fn (mut ctx Context) write(s string) {
	if s == '' {
		return
	}
	unsafe { ctx.print_buf.push_many(s.str, s.len) }
}

// bold sets the character state to bold.
@[inline]
pub fn (mut ctx Context) bold() {
	ctx.write('\x1b[1m')
}

// set_cursor_position positions the cursor at the given coordinates `x`,`y`.
@[inline]
pub fn (mut ctx Context) set_cursor_position(x int, y int) {
	ctx.write('\x1b[${y};${x}H')
}

// show_cursor will make the cursor appear if it is not already visible
@[inline]
pub fn (mut ctx Context) show_cursor() {
	ctx.write('\x1b[?25h')
}

// hide_cursor will make the cursor invisible
@[inline]
pub fn (mut ctx Context) hide_cursor() {
	ctx.write('\x1b[?25l')
}

// set_color sets the current foreground color used by any succeeding `draw_*` calls.
@[inline]
pub fn (mut ctx Context) set_color(c Color) {
	if ctx.enable_rgb {
		ctx.write('\x1b[38;2;${int(c.r)};${int(c.g)};${int(c.b)}m')
	} else {
		ctx.write('\x1b[38;5;${rgb2ansi(c.r, c.g, c.b)}m')
	}
}

// set_bg_color sets the current background color used by any succeeding `draw_*` calls.
@[inline]
pub fn (mut ctx Context) set_bg_color(c Color) {
	if ctx.enable_rgb {
		ctx.write('\x1b[48;2;${int(c.r)};${int(c.g)};${int(c.b)}m')
	} else {
		ctx.write('\x1b[48;5;${rgb2ansi(c.r, c.g, c.b)}m')
	}
}

// reset_color sets the current foreground color back to its default value.
@[inline]
pub fn (mut ctx Context) reset_color() {
	ctx.write('\x1b[39m')
}

// reset_bg_color sets the current background color back to its default value.
@[inline]
pub fn (mut ctx Context) reset_bg_color() {
	ctx.write('\x1b[49m')
}

// reset restores the state of all colors and text formats back to their default values.
@[inline]
pub fn (mut ctx Context) reset() {
	ctx.write('\x1b[0m')
}

// clear erases the entire terminal window and any saved lines.
@[inline]
pub fn (mut ctx Context) clear() {
	ctx.write('\x1b[2J\x1b[3J')
}

// set_window_title sets the string `s` as the window title.
@[inline]
pub fn (mut ctx Context) set_window_title(s string) {
	print('\x1b]0;${s}\x07')
	flush_stdout()
}

// draw_point draws a point at position `x`,`y`.
@[inline]
pub fn (mut ctx Context) draw_point(x int, y int) {
	ctx.set_cursor_position(x, y)
	ctx.write(' ')
}

// draw_text draws the string `s`, starting from position `x`,`y`.
@[inline]
pub fn (mut ctx Context) draw_text(x int, y int, s string) {
	ctx.set_cursor_position(x, y)
	ctx.write(s)
}

// draw_line draws a line segment, starting at point `x`,`y`, and ending at point `x2`,`y2`.
pub fn (mut ctx Context) draw_line(x int, y int, x2 int, y2 int) {
	min_x, min_y := if x < x2 { x } else { x2 }, if y < y2 { y } else { y2 }
	max_x, _ := if x > x2 { x } else { x2 }, if y > y2 { y } else { y2 }
	if y == y2 {
		// Horizontal line, performance improvement
		ctx.set_cursor_position(min_x, min_y)
		ctx.write(strings.repeat(` `, max_x + 1 - min_x))
		return
	}
	// Draw the various points with Bresenham's line algorithm
	mut x0, x1 := x, x2
	mut y0, y1 := y, y2
	sx := if x0 < x1 { 1 } else { -1 }
	sy := if y0 < y1 { 1 } else { -1 }
	dx := if x0 < x1 { x1 - x0 } else { x0 - x1 }
	dy := if y0 < y1 { y0 - y1 } else { y1 - y0 }
	mut err := dx + dy
	for {
		ctx.draw_point(x0, y0)
		if x0 == x1 && y0 == y1 {
			break
		}
		e2 := 2 * err
		if e2 >= dy {
			err += dy
			x0 += sx
		}
		if e2 <= dx {
			err += dx
			y0 += sy
		}
	}
}

// draw_dashed_line draws a dashed line segment.
pub fn (mut ctx Context) draw_dashed_line(x int, y int, x2 int, y2 int) {
	mut x0, x1 := x, x2
	mut y0, y1 := y, y2
	sx := if x0 < x1 { 1 } else { -1 }
	sy := if y0 < y1 { 1 } else { -1 }
	dx := if x0 < x1 { x1 - x0 } else { x0 - x1 }
	dy := if y0 < y1 { y0 - y1 } else { y1 - y0 }
	mut err := dx + dy
	mut i := 0
	for {
		if i % 2 == 0 {
			ctx.draw_point(x0, y0)
		}
		if x0 == x1 && y0 == y1 {
			break
		}
		e2 := 2 * err
		if e2 >= dy {
			err += dy
			x0 += sx
		}
		if e2 <= dx {
			err += dx
			y0 += sy
		}
		i++
	}
}

// draw_rect draws a filled rectangle.
pub fn (mut ctx Context) draw_rect(x int, y int, x2 int, y2 int) {
	if y == y2 || x == x2 {
		ctx.draw_line(x, y, x2, y2)
		return
	}
	min_y, max_y := if y < y2 { y, y2 } else { y2, y }
	for y_pos in min_y .. max_y + 1 {
		ctx.draw_line(x, y_pos, x2, y_pos)
	}
}

// draw_empty_dashed_rect draws a rectangle with dashed lines (no fill).
pub fn (mut ctx Context) draw_empty_dashed_rect(x int, y int, x2 int, y2 int) {
	if y == y2 || x == x2 {
		ctx.draw_dashed_line(x, y, x2, y2)
		return
	}
	min_x, max_x := if x < x2 { x, x2 } else { x2, x }
	min_y, max_y := if y < y2 { y, y2 } else { y2, y }
	ctx.draw_dashed_line(min_x, min_y, max_x, min_y)
	ctx.draw_dashed_line(min_x, min_y, min_x, max_y)
	if (max_y - min_y) & 1 == 0 {
		ctx.draw_dashed_line(min_x, max_y, max_x, max_y)
	} else {
		ctx.draw_dashed_line(min_x + 1, max_y, max_x, max_y)
	}
	if (max_x - min_x) & 1 == 0 {
		ctx.draw_dashed_line(max_x, min_y, max_x, max_y)
	} else {
		ctx.draw_dashed_line(max_x, min_y + 1, max_x, max_y)
	}
}

// draw_empty_rect draws a rectangle with no fill.
pub fn (mut ctx Context) draw_empty_rect(x int, y int, x2 int, y2 int) {
	if y == y2 || x == x2 {
		ctx.draw_line(x, y, x2, y2)
		return
	}
	ctx.draw_line(x, y, x2, y)
	ctx.draw_line(x, y2, x2, y2)
	ctx.draw_line(x, y, x, y2)
	ctx.draw_line(x2, y, x2, y2)
}

// horizontal_separator draws a horizontal separator spanning the window width.
@[inline]
pub fn (mut ctx Context) horizontal_separator(y int) {
	ctx.set_cursor_position(0, y)
	ctx.write(strings.repeat(`-`, ctx.window_width))
}
