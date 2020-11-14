// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by the MIT license distributed with this software.
// Don't use this editor for any serious work.
// A lot of funtionality is missing compared to your favourite editor :)
import strings
import os
import term.ui as tui

enum Movement {
	up
	down
	left
	right
	home
	end
}

struct View {
pub:
	raw    string
	cursor Cursor
}

struct App {
mut:
	tui           &tui.Context = 0
	ed            &Buffer = 0
	current_file  int
	files         []string
	status        string
	t             int
	footer_height int = 2
	viewport      int
}

fn (mut a App) set_status(msg string, duration_ms int) {
	a.status = msg
	a.t = duration_ms
}

fn (mut a App) save() {
	if a.cfile().len > 0 {
		b := a.ed
		os.write_file(a.cfile(), b.raw())
		a.set_status('Saved', 2000)
	} else {
		a.set_status('No file loaded', 4000)
	}
}

fn (mut a App) cfile() string {
	if a.files.len == 0 {
		return ''
	}
	if a.current_file >= a.files.len {
		return ''
	}
	return a.files[a.current_file]
}

fn (mut a App) visit_prev_file() {
	if a.files.len == 0 {
		a.current_file = 0
	} else {
		a.current_file = (a.current_file + a.files.len - 1) % a.files.len
	}
	a.init_file()
}

fn (mut a App) visit_next_file() {
	if a.files.len == 0 {
		a.current_file = 0
	} else {
		a.current_file = (a.current_file + a.files.len + 1) % a.files.len
	}
	a.init_file()
}


fn (mut a App) footer() {
	w, h := a.tui.window_width, a.tui.window_height
	mut b := a.ed
	// flat := b.flat()
	// snip := if flat.len > 19 { flat[..20] } else { flat }
	finfo := if a.cfile().len > 0 { ' (' + os.file_name(a.cfile()) + ')' } else { '' }
	mut status := a.status
	a.tui.draw_text(0, h - 1, '─'.repeat(w))
	footer := '$finfo Line ${b.cursor.pos_y + 1:4}/${b.lines.len:-4}, Column ${b.cursor.pos_x + 1:3}/${b.cur_line().len:-3} index: ${b.cursor_index():5} (ESC = quit, Ctrl+s = save)'
	if footer.len < w {
		a.tui.draw_text((w - footer.len) / 2, h, footer)
	} else if footer.len == w {
		a.tui.draw_text(0, h, footer)
	} else {
		a.tui.draw_text(0, h, footer[..w])
	}
	if a.t <= 0 {
		status = ''
	} else {
		a.tui.set_bg_color({
			r: 200
			g: 200
			b: 200
		})
		a.tui.set_color({
			r: 0
			g: 0
			b: 0
		})
		a.tui.draw_text((w + 4 - status.len) / 2, h - 1, ' $status ')
		a.tui.reset()
		a.t -= 33
	}
}

struct Buffer {
	tab_width int = 4
pub mut:
	lines     []string
	cursor    Cursor
}

fn (b Buffer) flat() string {
	return b.raw().replace_each(['\n', r'\n', '\t', r'\t'])
}

fn (b Buffer) raw() string {
	return b.lines.join('\n')
}

fn (b Buffer) view(from int, to int) View {
	l := b.cur_line()
	mut x := 0
	for i := 0; i < b.cursor.pos_x && i < l.len; i++ {
		if l[i] == `\t` {
			x += b.tab_width
			continue
		}
		x++
	}
	mut lines := []string{}
	for i, line in b.lines {
		if i >= from && i <= to {
			lines << line
		}
	}
	raw := lines.join('\n')
	return {
		raw: raw.replace('\t', strings.repeat(` `, b.tab_width))
		cursor: {
			pos_x: x
			pos_y: b.cursor.pos_y
		}
	}
}

fn (b Buffer) cur_line() string {
	_, y := b.cursor.xy()
	if b.lines.len == 0 {
		return ''
	}
	return b.lines[y]
}

fn (b Buffer) cursor_index() int {
	mut i := 0
	for y, line in b.lines {
		if b.cursor.pos_y == y {
			i += b.cursor.pos_x
			break
		}
		i += line.len + 1
	}
	return i
}

fn (mut b Buffer) put(s string) {
	has_line_ending := s.contains('\n')
	x, y := b.cursor.xy()
	if b.lines.len == 0 {
		b.lines.prepend('')
	}
	line := b.lines[y]
	l, r := line[..x], line[x..]
	if has_line_ending {
		mut lines := s.split('\n')
		lines[0] = l + lines[0]
		lines[lines.len - 1] += r
		b.lines.delete(y)
		b.lines.insert(y, lines)
		last := lines[lines.len - 1]
		b.cursor.set(last.len, y + lines.len - 1)
		if s == '\n' {
			b.cursor.set(0, b.cursor.pos_y)
		}
	} else {
		b.lines[y] = l + s + r
		b.cursor.set(x + s.len, y)
	}
	$if debug {
		flat := s.replace('\n', r'\n')
		eprintln(@MOD + '.' + @STRUCT + '::' + @FN + ' "$flat"')
	}
}

fn (mut b Buffer) del(amount int) string {
	if amount == 0 {
		return ''
	}
	x, y := b.cursor.xy()
	if amount < 0 { // don't delete left if we're at 0,0
		if x == 0 && y == 0 {
			return ''
		}
	} else if x >= b.cur_line().len && y >= b.lines.len - 1 {
		return ''
	}
	mut removed := ''
	if amount < 0 { // backspace (backward)
		i := b.cursor_index()
		removed = b.raw()[i + amount..i]
		mut left := amount * -1
		for li := y; li >= 0 && left > 0; li-- {
			ln := b.lines[li]
			if left > ln.len {
				b.lines.delete(li)
				if ln.len == 0 { // line break delimiter
					left--
					if y == 0 {
						return ''
					}
					line_above := b.lines[li - 1]
					b.cursor.pos_x = line_above.len
				} else {
					left -= ln.len
				}
				b.cursor.pos_y--
			} else {
				if x == 0 {
					if y == 0 {
						return ''
					}
					line_above := b.lines[li - 1]
					if ln.len == 0 { // at line break
						b.lines.delete(li)
						b.cursor.pos_y--
						b.cursor.pos_x = line_above.len
					} else {
						b.lines[li - 1] = line_above + ln
						b.lines.delete(li)
						b.cursor.pos_y--
						b.cursor.pos_x = line_above.len
					}
				} else if x == 1 {
					b.lines[li] = b.lines[li][left..]
					b.cursor.pos_x = 0
				} else {
					b.lines[li] = ln[..x - left] + ln[x..]
					b.cursor.pos_x -= left
				}
				left = 0
				break
			}
		}
	} else { // delete (forward)
		i := b.cursor_index() + 1
		removed = b.raw()[i - amount..i]
		mut left := amount
		for li := y; li >= 0 && left > 0; li++ {
			ln := b.lines[li]
			if x == ln.len { // at line end
				if y + 1 <= b.lines.len {
					b.lines[li] = ln + b.lines[y + 1]
					b.lines.delete(y + 1)
					left--
					b.del(left)
				}
			} else if left > ln.len {
				b.lines.delete(li)
				left -= ln.len
			} else {
				b.lines[li] = ln[..x] + ln[x + left..]
				left = 0
			}
		}
	}
	$if debug {
		flat := removed.replace('\n', r'\n')
		eprintln(@MOD + '.' + @STRUCT + '::' + @FN + ' "$flat"')
	}
	return removed
}

fn (mut b Buffer) free() {
	$if debug {
		eprintln(@MOD + '.' + @STRUCT + '::' + @FN)
	}
	for line in b.lines {
		line.free()
	}
	unsafe {b.lines.free()}
}

// move_cursor will navigate the cursor within the buffer bounds
fn (mut b Buffer) move_cursor(amount int, movement Movement) {
	cur_line := b.cur_line()
	match movement {
		.up {
			if b.cursor.pos_y - amount >= 0 {
				b.cursor.move(0, -amount)
				// Check the move
				line := b.cur_line()
				if b.cursor.pos_x > line.len {
					b.cursor.set(line.len, b.cursor.pos_y)
				}
			}
		}
		.down {
			if b.cursor.pos_y + amount < b.lines.len {
				b.cursor.move(0, amount)
				// Check the move
				line := b.cur_line()
				if b.cursor.pos_x > line.len {
					b.cursor.set(line.len, b.cursor.pos_y)
				}
			}
		}
		.left {
			if b.cursor.pos_x - amount >= 0 {
				b.cursor.move(-amount, 0)
			}
		}
		.right {
			if b.cursor.pos_x + amount <= cur_line.len {
				b.cursor.move(amount, 0)
			}
		}
		.home {
			b.cursor.set(0, b.cursor.pos_y)
		}
		.end {
			b.cursor.set(cur_line.len, b.cursor.pos_y)
		}
	}
}

struct Cursor {
pub mut:
	pos_x int
	pos_y int
}

fn (mut c Cursor) set(x int, y int) {
	c.pos_x = x
	c.pos_y = y
}

fn (mut c Cursor) move(x int, y int) {
	c.pos_x += x
	c.pos_y += y
}

fn (c Cursor) xy() (int, int) {
	return c.pos_x, c.pos_y
}

// App callbacks
fn init(x voidptr) {
	mut a := &App(x)
	a.init_file()
}

fn (mut a App) init_file() {
	a.ed = &Buffer{}
	mut init_y := 0
	mut init_x := 0
	eprintln('> a.files: $a.files | a.current_file: $a.current_file')
	if a.files.len > 0 && a.current_file < a.files.len && a.files[a.current_file].len > 0 {
		if !os.is_file(a.files[a.current_file]) && a.files[a.current_file].contains(':') {
			// support the file:line:col: format
			fparts := a.files[a.current_file].split(':')
			if fparts.len > 0 {
				a.files[a.current_file] = fparts[0]
			}
			if fparts.len > 1 {
				init_y = fparts[1].int() - 1
			}
			if fparts.len > 2 {
				init_x = fparts[2].int() - 1
			}
		}
		if os.is_file(a.files[a.current_file]) {
			// 'vico: ' +
			a.tui.set_window_title(a.files[a.current_file])
			mut b := a.ed
			content := os.read_file(a.files[a.current_file]) or {
				panic(err)
			}
			b.put(content)
			a.ed.cursor.pos_x = init_x
			a.ed.cursor.pos_y = init_y
		}
	}
}

fn frame(x voidptr) {
	mut a := &App(x)
	mut ed := a.ed
	a.tui.clear()
	scroll_limit := a.tui.window_height - a.footer_height - 1
	// scroll down
	if ed.cursor.pos_y > a.viewport + scroll_limit { // scroll down
		a.viewport = ed.cursor.pos_y - scroll_limit
	} else if ed.cursor.pos_y < a.viewport { // scroll up
		a.viewport = ed.cursor.pos_y
	}
	view := ed.view(a.viewport, scroll_limit + a.viewport)
	a.tui.draw_text(0, 0, view.raw)
	a.footer()
	a.tui.set_cursor_position(view.cursor.pos_x + 1, ed.cursor.pos_y + 1 - a.viewport)
	a.tui.flush()
}

fn event(e &tui.Event, x voidptr) {
	mut a := &App(x)
	mut buffer := a.ed
	if e.typ == .key_down {
		match e.code {
			.escape {
				a.tui.set_cursor_position(0, 0)
				a.tui.flush()
				exit(0)
			}
			.backspace {
				buffer.del(-1)
			}
			.delete {
				buffer.del(1)
			}
			.left {
				buffer.move_cursor(1, .left)
			}
			.right {
				buffer.move_cursor(1, .right)
			}
			.up {
				buffer.move_cursor(1, .up)
			}
			.down {
				buffer.move_cursor(1, .down)
			}
			.home {
				buffer.move_cursor(1, .home)
			}
			.end {
				buffer.move_cursor(1, .end)
			}
			48...57, 97...122 { // 0-9a-zA-Z
				if e.modifiers == tui.ctrl {
					if e.code == .s {
						a.save()
					}
				} else if e.modifiers in [tui.shift, 0] {
					buffer.put(e.ascii.str())
				}
			}
			else {
				if e.modifiers == tui.alt {
					if e.code == .comma {
						a.visit_prev_file()
						return
					}
					if e.code == .period {
						a.visit_next_file()
						return
					}
				}
				buffer.put(e.utf8.bytes().bytestr())
			}
		}
	} else if e.typ == .mouse_scroll {
		direction := if e.direction == .up { Movement.down } else { Movement.up }
		buffer.move_cursor(1, direction)
	}
}

fn main() {
	mut files := []string{}
	if os.args.len > 1 {
		files << os.args[1..]
	}
	mut a := &App{
		files: files
	}
	a.tui = tui.init({
		user_data: a
		init_fn: init
		frame_fn: frame
		event_fn: event
		capture_events: true
	})
	a.tui.run()
}
