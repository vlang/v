// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by the MIT license distributed with this software.
// Don't use this editor for any serious work.
// A lot of funtionality is missing compared to your favourite editor :)
import strings
import os
import term.ui

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
	ti     &ui.Context = 0
	ed     &Buffer = 0
	file   string
	status string
	t      int
	footer_height int = 2
}

fn (mut a App) set_status(msg string, duration_ms int) {
	a.status = msg
	a.t = duration_ms
}

fn (mut a App) save() {
	if a.file.len > 0 {
		b := a.ed
		os.write_file(a.file, b.raw())
		a.set_status('saved', 2000)
	} else {
		a.set_status('No file loaded', 4000)
	}
}

fn (mut a App) footer() {
	w, h := a.ti.window_width, a.ti.window_height
	mut b := a.ed
	flat := b.flat()
	snip := if flat.len > 19 { flat[..20] } else { flat }
	mut finfo := ''
	if a.file.len > 0 {
		finfo = ' (' + os.file_name(a.file) + ')'
	}
	mut status := a.status
	if a.t <= 0 {
		status = ''
	}
	line := '─'.repeat(w) + '\n'
	footer := line + '$finfo Line ${b.cursor.pos_y + 1}/$b.lines.len, Column ${b.cursor.pos_x +
		1}/$b.cur_line().len index: $b.cursor_index() (ESC = quit, Ctrl+s = save) Raw: "$snip" $status'
	a.ti.draw_text(0, h - 1, footer)
	a.t -= 33
	if a.t < 0 {
		a.t = 0
	}
}

struct Buffer {
	tab_width  int = 4
pub mut:
	lines      []string
	cursor     Cursor
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
	mut app := &App(x)
	app.ed = &Buffer{}
	if app.file.len > 0 {
		if os.is_file(app.file) {
			mut b := app.ed
			content := os.read_file(app.file) or {
				panic(err)
			}
			b.put(content)
			app.ed.cursor.pos_x = 0
			app.ed.cursor.pos_y = 0
		}
	}
}

fn frame(x voidptr) {
	mut app := &App(x)
	mut ed := app.ed
	app.ti.clear()
	scroll_limit := app.ti.window_height-app.footer_height-1
	mut view := View{}
	if ed.cursor.pos_y > scroll_limit { // Scroll
		view = ed.view(ed.cursor.pos_y-scroll_limit, ed.cursor.pos_y)
	} else {
		view = ed.view(0, scroll_limit)
	}
	app.ti.draw_text(0, 0, view.raw)
	app.footer()
	if ed.cursor.pos_y > scroll_limit {
		app.ti.set_cursor_position(view.cursor.pos_x + 1, scroll_limit+1)
	} else {
		app.ti.set_cursor_position(view.cursor.pos_x + 1, view.cursor.pos_y + 1)
	}
	app.ti.flush()
}

fn event(e &ui.Event, x voidptr) {
	mut app := &App(x)
	mut buffer := app.ed
	if e.typ == .key_down {
		match e.code {
			.escape {
				app.ti.set_cursor_position(0, 0)
				app.ti.flush()
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
				if e.modifiers == ui.ctrl {
					if e.code == .s {
						app.save()
					}
				} else if e.modifiers in [ui.shift, 0] {
					buffer.put(e.ascii.str())
				}
			}
			else {
				buffer.put(e.utf8.bytes().bytestr())
			}
		}
	} else if e.typ == .mouse_scroll {
		direction := if e.direction == .up { Movement.down } else { Movement.up }
		buffer.move_cursor(1, direction)
	}
}

// main
mut file := ''
if os.args.len > 1 {
	file = os.args[1]
}
mut app := &App{
	file: file
}
app.ti = ui.init({
	user_data: app
	init_fn: init
	frame_fn: frame
	event_fn: event
	capture_events: true
	frame_rate: 30
})
app.ti.run()
