// Copyright (c) 2020 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by the MIT license distributed with this software.
// Don't use this editor for any serious work.
// A lot of funtionality is missing compared to your favourite editor :)
import strings
import os
import term
import term.ui

type InputType = byte | rune | string

fn (ipt InputType) len() int {
	match ipt {
		byte, rune {
			return 1
		}
		string {
			return it.len
		}
	}
}

fn (ipt InputType) str() string {
	match ipt {
		byte, rune {
			return ipt.str()
		}
		string {
			return ipt.str()
		}
	}
}

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
	w, h := term.get_terminal_size()
	term.set_cursor_position({
		x: 0
		y: h - 1
	})
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
	footer := line + '$finfo Line ${b.cursor.pos.y + 1}/$b.lines.len, Column ${b.cursor.pos.x +
		1}/$b.cur_line().len index: $b.cursor_index() (ESC = quit, Ctrl+s = save) Raw: "$snip" $status'
	print(footer)
	flush()
	a.t -= 33
	if a.t < 0 {
		a.t = 0
	}
}

struct Buffer {
	line_break string = '\n'
	tab_width  int = 4
pub mut:
	lines      []string
	cursor     Cursor
}

fn (b Buffer) flat() string {
	return b.raw().replace(b.line_break, r'\n').replace('\t', r'\t')
}

fn (b Buffer) raw() string {
	return b.lines.join(b.line_break)
}

fn (b Buffer) view() View {
	l := b.cur_line()
	mut x := 0
	for i := 0; i < b.cursor.pos.x && i < l.len; i++ {
		if l[i] == `\t` {
			x += b.tab_width
			continue
		}
		x++
	}
	p := CursorPosition{
		y: b.cursor.pos.y
		x: x
	}
	return View{
		raw: b.raw().replace('\t', strings.repeat(` `, b.tab_width))
		cursor: Cursor{
			pos: p
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
		if b.cursor.pos.y == y {
			i += b.cursor.pos.x
			break
		}
		i += line.len + 1
	}
	return i
}

fn (mut b Buffer) put(ipt InputType) {
	s := ipt.str()
	has_line_ending := s.contains(b.line_break)
	x, y := b.cursor.xy()
	if b.lines.len == 0 {
		b.lines.prepend('')
	}
	line := b.lines[y]
	l := line[..x]
	r := line[x..]
	if has_line_ending {
		mut lines := s.split(b.line_break)
		lines[0] = l + lines[0]
		lines[lines.len - 1] += r
		b.lines.delete(y)
		b.lines.insert(y, lines)
		last := lines[lines.len - 1]
		b.cursor.set(last.len, y + lines.len - 1)
		if s == b.line_break {
			b.cursor.set(0, b.cursor.pos.y)
		}
	} else {
		b.lines[y] = l + s + r
		b.cursor.set(x + s.len, y)
	}
	$if debug {
		flat := s.replace(b.line_break, r'\n')
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
	} else {
		if x >= b.cur_line().len && y >= b.lines.len - 1 {
			return ''
		}
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
					b.cursor.pos.x = line_above.len
				} else {
					left -= ln.len
				}
				b.cursor.pos.y--
			} else {
				if x == 0 {
					if y == 0 {
						return ''
					}
					line_above := b.lines[li - 1]
					if ln.len == 0 { // at line break
						b.lines.delete(li)
						b.cursor.pos.y--
						b.cursor.pos.x = line_above.len
					} else {
						b.lines[li - 1] = line_above + ln
						b.lines.delete(li)
						b.cursor.pos.y--
						b.cursor.pos.x = line_above.len
					}
				} else if x == 1 {
					b.lines[li] = b.lines[li][left..]
					b.cursor.pos.x = 0
				} else {
					b.lines[li] = ln[..x - left] + ln[x..]
					b.cursor.pos.x -= left
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
		flat := removed.replace(b.line_break, r'\n')
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
	pos := b.cursor.pos
	cur_line := b.cur_line()
	match movement {
		.up {
			if pos.y - amount >= 0 {
				b.cursor.move(0, -amount)
				// Check the move
				line := b.cur_line()
				if b.cursor.pos.x > line.len {
					b.cursor.set(line.len, b.cursor.pos.y)
				}
			}
		}
		.down {
			if pos.y + amount < b.lines.len {
				b.cursor.move(0, amount)
				// Check the move
				line := b.cur_line()
				if b.cursor.pos.x > line.len {
					b.cursor.set(line.len, b.cursor.pos.y)
				}
			}
		}
		.left {
			if pos.x - amount >= 0 {
				b.cursor.move(-amount, 0)
			}
		}
		.right {
			if pos.x + amount <= cur_line.len {
				b.cursor.move(amount, 0)
			}
		}
		.home {
			b.cursor.set(0, b.cursor.pos.y)
		}
		.end {
			b.cursor.set(cur_line.len, b.cursor.pos.y)
		}
	}
}

// Cursor related
struct CursorPosition {
pub mut:
	x int
	y int
}

struct Cursor {
pub mut:
	pos CursorPosition
}

fn (mut c Cursor) set(x int, y int) {
	c.pos.x = x
	c.pos.y = y
}

fn (mut c Cursor) move(x int, y int) {
	c.pos.x += x
	c.pos.y += y
}

fn (c Cursor) xy() (int, int) {
	return c.pos.x, c.pos.y
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
		}
	}
}

fn frame(x voidptr) {
	mut app := &App(x)
	mut ed := app.ed
	term.erase_clear()
	term.erase_del_clear()
	term.set_cursor_position({
		x: 0
		y: 0
	})
	view := ed.view()
	print('$view.raw')
	flush()
	app.footer()
	term.set_cursor_position({
		x: view.cursor.pos.x + 1
		y: view.cursor.pos.y + 1
	})
}

fn cleanup(x voidptr) {
	mut app := &App(x)
	app.ed.free()
}

fn fail(error string) {
	eprintln(error)
}

fn event(e &ui.Event, x voidptr) {
	mut app := &App(x)
	mut buffer := app.ed
	if e.typ == .key_down {
		match e.code {
			.escape {
				term.set_cursor_position({
					x: 0
					y: 0
				})
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
			48...57 { // 0 - 9
				buffer.put(e.ascii)
			}
			97...122 { // a-zA-Z
				if e.modifiers == ui.ctrl && e.code == .s {
					app.save()
				} else {
					buffer.put(e.ascii)
				}
			}
			else {
				buffer.put(e.utf8.bytes().bytestr())
			}
		}
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
	cleanup_fn: cleanup
	event_fn: event
	fail_fn: fail
	capture_events: true
	frame_rate: 30
})
app.ti.run()
