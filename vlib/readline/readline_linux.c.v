// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Serves as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module readline

import term
import os

#include <termios.h>
#include <sys/ioctl.h>

const cclen = 10

// Termios stores the terminal options on Linux.
struct C.termios {
mut:
	c_iflag int
	c_oflag int
	c_cflag int
	c_lflag int
	c_line  u8
	c_cc    [cclen]int
}

struct Termios {
mut:
	c_iflag u32
	c_oflag u32
	c_cflag u32
	c_lflag u32
	c_line  u8
	c_cc    [cclen]int
}

fn C.tcgetattr(fd int, termios_p &C.termios) int

fn C.tcsetattr(fd int, optional_actions int, const_termios_p &C.termios) int

fn C.raise(sig int)

fn C.getppid() int

// Action defines what actions to be executed.
enum Action {
	eof
	nothing
	insert_character
	commit_line
	delete_left
	delete_right
	move_cursor_left
	move_cursor_right
	move_cursor_begining
	move_cursor_end
	move_cursor_word_left
	move_cursor_word_right
	history_previous
	history_next
	overwrite
	clear_screen
	suspend
}

// enable_raw_mode enables the raw mode of the terminal.
// In raw mode all key presses are directly sent to the program and no interpretation is done.
// Please note that `enable_raw_mode` catches the `SIGUSER` (CTRL + C) signal.
// For a method that does please see `enable_raw_mode_nosig`.
pub fn (mut r Readline) enable_raw_mode() {
	if unsafe { C.tcgetattr(0, &C.termios(&r.orig_termios)) } != 0 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := C.termios{}
	unsafe { vmemcpy(&raw, &r.orig_termios, int(sizeof(raw))) }
	// println('> r.orig_termios: $r.orig_termios')
	// println('>            raw: $raw')
	raw.c_iflag &= ~(C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON)
	raw.c_cflag |= C.CS8
	raw.c_lflag &= ~(C.ECHO | C.ICANON | C.IEXTEN | C.ISIG)
	raw.c_cc[C.VMIN] = u8(1)
	raw.c_cc[C.VTIME] = u8(0)
	unsafe { C.tcsetattr(0, C.TCSADRAIN, &raw) }
	// println('>   after    raw: $raw')
	r.is_raw = true
	r.is_tty = true
}

// enable_raw_mode_nosig enables the raw mode of the terminal.
// In raw mode all key presses are directly sent to the program and no interpretation is done.
// Please note that `enable_raw_mode_nosig` does not catch the `SIGUSER` (CTRL + C) signal
// as opposed to `enable_raw_mode`.
pub fn (mut r Readline) enable_raw_mode_nosig() {
	if unsafe { C.tcgetattr(0, &C.termios(&r.orig_termios)) } != 0 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := C.termios{}
	unsafe { vmemcpy(&raw, &r.orig_termios, int(sizeof(raw))) }
	raw.c_iflag &= ~(C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON)
	raw.c_cflag |= C.CS8
	raw.c_lflag &= ~(C.ECHO | C.ICANON | C.IEXTEN)
	raw.c_cc[C.VMIN] = u8(1)
	raw.c_cc[C.VTIME] = u8(0)
	unsafe { C.tcsetattr(0, C.TCSADRAIN, &raw) }
	r.is_raw = true
	r.is_tty = true
}

// disable_raw_mode disables the raw mode of the terminal.
// For a description of raw mode please see the `enable_raw_mode` method.
pub fn (mut r Readline) disable_raw_mode() {
	if r.is_raw {
		unsafe { C.tcsetattr(0, C.TCSADRAIN, &C.termios(&r.orig_termios)) }
		r.is_raw = false
	}
}

// read_char reads a single character.
pub fn (r Readline) read_char() int {
	return utf8_getchar()
}

// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete input line as an UTF-8 encoded `[]rune` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
pub fn (mut r Readline) read_line_utf8(prompt string) ?[]rune {
	r.current = []rune{}
	r.cursor = 0
	r.prompt = prompt
	r.search_index = 0
	r.prompt_offset = get_prompt_offset(prompt)
	if r.previous_lines.len <= 1 {
		r.previous_lines << []rune{}
		r.previous_lines << []rune{}
	} else {
		r.previous_lines[0] = []rune{}
	}
	if !r.is_raw {
		r.enable_raw_mode()
	}
	print(r.prompt)
	for {
		unsafe { C.fflush(C.stdout) }
		c := r.read_char()
		a := r.analyse(c)
		if r.execute(a, c) {
			break
		}
	}
	r.previous_lines[0] = []rune{}
	r.search_index = 0
	r.disable_raw_mode()
	if r.current.len == 0 {
		return error('empty line')
	}
	return r.current
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `[]rune` returned by `read_line_utf8`).
pub fn (mut r Readline) read_line(prompt string) ?string {
	s := r.read_line_utf8(prompt) ?
	return s.string()
}

// read_line_utf8 blocks execution in a loop and awaits user input
// characters from a terminal until `EOF` or `Enter` key is encountered
// in the input stream.
// read_line_utf8 returns the complete input line as an UTF-8 encoded `[]rune` or
// an error if the line is empty.
// The `prompt` `string` is output as a prefix text for the input capturing.
// read_line_utf8 is the main method of the `readline` module and `Readline` struct.
// NOTE that this version of `read_line_utf8` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line_utf8(prompt string) ?[]rune {
	mut r := Readline{}
	s := r.read_line_utf8(prompt) ?
	return s
}

// read_line does the same as `read_line_utf8` but returns user input as a `string`.
// (As opposed to `[]rune` as returned by `read_line_utf8`).
// NOTE that this version of `read_line` is a standalone function without
// persistent functionalities (e.g. history).
pub fn read_line(prompt string) ?string {
	mut r := Readline{}
	s := r.read_line(prompt) ?
	return s
}

// analyse returns an `Action` based on the type of input byte given in `c`.
fn (r Readline) analyse(c int) Action {
	if c > 255 {
		return Action.insert_character
	}
	match u8(c) {
		`\0`, 0x3, 0x4, 255 {
			return .eof
		} // NUL, End of Text, End of Transmission
		`\n`, `\r` {
			return .commit_line
		}
		`\f` {
			return .clear_screen
		} // CTRL + L
		`\b`, 127 {
			return .delete_left
		} // BS, DEL
		27 {
			return r.analyse_control()
		} // ESC
		1 {
			return .move_cursor_begining
		} // ^A
		5 {
			return .move_cursor_end
		} // ^E
		26 {
			return .suspend
		} // CTRL + Z, SUB
		else {
			if c >= ` ` {
				return Action.insert_character
			}
			return Action.nothing
		}
	}
}

// analyse_control returns an `Action` based on the type of input read by `read_char`.
fn (r Readline) analyse_control() Action {
	c := r.read_char()
	match u8(c) {
		`[` {
			sequence := r.read_char()
			match u8(sequence) {
				`C` { return .move_cursor_right }
				`D` { return .move_cursor_left }
				`B` { return .history_next }
				`A` { return .history_previous }
				`1` { return r.analyse_extended_control() }
				`2`, `3` { return r.analyse_extended_control_no_eat(u8(sequence)) }
				else {}
			}
		}
		else {}
	}
	/*
	//TODO
match c {
	case `[`:
	sequence := r.read_char()
	match sequence {
	case `C`: return .move_cursor_right
	case `D`: return .move_cursor_left
	case `B`: return .history_next
	case `A`: return .history_previous
	case `1`: return r.analyse_extended_control()
	case `2`: return r.analyse_extended_control_no_eat(sequence)
	case `3`: return r.analyse_extended_control_no_eat(sequence)
	case `9`:
		foo()
		bar()
	else:
	}
	else:
}
	*/
	return .nothing
}

// analyse_extended_control returns an `Action` based on the type of input read by `read_char`.
// analyse_extended_control specialises in cursor control.
fn (r Readline) analyse_extended_control() Action {
	r.read_char() // Removes ;
	c := r.read_char()
	match u8(c) {
		`5` {
			direction := r.read_char()
			match u8(direction) {
				`C` { return .move_cursor_word_right }
				`D` { return .move_cursor_word_left }
				else {}
			}
		}
		else {}
	}
	return .nothing
}

// analyse_extended_control_no_eat returns an `Action` based on the type of input byte given in `c`.
// analyse_extended_control_no_eat specialises in detection of delete and insert keys.
fn (r Readline) analyse_extended_control_no_eat(last_c u8) Action {
	c := r.read_char()
	match u8(c) {
		`~` {
			match last_c {
				`3` { return .delete_right } // Suppr key
				`2` { return .overwrite }
				else {}
			}
		}
		else {}
	}
	return .nothing
}

// execute executes the corresponding methods on `Readline` based on `a Action` and `c int` arguments.
fn (mut r Readline) execute(a Action, c int) bool {
	match a {
		.eof { return r.eof() }
		.insert_character { r.insert_character(c) }
		.commit_line { return r.commit_line() }
		.delete_left { r.delete_character() }
		.delete_right { r.suppr_character() }
		.move_cursor_left { r.move_cursor_left() }
		.move_cursor_right { r.move_cursor_right() }
		.move_cursor_begining { r.move_cursor_begining() }
		.move_cursor_end { r.move_cursor_end() }
		.move_cursor_word_left { r.move_cursor_word_left() }
		.move_cursor_word_right { r.move_cursor_word_right() }
		.history_previous { r.history_previous() }
		.history_next { r.history_next() }
		.overwrite { r.switch_overwrite() }
		.clear_screen { r.clear_screen() }
		.suspend { r.suspend() }
		else {}
	}
	return false
}

// get_screen_columns returns the number of columns (`width`) in the terminal.
fn get_screen_columns() int {
	ws := Winsize{}
	cols := if unsafe { C.ioctl(1, C.TIOCGWINSZ, &ws) } == -1 { 80 } else { int(ws.ws_col) }
	return cols
}

// shift_cursor warps the cursor to `xpos` with `yoffset`.
fn shift_cursor(xpos int, yoffset int) {
	if yoffset != 0 {
		if yoffset > 0 {
			term.cursor_down(yoffset)
		} else {
			term.cursor_up(-yoffset)
		}
	}
	// Absolute X position
	print('\x1b[${xpos + 1}G')
}

// calculate_screen_position returns a position `[x, y]int` based on various terminal attributes.
fn calculate_screen_position(x_in int, y_in int, screen_columns int, char_count int, inp []int) []int {
	mut out := inp.clone()
	mut x := x_in
	mut y := y_in
	out[0] = x
	out[1] = y
	for chars_remaining := char_count; chars_remaining > 0; {
		chars_this_row := if (x + chars_remaining) < screen_columns {
			chars_remaining
		} else {
			screen_columns - x
		}
		out[0] = x + chars_this_row
		out[1] = y
		chars_remaining -= chars_this_row
		x = 0
		y++
	}
	if out[0] == screen_columns {
		out[0] = 0
		out[1]++
	}
	return out
}

// get_prompt_offset computes the length of the `prompt` `string` argument.
fn get_prompt_offset(prompt string) int {
	mut len := 0
	for i := 0; i < prompt.len; i++ {
		if prompt[i] == `\e` {
			for ; i < prompt.len && prompt[i] != `m`; i++ {
			}
		} else {
			len = len + 1
		}
	}
	return prompt.len - len
}

// refresh_line redraws the current line, including the prompt.
fn (mut r Readline) refresh_line() {
	mut end_of_input := [0, 0]
	end_of_input = calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.current.len,
		end_of_input)
	end_of_input[1] += r.current.filter(it == `\n`).len
	mut cursor_pos := [0, 0]
	cursor_pos = calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.cursor,
		cursor_pos)
	shift_cursor(0, -r.cursor_row_offset)
	term.erase_toend()
	print(r.prompt)
	print(r.current.string())
	if end_of_input[0] == 0 && end_of_input[1] > 0 {
		print('\n')
	}
	shift_cursor(cursor_pos[0] - r.prompt_offset, -(end_of_input[1] - cursor_pos[1]))
	r.cursor_row_offset = cursor_pos[1]
}

// eof ends the line *without* a newline.
fn (mut r Readline) eof() bool {
	r.previous_lines.insert(1, r.current)
	r.cursor = r.current.len
	if r.is_tty {
		r.refresh_line()
	}
	return true
}

// insert_character inserts the character `c` at current cursor position.
fn (mut r Readline) insert_character(c int) {
	if !r.overwrite || r.cursor == r.current.len {
		r.current.insert(r.cursor, c)
	} else {
		r.current[r.cursor] = rune(c)
	}
	r.cursor++
	// Refresh the line to add the new character
	if r.is_tty {
		r.refresh_line()
	}
}

// Removes the character behind cursor.
fn (mut r Readline) delete_character() {
	if r.cursor <= 0 {
		return
	}
	r.cursor--
	r.current.delete(r.cursor)
	r.refresh_line()
}

// suppr_character removes (suppresses) the character in front of the cursor.
fn (mut r Readline) suppr_character() {
	if r.cursor >= r.current.len {
		return
	}
	r.current.delete(r.cursor)
	r.refresh_line()
}

// commit_line adds a line break and then stops the main loop.
fn (mut r Readline) commit_line() bool {
	r.previous_lines.insert(1, r.current)
	r.current << `\n`
	r.cursor = r.current.len
	if r.is_tty {
		r.refresh_line()
		println('')
	}
	return true
}

// move_cursor_left moves the cursor relative one cell to the left.
fn (mut r Readline) move_cursor_left() {
	if r.cursor > 0 {
		r.cursor--
		r.refresh_line()
	}
}

// move_cursor_right moves the cursor relative one cell to the right.
fn (mut r Readline) move_cursor_right() {
	if r.cursor < r.current.len {
		r.cursor++
		r.refresh_line()
	}
}

// move_cursor_begining moves the cursor to the beginning of the current line.
fn (mut r Readline) move_cursor_begining() {
	r.cursor = 0
	r.refresh_line()
}

// move_cursor_end moves the cursor to the end of the current line.
fn (mut r Readline) move_cursor_end() {
	r.cursor = r.current.len
	r.refresh_line()
}

// is_break_character returns true if the character is considered as a word-breaking character.
fn (r Readline) is_break_character(c string) bool {
	break_characters := ' \t\v\f\a\b\r\n`~!@#$%^&*()-=+[{]}\\|;:\'",<.>/?'
	return break_characters.contains(c)
}

// move_cursor_word_left moves the cursor relative one word length worth to the left.
fn (mut r Readline) move_cursor_word_left() {
	if r.cursor > 0 {
		for ; r.cursor > 0 && r.is_break_character(r.current[r.cursor - 1].str()); r.cursor-- {
		}
		for ; r.cursor > 0 && !r.is_break_character(r.current[r.cursor - 1].str()); r.cursor-- {
		}
		r.refresh_line()
	}
}

// move_cursor_word_right moves the cursor relative one word length worth to the right.
fn (mut r Readline) move_cursor_word_right() {
	if r.cursor < r.current.len {
		for ; r.cursor < r.current.len && r.is_break_character(r.current[r.cursor].str()); r.cursor++ {
		}
		for ; r.cursor < r.current.len && !r.is_break_character(r.current[r.cursor].str()); r.cursor++ {
		}
		r.refresh_line()
	}
}

// switch_overwrite toggles Readline `overwrite` mode on/off.
fn (mut r Readline) switch_overwrite() {
	r.overwrite = !r.overwrite
}

// clear_screen clears the current terminal window contents and positions the cursor at top left.
fn (mut r Readline) clear_screen() {
	term.set_cursor_position(x: 1, y: 1)
	term.erase_clear()
	r.refresh_line()
}

// history_previous sets current line to the content of the previous line in the history buffer.
fn (mut r Readline) history_previous() {
	if r.search_index + 2 >= r.previous_lines.len {
		return
	}
	if r.search_index == 0 {
		r.previous_lines[0] = r.current
	}
	r.search_index++
	prev_line := r.previous_lines[r.search_index]
	if r.skip_empty && prev_line == [] {
		r.history_previous()
	} else {
		r.current = prev_line
		r.cursor = r.current.len
		r.refresh_line()
	}
}

// history_next sets current line to the content of the next line in the history buffer.
fn (mut r Readline) history_next() {
	if r.search_index <= 0 {
		return
	}
	r.search_index--
	r.current = r.previous_lines[r.search_index]
	r.cursor = r.current.len
	r.refresh_line()
}

// suspend sends the `SIGSTOP` signal to the terminal.
fn (mut r Readline) suspend() {
	is_standalone := os.getenv('VCHILD') != 'true'
	r.disable_raw_mode()
	if !is_standalone {
		// We have to SIGSTOP the parent v process
		unsafe {
			ppid := C.getppid()
			C.kill(ppid, C.SIGSTOP)
		}
	}
	unsafe { C.raise(C.SIGSTOP) }
	r.enable_raw_mode()
	r.refresh_line()
	if r.is_tty {
		r.refresh_line()
	}
}
