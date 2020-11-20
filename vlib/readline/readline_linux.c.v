// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Linux version
// Will serve as more advanced input method
// Based on the work of https://github.com/AmokHuginnsson/replxx
module readline

import term
import os

#include <termios.h>
#include <sys/ioctl.h>
// Defines actions to execute
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

fn C.tcgetattr() int

fn C.tcsetattr() int

fn C.raise()

fn C.getppid() int

// Enable the raw mode of the terminal
// In raw mode all keypresses are directly sent to the program and no interpretation is done
// Catches the SIGUSER (CTRL+C) Signal
pub fn (mut r Readline) enable_raw_mode() {
	if C.tcgetattr(0, &r.orig_termios) == -1 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := r.orig_termios
	raw.c_iflag &= ~(C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON)
	raw.c_cflag |= (C.CS8)
	raw.c_lflag &= ~(C.ECHO | C.ICANON | C.IEXTEN | C.ISIG)
	raw.c_cc[C.VMIN] = 1
	raw.c_cc[C.VTIME] = 0
	C.tcsetattr(0, C.TCSADRAIN, &raw)
	r.is_raw = true
	r.is_tty = true
}

// Enable the raw mode of the terminal
// Does not catch the SIGUSER (CTRL+C) Signal
pub fn (mut r Readline) enable_raw_mode_nosig() {
	if C.tcgetattr(0, &r.orig_termios) == -1 {
		r.is_tty = false
		r.is_raw = false
		return
	}
	mut raw := r.orig_termios
	raw.c_iflag &= ~(C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON)
	raw.c_cflag |= (C.CS8)
	raw.c_lflag &= ~(C.ECHO | C.ICANON | C.IEXTEN)
	raw.c_cc[C.VMIN] = 1
	raw.c_cc[C.VTIME] = 0
	C.tcsetattr(0, C.TCSADRAIN, &raw)
	r.is_raw = true
	r.is_tty = true
}

// Disable the raw mode of the terminal
pub fn (mut r Readline) disable_raw_mode() {
	if r.is_raw {
		C.tcsetattr(0, C.TCSADRAIN, &r.orig_termios)
		r.is_raw = false
	}
}

// Read a single char
pub fn (r Readline) read_char() int {
	return utf8_getchar()
}

// Main function of the readline module
// Will loop and ingest characters until EOF or Enter
// Returns the completed line as utf8 ustring
// Will return an error if line is empty
pub fn (mut r Readline) read_line_utf8(prompt string) ?ustring {
	r.current = ''.ustring()
	r.cursor = 0
	r.prompt = prompt
	r.search_index = 0
	r.prompt_offset = get_prompt_offset(prompt)
	if r.previous_lines.len <= 1 {
		r.previous_lines << ''.ustring()
		r.previous_lines << ''.ustring()
	} else {
		r.previous_lines[0] = ''.ustring()
	}
	if !r.is_raw {
		r.enable_raw_mode()
	}
	print(r.prompt)
	for {
		C.fflush(C.stdout)
		c := r.read_char()
		a := r.analyse(c)
		if r.execute(a, c) {
			break
		}
	}
	r.previous_lines[0] = ''.ustring()
	r.search_index = 0
	r.disable_raw_mode()
	if r.current.s == '' {
		return error('empty line')
	}
	return r.current
}

// Returns the string from the utf8 ustring
pub fn (mut r Readline) read_line(prompt string) ?string {
	s := r.read_line_utf8(prompt) ?
	return s.s
}

// Standalone function without persistent functionnalities (eg: history)
// Returns utf8 based ustring
pub fn read_line_utf8(prompt string) ?ustring {
	mut r := Readline{}
	s := r.read_line_utf8(prompt) ?
	return s
}

// Standalone function without persistent functionnalities (eg: history)
// Return string from utf8 ustring
pub fn read_line(prompt string) ?string {
	mut r := Readline{}
	s := r.read_line(prompt) ?
	return s
}

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

fn (r Readline) analyse(c int) Action {
	match byte(c) {
		`\0`, 0x3, 0x4, 255 { return .eof } // NUL, End of Text, End of Transmission
		`\n`, `\r` { return .commit_line }
		`\f` { return .clear_screen } // CTRL + L
		`\b`, 127 { return .delete_left } // BS, DEL
		27 { return r.analyse_control() } // ESC
		1 { return .move_cursor_begining } // ^A
		5 { return .move_cursor_end } // ^E
		26 { return .suspend } // CTRL + Z, SUB
		else { return if c >= ` ` {
				Action.insert_character
			} else {
				Action.nothing
			} }
	}
}

fn (r Readline) analyse_control() Action {
	c := r.read_char()
	match byte(c) {
		`[` {
			sequence := r.read_char()
			match byte(sequence) {
				`C` { return .move_cursor_right }
				`D` { return .move_cursor_left }
				`B` { return .history_next }
				`A` { return .history_previous }
				`1` { return r.analyse_extended_control() }
				`2`, `3` { return r.analyse_extended_control_no_eat(byte(sequence)) }
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

fn (r Readline) analyse_extended_control() Action {
	r.read_char() // Removes ;
	c := r.read_char()
	match byte(c) {
		`5` {
			direction := r.read_char()
			match byte(direction) {
				`C` { return .move_cursor_word_right }
				`D` { return .move_cursor_word_left }
				else {}
			}
		}
		else {}
	}
	return .nothing
}

fn (r Readline) analyse_extended_control_no_eat(last_c byte) Action {
	c := r.read_char()
	match byte(c) {
		`~` { match last_c {
				`3` { return .delete_right } // Suppr key
				`2` { return .overwrite }
				else {}
			} }
		else {}
	}
	return .nothing
}

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

fn get_screen_columns() int {
	ws := Winsize{}
	cols := if C.ioctl(1, C.TIOCGWINSZ, &ws) == -1 { 80 } else { int(ws.ws_col) }
	return cols
}

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

fn calculate_screen_position(x_in int, y_in int, screen_columns int, char_count int, inp []int) []int {
	mut out := inp
	mut x := x_in
	mut y := y_in
	out[0] = x
	out[1] = y
	for chars_remaining := char_count; chars_remaining > 0; {
		chars_this_row := if (x + chars_remaining) < screen_columns { chars_remaining } else { screen_columns -
				x }
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

// Will redraw the line
fn (mut r Readline) refresh_line() {
	mut end_of_input := [0, 0]
	end_of_input = calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.current.len,
		end_of_input)
	end_of_input[1] += r.current.count('\n'.ustring())
	mut cursor_pos := [0, 0]
	cursor_pos = calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.cursor,
		cursor_pos)
	shift_cursor(0, -r.cursor_row_offset)
	term.erase_toend()
	print(r.prompt)
	print(r.current)
	if end_of_input[0] == 0 && end_of_input[1] > 0 {
		print('\n')
	}
	shift_cursor(cursor_pos[0] - r.prompt_offset, -(end_of_input[1] - cursor_pos[1]))
	r.cursor_row_offset = cursor_pos[1]
}

// End the line without a newline
fn (mut r Readline) eof() bool {
	r.previous_lines.insert(1, r.current)
	r.cursor = r.current.len
	if r.is_tty {
		r.refresh_line()
	}
	return true
}

fn (mut r Readline) insert_character(c int) {
	if !r.overwrite || r.cursor == r.current.len {
		r.current = r.current.left(r.cursor).ustring().add(utf32_to_str(u32(c)).ustring()).add(r.current.right(r.cursor).ustring())
	} else {
		r.current = r.current.left(r.cursor).ustring().add(utf32_to_str(u32(c)).ustring()).add(r.current.right(r.cursor +
			1).ustring())
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
	r.current = r.current.left(r.cursor).ustring().add(r.current.right(r.cursor + 1).ustring())
	r.refresh_line()
}

// Removes the character in front of cursor.
fn (mut r Readline) suppr_character() {
	if r.cursor > r.current.len {
		return
	}
	r.current = r.current.left(r.cursor).ustring().add(r.current.right(r.cursor + 1).ustring())
	r.refresh_line()
}

// Add a line break then stops the main loop
fn (mut r Readline) commit_line() bool {
	r.previous_lines.insert(1, r.current)
	a := '\n'.ustring()
	r.current = r.current.add(a)
	r.cursor = r.current.len
	if r.is_tty {
		r.refresh_line()
		println('')
	}
	return true
}

fn (mut r Readline) move_cursor_left() {
	if r.cursor > 0 {
		r.cursor--
		r.refresh_line()
	}
}

fn (mut r Readline) move_cursor_right() {
	if r.cursor < r.current.len {
		r.cursor++
		r.refresh_line()
	}
}

fn (mut r Readline) move_cursor_begining() {
	r.cursor = 0
	r.refresh_line()
}

fn (mut r Readline) move_cursor_end() {
	r.cursor = r.current.len
	r.refresh_line()
}

// Check if the character is considered as a word-breaking character
fn (r Readline) is_break_character(c string) bool {
	break_characters := ' \t\v\f\a\b\r\n`~!@#$%^&*()-=+[{]}\\|;:\'",<.>/?'
	return break_characters.contains(c)
}

fn (mut r Readline) move_cursor_word_left() {
	if r.cursor > 0 {
		for ; r.cursor > 0 && r.is_break_character(r.current.at(r.cursor - 1)); r.cursor-- {
		}
		for ; r.cursor > 0 && !r.is_break_character(r.current.at(r.cursor - 1)); r.cursor-- {
		}
		r.refresh_line()
	}
}

fn (mut r Readline) move_cursor_word_right() {
	if r.cursor < r.current.len {
		for ; r.cursor < r.current.len && r.is_break_character(r.current.at(r.cursor)); r.cursor++ {
		}
		for ; r.cursor < r.current.len && !r.is_break_character(r.current.at(r.cursor)); r.cursor++ {
		}
		r.refresh_line()
	}
}

fn (mut r Readline) switch_overwrite() {
	r.overwrite = !r.overwrite
}

fn (mut r Readline) clear_screen() {
	term.set_cursor_position({
		x: 1
		y: 1
	})
	term.erase_clear()
	r.refresh_line()
}

fn (mut r Readline) history_previous() {
	if r.search_index + 2 >= r.previous_lines.len {
		return
	}
	if r.search_index == 0 {
		r.previous_lines[0] = r.current
	}
	r.search_index++
	r.current = r.previous_lines[r.search_index]
	r.cursor = r.current.len
	r.refresh_line()
}

fn (mut r Readline) history_next() {
	if r.search_index <= 0 {
		return
	}
	r.search_index--
	r.current = r.previous_lines[r.search_index]
	r.cursor = r.current.len
	r.refresh_line()
}

fn (mut r Readline) suspend() {
	is_standalone := os.getenv('VCHILD') != 'true'
	r.disable_raw_mode()
	if !is_standalone {
		// We have to SIGSTOP the parent v process
		ppid := C.getppid()
		C.kill(ppid, C.SIGSTOP)
	}
	C.raise(C.SIGSTOP)
	r.enable_raw_mode()
	r.refresh_line()
	if r.is_tty {
		r.refresh_line()
	}
}
