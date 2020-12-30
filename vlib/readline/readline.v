// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Serves as a more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module readline

// Termios stores the terminal options on Linux.
struct Termios {
mut:
	c_iflag int
	c_oflag int
	c_cflag int
	c_lflag int
	c_cc    [12]int // NCCS == 12. Can't use the defined value here
}

// Winsize stores the screen information on Linux.
struct Winsize {
	ws_row    u16
	ws_col    u16
	ws_xpixel u16
	ws_ypixel u16
}

// Readline is the key struct for reading and holding user input via a terminal.
// Example: import readline { Readline }
pub struct Readline {
mut:
	is_raw            bool
	orig_termios      Termios // Linux
	current           ustring // Line being edited
	cursor            int // Cursor position
	overwrite         bool
	cursor_row_offset int
	prompt            string
	prompt_offset     int
	previous_lines    []ustring
	search_index      int
	is_tty            bool
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
