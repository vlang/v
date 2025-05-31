// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Serves as a more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module readline

import term.termios

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
pub mut:
	is_raw                 bool
	orig_termios           termios.Termios // Linux
	current                []rune          // Line being edited
	cursor                 int             // Cursor position
	overwrite              bool
	cursor_row_offset      int
	prompt                 string
	prompt_offset          int
	previous_lines         [][]rune
	skip_empty             bool // skip the empty lines when calling .history_previous()
	search_index           int
	is_tty                 bool
	last_prefix_completion []rune
	last_completion_offset int
	completion_list        []string
	completion_callback    fn (string) []string = unsafe { nil }
}
