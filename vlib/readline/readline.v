// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Will serve as more advanced input method
// Based on the work of https://github.com/AmokHuginnsson/replxx

module readline

// Linux
// Used to change the terminal options
struct Termios {
mut:
  c_iflag int
  c_oflag int
  c_cflag int
  c_lflag int
  c_cc [12]int //NCCS == 12. Cant use the defined value here
}

// Linux
// Used to collect the screen information
struct Winsize {
  ws_row u16
  ws_col u16
  ws_xpixel u16
  ws_ypixel u16
}

pub struct Readline {
mut:
  is_raw bool
  orig_termios Termios // Linux
  current ustring // Line being edited
  cursor int // Cursor position
  overwrite bool
  cursor_row_offset int
  prompt string
  prompt_offset int
  previous_lines []ustring
  search_index int
  is_tty bool
}
