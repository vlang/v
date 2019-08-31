// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Linux version
// Will serve as more advanced input method
// Based on the work of https://github.com/AmokHuginnsson/replxx 

module readline

import term

#include <termios.h>

struct termios {
mut:
  c_iflag int
  c_oflag int
  c_cflag int
  c_lflag int
  c_cc [12]int //NCCS == 12. Cant use the defined value here
}

struct Readline {
mut:
  is_raw bool
  orig_termios termios
  current string // Line being edited
  cursor int // Cursor position
  overwrite bool
}

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
}

// Toggle raw mode of the terminal by changing its attributes
pub fn (r mut Readline) enable_raw_mode() {
  if ( C.tcgetattr(0, &r.orig_termios) == -1 ) {
    panic('No tty')
  }
  mut raw := r.orig_termios
  raw.c_iflag &= ~( C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON )
  raw.c_cflag |=  ( C.CS8 )
  raw.c_lflag &= ~( C.ECHO | C.ICANON | C.IEXTEN | C.ISIG )
  raw.c_cc[C.VMIN] = 1
  raw.c_cc[C.VTIME] = 0
  C.tcsetattr(0, C.TCSADRAIN, &raw)
  r.is_raw = true
}

// Not catching the SIGUSER (CTRL+C) Signal
pub fn (r Readline) enable_raw_mode2() {
  if ( C.tcgetattr(0, &r.orig_termios) == -1 ) {
    panic('No tty')
  }
  mut raw := r.orig_termios
  raw.c_iflag &= ~( C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON )
  raw.c_cflag |=  ( C.CS8 )
  raw.c_lflag &= ~( C.ECHO | C.ICANON | C.IEXTEN )
  raw.c_cc[C.VMIN] = 1
  raw.c_cc[C.VTIME] = 0
  C.tcsetattr(0, C.TCSADRAIN, &raw)
}

// Reset back the terminal to its default value
pub fn (r Readline) disable_raw_mode() {
  if r.is_raw {
    C.tcsetattr(0, C.TCSADRAIN, &r.orig_termios)
  }
}

// Read single char
// TODO: Maybe handle UNICODE
fn (r Readline) read_char() byte {
  return C.getchar()
}

// Main function of the readline module
// Will loop and ingest characters until EOF or Enter
// Returns the completed line
pub fn (r mut Readline) read_line() string {
  r.current = ''
  r.cursor = 0

  for {
    c := r.read_char()
    a := r.analyse(c)
    if r.execute(a, c) {
      break
    }
  }
  return r.current
}

fn (r Readline) analyse(c byte) Action {
  switch c {
    case `\0`: return Action.eof
    case 0x3 : return Action.eof // End of Text
    case 0x4 : return Action.eof // End of Transmission
    case 255 : return Action.eof
    case `\n`: return Action.commit_line
    case `\r`: return Action.commit_line
    case `\b`: return Action.delete_left // Backspace
    case 127 : return Action.delete_left // DEL
    case 27  : return r.analyse_control() // ESC
    case 1   : return Action.move_cursor_begining // ^A
    case 5   : return Action.move_cursor_end // ^E
    default  : return Action.insert_character
  }
}

fn (r Readline) analyse_control() Action {
  c := r.read_char()
  switch c {
    case `[`:
      sequence := r.read_char()
      println('seq: $sequence')
      switch sequence {
        case `C`: return Action.move_cursor_right
        case `D`: return Action.move_cursor_left
        case `E`: return Action.history_next
        case `F`: return Action.history_previous
        // TODO : CTRL+Left and CTRL+Right
      }
  }
  return Action.nothing
}

fn (r mut Readline) execute(a Action, c byte) bool {
  switch a {
    case Action.eof: return true
    case Action.insert_character: r.insert_character(c)
    case Action.commit_line: return r.commit_line()
    case Action.delete_left: r.delete_character()
  }
  return false
}

// Will redraw the line
// TODO: Refreshing on wrapped line
fn (r Readline) refresh_line() {
  term.cursor_back(r.cursor)
  term.erase_toend()
  print(r.current)
  term.cursor_back(r.current.len - r.cursor)
}

fn (r mut Readline) insert_character(c byte) {
  // Small ASCII, to expand
  if c >= 127 {
    return
  }
  // TODO: Add character if overwrite at end
  if !r.overwrite {
    r.current = r.current.left(r.cursor) + c.str() + r.current.right(r.cursor)
  } else {
    // Overwriting
  }
  r.cursor++
  // Simply print new character if at end of line
  // Otherwise refresh the line if cursor != r.current.len
  if r.cursor == r.current.len {
    print(c.str())
  } else {
    //
  }
}

// Removes the character behind cursor.
fn (r mut Readline) delete_character() {
  if r.cursor <= 0 {
    return
  }
  r.current = r.current.left(r.cursor - 1) + r.current.right(r.cursor)
  r.refresh_line()
  r.cursor--
}

// Add a line break then stops the main loop
fn (r mut Readline) commit_line() bool {
  r.insert_character(`\n`)
  return true
}
