// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Linux version
// Will serve as more advanced input method
// Based on the work of https://github.com/AmokHuginnsson/replxx 

module readline

import term

#include <termios.h>
#include <sys/ioctl.h>

// Used to change the terminal options
struct termios {
mut:
  c_iflag int
  c_oflag int
  c_cflag int
  c_lflag int
  c_cc [12]int //NCCS == 12. Cant use the defined value here
}

// Used to collect the screen informations
struct winsize {
  ws_row u16
  ws_col u16
  ws_xpixel u16
  ws_ypixel u16
}

struct Readline {
mut:
  is_raw bool
  orig_termios termios
  current string // Line being edited
  cursor int // Cursor position
  overwrite bool
  cursor_row_offset int
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
      switch sequence {
        case `C`: return Action.move_cursor_right
        case `D`: return Action.move_cursor_left
        case `E`: return Action.history_next
        case `F`: return Action.history_previous
        case `1`: return r.analyse_extended_control()
        case `3`: return r.analyse_extended_control_no_eat()
      }
  }
  return Action.nothing
}

fn (r Readline) analyse_extended_control() Action {
  r.read_char() // Removes ;
  c := r.read_char()
  switch c {
    case `5`:
      direction := r.read_char()
      switch direction {
        case `C`: return Action.move_cursor_word_right
        case `D`: return Action.move_cursor_word_left
      }
  }
  return Action.nothing
}

fn (r Readline) analyse_extended_control_no_eat() Action {
  c := r.read_char()
  switch c {
    case `~`: return Action.delete_right // Suppr key
  }
  return Action.nothing
}

fn (r mut Readline) execute(a Action, c byte) bool {
  switch a {
    case Action.eof: return true
    case Action.insert_character: r.insert_character(c)
    case Action.commit_line: return r.commit_line()
    case Action.delete_left: r.delete_character()
    case Action.delete_right: r.suppr_character()
    case Action.move_cursor_left: r.move_cursor_left()
    case Action.move_cursor_right: r.move_cursor_right()
    case Action.move_cursor_begining: r.move_cursor_begining()
    case Action.move_cursor_end: r.move_cursor_end()
    case Action.move_cursor_word_left: r.move_cursor_word_left()
    case Action.move_cursor_word_right: r.move_cursor_word_right()
  }
  return false
}

fn get_screen_columns() int {
  ws := winsize{}
  cols := if C.ioctl(1, C.TIOCGWINSZ, &ws) == -1 { 80 } else { ws.ws_col }
  return int(cols)
}

fn shift_cursor(xpos int, yoffset int) {
  if yoffset != 0 {
    if yoffset > 0 {
      term.cursor_down(yoffset)
    }
    else {
      term.cursor_up(- yoffset)
    }
  }
  // Absolute X position
  print('\x1b[${xpos + 1}G')
}

fn calculate_screen_position(x_in int, y_in int, screen_columns int, char_count int, out mut []int) {
  mut x := x_in
  mut y := y_in
  out[0] = x
  out[1] = y
  for chars_remaining := char_count; chars_remaining > 0; {
    chars_this_row := if ( (x + chars_remaining) < screen_columns) { chars_remaining } else { screen_columns - x }
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
}

// Will redraw the line
fn (r mut Readline) refresh_line() {
  mut end_of_input := [0, 0]
  calculate_screen_position(0, 0, get_screen_columns(), r.current.len, mut end_of_input)
  end_of_input[1] += r.current.count('\n')
  mut cursor_pos := [0, 0]
  calculate_screen_position(0, 0, get_screen_columns(), r.cursor, mut cursor_pos)

  shift_cursor(0, -r.cursor_row_offset)
  term.erase_toend()
  print(r.current)
  if end_of_input[0] == 0 && end_of_input[1] > 0 {
    print('\n')
  }
  shift_cursor(cursor_pos[0], - (end_of_input[1] - cursor_pos[1]))
  r.cursor_row_offset = cursor_pos[1]
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
  if r.cursor == r.current.len && r.current.len < get_screen_columns() {
    print(c.str())
  } else {
    r.refresh_line()
  }
}

// Removes the character behind cursor.
fn (r mut Readline) delete_character() {
  if r.cursor <= 0 {
    return
  }
  r.cursor--
  r.current = r.current.left(r.cursor) + r.current.right(r.cursor + 1)
  r.refresh_line()
}

// Removes the character in front of cursor.
fn (r mut Readline) suppr_character() {
  if r.cursor > r.current.len {
    return
  }
  r.current = r.current.left(r.cursor) + r.current.right(r.cursor + 1)
  r.refresh_line()
}

// Add a line break then stops the main loop
fn (r mut Readline) commit_line() bool {
  r.current = r.current + '\n'
  return true
}

fn (r mut Readline) move_cursor_left() {
  if r.cursor > 0 {
    r.cursor--
    r.refresh_line()
  }
}

fn (r mut Readline) move_cursor_right() {
  if r.cursor < r.current.len {
    r.cursor++
    r.refresh_line()
  }
}

fn (r mut Readline) move_cursor_begining() {
  r.cursor = 0
  r.refresh_line()
}

fn (r mut Readline) move_cursor_end() {
  r.cursor = r.current.len
  r.refresh_line()
}

// Check if the character is considered as a word-breaking character
fn (r Readline) is_break_character(c byte) bool {
  break_characters := ' \t\v\f\a\b\r\n`~!@#$%^&*()-=+[{]}\\|;:\'",<.>/?'
  return break_characters.contains(c.str())
}

fn (r mut Readline) move_cursor_word_left() {
  if r.cursor > 0 {
    for ; r.cursor > 0 && r.is_break_character(r.current[r.cursor - 1]); r.cursor-- {}
    for ; r.cursor > 0 && !r.is_break_character(r.current[r.cursor - 1]); r.cursor-- {}
    r.refresh_line()
  }
}

fn (r mut Readline) move_cursor_word_right() {
  if r.cursor < r.current.len {
    for ; r.cursor < r.current.len && r.is_break_character(r.current[r.cursor]); r.cursor++ {}
    for ; r.cursor < r.current.len && !r.is_break_character(r.current[r.cursor]); r.cursor++ {}
    r.refresh_line()
  }
}
