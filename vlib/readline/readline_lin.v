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
struct Termios {
mut:
  c_iflag int
  c_oflag int
  c_cflag int
  c_lflag int
  c_cc [12]int //NCCS == 12. Cant use the defined value here
}

// Used to collect the screen information
struct Winsize {
  ws_row u16
  ws_col u16
  ws_xpixel u16
  ws_ypixel u16
}

struct Readline {
mut:
  is_raw bool
  orig_termios Termios
  current ustring // Line being edited
  cursor int // Cursor position
  overwrite bool
  cursor_row_offset int
  prompt string
  previous_lines []ustring
  search_index int
  is_tty bool
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
  clear_screen
  suspend
}

// Toggle raw mode of the terminal by changing its attributes
pub fn (r mut Readline) enable_raw_mode() {
  if ( C.tcgetattr(0, &r.orig_termios) == -1 ) {
    r.is_tty = false
    return
  }
  mut raw := r.orig_termios
  raw.c_iflag &= ~( C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON )
  raw.c_cflag |=  ( C.CS8 )
  raw.c_lflag &= ~( C.ECHO | C.ICANON | C.IEXTEN | C.ISIG )
  raw.c_cc[C.VMIN] = 1
  raw.c_cc[C.VTIME] = 0
  C.tcsetattr(0, C.TCSADRAIN, &raw)
  r.is_raw = true
  r.is_tty = true
}

// Not catching the SIGUSER (CTRL+C) Signal
pub fn (r mut Readline) enable_raw_mode2() {
  if ( C.tcgetattr(0, &r.orig_termios) == -1 ) {
    r.is_tty = false
    return
  }
  mut raw := r.orig_termios
  raw.c_iflag &= ~( C.BRKINT | C.ICRNL | C.INPCK | C.ISTRIP | C.IXON )
  raw.c_cflag |=  ( C.CS8 )
  raw.c_lflag &= ~( C.ECHO | C.ICANON | C.IEXTEN )
  raw.c_cc[C.VMIN] = 1
  raw.c_cc[C.VTIME] = 0
  C.tcsetattr(0, C.TCSADRAIN, &raw)
  r.is_raw = true
  r.is_tty = true
}

// Reset back the terminal to its default value
pub fn (r Readline) disable_raw_mode() {
  if r.is_raw {
    C.tcsetattr(0, C.TCSADRAIN, &r.orig_termios)
  }
}

// Read single char
fn (r Readline) read_char() int {
  return utf8_getchar()
}

// Main function of the readline module
// Will loop and ingest characters until EOF or Enter
// Returns the completed line
pub fn (r mut Readline) read_line_utf8(prompt string) ustring {
  r.current = ''.ustring()
  r.cursor = 0
  r.prompt = prompt
  r.search_index = 0
  if r.previous_lines.len <= 1 {
    r.previous_lines << ''.ustring()
    r.previous_lines << ''.ustring()
  }
  else {
    r.previous_lines[0] = ''.ustring()
  }

  print(r.prompt)
  for {
    c := r.read_char()
    a := r.analyse(c)
    if r.execute(a, c) {
      break
    }
  }
  r.previous_lines[0] = ''.ustring()
  r.search_index = 0
  return r.current
}

pub fn (r mut Readline) read_line(prompt string) string {
  return r.read_line_utf8(prompt).s
}

fn (r Readline) analyse(c byte) Action {
  switch c {
    case `\0`: return Action.eof
    case 0x3 : return Action.eof // End of Text
    case 0x4 : return Action.eof // End of Transmission
    case 255 : return Action.eof
    case `\n`: return Action.commit_line
    case `\r`: return Action.commit_line
    case `\f`: return Action.clear_screen // CTRL + L
    case `\b`: return Action.delete_left // Backspace
    case 127 : return Action.delete_left // DEL
    case 27  : return r.analyse_control() // ESC
    case 1   : return Action.move_cursor_begining // ^A
    case 5   : return Action.move_cursor_end // ^E
    case 26  : return Action.suspend // CTRL + Z, SUB
    default  : return if c >= ` ` { Action.insert_character } else { Action.nothing }
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
        case `B`: return Action.history_next
        case `A`: return Action.history_previous
        case `1`: return r.analyse_extended_control()
        case `2`: return r.analyse_extended_control_no_eat(sequence)
        case `3`: return r.analyse_extended_control_no_eat(sequence)
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

fn (r Readline) analyse_extended_control_no_eat(last_c byte) Action {
  c := r.read_char()
  switch c {
    case `~`:
      switch last_c {
        case `3`: return Action.delete_right // Suppr key
        case `2`: return Action.overwrite
      }
  }
  return Action.nothing
}

fn (r mut Readline) execute(a Action, c int) bool {
  switch a {
    case Action.eof: return r.eof()
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
    case Action.history_previous: r.history_previous()
    case Action.history_next: r.history_next()
    case Action.overwrite: r.switch_overwrite()
    case Action.clear_screen: r.clear_screen()
    case Action.suspend: r.suspend()
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
  calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.current.len, mut end_of_input)
  end_of_input[1] += r.current.count('\n'.ustring())
  mut cursor_pos := [0, 0]
  calculate_screen_position(r.prompt.len, 0, get_screen_columns(), r.cursor, mut cursor_pos)

  shift_cursor(0, -r.cursor_row_offset)
  term.erase_toend()
  print(r.prompt)
  print(r.current)
  if end_of_input[0] == 0 && end_of_input[1] > 0 {
    print('\n')
  }
  shift_cursor(cursor_pos[0], - (end_of_input[1] - cursor_pos[1]))
  r.cursor_row_offset = cursor_pos[1]
}

// End the line without a newline
fn (r mut Readline) eof() bool {
  r.previous_lines.insert(1, r.current)
  r.cursor = r.current.len
  r.refresh_line()
  return true
}

fn (r mut Readline) insert_character(c int) {
  if !r.overwrite || r.cursor == r.current.len {
    r.current = r.current.left(r.cursor).ustring() + utf32_to_str(u32(c)).ustring() + r.current.right(r.cursor).ustring()
  } else {
    r.current = r.current.left(r.cursor).ustring() + utf32_to_str(u32(c)).ustring() + r.current.right(r.cursor + 1).ustring()
  }
  r.cursor++
  // Refresh the line to add the new character
  if r.is_tty {
    r.refresh_line()
  }
}

// Removes the character behind cursor.
fn (r mut Readline) delete_character() {
  if r.cursor <= 0 {
    return
  }
  r.cursor--
  r.current = r.current.left(r.cursor).ustring() + r.current.right(r.cursor + 1).ustring()
  r.refresh_line()
}

// Removes the character in front of cursor.
fn (r mut Readline) suppr_character() {
  if r.cursor > r.current.len {
    return
  }
  r.current = r.current.left(r.cursor).ustring() + r.current.right(r.cursor + 1).ustring()
  r.refresh_line()
}

// Add a line break then stops the main loop
fn (r mut Readline) commit_line() bool {
  r.previous_lines.insert(1, r.current)
  a := '\n'.ustring()
  r.current = r.current + a
  r.cursor = r.current.len
  r.refresh_line()
  if r.is_tty {
    println('')
  }
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
fn (r Readline) is_break_character(c string) bool {
  break_characters := ' \t\v\f\a\b\r\n`~!@#$%^&*()-=+[{]}\\|;:\'",<.>/?'
  return break_characters.contains(c)
}

fn (r mut Readline) move_cursor_word_left() {
  if r.cursor > 0 {
    for ; r.cursor > 0 && r.is_break_character(r.current.at(r.cursor - 1)); r.cursor-- {}
    for ; r.cursor > 0 && !r.is_break_character(r.current.at(r.cursor - 1)); r.cursor-- {}
    r.refresh_line()
  }
}

fn (r mut Readline) move_cursor_word_right() {
  if r.cursor < r.current.len {
    for ; r.cursor < r.current.len && r.is_break_character(r.current.at(r.cursor)); r.cursor++ {}
    for ; r.cursor < r.current.len && !r.is_break_character(r.current.at(r.cursor)); r.cursor++ {}
    r.refresh_line()
  }
}

fn (r mut Readline) switch_overwrite() {
  r.overwrite = !r.overwrite
}

fn (r mut Readline) clear_screen() {
  term.set_cursor_position(1, 1)
  term.erase_clear()
  r.refresh_line()
}

fn (r mut Readline) history_previous() {
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

fn (r mut Readline) history_next() {
  if r.search_index <= 0 {
    return
  }
  r.search_index--
  r.current = r.previous_lines[r.search_index]
  r.cursor = r.current.len
  r.refresh_line()
}

fn (r mut Readline) suspend() {
  C.raise(C.SIGSTOP)
  r.refresh_line()
}
