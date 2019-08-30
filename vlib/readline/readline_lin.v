// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Linux version
// Will serve as more advanced input method
// Based on the work of https://github.com/AmokHuginnsson/replxx 

module readline

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
pub mut:
  is_raw bool
  orig_termios termios
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
fn (r Readline) read_char() byte {
  return C.getchar()
}

// Delete character in the string and in the terminal
fn (r Readline) delete_character(s string, cursor int) string {
  print('\b \b')
  return s.left(cursor - 1) + s.right(cursor)
} 

pub fn (r Readline) read_line() string {
  mut s := ''
  mut cursor := 0
  
  for {
    c := r.read_char()
    // Check if ending string
    if c == `\0` || c == 0x3 || c == 0x4 || c == 255 {
      break
    }
    // Check if backspace
    if c == `\b` || c == 127 {
      if cursor > 0 {
        s = r.delete_character(s, cursor)
        cursor--
      }
    } else {
      // Add new character according to cursor position
      s = s.left(cursor) + c.str() + s.right(cursor)
      cursor++
    }
    // Check if enter
    if c == `\n` || c == `\r` {
      println('')
      break
    } else {
      print(c.str())
    }
  }
  return s
}