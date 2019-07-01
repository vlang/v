// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module term

// Sources for ANSI Control Sequences
// https://github.com/RajeshPatkarInstitute/Panim
// https://www.gnu.org/software/screen/manual/html_node/Control-Sequences.html
// https://en.wikipedia.org/wiki/ANSI_escape_code

// Support for Windows
// https://en.wikipedia.org/wiki/ANSI.SYS
// #include <windows.h>
// C.SetConsoleMode(ENABLE_VIRTUAL_TERMINAL_INPUT)

// Setting cursor to the given position
// x is the x coordinate
// y is the y coordinate
pub fn set_cursor_position(x int,y int) {
    print('\x1b[$y;$x;H')
}

// n is number of cells
// direction: A is up / North
// direction: B is down / South
// direction: C is forward / East
// direction: D is backward / West
pub fn move(n int,direction string) string {
    return '\x1b[$n$direction'
}

pub fn cursor_up(n int) {
    print(move(n,'A'))
}

pub fn cursor_down(n int) {
    print(move(n,'B'))
}

pub fn cursor_forward(n int) {
    print(move(n,'C'))
}

pub fn cursor_back(n int) {
    print(move(n,'D'))
}

// type: 0 -> current cursor postion to end of the screen
// type: 1 -> current cursor postion to beginning of the screen
// type: 2 -> clears entire screen
// type: 3 -> clears entire screen and also delete scrollback buffer
pub fn erase_display(t string) {
    print('\x1b[' + t + 'J')
}

pub fn erase_toend()
{
    erase_display('0')
}

pub fn erase_tobeg()
{
    erase_display('1')
}

pub fn erase_clear()
{
    erase_display('2')
}

pub fn erase_del_clear()
{
    erase_display('3')
}

// type: 0 -> current cursor postion to end of the line
// type: 1 -> current cursor postion to beginning of the line
// type: 2 -> clears entire line
// Note: Cursor position does not change
pub fn erase_line(t string) {
    print('\x1b[' + t + 'K')
}

pub fn erase_line_toend()
{
    erase_line('0')
}

pub fn erase_line_tobeg()
{
    erase_line('1')
}

pub fn erase_line_clear()
{
    erase_line('2')
}

// Will make cursor appear if not visible
pub fn show_cursor()
{
	print('\x1b[?25h')
}

// Will make cursor invisible
pub fn hide_cursor()
{
	print('\x1b[?25l')
}