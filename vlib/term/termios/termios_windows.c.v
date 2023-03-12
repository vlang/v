// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// TODO Windows version needs to be implemented.
// Will serve as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module termios

type TcFlag = int
type Speed = int
type Cc = u8

pub fn flag(value int) TcFlag {
	$compile_error('feature not implemented')
}

pub fn invert(value TcFlag) TcFlag {
	$compile_error('feature not implemented')
}

struct Termios {
}

pub fn tcgetattr(fd int, mut termios_p Termios) int {
	$compile_error('feature not implemented')
}

pub fn tcsetattr(fd int, optional_actions int, mut termios_p Termios) int {
	$compile_error('feature not implemented')
}
