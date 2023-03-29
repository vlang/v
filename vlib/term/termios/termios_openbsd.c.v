// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Serves as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//
module termios

#include <termios.h>
#include <sys/ioctl.h>

// https://github.com/openbsd/src/blob/master/sys/sys/termios.h

const cclen = 20

type TcFlag = int
type Speed = int
type Cc = u8

// Termios stores the terminal options
struct C.termios {
mut:
	c_iflag  TcFlag
	c_oflag  TcFlag
	c_cflag  TcFlag
	c_lflag  TcFlag
	c_cc     [cclen]Cc
	c_ispeed Speed
	c_ospeed Speed
}

fn C.tcgetattr(fd int, termios_p &C.termios) int

fn C.tcsetattr(fd int, optional_actions int, const_termios_p &C.termios) int

fn C.ioctl(fd int, request u64, arg voidptr) int

// flag provides a termios flag of the correct size
// for the underlying C.termios structure
[inline]
pub fn flag(value int) TcFlag {
	return int(value)
}

// invert is a platform dependant way to bitwise NOT (~) TcFlag
// as its length varies across platforms
[inline]
pub fn invert(value TcFlag) TcFlag {
	return ~int(value)
}

pub struct Termios {
pub mut:
	c_iflag  TcFlag
	c_oflag  TcFlag
	c_cflag  TcFlag
	c_lflag  TcFlag
	c_cc     [cclen]Cc
	c_ispeed Speed
	c_ospeed Speed
}

// tcgetattr is an unsafe wrapper around C.termios and keeps its semantic
[inline]
pub fn tcgetattr(fd int, mut termios_p Termios) int {
	unsafe {
		return C.tcgetattr(fd, &C.termios(termios_p))
	}
}

// tcsetattr is an unsafe wrapper around C.termios and keeps its semantic
[inline]
pub fn tcsetattr(fd int, optional_actions int, mut termios_p Termios) int {
	unsafe {
		return C.tcsetattr(fd, optional_actions, &C.termios(termios_p))
	}
}

// ioctl is an unsafe wrapper around C.ioctl and keeps its semantic
[inline]
pub fn ioctl(fd int, request u64, arg voidptr) int {
	unsafe {
		return C.ioctl(fd, request, arg)
	}
}
