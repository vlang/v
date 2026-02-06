// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module termios

// not used but needed for function declarations
type TcFlag = int
type Speed = int
type Cc = u8

// Termios represents platform dependent flags representing the terminal state.
// Linux      https://github.com/lattera/glibc/blob/master/sysdeps/unix/sysv/linux/bits/termios.h
// OpenBSD    https://github.com/openbsd/src/blob/master/sys/sys/termios.h
// FreeBSD    https://web.mit.edu/freebsd/head/sys/sys/_termios.h
// Solaris    https://github.com/omniti-labs/illumos-omnios/blob/master/usr/src/uts/common/sys/termios.h
// DragonFly  https://gitweb.dragonflybsd.org/dragonfly.git/blob_plain/HEAD:/sys/sys/_termios.h
// QNX        https://github.com/vocho/openqnx/blob/master/trunk/lib/c/public/termios.h
pub struct Termios {
pub mut:
	c_iflag  TcFlag
	c_oflag  TcFlag
	c_cflag  TcFlag
	c_lflag  TcFlag
	c_line   Cc
	c_cc     [32]Cc
	c_ispeed Speed
	c_ospeed Speed
}

// flag provides a termios flag of the correct size for the underlying C.termios structure.
pub fn flag(value int) TcFlag {
	return TcFlag(value)
}

// invert is a platform dependant way to bitwise NOT (~) TcFlag as its length varies across platforms.
// It is only implemented for Unix like OSes.
pub fn invert(value TcFlag) TcFlag {
	return TcFlag(~int(value))
}

// tcgetattr is an unsafe wrapper around C.termios and keeps its semantic.
// It is only implemented for Unix like OSes
pub fn tcgetattr(fd int, mut t Termios) int {
	$if wasm32_emscripten {
		return 0
	}
	eprintln('term.termios: tcgetattr,tcsetattr,ioctl,set_state are not implemented for the platform')
	eprintln('tcgetattr, fd: ${fd}, t: ${t}')
	return 0
}

// tcsetattr is an unsafe wrapper around C.termios and keeps its semantic.
// It is only implemented for Unix like OSes.
pub fn tcsetattr(fd int, optional_actions int, mut t Termios) int {
	eprintln('tcsetattr, fd: ${fd}, optional_actions: ${optional_actions}, t: ${t}')
	return 0
}

// ioctl is an unsafe wrapper around C.ioctl and keeps its semantic.
// It is only implemented for Unix like OSes
pub fn ioctl(fd int, request u64, arg voidptr) int {
	eprintln('ioctl, fd: ${fd}, request: ${request}, arg: ${arg}')
	return 0
}

// set_state applies the flags in the `new_state` to the descriptor `fd`.
pub fn set_state(fd int, new_state Termios) int {
	eprintln('set_state, fd: ${fd} | new_state: ${new_state}')
	return 0
}

// disable_echo disables echoing characters as they are typed, when that Termios state is later set with termios.set_state(fd,t).
pub fn (mut t Termios) disable_echo() {
	t.c_lflag &= invert(8)
}
