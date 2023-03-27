// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// TODO Mac version needs to be implemented
// Will serve as more advanced input method
// based on the work of https://github.com/AmokHuginnsson/replxx
//

module termios

// not used but needed for function declarations
type TcFlag = int
type Speed = int
type Cc = u8

// flag provides a termios flag of the correct size
// for the underlying C.termios structure
// It is only implemented for Unix like OSes
pub fn flag(value int) TcFlag {
	$compile_error('feature not available')
}

// invert is a platform dependant way to bitwise NOT (~) TcFlag
// as its length varies across platforms
// It is only implemented for Unix like OSes
pub fn invert(value TcFlag) TcFlag {
	$compile_error('feature not available')
}

// termios definitions
// Linux      https://github.com/lattera/glibc/blob/master/sysdeps/unix/sysv/linux/bits/termios.h
// OpenBSD    https://github.com/openbsd/src/blob/master/sys/sys/termios.h
// FreeBSD    https://web.mit.edu/freebsd/head/sys/sys/_termios.h
// Solaris    https://github.com/omniti-labs/illumos-omnios/blob/master/usr/src/uts/common/sys/termios.h
// DragonFly  https://gitweb.dragonflybsd.org/dragonfly.git/blob_plain/HEAD:/sys/sys/_termios.h
// QNX        https://github.com/vocho/openqnx/blob/master/trunk/lib/c/public/termios.h

pub struct Termios {
}

// tcgetattr is an unsafe wrapper around C.termios and keeps its semantic
// It is only implemented for Unix like OSes
pub fn tcgetattr(fd int, mut termios_p Termios) int {
	$compile_error('feature not available')
}

// tcsetattr is an unsafe wrapper around C.termios and keeps its semantic
// It is only implemented for Unix like OSes
pub fn tcsetattr(fd int, optional_actions int, mut termios_p Termios) int {
	$compile_error('feature not available')
}

// ioctl is an unsafe wrapper around C.ioctl and keeps its semantic
[inline]
pub fn ioctl(fd int, request u64, arg voidptr) int {
	$compile_error('feature not available')
}
