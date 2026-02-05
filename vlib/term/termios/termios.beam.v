// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM-specific termios stubs
// Terminal I/O control is not directly available on BEAM, but we provide
// stub implementations that allow code to compile and run with limited
// functionality.
//
module termios

// Terminal control character array length (POSIX standard is typically 20-32)
const cclen = 20

// Type aliases for terminal flags and settings
type TcFlag = usize
type Speed = usize
type Cc = u8

// Terminal control actions for tcsetattr
pub const tcsanow = 0 // Make changes immediately
pub const tcsadrain = 1 // Drain output, then change
pub const tcsaflush = 2 // Drain output, flush input, then change

// Input mode flags (c_iflag)
pub const brkint = 0x0002 // Signal interrupt on break
pub const icrnl = 0x0100 // Map CR to NL on input
pub const inpck = 0x0010 // Enable input parity check
pub const istrip = 0x0020 // Strip 8th bit off characters
pub const ixon = 0x0400 // Enable start/stop output control

// Control mode flags (c_cflag)
pub const cs8 = 0x0300 // 8 bits per character

// Local mode flags (c_lflag)
pub const echo = 0x0008 // Echo input characters
pub const icanon = 0x0002 // Canonical input (line editing)
pub const iexten = 0x8000 // Enable extended input processing
pub const isig = 0x0001 // Enable signals

// Control character indices (c_cc)
pub const vmin = 6 // Minimum number of characters for read
pub const vtime = 5 // Timeout in deciseconds for read

// ioctl request for window size
pub const tiocgwinsz = 0x5413

// Termios stores the terminal options
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

// flag provides a termios flag of the correct size
// for the underlying Termios structure
@[inline]
pub fn flag(value int) TcFlag {
	return usize(value)
}

// invert is a platform-dependent way to bitwise NOT (~) TcFlag
// as its length varies across platforms
@[inline]
pub fn invert(value TcFlag) TcFlag {
	return ~usize(value)
}

// tcgetattr gets the parameters associated with the terminal.
// On BEAM: This is a stub that returns success (0) but doesn't
// modify the termios structure, as direct terminal control is
// not available on the BEAM runtime.
@[inline]
pub fn tcgetattr(fd int, mut termios_p Termios) int {
	// BEAM doesn't support direct terminal control
	// Return 0 (success) to allow code to proceed
	// The termios struct remains with default/zero values
	return 0
}

// tcsetattr sets the parameters associated with the terminal.
// On BEAM: This is a stub that returns success (0) but doesn't
// actually change terminal settings.
@[inline]
pub fn tcsetattr(fd int, optional_actions int, mut termios_p Termios) int {
	// BEAM doesn't support direct terminal control
	// Return 0 (success) to allow code to proceed
	return 0
}

// ioctl performs device-specific I/O operations.
// On BEAM: This is a stub that returns -1 (failure) as ioctl
// is not available on the BEAM runtime.
@[inline]
pub fn ioctl(fd int, request u64, arg voidptr) int {
	// BEAM doesn't support ioctl
	// Return -1 to indicate failure, which typically causes
	// callers to use fallback/default values
	return -1
}

// set_state applies the flags in the `new_state` to the descriptor `fd`.
// On BEAM: This is a stub that returns success (0) but doesn't
// actually change terminal settings.
pub fn set_state(fd int, new_state Termios) int {
	mut x := new_state
	return tcsetattr(0, tcsanow, mut x)
}

// disable_echo disables echoing characters as they are typed,
// when that Termios state is later set with termios.set_state(fd, t).
// On BEAM: Modifies the struct flag but actual echo control
// depends on the BEAM runtime's terminal handling.
pub fn (mut t Termios) disable_echo() {
	t.c_lflag &= invert(usize(echo))
}
