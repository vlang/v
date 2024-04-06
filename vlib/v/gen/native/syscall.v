// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module native

// unix syscall names
enum SysCall {
	write
	exit
}

fn (mut g Gen) nsyscall_macos(syscall SysCall) i32 {
	return match syscall {
		.write {
			0x2000004
		}
		.exit {
			0x2000001
		}
	}
}

fn (mut g Gen) nsyscall_linux(syscall SysCall) i32 {
	return match syscall {
		.write {
			1
		}
		.exit {
			60
		}
	}
}

fn (mut g Gen) nsyscall(syscall SysCall) i32 {
	match g.pref.os {
		.linux {
			return g.nsyscall_linux(syscall)
		}
		.macos {
			return g.nsyscall_macos(syscall)
		}
		else {
			panic('syscall on windows :(')
			// g.n_error('syscall is unsupported on platform ${g.pref.os}')
		}
	}
	return 0
}
