// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

fn builtin_init() {
	$if gcboehm ? {
		$if !gc_warn_on_stderr ? {
			gc_set_warn_proc(internal_gc_warn_proc_none)
		}
	}
}

fn break_if_debugger_attached() {
	unsafe {
		mut ptr := &voidptr(0)
		*ptr = nil
	}
}

@[noreturn]
pub fn panic_lasterr(base string) {
	// TODO: use strerror_r and errno
	panic(base + ' unknown')
}
