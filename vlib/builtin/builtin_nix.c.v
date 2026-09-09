// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

@[markused]
fn builtin_init() {
	$if gcboehm ? {
		$if !gc_warn_on_stderr ? {
			gc_set_warn_proc(internal_gc_warn_proc_none)
		}
	}
	$if !vinix {
		unbuffer_stdout()
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

// write_buf_to_console is a Windows-only helper; on non-Windows platforms
// it is a no-op stub so that callers guarded by `$if windows { ... }` still
// type-check on other targets.
fn write_buf_to_console(fd int, buf &u8, buf_len int) bool {
	return false
}

// write_buf_to_fd_kernel32 is a Windows-only helper for the minimal V2 PE path.
// Keep a non-Windows stub so optional compile-time branches type-check while
// bootstrapping V on other hosts.
fn write_buf_to_fd_kernel32(fd int, buf &u8, buf_len int) bool {
	return false
}

fn write_buf_to_fd_kernel32_or_exit(fd int, buf &u8, buf_len int) {}

fn write_buf_to_fd_windows_non_minimal(fd int, buf &u8, buf_len int) {}
