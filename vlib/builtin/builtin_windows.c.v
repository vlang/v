// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module builtin

// g_original_codepage - used to restore the original windows console code page when exiting
__global g_original_codepage = u32(0)

pub type C.BOOL = int

pub type C.HINSTANCE = voidptr

pub type C.HICON = voidptr

pub type C.HCURSOR = voidptr

pub type C.HBRUSH = voidptr

pub type C.HWND = voidptr

pub type C.HGLOBAL = voidptr

pub type C.HANDLE = voidptr

pub type C.LRESULT = voidptr

// utf8 to stdout needs C.SetConsoleOutputCP(cp_utf8)
fn C.GetConsoleOutputCP() u32

fn C.SetConsoleOutputCP(wCodePageID u32) bool

fn restore_codepage() {
	C.SetConsoleOutputCP(g_original_codepage)
}

fn is_terminal(fd int) int {
	mut mode := u32(0)
	osfh := voidptr(C._get_osfhandle(fd))
	C.GetConsoleMode(osfh, voidptr(&mode))
	return int(mode)
}

const (
	std_output_handle                  = -11
	std_error_handle                   = -12
	enable_processed_output            = 1
	enable_wrap_at_eol_output          = 2
	evable_virtual_terminal_processing = 4
)

fn builtin_init() {
	g_original_codepage = C.GetConsoleOutputCP()
	C.SetConsoleOutputCP(cp_utf8)
	C.atexit(restore_codepage)
	if is_terminal(1) > 0 {
		C.SetConsoleMode(C.GetStdHandle(std_output_handle), enable_processed_output | enable_wrap_at_eol_output | evable_virtual_terminal_processing)
		C.SetConsoleMode(C.GetStdHandle(std_error_handle), enable_processed_output | enable_wrap_at_eol_output | evable_virtual_terminal_processing)
		unsafe {
			C.setbuf(C.stdout, 0)
			C.setbuf(C.stderr, 0)
		}
	}
	$if !no_backtrace ? {
		add_unhandled_exception_handler()
	}
}

// TODO copypaste from os
// we want to be able to use this here without having to `import os`
struct ExceptionRecord {
pub:
	// status_ constants
	code        u32
	flags       u32
	record      &ExceptionRecord = unsafe { nil }
	address     voidptr
	param_count u32
	// params []voidptr
}

struct ContextRecord {
	// TODO
}

struct ExceptionPointers {
pub:
	exception_record &ExceptionRecord = unsafe { nil }
	context_record   &ContextRecord   = unsafe { nil }
}

type VectoredExceptionHandler = fn (&ExceptionPointers) int

fn C.AddVectoredExceptionHandler(int, voidptr)

fn add_vectored_exception_handler(handler VectoredExceptionHandler) {
	C.AddVectoredExceptionHandler(1, voidptr(handler))
}

[callconv: stdcall]
fn unhandled_exception_handler(e &ExceptionPointers) int {
	match e.exception_record.code {
		// These are 'used' by the backtrace printer
		// so we dont want to catch them...
		0x4001000A, 0x40010006, 0xE06D7363 {
			return 0
		}
		else {
			println('Unhandled Exception 0x${e.exception_record.code:X}')
			print_backtrace_skipping_top_frames(5)
		}
	}

	return 0
}

fn add_unhandled_exception_handler() {
	add_vectored_exception_handler(VectoredExceptionHandler(voidptr(unhandled_exception_handler)))
}

fn C.IsDebuggerPresent() bool

fn C.__debugbreak()

fn break_if_debugger_attached() {
	$if tinyc {
		unsafe {
			mut ptr := &voidptr(0)
			*ptr = nil
			_ = ptr
		}
	} $else {
		if C.IsDebuggerPresent() {
			C.__debugbreak()
		}
	}
}

const (
	format_message_allocate_buffer = 0x00000100
	format_message_argument_array  = 0x00002000
	format_message_from_hmodule    = 0x00000800
	format_message_from_string     = 0x00000400
	format_message_from_system     = 0x00001000
	format_message_ignore_inserts  = 0x00000200
)

// return an error message generated from WinAPI's `LastError`
pub fn winapi_lasterr_str() string {
	err_msg_id := C.GetLastError()
	if err_msg_id == 8 {
		// handle this case special since `FormatMessageW()` might not work anymore
		return 'insufficient memory'
	}
	mut msgbuf := &u16(0)
	res := C.FormatMessageW(format_message_allocate_buffer | format_message_from_system | format_message_ignore_inserts,
		0, err_msg_id, 0, voidptr(&msgbuf), 0, 0)
	err_msg := if res == 0 {
		'Win-API error ${err_msg_id}'
	} else {
		unsafe { string_from_wide(msgbuf) }
	}
	return err_msg
}

// panic with an error message generated from WinAPI's `LastError`
[noreturn]
pub fn panic_lasterr(base string) {
	panic(base + winapi_lasterr_str())
}
