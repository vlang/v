// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module builtin

#include <io.h>
#include <fcntl.h>

// See https://learn.microsoft.com/en-us/windows/win32/winprog/windows-data-types
// See https://www.codeproject.com/KB/string/cppstringguide1.aspx
pub type C.BOOL = int

pub type C.HINSTANCE = voidptr

pub type C.HICON = voidptr

pub type C.HCURSOR = voidptr

pub type C.HBRUSH = voidptr

pub type C.HWND = voidptr

pub type C.HGLOBAL = voidptr

pub type C.HANDLE = voidptr

pub type C.LRESULT = voidptr

pub type C.CHAR = char

pub type C.TCHAR = u16 // It is u8 if UNICODE is not defined, but for V programs it always is

pub type C.WCHAR = u16

pub type C.LPSTR = &char

pub type C.LPWSTR = &C.WCHAR

pub type C.LPTSTR = &C.TCHAR

pub type C.LPCTSTR = &C.TCHAR

fn C.WriteConsoleW(voidptr, &u16, u32, &u32, voidptr) bool

fn C._fileno(&C.FILE) int

fn C._setmode(int, int) int

// set_stream_binary_mode disables CRT newline translation for redirected stdio streams.
fn set_stream_binary_mode(stream &C.FILE) {
	fd := C._fileno(stream)
	if fd >= 0 {
		C._setmode(fd, C._O_BINARY)
	}
}

fn is_terminal(fd int) int {
	mut mode := u32(0)
	osfh := voidptr(C._get_osfhandle(fd))
	C.GetConsoleMode(osfh, voidptr(&mode))
	return int(mode)
}

const std_output_handle = -11
const std_error_handle = -12
const enable_processed_output = 1
const enable_wrap_at_eol_output = 2
const evable_virtual_terminal_processing = 4

// Write UTF-8 directly as UTF-16 for console hosts instead of changing the console code page.
@[manualfree]
fn write_buf_to_console(fd int, buf &u8, buf_len int) bool {
	if buf_len <= 0 || is_terminal(fd) <= 0 {
		return false
	}
	console_handle := if fd == 2 {
		C.GetStdHandle(std_error_handle)
	} else {
		C.GetStdHandle(std_output_handle)
	}
	if isnil(console_handle) {
		return false
	}
	unsafe {
		wide_len := C.MultiByteToWideChar(cp_utf8, 0, &char(buf), buf_len, 0, 0)
		if wide_len <= 0 {
			return false
		}
		mut wide_buf := &u16(malloc_noscan((wide_len + 1) * int(sizeof(u16))))
		if isnil(wide_buf) {
			return false
		}
		defer {
			free(wide_buf)
		}
		converted := C.MultiByteToWideChar(cp_utf8, 0, &char(buf), buf_len, wide_buf, wide_len)
		if converted <= 0 {
			return false
		}
		wide_buf[converted] = 0
		mut remaining_chars := converted
		mut wide_ptr := wide_buf
		for remaining_chars > 0 {
			mut chars_written := u32(0)
			if !C.WriteConsoleW(console_handle, wide_ptr, u32(remaining_chars), &chars_written, nil)
				|| chars_written == 0 {
				return false
			}
			wide_ptr += int(chars_written)
			remaining_chars -= int(chars_written)
		}
		return true
	}
}

@[markused]
fn builtin_init() {
	$if gcboehm ? {
		$if !gc_warn_on_stderr ? {
			gc_set_warn_proc(internal_gc_warn_proc_none)
		}
	}
	set_stream_binary_mode(C.stdout)
	set_stream_binary_mode(C.stderr)
	if is_terminal(1) > 0 {
		C.SetConsoleMode(C.GetStdHandle(std_output_handle),
			enable_processed_output | enable_wrap_at_eol_output | evable_virtual_terminal_processing)
		C.SetConsoleMode(C.GetStdHandle(std_error_handle),
			enable_processed_output | enable_wrap_at_eol_output | evable_virtual_terminal_processing)
		unsafe {
			set_stream_unbuffered(C.stdout)
			set_stream_unbuffered(C.stderr)
		}
	}
	$if !no_backtrace ? {
		add_unhandled_exception_handler()
	}
	// On windows, the default buffering is block based (~4096bytes), which interferes badly with non cmd shells
	// It is much better to have it off by default instead.
	unbuffer_stdout()
}

// TODO: copypaste from os
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

@[callconv: stdcall]
type TopLevelExceptionFilter = fn (&ExceptionPointers) int

fn C.SetUnhandledExceptionFilter(TopLevelExceptionFilter) voidptr

@[callconv: stdcall]
fn unhandled_exception_handler(e &ExceptionPointers) int {
	match e.exception_record.code {
		// These are 'used' by the backtrace printer
		// so we dont want to catch them...
		0x4001000A, 0x40010006, 0x406D1388, 0xE06D7363 {
			return 0
		}
		else {
			println('Unhandled Exception 0x' + ptr_str(e.exception_record.code))
			print_backtrace_skipping_top_frames(5)
		}
	}

	return 0
}

fn add_unhandled_exception_handler() {
	// A vectored handler also sees first-chance exceptions that Windows APIs may
	// handle internally, which can lead to false-positive "Unhandled Exception"
	// reports. Register a top-level filter instead.
	C.SetUnhandledExceptionFilter(unhandled_exception_handler)
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

const format_message_allocate_buffer = 0x00000100
const format_message_argument_array = 0x00002000
const format_message_from_hmodule = 0x00000800
const format_message_from_string = 0x00000400
const format_message_from_system = 0x00001000
const format_message_ignore_inserts = 0x00000200

// return an error message generated from WinAPI's `LastError`
pub fn winapi_lasterr_str() string {
	err_msg_id := C.GetLastError()
	if err_msg_id == 8 {
		// handle this case special since `FormatMessageW()` might not work anymore
		return 'insufficient memory'
	}
	mut msgbuf := &u16(unsafe { nil })
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
@[noreturn]
pub fn panic_lasterr(base string) {
	panic(base + winapi_lasterr_str())
}
