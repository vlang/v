// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module builtin

// g_original_codepage - used to restore the original windows console code page when exiting
__global g_original_codepage = u32(0)

// utf8 to stdout needs C.SetConsoleOutputCP(C.CP_UTF8)
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

fn builtin_init() {
	g_original_codepage = C.GetConsoleOutputCP()
	C.SetConsoleOutputCP(C.CP_UTF8)
	C.atexit(restore_codepage)
	if is_terminal(1) > 0 {
		C.SetConsoleMode(C.GetStdHandle(C.STD_OUTPUT_HANDLE), C.ENABLE_PROCESSED_OUTPUT | C.ENABLE_WRAP_AT_EOL_OUTPUT | 0x0004) // enable_virtual_terminal_processing
		C.SetConsoleMode(C.GetStdHandle(C.STD_ERROR_HANDLE), C.ENABLE_PROCESSED_OUTPUT | C.ENABLE_WRAP_AT_EOL_OUTPUT | 0x0004) // enable_virtual_terminal_processing
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

// return an error message generated from WinAPI's `LastError`
pub fn winapi_lasterr_str() string {
	err_msg_id := C.GetLastError()
	if err_msg_id == 8 {
		// handle this case special since `FormatMessage()` might not work anymore
		return 'insufficient memory'
	}
	mut msgbuf := &u16(0)
	res := C.FormatMessage(C.FORMAT_MESSAGE_ALLOCATE_BUFFER | C.FORMAT_MESSAGE_FROM_SYSTEM | C.FORMAT_MESSAGE_IGNORE_INSERTS,
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
