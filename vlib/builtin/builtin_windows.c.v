// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// dbghelp.h is already included in cheaders.v
#flag windows -l dbghelp
// SymbolInfo is used by print_backtrace_skipping_top_frames_msvc
pub struct SymbolInfo {
pub mut:
	f_size_of_struct u32 // must be 88 to be recognised by SymFromAddr
	f_type_index     u32 // Type Index of symbol
	f_reserved       [2]u64
	f_index          u32
	f_size           u32
	f_mod_base       u64 // Base Address of module comtaining this symbol
	f_flags          u32
	f_value          u64  // Value of symbol, ValuePresent should be 1
	f_address        u64  // Address of symbol including base address of module
	f_register       u32  // register holding value or pointer to value
	f_scope          u32  // scope of the symbol
	f_tag            u32  // pdb classification
	f_name_len       u32  // Actual length of name
	f_max_name_len   u32  // must be manually set
	f_name           byte // must be calloc(f_max_name_len)
}

pub struct SymbolInfoContainer {
pub mut:
	syminfo     SymbolInfo
	f_name_rest [254]char
}

pub struct Line64 {
pub mut:
	f_size_of_struct u32
	f_key            voidptr
	f_line_number    u32
	f_file_name      byteptr
	f_address        u64
}

// returns the current options mask
fn C.SymSetOptions(symoptions u32) u32

// returns handle
fn C.GetCurrentProcess() voidptr

fn C.SymInitialize(h_process voidptr, p_user_search_path byteptr, b_invade_process int) int

fn C.CaptureStackBackTrace(frames_to_skip u32, frames_to_capture u32, p_backtrace voidptr, p_backtrace_hash voidptr) u16

fn C.SymFromAddr(h_process voidptr, address u64, p_displacement voidptr, p_symbol voidptr) int

fn C.SymGetLineFromAddr64(h_process voidptr, address u64, p_displacement voidptr, p_line &Line64) int

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/dbghelp/nf-dbghelp-symsetoptions
const (
	symopt_undname               = 0x00000002
	symopt_deferred_loads        = 0x00000004
	symopt_no_cpp                = 0x00000008
	symopt_load_lines            = 0x00000010
	symopt_include_32bit_modules = 0x00002000
	symopt_allow_zero_address    = 0x01000000
	symopt_debug                 = 0x80000000
)

// g_original_codepage - used to restore the original windows console code page when exiting
__global ( g_original_codepage = u32(0))

// utf8 to stdout needs C.SetConsoleOutputCP(C.CP_UTF8)
fn C.GetConsoleOutputCP() u32

fn C.SetConsoleOutputCP(wCodePageID u32) bool

fn restore_codepage() {
	C.SetConsoleOutputCP(g_original_codepage)
}

fn builtin_init() {
	g_original_codepage = C.GetConsoleOutputCP()
	C.SetConsoleOutputCP(C.CP_UTF8)
	C.atexit(restore_codepage)
	if is_atty(1) > 0 {
		C.SetConsoleMode(C.GetStdHandle(C.STD_OUTPUT_HANDLE), C.ENABLE_PROCESSED_OUTPUT | C.ENABLE_WRAP_AT_EOL_OUTPUT | 0x0004) // enable_virtual_terminal_processing
		C.SetConsoleMode(C.GetStdHandle(C.STD_ERROR_HANDLE), C.ENABLE_PROCESSED_OUTPUT | C.ENABLE_WRAP_AT_EOL_OUTPUT | 0x0004) // enable_virtual_terminal_processing
		unsafe {
			C.setbuf(C.stdout, 0)
			C.setbuf(C.stderr, 0)
		}
	}
	add_unhandled_exception_handler()
}

fn print_backtrace_skipping_top_frames(skipframes int) bool {
	$if msvc {
		return print_backtrace_skipping_top_frames_msvc(skipframes)
	}
	$if tinyc {
		return print_backtrace_skipping_top_frames_tcc(skipframes)
	}
	$if mingw {
		return print_backtrace_skipping_top_frames_mingw(skipframes)
	}
	eprintln('print_backtrace_skipping_top_frames is not implemented')
	return false
}

fn print_backtrace_skipping_top_frames_msvc(skipframes int) bool {
	$if msvc {
		mut offset := u64(0)
		backtraces := [100]voidptr{}
		sic := SymbolInfoContainer{}
		mut si := &sic.syminfo
		si.f_size_of_struct = sizeof(SymbolInfo) // Note: C.SYMBOL_INFO is 88
		si.f_max_name_len = sizeof(SymbolInfoContainer) - sizeof(SymbolInfo) - 1
		fname := charptr(&si.f_name)
		mut sline64 := Line64{}
		sline64.f_size_of_struct = sizeof(Line64)

		handle := C.GetCurrentProcess()
		defer {
			C.SymCleanup(handle)
		}

		C.SymSetOptions(symopt_debug | symopt_load_lines | symopt_undname)

		syminitok := C.SymInitialize(handle, 0, 1)
		if syminitok != 1 {
			eprintln('Failed getting process: Aborting backtrace.\n')
			return false
		}

		frames := int(C.CaptureStackBackTrace(skipframes + 1, 100, backtraces, 0))
		if frames < 2 {
			eprintln('C.CaptureStackBackTrace returned less than 2 frames')
			return false
		}
		for i in 0 .. frames {
			frame_addr := backtraces[i]
			if C.SymFromAddr(handle, frame_addr, &offset, si) == 1 {
				nframe := frames - i - 1
				mut lineinfo := ''
				if C.SymGetLineFromAddr64(handle, frame_addr, &offset, &sline64) == 1 {
					file_name := unsafe { tos3(sline64.f_file_name) }
					lnumber := sline64.f_line_number
					lineinfo = '$file_name:$lnumber'
				} else {
					addr:
					lineinfo = '?? : address = 0x${(&frame_addr):x}'
				}
				sfunc := unsafe { tos3(fname) }
				eprintln('${nframe:-2d}: ${sfunc:-25s}  $lineinfo')
			} else {
				// https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes
				cerr := int(C.GetLastError())
				if cerr == 87 {
					eprintln('SymFromAddr failure: $cerr = The parameter is incorrect)')
				} else if cerr == 487 {
					// probably caused because the .pdb isn't in the executable folder
					eprintln('SymFromAddr failure: $cerr = Attempt to access invalid address (Verify that you have the .pdb file in the right folder.)')
				} else {
					eprintln('SymFromAddr failure: $cerr (see https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes)')
				}
			}
		}
		return true
	} $else {
		eprintln('print_backtrace_skipping_top_frames_msvc must be called only when the compiler is msvc')
		return false
	}
}

fn print_backtrace_skipping_top_frames_mingw(skipframes int) bool {
	eprintln('print_backtrace_skipping_top_frames_mingw is not implemented')
	return false
}

fn C.tcc_backtrace(fmt charptr, other ...charptr) int

fn print_backtrace_skipping_top_frames_tcc(skipframes int) bool {
	$if tinyc {
		$if no_backtrace ? {
			eprintln('backtraces are disabled')
			return false
		} $else {
			C.tcc_backtrace('Backtrace')
			return true
		}
	} $else {
		eprintln('print_backtrace_skipping_top_frames_tcc must be called only when the compiler is tcc')
		return false
	}
	// Not reachable, but it looks like it's not detectable by V
	return false
}

// TODO copypaste from os
// we want to be able to use this here without having to `import os`
struct ExceptionRecord {
pub:
	// status_ constants
	code        u32
	flags       u32
	record      &ExceptionRecord
	address     voidptr
	param_count u32
	// params []voidptr
}

struct ContextRecord {
	// TODO
}

struct ExceptionPointers {
pub:
	exception_record &ExceptionRecord
	context_record   &ContextRecord
}

type VectoredExceptionHandler = fn (&ExceptionPointers) int

fn C.AddVectoredExceptionHandler(int, C.PVECTORED_EXCEPTION_HANDLER)

fn add_vectored_exception_handler(handler VectoredExceptionHandler) {
	C.AddVectoredExceptionHandler(1, C.PVECTORED_EXCEPTION_HANDLER(handler))
}

[windows_stdcall]
fn unhandled_exception_handler(e &ExceptionPointers) int {
	match e.exception_record.code {
		// These are 'used' by the backtrace printer
		// so we dont want to catch them...
		0x4001000A, 0x40010006 {
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
			*ptr = voidptr(0)
		}
	} $else {
		if C.IsDebuggerPresent() {
			C.__debugbreak()
		}
	}
}
