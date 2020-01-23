// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

#include <dbghelp.h>
#flag windows -l dbghelp

pub struct SymbolInfo {
pub mut:
	f_size_of_struct u32 // must be 88 to be recognised by SymFromAddr
	f_type_index u32 // Type Index of symbol
	f_reserved [2]u64
	f_index u32
	f_size u32
	f_mod_base u64 // Base Address of module comtaining this symbol
	f_flags u32
	f_value u64 // Value of symbol, ValuePresent should be 1
	f_address u64 // Address of symbol including base address of module
	f_register u32 // register holding value or pointer to value
	f_scope u32 // scope of the symbol
	f_tag u32 // pdb classification
	f_name_len u32 // Actual length of name
	f_max_name_len u32 // must be manually set
	f_name byte // must be calloc(f_max_name_len)
}

pub struct SymbolInfoContainer {
pub mut:
	syminfo SymbolInfo
	f_name_rest [254]char
}

pub struct Line64 {
	f_size_of_struct u32
	f_key voidptr
	f_line_number u32
	f_file_name byteptr
	f_address u64
}

fn C.SymSetOptions(symoptions u32) u32 // returns the current options mask
fn C.GetCurrentProcess() voidptr // returns handle
fn C.SymInitialize(h_process voidptr, p_user_search_path byteptr, b_invade_process int) int
fn C.CaptureStackBackTrace(frames_to_skip u32, frames_to_capture u32, p_backtrace voidptr, p_backtrace_hash voidptr) u16
fn C.SymFromAddr(h_process voidptr, address u64, p_displacement voidptr, p_symbol voidptr) int
fn C.SymGetLineFromAddr64(h_process voidptr, address u64, p_displacement voidptr, p_line &Line64) int

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/dbghelp/nf-dbghelp-symsetoptions
const (
	SYMOPT_UNDNAME = 0x00000002
	SYMOPT_DEFERRED_LOADS = 0x00000004
	SYMOPT_NO_CPP = 0x00000008
	SYMOPT_LOAD_LINES = 0x00000010
	SYMOPT_INCLUDE_32BIT_MODULES = 0x00002000
	SYMOPT_ALLOW_ZERO_ADDRESS = 0x01000000
	SYMOPT_DEBUG = 0x80000000
)

fn print_backtrace_skipping_top_frames_msvc(skipframes int) bool {

$if msvc {
	mut offset := u64(0)
	backtraces := [100]voidptr
	sic := SymbolInfoContainer{}
	mut si := &sic.syminfo
	si.f_size_of_struct = sizeof(SymbolInfo) // Note: C.SYMBOL_INFO is 88
	si.f_max_name_len = sizeof(SymbolInfoContainer) - sizeof(SymbolInfo) - 1
	fname := charptr( &si.f_name )
	mut sline64 := Line64{}
	sline64.f_size_of_struct = sizeof(Line64)

	handle := C.GetCurrentProcess()
	defer { C.SymCleanup(handle) }

	options := C.SymSetOptions(SYMOPT_DEBUG | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME)
	syminitok := C.SymInitialize( handle, 0, 1)
	if syminitok != 1 {
		println('Failed getting process: Aborting backtrace.\n')
		return true
	}

	frames := int( C.CaptureStackBackTrace(skipframes + 1, 100, backtraces, 0) )
	for i:=0; i < frames; i++ {
		// fugly pointer arithmetics follows ...
		s := *voidptr( u64(backtraces) + u64(i*sizeof(voidptr)) )
		symfa_ok := C.SymFromAddr( handle, *s, &offset, si )
		if symfa_ok == 1 {
			nframe := frames - i - 1
			mut lineinfo := ''
			symglfa_ok := C.SymGetLineFromAddr64(handle, *s, &offset, &sline64)
			if symglfa_ok == 1 {
				lineinfo = ' ${sline64.f_file_name}:${sline64.f_line_number}'
			}
			else {
				//cerr := int(C.GetLastError()) println('SymGetLineFromAddr64 failure: $cerr ')
				lineinfo = ' ?? : address= $s'
			}
			sfunc := tos3(fname)
			println('${nframe:-2d}: ${sfunc:-25s}  $lineinfo')
		}
		else {
			// https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes
			cerr := int(C.GetLastError())
			if (cerr == 87) {
				println('SymFromAddr failure: $cerr = The parameter is incorrect)')
			}
			else if (cerr == 487) {
				// probably caused because the .pdb isn't in the executable folder
				println('SymFromAddr failure: $cerr = Attempt to access invalid address (Verify that you have the .pdb file in the right folder.)')
			}
			else {
				println('SymFromAddr failure: $cerr (see https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes)')
			}
		}
	}
	return true
}
$else {
	println('TODO: Not implemented on Windows without msvc.')
	return false
}
}


fn print_backtrace_skipping_top_frames_mingw(skipframes int) bool {
	println('TODO: print_backtrace_skipping_top_frames_mingw($skipframes)')
	return false
}

fn print_backtrace_skipping_top_frames_nix(skipframes int) bool {
	println('not implemented, see builtin_nix.v')
	return false
}

pub fn println(s string) {
	print('$s\n')
}

