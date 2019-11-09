// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

$if msvc {
	#include <dbghelp.h>
	#flag windows -l dbghelp
}

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

fn init() {
	$if windows {	
		if is_atty(0) > 0 {
			C._setmode(C._fileno(C.stdin), C._O_U16TEXT)
		} else {
			C._setmode(C._fileno(C.stdin), C._O_U8TEXT)		
		}
		C._setmode(C._fileno(C.stdout), C._O_U8TEXT)
		C.SetConsoleMode(C.GetStdHandle(C.STD_OUTPUT_HANDLE), C.ENABLE_PROCESSED_OUTPUT | 0x0004) // ENABLE_VIRTUAL_TERMINAL_PROCESSING
		C.setbuf(C.stdout,0)
	}
}

pub fn exit(code int) {
	C.exit(code)
}

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

fn on_panic(f fn (int) int) {
	// TODO
}

pub fn print_backtrace_skipping_top_frames(skipframes int) {
	$if mac {
		buffer := [100]byteptr
		nr_ptrs := C.backtrace(*voidptr(buffer), 100)
		C.backtrace_symbols_fd(*voidptr(&buffer[skipframes]), nr_ptrs-skipframes, 1)
		return
	}
	$if linux {
		$if !android {
			$if glibc {
			// backtrace is not available on Android.
			//if C.backtrace_symbols_fd != 0 {
				buffer := [100]byteptr
				nr_ptrs := C.backtrace(*voidptr(buffer), 100)
				nr_actual_frames := nr_ptrs-skipframes
				mut sframes := []string
				csymbols := *byteptr(C.backtrace_symbols(*voidptr(&buffer[skipframes]), nr_actual_frames))
				for i in 0..nr_actual_frames {  sframes << tos2(csymbols[i]) }
				for sframe in sframes {
					executable := sframe.all_before('(')
					addr := sframe.all_after('[').all_before(']')
					cmd := 'addr2line -e $executable $addr'

					// taken from os, to avoid depending on the os module inside builtin.v
					f := C.popen(cmd.str, 'r')
					if isnil(f) {
						println(sframe) continue
					}
					buf := [1000]byte
					mut output := ''
					for C.fgets(voidptr(buf), 1000, f) != 0 {
						output += tos(buf, vstrlen(buf))
					}
					output = output.trim_space()+':'
					if 0 != int(C.pclose(f)) {
						println(sframe) continue
					}
					println( '${output:-45s} | $sframe')
				}
				//C.backtrace_symbols_fd(*voidptr(&buffer[skipframes]), nr_actual_frames, 1)
				return
			}$else{
				C.printf('backtrace_symbols_fd is missing, so printing backtraces is not available.\n')
				C.printf('Some libc implementations like musl simply do not provide it.\n')
			}
		}
	}
	$if windows {
		println('print_backtrace window\n')
		$if msvc {
			print_backtrace_skipping_top_frames_msvc(skipframes)
		}
		$else {
			print_backtrace_skipping_top_frames_mingw(skipframes)
		}
		return
	}
	println('print_backtrace_skipping_top_frames is not implemented on this platform for now...\n')
}

fn print_backtrace_skipping_top_frames_msvc(skipframes int) {
$if msvc {
		mut offset := u64(0)
		backtraces := [100]voidptr
		sic := SymbolInfoContainer{}
		mut si := &sic.syminfo
		si.f_size_of_struct = sizeof(SymbolInfo) // Note: C.SYMBOL_INFO is 88
		si.f_max_name_len = sizeof(SymbolInfoContainer) - sizeof(SymbolInfo) - 1
		fname := *char( &si.f_name )
		mut sline64 := Line64{}
		sline64.f_size_of_struct = sizeof(Line64)

		handle := C.GetCurrentProcess()
		defer { C.SymCleanup(handle) }
		
		options := C.SymSetOptions(SYMOPT_DEBUG | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME)
		syminitok := C.SymInitialize( handle, 0, 1)
		if syminitok != 1 {
			println('Failed getting process: Aborting backtrace.\n')
			return
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
	}
}

fn print_backtrace_skipping_top_frames_mingw(skipframes int) {
	println('TODO: print_backtrace_skipping_top_frames_mingw($skipframes)')
}

pub fn print_backtrace(){
	// at the time of backtrace_symbols_fd call, the C stack would look something like this:
	// 1 frame for print_backtrace_skipping_top_frames
	// 1 frame for print_backtrace itself
	// ... print the rest of the backtrace frames ...
	// => top 2 frames should be skipped, since they will not be informative to the developer
	print_backtrace_skipping_top_frames(2)
}

// replaces panic when -debug arg is passed
fn panic_debug(line_no int, file,  mod, fn_name, s string) {
	println('================ V panic ================')
	println('   module: $mod')
	println(' function: ${fn_name}()')
	println('     file: $file')
	println('     line: ' + line_no.str())
	println('  message: $s')
	println('=========================================')
	print_backtrace_skipping_top_frames(1)
	C.exit(1)
}

pub fn panic(s string) {
	println('V panic: $s')
	print_backtrace()
	C.exit(1)
}

pub fn println(s string) {
	// Should never happen
	if isnil(s.str) {
		panic('println(NIL)')
	}
	$if windows {
		C._putws(s.to_wide())
	} $else {
		C.printf('%.*s\n', s.len, s.str)
	}
}

pub fn eprintln(s string) {
	if isnil(s.str) {
		panic('eprintln(NIL)')
	}
	$if mac {
		C.fprintf(stderr, '%.*s\n', s.len, s.str)
		C.fflush(stderr)
		return
	}
	$if linux {
		C.fprintf(stderr, '%.*s\n', s.len, s.str)
		C.fflush(stderr)
		return
	}
	// TODO issues with stderr and cross compiling for Linux
	println(s)
}

pub fn print(s string) {
	$if windows {
		C.wprintf(s.to_wide())
	} $else {
		C.printf('%.*s', s.len, s.str)
	}
}

__global total_m i64 = 0
//__global nr_mallocs int = 0
[unsafe_fn]
pub fn malloc(n int) byteptr {
	if n < 0 {
		panic('malloc(<0)')
	}
	//nr_mallocs++
/*
TODO
#ifdef VPLAY
	if n > 10000 {
		panic('allocating more than 10 KB is not allowed in the playground')
	}
#endif
#ifdef DEBUG_ALLOC
	total_m += n
	println('\n\n\nmalloc($n) total=$total_m')
	print_backtrace()
#endif
*/
	ptr := C.malloc(n)
	if isnil(ptr) {
		panic('malloc($n) failed')
	}
	return ptr
}

pub fn calloc(n int) byteptr {
	if n < 0 {
		panic('calloc(<0)')
	}
	return C.calloc(n, 1)
}

[unsafe_fn]
pub fn free(ptr voidptr) {
	C.free(ptr)
}

pub fn memdup(src voidptr, sz int) voidptr {
	mem := malloc(sz)
	return C.memcpy(mem, src, sz)
}

fn v_ptr_free(ptr voidptr) {
	C.free(ptr)
}

pub fn is_atty(fd int) int {
	$if windows {
		mut mode := 0
		C.GetConsoleMode(C._get_osfhandle(fd), &mode)
		return mode
	} $else {
		return C.isatty(fd)
	}
}

