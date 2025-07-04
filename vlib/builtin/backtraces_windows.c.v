module builtin

// dbghelp.h is already included in cheaders.v
#flag windows -l dbghelp

// SymbolInfo is used by print_backtrace_skipping_top_frames_msvc.
pub struct SymbolInfo {
pub mut:
	f_size_of_struct u32 // must be 88 to be recognised by SymFromAddr
	f_type_index     u32 // Type Index of symbol
	f_reserved       [2]u64
	f_index          u32
	f_size           u32
	f_mod_base       u64 // Base Address of module containing this symbol
	f_flags          u32
	f_value          u64 // Value of symbol, ValuePresent should be 1
	f_address        u64 // Address of symbol including base address of module
	f_register       u32 // register holding value or pointer to value
	f_scope          u32 // scope of the symbol
	f_tag            u32 // pdb classification
	f_name_len       u32 // Actual length of name
	f_max_name_len   u32 // must be manually set
	f_name           u8  // must be calloc(f_max_name_len)
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
	f_file_name      &u8 = unsafe { nil }
	f_address        u64
}

// returns the current options mask
fn C.SymSetOptions(symoptions u32) u32

// returns handle
fn C.GetCurrentProcess() voidptr

fn C.SymInitialize(h_process voidptr, p_user_search_path &u8, b_invade_process int) int

fn C.CaptureStackBackTrace(frames_to_skip u32, frames_to_capture u32, p_backtrace voidptr, p_backtrace_hash voidptr) u16

fn C.SymFromAddr(h_process voidptr, address u64, p_displacement voidptr, p_symbol voidptr) int

fn C.SymGetLineFromAddr64(h_process voidptr, address u64, p_displacement voidptr, p_line &Line64) int

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/dbghelp/nf-dbghelp-symsetoptions
const symopt_undname = 0x00000002
const symopt_deferred_loads = 0x00000004
const symopt_no_cpp = 0x00000008
const symopt_load_lines = 0x00000010
const symopt_include_32bit_modules = 0x00002000
const symopt_allow_zero_address = 0x01000000
const symopt_debug = u32(0x80000000)

// print_backtrace_skipping_top_frames prints the backtrace skipping N top frames.
pub fn print_backtrace_skipping_top_frames(skipframes int) bool {
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

@[direct_array_access]
fn print_backtrace_skipping_top_frames_msvc(skipframes int) bool {
	$if msvc {
		mut offset := u64(0)
		backtraces := [100]voidptr{}
		sic := SymbolInfoContainer{}
		mut si := &sic.syminfo
		si.f_size_of_struct = sizeof(SymbolInfo) // Note: C.SYMBOL_INFO is 88
		si.f_max_name_len = sizeof(SymbolInfoContainer) - sizeof(SymbolInfo) - 1
		fname := &char(&si.f_name)
		mut sline64 := Line64{
			f_file_name: &u8(unsafe { nil })
		}
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

		frames := int(C.CaptureStackBackTrace(skipframes + 1, 100, &backtraces[0], 0))
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
					lineinfo = file_name + i64(lnumber).str()
				} else {
					// addr:
					lineinfo = '?? : address = 0x' + ptr_str(frame_addr)
				}
				sfunc := unsafe { tos3(fname) }
				snframe := i64(nframe).str()
				eprint_space_padding(snframe, 2)
				eprint(': ')
				eprint(sfunc)
				eprint_space_padding(sfunc, 25)
				eprint('  ')
				eprint(lineinfo)
			} else {
				// https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes
				cerr := int(C.GetLastError())
				eprint('SymFromAddr failure: ')
				eprint(i64(cerr).str())
				if cerr == 87 {
					eprintln(' = The parameter is incorrect)')
				} else if cerr == 487 {
					// probably caused because the .pdb isn't in the executable folder
					eprintln(' = Attempt to access invalid address (Verify that you have the .pdb file in the right folder.)')
				} else {
					eprintln(' (see https://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes)')
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

fn C.tcc_backtrace(fmt &char) int

fn print_backtrace_skipping_top_frames_tcc(skipframes int) bool {
	$if tinyc && !native {
		$if no_backtrace ? {
			eprintln('backtraces are disabled')
			return false
		} $else {
			C.tcc_backtrace(c'Backtrace')
			return true
		}
	} $else {
		eprintln('print_backtrace_skipping_top_frames_tcc must be called only when the compiler is tcc')
		return false
	}
	// Not reachable, but it looks like it's not detectable by V
	return false
}
