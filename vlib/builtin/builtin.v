// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

__global g_m2_buf byteptr
__global g_m2_ptr byteptr

pub fn exit(code int) {
	C.exit(code)
}

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

/*
fn on_panic(f fn(int)int) {
	// TODO
}
*/

pub fn print_backtrace() {
	// at the time of backtrace_symbols_fd call, the C stack would look something like this:
	// 1 frame for print_backtrace_skipping_top_frames
	// 1 frame for print_backtrace itself
	// ... print the rest of the backtrace frames ...
	// => top 2 frames should be skipped, since they will not be informative to the developer
	print_backtrace_skipping_top_frames(2)
}

// replaces panic when -debug arg is passed
fn panic_debug(line_no int, file, mod, fn_name, s string) {
	// NB: the order here is important for a stabler test output
	// module is less likely to change than function, etc...
	// During edits, the line number will change most frequently,
	// so it is last
	eprintln('================ V panic ================')
	eprintln('   module: $mod')
	eprintln(' function: ${fn_name}()')
	eprintln('  message: $s')
	eprintln('     file: $file')
	eprintln('     line: ' + line_no.str())
	eprintln('=========================================')
	print_backtrace_skipping_top_frames(1)
	break_if_debugger_attached()
	C.exit(1)
}

pub fn panic(s string) {
	eprintln('V panic: $s')
	print_backtrace()
	break_if_debugger_attached()
	C.exit(1)
}

pub fn eprintln(s string) {
	// eprintln is used in panics, so it should not fail at all
	if s.str == 0 {
		eprintln('eprintln(NIL)')
	}
	$if !windows {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		C.fprintf(C.stderr, '%.*s\n', s.len, s.str)
		C.fflush(C.stderr)
		return
	}
	// TODO issues with stderr and cross compiling for Linux
	println(s)
}

pub fn eprint(s string) {
	if s.str == 0 {
		eprintln('eprint(NIL)')
	}
	$if !windows {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		C.fprintf(C.stderr, '%.*s', s.len, s.str)
		C.fflush(C.stderr)
		return
	}
	print(s)
}

pub fn print(s string) {
	$if windows {
		output_handle := C.GetStdHandle(C.STD_OUTPUT_HANDLE)
		mut bytes_written := 0
		if is_atty(1) > 0 {
			wide_str := s.to_wide()
			wide_len := C.wcslen(wide_str)
			C.WriteConsole(output_handle, wide_str, wide_len, &bytes_written, 0)
			unsafe {
				free(wide_str)
			}
		} else {
			C.WriteFile(output_handle, s.str, s.len, &bytes_written, 0)
		}
	} $else {
		C.printf('%.*s', s.len, s.str)
	}
}

const (
	new_line_character = '\n'
)
pub fn println(s string) {
	$if windows {
		print(s)
		print(new_line_character)
	} $else {
		//  TODO: a syscall sys_write on linux works, except for the v repl.
		//  Probably it is a stdio buffering issue. Needs more testing...
		//	$if linux {
		//		$if !android {
		//			snl := s + '\n'
		//			C.syscall(/* sys_write */ 1, /* stdout_value */ 1, snl.str, s.len+1)
		//			return
		//		}
		//	}
		C.printf('%.*s\n', s.len, s.str)
	}
}

__global total_m i64=0
__global nr_mallocs int=0

fn looo(){} // TODO remove, [ pratt

[unsafe_fn]
pub fn malloc(n int) byteptr {
	if n <= 0 {
		panic('malloc(<=0)')
	}
	$if prealloc {
		res := g_m2_ptr
		g_m2_ptr += n
		nr_mallocs++
		return res
	} $else {
		ptr := C.malloc(n)
		if ptr == 0 {
			panic('malloc($n) failed')
		}
		return ptr
	}
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
}

[unsafe_fn]
pub fn v_realloc(b byteptr, n int) byteptr {
	ptr := C.realloc(b, n)
	if ptr == 0 {
		panic('realloc($n) failed')
	}

	return ptr
}

pub fn v_calloc(n int) byteptr {
	return C.calloc(n, 1)
}

pub fn vcalloc(n int) byteptr {
	if n < 0 {
		panic('calloc(<=0)')
	} else if n == 0 {
		return byteptr(0)
	} else {
		return C.calloc(n, 1)
	}
}

[unsafe_fn]
pub fn free(ptr voidptr) {
	C.free(ptr)
}

pub fn memdup(src voidptr, sz int) voidptr {
	if sz == 0 {
		return vcalloc(1)
	}
	mem := malloc(sz)
	return C.memcpy(mem, src, sz)
}

fn v_ptr_free(ptr voidptr) {
	C.free(ptr)
}

pub fn is_atty(fd int) int {
	$if windows {
		mut mode := u32(0)
		osfh := voidptr(C._get_osfhandle(fd))
		C.GetConsoleMode(osfh, voidptr(&mode))
		return int(mode)
	} $else {
		return C.isatty(fd)
	}
}

fn __as_cast(obj voidptr, obj_type, expected_type int) voidptr {
	if obj_type != expected_type {
		panic('as cast: cannot cast $obj_type to $expected_type')
	}
	return obj
}
