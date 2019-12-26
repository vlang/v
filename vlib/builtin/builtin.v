// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

__global g_m2_buf byteptr
__global g_m2_ptr byteptr

fn init() {
	$if windows {
		if is_atty(0) > 0 {
			C._setmode(C._fileno(C.stdin), C._O_U16TEXT)
		}
		else {
			C._setmode(C._fileno(C.stdin), C._O_U8TEXT)
		}
		C._setmode(C._fileno(C.stdout), C._O_U8TEXT)
		C.SetConsoleMode(C.GetStdHandle(C.STD_OUTPUT_HANDLE), C.ENABLE_PROCESSED_OUTPUT | 0x0004) // ENABLE_VIRTUAL_TERMINAL_PROCESSING
		C.setbuf(C.stdout, 0)
	}
}

pub fn exit(code int) {
	C.exit(code)
}

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

fn on_panic(f fn(int)int) {
	// TODO
}

pub fn print_backtrace_skipping_top_frames(skipframes int) {
	$if windows {
		$if msvc {
			if print_backtrace_skipping_top_frames_msvc(skipframes) {
				return
			}
		}
		$if mingw {
			if print_backtrace_skipping_top_frames_mingw(skipframes) {
				return
			}
		}
	} $else {
		if print_backtrace_skipping_top_frames_nix(skipframes) {
			return
		}
	}
	println('print_backtrace_skipping_top_frames is not implemented on this platform for now...\n')
}

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

pub fn eprintln(s string) {
	if isnil(s.str) {
		panic('eprintln(NIL)')
	}
	$if !windows {
		C.fflush(stdout)
		C.fflush(stderr)
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

__global total_m i64=0
__global nr_mallocs int=0

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

pub fn calloc(n int) byteptr {
	if n <= 0 {
		panic('calloc(<=0)')
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
		mut mode := u32(0)
		osfh := voidptr(C._get_osfhandle(fd))
		C.GetConsoleMode(osfh, voidptr(&mode))
		return int(mode)
	} $else {
		return C.isatty(fd)
	}
}

