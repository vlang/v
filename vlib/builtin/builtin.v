// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn init() {
	$if windows {	
		if is_atty(0) {
			C._setmode(C._fileno(C.stdin), C._O_U16TEXT)
		} else {
			C._setmode(C._fileno(C.stdin), C._O_U8TEXT)		
		}
		C._setmode(C._fileno(C.stdout), C._O_U8TEXT)
		C.SetConsoleMode(C.GetStdHandle(C.STD_OUTPUT_HANDLE), C.ENABLE_PROCESSED_OUTPUT | 0x0004) // ENABLE_VIRTUAL_TERMINAL_PROCESSING
		C.setbuf(C.stdout,0)
	}
}

fn C.memcpy(byteptr, byteptr, int)
fn C.memmove(byteptr, byteptr, int)
//fn C.malloc(int) byteptr
fn C.realloc(byteptr, int) byteptr

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

fn C.backtrace(voidptr, int) int

pub fn print_backtrace_skipping_top_frames(skipframes int) {
	$if mac {
		buffer := [100]byteptr
		nr_ptrs := C.backtrace(*voidptr(buffer), 100)
		C.backtrace_symbols_fd(*voidptr(&buffer[skipframes]), nr_ptrs-skipframes, 1)
		return
	}
	$if linux {
		$if !android {
			// backtrace is not available on Android.
			if C.backtrace_symbols_fd != 0 {
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
					f := byteptr(C.popen(cmd.str, 'r'))
					if isnil(f) {
						println(sframe) continue
					}
					buf := [1000]byte
					mut output := ''
					for C.fgets(buf, 1000, f) != 0 {
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
			}else{
				C.printf('backtrace_symbols_fd is missing, so printing backtraces is not available.\n')
				C.printf('Some libc implementations like musl simply do not provide it.\n')
			}
		}
	}
	println('print_backtrace_skipping_top_frames is not implemented on this platform for now...\n')
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

pub fn free(ptr voidptr) {
	C.free(ptr)
}

fn memdup(src voidptr, sz int) voidptr {
	mem := malloc(sz)
	return C.memcpy(mem, src, sz)
}

fn v_ptr_free(ptr voidptr) {
	C.free(ptr)
}

pub fn is_atty(fd int) bool {
	$if windows {
		mut mode := 0
		C.GetConsoleMode(C._get_osfhandle(fd), &mode)
		return mode > 0
	} $else {
		return C.isatty(fd) != 0
	}
}

