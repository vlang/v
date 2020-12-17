module builtin

type FnExitCb = fn()
fn C.atexit(f FnExitCb) int

pub fn exit(code int) {
	C.exit(code)
}

// panic_debug - private function that V uses for panics, -cg/-g is passed
// recent versions of tcc print nicer backtraces automatically
// NB: the duplication here is because tcc_backtrace should be called directly
// inside the panic functions.
fn panic_debug(line_no int, file string, mod string, fn_name string, s string) {
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
	$if exit_after_panic_message ? {
		C.exit(1)
	} $else {
		$if no_backtrace ? {
			C.exit(1)
		} $else {
			$if tinyc {
				$if panics_break_into_debugger ? {
					break_if_debugger_attached()
				} $else {
					C.tcc_backtrace('Backtrace')
				}
				C.exit(1)
			}
			print_backtrace_skipping_top_frames(1)
			$if panics_break_into_debugger ? {
				break_if_debugger_attached()
			}
			C.exit(1)
		}
	}
}

// panic prints a nice error message, then exits the process with exit code of 1.
// It also shows a backtrace on most platforms.
pub fn panic(s string) {
	eprintln('V panic: $s')
	$if exit_after_panic_message ? {
		C.exit(1)
	} $else {
		$if no_backtrace ? {
			C.exit(1)
		} $else {
			$if tinyc {
				$if panics_break_into_debugger ? {
					break_if_debugger_attached()
				} $else {
					C.tcc_backtrace('Backtrace')
				}
				C.exit(1)
			}
			print_backtrace_skipping_top_frames(1)
			$if panics_break_into_debugger ? {
				break_if_debugger_attached()
			}
			C.exit(1)
		}
	}
}

// eprintln prints a message with a line end, to stderr. Both stderr and stdout are flushed.
pub fn eprintln(s string) {
	// eprintln is used in panics, so it should not fail at all
	if s.str == 0 {
		eprintln('eprintln(NIL)')
	}
	C.fflush(C.stdout)
	C.fflush(C.stderr)
	C.write(2, s.str, s.len)
	C.write(2, c'\n', 1)
	C.fflush(C.stderr)
}

// eprint prints a message to stderr. Both stderr and stdout are flushed.
pub fn eprint(s string) {
	if s.str == 0 {
		eprint('eprint(NIL)')
	}
	C.fflush(C.stdout)
	C.fflush(C.stderr)
	C.write(2, s.str, s.len)
	C.fflush(C.stderr)
}

// print prints a message to stdout
pub fn print(s string) {
	C.write(1, s.str, s.len)
}

//#include "@VROOT/vlib/darwin/darwin.m"
//fn C.nsstring2(s string) voidptr
//fn C.NSLog(x voidptr)
//#include <asl.h>
// fn C.asl_log(voidptr, voidptr, int, charptr)

pub fn println(s string) {
	$if windows {
		print(s)
		print('\n')
	} $else {
		// For debugging .app applications (no way to read stdout) so that it's printed to macOS Console
		/*
		$if macos {
			C.asl_log(0, 0, C.ASL_LEVEL_ERR, s.str)
		}
		*/
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

[unsafe]
pub fn malloc(n int) byteptr {
	if n <= 0 {
		panic('malloc(<=0)')
	}
	$if prealloc {
		//println('p')
		res := g_m2_ptr
		unsafe {
			g_m2_ptr += n
		}
		nr_mallocs++
		return res
	} $else {
		ptr := unsafe {C.malloc(n)}
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

//#include <malloc/malloc.h>
//fn malloc_size(b byteptr) int

[unsafe]
pub fn v_realloc(b byteptr, n int) byteptr {
	$if prealloc {
		unsafe {
			new_ptr := malloc(n)
			size := 0 //malloc_size(b)
			C.memcpy(new_ptr, b, size)
			return new_ptr
		}
	} $else {
		ptr := unsafe {C.realloc(b, n)}
		if ptr == 0 {
			panic('realloc($n) failed')
		}
		return ptr
	}
}

[unsafe]
pub fn v_calloc(n int) byteptr {
	return C.calloc(1, n)
}

[unsafe]
pub fn vcalloc(n int) byteptr {
	if n < 0 {
		panic('calloc(<=0)')
	} else if n == 0 {
		return byteptr(0)
	}
	return C.calloc(1, n)
}

[unsafe]
pub fn free(ptr voidptr) {
	$if prealloc {
		return
	}
	C.free(ptr)
}

pub fn memdup(src voidptr, sz int) voidptr {
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc(sz)
		return C.memcpy(mem, src, sz)
	}
}

fn v_ptr_free(ptr voidptr) {
	$if prealloc {
		return
	}
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
