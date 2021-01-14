module builtin

type FnExitCb = fn ()

fn C.atexit(f FnExitCb) int

// exit terminates execution immediately and returns exit `code` to the shell.
pub fn exit(code int) {
	C.exit(code)
}

// panic_debug private function that V uses for panics, -cg/-g is passed
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
	C.fflush(C.stdout)
	C.fflush(C.stderr)
	// eprintln is used in panics, so it should not fail at all
	if s.str == 0 {
		C.write(2, c'eprintln(NIL)\n', 14)
	} else {
		C.write(2, s.str, s.len)
		C.write(2, c'\n', 1)
	}
	C.fflush(C.stderr)
}

// eprint prints a message to stderr. Both stderr and stdout are flushed.
pub fn eprint(s string) {
	C.fflush(C.stdout)
	C.fflush(C.stderr)
	if s.str == 0 {
		C.write(2, c'eprint(NIL)\n', 12)
	} else {
		C.write(2, s.str, s.len)
	}
	C.fflush(C.stderr)
}

// print prints a message to stdout. Unlike `println` stdout is not automatically flushed.
// A call to `flush()` will flush the output buffer to stdout.
pub fn print(s string) {
	C.write(1, s.str, s.len)
}

/*
#include "@VROOT/vlib/darwin/darwin.m"
fn C.nsstring2(s string) voidptr
fn C.NSLog(x voidptr)
#include <asl.h>
fn C.asl_log(voidptr, voidptr, int, charptr)
*/
// println prints a message with a line end, to stdout. stdout is flushed.
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

// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
[unsafe]
pub fn malloc(n int) byteptr {
	if n <= 0 {
		panic('> V malloc(<=0)')
	}
	$if vplayground ? {
		if n > 10000 {
			panic('allocating more than 10 KB is not allowed in the playground')
		}
	}
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'v_malloc %d total %d\n', n, total_m)
		// print_backtrace()
	}
	mut res := byteptr(0)
	$if prealloc {
		res = g_m2_ptr
		unsafe {
			g_m2_ptr += n
		}
		nr_mallocs++
	} $else {
		res = unsafe { C.malloc(n) }
		if res == 0 {
			panic('malloc($n) failed')
		}
	}
	return res
}

/*
#include <malloc/malloc.h>
fn malloc_size(b byteptr) int
*/
// v_realloc resizes the memory block `b` with `n` bytes.
// The `b byteptr` must be a pointer to an existing memory block
// previously allocated with `malloc`, `v_calloc` or `vcalloc`.
[unsafe]
pub fn v_realloc(b byteptr, n int) byteptr {
	mut new_ptr := byteptr(0)
	$if prealloc {
		unsafe {
			new_ptr = malloc(n)
			C.memcpy(new_ptr, b, n)
		}
	} $else {
		$if debug_realloc ? {
			// NB: this is slower, but helps debugging memory problems.
			// The main idea is to always force reallocating:
			// 1) allocate a new memory block
			// 2) copy the old to the new
			// 3) fill the old with 0x57 (`W`)
			// 4) free the old block
			// => if there is still a pointer to the old block somewhere
			//    it will point to memory that is now filled with 0x57.
			unsafe {
				new_ptr = malloc(n)
				C.memcpy(new_ptr, b, n)
				C.memset(b, 0x57, n)
				C.free(b)
			}
		} $else {
			new_ptr = unsafe { C.realloc(b, n) }
			if new_ptr == 0 {
				panic('realloc($n) failed')
			}
		}
	}
	return new_ptr
}

// v_calloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// v_calloc returns a `byteptr` pointing to the memory address of the allocated space.
[unsafe]
pub fn v_calloc(n int) byteptr {
	return C.calloc(1, n)
}

// vcalloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// vcalloc returns a `byteptr` pointing to the memory address of the allocated space.
// Unlike `v_calloc` vcalloc checks for negative values given in `n`.
[unsafe]
pub fn vcalloc(n int) byteptr {
	if n < 0 {
		panic('calloc(<=0)')
	} else if n == 0 {
		return byteptr(0)
	}
	return C.calloc(1, n)
}

// free allows for manually freeing memory allocated at the address `ptr`.
[unsafe]
pub fn free(ptr voidptr) {
	$if prealloc {
		return
	}
	C.free(ptr)
}

// memdup dynamically allocates a `sz` bytes block of memory on the heap
// memdup then copies the contents of `src` into the allocated space and
// returns a pointer to the newly allocated space.
pub fn memdup(src voidptr, sz int) voidptr {
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc(sz)
		return C.memcpy(mem, src, sz)
	}
}

// v_ptr_free is used internally to manually free up memory allocated at the address `ptr`.
fn v_ptr_free(ptr voidptr) {
	$if prealloc {
		return
	}
	C.free(ptr)
}

// is_atty returns 1 if the `fd` file descriptor is open and refers to a terminal
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
