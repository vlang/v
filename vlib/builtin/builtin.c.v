module builtin

type FnExitCb = fn ()

fn C.atexit(f FnExitCb) int

// exit terminates execution immediately and returns exit `code` to the shell.
pub fn exit(code int) {
	C.exit(code)
}

fn vcommithash() string {
	return unsafe { tos5(&char(C.V_CURRENT_COMMIT_HASH)) }
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
	$if freestanding {
		bare_panic(s)
	} $else {
		eprintln('================ V panic ================')
		eprintln('   module: $mod')
		eprintln(' function: ${fn_name}()')
		eprintln('  message: $s')
		eprintln('     file: $file:$line_no')
		eprintln('   v hash: $vcommithash()')
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
						C.tcc_backtrace(c'Backtrace')
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
}

pub fn panic_optional_not_set(s string) {
	panic('optional not set ($s)')
}

// panic prints a nice error message, then exits the process with exit code of 1.
// It also shows a backtrace on most platforms.
pub fn panic(s string) {
	$if freestanding {
		bare_panic(s)
	} $else {
		eprint('V panic: ')
		eprintln(s)
		eprintln('v hash: $vcommithash()')
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
						C.tcc_backtrace(c'Backtrace')
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
}

// eprintln prints a message with a line end, to stderr. Both stderr and stdout are flushed.
pub fn eprintln(s string) {
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		if s.str == 0 {
			bare_eprint(c'eprintln(NIL)\n', 14)
		} else {
			bare_eprint(s.str, u64(s.len))
			bare_eprint(c'\n', 1)
		}
	} $else $if ios {
		if s.str == 0 {
			C.WrappedNSLog(c'eprintln(NIL)\n')
		} else {
			C.WrappedNSLog(s.str)
		}
	} $else {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		// eprintln is used in panics, so it should not fail at all
		$if android {
			if s.str == 0 {
				C.fprintf(C.stderr, c'eprintln(NIL)\n')
			} else {
				C.fprintf(C.stderr, c'%.*s\n', s.len, s.str)
			}
		} $else {
			if s.str == 0 {
				_ = C.write(2, c'eprintln(NIL)\n', 14)
			} else {
				_ = C.write(2, s.str, s.len)
				_ = C.write(2, c'\n', 1)
			}
		}
		C.fflush(C.stderr)
	}
}

// eprint prints a message to stderr. Both stderr and stdout are flushed.
pub fn eprint(s string) {
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		if s.str == 0 {
			bare_eprint(c'eprint(NIL)\n', 12)
		} else {
			bare_eprint(s.str, u64(s.len))
		}
	} $else $if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		if s.str == 0 {
			C.WrappedNSLog(c'eprint(NIL)')
		} else {
			C.WrappedNSLog(s.str)
		}
	} $else {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		$if android {
			if s.str == 0 {
				C.fprintf(C.stderr, c'eprint(NIL)')
			} else {
				C.fprintf(C.stderr, c'%.*s', s.len, s.str)
			}
		} $else {
			if s.str == 0 {
				_ = C.write(2, c'eprint(NIL)', 11)
			} else {
				_ = C.write(2, s.str, s.len)
			}
		}
		C.fflush(C.stderr)
	}
}

// print prints a message to stdout. Unlike `println` stdout is not automatically flushed.
// A call to `flush()` will flush the output buffer to stdout.
pub fn print(s string) {
	$if android {
		C.fprintf(C.stdout, c'%.*s', s.len, s.str)
	} $else $if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		C.WrappedNSLog(s.str)
	} $else $if freestanding {
		bare_print(s.str, u64(s.len))
	} $else {
		_ = C.write(1, s.str, s.len)
	}
}

/*
#include "@VEXEROOT/vlib/darwin/darwin.m"
fn C.nsstring2(s string) voidptr
fn C.NSLog(x voidptr)
#include <asl.h>
fn C.asl_log(voidptr, voidptr, int, charptr)
*/
// println prints a message with a line end, to stdout. stdout is flushed.
pub fn println(s string) {
	if s.str == 0 {
		$if android {
			C.fprintf(C.stdout, c'println(NIL)\n')
		} $else $if ios {
			C.WrappedNSLog(c'println(NIL)')
		} $else $if freestanding {
			bare_print(s.str, u64(s.len))
			bare_print(c'println(NIL)\n', 13)
		} $else {
			_ = C.write(1, c'println(NIL)\n', 13)
		}
		return
	}
	$if android {
		C.fprintf(C.stdout, c'%.*s\n', s.len, s.str)
	} $else $if ios {
		C.WrappedNSLog(s.str)
	} $else $if freestanding {
		bare_print(s.str, u64(s.len))
		bare_print(c'\n', 1)
	} $else {
		_ = C.write(1, s.str, s.len)
		_ = C.write(1, c'\n', 1)
	}
}

// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
[unsafe]
pub fn malloc(n int) &byte {
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
		C.fprintf(C.stderr, c'v_malloc %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	mut res := &byte(0)
	$if prealloc {
		res = g_m2_ptr
		unsafe {
			g_m2_ptr += n
		}
		nr_mallocs++
	} $else {
		$if gcboehm ? {
			unsafe {
				res = C.GC_MALLOC(n)
			}
		} $else $if freestanding {
			mut e := Errno{}
			res, e = mm_alloc(u64(n))
			if e != .enoerror {
				eprint('malloc() failed: ')
				eprintln(e.str())
				panic('malloc() failed')
			}
		} $else {
			res = unsafe { C.malloc(n) }
		}
		if res == 0 {
			panic('malloc($n) failed')
		}
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x88, n) }
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
// Please, see also realloc_data, and use it instead if possible.
[unsafe]
pub fn v_realloc(b &byte, n int) &byte {
	mut new_ptr := &byte(0)
	$if prealloc {
		unsafe {
			new_ptr = malloc(n)
			C.memcpy(new_ptr, b, n)
		}
	} $else {
		$if gcboehm ? {
			new_ptr = unsafe { C.GC_REALLOC(b, n) }
		} $else {
			new_ptr = unsafe { C.realloc(b, n) }
		}
		if new_ptr == 0 {
			panic('realloc($n) failed')
		}
	}
	return new_ptr
}

// realloc_data resizes the memory block pointed by `old_data` to `new_size`
// bytes. `old_data` must be a pointer to an existing memory block, previously
// allocated with `malloc`, `v_calloc` or `vcalloc`, of size `old_data`.
// realloc_data returns a pointer to the new location of the block.
// NB: if you know the old data size, it is preferable to call `realloc_data`,
// instead of `v_realloc`, at least during development, because `realloc_data`
// can make debugging easier, when you compile your program with
// `-d debug_realloc`.
[unsafe]
pub fn realloc_data(old_data &byte, old_size int, new_size int) &byte {
	$if prealloc {
		unsafe {
			new_ptr := malloc(new_size)
			min_size := if old_size < new_size { old_size } else { new_size }
			C.memcpy(new_ptr, old_data, min_size)
			return new_ptr
		}
	}
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
			new_ptr := malloc(new_size)
			min_size := if old_size < new_size { old_size } else { new_size }
			C.memcpy(new_ptr, old_data, min_size)
			C.memset(old_data, 0x57, old_size)
			free(old_data)
			return new_ptr
		}
	}
	mut nptr := &byte(0)
	$if gcboehm ? {
		nptr = unsafe { C.GC_REALLOC(old_data, new_size) }
	} $else {
		nptr = unsafe { C.realloc(old_data, new_size) }
	}
	if nptr == 0 {
		panic('realloc_data($old_data, $old_size, $new_size) failed')
	}
	return nptr
}

// vcalloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// vcalloc returns a `byteptr` pointing to the memory address of the allocated space.
// Unlike `v_calloc` vcalloc checks for negative values given in `n`.
pub fn vcalloc(n int) &byte {
	if n < 0 {
		panic('calloc(<0)')
	} else if n == 0 {
		return &byte(0)
	}
	$if gcboehm ? {
		return &byte(C.GC_MALLOC(n))
	} $else {
		return C.calloc(1, n)
	}
}

// special versions of the above that allocate memory which is not scanned
// for pointers (but is collected) when the Boehm garbage collection is used
pub fn vcalloc_noscan(n int) &byte {
	$if gcboehm ? {
		$if vplayground ? {
			if n > 10000 {
				panic('allocating more than 10 KB is not allowed in the playground')
			}
		}
		if n < 0 {
			panic('calloc(<0)')
		}
		return &byte(unsafe { C.memset(C.GC_MALLOC_ATOMIC(n), 0, n) })
	} $else {
		return unsafe { vcalloc(n) }
	}
}

// free allows for manually freeing memory allocated at the address `ptr`.
[unsafe]
pub fn free(ptr voidptr) {
	$if prealloc {
		return
	}
	$if gcboehm ? {
		// It is generally better to leave it to Boehm's gc to free things.
		// Calling C.GC_FREE(ptr) was tried initially, but does not work
		// well with programs that do manual management themselves.
		//
		// The exception is doing leak detection for manual memory management:
		$if gcboehm_leak ? {
			C.GC_FREE(ptr)
		}
		return
	}
	C.free(ptr)
}

// memdup dynamically allocates a `sz` bytes block of memory on the heap
// memdup then copies the contents of `src` into the allocated space and
// returns a pointer to the newly allocated space.
[unsafe]
pub fn memdup(src voidptr, sz int) voidptr {
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc(sz)
		return C.memcpy(mem, src, sz)
	}
}

[inline]
fn v_fixed_index(i int, len int) int {
	$if !no_bounds_checking ? {
		if i < 0 || i >= len {
			s := 'fixed array index out of range (index: $i, len: $len)'
			panic(s)
		}
	}
	return i
}
