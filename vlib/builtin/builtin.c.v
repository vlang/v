[has_globals]
module builtin

type FnExitCb = fn ()

fn C.atexit(f FnExitCb) int
fn C.strerror(int) &char

[noreturn]
fn vhalt() {
	for {}
}

// exit terminates execution immediately and returns exit `code` to the shell.
[noreturn]
pub fn exit(code int) {
	C.exit(code)
}

fn vcommithash() string {
	return unsafe { tos5(&char(C.V_CURRENT_COMMIT_HASH)) }
}

// panic_debug private function that V uses for panics, -cg/-g is passed
// recent versions of tcc print nicer backtraces automatically
// Note: the duplication here is because tcc_backtrace should be called directly
// inside the panic functions.
[noreturn]
fn panic_debug(line_no int, file string, mod string, fn_name string, s string) {
	// Note: the order here is important for a stabler test output
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
		} $else $if no_backtrace ? {
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
	vhalt()
}

// panic_optional_not_set prints given optional not set and exits the process
[noreturn]
pub fn panic_optional_not_set(s string) {
	panic('optional not set ($s)')
}

// panic prints a nice error message, then exits the process with exit code of 1.
// It also shows a backtrace on most platforms.
[noreturn]
pub fn panic(s string) {
	$if freestanding {
		bare_panic(s)
	} $else {
		eprint('V panic: ')
		eprintln(s)
		eprintln('v hash: $vcommithash()')
		$if exit_after_panic_message ? {
			C.exit(1)
		} $else $if no_backtrace ? {
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
	vhalt()
}

// return a C-API error message matching to `errnum`
pub fn c_error_number_str(errnum int) string {
	mut err_msg := ''
	$if freestanding {
		err_msg = 'error $errnum'
	} $else {
		$if !vinix {
			c_msg := C.strerror(errnum)
			err_msg = string{
				str: &u8(c_msg)
				len: unsafe { C.strlen(c_msg) }
				is_lit: 1
			}
		}
	}
	return err_msg
}

// panic with a C-API error message matching `errnum`
[noreturn]
pub fn panic_error_number(basestr string, errnum int) {
	panic(basestr + c_error_number_str(errnum))
}

// eprintln prints a message with a line end, to stderr. Both stderr and stdout are flushed.
pub fn eprintln(s string) {
	if s.str == 0 {
		eprintln('eprintln(NIL)')
		return
	}
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		bare_eprint(s.str, u64(s.len))
		bare_eprint(c'\n', 1)
	} $else $if ios {
		C.WrappedNSLog(s.str)
	} $else {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		// eprintln is used in panics, so it should not fail at all
		$if android {
			C.fprintf(C.stderr, c'%.*s\n', s.len, s.str)
		}
		_writeln_to_fd(2, s)
		C.fflush(C.stderr)
	}
}

// eprint prints a message to stderr. Both stderr and stdout are flushed.
pub fn eprint(s string) {
	if s.str == 0 {
		eprint('eprint(NIL)')
		return
	}
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		bare_eprint(s.str, u64(s.len))
	} $else $if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		C.WrappedNSLog(s.str)
	} $else {
		C.fflush(C.stdout)
		C.fflush(C.stderr)
		$if android {
			C.fprintf(C.stderr, c'%.*s', s.len, s.str)
		}
		_write_buf_to_fd(2, s.str, s.len)
		C.fflush(C.stderr)
	}
}

pub fn flush_stdout() {
	$if freestanding {
		not_implemented := 'flush_stdout is not implemented\n'
		bare_eprint(not_implemented.str, u64(not_implemented.len))
	} $else {
		C.fflush(C.stdout)
	}
}

pub fn flush_stderr() {
	$if freestanding {
		not_implemented := 'flush_stderr is not implemented\n'
		bare_eprint(not_implemented.str, u64(not_implemented.len))
	} $else {
		C.fflush(C.stderr)
	}
}

// print prints a message to stdout. Unlike `println` stdout is not automatically flushed.
[manualfree]
pub fn print(s string) {
	$if android {
		C.fprintf(C.stdout, c'%.*s', s.len, s.str) // logcat
	}
	// no else if for android termux support
	$if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		C.WrappedNSLog(s.str)
	} $else $if freestanding {
		bare_print(s.str, u64(s.len))
	} $else {
		_write_buf_to_fd(1, s.str, s.len)
	}
}

// println prints a message with a line end, to stdout. stdout is flushed.
[manualfree]
pub fn println(s string) {
	if s.str == 0 {
		println('println(NIL)')
		return
	}
	$if android {
		C.fprintf(C.stdout, c'%.*s\n', s.len, s.str) // logcat
		return
	}
	// no else if for android termux support
	$if ios {
		C.WrappedNSLog(s.str)
		return
	} $else $if freestanding {
		bare_print(s.str, u64(s.len))
		bare_print(c'\n', 1)
		return
	} $else {
		_writeln_to_fd(1, s)
	}
}

[manualfree]
fn _writeln_to_fd(fd int, s string) {
	unsafe {
		buf_len := s.len + 1 // space for \n
		mut buf := malloc(buf_len)
		defer {
			free(buf)
		}
		C.memcpy(buf, s.str, s.len)
		buf[s.len] = `\n`
		_write_buf_to_fd(fd, buf, buf_len)
	}
}

[manualfree]
fn _write_buf_to_fd(fd int, buf &u8, buf_len int) {
	if buf_len <= 0 {
		return
	}
	unsafe {
		mut ptr := buf
		mut remaining_bytes := buf_len
		for remaining_bytes > 0 {
			x := C.write(fd, ptr, remaining_bytes)
			ptr += x
			remaining_bytes -= x
		}
	}
}

__global total_m = i64(0)
// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
[unsafe]
pub fn malloc(n int) &u8 {
	if n <= 0 {
		panic('malloc($n <= 0)')
	}
	$if vplayground ? {
		if n > 10000 {
			panic('allocating more than 10 KB at once is not allowed in the V playground')
		}
		if total_m > 50 * 1024 * 1024 {
			panic('allocating more than 50 MB is not allowed in the V playground')
		}
	}
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'_v_malloc %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	mut res := &u8(0)
	$if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		unsafe {
			res = C.GC_MALLOC(n)
		}
	} $else $if freestanding {
		// todo: is this safe to call malloc there? We export __malloc as malloc and it uses dlmalloc behind the scenes
		// so theoretically it is safe
		res = unsafe { __malloc(usize(n)) }
	} $else {
		res = unsafe { C.malloc(n) }
	}
	if res == 0 {
		panic('malloc($n) failed')
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

[unsafe]
pub fn malloc_noscan(n int) &u8 {
	if n <= 0 {
		panic('malloc_noscan($n <= 0)')
	}
	$if vplayground ? {
		if n > 10000 {
			panic('allocating more than 10 KB at once is not allowed in the V playground')
		}
		if total_m > 50 * 1024 * 1024 {
			panic('allocating more than 50 MB is not allowed in the V playground')
		}
	}
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'malloc_noscan %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	mut res := &u8(0)
	$if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		$if gcboehm_opt ? {
			unsafe {
				res = C.GC_MALLOC_ATOMIC(n)
			}
		} $else {
			unsafe {
				res = C.GC_MALLOC(n)
			}
		}
	} $else $if freestanding {
		res = unsafe { __malloc(usize(n)) }
	} $else {
		res = unsafe { C.malloc(n) }
	}
	if res == 0 {
		panic('malloc_noscan($n) failed')
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

// v_realloc resizes the memory block `b` with `n` bytes.
// The `b byteptr` must be a pointer to an existing memory block
// previously allocated with `malloc`, `v_calloc` or `vcalloc`.
// Please, see also realloc_data, and use it instead if possible.
[unsafe]
pub fn v_realloc(b &u8, n int) &u8 {
	$if trace_realloc ? {
		C.fprintf(C.stderr, c'v_realloc %6d\n', n)
	}
	mut new_ptr := &u8(0)
	$if prealloc {
		unsafe {
			new_ptr = malloc(n)
			C.memcpy(new_ptr, b, n)
		}
		return new_ptr
	} $else $if gcboehm ? {
		new_ptr = unsafe { C.GC_REALLOC(b, n) }
	} $else {
		new_ptr = unsafe { C.realloc(b, n) }
	}
	if new_ptr == 0 {
		panic('realloc($n) failed')
	}
	return new_ptr
}

// realloc_data resizes the memory block pointed by `old_data` to `new_size`
// bytes. `old_data` must be a pointer to an existing memory block, previously
// allocated with `malloc`, `v_calloc` or `vcalloc`, of size `old_data`.
// realloc_data returns a pointer to the new location of the block.
// Note: if you know the old data size, it is preferable to call `realloc_data`,
// instead of `v_realloc`, at least during development, because `realloc_data`
// can make debugging easier, when you compile your program with
// `-d debug_realloc`.
[unsafe]
pub fn realloc_data(old_data &u8, old_size int, new_size int) &u8 {
	$if trace_realloc ? {
		C.fprintf(C.stderr, c'realloc_data old_size: %6d new_size: %6d\n', old_size, new_size)
	}
	$if prealloc {
		return unsafe { prealloc_realloc(old_data, old_size, new_size) }
	}
	$if debug_realloc ? {
		// Note: this is slower, but helps debugging memory problems.
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
	mut nptr := &u8(0)
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
pub fn vcalloc(n int) &u8 {
	if n < 0 {
		panic('calloc($n < 0)')
	} else if n == 0 {
		return &u8(0)
	}
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc %6d total %10d\n', n, total_m)
	}
	$if prealloc {
		return unsafe { prealloc_calloc(n) }
	} $else $if gcboehm ? {
		return unsafe { &u8(C.GC_MALLOC(n)) }
	} $else {
		return unsafe { C.calloc(1, n) }
	}
}

// special versions of the above that allocate memory which is not scanned
// for pointers (but is collected) when the Boehm garbage collection is used
pub fn vcalloc_noscan(n int) &u8 {
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc_noscan %6d total %10d\n', n, total_m)
	}
	$if prealloc {
		return unsafe { prealloc_calloc(n) }
	} $else $if gcboehm ? {
		$if vplayground ? {
			if n > 10000 {
				panic('allocating more than 10 KB is not allowed in the playground')
			}
		}
		if n < 0 {
			panic('calloc_noscan($n < 0)')
		}
		return $if gcboehm_opt ? {
			unsafe { &u8(C.memset(C.GC_MALLOC_ATOMIC(n), 0, n)) }
		} $else {
			unsafe { &u8(C.GC_MALLOC(n)) }
		}
	} $else {
		return unsafe { vcalloc(n) }
	}
}

// free allows for manually freeing memory allocated at the address `ptr`.
[unsafe]
pub fn free(ptr voidptr) {
	$if prealloc {
		return
	} $else $if gcboehm ? {
		// It is generally better to leave it to Boehm's gc to free things.
		// Calling C.GC_FREE(ptr) was tried initially, but does not work
		// well with programs that do manual management themselves.
		//
		// The exception is doing leak detection for manual memory management:
		$if gcboehm_leak ? {
			unsafe { C.GC_FREE(ptr) }
		}
	} $else {
		C.free(ptr)
	}
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

[unsafe]
pub fn memdup_noscan(src voidptr, sz int) voidptr {
	if sz == 0 {
		return vcalloc_noscan(1)
	}
	unsafe {
		mem := malloc_noscan(sz)
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

// print_backtrace shows a backtrace of the current call stack on stdout
pub fn print_backtrace() {
	// At the time of backtrace_symbols_fd call, the C stack would look something like this:
	// * print_backtrace_skipping_top_frames
	// * print_backtrace itself
	// * the rest of the backtrace frames
	// => top 2 frames should be skipped, since they will not be informative to the developer
	$if !no_backtrace ? {
		$if freestanding {
			println(bare_backtrace())
		} $else {
			$if tinyc {
				C.tcc_backtrace(c'Backtrace')
			} $else {
				print_backtrace_skipping_top_frames(2)
			}
		}
	}
}
