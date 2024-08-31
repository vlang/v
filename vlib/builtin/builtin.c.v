@[has_globals]
module builtin

pub type FnExitCb = fn ()

fn C.atexit(f FnExitCb) int
fn C.strerror(int) &char

@[noreturn]
fn vhalt() {
	for {}
}

@[markused]
fn v_segmentation_fault_handler(signal_number i32) {
	$if freestanding {
		eprintln('signal 11: segmentation fault')
	} $else {
		C.fprintf(C.stderr, c'signal %d: segmentation fault\n', signal_number)
	}
	$if use_libbacktrace ? {
		eprint_libbacktrace(1)
	} $else {
		print_backtrace()
	}
	exit(128 + signal_number)
}

// exit terminates execution immediately and returns exit `code` to the shell.
@[noreturn]
pub fn exit(code int) {
	C.exit(code)
}

// at_exit registers a fn callback, that will be called at normal process termination.
// It returns an error, if the registration was not successful.
// The registered callback functions, will be called either via exit/1,
// or via return from the main program, in the reverse order of their registration.
// The same fn may be registered multiple times.
// Each callback fn will called once for each registration.
pub fn at_exit(cb FnExitCb) ! {
	$if freestanding {
		return error('at_exit not implemented with -freestanding')
	} $else {
		res := C.atexit(cb)
		if res != 0 {
			return error_with_code('at_exit failed', res)
		}
	}
}

// panic_debug private function that V uses for panics, -cg/-g is passed
// recent versions of tcc print nicer backtraces automatically
// Note: the duplication here is because tcc_backtrace should be called directly
// inside the panic functions.
@[noreturn]
fn panic_debug(line_no int, file string, mod string, fn_name string, s string) {
	// Note: the order here is important for a stabler test output
	// module is less likely to change than function, etc...
	// During edits, the line number will change most frequently,
	// so it is last
	$if freestanding {
		bare_panic(s)
	} $else {
		eprintln('================ V panic ================')
		eprintln('   module: ${mod}')
		eprintln(' function: ${fn_name}()')
		eprintln('  message: ${s}')
		eprintln('     file: ${file}:${line_no}')
		eprintln('   v hash: ${@VCURRENTHASH}')
		eprintln('=========================================')
		$if native {
			C.exit(1) // TODO: native backtraces
		} $else $if exit_after_panic_message ? {
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
			$if use_libbacktrace ? {
				eprint_libbacktrace(1)
			} $else {
				print_backtrace_skipping_top_frames(1)
			}
			$if panics_break_into_debugger ? {
				break_if_debugger_attached()
			}
			C.exit(1)
		}
	}
	vhalt()
}

// panic_option_not_set is called by V, when you use option error propagation in your main function.
// It ends the program with a panic.
@[noreturn]
pub fn panic_option_not_set(s string) {
	panic('option not set (${s})')
}

// panic_result_not_set is called by V, when you use result error propagation in your main function
// It ends the program with a panic.
@[noreturn]
pub fn panic_result_not_set(s string) {
	panic('result not set (${s})')
}

// panic prints a nice error message, then exits the process with exit code of 1.
// It also shows a backtrace on most platforms.
@[noreturn]
pub fn panic(s string) {
	$if freestanding {
		bare_panic(s)
	} $else {
		eprint('V panic: ')
		eprintln(s)
		eprintln('v hash: ${@VCURRENTHASH}')
		$if native {
			C.exit(1) // TODO: native backtraces
		} $else $if exit_after_panic_message ? {
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
			$if use_libbacktrace ? {
				eprint_libbacktrace(1)
			} $else {
				print_backtrace_skipping_top_frames(1)
			}
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
		err_msg = 'error ${errnum}'
	} $else {
		$if !vinix {
			c_msg := C.strerror(errnum)
			err_msg = string{
				str:    &u8(c_msg)
				len:    unsafe { C.strlen(c_msg) }
				is_lit: 1
			}
		}
	}
	return err_msg
}

// panic with a C-API error message matching `errnum`
@[noreturn]
pub fn panic_error_number(basestr string, errnum int) {
	panic(basestr + c_error_number_str(errnum))
}

// eprintln prints a message with a line end, to stderr. Both stderr and stdout are flushed.
pub fn eprintln(s string) {
	if s.str == 0 {
		eprintln('eprintln(NIL)')
		return
	}
	$if builtin_print_use_fprintf ? {
		C.fprintf(C.stderr, c'%.*s\n', s.len, s.str)
		return
	}
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		bare_eprint(s.str, u64(s.len))
		bare_eprint(c'\n', 1)
	} $else $if ios {
		C.WrappedNSLog(s.str)
	} $else {
		flush_stdout()
		flush_stderr()
		// eprintln is used in panics, so it should not fail at all
		$if android && !termux {
			C.android_print(C.stderr, c'%.*s\n', s.len, s.str)
		}
		_writeln_to_fd(2, s)
		flush_stderr()
	}
}

// eprint prints a message to stderr. Both stderr and stdout are flushed.
pub fn eprint(s string) {
	if s.str == 0 {
		eprint('eprint(NIL)')
		return
	}
	$if builtin_print_use_fprintf ? {
		C.fprintf(C.stderr, c'%.*s', s.len, s.str)
		return
	}
	$if freestanding {
		// flushing is only a thing with C.FILE from stdio.h, not on the syscall level
		bare_eprint(s.str, u64(s.len))
	} $else $if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		C.WrappedNSLog(s.str)
	} $else {
		flush_stdout()
		flush_stderr()
		$if android && !termux {
			C.android_print(C.stderr, c'%.*s', s.len, s.str)
		}
		_write_buf_to_fd(2, s.str, s.len)
		flush_stderr()
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

// unbuffer_stdout will turn off the default buffering done for stdout.
// It will affect all consequent print and println calls, effectively making them behave like
// eprint and eprintln do. It is useful for programs, that want to produce progress bars, without
// cluttering your code with a flush_stdout() call after every print() call. It is also useful for
// programs (sensors), that produce small chunks of output, that you want to be able to process
// immediately.
// Note 1: if used, *it should be called at the start of your program*, before using
// print or println().
// Note 2: most libc implementations, have logic that use line buffering for stdout, when the output
// stream is connected to an interactive device, like a terminal, and otherwise fully buffer it,
// which is good for the output performance for programs that can produce a lot of output (like
// filters, or cat etc), but bad for latency. Normally, it is usually what you want, so it is the
// default for V programs too.
// See https://www.gnu.org/software/libc/manual/html_node/Buffering-Concepts.html .
// See https://pubs.opengroup.org/onlinepubs/9699919799/functions/V2_chap02.html#tag_15_05 .
pub fn unbuffer_stdout() {
	$if freestanding {
		not_implemented := 'unbuffer_stdout is not implemented\n'
		bare_eprint(not_implemented.str, u64(not_implemented.len))
	} $else {
		unsafe { C.setbuf(C.stdout, 0) }
	}
}

// print prints a message to stdout. Note that unlike `eprint`, stdout is not automatically flushed.
@[manualfree]
pub fn print(s string) {
	$if builtin_print_use_fprintf ? {
		C.fprintf(C.stdout, c'%.*s', s.len, s.str)
		return
	}
	$if android && !termux {
		C.android_print(C.stdout, c'%.*s\n', s.len, s.str)
	} $else $if ios {
		// TODO: Implement a buffer as NSLog doesn't have a "print"
		C.WrappedNSLog(s.str)
	} $else $if freestanding {
		bare_print(s.str, u64(s.len))
	} $else {
		_write_buf_to_fd(1, s.str, s.len)
	}
}

// println prints a message with a line end, to stdout. Note that unlike `eprintln`, stdout is not automatically flushed.
@[manualfree]
pub fn println(s string) {
	if s.str == 0 {
		println('println(NIL)')
		return
	}
	$if noprintln ? {
		return
	}
	$if builtin_print_use_fprintf ? {
		C.fprintf(C.stdout, c'%.*s\n', s.len, s.str)
		return
	}
	$if android && !termux {
		C.android_print(C.stdout, c'%.*s\n', s.len, s.str)
		return
	} $else $if ios {
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

@[manualfree]
fn _writeln_to_fd(fd int, s string) {
	$if !builtin_writeln_should_write_at_once ? {
		lf := u8(`\n`)
		_write_buf_to_fd(fd, s.str, s.len)
		_write_buf_to_fd(fd, &lf, 1)
		return
	}
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

@[manualfree]
fn _write_buf_to_fd(fd int, buf &u8, buf_len int) {
	if buf_len <= 0 {
		return
	}
	mut ptr := unsafe { buf }
	mut remaining_bytes := isize(buf_len)
	mut x := isize(0)
	$if freestanding || vinix || builtin_write_buf_to_fd_should_use_c_write ? {
		unsafe {
			for remaining_bytes > 0 {
				x = C.write(fd, ptr, remaining_bytes)
				ptr += x
				remaining_bytes -= x
			}
		}
	} $else {
		mut stream := voidptr(C.stdout)
		if fd == 2 {
			stream = voidptr(C.stderr)
		}
		unsafe {
			for remaining_bytes > 0 {
				x = isize(C.fwrite(ptr, 1, remaining_bytes, stream))
				ptr += x
				remaining_bytes -= x
			}
		}
	}
}

__global total_m = i64(0)
// malloc dynamically allocates a `n` bytes block of memory on the heap.
// malloc returns a `byteptr` pointing to the memory address of the allocated space.
// unlike the `calloc` family of functions - malloc will not zero the memory block.
@[unsafe]
pub fn malloc(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'_v_malloc %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	vplayground_mlimit(n)
	if n < 0 {
		panic('malloc(${n} < 0)')
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
		panic('malloc(${n}) failed')
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

@[unsafe]
pub fn malloc_noscan(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'malloc_noscan %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	vplayground_mlimit(n)
	if n < 0 {
		panic('malloc_noscan(${n} < 0)')
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
		panic('malloc_noscan(${n}) failed')
	}
	$if debug_malloc ? {
		// Fill in the memory with something != 0 i.e. `M`, so it is easier to spot
		// when the calling code wrongly relies on it being zeroed.
		unsafe { C.memset(res, 0x4D, n) }
	}
	return res
}

@[inline]
fn __at_least_one(how_many u64) u64 {
	// handle the case for allocating memory for empty structs, which have sizeof(EmptyStruct) == 0
	// in this case, just allocate a single byte, avoiding the panic for malloc(0)
	if how_many == 0 {
		return 1
	}
	return how_many
}

// malloc_uncollectable dynamically allocates a `n` bytes block of memory
// on the heap, which will NOT be garbage-collected (but its contents will).
@[unsafe]
pub fn malloc_uncollectable(n isize) &u8 {
	$if trace_malloc ? {
		total_m += n
		C.fprintf(C.stderr, c'malloc_uncollectable %6d total %10d\n', n, total_m)
		// print_backtrace()
	}
	vplayground_mlimit(n)
	if n < 0 {
		panic('malloc_uncollectable(${n} < 0)')
	}
	mut res := &u8(0)
	$if prealloc {
		return unsafe { prealloc_malloc(n) }
	} $else $if gcboehm ? {
		unsafe {
			res = C.GC_MALLOC_UNCOLLECTABLE(n)
		}
	} $else $if freestanding {
		res = unsafe { __malloc(usize(n)) }
	} $else {
		res = unsafe { C.malloc(n) }
	}
	if res == 0 {
		panic('malloc_uncollectable(${n}) failed')
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
@[unsafe]
pub fn v_realloc(b &u8, n isize) &u8 {
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
		panic('realloc(${n}) failed')
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
@[unsafe]
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
		panic('realloc_data(${old_data}, ${old_size}, ${new_size}) failed')
	}
	return nptr
}

// vcalloc dynamically allocates a zeroed `n` bytes block of memory on the heap.
// vcalloc returns a `byteptr` pointing to the memory address of the allocated space.
// Unlike `v_calloc` vcalloc checks for negative values given in `n`.
pub fn vcalloc(n isize) &u8 {
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc %6d total %10d\n', n, total_m)
	}
	if n < 0 {
		panic('calloc(${n} < 0)')
	} else if n == 0 {
		return &u8(0)
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
pub fn vcalloc_noscan(n isize) &u8 {
	$if trace_vcalloc ? {
		total_m += n
		C.fprintf(C.stderr, c'vcalloc_noscan %6d total %10d\n', n, total_m)
	}
	vplayground_mlimit(n)
	$if prealloc {
		return unsafe { prealloc_calloc(n) }
	} $else $if gcboehm ? {
		if n < 0 {
			panic('calloc_noscan(${n} < 0)')
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
@[unsafe]
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
@[unsafe]
pub fn memdup(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc(sz)
		return C.memcpy(mem, src, sz)
	}
}

@[unsafe]
pub fn memdup_noscan(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup_noscan size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc_noscan(1)
	}
	unsafe {
		mem := malloc_noscan(sz)
		return C.memcpy(mem, src, sz)
	}
}

// memdup_uncollectable dynamically allocates a `sz` bytes block of memory
// on the heap, which will NOT be garbage-collected (but its contents will).
// memdup_uncollectable then copies the contents of `src` into the allocated
// space and returns a pointer to the newly allocated space.
@[unsafe]
pub fn memdup_uncollectable(src voidptr, sz isize) voidptr {
	$if trace_memdup ? {
		C.fprintf(C.stderr, c'memdup_uncollectable size: %10d\n', sz)
	}
	if sz == 0 {
		return vcalloc(1)
	}
	unsafe {
		mem := malloc_uncollectable(sz)
		return C.memcpy(mem, src, sz)
	}
}

pub struct GCHeapUsage {
pub:
	heap_size      usize
	free_bytes     usize
	total_bytes    usize
	unmapped_bytes usize
	bytes_since_gc usize
}

// gc_heap_usage returns the info about heap usage
pub fn gc_heap_usage() GCHeapUsage {
	$if gcboehm ? {
		mut res := GCHeapUsage{}
		C.GC_get_heap_usage_safe(&res.heap_size, &res.free_bytes, &res.unmapped_bytes,
			&res.bytes_since_gc, &res.total_bytes)
		return res
	} $else {
		return GCHeapUsage{}
	}
}

// gc_memory_use returns the total memory use in bytes by all allocated blocks
pub fn gc_memory_use() usize {
	$if gcboehm ? {
		return C.GC_get_memory_use()
	} $else {
		return 0
	}
}

@[inline]
fn v_fixed_index(i int, len int) int {
	$if !no_bounds_checking {
		if i < 0 || i >= len {
			s := 'fixed array index out of range (index: ${i}, len: ${len})'
			panic(s)
		}
	}
	return i
}

// NOTE: g_main_argc and g_main_argv are filled in right after C's main start.
// They are used internally by V's builtin; for user code, it is much
// more convenient to just use `os.args` or call `arguments()` instead.

@[markused]
__global g_main_argc = int(0)

@[markused]
__global g_main_argv = unsafe { nil }

// arguments returns the command line arguments, used for starting the current program as a V array of strings.
// The first string in the array (index 0), is the name of the program, used for invoking the program.
// The second string in the array (index 1), if it exists, is the first argument to the program, etc.
// For example, if you started your program as `myprogram -option`, then arguments() will return ['myprogram', '-option'].
// Note: if you `v run file.v abc def`, then arguments() will return ['file', 'abc', 'def'], or ['file.exe', 'abc', 'def'] (on Windows).
pub fn arguments() []string {
	argv := &&u8(g_main_argv)
	mut res := []string{cap: g_main_argc}
	for i in 0 .. g_main_argc {
		$if windows {
			res << unsafe { string_from_wide(&u16(argv[i])) }
		} $else {
			res << unsafe { tos_clone(argv[i]) }
		}
	}
	return res
}

@[if vplayground ?]
fn vplayground_mlimit(n isize) {
	if n > 10000 {
		panic('allocating more than 10 KB at once is not allowed in the V playground')
	}
	if total_m > 50 * 1024 * 1024 {
		panic('allocating more than 50 MB is not allowed in the V playground')
	}
}
