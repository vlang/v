module builtin

// eprintln prints a message with a line end, to stderr. Both stderr and stdout are flushed.
@[if !noeprintln ?]
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
@[if !noeprintln ?]
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

// flush_stdout flushes the stdout buffer, ensuring all remaining data is written.
// See also unbuffer_stdout() .
pub fn flush_stdout() {
	$if freestanding {
		not_implemented := 'flush_stdout is not implemented\n'
		bare_eprint(not_implemented.str, u64(not_implemented.len))
	} $else $if native {
		// Native backend uses C.write() directly, no libc buffering to flush.
		// C.stdout data symbol cannot be resolved through GOT.
	} $else {
		C.fflush(C.stdout)
	}
}

// flush_stderr flushes the stderr buffer, ensuring all remaining data is written.
pub fn flush_stderr() {
	$if freestanding {
		not_implemented := 'flush_stderr is not implemented\n'
		bare_eprint(not_implemented.str, u64(not_implemented.len))
	} $else $if native {
		// Native backend uses C.write() directly, no libc buffering to flush.
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
@[if !noprintln ?; manualfree]
pub fn println(s string) {
	if s.str == 0 {
		println('println(NIL)')
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
	$if builtin_writeln_should_write_at_once ? {
		unsafe {
			buf_len := s.len + 1 // space for \n
			mut buf := malloc(buf_len)
			C.memcpy(buf, s.str, s.len)
			buf[s.len] = `\n`
			_write_buf_to_fd(fd, buf, buf_len)
			free(buf)
		}
	} $else {
		lf := u8(`\n`)
		_write_buf_to_fd(fd, s.str, s.len)
		_write_buf_to_fd(fd, &lf, 1)
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
		// Flush any pending libc stdio output (from C.puts, C.putchar, etc.)
		// before writing directly via write() syscall to prevent output reordering.
		C.fflush(unsafe { nil })
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
