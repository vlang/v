module builtin

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
		// vfmt off
		// Note: be carefull to not allocate here, avoid string interpolation
		flush_stdout()
		eprintln('================ V panic ================')
		eprint('   module: '); eprintln(mod)
		eprint(' function: '); eprint(fn_name); eprintln('()')
		eprint('  message: '); eprintln(s)
		eprint('     file: '); eprint(file); eprint(':');
	    C.fprintf(C.stderr, c'%d\n', line_no)
		eprint('   v hash: '); eprintln(vcurrent_hash())
		$if !vinix && !native {
			eprint('      pid: '); C.fprintf(C.stderr, c'%p\n', voidptr(v_getpid()))
			eprint('      tid: '); C.fprintf(C.stderr, c'%p\n', voidptr(v_gettid()))
		}
		eprintln('=========================================')
		flush_stdout()
		// vfmt on
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
	C.exit(1)
}

// panic_option_not_set is called by V, when you use option error propagation in your main function.
// It ends the program with a panic.
@[noreturn]
pub fn panic_option_not_set(s string) {
	panic('option not set (' + s + ')')
}

// panic_result_not_set is called by V, when you use result error propagation in your main function
// It ends the program with a panic.
@[noreturn]
pub fn panic_result_not_set(s string) {
	panic('result not set (' + s + ')')
}

// panic prints a nice error message, then exits the process with exit code of 1.
// It also shows a backtrace on most platforms.
@[noreturn]
pub fn panic(s string) {
	// Note: be careful to not use string interpolation here:
	$if freestanding {
		bare_panic(s)
	} $else {
		// vfmt off
		flush_stdout()
		eprint('V panic: ')
		eprintln(s)
		eprint(' v hash: ')
		eprintln(vcurrent_hash())
		$if !vinix && !native {
			eprint('    pid: '); C.fprintf(C.stderr, c'%p\n', voidptr(v_getpid()))
			eprint('    tid: '); C.fprintf(C.stderr, c'%p\n', voidptr(v_gettid()))
		}
		flush_stdout()
		// vfmt on
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
	C.exit(1)
}

// return a C-API error message matching to `errnum`
pub fn c_error_number_str(errnum int) string {
	mut err_msg := ''
	$if freestanding {
		err_msg = 'error ' + errnum.str()
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

// panic_n prints an error message, followed by the given number, then exits the process with exit code of 1.
@[noreturn]
pub fn panic_n(s string, number1 i64) {
	panic(s + impl_i64_to_string(number1))
}

// panic_n2 prints an error message, followed by the given numbers, then exits the process with exit code of 1.
@[noreturn]
pub fn panic_n2(s string, number1 i64, number2 i64) {
	panic(s + impl_i64_to_string(number1) + ', ' + impl_i64_to_string(number2))
}

// panic_n3 prints an error message, followed by the given numbers, then exits the process with exit code of 1.
@[noreturn]
fn panic_n3(s string, number1 i64, number2 i64, number3 i64) {
	panic(s + impl_i64_to_string(number1) + ', ' + impl_i64_to_string(number2) + ', ' +
		impl_i64_to_string(number3))
}

// panic with a C-API error message matching `errnum`
@[noreturn]
pub fn panic_error_number(basestr string, errnum int) {
	panic(basestr + c_error_number_str(errnum))
}
