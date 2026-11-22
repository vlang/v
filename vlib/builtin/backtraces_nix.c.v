module builtin

// backtrace_exec_capture avoids a shell while keeping builtin independent of os.
// It does not reuse os' C capture helpers: every program links builtin, and the
// module cache can not split a static C helper between builtin and os objects.
@[unsafe]
fn backtrace_exec_capture(args []string) (string, int) {
	mut cargs := []&char{cap: args.len + 1}
	for arg in args {
		cargs << &char(arg.str)
	}
	// The C argument vector must end with a null pointer.
	cargs << &char(unsafe { nil })
	mut pipefd := [2]i32{}
	if C.pipe(&pipefd[0]) != 0 {
		return '', -1
	}
	// Children that other threads start meanwhile must not inherit the pipe;
	// an open write end would keep the reader below from seeing EOF.
	C.fcntl(pipefd[0], C.F_SETFD, C.FD_CLOEXEC)
	C.fcntl(pipefd[1], C.F_SETFD, C.FD_CLOEXEC)
	pid := C.fork()
	if pid < 0 {
		C.close(pipefd[0])
		C.close(pipefd[1])
		return '', -1
	}
	if pid == 0 {
		// The duplicated stdout/stderr descriptors stay open across exec.
		C.dup2(pipefd[1], 1)
		C.dup2(pipefd[1], 2)
		C.execvp(cargs[0], unsafe { &&char(cargs.data) })
		C._exit(127)
	}
	C.close(pipefd[1])
	fd := pipefd[0]
	mut output := ''
	mut buf := [4096]u8{}
	for {
		n := C.read(fd, &buf[0], uint(buf.len))
		if n > 0 {
			// read returned n initialized bytes in the stack buffer.
			output += unsafe { tos(&buf[0], int(n)) }
		} else if n == 0 || C.errno != C.EINTR {
			break
		}
	}
	C.close(fd)
	mut status := 0
	for C.waitpid(pid, &status, 0) == -1 {
		if C.errno != C.EINTR {
			return output, -1
		}
	}
	return output, status
}

// print_backtrace_skipping_top_frames prints the backtrace skipping N top frames.
pub fn print_backtrace_skipping_top_frames(xskipframes int) bool {
	$if no_backtrace ? {
		return false
	} $else {
		skipframes := xskipframes + 2
		$if macos || freebsd || openbsd || netbsd {
			return print_backtrace_skipping_top_frames_bsd(skipframes)
		} $else $if linux {
			return print_backtrace_skipping_top_frames_linux(skipframes)
		} $else {
			println('print_backtrace_skipping_top_frames is not implemented. skipframes: ${skipframes}')
		}
	}
	return false
}

// the functions below are not called outside this file,
// so there is no need to have their twins in builtin_windows.v
@[direct_array_access]
fn print_backtrace_skipping_top_frames_bsd(skipframes int) bool {
	$if no_backtrace ? {
		return false
	} $else {
		$if macos || freebsd || netbsd {
			buffer := [100]voidptr{}
			nr_ptrs := C.backtrace(&buffer[0], 100)
			if nr_ptrs < 2 {
				eprintln('C.backtrace returned less than 2 frames')
				return false
			}
			$if macos {
				nr_actual_frames := nr_ptrs - skipframes
				if nr_actual_frames <= 0 {
					return false
				}
				csymbols := C.backtrace_symbols(voidptr(&buffer[skipframes]), nr_actual_frames)
				// Only a binary with debug info (`-g`) can map a frame to its .v line,
				// so do not start `atos` for one that has none.
				mut atos_symbols := []string{}
				mut file_lines := []string{}
				$if debug || detailed_backtraces ? {
					atos_symbols, file_lines = bsd_backtrace_resolve_atos(&buffer[skipframes],
						nr_actual_frames)
				}
				mut has_file_lines := false
				eprintln('')
				for i in 0 .. nr_actual_frames {
					sframe := unsafe { tos2(&u8(csymbols[i])) }
					// macOS format: `0   main   0x00000001047232f8 veb__run_T_main__App_main__Context + 356`
					image, symbol := bsd_backtrace_frame_image_and_symbol(sframe)
					// Skip the loader, libc and the frames of the garbage collector, which
					// start the threads of a V program.
					if symbol.len > 0 && image != 'dyld' && !image.starts_with('libsystem_')
						&& !symbol.starts_with('GC_') {
						mut file_line := if i < file_lines.len { file_lines[i] } else { '' }
						if backtrace_file_line_is_generated(file_line) {
							file_line = ''
						}
						if file_line.len > 0 {
							has_file_lines = true
						}
						eprint_backtrace_frame(if i < atos_symbols.len && atos_symbols[i].len > 0 {
							atos_symbols[i]
						} else {
							symbol
						}, file_line)
					}
				}
				if !has_file_lines {
					$if !debug {
						eprintln('(build with `-g` to see the .v line of every frame)')
					} $else $if tinyc {
						// `atos` can not read the debug info that TinyCC writes.
						eprintln('(build with `-cc clang -g` to see the .v line of every frame)')
					}
				}
				unsafe { C.free(csymbols) }
			} $else {
				C.backtrace_symbols_fd(&buffer[skipframes], nr_ptrs - skipframes, 2)
			}
		}
		return true
	}
}

// bsd_backtrace_frame_image_and_symbol splits a macOS `backtrace_symbols` line like
// `0   main   0x00000001047232f8 veb__run_T_main__App_main__Context + 356`
// into its image (`main`) and its C symbol (`veb__run_T_main__App_main__Context`).
fn bsd_backtrace_frame_image_and_symbol(sframe string) (string, string) {
	// The image name can contain spaces, so split around the address instead of
	// splitting all fields.
	addr_start := sframe.index(' 0x') or { return '', '' }
	// An ASCII `trim_chars`, like the other frame parsers in this file, keeps `trim`
	// and its rune path out of the backtrace code.
	number_and_image := sframe[..addr_start].trim_chars(' \t', .trim_both)
	number_end := number_and_image.index(' ') or { return '', '' }
	image := number_and_image[number_end..].trim_chars(' \t', .trim_both)
	addr_and_symbol := sframe[addr_start + 1..]
	symbol_start := addr_and_symbol.index(' ') or { return image, '' }
	symbol_and_offset := addr_and_symbol[symbol_start + 1..]
	return image, symbol_and_offset.all_before(' + ')
}

// bsd_backtrace_resolve_atos uses macOS `atos` to resolve addresses to file:line info.
// It returns the C symbol and the file:line of each frame, or empty strings for
// frames it could not resolve.
@[direct_array_access; unsafe]
fn bsd_backtrace_resolve_atos(buffer &voidptr, nr_frames int) ([]string, []string) {
	$if macos {
		exe_name := backtrace_current_executable_name()
		if exe_name.len == 0 {
			return []string{}, []string{}
		}
		base_addr := voidptr(C._dyld_get_image_header(0))
		if base_addr == unsafe { nil } {
			return []string{}, []string{}
		}
		// Build single atos command with all addresses for efficiency:
		mut args := ['atos', '--fullPath', '-o', exe_name, '-l', ptr_str(base_addr)]
		for i in 0 .. nr_frames {
			// The frames hold return addresses, which can belong to the line after a
			// call (always after a call to a `@[noreturn]` function like `panic`).
			// Look up the call instruction itself.
			addr := u64(unsafe { buffer[i] })
			args << ptr_str(voidptr(if addr > 0 { addr - 1 } else { addr }))
		}
		output, status := backtrace_exec_capture(args)
		if status != 0 {
			return []string{}, []string{}
		}
		mut symbols := []string{cap: nr_frames}
		mut lines := []string{cap: nr_frames}
		for output_line in output.split_into_lines() {
			line := output_line.trim_chars(' \t\n\r', .trim_both)
			// Unlike `backtrace_symbols`, which only sees exported symbols, `atos`
			// also names static functions.
			symbols << if line.contains(' (in ') { line.all_before(' (in ') } else { '' }
			lines << atos_output_file_line(line)
		}
		return symbols, lines
	}
	return []string{}, []string{}
}

// atos_output_file_line returns the `file:line` of an `atos` output line like
// `func_name (in binary) (file.v:42)`, or '' when atos did not resolve one.
fn atos_output_file_line(line string) string {
	paren_pos := line.index_last_('(')
	if paren_pos < 0 {
		return ''
	}
	file_part := line[paren_pos + 1..]
	end_paren := file_part.index_last_(')')
	if end_paren < 0 {
		return ''
	}
	file_line := file_part[..end_paren]
	if file_line.contains(':') && !file_line.starts_with('in ') {
		return file_line
	}
	return ''
}

// backtrace_file_line_is_generated reports whether a resolved `file:line` points
// into C code that V generated, rather than into a .v (or user C) source file.
fn backtrace_file_line_is_generated(file_line string) bool {
	return file_line.starts_with('<generated>') || file_line.contains('.tmp.c:')
		|| file_line.contains('.v3cc.')
}

@[c_extern]
fn C.tcc_backtrace(fmt &char) i32

fn backtrace_current_executable_name() string {
	args := arguments()
	if args.len == 0 {
		return ''
	}
	return args[0]
}

fn backtrace_addr2line_executable(executable string, current_executable_name string) string {
	if executable.len == 0 {
		return '/proc/self/exe'
	}
	if executable.contains('/') {
		return executable
	}
	if current_executable_name.len > 0
		&& executable.all_after_last('/') == current_executable_name.all_after_last('/') {
		return '/proc/self/exe'
	}
	return executable
}

fn backtrace_shell_quote(s string) string {
	mut quoted := "'"
	for i in 0 .. s.len {
		if s[i] == `'` {
			quoted += "'\\''"
		} else {
			quoted += s[i].ascii_str()
		}
	}
	return quoted + "'"
}

@[direct_array_access]
fn print_backtrace_skipping_top_frames_linux(skipframes int) bool {
	$if android {
		eprintln('On Android no backtrace is available.')
		return false
	}
	$if no_backtrace ? {
		return false
	} $else {
		$if linux && !freestanding {
			$if !glibc || (tinyc && arm64) {
				$if tinyc {
					C.tcc_backtrace(c'Backtrace')
				} $else {
					eprintln('backtrace_symbols is missing => printing backtraces is not available.')
					eprintln('Some libc implementations like musl simply do not provide it.')
				}
				return false
			} $else {
				current_executable_name := backtrace_current_executable_name()
				buffer := [100]voidptr{}
				nr_ptrs := C.backtrace(&buffer[0], 100)
				if nr_ptrs < 2 {
					eprintln('C.backtrace returned less than 2 frames')
					return false
				}
				nr_actual_frames := nr_ptrs - skipframes
				//////csymbols := backtrace_symbols(*voidptr(&buffer[skipframes]), nr_actual_frames)
				csymbols := C.backtrace_symbols(voidptr(&buffer[skipframes]), nr_actual_frames)
				eprintln('')
				for i in 0 .. nr_actual_frames {
					// Linux format: `./executable(symbol_name+0x1a4) [0x55d1c5a9b1a4]`
					sframe := unsafe { tos2(&u8(csymbols[i])) }
					executable := sframe.all_before('(')
					addr2line_executable := backtrace_addr2line_executable(executable,
						current_executable_name)
					// The frames hold return addresses, which can belong to the line after
					// a call (always after a call to a `@[noreturn]` function like `panic`).
					// Look up the call instruction itself.
					addr := u64(unsafe { buffer[skipframes + i] })
					text, status := backtrace_exec_capture(['addr2line', '-e', addr2line_executable,
						ptr_str(voidptr(if addr > 0 { addr - 1 } else { addr }))])
					mut file_line := ''
					if status == 0 {
						file_line = text.trim_chars(' \t\n', .trim_both).all_before(' (discriminator')
						if file_line.starts_with('??') || backtrace_file_line_is_generated(file_line) {
							file_line = ''
						}
					}
					symbol := if sframe.contains('(') {
						sframe.all_after('(').all_before(')').all_before('+')
					} else {
						''
					}
					if symbol.len == 0 {
						eprintln(sframe)
						if file_line.len > 0 {
							eprint('\t')
							eprintln(file_line)
						}
						continue
					}
					eprint_backtrace_frame(symbol, file_line)
				}
				if nr_actual_frames > 0 {
					unsafe { C.free(csymbols) }
				}
			}
		}
	}
	return true
}
