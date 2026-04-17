module builtin

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
			nr_actual_frames := nr_ptrs - skipframes
			csymbols := C.backtrace_symbols(voidptr(&buffer[skipframes]), nr_actual_frames)
			atos_lines := bsd_backtrace_resolve_atos(&buffer[skipframes], nr_actual_frames)
			for i in 0 .. nr_actual_frames {
				sframe := unsafe { tos2(&u8(csymbols[i])) }
				mut file_line := ''
				if i < atos_lines.len {
					file_line = atos_lines[i]
				}
				// macOS format: `0   main   0x00000001047232f8 veb__run_T_main__App_main__Context + 356`
				symbol_start := sframe.index('0x') or { -1 }
				if symbol_start < 0 {
					continue
				}
				rest := sframe[symbol_start..]
				space_after_addr := rest.index(' ') or { -1 }
				if space_after_addr < 0 {
					continue
				}
				symbol_and_offset := rest[space_after_addr + 1..]
				plus_pos := symbol_and_offset.index(' + ') or { -1 }
				mut raw_symbol := symbol_and_offset
				if plus_pos >= 0 {
					raw_symbol = symbol_and_offset[..plus_pos]
				}
				// Skip C runtime frames that are not V functions:
				if raw_symbol in ['main', 'start', '_main'] {
					continue
				}
				mut demangled := ''
				if plus_pos >= 0 {
					demangled = demangle_v_symbol(raw_symbol) + symbol_and_offset[plus_pos..]
				} else {
					demangled = demangle_v_symbol(raw_symbol)
				}
				if file_line.len > 0 {
					eprint(file_line)
					eprint_space_padding(file_line, 45)
					eprint(' | ')
					eprintln(demangled)
				} else {
					eprintln(demangled)
				}
			}
			if nr_actual_frames > 0 {
				unsafe { C.free(csymbols) }
			}
		}
		return true
	}
}

// bsd_backtrace_resolve_atos uses macOS `atos` to resolve addresses to file:line info.
// Returns an array of file:line strings (one per frame). Empty string if unresolved.
@[direct_array_access]
fn bsd_backtrace_resolve_atos(buffer &voidptr, nr_frames int) []string {
	$if macos {
		exe_name := backtrace_current_executable_name()
		if exe_name.len == 0 {
			return []string{}
		}
		base_addr := voidptr(C._dyld_get_image_header(0))
		if base_addr == unsafe { nil } {
			return []string{}
		}
		// Build single atos command with all addresses for efficiency:
		mut cmd := 'atos --fullPath -o "' + exe_name + '" -l ' + ptr_str(base_addr)
		for i in 0 .. nr_frames {
			cmd += ' ' + ptr_str(unsafe { buffer[i] })
		}
		f := C.popen(&char(cmd.str), c'r')
		if f == unsafe { nil } {
			return []string{}
		}
		buf := [4096]u8{}
		mut lines := []string{cap: nr_frames}
		unsafe {
			bp := &u8(&buf[0])
			for C.fgets(&char(bp), 4096, f) != 0 {
				line := tos(bp, vstrlen(bp)).trim_chars(' \t\n\r', .trim_both)
				// atos output format: `func_name (in binary) (file.v:42)`
				// Extract the last parenthesized (file:line) part:
				paren_pos := line.index_last_('(')
				if paren_pos >= 0 {
					file_part := line[paren_pos + 1..]
					end_paren := file_part.index_last_(')')
					if end_paren >= 0 {
						file_line := file_part[..end_paren]
						if file_line.contains(':') && !file_line.starts_with('in ')
							&& !file_line.contains('.tmp.c:') {
							lines << file_line
							continue
						}
					}
				}
				lines << ''
			}
		}
		C.pclose(f)
		return lines
	}
	return []string{}
}

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

@[direct_array_access]
fn print_backtrace_skipping_top_frames_linux(skipframes int) bool {
	$if android {
		eprintln('On Android no backtrace is available.')
		return false
	}
	$if native {
		eprintln('native backend does not support backtraces yet.')
		return false
	} $else $if no_backtrace ? {
		return false
	} $else {
		$if linux && !freestanding {
			$if tinyc {
				C.tcc_backtrace(c'Backtrace')
				return false
			} $else {
				$if !glibc {
					eprintln('backtrace_symbols is missing => printing backtraces is not available.')
					eprintln('Some libc implementations like musl simply do not provide it.')
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
					for i in 0 .. nr_actual_frames {
						sframe := unsafe { tos2(&u8(csymbols[i])) }
						executable := sframe.all_before('(')
						addr2line_executable := backtrace_addr2line_executable(executable,
							current_executable_name)
						addr := sframe.all_after('[').all_before(']')
						beforeaddr := sframe.all_before('[')
						cmd := 'addr2line -e ' + addr2line_executable + ' ' + addr
						// taken from os, to avoid depending on the os module inside builtin.v
						f := C.popen(&char(cmd.str), c'r')
						if f == unsafe { nil } {
							eprintln(sframe)
							continue
						}
						buf := [1000]u8{}
						mut output := ''
						unsafe {
							bp := &u8(&buf[0])
							for C.fgets(&char(bp), 1000, f) != 0 {
								output += tos(bp, vstrlen(bp))
							}
						}
						output = output.trim_chars(' \t\n', .trim_both) + ':'
						if C.pclose(f) != 0 {
							eprintln(sframe)
							continue
						}
						if output in ['??:0:', '??:?:'] {
							output = ''
						}
						// See http://wiki.dwarfstd.org/index.php?title=Path_Discriminators
						// Note: it is shortened here to just d. , just so that it fits, and so
						// that the common error file:lineno: line format is enforced.
						output = output.replace(' (discriminator', ': (d.')
						eprint(output)
						eprint_space_padding(output, 55)
						eprint(' | ')
						eprint(addr)
						eprint(' | ')
						eprintln(demangle_backtrace_sym(beforeaddr))
					}
					if nr_actual_frames > 0 {
						unsafe { C.free(csymbols) }
					}
				}
			}
		}
	}
	return true
}
