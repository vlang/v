module builtin

const prompt = 'vdbg>'

fn C.getline(voidptr, voidptr, voidptr) int

fn C.ferror(stream &C.FILE) int

fn C.feof(stream &C.FILE) int

// in most systems, these are __quad_t, which is an i64
pub struct C.timespec {
mut:
	tv_sec  i64
	tv_nsec i64
}

// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
fn C.clock_gettime(int, &C.timespec)

fn is_atty(fd int) int {
	$if windows {
		mut mode := u32(0)
		osfh := voidptr(C._get_osfhandle(fd))
		C.GetConsoleMode(osfh, voidptr(&mode))
		return int(mode)
	} $else {
		return C.isatty(fd)
	}
}

fn get_raw_line() string {
	$if windows {
		unsafe {
			max_line_chars := 256
			buf := malloc_noscan(max_line_chars * 2)
			h_input := C.GetStdHandle(C.STD_INPUT_HANDLE)
			mut bytes_read := u32(0)
			if is_atty(0) > 0 {
				x := C.ReadConsole(h_input, buf, max_line_chars * 2, voidptr(&bytes_read),
					0)
				if !x {
					return tos(buf, 0)
				}
				return string_from_wide2(&u16(buf), int(bytes_read))
			}
			mut offset := 0
			for {
				pos := buf + offset
				res := C.ReadFile(h_input, pos, 1, voidptr(&bytes_read), 0)
				if !res && offset == 0 {
					return tos(buf, 0)
				}
				if !res || bytes_read == 0 {
					break
				}
				if *pos == `\n` {
					offset++
					break
				}
				offset++
			}
			return buf.vstring_with_len(offset)
		}
	} $else {
		max := usize(0)
		buf := &char(0)
		nr_chars := unsafe { C.getline(&buf, &max, C.stdin) }
		str := unsafe { tos(&u8(buf), if nr_chars < 0 { 0 } else { nr_chars }) }
		ret := str.clone()
		unsafe {
			if nr_chars > 0 && buf != 0 {
				C.free(buf)
			}
		}
		return ret
	}
}

fn prompt(out string) {
	println('${prompt} ${out}')
}

fn min[T](a T, b T) T {
	return if a < b { a } else { b }
}

fn max[T](a T, b T) T {
	return if a > b { a } else { b }
}

@[manualfree]
fn print_current_file(path string, line int) ! {
	mut fp := unsafe { nil }
	mode := 'rb'
	$if windows {
		fp = C._wfopen(path.to_wide(), mode.to_wide())
	} $else {
		fp = C.fopen(&char(path.str), &char(mode.str))
	}
	defer {
		C.fclose(fp)
	}

	C.fseek(fp, 0, C.SEEK_END)
	raw_fsize := C.ftell(fp)
	C.rewind(fp)
	allocate := int(raw_fsize)

	unsafe {
		mut str := malloc_noscan(allocate + 1)
		nelements := int(C.fread(str, 1, allocate, fp))
		is_eof := int(C.feof(fp))
		is_error := int(C.ferror(fp))
		if is_eof == 0 && is_error != 0 {
			free(str)
			return error('fread failed')
		}
		str[nelements] = 0
		file_content := if nelements == 0 {
			str.vstring()
		} else {
			str.vstring_with_len(nelements)
		}
		chunks := file_content.split('\n')
		offset := max(line - 4, 1)
		for n, s in chunks[max(line - 5, 0)..min(chunks.len, line)] {
			println('${n + offset:04} ${s}')
		}
	}
}

fn print_help() {
	println('vdbg commands: ')
	println('  bt\t\t\tprints a backtrace')
	println('  c, continue\t\tcontinue debugging')
	println('  f,file\t\tshow current file name')
	println('  fn,func\t\tshow current function name')
	println('  h, help\t\tshow this help')
	println('  list\t\t\tshow 5 lines from current file')
	println('  l, line\t\tshow current line number')
	println('  m,mod\t\t\tshow current module name')
	println('  q,quit\t\texits debugging session in the code')
	println('  s,profile\t\tstart CPU profiling session')
	println('  e,profileEnd\t\tstop current CPU profiling session')
	println('')
}

fn sys_mono_now() u64 {
	$if macos {
		return sys_mono_now_darwin()
	} $else {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	}
}

fn elapsed_time(d u64) string {
	ms := d / 1_000_000
	sec := d / 1_000_000_000
	us := d / 1000
	if sec > 0 {
		return '${sec}.${ms:03}s'
	}
	return '${ms}.${us:03}ms'
}

@[unsafe]
fn v__debugger(line_no int, file string, mod string, fn_name string) ! {
	mut static profile := u64(0)
	mut static exited := 0
	if exited != 0 {
		return
	}

	println('debugger at ${file}:${line_no} - ctx: ${mod}:${fn_name}')

	for {
		print('${prompt} ')
		cmd := get_raw_line().trim('\r\n')
		match cmd {
			'bt' {
				print_backtrace_skipping_top_frames(2)
			}
			'', 'c', 'continue' {
				break
			}
			'f', 'file' {
				println(file)
			}
			'fn', 'func' {
				println(fn_name)
			}
			'h', 'help' {
				print_help()
			}
			'list' {
				print_current_file(file, line_no)!
			}
			'l', 'line' {
				println(line_no.str())
			}
			'm', 'mod' {
				println(mod)
			}
			's', 'profile' {
				profile = sys_mono_now()
				println('profiler :: starting profiler')
			}
			'e', 'profileEnd' {
				println('profiler :: elapsed time: ${elapsed_time(sys_mono_now() - profile)}')
			}
			'q', 'quit' {
				exited = 1
				break
			}
			else {
				println('unknown command `${cmd}`')
			}
		}
	}
}
