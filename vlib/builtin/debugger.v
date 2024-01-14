module builtin

const prompt = 'vdbg>'

fn C.getline(voidptr, voidptr, voidptr) int

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

fn print_help() {
	println('vdbg commands: ')
	println('  c, continue\t\tcontinue debugging')
	println('  l, line\t\tshow current line number')
	println('  f,file\t\tshow current file name')
	println('  fn,func\t\tshow current function name')
	println('  m,mod\t\tshow current module name')
	println('  h, help\t\tshow this help')
	println('')
}

fn v__debugger(line_no int, file string, mod string, fn_name string) {
	println('debugger at ${file}:${line_no} - ctx: ${mod}:${fn_name}')

	for {
		print('${prompt} ')
		cmd := get_raw_line().trim('\r\n')
		match cmd {
			'', 'c', 'continue' {
				break
			}
			'bt' {
				print_backtrace_skipping_top_frames(2)
			}
			'fn', 'func' {
				println(fn_name)
			}
			'f', 'file' {
				println(file)
			}
			'h', 'help' {
				print_help()
			}
			'l', 'line' {
				println(line_no.str())
			}
			'm', 'mod' {
				println(mod)
			}
			else {
				println('unknown command `${cmd}`')
			}
		}
	}
}
