module eval

import os

fn run_eval_strconv_program(code string) !os.Result {
	path := os.join_path(os.vtmp_dir(), 'eval_strconv_${os.getpid()}.v')
	os.write_file(path, code)!
	defer { os.rm(path) or {} }
	return os.exec([@VEXE, '-b', 'eval', path])
}

fn test_eval_signed_integer_parsing_from_string_loop() ! {
	path := os.join_path(os.vtmp_dir(), 'eval_strconv_loop_${os.getpid()}.v')
	exe := path.all_before_last('.')
	os.write_file(path, 'fn main() {
	for s in ["0", "7", "-7", "+7", "127", "128", "-128", "-129", "32767", "32768",
		"-32768", "-32769", "2147483647", "2147483648", "-2147483648", "-2147483649",
		"3000000000", "9223372036854775807", "9223372036854775808", "-9223372036854775808",
		"-9223372036854775809", "18446744073709551615", "18446744073709551616", "0x7f",
		"0x80", "-0x80", "-0x81", "0x7fffffffffffffff", "0x8000000000000000",
		"-0x8000000000000000", "-0x8000000000000001", "0b1010", "0o17", "017", "1_000",
		"_1", "1__0", "1.5", "12 ", "x", "-", ""] {
		println(s)
		println(s.i8())
		println(s.i16())
		println(s.i32())
		println(s.int())
		println(s.i64())
	}
}
')!
	defer {
		os.rm(path) or {}
		os.rm(exe) or {}
	}
	compiled := os.exec([@VEXE, '-b', 'c', '-o', exe, path])
	assert compiled.exit_code == 0, compiled.output
	c := os.exec([exe])
	assert c.exit_code == 0, c.output
	evaluated := os.exec([@VEXE, '-b', 'eval', path])
	assert evaluated.exit_code == 0, evaluated.output
	assert evaluated.output == c.output
}

fn test_eval_signed_integer_parsing() ! {
	result := run_eval_strconv_program('import strconv

fn main() {
	assert "7".i64() == i64(7)
	assert strconv.parse_int("7", 0, 0)! == i64(7)
	assert strconv.parse_int("-0x8000000000000000", 0, 64)! == min_i64
	assert strconv.parse_int("0", 0, 64)! == i64(0)
	assert strconv.parse_int("9223372036854775808", 0, 64)! == max_i64
	invalid := strconv.parse_int("x", 0, 64) or { i64(-1) }
	assert invalid == -1
	assert strconv.parse_int("7", 10, 64)! == i64(7)
	assert strconv.parse_int("-128", 10, 8)! == i64(-128)
	assert strconv.parse_int("0x_f", 0, 0)! == i64(15)
	underscored := strconv.parse_int("1_000", 10, 64) or { i64(-1) }
	assert underscored == -1
	println("parsed")
}
')!
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'parsed'
}
