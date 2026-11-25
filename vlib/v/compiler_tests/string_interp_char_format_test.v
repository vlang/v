import os
import v.tempname

fn test_character_interpolation_with_static_width() {
	dir := os.join_path(os.vtmp_dir(), 'v3_string_interp_char_format_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source_file := os.join_path(dir, 'main.v')
	os.write_file(source_file, "fn main() {
	println('|\${u8(`f`):1c}|')
	println('|\${u8(`f`):3c}|')
	println('|\${u8(`f`):-3c}|')
}
") or {
		panic(err)
	}
	bin := os.join_path(dir, 'app')
	compile := os.exec([@VEXE, '-nocache', source_file, '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '|f|\n|  f|\n|f  |'
}
