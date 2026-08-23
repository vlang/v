import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_character_interpolation_with_static_width() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_string_interp_char_format_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	source_file := os.join_path(os.temp_dir(), 'v3_string_interp_char_format_input.v')
	os.write_file(source_file, "fn main() {
	println('|\${u8(`f`):1c}|')
	println('|\${u8(`f`):3c}|')
	println('|\${u8(`f`):-3c}|')
}
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_string_interp_char_format_input')
	compile := os.execute('${v3_bin} -nocache ${source_file} -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '|f|\n|  f|\n|f  |'
}
