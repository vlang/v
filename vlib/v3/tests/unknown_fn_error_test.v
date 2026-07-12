import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// test_unknown_function_stops_in_type_checker validates this v3 regression case.
fn test_unknown_function_stops_in_type_checker() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_unknown_fn_error_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0

	bad_src := os.join_path(os.temp_dir(), 'v3_unknown_fn_error_input.v')
	os.write_file(bad_src, 'fn main() {\n\tx := missing_fn()\n\tprintln(x)\n}\n')!
	bad_bin := os.join_path(os.temp_dir(), 'v3_unknown_fn_error_input')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0
	assert result.output.contains('unknown function `missing_fn`')
	assert !result.output.contains('C compilation failed')
}
