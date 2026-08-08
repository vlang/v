import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// a signed wide literal captured as the default `int` truncated the assert comparison itself
fn test_assert_negated_wide_literal_is_not_captured_as_int() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_codegen_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_input.v')
	out_c := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_input.c')
	os.write_file(src,
		'fn get() i64 {\n\treturn i64(-123456789012345)\n}\n\nfn main() {\n\tassert get() == -123456789012345\n\tassert get() == 5 * -24691357802469\n}\n')!
	result := os.execute('${v3_bin} ${src} -o ${out_c}')
	assert result.exit_code == 0, result.output

	// the failure detail repeats the assert source, so only the condition line is proof
	lines := os.read_file(out_c)!.split_into_lines()
	conditions := lines.filter(
		it.trim_space().starts_with('if (!(') && (it.contains('123456789012345') || it.contains('24691357802469')))
	assert conditions.len == 2, 'expected two assert conditions holding the constants, got ${conditions.len}'
	assert conditions[0].contains('== -123456789012345'), 'literal not compared inline: ${conditions[0]}'
	assert conditions[1].contains('== (5 * -24691357802469)'), 'constant expr not compared inline: ${conditions[1]}'
	assert !lines.any(it.trim_space().starts_with('int _t')
		&& (it.contains('123456789012345') || it.contains('24691357802469'))), 'wide constant initialized into an int temp'
}
