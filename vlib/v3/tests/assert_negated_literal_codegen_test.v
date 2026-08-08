import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// A signed literal missed assert's literal exemption and was captured as the
// default `int`, truncating the comparison itself, not just the failure message.
fn test_assert_negated_wide_literal_is_not_captured_as_int() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_codegen_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_input.v')
	out_c := os.join_path(os.temp_dir(), 'v3_assert_negated_literal_input.c')
	os.write_file(src,
		'fn get() i64 {\n\treturn i64(-123456789012345)\n}\n\nfn main() {\n\tassert get() == -123456789012345\n}\n')!
	result := os.execute('${v3_bin} ${src} -o ${out_c}')
	assert result.exit_code == 0, result.output

	// the emitted failure detail repeats the assert source, so only the generated
	// condition proves the literal is compared inline rather than through a temp
	lines := os.read_file(out_c)!.split_into_lines()
	conditions := lines.filter(
		it.trim_space().starts_with('if (!(') && it.contains('123456789012345'))
	assert conditions.len == 1, 'expected one assert condition holding the literal, got ${conditions.len}'
	assert conditions[0].contains('== -123456789012345'), 'literal not compared inline: ${conditions[0]}'
	assert !lines.any(it.trim_space().starts_with('int _t') && it.contains('123456789012345')), 'wide literal initialized into an int temp'
}
