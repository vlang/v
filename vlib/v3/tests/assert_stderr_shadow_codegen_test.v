import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// test_assert_with_stderr_local_codegen validates assert emission when a local
// variable would shadow platform stderr macros in generated C.
fn test_assert_with_stderr_local_codegen() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_assert_stderr_shadow_codegen_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_assert_stderr_shadow_input.v')
	bin := os.join_path(os.temp_dir(), 'v3_assert_stderr_shadow_input')
	os.write_file(src, "fn main() {\n\tstderr := ''\n\tassert stderr == ''\n}\n")!
	result := os.execute('${v3_bin} ${src} -o ${bin}')
	assert result.exit_code == 0, result.output
}
