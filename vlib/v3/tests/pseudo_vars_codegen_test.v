import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// test_at_mod_codegen validates that v3 lowers @MOD to the current module name.
fn test_at_mod_codegen() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_pseudo_vars_codegen_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	main_src := os.join_path(os.temp_dir(), 'v3_at_mod_main.v')
	main_bin := os.join_path(os.temp_dir(), 'v3_at_mod_main')
	os.write_file(main_src, "fn main() {\n\tassert @MOD == 'main'\n}\n")!
	main_result := os.execute('${v3_bin} ${main_src} -o ${main_bin}')
	assert main_result.exit_code == 0, main_result.output

	module_test_src := os.join_path(os.temp_dir(), 'v3_at_mod_module_test.v')
	module_test_bin := os.join_path(os.temp_dir(), 'v3_at_mod_module_test')
	os.write_file(module_test_src,
		"module sample\n\nfn test_at_mod() {\n\tassert @MOD == 'sample'\n}\n")!
	module_test_result := os.execute('${v3_bin} ${module_test_src} -o ${module_test_bin}')
	assert module_test_result.exit_code == 0, module_test_result.output
}
