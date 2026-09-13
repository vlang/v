import os

const selfqual_vexe = @VEXE
const selfqual_tests_dir = os.dir(@FILE)
const selfqual_v3_dir = os.dir(selfqual_tests_dir)
const selfqual_vlib_dir = os.dir(selfqual_v3_dir)
const selfqual_v3_src = os.join_path(selfqual_v3_dir, 'v.v')

fn selfqual_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_self_qualified_module_name_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${selfqual_vexe} -gc none -path "${selfqual_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${selfqual_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn selfqual_write_module(name string, module_lines []string, test_lines []string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}')
	mod_dir := os.join_path(root, name)
	os.rmdir_all(root) or {}
	os.mkdir_all(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'lib.v'), module_lines.join('\n') + '\n') or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'lib_test.v'), test_lines.join('\n') + '\n') or { panic(err) }
	return root
}

// The compiler of the tree under test is built from source, and the fallback compiler is
// disabled while the fixture is built: a failed build would otherwise be retried with the
// fallback compiler, which hides the defect this test exists to catch.
fn selfqual_disable_fallback() {
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	os.setenv('V_C_ERROR_BUG_REPORT_DISABLED', '1', true)
}

// A file can qualify a symbol with the name of its own module (`m.greeting` written inside
// `m`). No import statement records that module, so the name was not recognized as a
// module base and the qualified name was written into the C output verbatim, where the C
// compiler reads the module name as an undeclared identifier. The qualified name is read
// from a test file of the module here, which is the case that reached the C output.
fn test_self_qualified_constant_name_is_generated_as_a_c_name() {
	v3_bin := selfqual_build_v3()
	mod := 'selfqual'
	root := selfqual_write_module(mod, [
		'module ${mod}',
		'',
		"pub const greeting = 'hi'",
	], [
		'module ${mod}',
		'',
		'fn test_qualified_const() {',
		'\tprintln(${mod}.greeting.len)',
		'}',
	])
	mod_dir := os.join_path(root, mod)
	selfqual_disable_fallback()
	test_out := os.execute('${v3_bin} test ${os.quoted_path(mod_dir)}')
	assert test_out.exit_code == 0, test_out.output
	assert test_out.output.contains('1 passed'), test_out.output
	c_path := os.join_path(os.temp_dir(), 'v3_self_qualified_module_name.c')
	generate := os.execute('${v3_bin} -o ${c_path} ${os.quoted_path(os.join_path(mod_dir, 'lib_test.v'))}')
	assert generate.exit_code == 0, generate.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('${mod}__greeting.len')
}
