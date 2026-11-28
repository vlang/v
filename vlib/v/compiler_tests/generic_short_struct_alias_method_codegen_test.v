import os
import v.cmdexec
import v.tempname

fn test_generic_short_struct_and_alias_method_compile_without_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_generic_compat_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	vlib_dir := os.dir(os.dir(os.dir(@FILE)))
	fixture_env := {
		'VJOBS': '1'
	}
	fixtures := [
		'generic_fn_short_syntax_struct_param_test.v',
		'generic_fn_type_generic_method_test.v',
	]
	for fixture_name in fixtures {
		fixture := os.join_path(vlib_dir, 'v/tests/generics', fixture_name)
		output := os.join_path(root, fixture_name.all_before_last('.'))
		compile := cmdexec.run_in_merged_with_env(@VEXE, ['-gc', 'none', '-no-parallel', '-o',
			output, fixture], '', fixture_env)
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run_in_merged_with_env(output, [], '', fixture_env)
		assert run.exit_code == 0, run.output
	}
}
