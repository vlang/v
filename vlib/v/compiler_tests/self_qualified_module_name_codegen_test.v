import os
import v.cmdexec
import v.tempname

fn selfqual_write_module(mod_dir string, module_lines []string, test_lines []string) {
	os.mkdir(mod_dir) or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'lib.v'), module_lines.join('\n') + '\n') or { panic(err) }
	os.write_file(os.join_path(mod_dir, 'lib_test.v'), test_lines.join('\n') + '\n') or { panic(err) }
}

fn selfqual_run_v(args []string) os.Result {
	// The outer runner's filters select this test, while the child must run its fixture.
	return cmdexec.run_in_merged_with_env(@VEXE, args, '', {
		'VTEST_ONLY':    ''
		'VTEST_ONLY_FN': ''
	})
}

// A file can qualify a symbol with the name of its own module (`m.greeting` written inside
// `m`). No import statement records that module, so the name was not recognized as a
// module base and the qualified name was written into the C output verbatim, where the C
// compiler reads the module name as an undeclared identifier. The qualified name is read
// from a test file of the module here, which is the case that reached the C output.
fn test_self_qualified_constant_name_is_generated_as_a_c_name() {
	root := os.join_path(os.vtmp_dir(), 'v3 selfqual ${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	mod := 'selfqual'
	mod_dir := os.join_path(root, mod)
	selfqual_write_module(mod_dir, [
		'module ${mod}',
		'',
		"pub const greeting = 'hi'",
	], [
		'module ${mod}',
		'',
		'fn test_qualified_const() {',
		'\tprintln(${mod}.greeting)',
		'\tprintln(${mod}.greeting.len)',
		'}',
	])
	test_out := selfqual_run_v(['-gc', 'none', 'test', mod_dir])
	assert test_out.exit_code == 0, test_out.output
	assert test_out.output.contains('1 passed'), test_out.output
	c_path := os.join_path(root, 'self qualified module name.c')
	generate := selfqual_run_v(['-o', c_path, os.join_path(mod_dir, 'lib_test.v')])
	assert generate.exit_code == 0, generate.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('println(${mod}__greeting);')
	// The qualified string still reaches C, while its static length folds to a literal.
	assert generated.contains('println(int__str(2));')
	assert !generated.contains('${mod}__greeting.len')
	run_path := os.join_path(root, 'self qualified module name run' + $if windows {
		'.exe'
	} $else {
		''
	})
	compile := selfqual_run_v(['-gc', 'none', '-o', run_path, os.join_path(mod_dir, 'lib_test.v')])
	assert compile.exit_code == 0, compile.output
	run := os.exec([run_path])
	assert run.exit_code == 0, run.output
	assert run.output.replace('\r\n', '\n') == 'hi\n2\n', run.output
}
