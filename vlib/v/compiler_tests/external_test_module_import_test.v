import os
import v.cmdexec

// An external test module (`module foo_test` in `foo/`) is parsed from the same
// directory as the module it tests, so that directory was already recorded
// under the test module's identity. Reusing it rewrote `import foo` to
// `foo_test`, and the checker then rejected the import as naming the current
// module.
fn test_external_test_module_can_import_the_module_it_tests() {
	root := os.join_path(os.vtmp_dir(), 'v3_external_test_module_${os.getpid()}')
	module_dir := os.join_path(root, 'foo')
	os.mkdir_all(module_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(module_dir, 'foo.v'), "module foo\n\npub fn greet() string {\n\treturn 'hi'\n}\n")!
	os.write_file(os.join_path(module_dir, 'foo_test.v'), "module foo_test\n\nimport foo\n\nfn test_greet() {\n\tassert foo.greet() == 'hi'\n}\n")!
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	res := cmdexec.run_with_timeout(vexe, ['test', module_dir], 180_000)
	assert !res.output.contains('cannot import `foo` into a module with the same name'), res.output
	assert res.exit_code == 0, res.output
}

// The fix above must not weaken the diagnostic for a module that really does
// import itself.
fn test_a_module_importing_itself_is_still_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_self_import_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'foo.v')
	os.write_file(source, 'module foo\n\nimport foo\n\nfn main() {}\n')!
	vexe := os.join_path(@VMODROOT, 'v' + $if windows { '.exe' } $else { '' })
	res := cmdexec.run_with_timeout(vexe, ['-check', source], 120_000)
	assert res.exit_code != 0, res.output
	assert res.output.contains('cannot import `foo` into a module with the same name'), res.output
}
