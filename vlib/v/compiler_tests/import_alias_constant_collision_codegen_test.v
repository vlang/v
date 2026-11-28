import os
import v.cmdexec
import v.tempname

fn test_import_alias_wins_over_same_named_exported_constant_in_call_selector() {
	root := os.join_path(os.vtmp_dir(), 'v3_import_alias_collision_input_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	// A const named like its own module is rejected for a top-level module
	// (`duplicate of a module name`), so nest the module: its import alias is
	// still `registry`, the name of the exported constant.
	mod_dir := os.join_path(root, 'data', 'registry')
	os.mkdir_all(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'registry.v'),
		'module registry\n\npub const registry = [1, 2]\n\npub fn value() int {\n\treturn registry.len\n}\n')!
	os.write_file(os.join_path(root, 'main.v'),
		"module main\n\nimport data.registry\n\nfn main() {\n\tassert registry.value() == 2\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in(@VEXE, ['-no-parallel', 'run', 'main.v'], root)
	assert out.exit_code == 0, out.output
	assert out.output.split_into_lines().contains('ok'), out.output
}
