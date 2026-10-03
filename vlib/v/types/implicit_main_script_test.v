module types

import os

fn test_implicit_main_script_with_imported_module() {
	root := os.join_path(os.vtmp_dir(), 'v3_implicit_main_script_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'script_example' }\n")!
	os.write_file(os.join_path(root, 'values', 'values.v'), 'module values\npub fn answer() int { return 42 }\n')!
	script := os.join_path(root, 'main.v')
	os.write_file(script, 'import values\nprintln(values.answer())\n')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), 'run', '${script}'])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space().ends_with('42'), result.output
	}
	os.write_file(script, 'module example\nimport values\nfn main() { println(values.answer()) }\n')!
	non_main := os.exec([@VEXE, '-check', '${script}'])
	assert non_main.exit_code != 0, non_main.output
	assert non_main.output.contains('project must include a `main` module'), non_main.output
}
