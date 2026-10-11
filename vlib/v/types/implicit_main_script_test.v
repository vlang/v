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

fn test_script_statements_after_import_semicolons_are_checked() {
	root := os.join_path(os.vtmp_dir(), 'v3_script_import_semicolons_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'values'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'script_semicolons' }\n")!
	os.write_file(os.join_path(root, 'values', 'values.v'), 'module values\npub fn answer() f64 { return 1.25 }\n')!
	script := os.join_path(root, 'main.v')
	for source in [
		'import values; println(values.answer())\n',
		'import values as v; println(v.answer())\n',
		'import values { answer }; println(answer())\n',
		'import values; value := values.answer(); println(value)\n',
		'import values /* comment ; import values */; println(values.answer())\n',
		'import values; /* comment ; import values */ println(values.answer())\n',
		'import values; /* outer /* nested ; import values */ */ println(values.answer())\n',
		'import values;\n/*\nimport values */ println(values.answer())\n',
		'import values; marker := "quoted ; import values"; assert marker.len > 0; println(values.answer())\n',
		'import values; marker := r"quoted ; import values"; assert marker.len > 0; println(values.answer())\n',
		'import values // comment ; import values\nprintln(values.answer())\n',
	] {
		os.write_file(script, source)!
		for flags in ['', '-no-parallel -nocache'] {
			result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation',
				...(os.split_args(flags) or { panic(err) }), '-gc', 'none', 'run', script])
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == '1.25', result.output
		}
	}
	os.write_file(script, 'import values; value := values.answer(); println(value.missing())\n')!
	invalid := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', script])
	assert invalid.exit_code != 0, invalid.output
	assert invalid.output.contains('missing'), invalid.output
	os.write_file(script, 'import values values\n')!
	malformed_import := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', script])
	assert malformed_import.exit_code != 0, malformed_import.output
	assert malformed_import.output.contains('cannot import multiple modules at a time'), malformed_import.output
	assert !malformed_import.output.contains('unknown ident'), malformed_import.output
}
