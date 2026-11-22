import os

const vexe = @VEXE

// A generic of an imported project module is not one of the files a check
// diagnoses, but a specialization of it can still shadow a global.
fn test_check_reports_a_global_shadowed_in_a_specialization_of_an_imported_generic() {
	root := os.join_path(os.vtmp_dir(), 'check_generic_global_shadow_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'helper'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'shadow_app'\n}\n")!
	os.write_file(os.join_path(root, 'main.v'),
		'@[has_globals]\nmodule main\n\nimport helper\n\n__global counter = int(0)\n\nfn main() {\n\tcounter++\n\tprintln(helper.show[int]())\n}\n')!
	os.write_file(os.join_path(root, 'helper', 'helper.v'),
		'module helper\n\npub fn show[T]() int {\n\t\$if T is int {\n\t\tcounter := 1\n\t\treturn counter\n\t}\n\treturn 0\n}\n')!
	result := os.exec([vexe, '-enable-globals', '-check', os.join_path(root, 'main.v')])
	assert result.exit_code != 0, result.output
	assert result.output.contains('shadows a global variable'), result.output
}
