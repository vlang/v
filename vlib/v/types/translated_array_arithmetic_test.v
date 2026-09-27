module types

import os

fn test_translated_array_arithmetic_rejects_invalid_operands() {
	root := os.join_path(os.vtmp_dir(), 'translated_array_arithmetic_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for index, source in [
		'module main\nfn main() { values := [1, 2]!; _ = values + 1 }',
		'@[translated]\nmodule main\nfn main() { values := [1, 2]!; _ = values + values }',
		'@[translated]\nmodule main\nfn main() { values := [1, 2]!; _ = values + 1.5 }',
	] {
		path := os.join_path(root, 'case_${index}.v')
		os.write_file(path, source)!
		result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('error:'), result.output
		os.rm(path)!
	}
}
