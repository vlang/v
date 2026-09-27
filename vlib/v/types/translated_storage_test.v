module types

import os

fn test_regular_v_storage_rules_remain_checked() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_storage_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	translated := os.join_path(root, 'translated.v')
	regular := os.join_path(root, 'main.v')
	os.write_file(translated, '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(regular, 'module main\n__global ordinary = int(0)\nfn write_pointer(p &int) { *p = 3 }\nfn main() { translated() }\n')!
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('enable globals'), result.output
		assert result.output.contains('modifying variables via dereferencing'), result.output
	}
}
