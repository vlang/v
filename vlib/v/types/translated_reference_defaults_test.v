module types

import os

fn test_translated_reference_defaults_do_not_leak_into_ordinary_initializers() {
	root := os.join_path(os.vtmp_dir(), 'translated_references_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nstruct Cell { link &int }\nfn translated_cell() Cell { return Cell{} }\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nfn main() { _ = Cell{}; _ = translated_cell() }\n')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('reference field `Cell.link` must be initialized'), result.output
		assert !result.output.contains('translated.v:'), result.output
	}
}

fn test_translated_reference_defaults_preserve_required_and_typed_fields() {
	root := os.join_path(os.vtmp_dir(), 'translated_required_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
struct Cell { link &int }
struct Required { value int @[required] }
fn main() {
 _ = Required{}
 _ = Cell{link: "invalid"}
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('field `Required.value` must be initialized'), result.output
	assert result.output.contains('reference field must be initialized with reference'), result.output
}
