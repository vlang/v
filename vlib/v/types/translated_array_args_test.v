module types

import os

fn test_translated_array_decay_does_not_leak_into_regular_calls() {
	root := os.join_path(os.vtmp_dir(), 'translated_array_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn pointer_arg(values &int) {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nfn main() { values := [1, 2]!; pointer_arg(values); pointer := unsafe { &values[0] }; _ = pointer == values }\n')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot use `[2]int` as `&int`'), result.output
		assert result.output.contains('infix expr:'), result.output
	}
}

fn test_translated_array_decay_keeps_element_and_container_checks() {
	root := os.join_path(os.vtmp_dir(), 'translated_array_types_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
fn pointer_arg(values &int) {}
fn main() {
	strings := ["first", "second"]!
	dynamic := [1, 2]
	narrow := [i16(1), 2]!
	pointer_arg(strings)
	pointer_arg(dynamic)
	pointer_arg(narrow)
	pointer := unsafe { &narrow[0] }
	_ = pointer == [1, 2]!
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot use `[2]string` as `&int`'), result.output
	assert result.output.contains('cannot use `[]int` as `&int`'), result.output
	assert result.output.contains('cannot use `[2]i16` as `&int`'), result.output
	assert result.output.contains('infix expr:'), result.output
}
