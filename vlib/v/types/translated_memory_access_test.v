module types

import os

fn test_translated_memory_rules_are_local_to_the_source_file() {
	root := os.join_path(os.vtmp_dir(), 'translated_memory_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nstruct State { count int }\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn main() {
	state := &State{}
	state.count++
	mut values := [3, 5]!
	pointer := unsafe { &values[1] }
	println(unsafe { pointer[-1] })
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('field `count`'), result.output
		assert result.output.contains('is immutable'), result.output
		assert result.output.contains('negative index `-1`'), result.output
	}
}

fn test_translated_arrays_still_reject_negative_indexes() {
	root := os.join_path(os.vtmp_dir(), 'translated_array_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { values := [3, 5]!; println(values[-1]) }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('negative index `-1`'), result.output
}
