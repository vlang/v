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
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
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
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('negative index `-1`'), result.output
}

fn test_ordinary_files_keep_global_shadow_and_alias_checks() {
	root := os.join_path(os.vtmp_dir(), 'translated_alias_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), '@[has_globals]
module main
struct State { mut: count int }
__global count = int(5)
fn state_alias(state &State) &State { return state }
fn echo_count(count int) int { return count }
fn main() {
	state := State{count: 40}
	state_alias(&state).count |= 2
	translated()
}
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('shadows a global variable'), result.output
		assert result.output.contains('aliases mutable data from an immutable value'), result.output
	}
}
