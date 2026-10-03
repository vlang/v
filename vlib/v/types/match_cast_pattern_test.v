module types

import os

fn test_repeated_optional_cast_condition_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'match_cast_pattern_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
enum Choice { first second }
fn choose(value ?Choice) int {
	return match true {
		value == ?Choice(.first) { 1 }
		value == ?Choice(.first) { 2 }
		else { 0 }
	}
}
fn main() { println(choose(.first)) }
')!
	for flags in ['', '-no-parallel -nocache'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('is handled more than once'), result.output
	}
}
