module types

import os

fn test_quoted_reflection_metadata_does_not_use_generic_parameter() {
	root := os.join_path(os.vtmp_dir(), 'comptime_param_notice_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for literal in ["'T'", '"T"', "r'T'", "'escaped \\'T\\''"] {
		os.write_file(path, 'module main
struct Probe { value int }
fn check[T](unused T) {
	\$for field in Probe.fields {
		\$if field.name == ${literal} {
			assert true
		}
	}
}
fn main() { check(1) }
')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code == 0, result.output
		assert result.output.count('unused parameter: `unused`') == 1, result.output
		strict := os.exec([@VEXE, '-N', '-check', path])
		assert strict.exit_code != 0, strict.output
		assert strict.output.count('unused parameter: `unused`') == 1, strict.output
	}
}

fn test_type_only_reflection_still_uses_generic_parameter() {
	root := os.join_path(os.vtmp_dir(), 'comptime_param_type_use_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main
fn assert_function[T](value T) {
	\$if T is \$function { assert true } \$else { assert false }
}
fn assert_not_function[T](value T) {
	\$if T is \$function { assert false } \$else { assert true }
}
fn main() {
	assert_function(fn () {})
	assert_not_function(1)
}
')!
	result := os.exec([@VEXE, '-N', '-check', path])
	assert result.exit_code == 0, result.output
	assert !result.output.contains('unused parameter:'), result.output
}

fn test_other_type_does_not_use_parameter_type() {
	assert !comptime_cond_mentions_type('TT is \$struct', 'T')
	root := os.join_path(os.vtmp_dir(), 'comptime_param_other_type_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'module main
struct TT {}
fn check[T](unused T) {
	\$if TT is \$struct { assert true }
}
fn main() { check(1) }
')!
	result := os.exec([@VEXE, '-N', '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.count('unused parameter: `unused`') == 1, result.output
}
