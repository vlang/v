module types

import os

fn test_translated_scalar_rules_do_not_leak_into_regular_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_scalars_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated() {}\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
enum Code { zero one }
fn numeric(value int) int { return value }
fn main() {
	mut value := int(0)
	value = Code.one
	value = true
	flag := char(0)
	_ = !flag
	if flag && value { println(value) }
	_ = numeric(Code.one)
	_ = 4 - Code.one
	_ = 4 + Code.one
	translated()
}
')!
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(root)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('expected `int`, not `Code`'), result.output
		assert result.output.contains('expected `int`, not `bool`'), result.output
		assert result.output.contains('can only be used with bool types'), result.output
		assert result.output.contains('left operand for `&&` is not a boolean'), result.output
		assert result.output.contains('cannot use `Code` as `int` in argument'), result.output
		assert result.output.contains('infix expr: cannot use `Code` (right expression) as `int literal`'), result.output
	}
}

fn test_translated_arithmetic_preserves_division_by_zero_error() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_zero_division_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { _ = 3 / 0 }\n')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('division by zero'), result.output
}

fn test_translated_scalar_rules_do_not_convert_containers() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_scalar_containers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]
module main
fn main() {
	mut values := [1]
	flags := [true]
	values = flags
	if values { println(values) }
	_ = !values
}
')!
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('expected `[]int`, not `[]bool`'), result.output
	assert result.output.contains('non-bool type `[]int`'), result.output
	assert result.output.contains('can only be used with bool types'), result.output
}
