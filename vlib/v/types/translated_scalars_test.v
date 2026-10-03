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
	value += true
	value -= Code.one
	_ = 3 ^ true
	mut state := Code.zero
	state++
	mut character := char(0)
	character--
	mut boolean := false
	boolean++
	translated()
}
')!
	for flags in ['', '-no-parallel'] {
		result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains('expected `int`, not `Code`'), result.output
		assert result.output.contains('expected `int`, not `bool`'), result.output
		assert result.output.contains('can only be used with bool types'), result.output
		assert result.output.contains('left operand for `&&` is not a boolean'), result.output
		assert result.output.contains('cannot use `Code` as `int` in argument'), result.output
		assert result.output.contains('infix expr: cannot use `Code` (right expression) as `int literal`'), result.output
		assert result.output.contains('invalid right operand: int += bool'), result.output
		assert result.output.contains('invalid right operand: int -= Code'), result.output
		assert result.output.contains('right type of `^` cannot be non-integer type `bool`'), result.output
		assert result.output.contains('invalid operation: ++ (non-numeric type `Code`)'), result.output
		assert result.output.contains('invalid operation: -- (non-numeric type `char`)'), result.output
		assert result.output.contains('invalid operation: ++ (non-numeric type `bool`)'), result.output
	}
}

fn test_translated_bitwise_operators_still_require_integral_operands() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_bitwise_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { _ = 3 ^ 1.5 }\n')!
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('right type of `^` cannot be non-integer'), result.output
}

fn test_translated_arithmetic_preserves_division_by_zero_error() {
	root := os.join_path(os.vtmp_dir(), 'v3_translated_zero_division_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), '@[translated]\nmodule main\nfn main() { _ = 3 / 0 }\n')!
	result := os.exec([@VEXE, '-check', root])
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
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.contains('expected `[]int`, not `[]bool`'), result.output
	assert result.output.contains('non-bool type `[]int`'), result.output
	assert result.output.contains('can only be used with bool types'), result.output
}
