module main

import os

fn array_pointer_index_test_body(generated string, name string) string {
	for line in generated.split_into_lines() {
		if line.contains(' ${name}(') && line.ends_with(' {') {
			return generated.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}

fn test_fixed_array_pointer_indexing_preserves_alias_indirection_and_nested_rows() {
	root := os.join_path(os.vtmp_dir(), 'array_pointer_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(path, 'type Row = [4]int
type Values = Row
type Matrix = [2]Row
fn direct(value &[4]int) int { return unsafe { value[3] } }
fn aliased(value &Row) int { return unsafe { value[3] } }
fn chained(value &Values) int { return unsafe { value[0][3] } }
fn nested(value &Matrix) int { return unsafe { value[1][3] } }
fn main() {
	_ = direct(&[4]int{})
	_ = aliased(&Row{})
	_ = chained(&Values{})
	_ = nested(&Matrix{})
}
')!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	direct := array_pointer_index_test_body(generated, 'direct')
	assert direct.contains('return (*value)[3];'), direct
	aliased := array_pointer_index_test_body(generated, 'aliased')
	assert aliased.contains('return (*value)[3];'), aliased
	chained := array_pointer_index_test_body(generated, 'chained')
	assert chained.contains('return (value)[0][3];'), chained
	assert !chained.contains('(*value)'), chained
	nested := array_pointer_index_test_body(generated, 'nested')
	assert nested.contains('return ((*value)[1])[3];'), nested
}
