module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn array_pointer_index_test_body(generated string, name string) string {
	for line in generated.split_into_lines() {
		if line.contains(' ${name}(') && line.ends_with(' {') {
			return generated.all_after(line).all_before('\n}')
		}
	}
	panic('missing generated function ${name}')
}

fn test_fixed_array_pointer_indexing_preserves_alias_indirection_and_nested_rows() {
	path := os.join_path(os.vtmp_dir(), 'array_pointer_index_${os.getpid()}.v')
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
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	generated := g.gen_with_used_options(a, used, &tc, true)
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
