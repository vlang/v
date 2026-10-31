module types

import os
import v.parser
import v.pref

fn sum_pointer_cast_errors(source string) ![]TypeError {
	path := os.join_path(os.vtmp_dir(), 'sum_pointer_cast_${os.getpid()}.v')
	os.write_file(path, source)!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([path])
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics_opt(false)
	return tc.errors
}

fn test_sum_cast_accepts_references_to_value_variants() {
	errors := sum_pointer_cast_errors('module main
type Value = int | []Value | map[string]Value
fn main() {
	map_ref := &map[string]Value{}
	array_ref := &[]Value{}
	answer := 42
	int_ref := &answer
	_ = Value(map_ref)
	_ = Value(array_ref)
	_ = Value(int_ref)
}
')!
	assert errors.len == 0, errors.str()
}

fn test_sum_cast_still_rejects_references_to_non_variants() {
	errors := sum_pointer_cast_errors('module main
type Value = int | map[string]Value
fn main() {
	array_ref := &[]Value{}
	_ = Value(array_ref)
}
')!
	assert errors.any(it.msg == 'cannot cast `&[]Value` to `Value`'), errors.str()
}
