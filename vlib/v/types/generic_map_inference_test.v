module types

import os
import v.parser
import v.pref

fn check_generic_map_inference(name string, expression string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'generic_map_inference_${name}_${os.getpid()}.v')
	os.write_file(path, 'interface Val { value() string }
struct Item { text string }
fn (i Item) value() string { return i.text }
fn invert[K, V](input map[K]V) map[V]K { return map[V]K{} }
fn to_map[K, V, X, Y](input map[K]V, f fn (K, V) (X, Y)) map[X]Y { return map[X]Y{} }
fn convert(key string, value Val) (string, string) { return key, value.value() }
fn takes(input map[string]Val) {}
fn main() {
 input := map[string]Val{}
 ${expression}
}
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_generic_map_invert_cannot_adopt_an_incompatible_expected_type() {
	tc := check_generic_map_inference('invert', 'takes(invert(input))')
	assert tc.errors.any(it.msg.contains('map[Val]string') && it.msg.contains('map[string]Val')), tc.errors.str()
}

fn test_generic_map_callback_result_cannot_adopt_an_incompatible_expected_type() {
	tc := check_generic_map_inference('direct', 'takes(to_map(input, convert))')
	assert tc.errors.any(it.msg.contains('map[string]string') && it.msg.contains('map[string]Val')), tc.errors.str()
}

fn test_generic_map_callback_result_keeps_its_type_in_a_local() {
	tc := check_generic_map_inference('local', 'result := to_map(input, convert)\n takes(result)')
	assert tc.errors.any(it.msg.contains('map[string]string') && it.msg.contains('map[string]Val')), tc.errors.str()
}
