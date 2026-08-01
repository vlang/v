module driver

import os
import v3.flat
import v3.parser
import v3.pref

fn test_default_bin_file_strips_backend_source_extension() {
	assert default_bin_file_for_input('foo.c.v') == 'foo'
	assert default_bin_file_for_input('foo.js.v') == 'foo'
	assert default_bin_file_for_input('foo.wasm.v') == 'foo'
	assert default_bin_file_for_input('foo.v') == 'foo'
}

fn scan_implicit_import_source(name string, source string) ImplicitImportScan {
	path := os.join_path(os.temp_dir(), 'v3_implicit_import_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	mut scan := ImplicitImportScan{}
	scan_implicit_imports(a, a.nodes.len, mut scan)
	return scan
}

fn test_builtin_len_fields_do_not_require_closure_runtime() {
	scan := scan_implicit_import_source('builtin_len', '
fn lengths(s string, values []int, lookup map[string]int, fixed [4]int) int {
	selected := if s.len > 0 { s } else { s }
	return selected.len + values.len + lookup.len + fixed.len
}
')
	assert !scan.needs_closure
}

fn test_custom_len_method_value_requires_closure_runtime() {
	scan := scan_implicit_import_source('custom_len', '
struct Item {}

fn (item Item) len() int {
	return 1
}

fn use(item Item) {
	callback := item.len
	_ = callback
}
')
	assert scan.needs_closure
}

fn test_method_value_on_call_result_requires_closure_runtime() {
	scan := scan_implicit_import_source('call_result_method', '
struct Item {}

fn (item Item) value() int {
	return 1
}

fn make_item() Item {
	return Item{}
}

fn use() {
	callback := make_item().value
	_ = callback
}
')
	assert scan.needs_closure
}

fn test_strings_similarity_len_fields_do_not_require_closure_runtime() {
	path := os.join_path(@VEXEROOT, 'vlib', 'strings', 'similarity.v')
	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	mut scan := ImplicitImportScan{}
	scan_implicit_imports(a, a.nodes.len, mut scan)
	assert !scan.needs_closure
}

fn test_shared_parameter_and_local_require_sync_runtime() {
	param_scan := scan_implicit_import_source('shared_param', '
struct State {}

fn use(shared state State) {}
')
	assert param_scan.needs_sync

	local_scan := scan_implicit_import_source('shared_local', '
struct State {}

fn main() {
	shared state := State{}
}
')
	assert local_scan.needs_sync
}

fn test_known_fields_and_call_returns_do_not_require_closure_runtime() {
	scan := scan_implicit_import_source('known_fields', '
type Builder = []u8

struct Info {
	value int
}

enum Kind {
	first
}

fn text() string {
	return "ok"
}

fn inspect(mut builder Builder, info Info) int {
	value := text()
	_ = builder.flags
	_ = info.value
	_ = Kind.first
	return value.len
}
')
	assert !scan.needs_closure
}

fn test_synthetic_import_insertion_remaps_declaration_attribute_targets() {
	mut ast := flat.FlatAst.new()
	ast.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
	})
	struct_id := ast.add_node(flat.Node{
		kind:  .struct_decl
		value: 'Packed'
	})
	ast.add_node(flat.Node{
		kind:  .directive
		value: '@attributes:${int(struct_id)}'
	})
	insert_synthetic_imports(mut ast, [
		SyntheticInsertion{
			pos:  0
			node: flat.Node{
				kind:  .import_decl
				value: 'builtin'
			}
		},
	])
	assert ast.nodes[3].kind == .directive
	assert ast.nodes[3].value == '@attributes:2'
}
