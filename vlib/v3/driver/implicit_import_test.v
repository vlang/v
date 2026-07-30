module driver

import os
import v3.parser
import v3.pref

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
