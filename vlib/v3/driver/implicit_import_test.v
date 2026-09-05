module driver

import os
import v3.flat
import v3.parser
import v3.pref
import v3.types

fn test_default_bin_file_strips_backend_source_extension() {
	assert default_bin_file_for_input('foo.c.v') == 'foo'
	assert default_bin_file_for_input('foo.js.v') == 'foo'
	assert default_bin_file_for_input('foo.wasm.v') == 'foo'
	assert default_bin_file_for_input('foo.v') == 'foo'
	assert default_bin_file_for_input('foo.vv') == 'foo'
}

fn test_default_bin_file_for_directory_is_source_adjacent() {
	root := os.join_path(os.temp_dir(), 'v3_default_bin_directory_${os.getpid()}')
	source_dir := os.join_path(root, 'app')
	os.rmdir_all(root) or {}
	os.mkdir_all(source_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	real_source_dir := os.real_path(source_dir)
	assert default_bin_file_for_input(source_dir) == os.join_path_single(real_source_dir, 'app')
}

fn test_profile_optional_arg_recognizes_vv_source() {
	value, consumed := v3_profile_optional_arg_value(['-profile', 'fixture.vv', '-o', 'out'], 0, false)
	assert value == '-'
	assert !consumed
}

fn test_default_bin_file_uses_safe_hidden_source_name() {
	assert default_bin_file_for_input('.v') == '.v.out'
	assert default_bin_file_for_input('.vv') == '.vv.out'
	assert default_bin_file_for_input('.vsh') == '.vsh.out'
	assert default_bin_file_for_input(os.join_path('source', '.v')) == os.join_path('source', '.v.out')
	assert default_bin_file_for_input('unsafe\t.v') == 'unsafe_.v.out'
}

fn test_default_bin_file_resolves_source_symlink() {
	$if !windows {
		root := os.join_path(os.temp_dir(), 'v3_default_bin_symlink_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(os.join_path(root, 'source'))!
		os.mkdir_all(os.join_path(root, 'links'))!
		defer {
			os.rmdir_all(root) or {}
		}
		target := os.join_path(root, 'source', 'app.v')
		link := os.join_path(root, 'links', 'app.v')
		os.write_file(target, 'fn main() {}\n')!
		os.symlink(target, link)!
		expected := os.join_path_single(os.dir(os.real_path(target)), 'app')
		assert default_bin_file_for_input(link) == expected
	}
}

fn test_c_executable_bin_file_uses_target_postfix() {
	assert c_executable_bin_file_for_target('app', 'windows', false, false, false) == 'app.exe'
	assert c_executable_bin_file_for_target('app.exe', 'windows', false, false, false) == 'app.exe'
	assert c_executable_bin_file_for_target('app', 'macos', false, false, false) == 'app'
	assert c_executable_bin_file_for_target('library', 'windows', true, false, false) == 'library'
	assert c_executable_bin_file_for_target('unit.o', 'windows', false, true, false) == 'unit.o'
	assert c_executable_bin_file_for_target('source', 'windows', false, false, true) == 'source'
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
		kind: .field_decl
		value: 'value'
	})
	struct_id := ast.add_node(flat.Node{
		kind: .struct_decl
		value: 'Packed'
	})
	ast.add_node(flat.Node{
		kind: .directive
		value: '@attributes:${int(struct_id)}'
	})
	insert_synthetic_imports(mut ast, [
		SyntheticInsertion{
			pos: 0
			node: flat.Node{
				kind: .import_decl
				value: 'builtin'
			}
		},
	])
	assert ast.nodes[3].kind == .directive
	assert ast.nodes[3].value == '@attributes:2'
}

fn test_synthetic_import_insertion_preserves_file_index_and_import_order() {
	root := os.join_path(os.vtmp_dir(), 'v3_synthetic_file_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.v')
	second := os.join_path(root, 'second.v')
	os.write_file(first, 'module first\nimport math\nfn first() {}\n')!
	os.write_file(second, 'module second\nimport strings\nfn second() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(first)
	mut ast := p.parse_file(second)
	assert p.diagnostics.len == 0
	boundary := ast.file_node_ids[2]
	insert_synthetic_imports(mut ast, [
		SyntheticInsertion{ pos: boundary, node: sync_import_node() },
		SyntheticInsertion{ pos: boundary, node: embed_file_import_node() },
		SyntheticInsertion{ pos: ast.nodes.len, node: closure_import_node() },
	])
	assert ast.file_node_ids.len == 4
	assert !ast.file_index_incomplete
	assert ast.file_node_ids[2] == boundary + 2
	for pair in 0 .. 2 {
		marker := ast.nodes[ast.file_node_ids[pair * 2]]
		trailing := ast.nodes[ast.file_node_ids[pair * 2 + 1]]
		assert marker.kind == .file && trailing.kind == .file
		assert marker.value == trailing.value
	}
	for region in [0, boundary] {
		mut expected := []int{}
		for i in region .. ast.nodes.len {
			if ast.nodes[i].kind in [.file, .module_decl, .import_decl] {
				expected << i
			}
		}
		actual, _ := collect_import_scan_ids(ast, region, if region == 0 { 0 } else { 2 })
		assert actual == expected
	}
	mut indexed := types.TypeChecker.new(ast)
	indexed.collect(ast)
	ast.file_index_incomplete = true
	mut scanned := types.TypeChecker.new(ast)
	scanned.collect(ast)
	assert indexed.top_level_idx == scanned.top_level_idx
	assert indexed.file_imports == scanned.file_imports
	assert indexed.file_imports['${first}\nsync'] == 'sync'
	assert indexed.file_imports['${first}\nembed_file'] == 'v.embed_file'
	assert indexed.file_imports['${second}\n${closure_runtime_import_alias}'] == 'builtin.closure'
}

fn test_file_index_gap_preserves_bundle_file_context() {
	root := os.join_path(os.vtmp_dir(), 'v3_file_index_gap_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	for name in ['first', 'bundle', 'last'] {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, 'module ${name}\nimport math as m\nfn ${name}() {}\n')!
		p.parse_file(path)
	}
	mut ast := p.a
	assert p.diagnostics.len == 0
	// A synthesized bundle may have complete file nodes but no indexed pair.
	ast.file_node_ids = [ast.file_node_ids[0], ast.file_node_ids[1], ast.file_node_ids[4],
		ast.file_node_ids[5]]
	mut indexed := types.TypeChecker.new(ast)
	indexed.collect(ast)
	ast.file_index_incomplete = true
	mut scanned := types.TypeChecker.new(ast)
	scanned.collect(ast)
	assert indexed.top_level_idx == scanned.top_level_idx
	assert indexed.file_imports == scanned.file_imports
	for name in ['first', 'bundle', 'last'] {
		path := os.join_path(root, '${name}.v')
		assert indexed.file_imports['${path}\nm'] == 'math'
		assert '${name}.${name}' in indexed.fn_ret_types
	}
}
