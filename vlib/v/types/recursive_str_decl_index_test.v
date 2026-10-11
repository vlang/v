module types

import os
import v.flat
import v.parser
import v.pref

fn recursive_str_decl_scan(tc &TypeChecker, name string) ?flat.NodeId {
	short_name := name.all_after_last('.')
	if idx := tc.fn_decl_short_name_ids[short_name] {
		id := flat.NodeId(idx)
		if tc.recursive_str_fn_decl_matches(*tc.a.node(id), name) {
			return id
		}
	}
	for idx in tc.top_level_idx {
		id := flat.NodeId(idx)
		node := tc.a.node(id)
		if node.kind == .fn_decl && tc.recursive_str_fn_decl_matches(*node, name) {
			return id
		}
	}
	return none
}

fn test_recursive_str_declaration_index_preserves_collision_order_and_aliases() {
	root := os.join_path(os.vtmp_dir(), 'recursive_str_decl_index_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut paths := []string{}
	for module_name in ['alpha', 'beta', 'main'] {
		path := os.join_path(root, '${module_name}.v')
		os.write_file(path, 'module ${module_name}
struct Value {}
fn (value Value) str() string { return "${module_name}" }
fn same() {}
interface Contract { missing() string }
')!
		paths << path
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files(paths)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.recursive_str_fn_decl_index_complete
	assert tc.recursive_str_fn_decl_index_size == tc.top_level_idx.len
	for name in ['Value.str', 'alpha.Value.str', 'beta.Value.str', 'main.Value.str',
		'main.alpha.Value.str', 'same', 'alpha.same', 'beta.same', 'main.same', 'Contract.missing',
		'alpha.Contract.missing', 'absent', 'alpha.absent'] {
		assert (tc.recursive_str_fn_decl_id(name) or { flat.empty_node }) == (recursive_str_decl_scan(&tc,
			name) or { flat.empty_node }), name
	}
	first := tc.recursive_str_fn_decl_id('Value.str') or { panic('missing raw method') }
	first_source := tc.a.source_files[tc.a.node(first).pos.id] or { panic('missing first source') }
	assert first_source.name == paths[0]
	qualified := tc.recursive_str_fn_decl_id('beta.Value.str') or { panic('missing qualified method') }
	qualified_source := tc.a.source_files[tc.a.node(qualified).pos.id] or { panic('missing qualified source') }
	assert qualified_source.name == paths[1]
	assert tc.recursive_str_fn_decl_id('alpha.Contract.missing') == none
	worker := tc.fork_program_view(tc.a, tc.direct_dependencies_by_fn)
	assert (worker.recursive_str_fn_decl_id('beta.Value.str') or { flat.empty_node }) == qualified
}

fn test_recursive_str_declaration_index_extends_prepared_collection() {
	root := os.join_path(os.vtmp_dir(), 'recursive_str_decl_append_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'alpha.v')
	second := os.join_path(root, 'beta.v')
	os.write_file(first, 'module alpha\nfn earlier() {}\n')!
	os.write_file(second, 'module beta\nfn later() {}\n')!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(first)
	mut tc := TypeChecker.new(a)
	assert tc.prepare_collect(a)
	earlier := tc.recursive_str_fn_decl_id('alpha.earlier') or { panic('missing earlier fn') }
	a = p.parse_file(second)
	assert tc.collect_continue(a)
	assert tc.recursive_str_fn_decl_index_complete
	assert tc.recursive_str_fn_decl_index_size == tc.top_level_idx.len
	assert (tc.recursive_str_fn_decl_id('alpha.earlier') or { flat.empty_node }) == earlier
	assert tc.recursive_str_fn_decl_id('beta.later') == recursive_str_decl_scan(&tc, 'beta.later')
	assert tc.recursive_str_fn_decl_id('beta.absent') == none
}

fn test_recursive_str_declaration_index_keeps_fallback_and_validates_hits() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.fn_decl, 'First.str')
	second := a.add_val(.fn_decl, 'Second.str')
	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [i32(first), i32(second)]
	tc.cur_module = 'scope'
	tc.build_fn_name_indexes(&a)
	assert !tc.recursive_str_fn_decl_index_complete
	assert (tc.recursive_str_fn_decl_id('scope.Second.str') or { flat.empty_node }) == second
	assert (tc.recursive_str_fn_decl_id('main.Second.str') or { flat.empty_node }) == second
	a.nodes[int(second)].value = 'Changed.str'
	assert tc.recursive_str_fn_decl_id('Second.str') == none
	assert (tc.recursive_str_fn_decl_id('scope.Changed.str') or { flat.empty_node }) == second
	third := a.add_val(.fn_decl, 'Third.str')
	tc.top_level_idx << i32(third)
	assert (tc.recursive_str_fn_decl_id('scope.Third.str') or { flat.empty_node }) == third
	tc.build_fn_name_indexes(&a)
	assert (tc.recursive_str_fn_decl_id('Third.str') or { flat.empty_node }) == third
}

fn test_recursive_str_declaration_index_keeps_missing_module_metadata_fallback() {
	path := os.join_path(os.vtmp_dir(), 'recursive_str_missing_module_${os.getpid()}.v')
	os.write_file(path, 'module alpha\nfn available() {}\n')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.recursive_str_fn_decl_index_complete
	id := tc.recursive_str_fn_decl_id('alpha.available') or { panic('missing declaration') }
	// A partial view can retain source files without collection's module metadata.
	tc.file_modules.delete(path)
	tc.top_level_idx = tc.top_level_idx.filter(tc.a.nodes[it].kind != .module_decl)
	tc.cur_module = 'scope'
	tc.build_fn_name_indexes(a)
	assert !tc.recursive_str_fn_decl_index_complete
	assert (tc.recursive_str_fn_decl_id('scope.available') or { flat.empty_node }) == id
	assert tc.recursive_str_fn_decl_id('scope.available') == recursive_str_decl_scan(&tc, 'scope.available')
}
