module c

import os
import v3.flat
import v3.parser
import v3.pref
import v3.types

fn test_optional_typedef_collection_ignores_incomplete_call_type_text() {
	mut ast := &flat.FlatAst{}
	ast.nodes = [flat.Node{
		kind: .call
		typ:  '?([]'
	}, flat.Node{
		kind: .call
		typ:  '?string'
	}]
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.collect_optional_typedefs()
	assert 'Optional_string' in g.needed_optional_types
	assert g.needed_optional_types.len == 1
}

fn test_enum_decls_resets_checker_module_at_file_boundary() {
	test_dir := os.join_path(os.vtmp_dir(), 'v3_enum_decls_module_reset_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	main_path := os.join_path(test_dir, 'main.v')
	shadow_path := os.join_path(test_dir, 'shadow.v')
	os.write_file(main_path, 'type Storage = u64

const base = 300

enum E as Storage {
	a = base + 2
	b
}
') or {
		panic(err)
	}
	os.write_file(shadow_path, 'module shadow

type Storage = u8

const base = 4
') or {
		panic(err)
	}

	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files([main_path, shadow_path])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	tc.cur_file = main_path
	tc.cur_module = 'shadow'

	mut g := FlatGen.new()
	g.a = a
	g.tc = &tc
	g.enum_decls()
	c_source := g.sb.str()
	assert c_source.contains('typedef u64 E;'), c_source
	assert c_source.contains('#define E__a ((E)(302))'), c_source
	assert tc.cur_module == 'shadow'
}
