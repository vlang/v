module types

import v.flat
import v.token

fn test_private_declaration_exact_lookup_preserves_visibility() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	private := DeclarationVisibility{
		module_name: 'dep'
		kind:        .fn_decl
	}
	tc.declaration_visibility['dep.hidden'] = private
	tc.declaration_visibility['dep.visible'] = DeclarationVisibility{
		...private
		is_pub: true
	}
	found := tc.private_declaration('dep.hidden') or { panic('missing private declaration') }
	assert found == private
	assert tc.private_declaration('dep.visible') == none
	assert tc.private_declaration('') == none
	assert tc.private_declaration('missing') == none
	tc.cur_module = 'dep'
	assert tc.private_declaration('dep.hidden') == none
	for owner in ['', 'main'] {
		tc.declaration_visibility['main.hidden'] = DeclarationVisibility{
			module_name: owner
			kind:        .fn_decl
		}
		for current in ['', 'main'] {
			tc.cur_module = current
			assert tc.private_declaration('main.hidden') == none
		}
		tc.cur_module = 'consumer'
		assert tc.private_declaration('main.hidden') != none
	}
}

fn test_private_declaration_lookup_preserves_fallback_order() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	private := DeclarationVisibility{
		module_name: 'dep'
		kind:        .fn_decl
	}
	tc.declaration_visibility['dep.hidden'] = private
	tc.declaration_visibility['dep.Box.method'] = private
	assert tc.private_declaration('pkg.dep.hidden') != none
	assert tc.private_declaration('pkg.dep.Box[int].method') != none
	// The first declaration wins, including a public exact or shortened match.
	tc.declaration_visibility['pkg.dep.hidden'] = DeclarationVisibility{
		...private
		is_pub: true
	}
	assert tc.private_declaration('pkg.dep.hidden') == none
	assert tc.private_declaration('outer.pkg.dep.hidden') == none
	tc.declaration_visibility['dep.Box[int].method'] = DeclarationVisibility{
		...private
		is_pub: true
	}
	assert tc.private_declaration('dep.Box[int].method') == none
}

fn test_private_declaration_lookup_preserves_test_and_c_mirror_bypasses() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	tc.declaration_visibility['C.Hidden'] = DeclarationVisibility{
		module_name: 'dep'
		kind:        .struct_decl
	}
	assert tc.private_declaration('C.Hidden') != none
	tc.c_struct_scoped_fields[c_struct_module_key('consumer', 'C.Hidden')] = []StructField{}
	assert tc.private_declaration('C.Hidden') == none
	tc.c_struct_scoped_fields.clear()
	for file in ['consumer_test.v', 'consumer_test.vv'] {
		tc.cur_file = file
		assert tc.private_declaration('C.Hidden') == none
	}
	tc.cur_file = 'consumer.v'
	assert tc.private_declaration('C.Hidden') != none
}

fn test_translated_file_lookup_handles_empty_maps_and_missing_files() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('ordinary.v', 1)
	a.source_files[2] = files.add_file('translated.c.v', 1)
	ordinary := flat.Node{
		kind: .ident
		pos:  token.new_span(1, 0, 1)
	}
	translated := flat.Node{
		kind: .ident
		pos:  token.new_span(2, 0, 1)
	}
	missing := flat.Node{
		kind: .ident
		pos:  token.new_span(3, 0, 1)
	}
	mut tc := TypeChecker.new(&a)
	assert !tc.node_is_from_translated_file(ordinary)
	assert !tc.node_is_from_translated_file(translated)
	assert !tc.node_is_from_translated_file(missing)
	tc.translated_files['translated.c.v'] = true
	assert tc.node_is_from_translated_file(translated)
	assert !tc.node_is_from_translated_file(ordinary)
	assert !tc.node_is_from_translated_file(missing)
	tc.translated_files['translated.c.v'] = false
	assert !tc.node_is_from_translated_file(translated)
	tc.translated_files['ordinary.v'] = true
	assert tc.node_is_from_translated_file(ordinary)
	tc.translated_files.clear()
	assert !tc.node_is_from_translated_file(ordinary)
}
