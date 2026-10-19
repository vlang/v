module types

import v.flat

fn test_type_declaration_attributes_follow_semantic_module_identity() {
	mut a := flat.FlatAst.new()
	a.nodes = [
		flat.Node{ kind: .module_decl, value: 'homonym' },
		flat.Node{ kind: .struct_decl, value: 'Receiver' },
		flat.Node{ kind: .struct_decl, value: 'Box' },
		flat.Node{ kind: .module_decl, value: 'main' },
		flat.Node{ kind: .struct_decl, value: 'Receiver' },
		flat.Node{ kind: .struct_decl, value: 'Box' },
		flat.Node{ kind: .module_decl, value: 'plain' },
		flat.Node{ kind: .struct_decl, value: 'Receiver' },
	]
	mut tc := TypeChecker.new(&a)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	tc.build_type_declaration_index(&a)
	tc.declaration_attributes[1] = ['heap', 'nocopy', 'noinit', 'attribute', 'custom: 42']
	tc.declaration_attributes[2] = ['heap']
	tc.declaration_attributes[4] = ['local_only']
	for context in ['main', 'homonym', 'plain'] {
		tc.cur_module = context
		for attribute in ['heap', 'nocopy', 'noinit', 'attribute', 'custom'] {
			assert tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('homonym.Receiver') }), attribute)
			assert !tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('Receiver') }), attribute)
			assert !tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('plain.Receiver') }), attribute)
		}
		assert tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('Receiver') }), 'local_only')
		assert !tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('homonym.Receiver') }), 'local_only')
		assert !tc.type_has_declaration_attribute(Type(Struct{ name_id: flat.source_type_id('missing.Receiver') }), 'heap')
		for base in ['Receiver', 'homonym.Receiver', 'Box[int]', 'homonym.Box[int]'] {
			typ := Type(Struct{ name_id: flat.source_type_id(base) })
			alias := Type(Alias{ name_id: flat.source_type_id('Alias'), base_type: typ })
			nested := Type(Alias{ name_id: flat.source_type_id('NestedAlias'), base_type: alias })
			for wrapped in [typ, alias, nested, Type(Pointer{ base_type: nested })] {
				assert tc.type_has_declaration_attribute(wrapped, 'heap') == base.starts_with('homonym.')
			}
		}
	}
}
