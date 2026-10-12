module c

import v.flat
import v.types

fn escaped_c_field_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn test_escaped_c_field_names_follow_the_resolved_owner() {
	g := FlatGen.new()
	c_struct := types.Type(types.Struct{ name: 'C.EscapedFields' })
	alias := types.Type(types.Alias{ name: 'Fields', base_type: c_struct })
	pointer := types.Type(types.Pointer{ base_type: alias })
	pointer_alias := types.Type(types.Alias{ name: 'FieldsPtr', base_type: pointer })
	for owner in [c_struct, alias, pointer, pointer_alias] {
		assert g.field_c_name(owner, '@type') == 'type'
		assert g.field_c_name(owner, '@module') == 'module'
		assert g.field_c_name(owner, '@select') == 'select'
		assert g.field_c_name(owner, '_v_type') == '_v_type'
		assert g.field_c_name(owner, 'type') == 'type'
		for field in ['index', 'select', 'malloc', 'exit', 'byte', 'int_str', 'v_index'] {
			assert g.field_c_name(owner, field) == field
		}
	}
}

fn test_escaped_v_field_names_keep_their_existing_spelling() {
	g := FlatGen.new()
	v_struct := types.Type(types.Struct{ name: 'EscapedFields' })
	alias := types.Type(types.Alias{ name: 'Fields', base_type: v_struct })
	pointer := types.Type(types.Pointer{ base_type: alias })
	for owner in [v_struct, alias, pointer] {
		for field in ['@type', '@struct', '@select', '_v_type', 'type', 'index', 'select', 'malloc',
			'exit', 'byte', 'int_str'] {
			assert g.field_c_name(owner, field) == c_name(field)
		}
	}
}

fn test_escaped_c_initializer_field_names_resolve_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['C.EscapedFields'] = []types.StructField{}
	tc.structs['EscapedFields'] = []types.StructField{}
	tc.type_aliases['Fields'] = 'C.EscapedFields'
	tc.type_alias_modules['Fields'] = 'main'
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.init_field_c_name('C.EscapedFields', '@type') == 'type'
	assert g.init_field_c_name('Fields', '@type') == 'type'
	assert g.init_field_c_name('EscapedFields', '@type') == '_v_type'
	assert g.init_field_c_name('EscapedFields', '@struct') == '_v_struct'
	for field in ['index', 'select', 'malloc', 'exit', 'byte', 'int_str', 'v_index'] {
		assert g.init_field_c_name('C.EscapedFields', field) == field
		assert g.init_field_c_name('Fields', field) == field
		assert g.init_field_c_name('EscapedFields', field) == c_name(field)
	}
}

fn test_sizeof_c_field_names_follow_alias_pointer_owners() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['C.Record'] = [types.StructField{ name: 'index', typ: types.Type(types.u64_) }]
	tc.structs['Record'] = tc.structs['C.Record']
	tc.type_aliases['RecordAlias'] = 'C.Record'
	tc.type_aliases['RecordPointer'] = '&RecordAlias'
	tc.type_alias_modules['RecordAlias'] = 'main'
	tc.type_alias_modules['RecordPointer'] = 'main'
	tc.cur_module = 'main'
	tc.push_scope()
	tc.cur_scope.insert('record', tc.parse_type('RecordAlias'))
	tc.cur_scope.insert('pointer', tc.parse_type('RecordPointer'))
	tc.cur_scope.insert('value', tc.parse_type('Record'))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.sizeof_selector_target('record', ['index']) == 'record.index'
	assert g.sizeof_selector_target('pointer', ['index']) == 'pointer->index'
	assert g.sizeof_selector_target('value', ['index']) == 'value.v_index'
	tc.pop_scope()
}

fn test_escaped_c_callback_selector_assignment_keeps_c_abi_adapter() {
	for owner_name in ['C.CallbackHolder', 'CallbackAlias', 'CallbackPointerAlias'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		callback_type := types.Type(types.FnType{
			params:      [types.Type(types.int_)]
			return_type: types.Type(types.int_)
		})
		tc.cur_module = 'main'
		tc.structs['C.CallbackHolder'] = [types.StructField{ name: 'type', typ: callback_type, is_mut: true }]
		tc.type_aliases['CallbackAlias'] = 'C.CallbackHolder'
		tc.type_aliases['CallbackPointerAlias'] = '&CallbackAlias'
		tc.type_alias_modules['CallbackAlias'] = 'main'
		tc.type_alias_modules['CallbackPointerAlias'] = 'main'
		tc.struct_field_c_abi_fns['C.CallbackHolder\ntype'] = 'fn_ptr:int|int'
		tc.fn_param_types['callback'] = [types.Type(types.int_)]
		tc.fn_ret_types['callback'] = types.Type(types.int_)
		tc.push_scope()
		tc.cur_scope.insert('holder', tc.parse_type(owner_name))
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		base := escaped_c_field_test_node(mut a, .ident, 'holder', [])
		lhs := escaped_c_field_test_node(mut a, .selector, '@type', [base])
		rhs := escaped_c_field_test_node(mut a, .ident, 'callback', [])
		assert g.assign_lhs_c_abi_fn_ptr_type(lhs) or { '' } == 'fn_ptr:int|int', owner_name
		start := a.children.len
		a.children << [lhs, rhs]
		g.gen_assign(flat.Node{
			kind:           .assign
			op:             .assign
			children_start: i32(start)
			children_count: 2
		})
		generated := g.sb.str()
		assert generated.contains('type = callback_callback_adapter_'), generated
		assert !generated.contains('_v_type'), generated
		tc.pop_scope()
	}
}

fn test_c_callback_metadata_fallback_keeps_v_field_identity() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Holder'] = []types.StructField{}
	tc.structs['C.Holder'] = []types.StructField{}
	tc.struct_field_c_abi_fns['Holder\ntype'] = 'fn_ptr:int|int'
	tc.struct_field_c_abi_fns['C.Holder\ntype'] = 'fn_ptr:int|int'
	tc.struct_field_c_abi_fns['C.Holder\n@type'] = 'fn_ptr:int|void*'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.struct_field_c_abi_fn_ptr_type('Holder', '@type') == none
	assert g.struct_field_c_abi_fn_ptr_type('C.Holder', '@type') or { '' } == 'fn_ptr:int|void*'
}
