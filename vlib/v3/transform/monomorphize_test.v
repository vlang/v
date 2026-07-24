module transform

import v3.flat
import v3.types

fn test_materialized_generic_struct_fields_preserve_plain_alias_arguments() {
	mut a := flat.FlatAst.new()
	mut values_alias := flat.Node{
		kind:  .type_decl
		value: 'Values'
		typ:   '[]T'
	}
	values_alias.set_generic_params(['T'])
	a.add_node(values_alias)

	value_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   'T'
	})
	values_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'values'
		typ:   'Values[T]'
	})
	children_start := a.children.len
	a.children << value_field
	a.children << values_field
	mut box_decl := flat.Node{
		kind:           .struct_decl
		value:          'Box'
		children_start: children_start
		children_count: 2
	}
	box_decl.set_generic_params(['T'])
	box_id := a.add_node(box_decl)

	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.type_aliases['UserId'] = 'int'
	tc.type_aliases['Values'] = '[]T'
	tc.type_alias_generic_params['Values'] = ['T']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.materialize_generic_struct_spec('Box[UserId]', GenericStructDecl{
		id:     box_id
		node:   box_decl
		module: 'main'
		key:    'Box'
	})

	fields := tc.structs['Box[UserId]'] or {
		assert false, 'missing materialized Box[UserId] fields'
		return
	}
	assert fields.len == 2
	assert fields[0].typ is types.Alias
	assert fields[0].typ.name() == 'UserId'
	assert fields[1].typ is types.Array
	assert fields[1].typ.name() == '[]int'
}

fn test_lock_colliding_main_generic_type_text_locks_args_behind_qualified_base() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// The program (main) declares `Context` (bare-keyed); the `callee` module declares a
	// colliding `Context` of its own.
	t.structs['Context'] = StructInfo{}
	t.structs['callee.Context'] = StructInfo{}

	// A bare program type substituted into a specialization is locked to `main.`.
	assert t.lock_colliding_main_generic_type_text('Context', 'callee') == 'main.Context'
	// A qualified generic base with a bare program argument (`other.Box[Context]`) must
	// still lock the nested `Context`, not return early on the qualified spelling.
	assert t.lock_colliding_main_generic_type_text('other.Box[Context]', 'callee') == 'other.Box[main.Context]'
	// The same nested program type behind a map / fixed array is locked too.
	assert t.lock_colliding_main_generic_type_text('map[string]Context', 'callee') == 'map[string]main.Context'
	assert t.lock_colliding_main_generic_type_text('[3]Context', 'callee') == '[3]main.Context'
	// A simple qualified type has no lockable bare component and is returned verbatim.
	assert t.lock_colliding_main_generic_type_text('veb.Context', 'callee') == 'veb.Context'
	// An already-qualified argument keeps its exact spelling (no rewrite / name desync).
	assert t.lock_colliding_main_generic_type_text('other.Box[user.LocalWriter]', 'callee') == 'other.Box[user.LocalWriter]'
	// No lock target: the callee module does not declare `Other`, so nothing changes.
	assert t.lock_colliding_main_generic_type_text('other.Box[Other]', 'callee') == 'other.Box[Other]'
}
