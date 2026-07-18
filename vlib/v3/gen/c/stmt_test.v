module c

import v3.flat
import v3.types

fn stmt_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn stmt_test_prefix(mut a flat.FlatAst, op flat.Op, child flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << child
	return a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: i32(start)
		children_count: 1
	})
}

fn test_local_pointer_alias_clear_preserves_outer_branch_markers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	ptr_type := types.Type(types.Pointer{
		base_type: types.Type(types.int_)
	})

	tc.push_scope()
	outer_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	g.declare_local_pointer_alias_source(outer_owner, 'x')
	assert g.local_pointer_alias_assignment_can_clear(outer_owner)

	tc.push_scope()
	g.enter_conditional_branch(true)
	assert !g.local_pointer_alias_assignment_can_clear(outer_owner)

	branch_owner := tc.cur_scope.insert_with_owner('q', ptr_type)
	g.declare_local_pointer_alias_source(branch_owner, 'y')
	assert g.local_pointer_alias_assignment_can_clear(branch_owner)

	tc.push_scope()
	assert g.local_pointer_alias_assignment_can_clear(branch_owner)
	nested_owner := tc.cur_scope.insert_with_owner('r', ptr_type)
	g.declare_local_pointer_alias_source(nested_owner, 'z')
	assert g.local_pointer_alias_assignment_can_clear(nested_owner)
	tc.pop_scope()

	g.leave_conditional_branch()
	tc.pop_scope()

	g.enter_conditional_branch(false)
	assert !g.local_pointer_alias_assignment_can_clear(outer_owner)
	g.leave_conditional_branch()

	assert g.local_pointer_alias_assignment_can_clear(outer_owner)
	tc.pop_scope()
}

fn test_fn_decl_signature_registration_preserves_call_name_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'pkg'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	name := 'Widget.call'
	full_name := qualify_name_in_module(tc.cur_module, name)
	params := [types.Type(types.int_)]
	shared_flags := [true]
	g.register_fn_decl_signature(name, full_name, params, shared_flags, true, true, 'int')
	aliases := [
		fn_decl_module_key(tc.cur_module, name),
		name,
		g.cname(name),
		'${tc.cur_module}.${name}',
		g.cname('${tc.cur_module}.${name}'),
	]
	for alias in aliases {
		assert g.fn_decl_param_types[alias] == params
		assert g.fn_decl_ret_types[alias] or { types.Type(types.void_) } == types.Type(types.int_)
		assert g.fn_decl_variadic[alias]
		if alias != fn_decl_module_key(tc.cur_module, name) {
			assert g.fn_decl_shared_params[alias] == shared_flags
			assert g.fn_decl_mut_receivers[alias]
		}
	}
}

fn test_local_pointer_alias_branch_assignment_merges_outer_markers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	arg_owner := tc.cur_scope.insert_with_owner('arg', int_type)
	g.cur_mut_params['arg'] = true
	g.cur_mut_param_owners['arg'] = arg_owner
	p_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	g.declare_local_pointer_alias_source(p_owner, 'x')

	tc.push_scope()
	g.enter_conditional_branch(true)
	arg_id := stmt_test_node(mut a, .ident, 'arg', [])
	amp_arg := stmt_test_prefix(mut a, .amp, arg_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_arg)
	assert g.local_pointer_alias_source('p') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()

	maybe_owner := tc.cur_scope.insert_with_owner('maybe', ptr_type)
	tc.push_scope()
	g.enter_conditional_branch(true)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'maybe'
	}, amp_x)
	assert g.local_pointer_alias_source('maybe') or { '' } == ''
	assert !g.local_pointer_alias_assignment_can_clear(maybe_owner)
	g.leave_conditional_branch()
	tc.pop_scope()

	g.declare_local_pointer_alias_source_kind(p_owner, 'arg', true)
	tc.push_scope()
	g.enter_conditional_branch(true)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_x)
	assert g.local_pointer_alias_source('p') or { '' } == 'arg'
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()
	tc.pop_scope()
}

fn test_local_pointer_alias_branch_assignment_without_outer_marker_stays_conditional() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	tc.cur_scope.insert_with_owner('p', ptr_type)
	tc.push_scope()
	g.enter_conditional_branch(true)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_x)
	assert g.local_pointer_alias_source('p') or { '' } == ''
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()
	tc.pop_scope()
}

fn test_pointer_alias_stack_source_propagates_identifier_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	p_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'p'
	}, p_owner, amp_x, ptr_type)
	assert g.local_pointer_alias_source('p') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('p')

	q_owner := tc.cur_scope.insert_with_owner('q', ptr_type)
	p_id := stmt_test_node(mut a, .ident, 'p', [])
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'q'
	}, q_owner, p_id, ptr_type)
	assert g.local_pointer_alias_source('q') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('q')

	assigned_owner := tc.cur_scope.insert_with_owner('assigned', ptr_type)
	g.declare_local_pointer_alias_source(assigned_owner, '')
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'assigned'
	}, p_id)
	assert g.local_pointer_alias_source('assigned') or { '' } == 'x'

	mut_owner := tc.cur_scope.insert_with_owner('mut_alias', ptr_type)
	g.declare_local_pointer_alias_source_kind(mut_owner, 'x', true)
	mut_id := stmt_test_node(mut a, .ident, 'mut_alias', [])
	mut_copy_owner := tc.cur_scope.insert_with_owner('mut_copy', ptr_type)
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'mut_copy'
	}, mut_copy_owner, mut_id, ptr_type)
	assert g.local_pointer_alias_source('mut_copy') or { '' } == 'x'
	assert g.local_pointer_alias_source_is_mut_param('mut_copy')
	tc.pop_scope()
}

fn test_heap_local_memdup_expr_uses_aligned_memdup_for_aligned_structs() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	g.register_struct_decl_info('Aligned', 'Aligned', 'main', '', flat.Node{
		value: 'Aligned'
		typ:   'aligned=64'
	})
	aligned_type := types.Type(types.Struct{
		name: 'Aligned'
	})
	pointer_copy := g.heap_local_memdup_expr('p', aligned_type, 'Aligned', true)
	assert pointer_copy == '(Aligned*)v3_aligned_memdup(p, sizeof(Aligned), 64)'
	value_copy := g.heap_local_memdup_expr('x', aligned_type, 'Aligned', false)
	assert value_copy == '(Aligned*)v3_aligned_memdup(&x, sizeof(Aligned), 64)'
	plain_type := types.Type(types.Struct{
		name: 'Plain'
	})
	assert g.heap_local_memdup_expr('p', plain_type, 'Plain', true) == '(Plain*)memdup(p, sizeof(Plain))'
}
