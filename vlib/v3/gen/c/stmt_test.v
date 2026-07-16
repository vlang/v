module c

import v3.flat
import v3.types

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
