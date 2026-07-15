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
