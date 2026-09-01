module types

import v3.flat

// build_guarded_logical_condition assembles `infix(op, (value := opt), true)`,
// i.e. an if-guard combined with a logical operator, and returns the bindings
// check_condition would export into the then-branch scope.
fn build_guarded_logical_condition(op flat.Op) []LocalBinding {
	mut a := flat.FlatAst.new()
	value_ident := a.add_val(.ident, 'value')
	opt_ident := a.add_val(.ident, 'opt')

	guard_children := a.begin_children()
	a.add_child(value_ident)
	a.add_child(opt_ident)
	guard_id := a.add_node(flat.Node{
		kind:           .decl_assign
		op:             .assign
		children_start: guard_children
		children_count: 2
	})

	true_lit := a.add_val(.bool_literal, 'true')
	cond_children := a.begin_children()
	a.add_child(guard_id)
	a.add_child(true_lit)
	cond_id := a.add_node(flat.Node{
		kind:           .infix
		op:             op
		children_start: cond_children
		children_count: 2
	})

	mut tc := TypeChecker.new(&a)
	tc.cur_scope = new_scope(tc.file_scope)
	tc.cur_scope.insert_with_owner('opt', Type(OptionType{
		base_type: Type(int_)
	}))
	tc.valid_resolution_fast = true
	return tc.check_condition(cond_id)
}

// A `&&` guard holds for the whole true condition, so its payload binding is
// exported into the then-branch.
fn test_and_guard_exports_its_binding() {
	bindings := build_guarded_logical_condition(.logical_and)
	assert bindings.len == 1
	assert bindings[0].name == 'value'
}

// An `||` guard short-circuits: `value` may be unbound when the branch is taken
// via the right operand, so its binding must never reach the then-branch.
fn test_or_guard_does_not_export_its_binding() {
	bindings := build_guarded_logical_condition(.logical_or)
	assert bindings.len == 0
}
