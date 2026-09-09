module transform

import v3.flat
import v3.types

// build_namespace_call assembles `<base>.<method>(if cond { 1 } else { 2 })`, where `base` is an
// identifier the checker typed at `base_type`. The value `if` argument is what makes the
// call-operand ordering guards look at the preceding operand — here the callee base.
fn build_namespace_call(mut t Transformer, base_name string, base_type string, method string) (flat.NodeId, flat.Node) {
	base := t.a.add_val(.ident, base_name)
	t.set_node_typ(int(base), base_type)
	callee := t.make_selector_op(base, method, 'string', .dot)

	cond := t.a.add_val(.bool_literal, 'true')
	then_val := t.a.add_val(.int_literal, '1')
	else_val := t.a.add_val(.int_literal, '2')
	branch_start := t.a.children.len
	t.a.children << cond
	t.a.children << then_val
	t.a.children << else_val
	arg := t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: branch_start
		children_count: 3
		typ: 'int'
	})

	call_start := t.a.children.len
	t.a.children << callee
	t.a.children << arg
	call := t.a.add_node(flat.Node{
		kind: .call
		children_start: call_start
		children_count: 2
		value: '${base_name}.${method}'
	})
	return call, t.a.nodes[int(call)]
}

fn ordering_snapshot_decl_count(t &Transformer) int {
	mut n := 0
	for stmt in t.pending_stmts {
		node := t.a.nodes[int(stmt)]
		if node.kind != .decl_assign || node.children_count == 0 {
			continue
		}
		lhs := t.a.nodes[int(t.a.child(&node, 0))]
		if lhs.kind == .ident && lhs.value.starts_with('__order_snapshot') {
			n++
		}
	}
	return n
}

// Direct coverage of the backstop. call_selector_base_is_namespace recognizes every namespace
// spelling that reaches it in practice, so the base here is deliberately absent from `tc.imports`
// and from the file-import table: the classifier genuinely misses it (asserted below), leaving
// only the backstop to keep the spill out of the output. Removing the check in
// transform_call_expr fails this test.
//
// The guards must leave such a base inline rather than declare a temp at a name no C declaration
// can use (`unknown __order_snapshot_0 = os;`, `void __order_snapshot_0 = shapes__Box;`).
fn test_untyped_callee_base_is_not_spilled_into_an_ordering_temp() {
	for base_type in ['unknown', 'void'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.cur_module = 'main'

		call, node := build_namespace_call(mut t, 'nsbase', base_type, 'abs_path')
		// `call_selector_base_is_namespace` takes the *base* of the callee selector, so unwrap the
		// callee first — passing the callee itself would assert about the wrong node and could
		// read false even for a base the classifier does recognize.
		callee := t.a.nodes[int(t.a.child(&node, 0))]
		assert callee.kind == .selector
		assert !t.call_selector_base_is_namespace(t.a.child(&callee, 0), 'abs_path', node.value), 'the classifier must miss this base, otherwise the backstop is not what is under test'

		t.transform_call_expr(call, node)
		assert ordering_snapshot_decl_count(&t) == 0, 'a `${base_type}` callee base must not be spilled into an ordering temp'
	}
}

// The same call shape with a base at an ordinary runtime type must still be snapshotted, so the
// backstop does not weaken the ordering guarantee the guards exist to provide.
fn test_runtime_callee_base_is_still_spilled_into_an_ordering_temp() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.set_var_type('holder', 'Holder')
	t.structs['Holder'] = StructInfo{
		name: 'Holder'
	}

	call, node := build_namespace_call(mut t, 'holder', 'Holder', 'render')
	t.transform_call_expr(call, node)
	assert ordering_snapshot_decl_count(&t) == 1, 'a runtime receiver preceding a value branch must keep its source-order snapshot'
}

// The case the backstop must not swallow: a runtime receiver the checker could not type still
// reads back as `unknown`, but its binding proves it is a value, so the snapshot must survive.
fn test_untyped_but_bound_receiver_is_still_spilled_into_an_ordering_temp() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	t.set_var_type('cloned', 'Holder')
	t.structs['Holder'] = StructInfo{
		name: 'Holder'
	}

	call, node := build_namespace_call(mut t, 'cloned', 'unknown', 'render')
	t.transform_call_expr(call, node)
	assert ordering_snapshot_decl_count(&t) == 1, 'a bound receiver must keep its snapshot even when the checker lost its type'
}

// The classification is positive: `void` is conclusive, `unknown` counts only for an identifier
// that names nothing in scope. An unresolved *runtime* operand also reads back as `unknown`, so
// the bound cases below are what keep the backstop from weakening the ordering guards.
fn test_callee_base_runtime_value_classification() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})

	// A module identifier: unresolved and not a binding.
	module_base := t.a.add_val(.ident, 'os')
	t.set_node_typ(int(module_base), 'unknown')
	assert t.callee_base_is_not_a_runtime_value(module_base)

	// A type name reads back as `void` whatever its spelling.
	type_base := t.a.add_val(.ident, 'Box')
	t.set_node_typ(int(type_base), 'void')
	assert t.callee_base_is_not_a_runtime_value(type_base)

	// A runtime receiver whose type the checker lost — the case a generic clone produces. Its
	// binding survives, so it must stay snapshotted.
	t.set_var_type('cloned', 'Holder')
	cloned_base := t.a.add_val(.ident, 'cloned')
	t.set_node_typ(int(cloned_base), 'unknown')
	assert !t.callee_base_is_not_a_runtime_value(cloned_base)

	// An unresolved operand that is not a bare identifier stays snapshotted too.
	inner := t.a.add_val(.ident, 'holder')
	field_base := t.make_selector_op(inner, 'field', 'unknown', .dot)
	assert !t.callee_base_is_not_a_runtime_value(field_base)

	typed_base := t.a.add_val(.ident, 'builder')
	t.set_node_typ(int(typed_base), 'strings.Builder')
	assert !t.callee_base_is_not_a_runtime_value(typed_base)
}
