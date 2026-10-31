module transform

import v.flat
import v.types

fn test_sum_reference_rvalues_use_scoped_storage() {
	for explicit in [false, true] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		sum := flat.source_type_id('Choice')
		pointer := flat.source_type_id('&Choice')
		tc.sum_types.set(sum, ['int', 'string'])
		tc.fn_ret_types['make_value'] = types.SumType{ name_id: sum }
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.sum_types.set(sum, ['int', 'string'])
		t.fn_ret_types.set_id('make_value', sum)
		value := t.make_call_typed('make_value', [], sum)
		argument := if explicit { t.make_prefix(.amp, value) } else { value }
		lowered := t.transform_call_arg_for_param(argument, pointer)
		assert t.pending_stmts.len == 1
		declaration := a.node(t.pending_stmts[0])
		assert declaration.kind == .decl_assign
		assert declaration.typ == sum
		assert a.child_node(declaration, 1).kind == .call
		address := a.node(lowered)
		assert address.kind == .prefix && address.op == .amp
		assert a.child_node(address, 0).kind == .ident
		assert address.typ == pointer
	}
}
