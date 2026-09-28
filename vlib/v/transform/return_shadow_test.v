module transform

import v.flat
import v.types

fn test_shadowed_err_in_value_block_is_result_payload() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type_binding('err', 'IError', 'IError', true)
	err_id := t.make_ident('err')
	t.set_node_typ(int(err_id), 'IError')
	assert t.return_expr_is_propagated_err(err_id, 'IError')
	decl := t.make_decl_assign('err', t.make_ident('payload'))
	block := t.make_block([decl, t.make_expr_stmt(err_id)])
	assert !t.return_expr_is_propagated_err(block, 'IError')
}
