module parser

import v.ast

fn test_check_cross_variables_skips_unresolved_call_arg_stringification() {
	mut p := Parser{}
	left := [ast.Expr(ast.Ident{
		name: 'size'
	}), ast.Expr(ast.Ident{
		name: 'align'
	})]
	right := ast.Expr(ast.CallExpr{
		name: 'type_size'
		args: [
			ast.CallArg{
				expr: ast.ArrayInit{
					elem_type: ast.Type(45)
				}
			},
		]
	})
	assert !p.check_cross_variables(left, right)
}
