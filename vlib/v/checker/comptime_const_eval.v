module checker

import v.ast

fn eval_comptime_const_expr(expr ast.Expr, nlevel int) ?ast.ComptTimeConstValue {
	if nlevel > 100 {
		// protect against a too deep comptime eval recursion
		return none
	}
	x := expr
	match expr {
		ast.IntegerLiteral {
			return expr.val.i64()
		}
		ast.StringLiteral {
			return expr.val
		}
		ast.CastExpr {
			cast_expr_value := eval_comptime_const_expr(expr.expr, nlevel + 1) or { return none }
			if expr.typ == ast.byte_type {
				return cast_expr_value.byte() or { return none }
			}
			if expr.typ == ast.int_type {
				match cast_expr_value {
					byte {
						eprintln('>>>>>>> byte cast_expr_value: $cast_expr_value | x: $x')
						return i64(cast_expr_value)
					}
					i64 {
						eprintln('>>>>>>> i64 cast_expr_value: $cast_expr_value | x: $x')
						if int_min <= cast_expr_value && cast_expr_value <= int_max {
							return i64(cast_expr_value)
						}
					}
					else {}
				}
				return none
			}
		}
		ast.InfixExpr {
			left := eval_comptime_const_expr(expr.left, nlevel + 1) ?
			right := eval_comptime_const_expr(expr.right, nlevel + 1) ?
			if left is string && right is string {
				match expr.op {
					.plus {
						return left + right
					}
					else {
						return none
					}
				}
			} else if left is i64 && right is i64 {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return left << right }
					.right_shift { return left >> right }
					else { return none }
				}
			} else if left is byte && right is byte {
				match expr.op {
					.plus { return left + right }
					.minus { return left - right }
					.mul { return left * right }
					.div { return left / right }
					.mod { return left % right }
					.xor { return left ^ right }
					.pipe { return left | right }
					.amp { return left & right }
					.left_shift { return left << right }
					.right_shift { return left >> right }
					else { return none }
				}
			}
		}
		ast.Ident {
			if expr.obj is ast.ConstField {
				// an existing constant?
				return eval_comptime_const_expr(expr.obj.expr, nlevel + 1)
			}
		}
		else {
			// eprintln('>>> nlevel: $nlevel | another $expr.type_name() | $expr ')
			return none
		}
	}
	return none
}
