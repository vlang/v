// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

// NOTE: this is just a very naive example of how it could possibly work.
// actual implementation may work during AST -> IR (or not). it may also
// need type information which we don't have here. as I said, just an example.
pub fn (match_expr &MatchExpr) desugar() Expr {
	mut if_expr := IfExpr{}
	for i, branch in match_expr.branches {
		mut branch_cond := empty_expr
		for cond in branch.cond {
			op := if cond in [Ident, SelectorExpr] { token.Token.key_is } else { token.Token.eq }
			c := InfixExpr{
				lhs: match_expr.expr
				op:  op
				rhs: cond
			}
			if branch_cond !is EmptyExpr {
				branch_cond = InfixExpr{
					lhs: branch_cond
					op:  .logical_or
					rhs: c
				}
			} else {
				branch_cond = c
			}
		}
		if_expr2 := IfExpr{
			cond:  branch_cond
			stmts: branch.stmts
		}
		if i == 0 {
			if_expr = if_expr2
		} else {
			if_expr = IfExpr{
				...if_expr
				else_expr: if_expr2
			}
		}
	}
	return if_expr
}

pub fn (or_expr &OrExpr) desugar() Expr {
	if or_expr.expr is IndexExpr {
		return IfExpr{
			// has index fn call
			// cond: CallExpr{
			// 	lhs: Ident{name: 'array_has_index'},
			// 	args: [or_expr.expr.lhs, or_expr.expr.expr]
			// }
			// array.len
			cond:      InfixExpr{
				lhs: SelectorExpr{
					lhs: or_expr.expr.lhs
					rhs: Ident{
						name: 'len'
					}
				}
				op:  .gt
				rhs: or_expr.expr.expr
			}
			stmts:     [ExprStmt{
				expr: or_expr.expr
			}]
			else_expr: IfExpr{
				stmts: or_expr.stmts
			}
		}
	} else {
		return IfExpr{
			cond:  InfixExpr{
				lhs: or_expr.expr
				op:  .eq
				rhs: BasicLiteral{
					kind: .key_true
				}
			}
			stmts: or_expr.stmts
		}
	}
}
