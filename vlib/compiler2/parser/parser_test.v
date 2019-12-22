module parser

import compiler2.ast

fn test_parser() {
	//expr := ast.IntegerExpr {val:10}
	//expr := ast.BinaryExpr{}
	expr := parse_expr('3 + 7')
	walk(expr)
	println('')
}

fn walk(node ast.Expr) {
	//println('walk()')
	match node {
		ast.IntegerExpr {
			print(it.val)
		}
		ast.BinaryExpr {
			walk(it.left)
			match it.op {
				.plus {
					print(' + ')
				}
				.minus {
					print(' - ')
				}
				else {}

			}
			walk(it.right)
		}
		else {}
	}
}
