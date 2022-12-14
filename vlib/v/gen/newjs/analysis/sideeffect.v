module analysis

import v.ast 
import v.ast.walker

struct HasSideEffectVisitor {
mut: 
	has_sideeffect bool 
}

pub fn has_sideeffect(n &ast.Node) bool {
	mut v := HasSideEffectVisitor{has_sideeffect: false}
	walker.walk(mut v, n)
	return v.has_sideeffect
}

fn (mut v HasSideEffectVisitor) visit(node &ast.Node) ! {
	if v.has_sideeffect {
		return 
	}

	if node is ast.Expr {
		match node {
			ast.InfixExpr {
				if node.op == .arrow {
					v.has_sideeffect = true 
					return
				}
			}
			ast.CallExpr {
				v.has_sideeffect = true 
				return
			}
			// todo: overloaded operators can lead to side effects as well?
			else {}
		}
	}
}

