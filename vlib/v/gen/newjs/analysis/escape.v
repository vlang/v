module analysis 

import v.ast 
import v.ast.walker


pub fn escaping_objects(n &ast.Node) []&ast.Var {
	mut v := EscapeAnalysis {
		escaping: map[voidptr]bool{}
		bottom_scopes: map[voidptr]bool{}
	}

	walker.walk(mut v, n)

	mut list := []&ast.Var{}

	for obj, _ in v.escaping {
		list << &ast.Var(obj)
	}

	return list
}

struct EscapeAnalysis {
mut:
	escaping map[voidptr]bool 
	top_scope &ast.Scope = unsafe { nil }
	bottom_scopes map[voidptr]bool

}

fn (mut v EscapeAnalysis) visit(node &ast.Node) ! {
	match node {
		ast.Expr {
			if node is ast.PostfixExpr {
				if node.op == .amp {
					if node.expr is ast.Ident {
						mut collector := EscapingObjectCollector {
							analysis: unsafe { v }
						}
						walker.walk(mut collector, &ast.Node(ast.Expr(node.expr)))
					}
				}
			}
		}

		ast.Stmt {
			match node {
				ast.FnDecl {
					v.bottom_scopes[voidptr(node.scope)] = true
					mut collector := EscapingObjectCollector {
						analysis: unsafe { v }
					}
					walker.walk(mut collector, &ast.Node(ast.Stmt(node)))
					return 
				}

				ast.ForStmt {
					v.bottom_scopes[voidptr(node.scope)] = true
					return 
				}

				else {
					return
				}
			}
		} else {
			return
		}
	}
}

struct EscapingObjectCollector {
mut:
	analysis &EscapeAnalysis
}

fn (mut v EscapingObjectCollector) visit(node &ast.Node) ! {
	if node is ast.Expr {
		match node {
			ast.Ident {
				if node.obj is ast.Var {
					mut s := node.scope 

					for unsafe { s != nil } {
						if s == v.analysis.top_scope {
							v.analysis.escaping[voidptr(unsafe { &node })] = true
							break
						}

						if v.analysis.bottom_scopes[voidptr(s)] {
							break
						}
						s = s.parent 
					}
				}
			} else {

			}
		}
	}
}