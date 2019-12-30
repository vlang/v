module cgen

import (
	strings
	v.ast
	term
)

struct Gen {
	out strings.Builder
}

pub fn gen(program ast.Program) string {
	mut g := Gen{
		out: strings.new_builder(100)
	}
	for stmt in program.stmts {
		g.stmt(stmt)
		g.writeln('')
	}
	return (g.out.str())
}

pub fn (g &Gen) save() {}

pub fn (g mut Gen) write(s string) {
	g.out.write(s)
}

pub fn (g mut Gen) writeln(s string) {
	g.out.writeln(s)
}

fn (g mut Gen) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			g.expr(it.left)
			g.write(' $it.op.str() ')
			g.expr(it.right)
			g.writeln(';')
		}
		ast.FnDecl {
			g.write('$it.typ.name ${it.name}(')
			for arg in it.args {
				g.write(arg.typ.name + ' ' + arg.name)
			}
			g.writeln(') { ')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		ast.Return {
			g.write('return ')
			g.expr(it.expr)
			g.writeln(';')
		}
		ast.VarDecl {
			g.write('$it.typ.name $it.name = ')
			g.expr(it.expr)
			g.writeln(';')
		}
		ast.ExprStmt {
			g.expr(it.expr)
			match it.expr {
				// no ; after an if expression
				ast.IfExpr {}
				else {
					g.writeln(';')
				}
	}
		}
		else {
			verror('stmt bad node')
		}
	}
}

fn (g mut Gen) expr(node ast.Expr) {
	// println('cgen expr()')
	match node {
		ast.IntegerLiteral {
			g.write(it.val.str())
		}
		ast.FloatLiteral {
			g.write(it.val)
		}
		ast.UnaryExpr {
			g.expr(it.left)
			g.write(it.op.str())
		}
		ast.StringLiteral {
			g.write('tos3("$it.val")')
		}
		ast.BinaryExpr {
			g.expr(it.left)
			match it.op {
				.plus {
					g.write(' + ')
				}
				.minus {
					g.write(' - ')
				}
				.mul {
					g.write(' * ')
				}
				.div {
					g.write(' / ')
				}
				.plus_assign {
					g.write(' += ')
				}
				else { g.write(it.op.str()) }
	}
			g.expr(it.right)
			// if it.op in [.plus_assign] {
			// g.writeln(';')
			// }
			// if typ.name != typ2.name {
			// verror('bad types $typ.name $typ2.name')
			// }
		}
		ast.CallExpr {
			g.write('${it.name}(')
			for i, expr in it.args {
				g.expr(expr)
				if i != it.args.len - 1 {
					g.write(', ')
				}
			}
			g.write(')')
		}
		ast.Ident {
			g.write('$it.name')
		}
		ast.BoolLiteral {
			if it.val == true {
				g.write('true')
			}
			else {
				g.write('false')
			}
		}
		ast.IfExpr {
			g.write('if (')
			g.expr(it.cond)
			g.writeln(') {')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		else {
			println(term.red('cgen.expr(): bad node'))
		}
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
