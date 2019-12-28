module cgen

import (
	strings
	v.ast
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
			g.writeln('$it.typ.name ${it.name}() { ')
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
			g.writeln(';')
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
			g.write(' $it.op ')
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
				else {}
	}
			g.expr(it.right)
			// if it.op in [.plus_assign] {
			// g.writeln(';')
			// }
			// if typ.name != typ2.name {
			// verror('bad types $typ.name $typ2.name')
			// }
		}
		ast.Ident {
			g.write('$it.name')
		}
		else {
			println('cgen.expr(): bad node')
		}
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
