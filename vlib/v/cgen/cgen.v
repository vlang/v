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
	for expr in program.exprs {
		g.expr(expr)
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
		ast.FnDecl {
			g.writeln('$it.typ.name ${it.name}() { ')
			for expr in it.exprs {
				g.expr(expr)
			}
			g.writeln('}')
		}
		ast.Return {
			g.write('return ')
			g.expr(it.expr)
			g.writeln(';')
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
			// if typ.name != typ2.name {
			// verror('bad types $typ.name $typ2.name')
			// }
		}
		ast.VarDecl {
			g.write('$it.typ.name $it.name = ')
			g.expr(it.expr)
			g.writeln(';')
		}
		else {
			println('bad node')
		}
	}
}

fn verror(s string) {
	println(s)
	exit(1)
}
