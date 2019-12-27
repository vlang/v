module cgen

import (
	strings
	v.ast
)

struct Gen {
	out strings.Builder

}

pub fn gen(program ast.Program) string {
	mut g := Gen{out:strings.new_builder(100)}
	for expr in program.exprs {
		g.expr(expr)
		g.writeln('')
	}
	return (g.out.str())
}

pub fn (g &Gen) save() {

}

pub fn (g mut Gen) write(s string) {
	g.out.write(s)
}

pub fn (g mut Gen) writeln(s string) {
	g.out.writeln(s)
}

struct Type {
	name string
}

const (
	string_type = Type{'string'}
	int_type = Type{'int'}
	void_type = Type{'void'}
)

fn (g mut Gen) expr(node ast.Expr) {
	//println('cgen expr()')
	match node {
		ast.IntegerLiteral {
			g.write(it.val.str())
		}
		ast.UnaryExpr {
			g.expr(it.left)
			g.write(' $it.op ')
		}
		ast.StringLiteral {
			g.write('"$it.val"')
		}
		ast.BinaryExpr {
			g.expr(it.left)
			match it.op {
				.plus {	g.write(' + ')	}
				.minus {	g.write(' - ')	}
				.mul {	g.write(' * ')	}
				.div {	g.write(' / ')	}
				else {}
			}
			g.expr(it.right)
		//	if typ.name != typ2.name {
				//verror('bad types $typ.name $typ2.name')
			//}
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

