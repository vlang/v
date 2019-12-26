module cgen

import (
	strings
	compiler2.ast
)

struct Gen {
	out strings.Builder

}

pub fn gen(program ast.Program) {
	mut g := Gen{out:strings.new_builder(100)}
	for expr in program.exprs {
		g.expr(expr)
		g.writeln('')
	}
	println(g.out.str())
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

fn (g mut Gen) expr(node ast.Expr) Type {
	//println('cgen expr()')
	match node {
		ast.IntegerExpr {
			g.write(it.val.str())
			return int_type
		}
		ast.StringLiteral {
			g.write('"$it.val"')
			return string_type
		}
		ast.BinaryExpr {
			typ := g.expr(it.left)
			match it.op {
				.plus {	g.write(' + ')	}
				.minus {	g.write(' - ')	}
				.mul {	g.write(' * ')	}
				.div {	g.write(' / ')	}
				else {}
			}
			typ2 := g.expr(it.right)
			if typ.name != typ2.name {
				println('bad types $typ.name $typ2.name')
			}
			return typ
		}
		ast.VarDecl {
			g.write('var $it.name = ')
			g.expr(it.expr)
			g.writeln(';')
			return void_type
		}
		else {
			println('bad node')
		}
	}
	return void_type
}

