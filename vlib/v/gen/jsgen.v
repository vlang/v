module gen

import (
	strings
	v.ast
	v.table
	term
)

struct JsGen {
	out   strings.Builder
	table &table.Table
}

pub fn jsgen(program ast.File, table &table.Table) string {
	mut g := JsGen{
		out: strings.new_builder(100)
		table: table
	}
	for stmt in program.stmts {
		g.stmt(stmt)
		g.writeln('')
	}
	return (g.out.str())
}

pub fn (g &JsGen) save() {}

pub fn (g mut JsGen) write(s string) {
	g.out.write(s)
}

pub fn (g mut JsGen) writeln(s string) {
	g.out.writeln(s)
}

fn (g mut JsGen) stmt(node ast.Stmt) {
	match node {
		ast.FnDecl {
			type_sym := g.table.get_type_symbol(it.typ)
			g.write('/** @return { $type_sym.name } **/\nfunction ${it.name}(')
			for arg in it.args {
				arg_type_sym := g.table.get_type_symbol(arg.typ)
				g.write(' /** @type { $arg_type_sym.name } **/ $arg.name')
			}
			g.writeln(') { ')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		ast.Return {
			g.write('return ')
			if it.exprs.len > 0 {}
			else {
				g.expr(it.exprs[0])
			}
			g.writeln(';')
		}
		ast.AssignStmt {}
		ast.VarDecl {
			type_sym := g.table.get_type_symbol(it.typ)
			g.write('var /* $type_sym.name */ $it.name = ')
			g.expr(it.expr)
			g.writeln(';')
		}
		ast.ForStmt {
			g.write('while (')
			g.expr(it.cond)
			g.writeln(') {')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		ast.StructDecl {
			// g.writeln('typedef struct {')
			// for field in it.fields {
			// g.writeln('\t$field.ti.name $field.name;')
			// }
			g.writeln('var $it.name = function() {};')
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
			verror('jsgen.stmt(): bad node')
		}
	}
}

fn (g mut JsGen) expr(node ast.Expr) {
	// println('cgen expr()')
	match node {
		ast.IntegerLiteral {
			g.write(it.val.str())
		}
		ast.FloatLiteral {
			g.write(it.val)
		}
		/*
		ast.UnaryExpr {
			g.expr(it.left)
			g.write(' $it.op ')
		}
		*/

		ast.StringLiteral {
			g.write('tos3("$it.val")')
		}
		ast.InfixExpr {
			g.expr(it.left)
			g.write(' $it.op.str() ')
			g.expr(it.right)
		}
		// `user := User{name: 'Bob'}`
		ast.StructInit {
			type_sym := g.table.get_type_symbol(it.typ)
			g.writeln('/*$type_sym.name*/{')
			for i, field in it.fields {
				g.write('\t$field : ')
				g.expr(it.exprs[i])
				g.writeln(', ')
			}
			g.write('}')
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
			println(term.red('jsgen.expr(): bad node'))
		}
	}
}
