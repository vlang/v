module gen

import (
	strings
	v.ast
	v.table
	term
)

struct Gen {
	out         strings.Builder
	definitions strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	table       &table.Table
}

pub fn cgen(files []ast.File, table &table.Table) string {
	mut g := Gen{
		out: strings.new_builder(100)
		definitions: strings.new_builder(100)
		table: table
	}
	for file in files {
		for stmt in file.stmts {
			g.stmt(stmt)
			g.writeln('')
		}
	}
	return g.definitions.str() + g.out.str()
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
		ast.Import {
			
		}
		ast.AssignStmt {
			g.expr(it.left)
			g.write(' $it.op.str() ')
			g.expr(it.right)
			g.writeln(';')
		}
		ast.FnDecl {
			is_main := it.name == 'main'
			if is_main {
				g.write('int ${it.name}(')
			}
			else {
				g.write('$it.ti.type_name ${it.name}(')
				g.definitions.write('$it.ti.type_name ${it.name}(')
			}
			for arg in it.args {
				g.write(arg.ti.type_name + ' ' + arg.name)
				g.definitions.write(arg.ti.type_name + ' ' + arg.name)
			}
			g.writeln(') { ')
			if !is_main {
				g.definitions.writeln(');')
			}
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			if is_main {
				g.writeln('return 0;')
			}
			g.writeln('}')
		}
		ast.Return {
			g.write('return ')
			g.expr(it.expr)
			g.writeln(';')
		}
		ast.VarDecl {
			g.write('$it.ti.type_name $it.name = ')
			g.expr(it.expr)
			g.writeln(';')
		}
		ast.ForStmt {
			g.write('while (')
			if !it.is_in {
				// `for in` loops don't have a condition
				println('it cond')
				g.expr(it.cond)
			}
			g.writeln(') {')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		ast.StructDecl {
			g.writeln('typedef struct {')
			for field in it.fields {
				g.writeln('\t$field.ti.type_name $field.name;')
			}
			g.writeln('} $it.name;')
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
			verror('cgen.stmt(): bad node')
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
			// probably not :D
			if it.op in [.inc, .dec] {
				g.expr(it.left)
				g.write(it.op.str())
			}
			else {
				g.write(it.op.str())
				g.expr(it.left)
			}
		}
		ast.StringLiteral {
			g.write('tos3("$it.val")')
		}
		ast.BinaryExpr {
			g.expr(it.left)
			if it.op == .dot {
				println('!! dot')
			}
			g.write(' $it.op.str() ')
			g.expr(it.right)
			// if typ.name != typ2.name {
			// verror('bad types $typ.name $typ2.name')
			// }
		}
		// `user := User{name: 'Bob'}`
		ast.StructInit {
			g.writeln('($it.ti.type_name){')
			for i, field in it.fields {
				g.write('\t.$field = ')
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
		ast.ArrayInit {
			g.writeln('new_array_from_c_array($it.exprs.len, $it.exprs.len, sizeof($it.ti.type_name), {\t')
			for expr in it.exprs {
				g.expr(expr)
				g.write(', ')
			}
			g.write('\n})')
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
		ast.SelectorExpr {
			g.expr(it.expr)
			g.write('.')
			g.write(it.field)
		}
		ast.IfExpr {
			// If expression? Assign the value to a temp var.
			// Previously ?: was used, but it's too unreliable.
			mut tmp := ''
			if it.ti.type_kind != ._void {
				tmp = g.table.new_tmp_var()
				// g.writeln('$it.typ.name $tmp;')
			}
			g.write('if (')
			g.expr(it.cond)
			g.writeln(') {')
			for i, stmt in it.stmts {
				// Assign ret value
				if i == it.stmts.len - 1 && it.ti.type_kind != ._void {
					// g.writeln('$tmp =')
					println(1)
				}
				g.stmt(stmt)
			}
			g.writeln('}')
			if it.else_stmts.len > 0 {
				g.writeln('else { ')
				for stmt in it.else_stmts {
					g.stmt(stmt)
				}
				g.writeln('}')
			}
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
