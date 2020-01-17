module gen

import (
	strings
	v.ast
	v.table
	// v.types
	term
)

struct Gen {
	out         strings.Builder
	definitions strings.Builder // typedefs, defines etc (everything that goes to the top of the file)
	table       &table.Table
mut:
	fn_decl     &ast.FnDecl // pointer to the FnDecl we are currently inside otherwise 0
}

pub fn cgen(files []ast.File, table &table.Table) string {
	println('start cgen')
	mut g := Gen{
		out: strings.new_builder(100)
		definitions: strings.new_builder(100)
		table: table
		fn_decl: 0
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
	// println('cgen.stmt()')
	// g.writeln('//// stmt start')
	match node {
		ast.Import {}
		ast.FnDecl {
			g.fn_decl = &it
			is_main := it.name == 'main'
			if is_main {
				g.write('int ${it.name}(')
			}
			else {
				t := g.table.get_type(it.ti.idx)
				g.write('$t.name ${it.name}(')
				g.definitions.write('$t.name ${it.name}(')
			}
			for i, arg in it.args {
				t := g.table.get_type(arg.ti.idx)
				g.write(t.name + ' ' + arg.name)
				if i < it.args.len - 1 {
					g.write(', ')
				}
				g.definitions.write(t.name + ' ' + arg.name)
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
			g.fn_decl = 0
		}
		ast.Return {
			g.write('return ')
			// multiple returns
			if it.exprs.len > 1 {
				t := g.table.get_type(g.fn_decl.ti.idx)
				g.write('($t.name){')
				for i, expr in it.exprs {
					g.write('.arg$i=')
					g.expr(expr)
					if i < it.exprs.len - 1 {
						g.write(',')
					}
				}
				g.write('}')
			}
			// normal return
			else {
				g.expr(it.exprs[0])
			}
			g.writeln(';')
		}
		ast.VarDecl {
			t := g.table.get_type(it.ti.idx)
			g.write('$t.name $it.name = ')
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
		ast.ForCStmt {
			g.write('for (')
			g.stmt(it.init)
			// g.write('; ')
			g.expr(it.cond)
			g.write('; ')
			g.stmt(it.inc)
			g.writeln(') {')
			for stmt in it.stmts {
				g.stmt(stmt)
			}
			g.writeln('}')
		}
		ast.StructDecl {
			g.writeln('typedef struct {')
			for field in it.fields {
				t := g.table.get_type(field.ti.idx)
				g.writeln('\t$t.name $field.name;')
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
		ast.AssignExpr {
			g.expr(it.left)
			g.write(' $it.op.str() ')
			g.expr(it.val)
		}
		ast.IntegerLiteral {
			g.write(it.val.str())
		}
		ast.FloatLiteral {
			g.write(it.val)
		}
		ast.PostfixExpr {
			g.expr(it.expr)
			g.write(it.op.str())
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
		ast.PrefixExpr {
			g.write(it.op.str())
			g.expr(it.right)
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
			t := g.table.get_type(it.ti.idx)
			g.writeln('($t.name){')
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
		ast.MethodCallExpr {}
		ast.ArrayInit {
			t := g.table.get_type(it.ti.idx)
			g.writeln('new_array_from_c_array($it.exprs.len, $it.exprs.len, sizeof($t.name), {\t')
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
		ast.IndexExpr {
			g.expr(it.left)
			g.write('[')
			g.expr(it.index)
			g.write(']')
		}
		ast.IfExpr {
			// If expression? Assign the value to a temp var.
			// Previously ?: was used, but it's too unreliable.
			t := g.table.get_type(it.ti.idx)
			mut tmp := ''
			if t.kind != .void {
				tmp = g.table.new_tmp_var()
				// g.writeln('$it.ti.name $tmp;')
			}
			g.write('if (')
			g.expr(it.cond)
			g.writeln(') {')
			for i, stmt in it.stmts {
				// Assign ret value
				if i == it.stmts.len - 1 && t.kind != .void {
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
