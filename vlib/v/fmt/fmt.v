// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import (
	v.ast
	v.table
	strings
)

const (
	tabs = ['', '\t', '\t\t', '\t\t\t', '\t\t\t\t', '\t\t\t\t\t', '\t\t\t\t\t\t']
	// tabs = ['', '  ', '    ', '      ', '        ']
)

struct Fmt {
	out        strings.Builder
	table      &table.Table
mut:
	indent     int
	empty_line bool
}

pub fn fmt(file ast.File, table &table.Table) string {
	mut f := Fmt{
		out: strings.new_builder(1000)
		table: table
		indent: -1
	}
	f.stmts(file.stmts)
	return f.out.str().trim_space() + '\n'
}

pub fn (f mut Fmt) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.out.write(tabs[f.indent])
	}
	f.out.write(s)
	f.empty_line = false
}

pub fn (f mut Fmt) writeln(s string) {
	if f.indent > 0 && f.empty_line {
		// println(f.indent.str() + s)
		f.out.write(tabs[f.indent])
	}
	f.out.writeln(s)
	f.empty_line = true
}

fn (f mut Fmt) stmts(stmts []ast.Stmt) {
	f.indent++
	for stmt in stmts {
		f.stmt(stmt)
	}
	f.indent--
}

fn (f mut Fmt) stmt(node ast.Stmt) {
	match node {
		ast.AssignStmt {
			for i, left in it.left {
				f.expr(left)
				if i < it.left.len - 1 {
					f.write(', ')
				}
			}
			f.write(' = ')
			for right in it.right {
				f.expr(right)
			}
		}
		ast.BranchStmt {
			match it.tok.kind {
				.key_break {
					f.writeln('break')
				}
				.key_continue {
					f.writeln('continue')
				}
				else {}
	}
		}
		ast.ConstDecl {
			f.writeln('const (')
			f.indent++
			for i, field in it.fields {
				f.write('$field.name = ')
				f.expr(it.exprs[i])
			}
			f.indent--
			f.writeln('\n)\n')
		}
		ast.ExprStmt {
			f.expr(it.expr)
			f.writeln('')
		}
		ast.FnDecl {
			mut receiver := ''
			if it.is_method {
				sym := f.table.get_type_symbol(it.receiver.typ)
				name := sym.name.after('.')
				m := if it.rec_mut { 'mut ' } else { '' }
				receiver = '($it.receiver.name ${m}$name) '
			}
			f.write('fn ${receiver}${it.name}(')
			for i, arg in it.args {
				is_last_arg := i == it.args.len - 1
				should_add_type := is_last_arg || it.args[i + 1].typ != arg.typ
				f.write(arg.name)
				if should_add_type {
					arg_typ_sym := f.table.get_type_symbol(arg.typ)
					f.write(' ${arg_typ_sym.name}')
				}
				if !is_last_arg {
					f.write(', ')
				}
			}
			f.write(')')
			if it.typ != table.void_type {
				sym := f.table.get_type_symbol(it.typ)
				f.write(' ' + sym.name)
			}
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}\n')
		}
		ast.ForStmt {
			f.write('for ')
			f.expr(it.cond)
			f.writeln(' {')
			f.stmts(it.stmts)
			f.writeln('}')
		}
		ast.Return {
			f.write('return')
			// multiple returns
			if it.exprs.len > 1 {
				f.write(' ')
				for i, expr in it.exprs {
					f.expr(expr)
					if i < it.exprs.len - 1 {
						f.write(', ')
					}
				}
			}
			// normal return
			else if it.exprs.len == 1 {
				f.write(' ')
				f.expr(it.exprs[0])
			}
			f.writeln('')
		}
		ast.StructDecl {
			f.struct_decl(it)
		}
		ast.VarDecl {
			// type_sym := f.table.get_type_symbol(it.typ)
			if it.is_mut {
				f.write('mut ')
			}
			f.write('$it.name := ')
			f.expr(it.expr)
			f.writeln('')
		}
		else {
			println('unknown node')
			// exit(1)
		}
	}
}

fn (f mut Fmt) struct_decl(node ast.StructDecl) {
	f.writeln('struct $node.name {')
	mut max := 0
	for field in node.fields {
		if field.name.len > max {
			max = field.name.len
		}
	}
	for field in node.fields {
		field_type_sym := f.table.get_type_symbol(field.typ)
		f.write('\t$field.name ')
		f.write(strings.repeat(` `, max - field.name.len))
		f.writeln('$field_type_sym.name')
	}
	f.writeln('}\n')
}

fn (f mut Fmt) expr(node ast.Expr) {
	match node {
		ast.ArrayInit {
			// type_sym := f.table.get_type_symbol(it.typ)
			f.write('[')
			for i, expr in it.exprs {
				f.expr(expr)
				if i < it.exprs.len - 1 {
					f.write(', ')
				}
			}
			f.write(']')
		}
		ast.AssignExpr {
			f.expr(it.left)
			f.write(' $it.op.str() ')
			f.expr(it.val)
		}
		ast.BoolLiteral {
			f.write(it.val.str())
		}
		ast.CallExpr {
			f.write('${it.name}(')
			for i, expr in it.args {
				f.expr(expr)
				if i != it.args.len - 1 {
					f.write(', ')
				}
			}
			f.write(')')
		}
		ast.EnumVal {
			f.write('.' + it.name)
		}
		ast.FloatLiteral {
			f.write(it.val)
		}
		ast.IfExpr {
			f.write('if ')
			f.expr(it.cond)
			f.writeln(' {')
			f.stmts(it.stmts)
			f.write('}')
			if it.else_stmts.len > 0 {
				f.writeln(' else {')
				f.stmts(it.else_stmts)
				f.write('}')
			}
		}
		ast.Ident {
			f.write('$it.name')
		}
		ast.InfixExpr {
			f.expr(it.left)
			f.write(' $it.op.str() ')
			f.expr(it.right)
		}
		ast.IndexExpr {
			f.index_expr(it)
		}
		ast.IntegerLiteral {
			f.write(it.val.str())
		}
		ast.MethodCallExpr {
			f.expr(it.expr)
			f.write('.' + it.name + '(')
			for i, arg in it.args {
				f.expr(arg)
				if i < it.args.len - 1 {
					f.write(', ')
				}
			}
			f.write(')')
		}
		ast.PostfixExpr {
			f.expr(it.expr)
			f.write(it.op.str())
		}
		ast.PrefixExpr {
			f.write(it.op.str())
			f.expr(it.right)
		}
		ast.SelectorExpr {
			f.expr(it.expr)
			f.write('.')
			f.write(it.field)
		}
		ast.StringLiteral {
			if it.val.contains("'") {
				f.write('"$it.val"')
			}
			else {
				f.write("'$it.val'")
			}
		}
		ast.StructInit {
			type_sym := f.table.get_type_symbol(it.typ)
			f.writeln('$type_sym.name{')
			for i, field in it.fields {
				f.write('\t$field: ')
				f.expr(it.exprs[i])
				f.writeln('')
			}
			f.write('}')
		}
		else {}
	}
}

fn (f mut Fmt) index_expr(node ast.IndexExpr) {
	mut is_range := false
	match node.index {
		ast.RangeExpr {
			is_range = true
			f.expr(node.left)
			f.write('..')
			f.expr(it.high)
			f.write(')')
		}
		else {}
	}
	if !is_range {
		f.expr(node.left)
		f.write('[')
		f.expr(node.index)
		f.write(']')
	}
}
