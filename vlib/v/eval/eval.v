// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module eval

import v.ast
import v.checker
import v.table
import v.pref

pub type Object = int | string

pub struct Eval {
mut:
	checker checker.Checker
	vars    map[string]Var
	table   &table.Table
}

pub struct Var {
	value Object
}

pub fn (mut e Eval) eval(file ast.File, table &table.Table) string {
	vpref := &pref.Preferences{}
	e.table = table
	mut res := ''
	e.checker = checker.new_checker(table, vpref)
	for stmt in file.stmts {
		res += e.stmt(stmt) + '\n'
	}
	return res.trim_space()
}

fn print_object(o Object) {
	match o {
		int { println(it) }
		else { println('unknown object') }
	}
}

pub fn (o Object) str() string {
	match o {
		int { return it.str() }
		else { println('unknown object') }
	}
	return ''
}

fn (mut e Eval) stmt(node ast.Stmt) string {
	match node {
		ast.AssignStmt {
			// TODO; replaced VarDecl
		}
		ast.ExprStmt {
			o := e.expr(it.expr)
			print('out: ')
			print_object(o)
			return o.str()
		}
		// ast.StructDecl {
		//	println('s decl')
		// }
		// ast.VarDecl {
		//	e.vars[it.name] = Var{
		//		value: e.expr(it.expr)
		//	}
		// }
		else {}
	}
	return '>>'
}

fn (mut e Eval) expr(node ast.Expr) Object {
	match node {
		ast.IntegerLiteral {
			return it.val
		}
		ast.Ident {
			print_object(it.value)
			// Find the variable
			v := e.vars[it.name]
			return v.value
		}
		ast.InfixExpr {
			e.checker.infix_expr(mut it)
			// println('bin $it.op')
			left := e.expr(it.left) as int
			right := e.expr(it.right) as int
			match it.op {
				.plus { return left + right }
				.mul { return left * right }
				else {}
			}
		}
		else {}
	}
	return 0
	// return Object{}
}
