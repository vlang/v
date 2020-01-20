// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.table
	v.types
	v.token
)

pub struct Checker {
	table      &table.Table
mut:
	file_name  string
	// TODO: resolved
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
	}
}

pub fn (c &Checker) check(ast_file ast.File) {
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
}

pub fn (c mut Checker) check_files(v_files []string, ast_files []ast.File) {
	for i, file in ast_files {
		c.file_name = v_files[i]
		c.check(file)
	}
}

pub fn (c &Checker) check_struct_init(struct_init ast.StructInit) {
	typ := c.table.find_type(struct_init.ti.name) or {
		c.error('unknown struct: $struct_init.ti.name', struct_init.pos)
		panic('')
	}
	match typ.kind {
		.placeholder {
			c.error('unknown struct: $struct_init.ti.name', struct_init.pos)
		}
		.struct_ {
			info := typ.info as types.Struct
			for i, expr in struct_init.exprs {
				field := info.fields[i]
				expr_ti := c.table.get_expr_ti(expr)
				if !c.table.check(expr_ti, field.ti) {
					c.error('cannot assign $expr_ti.name as $field.ti.name for field $field.name', struct_init.pos)
				}
			}
		}
		else {}
	}
}

pub fn (c &Checker) check_binary_expr(binary_expr ast.BinaryExpr) {
	left_ti := c.table.get_expr_ti(binary_expr.left)
	right_ti := c.table.get_expr_ti(binary_expr.right)
	if !c.table.check(&right_ti, &left_ti) {
		c.error('binary expr: cannot use $right_ti.name as $left_ti.name', binary_expr.pos)
	}
}

fn (c &Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_ti := c.table.get_expr_ti(assign_expr.left)
	right_ti := c.table.get_expr_ti(assign_expr.val)
	if !c.table.check(right_ti, left_ti) {
		c.error('cannot assign $right_ti.name to $left_ti.name', assign_expr.pos)
	}
}

pub fn (c &Checker) check_call_expr(call_expr ast.CallExpr) {
	fn_name := call_expr.name
	if f := c.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if call_expr.args.len < f.args.len {
			c.error('too few arguments in call to `$fn_name`', call_expr.pos)
		} else if call_expr.args.len > f.args.len {
			c.error('too many arguments in call to `$fn_name`', call_expr.pos)
		}
		for i, arg in f.args {
			arg_expr := call_expr.args[i]
			ti := c.table.get_expr_ti(arg_expr)
			if !c.table.check(&ti, &arg.ti) {
				c.error('cannot use type `$ti.name` as type `$arg.ti.name` in argument to `$fn_name`', call_expr.pos)
			}
		}
	} else {
		c.error('unknown fn: $fn_name', call_expr.pos)
		// c.warn('unknown function `$fn_name`')
	}
}

pub fn (c &Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) {
	ti := c.table.get_expr_ti(method_call_expr.expr)
	if !c.table.has_method(ti.idx, method_call_expr.name) {
		c.error('type `$ti.name` has no method `$method_call_expr.name`', method_call_expr.pos)
	}
}

pub fn (c &Checker) check_selector_expr(selector_expr ast.SelectorExpr) {
	ti := c.table.get_expr_ti(selector_expr.expr)
	field_name := selector_expr.field
	// struct_ := c.table.types[ti.idx] as types.Struct
	typ := c.table.types[ti.idx]
	match typ.kind {
		.struct_ {
			// if !c.table.struct_has_field(it, field) {
			// 	c.error('AAA unknown field `${it.name}.$field`')
			// }
			// TODO: fix bug
			c.table.struct_find_field(typ, field_name) or {
				c.error('unknown field `${typ.name}.$field_name`', selector_expr.pos)
			}
		}
		else {
			c.error('$ti.name is not a struct', selector_expr.pos)
		}
	}
}

// TODO: non deferred
pub fn (c &Checker) check_return_stmt(return_stmt ast.Return) {
	mut got_tis := []types.TypeIdent
	for expr in return_stmt.exprs {
		ti := c.table.get_expr_ti(expr)
		got_tis << ti
	}
	expected_ti := return_stmt.expected_ti
	mut expected_tis := [expected_ti]
	if expected_ti.kind == .multi_return {
		mr_type := c.table.types[expected_ti.idx]
		mr_info := mr_type.info as types.MultiReturn
		expected_tis = mr_info.tis
	}
	if expected_tis.len != got_tis.len {
		c.error('wrong number of return arguments:\n\texpected: $expected_tis.str()\n\tgot: $got_tis.str()', return_stmt.pos)
	}
	for i, exp_ti in expected_tis {
		got_ti := got_tis[i]
		if !c.table.check(got_ti, exp_ti) {
			c.error('cannot use `$got_ti.name` as type `$exp_ti.name` in return argument', return_stmt.pos)
		}
	}
}

pub fn (c &Checker) check_array_init(array_init ast.ArrayInit) {
	mut val_ti := types.void_ti
	for i, expr in array_init.exprs {
		c.expr(expr)
		ti := c.table.get_expr_ti(expr)
		// The first element's type
		if i == 0 {
			val_ti = ti
			continue
		}
		if !c.table.check(val_ti, ti) {
			c.error('expected array element with type `$val_ti.name`', array_init.pos)
		}
	}
}

fn (c &Checker) stmt(node ast.Stmt) {
	match node {
		ast.FnDecl {
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.Return {
			c.check_return_stmt(it)
		}
		ast.VarDecl {
			c.expr(it.expr)
		}
		ast.ForStmt {
			c.expr(it.cond)
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.ForCStmt {
			c.stmt(it.init)
			c.expr(it.cond)
			c.stmt(it.inc)
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		// ast.StructDecl {}
		ast.ExprStmt {
			c.expr(it.expr)
		}
		else {}
	}
}

fn (c &Checker) expr(node ast.Expr) {
	match node {
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		// ast.IntegerLiteral {}
		// ast.FloatLiteral {}
		ast.PostfixExpr {
			c.expr(it.expr)
		}
		ast.UnaryExpr {
			c.expr(it.left)
		}
		// ast.StringLiteral {}
		ast.PrefixExpr {
			c.expr(it.right)
		}
		ast.BinaryExpr {
			c.check_binary_expr(it)
		}
		ast.StructInit {
			c.check_struct_init(it)
		}
		ast.CallExpr {
			c.check_call_expr(it)
		}
		ast.MethodCallExpr {
			c.check_method_call_expr(it)
		}
		ast.ArrayInit {
			c.check_array_init(it)
		}
		// ast.Ident {}
		// ast.BoolLiteral {}
		ast.SelectorExpr {
			c.check_selector_expr(it)
		}
		ast.IndexExpr {
			c.expr(it.left)
			c.expr(it.index)
		}
		ast.IfExpr {
			c.expr(it.cond)
			for i, stmt in it.stmts {
				c.stmt(stmt)
			}
			if it.else_stmts.len > 0 {
				for stmt in it.else_stmts {
					c.stmt(stmt)
				}
			}
		}
		else {}
	}
}

pub fn (c &Checker) error(s string, pos token.Position) {
	print_backtrace()
	final_msg_line := '$c.file_name:$pos.line_nr: error: $s'
	eprintln(final_msg_line)
	/*
	if colored_output {
		eprintln(term.bold(term.red(final_msg_line)))
	}else{
		eprintln(final_msg_line)
	}
	*/
	exit(1)
}
