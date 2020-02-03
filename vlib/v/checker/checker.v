// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.table
	v.token
)

pub struct Checker {
	table     &table.Table
mut:
	file_name string
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

pub fn (c mut Checker) check_files(ast_files []ast.File) {
	for file in ast_files {
		c.file_name = file.path
		c.check(file)
	}
}

pub fn (c &Checker) check_struct_init(struct_init ast.StructInit) table.Type {
	typ := c.table.find_type(struct_init.ti.name) or {
		c.error('unknown struct: $struct_init.ti.name', struct_init.pos)
		panic('')
	}
	match typ.kind {
		.placeholder {
			c.error('unknown struct: $struct_init.ti.name', struct_init.pos)
		}
		.struct_ {
			info := typ.info as table.Struct
			for i, expr in struct_init.exprs {
				field := info.fields[i]
				// expr_ti := c.expr(expr)
				field_type := c.expr(expr)
				if !c.table.check(field_type, field.ti) {
					c.error('cannot assign $field_type.name as $field.ti.name for field $field.name', struct_init.pos)
				}
			}
		}
		else {}
	}
	return struct_init.ti
}

pub fn (c &Checker) infix_expr(infix_expr ast.InfixExpr) table.Type {
	left_ti := c.expr(infix_expr.left)
	right_ti := c.expr(infix_expr.right)
	if !c.table.check(&right_ti, &left_ti) {
		// if !c.table.check(&infix_expr.right_type, &infix_expr.right_type) {
		// c.error('infix expr: cannot use `$infix_expr.right_type.name` as `$infix_expr.left_type.name`', infix_expr.pos)
		c.error('infix expr: cannot use `$left_ti.name` as `$right_ti.name`', infix_expr.pos)
	}
	if infix_expr.op.is_relational() {
		return table.bool_type
	}
	return left_ti
}

fn (c &Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_ti := c.expr(assign_expr.left)
	right_ti := c.expr(assign_expr.val)
	if !c.table.check(right_ti, left_ti) {
		c.error('cannot assign $right_ti.name to $left_ti.name', assign_expr.pos)
	}
}

pub fn (c &Checker) call_expr(call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	if f := c.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if call_expr.args.len < f.args.len {
			c.error('too few arguments in call to `$fn_name`', call_expr.pos)
		}
		else if call_expr.args.len > f.args.len {
			c.error('too many arguments in call to `$fn_name`', call_expr.pos)
		}
		for i, arg in f.args {
			arg_expr := call_expr.args[i]
			ti := c.expr(arg_expr)
			if !c.table.check(&ti, &arg.typ) {
				c.error('!cannot use type `$ti.name` as type `$arg.typ.name` in argument to `$fn_name`', call_expr.pos)
			}
		}
		return f.return_type
	}else{
		c.error('unknown fn: $fn_name', call_expr.pos)
		exit(0)
		// c.warn('unknown function `$fn_name`')
	}
}

pub fn (c &Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) table.Type {
	ti := c.expr(method_call_expr.expr)
	if !c.table.has_method(ti.idx, method_call_expr.name) {
		c.error('type `$ti.name` has no method `$method_call_expr.name`', method_call_expr.pos)
	}
	return ti
}

pub fn (c &Checker) selector_expr(selector_expr ast.SelectorExpr) table.Type {
	ti := c.expr(selector_expr.expr)
	field_name := selector_expr.field
	// struct_ := c.table.types[ti.idx]
	// struct_info := struct_.info as table.Struct
	typ := c.table.types[ti.idx]
	match typ.kind {
		.struct_ {
			// if !c.table.struct_has_field(it, field) {
			// c.error('AAA unknown field `${it.name}.$field`')
			// }
			// TODO: fix bug
			field := c.table.struct_find_field(typ, field_name) or {
				c.error('unknown field `${typ.name}.$field_name`', selector_expr.pos)
				exit(0)
			}
			return field.ti
		}
		else {
			c.error('`$typ.name` is not a struct', selector_expr.pos)
		}
	}
	return table.void_type
}

// TODO: non deferred
pub fn (c &Checker) return_stmt(return_stmt ast.Return) {
	mut got_tis := []table.Type
	if return_stmt.exprs.len == 0 {
		return
	}
	for expr in return_stmt.exprs {
		ti := c.expr(expr)
		got_tis << ti
	}
	expected_ti := return_stmt.expected_ti
	mut expected_tis := [expected_ti]
	if expected_ti.kind == .multi_return {
		mr_type := c.table.types[expected_ti.idx]
		mr_info := mr_type.info as table.MultiReturn
		expected_tis = mr_info.tis
	}
	if expected_tis.len > 0 && expected_tis.len != got_tis.len {
		c.error('wrong number of return arguments:\n\texpected: $expected_tis.str()\n\tgot: $got_tis.str()', return_stmt.pos)
	}
	for i, exp_ti in expected_tis {
		got_ti := got_tis[i]
		if !c.table.check(got_ti, exp_ti) {
			c.error('cannot use `$got_ti.name` as type `$exp_ti.name` in return argument', return_stmt.pos)
		}
	}
}

pub fn (c &Checker) array_init(array_init ast.ArrayInit) table.Type {
	mut val_ti := table.void_type
	for i, expr in array_init.exprs {
		c.expr(expr)
		ti := c.expr(expr)
		// The first element's type
		if i == 0 {
			val_ti = ti
			continue
		}
		if !c.table.check(val_ti, ti) {
			c.error('expected array element with type `$val_ti.name`', array_init.pos)
		}
	}
	return array_init.ti
}

fn (c &Checker) stmt(node ast.Stmt) {
	match mut node {
		ast.FnDecl {
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.Return {
			c.return_stmt(it)
		}
		ast.VarDecl {
			typ := c.expr(it.expr)
			// it.typ = typ
			// println('checker: var decl $typ.name  it.typ=$it.typ.name $it.pos.line_nr')
			if typ.kind != .void {
				it.typ = typ
			}
			// if it.typ.kind == .unresolved {
			// it.ti = typ
			// println('unresolved var')
			// }
		}
		ast.ForStmt {
			typ := c.expr(it.cond)
			if typ.kind != .bool {
				c.error('non-bool used as for condition', it.pos)
			}
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

pub fn (c &Checker) expr(node ast.Expr) table.Type {
	match node {
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		ast.IntegerLiteral {
			return table.int_type
		}
		// ast.FloatLiteral {}
		ast.PostfixExpr {
			return c.expr(it.expr)
		}
		/*
		ast.UnaryExpr {
			c.expr(it.left)
		}
		*/

		ast.StringLiteral {
			return table.string_type
		}
		ast.PrefixExpr {
			return c.expr(it.right)
		}
		ast.InfixExpr {
			return c.infix_expr(it)
		}
		ast.StructInit {
			return c.check_struct_init(it)
		}
		ast.CallExpr {
			return c.call_expr(it)
		}
		ast.MethodCallExpr {
			return c.check_method_call_expr(it)
		}
		ast.ArrayInit {
			return c.array_init(it)
		}
		ast.Ident {
			if it.kind == .variable {
				info := it.info as ast.IdentVar
				if info.typ.kind != .unresolved {
					return info.typ
				}
				return c.expr(info.expr)
			}
			return table.void_type
		}
		ast.BoolLiteral {
			return table.bool_type
		}
		ast.SelectorExpr {
			return c.selector_expr(it)
		}
		ast.IndexExpr {
			return c.index_expr(it)
		}
		ast.IfExpr {
			typ := c.expr(it.cond)
			if typ.kind != .bool {
				c.error('non-bool (`$typ.name`) used as if condition', it.pos)
			}
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
	return table.void_type
}

pub fn (c &Checker) index_expr(node ast.IndexExpr) table.Type {
	mut typ := c.expr(node.left)
	mut is_range := false // TODO is_range := node.index is ast.RangeExpr
	match node.index {
		ast.RangeExpr {
			is_range = true
		}
		else {}
	}
	// TODO
	// info := ti.info as table.Array
	// ti = p.table.types[info.elem_type_idx]
	if typ.name.starts_with('array_') {
		if is_range {} // `x[start..end]` has the same type as `x`
		else {
			elm_typ := typ.name[6..]
			// TODO `typ = ... or ...`
			x := c.table.find_type(elm_typ) or {
				c.error(elm_typ, node.pos)
				exit(0)
			}
			typ = x
			// Check index type
			index_type := c.expr(node.index)
			if index_type.kind != .int {
				c.error('non-integer index (type `$index_type.name`)', node.pos)
			}
		}
	}
	else {
		typ = table.int_type
	}
	return typ
	// c.expr(it.index)
	// return it.typ
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
