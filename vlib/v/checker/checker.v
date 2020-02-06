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
	table      &table.Table
mut:
	file_name  string
	unresolved []ast.Expr
	resolved   []table.TypeRef
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
	}
}

pub fn (c mut Checker) check(ast_file ast.File) {
	c.file_name = ast_file.path
	c.unresolved = ast_file.unresolved
	c.resolve_types()
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
}

pub fn (c mut Checker) check_files(ast_files []ast.File) {
	for file in ast_files {
		c.check(file)
	}
}

fn (c mut Checker) resolve_types() {
	for x in c.unresolved {
		c.resolved << c.expr(x)
	}

	for idx, t in c.table.types {
		if t.kind == .array {
			mut info := t.info as table.Array
			if info.elem_type.typ.kind == .unresolved {
				rt := c.resolved[info.elem_type.idx]
				info.elem_type = c.resolved[info.elem_type.idx]
				mut t1 := &c.table.types[idx]
				t1.name = 'array_$rt.typ.name' // TODO name fn
				t1.info = info
			}
		}
	}
}

pub fn (c &Checker) check_struct_init(struct_init ast.StructInit) table.TypeRef {
	typ := c.table.find_type(struct_init.typ.typ.name) or {
		println('# A')
		c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
		panic('')
	}
	match typ.kind {
		.placeholder {
			println('# B')
			c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
		}
		.struct_ {
			info := typ.info as table.Struct
			for i, expr in struct_init.exprs {
				field := info.fields[i]
				// expr_ti := c.expr(expr)
				field_type := c.expr(expr)
				if !c.table.check(field_type, field.typ) {
					c.error('cannot assign $field_type.typ.name as $field.typ.typ.name for field $field.name', struct_init.pos)
				}
			}
		}
		else {}
	}
	return struct_init.typ
}

pub fn (c &Checker) infix_expr(infix_expr ast.InfixExpr) table.TypeRef {
	left_type := c.expr(infix_expr.left)
	right_type := c.expr(infix_expr.right)
	if !c.table.check(&right_type, &left_type) {
		// if !c.table.check(&infix_expr.right_type, &infix_expr.right_type) {
		// c.error('infix expr: cannot use `$infix_expr.right_type.name` as `$infix_expr.left_type.name`', infix_expr.pos)
		c.error('infix expr: cannot use `$left_type.typ.name` as `$right_type.typ.name`', infix_expr.pos)
	}
	if infix_expr.op.is_relational() {
		return c.table.type_ref(table.bool_type_idx)
	}
	return left_type
}

fn (c &Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_type := c.expr(assign_expr.left)
	right_type := c.expr(assign_expr.val)
	if !c.table.check(right_type, left_type) {
		c.error('cannot assign $right_type.typ.name to $left_type.typ.name', assign_expr.pos)
	}
}

pub fn (c &Checker) call_expr(call_expr ast.CallExpr) table.TypeRef {
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
			typ := c.expr(arg_expr)
			if !c.table.check(&typ, &arg.typ) {
				c.error('!cannot use type `$typ.typ.name` as type `$arg.typ.typ.name` in argument to `$fn_name`', call_expr.pos)
			}
		}
		return f.return_type
	}
	c.error('unknown fn: $fn_name', call_expr.pos)
	exit(1)
}

pub fn (c &Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) table.TypeRef {
	typ := c.expr(method_call_expr.expr)
	if method := typ.typ.find_method(method_call_expr.name) {
		return method.return_type
	}
	c.error('type `$typ.typ.name` has no method `$method_call_expr.name`', method_call_expr.pos)
	exit(1)
}

pub fn (c &Checker) selector_expr(selector_expr ast.SelectorExpr) table.TypeRef {
	typ := c.expr(selector_expr.expr)
	field_name := selector_expr.field
	match typ.typ.kind {
		.struct_ {
			field := c.table.struct_find_field(typ.typ, field_name) or {
				c.error('unknown field `${typ.typ.name}.$field_name`', selector_expr.pos)
				exit(0)
			}
			return field.typ
		}
		else {
			c.error('`$typ.typ.name` is not a struct', selector_expr.pos)
		}
	}
	return c.table.type_ref(table.void_type_idx)
}

// TODO: non deferred
pub fn (c &Checker) return_stmt(return_stmt ast.Return) {
	mut got_types := []table.TypeRef
	if return_stmt.exprs.len == 0 {
		return
	}
	for expr in return_stmt.exprs {
		typ := c.expr(expr)
		got_types << typ
	}
	expected_type := return_stmt.expected_type
	mut expected_types := [expected_type]
	if expected_type.typ.kind == .multi_return {
		mr_info := expected_type.typ.info as table.MultiReturn
		expected_types = mr_info.types
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		c.error('wrong number of return arguments:\n\texpected: $expected_types.str()\n\tgot: $got_types.str()', return_stmt.pos)
	}
	for i, exp_typ in expected_types {
		got_typ := got_types[i]
		if !c.table.check(got_typ, exp_typ) {
			c.error('cannot use `$got_typ.typ.name` as type `$exp_typ.typ.name` in return argument', return_stmt.pos)
		}
	}
}

pub fn (c &Checker) array_init(array_init ast.ArrayInit) table.TypeRef {
	mut elem_type := c.table.type_ref(table.void_type_idx)
	for i, expr in array_init.exprs {
		c.expr(expr)
		typ := c.expr(expr)
		println('# ARR TYP: $typ.typ.name')
		// The first element's type
		if i == 0 {
			elem_type = typ
			continue
		}
		if !c.table.check(elem_type, typ) {
			c.error('expected array element with type `$elem_type.typ.name`', array_init.pos)
		}
	}
	return array_init.typ
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
			if typ.typ.kind != .void {
				it.typ = typ
			}
			// if it.typ.kind == .unresolved {
			// it.ti = typ
			// println('unresolved var')
			// }
		}
		ast.ForStmt {
			typ := c.expr(it.cond)
			if typ.typ.kind != .bool {
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

pub fn (c &Checker) expr(node ast.Expr) table.TypeRef {
	match mut node {
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		ast.IntegerLiteral {
			// return table.int_type
			return c.table.type_ref_ptr(table.int_type_idx, 0)
		}
		// ast.FloatLiteral {}
		ast.PostfixExpr {
			return c.postfix_expr(it)
		}
		/*
		ast.UnaryExpr {
			c.expr(it.left)
		}
		*/

		ast.StringLiteral {
			// return table.string_type
			return c.table.type_ref_ptr(table.string_type_idx, 0)
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
				mut info := it.info as ast.IdentVar
				//if info.typ.typ == 0 {
				//	panic('EMPTY TYPE')
				//}
				if info.typ.typ.kind == .unresolved {
					typ := c.resolved[info.typ.idx]
					info.typ = typ
					it.info = info
					return typ
				}
				return info.typ
			}
			// return table.void_type
			return c.table.type_ref_ptr(table.void_type_idx, 0)
		}
		ast.BoolLiteral {
			// return table.bool_type
			return c.table.type_ref_ptr(table.bool_type_idx, 0)
		}
		ast.SelectorExpr {
			return c.selector_expr(it)
		}
		ast.IndexExpr {
			return c.index_expr(it)
		}
		ast.IfExpr {
			typ := c.expr(it.cond)
			if typ.typ.kind != .bool {
				c.error('non-bool (`$typ.typ.name`) used as if condition', it.pos)
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
	// return table.void_type
	return c.table.type_ref_ptr(table.void_type_idx, 0)
}

pub fn (c &Checker) postfix_expr(node ast.PostfixExpr) table.TypeRef {
	/*
	match node.expr {
		ast.IdentVar {
			println('postfix identvar')
		}
		else {}
	}
	*/
	typ := c.expr(node.expr)
	if typ.typ.kind != .int {
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ.typ.name`)', node.pos)
	}
	return typ
}

pub fn (c &Checker) index_expr(node ast.IndexExpr) table.TypeRef {
	mut typ := c.expr(node.left)
	mut is_range := false // TODO is_range := node.index is ast.RangeExpr
	match node.index {
		ast.RangeExpr {
			is_range = true
		}
		else {}
	}
	// if typ.name.starts_with('array_') {
	if typ.typ.kind == .array {
		// info := typ.typ.info as table.Array
		if is_range {} // `x[start..end]` has the same type as `x`
		else {
			// println('@@@@@@@ A: $typ.typ.name')
			match typ.typ.info {
				table.Array {
					// if it.elem_type.typ != 0 {
					// 	return it.elem_type
					// }
					// println('@@@@@@@ B: $it.elem_type.typ.name')
					return it.elem_type
				}
				else {}
			}
			// TODO `typ = ... or ...`
			// x := c.table.find_type_idx(elm_typ) or {
			// 	c.error('cannot find array elem type: $elm_typ', node.pos)
			// 	exit(0)
			// }
			// Check index type
			index_type := c.expr(node.index)
			if index_type.typ.kind != .int {
				c.error('non-integer index (type `$index_type.typ.name`)', node.pos)
			}
		}
	}
	else {
		// typ = table.int_type
		typ = c.table.type_ref_ptr(table.int_type_idx, 0)
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
