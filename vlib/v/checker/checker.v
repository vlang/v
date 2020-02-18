// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.table
	v.token
	os
	filepath
)

pub struct Checker {
	table     &table.Table
mut:
	file_name string
	scope     &ast.Scope
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
		scope: 0
	}
}

pub fn (c mut Checker) check(ast_file ast.File) {
	c.file_name = ast_file.path
	c.scope = &ast_file.scope
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
}

pub fn (c mut Checker) check_files(ast_files []ast.File) {
	for file in ast_files {
		c.check(file)
	}
}

pub fn (c mut Checker) check_struct_init(struct_init ast.StructInit) table.Type {
	// typ := c.table.find_type(struct_init.typ.typ.name) or {
	// c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
	// panic('')
	// }
	typ_sym := c.table.get_type_symbol(struct_init.typ)
	match typ_sym.kind {
		.placeholder {
			c.error('unknown struct: $typ_sym.name', struct_init.pos)
		}
		.struct_ {
			info := typ_sym.info as table.Struct
			if struct_init.exprs.len > info.fields.len {
				c.error('too many fields', struct_init.pos)
			}
			for i, expr in struct_init.exprs {
				// struct_field info.
				field_name := struct_init.fields[i]
				mut field := info.fields[i]
				mut found_field := false
				for f in info.fields {
					if f.name == field_name {
						field = f
						found_field = true
						break
					}
				}
				if !found_field {
					c.error('struct init: no such field `$field_name` for struct `$typ_sym.name`', struct_init.pos)
				}
				expr_type := c.expr(expr)
				expr_type_sym := c.table.get_type_symbol(expr_type)
				field_type_sym := c.table.get_type_symbol(field.typ)
				if !c.table.check(expr_type, field.typ) {
					c.error('cannot assign $expr_type_sym.name as $field_type_sym.name for field $field.name', struct_init.pos)
				}
			}
		}
		else {}
	}
	return struct_init.typ
}

pub fn (c mut Checker) infix_expr(infix_expr ast.InfixExpr) table.Type {
	left_type := c.expr(infix_expr.left)
	right_type := c.expr(infix_expr.right)
	if !c.table.check(right_type, left_type) {
		left_type_sym := c.table.get_type_symbol(left_type)
		right_type_sym := c.table.get_type_symbol(right_type)
		// if !c.table.check(&infix_expr.right_type, &infix_expr.right_type) {
		// c.error('infix expr: cannot use `$infix_expr.right_type.name` as `$infix_expr.left_type.name`', infix_expr.pos)
		c.error('infix expr: cannot use `$right_type_sym.name` as `$left_type_sym.name`', infix_expr.pos)
	}
	if infix_expr.op.is_relational() {
		return table.bool_type
	}
	return left_type
}

fn (c mut Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_type := c.expr(assign_expr.left)
	right_type := c.expr(assign_expr.val)
	if !c.table.check(right_type, left_type) {
		left_type_sym := c.table.get_type_symbol(left_type)
		right_type_sym := c.table.get_type_symbol(right_type)
		c.error('cannot assign $right_type_sym.name to $left_type_sym.name', assign_expr.pos)
	}
}

pub fn (c mut Checker) call_expr(call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	if f := c.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if f.is_c || call_expr.is_c {
			return f.return_type
		}
		if call_expr.args.len < f.args.len {
			c.error('too few arguments in call to `$fn_name`', call_expr.pos)
		}
		else if !f.is_variadic && call_expr.args.len > f.args.len {
			c.error('too many arguments in call to `$fn_name` ($call_expr.args.len instead of $f.args.len)', call_expr.pos)
		}
		for i, arg_expr in call_expr.args {
			arg := if f.is_variadic && i >= f.args.len - 1 { f.args[f.args.len - 1] } else { f.args[i] }
			typ := c.expr(arg_expr)
			typ_sym := c.table.get_type_symbol(typ)
			arg_typ_sym := c.table.get_type_symbol(arg.typ)
			if !c.table.check(typ, arg.typ) {
				c.error('!cannot use type `$typ_sym.name` as type `$arg_typ_sym.name` in argument ${i+1} to `$fn_name`', call_expr.pos)
			}
		}
		return f.return_type
	}
	c.error('unknown fn: $fn_name', call_expr.pos)
	exit(1)
}

pub fn (c mut Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) table.Type {
	typ := c.expr(method_call_expr.expr)
	typ_sym := c.table.get_type_symbol(typ)
	if method := typ_sym.find_method(method_call_expr.name) {
		return method.return_type
	}
	// check parent
	if !isnil(typ_sym.parent) {
		if method := typ_sym.parent.find_method(method_call_expr.name) {
			return method.return_type
		}
	}
	c.error('type `$typ_sym.name` has no method `$method_call_expr.name`', method_call_expr.pos)
	exit(1)
}

pub fn (c mut Checker) selector_expr(selector_expr ast.SelectorExpr) table.Type {
	typ := c.expr(selector_expr.expr)
	typ_sym := c.table.get_type_symbol(typ)
	field_name := selector_expr.field
	if field := typ_sym.find_field(field_name) {
		return field.typ
	}
	// check parent
	if !isnil(typ_sym.parent) {
		if field := typ_sym.parent.find_field(field_name) {
			return field.typ
		}
	}
	if typ_sym.kind != .struct_ {
		c.error('`$typ_sym.name` is not a struct', selector_expr.pos)
	}
	else {
		c.error('unknown field `${typ_sym.name}.$field_name`', selector_expr.pos)
	}
	exit(0)
}

// TODO: non deferred
pub fn (c mut Checker) return_stmt(return_stmt ast.Return) {
	mut got_types := []table.Type
	if return_stmt.exprs.len == 0 {
		return
	}
	for expr in return_stmt.exprs {
		typ := c.expr(expr)
		got_types << typ
	}
	expected_type := return_stmt.expected_type
	expected_type_sym := c.table.get_type_symbol(expected_type)
	mut expected_types := [expected_type]
	if expected_type_sym.kind == .multi_return {
		mr_info := expected_type_sym.info as table.MultiReturn
		expected_types = mr_info.types
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		c.error('wrong number of return arguments:\n\texpected: $expected_types.str()\n\tgot: $got_types.str()', return_stmt.pos)
	}
	for i, exp_typ in expected_types {
		got_typ := got_types[i]
		if !c.table.check(got_typ, exp_typ) {
			got_typ_sym := c.table.get_type_symbol(got_typ)
			exp_typ_sym := c.table.get_type_symbol(exp_typ)
			c.error('cannot use `$got_typ_sym.name` as type `$exp_typ_sym.name` in return argument', return_stmt.pos)
		}
	}
}

pub fn (c &Checker) assign_stmt(assign_stmt ast.AssignStmt) {}

pub fn (c mut Checker) array_init(array_init mut ast.ArrayInit) table.Type {
	mut elem_type := table.void_type
	// a = []
	if array_init.exprs.len == 0 {}
	for i, expr in array_init.exprs {
		c.expr(expr)
		typ := c.expr(expr)
		// The first element's type
		if i == 0 {
			elem_type = typ
			continue
		}
		if !c.table.check(elem_type, typ) {
			elem_type_sym := c.table.get_type_symbol(elem_type)
			c.error('expected array element with type `$elem_type_sym.name`', array_init.pos)
		}
	}
	// idx := if is_fixed { p.table.find_or_register_array_fixed(val_type, fixed_size, 1) } else { p.table.find_or_register_array(val_type, 1) }
	is_fixed := false
	fixed_size := 1
	idx := if is_fixed { c.table.find_or_register_array_fixed(elem_type, fixed_size, 1) } else { c.table.find_or_register_array(elem_type, 1) }
	array_type := table.new_type(idx)
	array_init.typ = array_type
	return array_init.typ
}

fn (c mut Checker) stmt(node ast.Stmt) {
	match mut node {
		ast.FnDecl {
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.Return {
			c.return_stmt(it)
		}
		ast.AssignStmt {
			c.assign_stmt(it)
		}
		ast.ConstDecl {
			for i, expr in it.exprs {
				mut field := it.fields[i]
				typ := c.expr(expr)
				mut xconst := c.table.consts[field.name]
				// if xconst.typ == 0 {
				xconst.typ = typ
				c.table.consts[field.name] = xconst
				// }
				field.typ = typ
				it.fields[i] = field
			}
		}
		ast.VarDecl {
			typ := c.expr(it.expr)
			// typ_sym := c.table.get_type_symbol(typ)
			// println('var $it.name - $typ - $it.typ - $typ_sym.name')
			// if it.typ == 0 {
			// it.typ = typ
			// }
			it.typ = typ
		}
		ast.ForStmt {
			typ := c.expr(it.cond)
			// typ_sym := c.table.get_type_symbol(typ)
			// if typ_sym.kind != .bool {
			if table.type_idx(typ) != table.bool_type_idx {
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

pub fn (c mut Checker) expr(node ast.Expr) table.Type {
	match mut node {
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		ast.FloatLiteral {
			return table.f64_type
		}
		ast.IntegerLiteral {
			return table.int_type
		}
		ast.PostfixExpr {
			return c.postfix_expr(it)
		}
		/*
		ast.UnaryExpr {
			c.expr(it.left)
		}
		*/

		ast.SizeOf {
			return table.int_type
		}
		ast.StringLiteral {
			return table.string_type
		}
		ast.CharLiteral {
			return table.byte_type
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
			return c.array_init(mut it)
		}
		ast.Ident {
			return c.ident(mut it)
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
			return c.if_expr(mut it)
		}
		ast.MatchExpr {
			return c.match_expr(mut it)
		}
		ast.CastExpr {
			return it.typ
		}
		else {}
	}
	return table.void_type
}

pub fn (c mut Checker) ident(ident mut ast.Ident) table.Type {
	// println('IDENT: $it.name - $it.pos.pos')
	if ident.kind == .variable {
		// println('===========================')
		// c.scope.print_vars(0)
		// println('===========================')
		info := ident.info as ast.IdentVar
		if info.typ != 0 {
			return info.typ
		}
		start_scope := c.scope.innermost(ident.pos.pos) or {
			c.scope
		}
		mut found := true
		mut var_scope := &ast.Scope(0)
		mut var := ast.VarDecl{}
		// mut var_scope, mut var := start_scope.find_scope_and_var(ident.name) or {
		var_scope,var = start_scope.find_scope_and_var(ident.name) or {
			found = false
			c.error('not found: $ident.name - POS: $ident.pos.pos', ident.pos)
			panic('')
		}
		if found {
			// update the variable
			// we need to do this here instead of var_decl since some
			// vars are registered manually for things like for loops etc
			// NOTE: or consider making those declerations part of those ast nodes
			mut typ := var.typ
			// set var type on first use
			if typ == 0 {
				typ = c.expr(var.expr)
				var.typ = typ
				var_scope.override_var(var)
			}
			// update ident
			ident.kind = .variable
			ident.info = ast.IdentVar{
				typ: typ
			}
			return typ
		}
	}
	// second use, already resovled in unresovled branch
	else if ident.kind == .constant {
		info := ident.info as ast.IdentVar
		return info.typ
	}
	// second use, already resovled in unresovled branch
	else if ident.kind == .function {
		info := ident.info as ast.IdentFunc
		return info.return_type
	}
	// Handle indents with unresolved types during the parsing step
	// (declared after first usage)
	else if ident.kind == .unresolved {
		// constant
		if constant := c.table.find_const(ident.name) {
			ident.kind = .constant
			ident.info = ast.IdentVar{
				typ: constant.typ
			}
			return constant.typ
		}
		// Function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(ident.name) {
			ident.kind = .function
			ident.info = ast.IdentFunc{
				return_type: func.return_type
			}
			return func.return_type
		}
	}
	if ident.is_c {
		return table.int_type
	}
	return table.void_type
}

pub fn (c mut Checker) match_expr(node mut ast.MatchExpr) table.Type {
	t := c.expr(node.cond)
	for i, block in node.blocks {
		match_expr := node.match_exprs[i]
		c.expr(match_expr)
		for stmt in block.stmts {
			c.stmt(stmt)
		}
		// If the last statement is an expression, return its type
		if block.stmts.len > 0 {
			match block.stmts[block.stmts.len - 1] {
				ast.ExprStmt {}
				// TODO: ask alex about this
				// typ := c.expr(it.expr)
				// type_sym := c.table.get_type_symbol(typ)
				// p.warn('match expr ret $type_sym.name')
				// node.typ = typ
				// return typ
				else {}
	}
		}
	}
	node.typ = t
	return t
}

pub fn (c mut Checker) if_expr(node mut ast.IfExpr) table.Type {
	typ := c.expr(node.cond)
	node.typ = typ
	typ_sym := c.table.get_type_symbol(typ)
	// if typ_sym.kind != .bool {
	if table.type_idx(typ) != table.bool_type_idx {
		c.error('non-bool (`$typ_sym.name`) used as if condition', node.pos)
	}
	for i, stmt in node.stmts {
		c.stmt(stmt)
	}
	if node.else_stmts.len > 0 {
		for stmt in node.else_stmts {
			c.stmt(stmt)
		}
	}
	if node.stmts.len > 0 {
		match node.stmts[node.stmts.len - 1] {
			ast.ExprStmt {
				// type_sym := p.table.get_type_symbol(it.typ)
				// p.warn('if expr ret $type_sym.name')
				// typ = it.typ
				// return it.typ
				t := c.expr(it.expr)
				node.typ = t
				return t
				// return node,it.ti
				// left =
			}
			else {}
	}
	}
	return typ
	// return table.void_type
}

pub fn (c mut Checker) postfix_expr(node ast.PostfixExpr) table.Type {
	/*
	match node.expr {
		ast.IdentVar {
			println('postfix identvar')
		}
		else {}
	}
	*/
	typ := c.expr(node.expr)
	// if typ.typ.kind != .int {
	if table.type_idx(typ) != table.int_type_idx {
		typ_sym := c.table.get_type_symbol(typ)
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ_sym.name`)', node.pos)
	}
	return typ
}

pub fn (c mut Checker) index_expr(node ast.IndexExpr) table.Type {
	/*
	mut typ := left_type
	left_type_sym := p.table.get_type_symbol(left_type)
	if left_type_sym.kind == .array {
		info := left_type_sym.info as table.Array
		typ = info.elem_type
	}
*/
	mut typ := c.expr(node.left)
	mut is_range := false // TODO is_range := node.index is ast.RangeExpr
	match node.index {
		ast.RangeExpr {
			is_range = true
		}
		else {}
	}
	typ_sym := c.table.get_type_symbol(typ)
	if typ_sym.kind == .array {
		if is_range {} // `x[start..end]` has the same type as `x`
		else {
			// Check index type
			index_type := c.expr(node.index)
			// if index_type.typ.kind != .int {
			if table.type_idx(index_type) != table.int_type_idx {
				index_type_sym := c.table.get_type_symbol(index_type)
				c.error('non-integer index (type `$index_type_sym.name`)', node.pos)
			}
			info := typ_sym.info as table.Array
			return info.elem_type
		}
	}
	else {
		typ = table.int_type
	}
	return typ
}

pub fn (c &Checker) error(s string, pos token.Position) {
	print_backtrace()
	mut path := c.file_name
	// Get relative path
	workdir := os.getwd() + filepath.separator
	if path.starts_with(workdir) {
		path = path.replace(workdir, '')
	}
	final_msg_line := '$path:$pos.line_nr: checker error: $s'
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
