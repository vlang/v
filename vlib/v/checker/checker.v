// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import (
	v.ast
	v.table
	v.token
	os
)

pub struct Checker {
	table     &table.Table
mut:
	file_name string
	resolved  []table.Type
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
	}
}

pub fn (c mut Checker) check(ast_file ast.File) {
	c.file_name = ast_file.path
	// if ast_file.unresolved.len != c.resolved.len {
	// c.resolve_exprs(file)
	// }
	c.complete_types()
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
}

pub fn (c mut Checker) check_files(ast_files []ast.File) {
	// this cant be moved to check() for multiple
	// files this muse be done first. TODO: optimize
	for file in ast_files {
		c.file_name = file.path
		c.resolve_expr_types(file.unresolved)
	}
	for file in ast_files {
		c.check(file)
	}
}

// resolve type of unresolved expressions
fn (c mut Checker) resolve_expr_types(exprs []ast.Expr) {
	for x in exprs {
		c.resolved << c.expr(x)
	}
}

// update any types chich contain unresolved sub types
fn (c &Checker) complete_types() {
	for idx, t in c.table.types {
		// skip builtin types
		if idx <= table.map_type_idx {
			continue
		}
		// println('Resolve type: $t.name')
		if t.kind == .array {
			mut info := t.array_info()
			if table.type_is_unresolved(info.elem_type) {
				info.elem_type = c.resolve(info.elem_type)
				elem_type_sym := c.table.get_type_symbol(info.elem_type)
				mut t1 := &c.table.types[idx]
				t1.name = table.array_name(elem_type_sym, info.nr_dims)
				t1.info = info
			}
		}
		else if t.kind == .map {
			mut info := t.map_info()
			mut updated := false
			if table.type_is_unresolved(info.key_type) {
				info.key_type = c.resolve(info.key_type)
				updated = true
			}
			if table.type_is_unresolved(info.value_type) {
				info.value_type = c.resolve(info.value_type)
				updated = true
			}
			if updated {
				mut t1 := &c.table.types[idx]
				key_type_sym := c.table.get_type_symbol(info.key_type)
				value_type_sym := c.table.get_type_symbol(info.value_type)
				t1.name = table.map_name(key_type_sym, value_type_sym)
				t1.info = info
			}
		}
		else if t.kind == .multi_return {
			mut info := t.mr_info()
			mut types := info.types
			mut updated := false
			for i, ut in types {
				if table.type_is_unresolved(ut) {
					types[i] = c.resolve(ut)
					updated = true
				}
			}
			if updated {
				mut t1 := &c.table.types[idx]
				info.types = types
				t1.info = info
			}
		}
	}
}

// return the resolved Type from unresovled Type
pub fn (c &Checker) resolve(unresolved table.Type) table.Type {
	return c.resolved[-table.type_idx(unresolved) - 1]
}

pub fn (c &Checker) check_struct_init(struct_init ast.StructInit) table.Type {
	// typ := c.table.find_type(struct_init.typ.typ.name) or {
	// c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
	// panic('')
	// }
	typ := c.table.get_type_symbol(struct_init.typ)
	match typ.kind {
		.placeholder {
			c.error('unknown struct: $typ.name', struct_init.pos)
		}
		.struct_ {
			info := typ.info as table.Struct
			for i, expr in struct_init.exprs {
				field := info.fields[i]
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

pub fn (c &Checker) infix_expr(infix_expr ast.InfixExpr) table.Type {
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

fn (c &Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	left_type := c.expr(assign_expr.left)
	right_type := c.expr(assign_expr.val)
	if !c.table.check(right_type, left_type) {
		left_type_sym := c.table.get_type_symbol(left_type)
		right_type_sym := c.table.get_type_symbol(right_type)
		c.error('cannot assign $right_type_sym.name to $left_type_sym.name', assign_expr.pos)
	}
}

pub fn (c &Checker) call_expr(call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	if f := c.table.find_fn(fn_name) {
		// return_ti := f.return_ti
		if f.is_c {
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

pub fn (c &Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) table.Type {
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

pub fn (c &Checker) selector_expr(selector_expr ast.SelectorExpr) table.Type {
	typ := c.expr(selector_expr.expr)
	typ_sym := c.table.get_type_symbol(typ)
	field_name := selector_expr.field
	if field := typ_sym.find_field(field_name) {
		return field.typ
	}
	// check parent
	if !isnil(typ_sym.parent) {
		if field := typ_sym.parent.find_field(field_name) {
			if table.type_is_unresolved(field.typ) {
				return c.resolved[field.typ]
			}
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
pub fn (c &Checker) return_stmt(return_stmt ast.Return) {
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

pub fn (c &Checker) array_init(array_init ast.ArrayInit) table.Type {
	mut elem_type := table.void_type
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
		ast.AssignStmt {
			c.assign_stmt(it)
		}
		ast.VarDecl {
			typ := c.expr(it.expr)
			// println('checker: var decl $typ.name  it.typ=$it.typ.name $it.pos.line_nr')
			// if typ.typ.kind != .void {
			if table.type_idx(typ) != table.void_type_idx {
				it.typ = typ
			}
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

pub fn (c &Checker) expr(node ast.Expr) table.Type {
	match mut node {
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		ast.IntegerLiteral {
			return table.int_type
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
				mut info := it.info as ast.IdentVar
				if table.type_is_unresolved(info.typ) {
					typ := c.resolve(info.typ)
					info.typ = typ
					it.info = info
					return typ
				}
				return info.typ
			}
			// Handle indents with unresolved types during the parsing step
			// (declared after first usage)
			else if it.kind == .blank_ident {
				if constant := c.table.find_const(it.name) {
					return constant.typ
				}
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
			typ_sym := c.table.get_type_symbol(typ)
			// if typ_sym.kind != .bool {
			if table.type_idx(typ) != table.bool_type_idx {
				c.error('non-bool (`$typ_sym.name`) used as if condition', it.pos)
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
		ast.CastExpr {
			return it.typ
		}
		else {}
	}
	return table.void_type
}

pub fn (c &Checker) postfix_expr(node ast.PostfixExpr) table.Type {
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

pub fn (c &Checker) index_expr(node ast.IndexExpr) table.Type {
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
	workdir := os.getwd() + os.path_separator
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
