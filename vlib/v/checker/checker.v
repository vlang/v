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

const (
	max_nr_errors = 100
)

pub struct Checker {
	table          &table.Table
mut:
	file           ast.File
	nr_errors      int
	errors         []string
	expected_type  table.Type
	fn_return_type table.Type // current function's return type
}

pub fn new_checker(table &table.Table) Checker {
	return Checker{
		table: table
	}
}

pub fn (c mut Checker) check(ast_file ast.File) {
	c.file = ast_file
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
	/*
	println('all types:')
	for t in c.table.types {
		println(t.name + ' - ' + t.kind.str())
	}
	*/

}

pub fn (c mut Checker) check2(ast_file ast.File) []string {
	c.file = ast_file
	for stmt in ast_file.stmts {
		c.stmt(stmt)
	}
	return c.errors
}

pub fn (c mut Checker) check_files(ast_files []ast.File) {
	// TODO: temp fix, impl proper solution
	for file in ast_files {
		c.file = file
		for stmt in file.stmts {
			match mut stmt {
				ast.ConstDecl {
					c.stmt(*it)
				}
				else {}
	}
		}
	}
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
	// println('check struct $typ_sym.name')
	match typ_sym.kind {
		.placeholder {
			c.error('unknown struct: $typ_sym.name', struct_init.pos)
		}
		.struct_ {
			info := typ_sym.info as table.Struct
			if struct_init.fields.len == 0 {
				// Short syntax TODO check
				return struct_init.typ
			}
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
				c.expected_type = field.typ
				expr_type := c.expr(expr)
				expr_type_sym := c.table.get_type_symbol(expr_type)
				field_type_sym := c.table.get_type_symbol(field.typ)
				if !c.table.check(expr_type, field.typ) {
					c.error('cannot assign `$expr_type_sym.name` as `$field_type_sym.name` for field `$field.name`', struct_init.pos)
				}
			}
		}
		else {}
	}
	return struct_init.typ
}

pub fn (c mut Checker) infix_expr(infix_expr ast.InfixExpr) table.Type {
	// println('checker: infix expr(op $infix_expr.op.str())')
	left_type := c.expr(infix_expr.left)
	c.expected_type = left_type
	right_type := c.expr(infix_expr.right)
	if !c.table.check(right_type, left_type) {
		left := c.table.get_type_symbol(left_type)
		right := c.table.get_type_symbol(right_type)
		// `array << elm`
		// the expressions have different types (array_x and x)
		if left.kind == .array && infix_expr.op == .left_shift {
			return table.void_type
		}
		// `elm in array`
		if right.kind in [.array, .map] && infix_expr.op == .key_in {
			return table.bool_type
		}
		c.error('infix expr: cannot use `$right.name` (right) as `$left.name`', infix_expr.pos)
	}
	if infix_expr.op.is_relational() {
		return table.bool_type
	}
	return left_type
}

fn (c mut Checker) check_assign_expr(assign_expr ast.AssignExpr) {
	match assign_expr.left {
		ast.Ident {
			if it.kind == .blank_ident {
				return
			}
		}
		else {}
	}
	left_type := c.expr(assign_expr.left)
	c.expected_type = left_type
	// t := c.table.get_type_symbol(left_type)
	// println('setting exp type to $c.expected_type $t.name')
	right_type := c.expr(assign_expr.val)
	if !c.table.check(right_type, left_type) {
		left_type_sym := c.table.get_type_symbol(left_type)
		right_type_sym := c.table.get_type_symbol(right_type)
		c.error('cannot assign $right_type_sym.name to $left_type_sym.name', assign_expr.pos)
	}
}

pub fn (c mut Checker) call_expr(call_expr ast.CallExpr) table.Type {
	fn_name := call_expr.name
	// TODO: impl typeof properly (probably not going to be a fn call)
	if fn_name == 'typeof' {
		return table.string_type
	}
	// start hack: until v1 is fixed and c definitions are added for these
	if fn_name == 'C.calloc' {
		return table.byteptr_type
	}
	else if fn_name == 'C.exit' {
		return table.void_type
	}
	else if fn_name == 'C.free' {
		return table.void_type
	}
	// end hack
	// look for function in format `mod.fn` or `fn` (main/builtin)
	mut f := table.Fn{}
	mut found := false
	// try prefix with current module as it would have never gotten prefixed
	if !fn_name.contains('.') && !(c.file.mod.name in ['builtin', 'main']) {
		if f1 := c.table.find_fn('${c.file.mod.name}.$fn_name') {
			found = true
			f = f1
		}
	}
	// already prefixed (mod.fn) or C/builtin/main
	if !found {
		if f1 := c.table.find_fn(fn_name) {
			found = true
			f = f1
		}
	}
	if !found {
		c.error('unknown fn: $fn_name', call_expr.pos)
	}
	if f.is_c || call_expr.is_c {
		return f.return_type
	}
	if call_expr.args.len < f.args.len {
		c.error('too few arguments in call to `$fn_name`', call_expr.pos)
	}
	else if !f.is_variadic && call_expr.args.len > f.args.len {
		c.error('too many arguments in call to `$fn_name` ($call_expr.args.len instead of $f.args.len)', call_expr.pos)
	}
	// println can print anything
	if fn_name == 'println' {
		return f.return_type
	}
	for i, arg_expr in call_expr.args {
		arg := if f.is_variadic && i >= f.args.len - 1 { f.args[f.args.len - 1] } else { f.args[i] }
		c.expected_type = arg.typ
		typ := c.expr(arg_expr)
		typ_sym := c.table.get_type_symbol(typ)
		arg_typ_sym := c.table.get_type_symbol(arg.typ)
		if !c.table.check(typ, arg.typ) {
			// str method, allow type with str method if fn arg is string
			if arg_typ_sym.kind == .string && typ_sym.has_method('str') {
				continue
			}
			// TODO const bug
			if typ_sym.kind == .void && arg_typ_sym.kind == .string {
				continue
			}
			if typ_sym.kind == .array_fixed {}
			// println('fixed')
			c.error('!cannot use type `$typ_sym.str()` as type `$arg_typ_sym.str()` in argument ${i+1} to `$fn_name`', call_expr.pos)
		}
	}
	return f.return_type
}

pub fn (c mut Checker) check_method_call_expr(method_call_expr ast.MethodCallExpr) table.Type {
	typ := c.expr(method_call_expr.expr)
	typ_sym := c.table.get_type_symbol(typ)
	name := method_call_expr.name
	if method := typ_sym.find_method(name) {
		return method.return_type
	}
	if typ_sym.kind == .array && name in ['filter', 'clone'] {
		return typ
	}
	if typ_sym.kind == .array && name in ['first', 'last'] {
		info := typ_sym.info as table.Array
		return info.elem_type
	}
	// check parent
	if typ_sym.parent_idx != 0 {
		parent := &c.table.types[typ_sym.parent_idx]
		if method := parent.find_method(name) {
			// println('got method $name, returning')
			return method.return_type
		}
	}
	c.error('type `$typ_sym.name` has no method `$name`', method_call_expr.pos)
	return table.void_type
}

pub fn (c mut Checker) selector_expr(selector_expr ast.SelectorExpr) table.Type {
	typ := c.expr(selector_expr.expr)
	typ_sym := c.table.get_type_symbol(typ)
	field_name := selector_expr.field
	if field := typ_sym.find_field(field_name) {
		return field.typ
	}
	// variadic
	if table.type_is_variadic(typ) {
		if field_name == 'len' {
			return table.int_type
		}
	}
	// check parent
	if typ_sym.parent_idx != 0 {
		parent := &c.table.types[typ_sym.parent_idx]
		if field := parent.find_field(field_name) {
			return field.typ
		}
	}
	if typ_sym.kind != .struct_ {
		if field_name == 'default_mode' {
			// TODO
			return table.bool_type
		}
		c.error('`$typ_sym.name` is not a struct', selector_expr.pos)
	}
	else {
		c.error('unknown field `${typ_sym.name}.$field_name`', selector_expr.pos)
	}
	return table.void_type
}

// TODO: non deferred
pub fn (c mut Checker) return_stmt(return_stmt ast.Return) {
	mut got_types := []table.Type
	c.expected_type = c.fn_return_type
	if return_stmt.exprs.len == 0 {
		return
	}
	for expr in return_stmt.exprs {
		typ := c.expr(expr)
		got_types << typ
	}
	expected_type := return_stmt.expected_type
	expected_type_sym := c.table.get_type_symbol(expected_type)
	exp_is_optional := table.type_is_optional(expected_type)
	mut expected_types := [expected_type]
	if expected_type_sym.kind == .multi_return {
		mr_info := expected_type_sym.info as table.MultiReturn
		expected_types = mr_info.types
	}
	// allow `none` & `error (Option)` return types for function that returns optional
	if exp_is_optional && table.type_idx(got_types[0]) in [table.none_type_idx, c.table.type_idxs['Option']] {
		return
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

pub fn (c mut Checker) assign_stmt(assign_stmt ast.AssignStmt) {
	// multi return
	if assign_stmt.left.len > assign_stmt.right.len {
		right := c.expr(assign_stmt.right[0])
		right_sym := c.table.get_type_symbol(right)
		info := right_sym.mr_info()
		if right_sym.kind != .multi_return {
			c.error('wrong number of vars', assign_stmt.pos)
		}
		mut scope := c.file.scope.innermost(assign_stmt.pos.pos) or {
			c.file.scope
		}
		for i, ident in assign_stmt.left {
			// TODO: check types
			scope.override_var(ast.VarDecl{
				name: ident.name
				typ: info.types[i]
			})
		}
	}
	// TODO: multiple assign
}

pub fn (c mut Checker) array_init(array_init mut ast.ArrayInit) table.Type {
	mut elem_type := table.void_type
	// []string - was set in parser
	if array_init.typ != table.void_type {
		return array_init.typ
	}
	// a = []
	if array_init.exprs.len == 0 {
		return c.expected_type
	}
	// [1,2,3]
	if array_init.exprs.len > 0 && array_init.elem_type == table.void_type {
		for i, expr in array_init.exprs {
			c.expr(expr)
			typ := c.expr(expr)
			// The first element's type
			if i == 0 {
				elem_type = typ
				c.expected_type = typ
				continue
			}
			if !c.table.check(elem_type, typ) {
				elem_type_sym := c.table.get_type_symbol(elem_type)
				c.error('expected array element with type `$elem_type_sym.name`', array_init.pos)
			}
		}
		idx := c.table.find_or_register_array(elem_type, 1)
		array_init.typ = table.new_type(idx)
	}
	// [50]byte
	else if array_init.exprs.len == 1 && array_init.elem_type != table.void_type {
		mut fixed_size := 1
		match array_init.exprs[0] {
			ast.IntegerLiteral {
				fixed_size = it.val
			}
			else {
				c.error('expecting `int` for fixed size', array_init.pos)
			}
	}
		idx := c.table.find_or_register_array_fixed(array_init.elem_type, fixed_size, 1)
		array_type := table.new_type(idx)
		array_init.typ = array_type
	}
	return array_init.typ
}

fn (c mut Checker) stmt(node ast.Stmt) {
	// c.expected_type = table.void_type
	match mut node {
		ast.AssignStmt {
			c.assign_stmt(it)
		}
		// ast.Attr {}
		// ast.CompIf {}
		ast.ConstDecl {
			for i, expr in it.exprs {
				mut field := it.fields[i]
				typ := c.expr(expr)
				mut xconst := c.table.consts[field.name]
				xconst.typ = typ
				c.table.consts[field.name] = xconst
				field.typ = typ
				it.fields[i] = field
			}
		}
		ast.ExprStmt {
			c.expr(it.expr)
		}
		ast.FnDecl {
			c.fn_return_type = it.typ
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.ForStmt {
			typ := c.expr(it.cond)
			if !it.is_inf && table.type_idx(typ) != table.bool_type_idx {
				c.error('non-bool used as for condition', it.pos)
			}
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.ForCStmt {
			c.stmt(it.init)
			c.expr(it.cond)
			// c.stmt(it.inc)
			c.expr(it.inc)
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		// ast.ForInStmt {}
		// ast.GlobalDecl {}
		// ast.HashStmt {}
		ast.Import {}
		ast.Return {
			c.return_stmt(it)
		}
		// ast.StructDecl {}
		ast.UnsafeStmt {
			for stmt in it.stmts {
				c.stmt(stmt)
			}
		}
		ast.VarDecl {
			typ := c.expr(it.expr)
			it.typ = typ
		}
		else {}
		// println('checker.stmt(): unhandled node')
		// println('checker.stmt(): unhandled node (${typeof(node)})')
		// }
	}
}

fn (c mut Checker) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		c.stmt(stmt)
	}
}

pub fn (c mut Checker) expr(node ast.Expr) table.Type {
	match mut node {
		ast.ArrayInit {
			return c.array_init(mut it)
		}
		ast.AsCast {
			return it.typ
		}
		ast.AssignExpr {
			c.check_assign_expr(it)
		}
		ast.Assoc {
			scope := c.file.scope.innermost(it.pos.pos) or {
				c.file.scope
			}
			var := scope.find_var(it.var_name) or {
				panic(err)
			}
			return var.typ
		}
		ast.BoolLiteral {
			return table.bool_type
		}
		ast.CastExpr {
			return it.typ
		}
		ast.CallExpr {
			return c.call_expr(it)
		}
		ast.CharLiteral {
			return table.byte_type
		}
		ast.EnumVal {
			return c.enum_val(it)
		}
		ast.FloatLiteral {
			return table.f64_type
		}
		ast.Ident {
			return c.ident(mut it)
		}
		ast.IfExpr {
			return c.if_expr(mut it)
		}
		ast.IfGuardExpr {
			return table.bool_type
		}
		ast.IndexExpr {
			return c.index_expr(it)
		}
		ast.InfixExpr {
			return c.infix_expr(it)
		}
		ast.IntegerLiteral {
			return table.int_type
		}
		ast.MapInit {
			return c.map_init(mut it)
		}
		ast.MatchExpr {
			return c.match_expr(mut it)
		}
		ast.MethodCallExpr {
			return c.check_method_call_expr(it)
		}
		ast.PostfixExpr {
			return c.postfix_expr(it)
		}
		ast.PrefixExpr {
			return c.expr(it.right)
		}
		ast.None {
			return table.none_type
		}
		ast.ParExpr {
			return c.expr(it.expr)
		}
		ast.SelectorExpr {
			return c.selector_expr(it)
		}
		ast.SizeOf {
			return table.int_type
		}
		ast.StringLiteral {
			return table.string_type
		}
		ast.StructInit {
			return c.check_struct_init(it)
		}
		ast.Type {
			return it.typ
		}
		/*
		ast.UnaryExpr {
			c.expr(it.left)
		}
		*/

		else {}
		// println('checker.expr(): unhandled node')
		// TODO: find nil string bug triggered with typeof
		// println('checker.expr(): unhandled node (${typeof(node)})')
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
		start_scope := c.file.scope.innermost(ident.pos.pos) or {
			c.file.scope
		}
		mut found := true
		mut var_scope := &ast.Scope(0)
		mut var := ast.VarDecl{}
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
		// prepend mod to look for fn call or const
		mut name := ident.name
		if !name.contains('.') && !(c.file.mod.name in ['builtin', 'main']) {
			name = '${c.file.mod.name}.$ident.name'
		}
		// println('# name: $name')
		// constant
		if constant := c.table.find_const(name) {
			ident.kind = .constant
			ident.info = ast.IdentVar{
				typ: constant.typ
			}
			return constant.typ
		}
		// Function object (not a call), e.g. `onclick(my_click)`
		if func := c.table.find_fn(name) {
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
	expr_type := c.expr(node.cond)
	if expr_type == 0 {
		c.error('match 0 expr type', node.pos)
	}
	c.expected_type = expr_type
	mut ret_type := table.void_type
	for branch in node.branches {
		for expr in branch.exprs {
			c.expected_type = expr_type
			typ := c.expr(expr)
			typ_sym := c.table.get_type_symbol(typ)
			// TODO:
			if typ_sym.kind == .sum_type {}
		}
		c.stmts(branch.stmts)
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			match branch.stmts[branch.stmts.len - 1] {
				ast.ExprStmt {
					ret_type = c.expr(it.expr)
				}
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
	node.expr_type = expr_type
	// println('!m $expr_type')
	return ret_type
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
				t := c.expr(it.expr)
				node.typ = t
				return t
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
	typ_sym := c.table.get_type_symbol(typ)
	// if !table.is_number(typ) {
	if !typ_sym.is_number() {
		println(typ_sym.kind.str())
		c.error('invalid operation: $node.op.str() (non-numeric type `$typ_sym.name`)', node.pos)
	}
	return typ
}

pub fn (c mut Checker) index_expr(node ast.IndexExpr) table.Type {
	typ := c.expr(node.left)
	mut is_range := false // TODO is_range := node.index is ast.RangeExpr
	match node.index {
		ast.RangeExpr {
			is_range = true
		}
		else {}
	}
	if !is_range {
		typ_sym := c.table.get_type_symbol(typ)
		index_type := c.expr(node.index)
		index_type_sym := c.table.get_type_symbol(index_type)
		// println('index expr left=$typ_sym.name $node.pos.line_nr')
		if typ_sym.kind == .array && (!(table.type_idx(index_type) in table.number_idxs) && index_type_sym.kind != .enum_) {
			c.error('non-integer index `$index_type_sym.name` (array type `$typ_sym.name`)', node.pos)
		}
		else if typ_sym.kind == .map && table.type_idx(index_type) != table.string_type_idx {
			c.error('non-string map index (type `$typ_sym.name`)', node.pos)
		}
		if typ_sym.kind == .array {
			// Check index type
			info := typ_sym.info as table.Array
			return info.elem_type
		}
		else if typ_sym.kind == .array_fixed {
			info := typ_sym.info as table.ArrayFixed
			return info.elem_type
		}
		else if typ_sym.kind == .map {
			info := typ_sym.info as table.Map
			return info.value_type
		}
		else if typ_sym.kind in [.byteptr, .string] {
			// TODO: hack need to handle &a[0] comment to see wyahsh errors
			if typ_sym.kind == .byteptr {
				return table.type_to_ptr(table.byte_type)
			}
			return table.byte_type
		}
		// else {
		// return table.int_type
		// }
	}
	return typ
}

// `.green` or `Color.green`
// If a short form is used, `expected_type` needs to be an enum
// with this value.
pub fn (c mut Checker) enum_val(node ast.EnumVal) table.Type {
	typ_idx := if node.enum_name == '' { c.expected_type } else { //
	c.table.find_type_idx(node.enum_name) }
	// println('checker: enum_val: $node.enum_name typeidx=$typ_idx')
	if typ_idx == 0 {
		c.error('not an enum (name=$node.enum_name) (type_idx=0)', node.pos)
	}
	typ := c.table.get_type_symbol(table.Type(typ_idx))
	// println('tname=$typ.name')
	if typ.kind != .enum_ {
		c.error('not an enum', node.pos)
	}
	// info := typ.info as table.Enum
	info := typ.enum_info()
	// rintln('checker: x = $info.x enum val $c.expected_type $typ.name')
	// println(info.vals)
	if !(node.val in info.vals) {
		c.error('enum `$typ.name` does not have a value `$node.val`', node.pos)
	}
	return typ_idx
}

pub fn (c mut Checker) map_init(node mut ast.MapInit) table.Type {
	key0_type := c.expr(node.keys[0])
	val0_type := c.expr(node.vals[0])
	for i, key in node.keys {
		if i == 0 {
			continue
		}
		val := node.vals[i]
		key_type := c.expr(key)
		val_type := c.expr(val)
		if !c.table.check(key_type, key0_type) {
			key0_type_sym := c.table.get_type_symbol(key0_type)
			key_type_sym := c.table.get_type_symbol(key_type)
			c.error('map init: cannot use `$key_type_sym.name` as `$key0_type_sym` for map key', node.pos)
		}
		if !c.table.check(val_type, val0_type) {
			val0_type_sym := c.table.get_type_symbol(val0_type)
			val_type_sym := c.table.get_type_symbol(val_type)
			c.error('map init: cannot use `$val_type_sym.name` as `$val0_type_sym` for map value', node.pos)
		}
	}
	map_type := table.new_type(c.table.find_or_register_map(key0_type, val0_type))
	node.typ = map_type
	return map_type
}

pub fn (c mut Checker) error(s string, pos token.Position) {
	c.nr_errors++
	print_backtrace()
	mut path := c.file.path
	// Get relative path
	workdir := os.getwd() + filepath.separator
	if path.starts_with(workdir) {
		path = path.replace(workdir, '')
	}
	final_msg_line := '$path:$pos.line_nr: checker error #$c.nr_errors: $s'
	c.errors << final_msg_line
	eprintln(final_msg_line)
	/*
	if colored_output {
		eprintln(term.bold(term.red(final_msg_line)))
	}else{
		eprintln(final_msg_line)
	}
	*/

	println('\n\n')
	if c.nr_errors >= max_nr_errors {
		exit(1)
	}
}
