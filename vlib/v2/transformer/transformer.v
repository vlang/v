// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.types

// Transformer performs AST-level transformations to simplify
// and normalize code before codegen. This avoids duplicating
// transformation logic across multiple backends (SSA, cleanc, etc.)
pub struct Transformer {
	env &types.Environment
mut:
	flag_enum_names map[string]bool
	// Track variable -> enum type mappings (inferred from assignments)
	var_enum_types map[string]string
	// Track function parameter -> type name mappings
	param_types map[string]string
	// Track variable -> type mappings for string detection
	var_types map[string]string
	// Track function return types
	fn_return_types map[string]string
	// Current module for scope lookups
	cur_module string
}

pub fn Transformer.new(files []ast.File, env &types.Environment) &Transformer {
	mut t := &Transformer{
		env:             unsafe { env }
		flag_enum_names: map[string]bool{}
		var_enum_types:  map[string]string{}
		param_types:     map[string]string{}
		var_types:       map[string]string{}
		fn_return_types: map[string]string{}
	}
	// Collect flag enum names and function return types from AST
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.EnumDecl {
				if stmt.attributes.has('flag') {
					t.flag_enum_names[stmt.name] = true
				}
			}
			if stmt is ast.FnDecl {
				ret_type := t.expr_to_type_name(stmt.typ.return_type)
				if ret_type != '' {
					t.fn_return_types[stmt.name] = ret_type
				}
			}
		}
	}
	// Also collect flag enums from type environment (includes builtin types)
	lock env.scopes {
		for _, scope in env.scopes {
			t.collect_flag_enums_from_scope(scope)
		}
	}
	return t
}

fn (mut t Transformer) collect_flag_enums_from_scope(scope &types.Scope) {
	for name, obj in scope.objects {
		if obj is types.Type {
			if obj is types.Enum {
				if obj.is_flag {
					t.flag_enum_names[name] = true
				}
			}
		}
	}
}

// transform_files transforms all files and returns transformed copies
pub fn (mut t Transformer) transform_files(files []ast.File) []ast.File {
	mut result := []ast.File{cap: files.len}
	for file in files {
		result << t.transform_file(file)
	}
	return result
}

fn (mut t Transformer) transform_file(file ast.File) ast.File {
	// Set current module for scope lookups
	t.cur_module = file.mod
	// Clear per-file variable tracking
	t.var_enum_types.clear()

	mut stmts := []ast.Stmt{cap: file.stmts.len}
	for stmt in file.stmts {
		stmts << t.transform_stmt(stmt)
	}
	return ast.File{
		attributes: file.attributes
		mod:        file.mod
		name:       file.name
		stmts:      stmts
		imports:    file.imports
	}
}

fn (mut t Transformer) transform_stmt(stmt ast.Stmt) ast.Stmt {
	return match stmt {
		ast.AssignStmt {
			t.transform_assign_stmt(stmt)
		}
		ast.BlockStmt {
			ast.BlockStmt{
				stmts: t.transform_stmts(stmt.stmts)
			}
		}
		ast.ComptimeStmt {
			ast.ComptimeStmt{
				stmt: t.transform_stmt(stmt.stmt)
			}
		}
		ast.DeferStmt {
			ast.DeferStmt{
				mode:  stmt.mode
				stmts: t.transform_stmts(stmt.stmts)
			}
		}
		ast.ExprStmt {
			ast.ExprStmt{
				expr: t.transform_expr(stmt.expr)
			}
		}
		ast.FnDecl {
			t.transform_fn_decl(stmt)
		}
		ast.ForStmt {
			t.transform_for_stmt(stmt)
		}
		ast.ForInStmt {
			t.transform_for_in_stmt(stmt)
		}
		ast.ReturnStmt {
			t.transform_return_stmt(stmt)
		}
		ast.ConstDecl {
			t.transform_const_decl(stmt)
		}
		else {
			stmt
		}
	}
}

fn (mut t Transformer) transform_stmts(stmts []ast.Stmt) []ast.Stmt {
	mut result := []ast.Stmt{cap: stmts.len}
	for stmt in stmts {
		result << t.transform_stmt(stmt)
	}
	return result
}

fn (mut t Transformer) transform_const_decl(decl ast.ConstDecl) ast.ConstDecl {
	mut fields := []ast.FieldInit{cap: decl.fields.len}
	for field in decl.fields {
		fields << ast.FieldInit{
			name:  field.name
			value: t.transform_expr(field.value)
		}
	}
	return ast.ConstDecl{
		is_public: decl.is_public
		fields:    fields
	}
}

fn (mut t Transformer) transform_assign_stmt(stmt ast.AssignStmt) ast.AssignStmt {
	mut rhs := []ast.Expr{cap: stmt.rhs.len}
	for i, expr in stmt.rhs {
		// Track variable -> enum type mapping
		if i < stmt.lhs.len {
			lhs_expr := stmt.lhs[i]
			if lhs_expr is ast.Ident {
				var_name := lhs_expr.name
				enum_type := t.infer_enum_type_from_expr(expr)
				if enum_type in t.flag_enum_names {
					t.var_enum_types[var_name] = enum_type
				}
				// Track string variable assignments
				if t.is_string_expr(expr) {
					t.var_types[var_name] = 'string'
				}
			}
		}
		rhs << t.transform_expr(expr)
	}
	return ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: rhs
		pos: stmt.pos
	}
}

// infer_enum_type_from_expr tries to infer enum type from an expression
// Handles: Permissions.read, Permissions.read | Permissions.write, etc.
fn (t &Transformer) infer_enum_type_from_expr(expr ast.Expr) string {
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			return sel.lhs.name
		}
	}
	// For binary expressions like Permissions.read | Permissions.write,
	// check the LHS
	if expr is ast.InfixExpr {
		infix := expr as ast.InfixExpr
		return t.infer_enum_type_from_expr(infix.lhs)
	}
	if expr is ast.ParenExpr {
		paren := expr as ast.ParenExpr
		return t.infer_enum_type_from_expr(paren.expr)
	}
	return ''
}

fn (mut t Transformer) transform_fn_decl(decl ast.FnDecl) ast.FnDecl {
	// Save current variable/param tracking state
	mut old_var_enum_types := t.var_enum_types.clone()
	mut old_param_types := t.param_types.clone()
	mut old_var_types := t.var_types.clone()

	// Clear per-function tracking
	t.var_enum_types.clear()
	t.param_types.clear()
	t.var_types.clear()

	// Track receiver type for methods
	if decl.is_method {
		receiver_type := t.expr_to_type_name(decl.receiver.typ)
		if receiver_type != '' {
			t.param_types[decl.receiver.name] = receiver_type
		}
	}

	// Track parameter types
	for param in decl.typ.params {
		param_type := t.expr_to_type_name(param.typ)
		if param_type != '' {
			t.param_types[param.name] = param_type
			t.var_types[param.name] = param_type
		}
	}

	// Transform function body
	transformed_stmts := t.transform_stmts(decl.stmts)

	// Restore previous state
	t.var_enum_types = old_var_enum_types.move()
	t.param_types = old_param_types.move()
	t.var_types = old_var_types.move()

	return ast.FnDecl{
		attributes: decl.attributes
		is_public:  decl.is_public
		is_method:  decl.is_method
		is_static:  decl.is_static
		receiver:   decl.receiver
		language:   decl.language
		name:       decl.name
		typ:        decl.typ
		stmts:      transformed_stmts
		pos:        decl.pos
	}
}

// expr_to_type_name extracts a type name from a type expression
fn (t &Transformer) expr_to_type_name(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.SelectorExpr {
		// For module.Type, return just the type name
		return expr.rhs.name
	}
	if expr is ast.PrefixExpr {
		// For &Type or *Type, get the base type
		return t.expr_to_type_name(expr.expr)
	}
	if expr is ast.Type {
		// Handle ast.Type variants
		if expr is ast.ArrayType {
			return 'Array'
		}
		if expr is ast.MapType {
			return 'map'
		}
		if expr is ast.OptionType {
			return t.expr_to_type_name(expr.base_type)
		}
		if expr is ast.ResultType {
			return t.expr_to_type_name(expr.base_type)
		}
	}
	return ''
}

fn (mut t Transformer) transform_for_stmt(stmt ast.ForStmt) ast.ForStmt {
	return ast.ForStmt{
		init:  t.transform_stmt(stmt.init)
		cond:  t.transform_expr(stmt.cond)
		post:  t.transform_stmt(stmt.post)
		stmts: t.transform_stmts(stmt.stmts)
	}
}

fn (mut t Transformer) transform_for_in_stmt(stmt ast.ForInStmt) ast.ForInStmt {
	return ast.ForInStmt{
		key:   stmt.key
		value: stmt.value
		expr:  t.transform_expr(stmt.expr)
	}
}

fn (mut t Transformer) transform_return_stmt(stmt ast.ReturnStmt) ast.ReturnStmt {
	mut exprs := []ast.Expr{cap: stmt.exprs.len}
	for expr in stmt.exprs {
		exprs << t.transform_expr(expr)
	}
	return ast.ReturnStmt{
		exprs: exprs
	}
}

fn (mut t Transformer) transform_expr(expr ast.Expr) ast.Expr {
	return match expr {
		ast.CallExpr {
			t.transform_call_expr(expr)
		}
		ast.CallOrCastExpr {
			t.transform_call_or_cast_expr(expr)
		}
		ast.IfExpr {
			t.transform_if_expr(expr)
		}
		ast.InfixExpr {
			t.transform_infix_expr(expr)
		}
		ast.ParenExpr {
			ast.Expr(ast.ParenExpr{
				expr: t.transform_expr(expr.expr)
			})
		}
		ast.PrefixExpr {
			ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.CastExpr {
			ast.Expr(ast.CastExpr{
				typ:  expr.typ
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.IndexExpr {
			ast.Expr(ast.IndexExpr{
				lhs:      t.transform_expr(expr.lhs)
				expr:     t.transform_expr(expr.expr)
				is_gated: expr.is_gated
			})
		}
		ast.ArrayInitExpr {
			t.transform_array_init_expr(expr)
		}
		ast.MatchExpr {
			t.transform_match_expr(expr)
		}
		ast.ComptimeExpr {
			t.transform_comptime_expr(expr)
		}
		else {
			expr
		}
	}
}

fn (mut t Transformer) transform_array_init_expr(expr ast.ArrayInitExpr) ast.Expr {
	mut exprs := []ast.Expr{cap: expr.exprs.len}
	for e in expr.exprs {
		exprs << t.transform_expr(e)
	}
	return ast.ArrayInitExpr{
		typ:   expr.typ
		exprs: exprs
		init:  t.transform_expr(expr.init)
		cap:   expr.cap
		len:   expr.len
		pos:   expr.pos
	}
}

fn (mut t Transformer) transform_match_expr(expr ast.MatchExpr) ast.Expr {
	mut branches := []ast.MatchBranch{cap: expr.branches.len}
	for branch in expr.branches {
		branches << ast.MatchBranch{
			cond:  branch.cond
			stmts: t.transform_stmts(branch.stmts)
			pos:   branch.pos
		}
	}
	return ast.MatchExpr{
		expr:     t.transform_expr(expr.expr)
		branches: branches
		pos:      expr.pos
	}
}

fn (mut t Transformer) transform_if_expr(expr ast.IfExpr) ast.Expr {
	return ast.IfExpr{
		cond:      t.transform_expr(expr.cond)
		stmts:     t.transform_stmts(expr.stmts)
		else_expr: t.transform_expr(expr.else_expr)
	}
}

fn (mut t Transformer) transform_infix_expr(expr ast.InfixExpr) ast.Expr {
	// Check for string concatenation: string + string
	if expr.op == .plus {
		lhs_is_str := t.is_string_expr(expr.lhs)
		rhs_is_str := t.is_string_expr(expr.rhs)

		// Check for chained concatenation: (s1 + s2) + s3 -> string__plus_two(s1, s2, s3)
		if expr.lhs is ast.InfixExpr {
			lhs_infix := expr.lhs as ast.InfixExpr
			if lhs_infix.op == .plus && t.is_string_expr(lhs_infix.lhs)
				&& t.is_string_expr(lhs_infix.rhs) && rhs_is_str {
				// Transform to string__plus_two(s1, s2, s3)
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'string__plus_two'
					}
					args: [
						t.transform_expr(lhs_infix.lhs),
						t.transform_expr(lhs_infix.rhs),
						t.transform_expr(expr.rhs),
					]
					pos:  expr.pos
				}
			}
		}
		// Check for simple concatenation: s1 + s2 -> string__plus(s1, s2)
		if lhs_is_str && rhs_is_str {
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: 'string__plus'
				}
				args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
				pos:  expr.pos
			}
		}
	}
	// Check for string comparisons: s1 == s2, s1 < s2, etc.
	if expr.op in [.eq, .ne, .lt, .gt, .le, .ge] {
		lhs_is_str := t.is_string_expr(expr.lhs)
		rhs_is_str := t.is_string_expr(expr.rhs)
		if lhs_is_str && rhs_is_str {
			// Transform string comparisons to function calls
			match expr.op {
				.eq {
					// s1 == s2 -> string__eq(s1, s2)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__eq'
						}
						args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
						pos:  expr.pos
					}
				}
				.ne {
					// s1 != s2 -> !string__eq(s1, s2)
					return ast.PrefixExpr{
						op:   .not
						expr: ast.CallExpr{
							lhs:  ast.Ident{
								name: 'string__eq'
							}
							args: [t.transform_expr(expr.lhs),
								t.transform_expr(expr.rhs)]
							pos:  expr.pos
						}
						pos:  expr.pos
					}
				}
				.lt {
					// s1 < s2 -> string__lt(s1, s2)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__lt'
						}
						args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
						pos:  expr.pos
					}
				}
				.gt {
					// s1 > s2 -> string__lt(s2, s1)
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__lt'
						}
						args: [t.transform_expr(expr.rhs), t.transform_expr(expr.lhs)]
						pos:  expr.pos
					}
				}
				.le {
					// s1 <= s2 -> !string__lt(s2, s1)
					return ast.PrefixExpr{
						op:   .not
						expr: ast.CallExpr{
							lhs:  ast.Ident{
								name: 'string__lt'
							}
							args: [t.transform_expr(expr.rhs),
								t.transform_expr(expr.lhs)]
							pos:  expr.pos
						}
						pos:  expr.pos
					}
				}
				.ge {
					// s1 >= s2 -> !string__lt(s1, s2)
					return ast.PrefixExpr{
						op:   .not
						expr: ast.CallExpr{
							lhs:  ast.Ident{
								name: 'string__lt'
							}
							args: [t.transform_expr(expr.lhs),
								t.transform_expr(expr.rhs)]
							pos:  expr.pos
						}
						pos:  expr.pos
					}
				}
				else {}
			}
		}
	}
	// Default: just transform children
	return ast.InfixExpr{
		op:  expr.op
		lhs: t.transform_expr(expr.lhs)
		rhs: t.transform_expr(expr.rhs)
		pos: expr.pos
	}
}

fn (mut t Transformer) transform_call_expr(expr ast.CallExpr) ast.Expr {
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		method_name := sel.rhs.name
		if method_name in ['has', 'all'] {
			// Try to detect if receiver is a flag enum
			receiver_type := t.infer_enum_type(sel.lhs)
			if receiver_type in t.flag_enum_names {
				// Transform the method call
				return t.transform_flag_enum_method(sel.lhs, method_name, expr.args, receiver_type)
			}
		}
	}
	// Default: transform arguments recursively
	mut args := []ast.Expr{cap: expr.args.len}
	for arg in expr.args {
		args << t.transform_expr(arg)
	}
	return ast.CallExpr{
		lhs:  expr.lhs
		args: args
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_call_or_cast_expr(expr ast.CallOrCastExpr) ast.Expr {
	// Check if this is a flag enum method call: receiver.has(arg) or receiver.all(arg)
	if expr.lhs is ast.SelectorExpr {
		sel := expr.lhs as ast.SelectorExpr
		method_name := sel.rhs.name
		if method_name in ['has', 'all'] {
			// Try to detect if receiver is a flag enum
			receiver_type := t.infer_enum_type(sel.lhs)
			if receiver_type in t.flag_enum_names {
				// Transform the method call
				return t.transform_flag_enum_method(sel.lhs, method_name, [expr.expr],
					receiver_type)
			}
		}
	}
	// Default: transform expression recursively
	return ast.CallOrCastExpr{
		lhs:  expr.lhs
		expr: t.transform_expr(expr.expr)
		pos:  expr.pos
	}
}

// infer_enum_type tries to infer the enum type name from an expression
fn (t &Transformer) infer_enum_type(expr ast.Expr) string {
	// For SelectorExpr like Permissions.read or a.flags
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			lhs_name := sel.lhs.name
			// Check if lhs is directly a flag enum name (e.g., Permissions.read)
			if lhs_name in t.flag_enum_names {
				return lhs_name
			}
			// Check if lhs is a variable and rhs is a field (e.g., a.flags)
			// Look up the variable type and find the field type
			field_type := t.resolve_field_type(lhs_name, sel.rhs.name)
			if field_type in t.flag_enum_names {
				return field_type
			}
		}
		// Handle nested selectors (a.b.flags) by recursing on lhs
		if sel.lhs is ast.SelectorExpr {
			// For a.b.flags, we need to resolve a.b first, then .flags
			// This is more complex - for now, try to get type from the innermost
			inner_type := t.infer_expr_type(sel.lhs)
			if inner_type != '' {
				field_type := t.resolve_struct_field_type(inner_type, sel.rhs.name)
				if field_type in t.flag_enum_names {
					return field_type
				}
			}
		}
	}
	// For Ident (variable), check if we tracked its type
	if expr is ast.Ident {
		ident := expr as ast.Ident
		if ident.name in t.var_enum_types {
			return t.var_enum_types[ident.name]
		}
	}
	// For binary expressions, check LHS
	if expr is ast.InfixExpr {
		infix := expr as ast.InfixExpr
		return t.infer_enum_type(infix.lhs)
	}
	if expr is ast.ParenExpr {
		paren := expr as ast.ParenExpr
		return t.infer_enum_type(paren.expr)
	}
	return ''
}

// resolve_field_type looks up the type of a field on a variable
// e.g., for a.flags where a is Array, returns 'ArrayFlags'
fn (t &Transformer) resolve_field_type(var_name string, field_name string) string {
	// First, try to get the variable's type from local tracking
	if var_name in t.var_enum_types {
		// Variable is already a flag enum, no field access needed
		return ''
	}

	// Check if it's a function parameter
	if var_name in t.param_types {
		param_type := t.param_types[var_name]
		// Look up the struct type and find the field
		return t.resolve_struct_field_type(param_type, field_name)
	}

	// Look up the variable in the current module's scope
	mut scope := t.get_current_scope() or { return '' }
	obj := scope.lookup_parent(var_name, 0) or { return '' }

	// Get the variable's type
	var_type := obj.typ()
	return t.get_field_type_name(var_type, field_name)
}

// resolve_struct_field_type looks up a field type given a struct type name
fn (t &Transformer) resolve_struct_field_type(struct_name string, field_name string) string {
	// Look up the struct type in scopes
	lock t.env.scopes {
		for _, scope in t.env.scopes {
			if obj := scope.objects[struct_name] {
				if obj is types.Type {
					return t.get_field_type_name(obj, field_name)
				}
			}
		}
	}
	return ''
}

// get_field_type_name gets the type name of a field from a Type
fn (t &Transformer) get_field_type_name(typ types.Type, field_name string) string {
	if typ is types.Struct {
		for field in typ.fields {
			if field.name == field_name {
				return t.type_to_name(field.typ)
			}
		}
	}
	if typ is types.Pointer {
		// Dereference pointer and recurse
		return t.get_field_type_name(typ.base_type, field_name)
	}
	return ''
}

// type_to_name converts a Type to its name string
fn (t &Transformer) type_to_name(typ types.Type) string {
	if typ is types.Enum {
		return typ.name
	}
	if typ is types.Struct {
		return typ.name
	}
	if typ is types.Alias {
		return typ.name
	}
	if typ is types.NamedType {
		return string(typ)
	}
	return ''
}

// infer_expr_type tries to infer the type name of an expression
fn (t &Transformer) infer_expr_type(expr ast.Expr) string {
	if expr is ast.Ident {
		// Look up variable type
		mut scope := t.get_current_scope() or { return '' }
		obj := scope.lookup_parent(expr.name, 0) or { return '' }
		return t.type_to_name(obj.typ())
	}
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			field_type := t.resolve_field_type(sel.lhs.name, sel.rhs.name)
			return field_type
		}
	}
	return ''
}

// get_current_scope returns the scope for the current module
fn (t &Transformer) get_current_scope() ?&types.Scope {
	return lock t.env.scopes {
		t.env.scopes[t.cur_module] or { return none }
	}
}

// transform_flag_enum_method transforms:
//   receiver.has(flag) → (int(receiver) & int(flag)) != 0
//   receiver.all(flags) → (int(receiver) & int(flags)) == int(flags)
fn (mut t Transformer) transform_flag_enum_method(receiver ast.Expr, method string, args []ast.Expr, enum_type string) ast.Expr {
	if args.len == 0 {
		return ast.empty_expr
	}

	arg := args[0]

	// Resolve enum shorthand: .read → EnumType.read
	resolved_arg := t.resolve_enum_shorthand(arg, enum_type)

	// Cast receiver to int: int(receiver)
	receiver_int := ast.CastExpr{
		typ:  ast.Ident{
			name: 'int'
		}
		expr: receiver
	}

	// Cast arg to int: int(flag)
	arg_int := ast.CastExpr{
		typ:  ast.Ident{
			name: 'int'
		}
		expr: resolved_arg
	}

	// receiver & flag
	and_expr := ast.InfixExpr{
		op:  .amp
		lhs: receiver_int
		rhs: arg_int
	}

	if method == 'has' {
		// (receiver & flag) != 0
		return ast.InfixExpr{
			op:  .ne
			lhs: ast.ParenExpr{
				expr: and_expr
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '0'
			}
		}
	} else { // all
		// (receiver & flags) == int(flags)
		arg_int2 := ast.CastExpr{
			typ:  ast.Ident{
				name: 'int'
			}
			expr: resolved_arg
		}
		return ast.InfixExpr{
			op:  .eq
			lhs: ast.ParenExpr{
				expr: and_expr
			}
			rhs: arg_int2
		}
	}
}

// resolve_enum_shorthand resolves .member → EnumType.member
fn (t &Transformer) resolve_enum_shorthand(expr ast.Expr, enum_type string) ast.Expr {
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		// Check if it's a shorthand: .member (lhs is EmptyExpr or missing)
		if sel.lhs is ast.EmptyExpr {
			// Resolve to EnumType.member
			return ast.SelectorExpr{
				lhs: ast.Ident{
					name: enum_type
				}
				rhs: sel.rhs
				pos: sel.pos
			}
		}
	}
	// For complex expressions (like flag1 | flag2), transform recursively
	if expr is ast.InfixExpr {
		infix := expr as ast.InfixExpr
		return ast.InfixExpr{
			op:  infix.op
			lhs: t.resolve_enum_shorthand(infix.lhs, enum_type)
			rhs: t.resolve_enum_shorthand(infix.rhs, enum_type)
			pos: infix.pos
		}
	}
	if expr is ast.ParenExpr {
		paren := expr as ast.ParenExpr
		return ast.ParenExpr{
			expr: t.resolve_enum_shorthand(paren.expr, enum_type)
		}
	}
	return expr
}

// is_string_expr returns true if the expression is known to be a string
fn (t &Transformer) is_string_expr(expr ast.Expr) bool {
	if expr is ast.StringLiteral {
		// Check for c-strings which are char*, not string
		return !expr.value.starts_with("c'")
	}
	if expr is ast.StringInterLiteral {
		return true
	}
	if expr is ast.Ident {
		// Check tracked variable types
		if t.var_types[expr.name] == 'string' {
			return true
		}
	}
	if expr is ast.ParenExpr {
		return t.is_string_expr(expr.expr)
	}
	if expr is ast.InfixExpr {
		// For + on strings, result is string
		if expr.op == .plus && t.is_string_expr(expr.lhs) {
			return true
		}
	}
	if expr is ast.CallExpr {
		// Check function return type
		if expr.lhs is ast.Ident {
			fn_name := expr.lhs.name
			if t.is_string_returning_fn(fn_name) {
				return true
			}
		}
	}
	if expr is ast.CallOrCastExpr {
		// Check function return type for CallOrCastExpr (single-arg calls)
		if expr.lhs is ast.Ident {
			fn_name := expr.lhs.name
			if t.is_string_returning_fn(fn_name) {
				return true
			}
		}
	}
	return false
}

// is_string_returning_fn returns true if a function is known to return a string
fn (t &Transformer) is_string_returning_fn(fn_name string) bool {
	// Known string-returning functions
	if fn_name in ['string__plus', 'string__plus_two'] {
		return true
	}
	// Check tracked function return types
	if t.fn_return_types[fn_name] == 'string' {
		return true
	}
	// Recognize functions by naming pattern
	if fn_name.ends_with('_to_string') || fn_name.ends_with('__str') {
		return true
	}
	return false
}

// transform_comptime_expr evaluates compile-time conditionals and returns the selected branch
fn (mut t Transformer) transform_comptime_expr(expr ast.ComptimeExpr) ast.Expr {
	// The inner expression should be an IfExpr for $if
	inner := expr.expr
	if inner is ast.IfExpr {
		return t.eval_comptime_if(inner)
	}
	// For other comptime expressions, just return them transformed
	return ast.ComptimeExpr{
		expr: t.transform_expr(inner)
		pos:  expr.pos
	}
}

// eval_comptime_if evaluates a compile-time $if and returns the selected branch expression
fn (mut t Transformer) eval_comptime_if(node ast.IfExpr) ast.Expr {
	cond_result := t.eval_comptime_cond(node.cond)

	if cond_result {
		// Condition is true - return the then branch with transformed statements
		if node.stmts.len == 1 {
			stmt := node.stmts[0]
			if stmt is ast.ExprStmt {
				return t.transform_expr(stmt.expr)
			}
		}
		// Transform all statements in the block
		return ast.ComptimeExpr{
			expr: ast.IfExpr{
				cond:      node.cond
				stmts:     t.transform_stmts(node.stmts)
				else_expr: t.transform_expr(node.else_expr)
			}
		}
	} else {
		// Condition is false - evaluate else branch
		else_e := node.else_expr
		if else_e !is ast.EmptyExpr {
			if else_e is ast.IfExpr {
				if else_e.cond is ast.EmptyExpr {
					// Plain $else block
					if else_e.stmts.len == 1 {
						stmt := else_e.stmts[0]
						if stmt is ast.ExprStmt {
							return t.transform_expr(stmt.expr)
						}
					}
				} else {
					// $else $if - recursive evaluation
					return t.eval_comptime_if(else_e)
				}
			}
		}
	}
	// Fallback - keep original comptime expr instead of returning empty
	return ast.ComptimeExpr{
		expr: node
	}
}

// eval_comptime_cond evaluates a compile-time condition expression
fn (t &Transformer) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			return t.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !t.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return t.eval_comptime_cond(cond.lhs) && t.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return t.eval_comptime_cond(cond.lhs) || t.eval_comptime_cond(cond.rhs)
			}
		}
		ast.PostfixExpr {
			// Handle optional feature check: feature?
			if cond.op == .question {
				inner := cond.expr
				if inner is ast.Ident {
					return t.eval_comptime_flag(inner.name)
				}
			}
		}
		ast.ParenExpr {
			return t.eval_comptime_cond(cond.expr)
		}
		else {}
	}
	return false
}

// eval_comptime_flag evaluates a single comptime flag/identifier
fn (t &Transformer) eval_comptime_flag(name string) bool {
	match name {
		'macos', 'darwin' {
			$if macos {
				return true
			}
			return false
		}
		'linux' {
			$if linux {
				return true
			}
			return false
		}
		'windows' {
			$if windows {
				return true
			}
			return false
		}
		'freebsd' {
			$if freebsd {
				return true
			}
			return false
		}
		'x64', 'amd64' {
			$if amd64 {
				return true
			}
			return false
		}
		'arm64', 'aarch64' {
			$if arm64 {
				return true
			}
			return false
		}
		'little_endian' {
			$if little_endian {
				return true
			}
			return false
		}
		'big_endian' {
			$if big_endian {
				return true
			}
			return false
		}
		'debug' {
			$if debug {
				return true
			}
			return false
		}
		// Feature flags that are typically false
		'new_int', 'gcboehm', 'prealloc', 'autofree' {
			return false
		}
		else {
			return false
		}
	}
}
