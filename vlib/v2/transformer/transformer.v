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
	// Track which functions return Result types (!T) -> base type (empty for void)
	fn_returns_result map[string]string
	// Track which functions return Option types (?T) -> base type (empty for void)
	fn_returns_option map[string]string
	// Current module for scope lookups
	cur_module string
	// Temp variable counter for desugaring
	temp_counter int
}

pub fn Transformer.new(files []ast.File, env &types.Environment) &Transformer {
	mut t := &Transformer{
		env:               unsafe { env }
		flag_enum_names:   map[string]bool{}
		var_enum_types:    map[string]string{}
		param_types:       map[string]string{}
		var_types:         map[string]string{}
		fn_return_types:   map[string]string{}
		fn_returns_result: map[string]string{}
		fn_returns_option: map[string]string{}
	}
	// Collect flag enum names, function return types, and string constants from AST
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
				// Track Result/Option return types with their base type
				ret_expr := stmt.typ.return_type
				if ret_expr is ast.Type {
					if ret_expr is ast.ResultType {
						base_type := t.expr_to_type_name(ret_expr.base_type)
						t.fn_returns_result[stmt.name] = base_type
					} else if ret_expr is ast.OptionType {
						base_type := t.expr_to_type_name(ret_expr.base_type)
						t.fn_returns_option[stmt.name] = base_type
					}
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
	// Add known builtin functions that return Result or Option
	t.add_builtin_result_option_fns()
	return t
}

// add_builtin_result_option_fns adds known builtin functions that return Result or Option types
fn (mut t Transformer) add_builtin_result_option_fns() {
	// Builtin functions returning Result (!)
	t.fn_returns_result['at_exit'] = '' // returns !
	t.fn_returns_result['atexit'] = '' // alias for at_exit
	// String methods returning Option
	t.fn_returns_option['index'] = 'int' // string.index() returns ?int
	t.fn_returns_option['last_index'] = 'int' // string.last_index() returns ?int
	t.fn_returns_option['index_any'] = 'int' // string.index_any() returns ?int
	t.fn_returns_option['index_after'] = 'int' // string.index_after() returns ?int
	// Array methods returning Option
	t.fn_returns_option['first'] = '' // array.first() - type unknown, will use default
	t.fn_returns_option['last'] = '' // array.last()
	t.fn_returns_option['pop'] = '' // array.pop()
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
	// Check for OrExpr assignment that needs expansion
	if stmt is ast.AssignStmt {
		if expanded := t.try_expand_or_expr_assign(stmt) {
			return expanded
		}
		// Check for map index assignment: m[key] = val -> __Map_K_V_set(&m, key, val)
		if transformed := t.try_transform_map_index_assign(stmt) {
			return transformed
		}
	}
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
		// Check for OrExpr assignment that expands to multiple statements
		if stmt is ast.AssignStmt {
			if expanded := t.try_expand_or_expr_assign_stmts(stmt) {
				// Transform each expanded statement to trigger type tracking
				for exp_stmt in expanded {
					result << t.transform_stmt(exp_stmt)
				}
				continue
			}
		}
		// Check for OrExpr in expression statements (e.g., println(may_fail() or { 0 }))
		if stmt is ast.ExprStmt {
			if expanded := t.try_expand_or_expr_stmt(stmt) {
				// Transform each expanded statement
				for exp_stmt in expanded {
					result << t.transform_stmt(exp_stmt)
				}
				continue
			}
		}
		// Check for OrExpr in return statements
		if stmt is ast.ReturnStmt {
			if expanded := t.try_expand_or_expr_return(stmt) {
				// Transform each expanded statement
				for exp_stmt in expanded {
					result << t.transform_stmt(exp_stmt)
				}
				continue
			}
		}
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
	// Check for string compound assignment: p += x -> p = string__plus(p, x)
	if stmt.op == .plus_assign && stmt.lhs.len == 1 && stmt.rhs.len == 1 {
		lhs_expr := stmt.lhs[0]
		if t.is_string_expr(lhs_expr) {
			// Transform p += x to p = string__plus(p, x)
			return ast.AssignStmt{
				op:  .assign
				lhs: stmt.lhs
				rhs: [
					ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__plus'
						}
						args: [t.transform_expr(lhs_expr), t.transform_expr(stmt.rhs[0])]
						pos:  stmt.pos
					},
				]
				pos: stmt.pos
			}
		}
	}

	mut rhs := []ast.Expr{cap: stmt.rhs.len}
	for i, expr in stmt.rhs {
		// Track variable -> enum type mapping
		if i < stmt.lhs.len {
			lhs_expr := stmt.lhs[i]
			// Get variable name, handling ModifierExpr (mut, shared, etc.)
			var_name := t.get_var_name(lhs_expr)
			if var_name != '' {
				enum_type := t.infer_enum_type_from_expr(expr)
				if enum_type != '' {
					// Track all enum types in var_types for shorthand resolution
					t.var_types[var_name] = enum_type
					// Also track flag enums separately for flag-specific transformations
					if enum_type in t.flag_enum_names {
						t.var_enum_types[var_name] = enum_type
					}
				}
				// Track array variable assignments FIRST
				// (we check array before string because spath[i] from []string is a string, not an array)
				if array_type := t.infer_array_type(expr) {
					t.var_types[var_name] = array_type
				} else if t.is_string_expr(expr) {
					// Track string variable assignments
					t.var_types[var_name] = 'string'
				}
				// Track map variable assignments
				if map_type := t.infer_map_type(expr) {
					t.var_types[var_name] = map_type
				}
				// Track struct/pointer variable assignments (e.g., mut parent := &mapnode(unsafe { nil }))
				if struct_type := t.infer_struct_type(expr) {
					t.var_types[var_name] = struct_type
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

// get_var_name extracts the variable name from an expression, handling ModifierExpr
fn (t &Transformer) get_var_name(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.ModifierExpr {
		// Unwrap modifier (mut, shared, etc.) to get the actual ident
		if expr.expr is ast.Ident {
			return expr.expr.name
		}
	}
	return ''
}

// try_expand_or_expr_assign checks if an assignment has an OrExpr RHS (used by transform_stmt)
// Returns none since expansion is handled by try_expand_or_expr_assign_stmts at the list level
fn (mut t Transformer) try_expand_or_expr_assign(stmt ast.AssignStmt) ?ast.Stmt {
	return none
}

// try_transform_map_index_assign transforms map index assignment to a function call.
// Transforms: m[key] = val -> __Map_K_V_set(&m, key, val)
fn (mut t Transformer) try_transform_map_index_assign(stmt ast.AssignStmt) ?ast.Stmt {
	// Only handle simple assignment (not compound assignment like +=)
	if stmt.op != .assign {
		return none
	}
	// Check for single LHS that is an IndexExpr
	if stmt.lhs.len != 1 || stmt.rhs.len != 1 {
		return none
	}
	lhs := stmt.lhs[0]
	if lhs !is ast.IndexExpr {
		return none
	}
	index_expr := lhs as ast.IndexExpr
	// Check if the indexed expression is a map
	map_type := t.get_map_type_for_expr(index_expr.lhs) or { return none }

	// Transform to: __Map_K_V_set(&m, key, val)
	return ast.ExprStmt{
		expr: ast.CallExpr{
			lhs:  ast.Ident{
				name: '__${map_type}_set'
			}
			args: [
				// &m (address of map)
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: t.transform_expr(index_expr.lhs)
				}),
				// key
				t.transform_expr(index_expr.expr),
				// val
				t.transform_expr(stmt.rhs[0]),
			]
			pos:  stmt.pos
		}
	}
}

// try_expand_or_expr_assign_stmts expands an OrExpr assignment to multiple statements.
// Transforms: a := may_fail(5) or { 0 }
// Into:
//   _t1 := may_fail(5)
//   if _t1.is_error { err := _t1.err; _t1.data = 0 }
//   a := _t1.data
fn (mut t Transformer) try_expand_or_expr_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	// Check for single assignment with OrExpr somewhere in RHS
	if stmt.rhs.len != 1 || stmt.lhs.len != 1 {
		return none
	}
	rhs_expr := stmt.rhs[0]
	// Check if RHS is directly an OrExpr (simple case)
	if rhs_expr is ast.OrExpr {
		return t.expand_direct_or_expr_assign(stmt, rhs_expr)
	}
	// Check if RHS contains an OrExpr (nested case like cast(OrExpr))
	if t.expr_has_or_expr(rhs_expr) {
		mut prefix_stmts := []ast.Stmt{}
		new_rhs := t.extract_or_expr(rhs_expr, mut prefix_stmts)
		if prefix_stmts.len == 0 {
			return none
		}
		// Add the final assignment with the extracted expression
		prefix_stmts << ast.AssignStmt{
			op:  stmt.op
			lhs: stmt.lhs
			rhs: [t.transform_expr(new_rhs)]
			pos: stmt.pos
		}
		return prefix_stmts
	}
	return none
}

// expand_direct_or_expr_assign handles the simple case where RHS is directly an OrExpr
fn (mut t Transformer) expand_direct_or_expr_assign(stmt ast.AssignStmt, or_expr ast.OrExpr) ?[]ast.Stmt {
	// The inner expression should be a call that returns Result or Option
	call_expr := or_expr.expr
	fn_name := t.get_call_fn_name(call_expr)
	if fn_name == '' {
		return none
	}
	// Check if function returns Result or Option and get base type
	mut is_result := false
	mut is_option := false
	mut base_type := ''
	if fn_name in t.fn_returns_result {
		is_result = true
		base_type = t.fn_returns_result[fn_name]
	} else if fn_name in t.fn_returns_option {
		is_option = true
		base_type = t.fn_returns_option[fn_name]
	} else {
		return none
	}
	is_void_result := base_type == '' || base_type == 'void'
	_ = is_option // suppress unused warning
	// Generate temp variable name
	temp_name := t.gen_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}
	// Build the expanded statements
	mut stmts := []ast.Stmt{}
	// 1. _t1 := call_expr
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(temp_ident)]
		rhs: [t.transform_expr(call_expr)]
		pos: stmt.pos
	}
	// 2. if _t1.is_error { ... } (for Result) or if _t1.state != 0 { ... } (for Option)
	error_cond := if is_result {
		// _t1.is_error
		ast.Expr(ast.SelectorExpr{
			lhs: temp_ident
			rhs: ast.Ident{
				name: 'is_error'
			}
		})
	} else {
		// _t1.state != 0
		ast.Expr(ast.InfixExpr{
			op:  .ne
			lhs: ast.SelectorExpr{
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'state'
				}
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '0'
			}
		})
	}
	// Build the if-block statements
	mut if_stmts := []ast.Stmt{}
	// Declare err variable: err := _t1.err
	if_stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'err'
		})]
		rhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'err'
				}
			}),
		]
	}
	// Check if or-block contains a return statement (control flow)
	if t.or_block_has_return(or_expr.stmts) {
		// Or-block contains return - use the statements directly
		if_stmts << t.transform_stmts(or_expr.stmts)
	} else if !is_void_result {
		// Or-block provides a value - assign to data (only for non-void results)
		or_value := t.get_or_block_value(or_expr.stmts)
		// _t1.data = or_value (the backend will handle proper casting)
		if_stmts << ast.AssignStmt{
			op:  .assign
			lhs: [
				ast.Expr(ast.SelectorExpr{
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'data'
					}
				}),
			]
			rhs: [or_value]
		}
	}
	stmts << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  error_cond
			stmts: if_stmts
		}
	}
	// 3. a := _t1.data (extract value) - only for non-void results
	if !is_void_result {
		// Track the variable type based on the known return type
		// This is necessary because the RHS (_t.data) is a SelectorExpr which
		// isn't recognized as a string by is_string_expr
		if stmt.lhs.len > 0 {
			var_name := t.get_var_name(stmt.lhs[0])
			if var_name != '' && base_type != '' {
				t.var_types[var_name] = base_type
			}
		}
		stmts << ast.AssignStmt{
			op:  stmt.op
			lhs: stmt.lhs
			rhs: [
				ast.Expr(ast.SelectorExpr{
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'data'
					}
				}),
			]
			pos: stmt.pos
		}
	}
	return stmts
}

// gen_temp_name generates a unique temporary variable name
fn (mut t Transformer) gen_temp_name() string {
	t.temp_counter++
	return '_or_t${t.temp_counter}'
}

// get_call_fn_name extracts the function name from a call expression
fn (t &Transformer) get_call_fn_name(expr ast.Expr) string {
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name
		}
		// Handle module-qualified calls: strconv.common_parse_int(...)
		if expr.lhs is ast.SelectorExpr {
			return expr.lhs.rhs.name
		}
	}
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			return expr.lhs.name
		}
		// Handle module-qualified calls: strconv.common_parse_int(...)
		if expr.lhs is ast.SelectorExpr {
			return expr.lhs.rhs.name
		}
	}
	return ''
}

// or_block_has_return checks if the or-block contains a control flow statement
// (return, continue, break, panic, exit)
fn (t &Transformer) or_block_has_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if stmt is ast.ReturnStmt {
			return true
		}
		if stmt is ast.FlowControlStmt {
			// break, continue, goto
			return true
		}
		if stmt is ast.ExprStmt {
			// Check for panic() or exit() calls
			if stmt.expr is ast.CallExpr {
				if stmt.expr.lhs is ast.Ident {
					name := stmt.expr.lhs.name
					if name in ['panic', 'exit'] {
						return true
					}
				}
			} else if stmt.expr is ast.CallOrCastExpr {
				if stmt.expr.lhs is ast.Ident {
					name := stmt.expr.lhs.name
					if name in ['panic', 'exit'] {
						return true
					}
				}
			}
		}
	}
	return false
}

// get_or_block_value extracts the value expression from an or-block
// The value is typically the last expression statement, or 0/default for empty blocks
fn (mut t Transformer) get_or_block_value(stmts []ast.Stmt) ast.Expr {
	if stmts.len == 0 {
		return ast.BasicLiteral{
			kind:  .number
			value: '0'
		}
	}
	// Check if last statement is an expression statement (the value)
	last := stmts[stmts.len - 1]
	if last is ast.ExprStmt {
		return t.transform_expr(last.expr)
	}
	// For more complex blocks, just return 0 for now
	return ast.BasicLiteral{
		kind:  .number
		value: '0'
	}
}

// try_expand_or_expr_stmt handles OrExpr in expression statements like println(may_fail() or { 0 })
// Transforms: println(may_fail(5) or { 0 })
// Into:
//   _t1 := may_fail(5)
//   if _t1.is_error { err := _t1.err; _t1.data = 0 }
//   println(_t1.data)
fn (mut t Transformer) try_expand_or_expr_stmt(stmt ast.ExprStmt) ?[]ast.Stmt {
	// Check if expression contains any OrExpr
	if !t.expr_has_or_expr(stmt.expr) {
		return none
	}
	// Extract OrExpr and get prefix statements + transformed expression
	mut prefix_stmts := []ast.Stmt{}
	new_expr := t.extract_or_expr(stmt.expr, mut prefix_stmts)
	if prefix_stmts.len == 0 {
		return none
	}
	// Add the final expression statement
	prefix_stmts << ast.ExprStmt{
		expr: t.transform_expr(new_expr)
	}
	return prefix_stmts
}

// try_expand_or_expr_return handles OrExpr in return statements
// Transforms: return may_fail(5) or { 0 }
// Into:
//   _t1 := may_fail(5)
//   if _t1.is_error { err := _t1.err; _t1.data = 0 }
//   return _t1.data
fn (mut t Transformer) try_expand_or_expr_return(stmt ast.ReturnStmt) ?[]ast.Stmt {
	// Check if any return expression contains OrExpr
	mut has_or_expr := false
	for expr in stmt.exprs {
		if t.expr_has_or_expr(expr) {
			has_or_expr = true
			break
		}
	}
	if !has_or_expr {
		return none
	}
	// Extract OrExpr from all return expressions
	mut prefix_stmts := []ast.Stmt{}
	mut new_exprs := []ast.Expr{cap: stmt.exprs.len}
	for expr in stmt.exprs {
		new_expr := t.extract_or_expr(expr, mut prefix_stmts)
		new_exprs << t.transform_expr(new_expr)
	}
	if prefix_stmts.len == 0 {
		return none
	}
	// Add the final return statement
	prefix_stmts << ast.ReturnStmt{
		exprs: new_exprs
	}
	return prefix_stmts
}

// expr_has_or_expr checks if an expression contains any OrExpr
fn (t &Transformer) expr_has_or_expr(expr ast.Expr) bool {
	if expr is ast.OrExpr {
		return true
	}
	match expr {
		ast.CallExpr {
			for arg in expr.args {
				if t.expr_has_or_expr(arg) {
					return true
				}
			}
		}
		ast.CallOrCastExpr {
			if t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.InfixExpr {
			if t.expr_has_or_expr(expr.lhs) || t.expr_has_or_expr(expr.rhs) {
				return true
			}
		}
		ast.PrefixExpr {
			if t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.ParenExpr {
			if t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.IndexExpr {
			if t.expr_has_or_expr(expr.lhs) || t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.SelectorExpr {
			if t.expr_has_or_expr(expr.lhs) {
				return true
			}
		}
		ast.CastExpr {
			if t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.IfExpr {
			if t.expr_has_or_expr(expr.cond) {
				return true
			}
			if t.expr_has_or_expr(expr.else_expr) {
				return true
			}
			// Note: stmts inside IfExpr are handled separately by transform_stmts
		}
		ast.MatchExpr {
			if t.expr_has_or_expr(expr.expr) {
				return true
			}
		}
		ast.ArrayInitExpr {
			for e in expr.exprs {
				if t.expr_has_or_expr(e) {
					return true
				}
			}
		}
		ast.InitExpr {
			for field in expr.fields {
				if t.expr_has_or_expr(field.value) {
					return true
				}
			}
		}
		else {}
	}
	return false
}

// extract_or_expr extracts OrExpr from an expression tree.
// It generates prefix statements for the OrExpr expansion and returns the expression
// with OrExpr replaced by the temp variable's data access.
fn (mut t Transformer) extract_or_expr(expr ast.Expr, mut prefix_stmts []ast.Stmt) ast.Expr {
	// If this is an OrExpr, expand it directly
	if expr is ast.OrExpr {
		return t.expand_single_or_expr(expr, mut prefix_stmts)
	}
	// Recursively check sub-expressions
	match expr {
		ast.CallExpr {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.extract_or_expr(arg, mut prefix_stmts)
			}
			return ast.CallExpr{
				lhs:  expr.lhs
				args: new_args
				pos:  expr.pos
			}
		}
		ast.CallOrCastExpr {
			new_inner := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.CallOrCastExpr{
				lhs:  expr.lhs
				expr: new_inner
				pos:  expr.pos
			}
		}
		ast.InfixExpr {
			new_lhs := t.extract_or_expr(expr.lhs, mut prefix_stmts)
			new_rhs := t.extract_or_expr(expr.rhs, mut prefix_stmts)
			return ast.InfixExpr{
				op:  expr.op
				lhs: new_lhs
				rhs: new_rhs
				pos: expr.pos
			}
		}
		ast.PrefixExpr {
			new_inner := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.PrefixExpr{
				op:   expr.op
				expr: new_inner
				pos:  expr.pos
			}
		}
		ast.ParenExpr {
			new_inner := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.ParenExpr{
				expr: new_inner
			}
		}
		ast.IndexExpr {
			new_lhs := t.extract_or_expr(expr.lhs, mut prefix_stmts)
			new_idx := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.IndexExpr{
				lhs:      new_lhs
				expr:     new_idx
				is_gated: expr.is_gated
			}
		}
		ast.SelectorExpr {
			new_lhs := t.extract_or_expr(expr.lhs, mut prefix_stmts)
			return ast.SelectorExpr{
				lhs: new_lhs
				rhs: expr.rhs
				pos: expr.pos
			}
		}
		ast.CastExpr {
			new_inner := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.CastExpr{
				typ:  expr.typ
				expr: new_inner
				pos:  expr.pos
			}
		}
		ast.IfExpr {
			new_cond := t.extract_or_expr(expr.cond, mut prefix_stmts)
			new_else := t.extract_or_expr(expr.else_expr, mut prefix_stmts)
			return ast.IfExpr{
				cond:      new_cond
				stmts:     expr.stmts // stmts are processed separately by transform_stmts
				else_expr: new_else
			}
		}
		ast.MatchExpr {
			new_matched := t.extract_or_expr(expr.expr, mut prefix_stmts)
			return ast.MatchExpr{
				expr:     new_matched
				branches: expr.branches
				pos:      expr.pos
			}
		}
		ast.ArrayInitExpr {
			mut new_exprs := []ast.Expr{cap: expr.exprs.len}
			for e in expr.exprs {
				new_exprs << t.extract_or_expr(e, mut prefix_stmts)
			}
			return ast.ArrayInitExpr{
				typ:   expr.typ
				exprs: new_exprs
			}
		}
		ast.InitExpr {
			mut new_fields := []ast.FieldInit{cap: expr.fields.len}
			for field in expr.fields {
				new_fields << ast.FieldInit{
					name:  field.name
					value: t.extract_or_expr(field.value, mut prefix_stmts)
				}
			}
			return ast.InitExpr{
				typ:    expr.typ
				fields: new_fields
			}
		}
		else {
			return expr
		}
	}
}

// expand_single_or_expr expands a single OrExpr and returns the data access expression
fn (mut t Transformer) expand_single_or_expr(or_expr ast.OrExpr, mut prefix_stmts []ast.Stmt) ast.Expr {
	call_expr := or_expr.expr
	fn_name := t.get_call_fn_name(call_expr)
	if fn_name == '' {
		// Not a known function call, return as-is (shouldn't happen)
		return or_expr
	}
	// Check if function returns Result or Option and get base type
	mut is_result := false
	mut is_option := false
	mut base_type := ''
	if fn_name in t.fn_returns_result {
		is_result = true
		base_type = t.fn_returns_result[fn_name]
	} else if fn_name in t.fn_returns_option {
		is_option = true
		base_type = t.fn_returns_option[fn_name]
	} else {
		// Not a Result/Option function, return as-is
		return or_expr
	}
	is_void_result := base_type == '' || base_type == 'void'
	_ = is_option // suppress unused warning
	// Generate temp variable name
	temp_name := t.gen_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}
	// 1. _t1 := call_expr
	prefix_stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(temp_ident)]
		rhs: [t.transform_expr(call_expr)]
	}
	// 2. if _t1.is_error { ... } (for Result) or if _t1.state != 0 { ... } (for Option)
	error_cond := if is_result {
		// _t1.is_error
		ast.Expr(ast.SelectorExpr{
			lhs: temp_ident
			rhs: ast.Ident{
				name: 'is_error'
			}
		})
	} else {
		// _t1.state != 0
		ast.Expr(ast.InfixExpr{
			op:  .ne
			lhs: ast.SelectorExpr{
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'state'
				}
			}
			rhs: ast.BasicLiteral{
				kind:  .number
				value: '0'
			}
		})
	}
	// Build the if-block statements
	mut if_stmts := []ast.Stmt{}
	// Declare err variable: err := _t1.err
	if_stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'err'
		})]
		rhs: [
			ast.Expr(ast.SelectorExpr{
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'err'
				}
			}),
		]
	}
	// Check if or-block contains a return statement (control flow)
	if t.or_block_has_return(or_expr.stmts) {
		// Or-block contains return - use the statements directly
		if_stmts << t.transform_stmts(or_expr.stmts)
	} else if !is_void_result {
		// Or-block provides a value - assign to data (only for non-void results)
		or_value := t.get_or_block_value(or_expr.stmts)
		// _t1.data = or_value
		if_stmts << ast.AssignStmt{
			op:  .assign
			lhs: [
				ast.Expr(ast.SelectorExpr{
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'data'
					}
				}),
			]
			rhs: [or_value]
		}
	}
	prefix_stmts << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  error_cond
			stmts: if_stmts
		}
	}
	// Return the data access expression (or empty expr for void)
	if is_void_result {
		// For void results, return an empty expression since there's no value
		return ast.empty_expr
	}
	return ast.SelectorExpr{
		lhs: temp_ident
		rhs: ast.Ident{
			name: 'data'
		}
	}
}

// infer_enum_type_from_expr tries to infer enum type from an expression
// Handles: Permissions.read, Permissions.read | Permissions.write, StrIntpType(x), unsafe { EnumType(...) }
fn (t &Transformer) infer_enum_type_from_expr(expr ast.Expr) string {
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			// Check if it's actually an enum type before returning
			// This prevents incorrectly treating _or_t.data as an enum expression
			name := sel.lhs.name
			// If name starts with underscore, it's likely a temp variable, not an enum
			if name.starts_with('_') {
				return ''
			}
			// Verify it's a known type by checking the scope
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(name, 0) {
					typ := obj.typ()
					if typ is types.Enum {
						return name
					}
					// Also return if it looks like an enum type (first char is uppercase for V enums)
					if typ is types.Struct {
						return '' // Structs shouldn't be treated as enums
					}
				}
			}
			// Fallback: assume it's an enum if name starts with uppercase (V enum convention)
			if name.len > 0 && name[0] >= `A` && name[0] <= `Z` {
				return name
			}
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
	// Handle cast expressions like StrIntpType(x & 0x1F)
	if expr is ast.CastExpr {
		cast := expr as ast.CastExpr
		if cast.typ is ast.Ident {
			return cast.typ.name
		}
	}
	// Handle CallOrCastExpr which might be a cast
	if expr is ast.CallOrCastExpr {
		call_or_cast := expr as ast.CallOrCastExpr
		// If it looks like EnumType(value), treat it as a cast to enum type
		if call_or_cast.lhs is ast.Ident {
			return (call_or_cast.lhs as ast.Ident).name
		}
	}
	// Handle unsafe { expr } - look inside
	if expr is ast.UnsafeExpr {
		unsafe_expr := expr as ast.UnsafeExpr
		// Look at the last statement in the unsafe block for the return value
		if unsafe_expr.stmts.len > 0 {
			last_stmt := unsafe_expr.stmts[unsafe_expr.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				return t.infer_enum_type_from_expr(last_stmt.expr)
			}
		}
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
		mut receiver_type := t.expr_to_type_name(decl.receiver.typ)
		// mut receivers are passed as pointers
		if decl.receiver.is_mut && !receiver_type.ends_with('*') {
			receiver_type += '*'
		}
		if receiver_type != '' {
			t.param_types[decl.receiver.name] = receiver_type
			t.var_types[decl.receiver.name] = receiver_type
		}
	}

	// Track parameter types
	for param in decl.typ.params {
		mut param_type := t.expr_to_type_name(param.typ)
		// mut parameters are passed as pointers
		if param.is_mut && !param_type.ends_with('*') {
			param_type += '*'
		}
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
		// For module.Type, return module__Type
		if expr.lhs is ast.Ident {
			return '${expr.lhs.name}__${expr.rhs.name}'
		}
		return expr.rhs.name
	}
	if expr is ast.PrefixExpr {
		// For &Type or *Type, preserve pointer type
		base_type := t.expr_to_type_name(expr.expr)
		if expr.op == .amp {
			// &char -> charptr, &Type -> Type*
			if base_type == 'char' {
				return 'charptr'
			}
			return base_type + '*'
		}
		// For variadic types (...Type), return VArg_Type
		if expr.op == .ellipsis {
			return 'VArg_${base_type}'
		}
		return base_type
	}
	if expr is ast.Type {
		// Handle ast.Type variants
		if expr is ast.ArrayType {
			elem_type := t.expr_to_type_name(expr.elem_type)
			if elem_type != '' {
				return 'Array_${elem_type}'
			}
			return 'Array'
		}
		if expr is ast.MapType {
			key_type := t.expr_to_type_name(expr.key_type)
			value_type := t.expr_to_type_name(expr.value_type)
			return 'Map_${key_type}_${value_type}'
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
	// Track loop variable types for ForInStmt
	if stmt.init is ast.ForInStmt {
		for_in := stmt.init as ast.ForInStmt
		// Try to get the type of the iterated expression
		if expr_type := t.get_expr_type(for_in.expr) {
			// Track key variable type
			key_type_name := t.get_loop_key_type_name(expr_type)
			if for_in.key is ast.Ident {
				if key_type_name != '' {
					t.var_types[for_in.key.name] = key_type_name
				}
			}
			// Track value variable type
			value_type_name := t.get_loop_value_type_name(expr_type)
			if for_in.value is ast.Ident {
				if value_type_name != '' {
					t.var_types[for_in.value.name] = value_type_name
				}
			} else if for_in.value is ast.ModifierExpr {
				// Handle `for mut v in arr` pattern
				mod_expr := for_in.value as ast.ModifierExpr
				if mod_expr.expr is ast.Ident {
					if value_type_name != '' {
						t.var_types[mod_expr.expr.name] = value_type_name
					}
				}
			}
		} else {
			// Fallback: try to infer from var_types if expr is an Ident
			t.track_for_in_types_from_var_types(for_in)
		}
	}
	return ast.ForStmt{
		init:  t.transform_stmt(stmt.init)
		cond:  t.transform_expr(stmt.cond)
		post:  t.transform_stmt(stmt.post)
		stmts: t.transform_stmts(stmt.stmts)
	}
}

// get_loop_key_type_name returns the type name for the key variable in a for-in loop
fn (t &Transformer) get_loop_key_type_name(typ types.Type) string {
	// For most iterable types, key is int (index)
	// Map keys are handled in the fallback
	return 'int'
}

// get_loop_value_type_name returns the type name for the value variable in a for-in loop
fn (t &Transformer) get_loop_value_type_name(typ types.Type) string {
	match typ {
		types.Array {
			return t.type_to_c_name(typ.elem_type)
		}
		types.Pointer {
			// For pointers, recurse into the base type
			return t.get_loop_value_type_name(typ.base_type)
		}
		types.Struct {
			// String iteration yields u8
			if typ.name == 'string' {
				return 'u8'
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (mut t Transformer) transform_for_in_stmt(stmt ast.ForInStmt) ast.ForInStmt {
	return ast.ForInStmt{
		key:   stmt.key
		value: stmt.value
		expr:  t.transform_expr(stmt.expr)
	}
}

// track_for_in_types_from_var_types infers loop variable types from var_types when get_expr_type fails
fn (mut t Transformer) track_for_in_types_from_var_types(for_in ast.ForInStmt) {
	if for_in.expr is ast.Ident {
		// Check both var_types and param_types for the iterable type
		var_type := t.var_types[for_in.expr.name] or { t.param_types[for_in.expr.name] or { '' } }
		if var_type != '' {
			// For arrays, key is int and value is element type
			if var_type.starts_with('Array_') {
				elem_type := var_type['Array_'.len..]
				// Track key as int
				if for_in.key is ast.Ident {
					t.var_types[for_in.key.name] = 'int'
				}
				// Track value as element type
				if for_in.value is ast.Ident {
					t.var_types[for_in.value.name] = elem_type
				} else if for_in.value is ast.ModifierExpr {
					mod_expr := for_in.value as ast.ModifierExpr
					if mod_expr.expr is ast.Ident {
						t.var_types[mod_expr.expr.name] = elem_type
					}
				}
			}
			// For variadic arguments (VArg_Type), key is int and value is element type
			if var_type.starts_with('VArg_') {
				elem_type := var_type['VArg_'.len..]
				// Track key as int
				if for_in.key is ast.Ident {
					t.var_types[for_in.key.name] = 'int'
				}
				// Track value as element type
				if for_in.value is ast.Ident {
					t.var_types[for_in.value.name] = elem_type
				} else if for_in.value is ast.ModifierExpr {
					mod_expr := for_in.value as ast.ModifierExpr
					if mod_expr.expr is ast.Ident {
						t.var_types[mod_expr.expr.name] = elem_type
					}
				}
			}
			// For maps, key_type and value_type are in the map type name
			// Map_KeyType_ValueType
			if var_type.starts_with('Map_') {
				rest := var_type['Map_'.len..]
				// Find the separator between key and value types
				// This is a simplification; may need more robust parsing
				parts := rest.split('_')
				if parts.len >= 2 {
					key_type := parts[0]
					value_type := parts[1..].join('_')
					if for_in.key is ast.Ident {
						t.var_types[for_in.key.name] = key_type
					}
					if for_in.value is ast.Ident {
						t.var_types[for_in.value.name] = value_type
					}
				}
			}
		}
	}
	// Handle method calls that return string arrays
	// Check both CallExpr and CallOrCastExpr (single-arg method calls)
	mut method_name := ''
	if for_in.expr is ast.CallExpr {
		call := for_in.expr as ast.CallExpr
		if call.lhs is ast.SelectorExpr {
			sel := call.lhs as ast.SelectorExpr
			method_name = sel.rhs.name
		}
	} else if for_in.expr is ast.CallOrCastExpr {
		call := for_in.expr as ast.CallOrCastExpr
		if call.lhs is ast.SelectorExpr {
			sel := call.lhs as ast.SelectorExpr
			method_name = sel.rhs.name
		}
	}
	// Methods that return []string - value type is string
	if method_name in ['split', 'split_any', 'split_nth', 'rsplit_nth', 'split_into_lines', 'fields'] {
		if for_in.key is ast.Ident {
			t.var_types[for_in.key.name] = 'int'
		}
		if for_in.value is ast.Ident {
			t.var_types[for_in.value.name] = 'string'
		} else if for_in.value is ast.ModifierExpr {
			mod_expr := for_in.value as ast.ModifierExpr
			if mod_expr.expr is ast.Ident {
				t.var_types[mod_expr.expr.name] = 'string'
			}
		}
	}
	// Also handle string iteration
	if for_in.expr is ast.SelectorExpr {
		// String methods like .bytes() etc. - key is int
		if for_in.key is ast.Ident {
			t.var_types[for_in.key.name] = 'int'
		}
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
			// Check for &ErrorType{} pattern - this gets transformed to IError{...}
			// and the outer & should be removed since IError is already a value type
			if expr.op == .amp && expr.expr is ast.InitExpr {
				type_name := t.get_init_expr_type_name(expr.expr.typ)
				if t.is_error_type_name(type_name) {
					// Transform &ErrorType{} to IError{...} (without the &)
					return t.transform_expr(expr.expr)
				}
			}
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
		ast.MapInitExpr {
			t.transform_map_init_expr(expr)
		}
		ast.MatchExpr {
			t.transform_match_expr(expr)
		}
		ast.ComptimeExpr {
			t.transform_comptime_expr(expr)
		}
		ast.InitExpr {
			t.transform_init_expr(expr)
		}
		ast.UnsafeExpr {
			// Transform the statements inside unsafe blocks
			ast.Expr(ast.UnsafeExpr{
				stmts: t.transform_stmts(expr.stmts)
			})
		}
		ast.FieldInit {
			// Transform the value inside field initializations (e.g., fn(name: expr))
			ast.Expr(ast.FieldInit{
				name:  expr.name
				value: t.transform_expr(expr.value)
			})
		}
		else {
			expr
		}
	}
}

fn (mut t Transformer) transform_array_init_expr(expr ast.ArrayInitExpr) ast.Expr {
	// Transform value expressions
	mut exprs := []ast.Expr{cap: expr.exprs.len}
	for e in expr.exprs {
		exprs << t.transform_expr(e)
	}

	// Check if this is a fixed-size array
	mut is_fixed := false
	mut elem_type_expr := ast.empty_expr
	// Check for ArrayFixedType or ArrayType (expr.typ is ast.Type sum type)
	if expr.typ is ast.Type {
		if expr.typ is ast.ArrayFixedType {
			is_fixed = true
		} else if expr.typ is ast.ArrayType {
			elem_type_expr = expr.typ.elem_type
		}
	}

	if is_fixed {
		// Fixed-size array: keep as ArrayInitExpr
		return ast.ArrayInitExpr{
			typ:   expr.typ
			exprs: exprs
			init:  t.transform_expr(expr.init)
			cap:   expr.cap
			len:   expr.len
			pos:   expr.pos
		}
	}

	// Dynamic array: transform to builtin__new_array_from_c_array_noscan(len, cap, sizeof(elem), values)
	arr_len := exprs.len

	// Handle empty arrays - keep as ArrayInitExpr with empty elements, cleanc will generate (array){0}
	if arr_len == 0 {
		return ast.ArrayInitExpr{
			typ:   expr.typ
			exprs: []ast.Expr{}
			init:  expr.init
			cap:   expr.cap
			len:   expr.len
			pos:   expr.pos
		}
	}

	// Determine element type name and sizeof argument
	mut elem_type_name := 'int'
	sizeof_arg := if elem_type_expr !is ast.EmptyExpr {
		elem_type_name = t.expr_to_type_name(elem_type_expr)
		elem_type_expr
	} else if exprs.len > 0 {
		// Infer from first element
		first := exprs[0]
		if first is ast.BasicLiteral {
			if first.kind == .number {
				elem_type_name = 'int'
			} else if first.kind == .string {
				elem_type_name = 'string'
			}
			ast.Expr(ast.Ident{
				name: elem_type_name
			})
		} else if first is ast.StringLiteral {
			elem_type_name = 'string'
			ast.Expr(ast.Ident{
				name: 'string'
			})
		} else if first is ast.SelectorExpr {
			// For enum values like .trim_left, use int for sizeof
			// Try to get actual enum type from environment
			if enum_type := t.get_expr_type(first) {
				type_name := t.type_to_c_name(enum_type)
				if type_name != '' {
					elem_type_name = type_name
					ast.Expr(ast.Ident{
						name: type_name
					})
				} else {
					elem_type_name = 'int'
					ast.Expr(ast.Ident{
						name: 'int'
					})
				}
			} else {
				elem_type_name = 'int'
				ast.Expr(ast.Ident{
					name: 'int'
				})
			}
		} else if first is ast.Ident {
			// Try to get type from var_types
			if var_type := t.var_types[first.name] {
				elem_type_name = var_type
				ast.Expr(ast.Ident{
					name: var_type
				})
			} else {
				// Default: use int
				ast.Expr(ast.Ident{
					name: 'int'
				})
			}
		} else {
			// Default: use int
			ast.Expr(ast.Ident{
				name: 'int'
			})
		}
	} else {
		ast.Expr(ast.Ident{
			name: 'int'
		})
	}

	// Create proper array type for the inner ArrayInitExpr
	inner_array_typ := ast.Type(ast.ArrayType{
		elem_type: ast.Ident{
			name: elem_type_name
		}
	})

	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'builtin__new_array_from_c_array_noscan'
		}
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${arr_len}'
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${arr_len}'
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [sizeof_arg]
			}),
			ast.Expr(ast.ArrayInitExpr{
				typ:   inner_array_typ
				exprs: exprs
			}),
		]
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_map_init_expr(expr ast.MapInitExpr) ast.Expr {
	// Empty map - keep as MapInitExpr, cleanc will generate __new_Map_*()
	if expr.keys.len == 0 {
		return expr
	}

	// Transform key and value expressions
	mut keys := []ast.Expr{cap: expr.keys.len}
	mut vals := []ast.Expr{cap: expr.vals.len}
	for k in expr.keys {
		keys << t.transform_expr(k)
	}
	for v in expr.vals {
		vals << t.transform_expr(v)
	}

	// Determine key and value types
	mut key_type_name := 'int'
	mut val_type_name := 'int'
	match expr.typ {
		ast.Type {
			if expr.typ is ast.MapType {
				mt := expr.typ as ast.MapType
				key_type_name = t.expr_to_type_name(mt.key_type)
				val_type_name = t.expr_to_type_name(mt.value_type)
			}
		}
		else {}
	}
	if key_type_name == 'int' && keys.len > 0 {
		// Infer from first key/value
		first_key := keys[0]
		first_val := vals[0]
		if first_key is ast.BasicLiteral {
			if first_key.kind == .string {
				key_type_name = 'string'
			}
		} else if first_key is ast.StringLiteral {
			key_type_name = 'string'
		}
		if first_val is ast.BasicLiteral {
			if first_val.kind == .number {
				val_type_name = 'int'
			} else if first_val.kind == .string {
				val_type_name = 'string'
			}
		} else if first_val is ast.StringLiteral {
			val_type_name = 'string'
		}
	}

	n := keys.len

	// Create array types for keys and values
	key_array_typ := ast.Type(ast.ArrayType{
		elem_type: ast.Ident{
			name: key_type_name
		}
	})
	val_array_typ := ast.Type(ast.ArrayType{
		elem_type: ast.Ident{
			name: val_type_name
		}
	})

	// builtin__new_map_init_noscan_value(hash_fn, eq_fn, clone_fn, free_fn, n, key_size, val_size, keys, vals)
	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'builtin__new_map_init_noscan_value'
		}
		args: [
			// &builtin__map_hash_<key_type>
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: 'builtin__map_hash_${key_type_name}'
				}
			}),
			// &builtin__map_eq_<key_type>
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: 'builtin__map_eq_${key_type_name}'
				}
			}),
			// &builtin__map_clone_<key_type>
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: 'builtin__map_clone_${key_type_name}'
				}
			}),
			// &builtin__map_free_<key_type>
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: 'builtin__map_free_${key_type_name}'
				}
			}),
			// n (number of elements)
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${n}'
			}),
			// sizeof(key_type)
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [ast.Expr(ast.Ident{
					name: key_type_name
				})]
			}),
			// sizeof(val_type)
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [ast.Expr(ast.Ident{
					name: val_type_name
				})]
			}),
			// keys array
			ast.Expr(ast.ArrayInitExpr{
				typ:   key_array_typ
				exprs: keys
			}),
			// vals array
			ast.Expr(ast.ArrayInitExpr{
				typ:   val_array_typ
				exprs: vals
			}),
		]
		pos:  expr.pos
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

fn (mut t Transformer) transform_init_expr(expr ast.InitExpr) ast.Expr {
	// Transform field values recursively
	// Note: ArrayInitExpr is NOT transformed here because cleanc uses field type info
	// to determine if it's a fixed-size array (which transformer doesn't have access to)
	mut fields := []ast.FieldInit{cap: expr.fields.len}
	for field in expr.fields {
		transformed_value := if field.value is ast.ArrayInitExpr {
			// Keep array inits as-is for struct fields - cleanc handles fixed vs dynamic
			ast.Expr(field.value)
		} else {
			t.transform_expr(field.value)
		}
		fields << ast.FieldInit{
			name:  field.name
			value: transformed_value
		}
	}

	// Check if this is an error struct literal that needs IError boxing
	type_name := t.get_init_expr_type_name(expr.typ)
	if t.is_error_type_name(type_name) {
		// Transform to IError struct init with explicit boxing
		// Generate: IError{ ._object = &ErrorType{...}, ._type_id = __type_id_ErrorType,
		//                   .type_name = IError_WrapperType_type_name_wrapper,
		//                   .msg = IError_WrapperType_msg_wrapper,
		//                   .code = IError_WrapperType_code_wrapper }
		c_type_name := t.get_c_type_name(type_name)
		// Determine wrapper type - types that embed Error use Error wrappers,
		// types with custom msg/code methods use their own wrappers
		wrapper_type := t.get_error_wrapper_type(type_name)

		// Create &ErrorType{...} - heap-allocated error object
		inner_init := ast.InitExpr{
			typ:    expr.typ
			fields: fields
		}
		heap_alloc := ast.PrefixExpr{
			op:   .amp
			expr: inner_init
		}

		return ast.InitExpr{
			typ:    ast.Ident{
				name: 'IError'
			}
			fields: [
				ast.FieldInit{
					name:  '_object'
					value: heap_alloc
				},
				ast.FieldInit{
					name:  '_type_id'
					value: ast.Ident{
						name: '__type_id_${c_type_name}'
					}
				},
				ast.FieldInit{
					name:  'type_name'
					value: ast.Ident{
						name: 'IError_${wrapper_type}_type_name_wrapper'
					}
				},
				ast.FieldInit{
					name:  'msg'
					value: ast.Ident{
						name: 'IError_${wrapper_type}_msg_wrapper'
					}
				},
				ast.FieldInit{
					name:  'code'
					value: ast.Ident{
						name: 'IError_${wrapper_type}_code_wrapper'
					}
				},
			]
		}
	}

	return ast.InitExpr{
		typ:    expr.typ
		fields: fields
	}
}

// get_error_wrapper_type returns the wrapper type name for IError interface methods.
// Types that embed Error use 'Error' wrappers; types with custom msg/code use their C type name.
fn (t &Transformer) get_error_wrapper_type(type_name string) string {
	base_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// Types that embed Error and use Error's msg/code methods
	if base_name in ['Eof', 'FileNotOpenedError', 'SizeOfTypeIs0Error', 'ExecutableNotFoundError',
		'Error'] {
		return 'Error'
	}
	// Types with custom msg/code methods use their full C type name (with module prefix)
	// The wrapper name must match what cleanc generates
	if base_name in ['NotExpected', 'MessageError'] {
		return type_name // Use full name with module prefix if present
	}
	// Default to Error wrappers
	return 'Error'
}

// get_c_type_name converts a V type name to C type name format
fn (t &Transformer) get_c_type_name(type_name string) string {
	// Already in C format (module__Type) or plain name
	return type_name
}

// get_init_expr_type_name extracts the type name from an InitExpr's typ field
// Returns the C-style mangled name (module__Type) for proper wrapper resolution
fn (t &Transformer) get_init_expr_type_name(typ ast.Expr) string {
	if typ is ast.Ident {
		// Add module prefix if we're in a non-main module and the type is a known error type
		base_name := typ.name
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
			// Check if this is an error type that should be module-qualified
			if base_name in ['Eof', 'NotExpected', 'MessageError', 'Error', 'FileNotOpenedError',
				'SizeOfTypeIs0Error', 'ExecutableNotFoundError'] {
				return '${t.cur_module}__${base_name}'
			}
		}
		return base_name
	}
	if typ is ast.SelectorExpr {
		// Module-qualified: os.Eof -> os__Eof
		if typ.lhs is ast.Ident {
			return '${typ.lhs.name}__${typ.rhs.name}'
		}
		return typ.rhs.name
	}
	return ''
}

// is_error_type_name checks if a type name is a known error type that implements IError
fn (t &Transformer) is_error_type_name(type_name string) bool {
	// Get base name (strip module prefix if present)
	base_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// Known error types that implement IError
	return base_name in ['Eof', 'NotExpected', 'MessageError', 'Error', 'FileNotOpenedError',
		'SizeOfTypeIs0Error', 'ExecutableNotFoundError']
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
	// Check for 'in' operator with arrays: elem in arr => array__contains_T(arr, elem)
	if expr.op in [.key_in, .not_in] {
		// Get the element type of the array (RHS)
		if elem_type_name := t.get_array_elem_type_str(expr.rhs) {
			// Get enum type from LHS for resolving shorthand in array
			enum_type := t.get_enum_type_name(expr.lhs)
			// If array contains enum shorthand but we can't resolve the type,
			// skip transformation and let cleanc handle it
			mut has_unresolved_shorthand := false
			if enum_type == '' && expr.rhs is ast.ArrayInitExpr {
				arr := expr.rhs as ast.ArrayInitExpr
				if arr.exprs.len > 0 {
					first := arr.exprs[0]
					// Check for enum shorthand (.value with empty LHS)
					if first is ast.SelectorExpr {
						sel := first as ast.SelectorExpr
						// Check for enum shorthand: EmptyExpr or empty Ident as LHS
						is_shorthand := if sel.lhs is ast.EmptyExpr {
							true
						} else if sel.lhs is ast.Ident {
							// Also check for empty ident name
							(sel.lhs as ast.Ident).name == ''
						} else {
							false
						}
						if is_shorthand {
							has_unresolved_shorthand = true
						}
					}
				}
			}
			if !has_unresolved_shorthand {
				// Transform array with enum context if needed
				transformed_rhs := if enum_type != '' && expr.rhs is ast.ArrayInitExpr {
					t.transform_array_with_enum_context(expr.rhs as ast.ArrayInitExpr,
						enum_type)
				} else {
					t.transform_expr(expr.rhs)
				}
				contains_call := ast.CallExpr{
					lhs:  ast.Ident{
						name: 'array__contains_${elem_type_name}'
					}
					args: [transformed_rhs, t.transform_expr(expr.lhs)]
					pos:  expr.pos
				}
				if expr.op == .not_in {
					// !in => !Array_T_contains(arr, elem)
					return ast.PrefixExpr{
						op:   .not
						expr: contains_call
						pos:  expr.pos
					}
				}
				return contains_call
			}
		}
	}
	// Check for array append: arr << elem => builtin__array_push_noscan((array*)&arr, _MOV((T[]){ elem }))
	// If RHS is also an array, use push_many instead
	if expr.op == .left_shift {
		if elem_type_name := t.get_array_elem_type_str(expr.lhs) {
			// Check if RHS is also an array (arr << other_arr => push_many)
			rhs_is_array := t.get_array_elem_type_str(expr.rhs) != none

			// Check if LHS is already a pointer (e.g., mut receiver of type strings.Builder*)
			lhs_is_ptr := t.is_pointer_type_expr(expr.lhs)

			// Create (array*)&arr or (array*)arr expression depending on whether LHS is already a pointer
			arr_ptr_expr := if lhs_is_ptr {
				// Already a pointer, just cast
				ast.Expr(ast.CastExpr{
					typ:  ast.Ident{
						name: 'array*'
					}
					expr: t.transform_expr(expr.lhs)
				})
			} else {
				// Take address then cast
				ast.Expr(ast.CastExpr{
					typ:  ast.Ident{
						name: 'array*'
					}
					expr: ast.PrefixExpr{
						op:   .amp
						expr: t.transform_expr(expr.lhs)
						pos:  expr.pos
					}
				})
			}

			if rhs_is_array {
				// RHS is an array - use array__push_many(array*, val.data, val.len)
				rhs_transformed := t.transform_expr(expr.rhs)
				// Wrap PrefixExpr in parens to fix operator precedence (*other.data -> (*other).data)
				rhs_for_selector := if expr.rhs is ast.PrefixExpr {
					ast.Expr(ast.ParenExpr{
						expr: rhs_transformed
					})
				} else {
					rhs_transformed
				}
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'array__push_many'
					}
					args: [
						arr_ptr_expr,
						ast.SelectorExpr{
							lhs: rhs_for_selector
							rhs: ast.Ident{
								name: 'data'
							}
						},
						ast.SelectorExpr{
							lhs: rhs_for_selector
							rhs: ast.Ident{
								name: 'len'
							}
						},
					]
					pos:  expr.pos
				}
			}

			// Create (T[]){ elem } expression for single element push
			// Note: cleanc will add _MOV wrapper when generating ArrayInitExpr
			arr_literal := ast.ArrayInitExpr{
				typ:   ast.Type(ast.ArrayType{
					elem_type: ast.Ident{
						name: elem_type_name
					}
				})
				exprs: [t.transform_expr(expr.rhs)]
			}
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: 'builtin__array_push_noscan'
				}
				args: [
					arr_ptr_expr,
					ast.Expr(arr_literal),
				]
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
		// Check for array comparisons: arr1 == arr2 or arr1 != arr2
		lhs_arr_type := t.infer_array_type(expr.lhs)
		rhs_arr_type := t.infer_array_type(expr.rhs)
		if lhs_arr_type != none && rhs_arr_type != none {
			// Transform array comparisons to function calls
			eq_call := ast.CallExpr{
				lhs:  ast.Ident{
					name: 'array__eq'
				}
				args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
				pos:  expr.pos
			}
			if expr.op == .ne {
				// arr1 != arr2 -> !array__eq(arr1, arr2)
				return ast.PrefixExpr{
					op:   .not
					expr: eq_call
					pos:  expr.pos
				}
			}
			// arr1 == arr2 -> array__eq(arr1, arr2)
			return eq_call
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
		// Strip pointer suffix for struct lookup
		clean_type := if param_type.ends_with('*') {
			param_type[..param_type.len - 1]
		} else {
			param_type
		}
		return t.resolve_struct_field_type(clean_type, field_name)
	}

	// Check var_types (includes local variables)
	if var_type := t.var_types[var_name] {
		// Strip pointer suffix for struct lookup
		clean_type := if var_type.ends_with('*') { var_type[..var_type.len - 1] } else { var_type }
		return t.resolve_struct_field_type(clean_type, field_name)
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
			// Try the struct name directly
			if obj := scope.objects[struct_name] {
				if obj is types.Type {
					return t.get_field_type_name(obj, field_name)
				}
			}
			// Try with current module prefix
			if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
				mangled := '${t.cur_module}__${struct_name}'
				if obj := scope.objects[mangled] {
					if obj is types.Type {
						return t.get_field_type_name(obj, field_name)
					}
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
	if typ is types.String {
		return 'string'
	}
	if typ is types.Map {
		// Convert Map type to 'Map_K_V' format
		key_type := t.type_to_name(typ.key_type)
		value_type := t.type_to_name(typ.value_type)
		if key_type != '' && value_type != '' {
			return 'Map_${key_type}_${value_type}'
		}
	}
	if typ is types.Primitive {
		return t.type_to_c_name(typ)
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

// get_module_scope returns the scope for a specific module
fn (t &Transformer) get_module_scope(module_name string) ?&types.Scope {
	return lock t.env.scopes {
		t.env.scopes[module_name] or { return none }
	}
}

// get_expr_type returns the types.Type for an expression by looking it up in the environment
fn (t &Transformer) get_expr_type(expr ast.Expr) ?types.Type {
	// Handle literal types by looking up their type in the scope
	if expr is ast.StringLiteral || expr is ast.StringInterLiteral {
		if mut scope := t.get_current_scope() {
			if obj := scope.lookup_parent('string', 0) {
				return obj.typ()
			}
		}
	}
	if expr is ast.BasicLiteral {
		if expr.kind == .string {
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent('string', 0) {
					return obj.typ()
				}
			}
		}
	}
	if expr is ast.Ident {
		// First check var_types and param_types (local variables/parameters)
		type_name := t.var_types[expr.name] or { t.param_types[expr.name] or { '' } }
		if type_name != '' {
			// Look up the type from the scope
			clean_name := type_name.trim_right('*')
			is_ptr := type_name.ends_with('*')
			mut scope := t.get_current_scope() or { return none }
			obj := scope.lookup_parent(clean_name, 0) or { return none }
			typ := obj.typ()
			if is_ptr {
				return types.Pointer{
					base_type: typ
				}
			}
			return typ
		}
		// Fall back to scope lookup
		mut scope := t.get_current_scope() or { return none }
		obj := scope.lookup_parent(expr.name, 0) or { return none }
		return obj.typ()
	}
	if expr is ast.SelectorExpr {
		// Check for module-qualified variables (e.g., os.args)
		if expr.lhs is ast.Ident {
			mod_name := (expr.lhs as ast.Ident).name
			var_name := expr.rhs.name
			// Try to look up the variable in the module's scope
			if mut mod_scope := t.get_module_scope(mod_name) {
				if obj := mod_scope.lookup_parent(var_name, 0) {
					return obj.typ()
				}
			}
		}
		// For field access, get the type of the LHS and look up the field
		lhs_type := t.get_expr_type(expr.lhs) or { return none }
		base_type := if lhs_type is types.Pointer {
			lhs_type.base_type
		} else {
			lhs_type
		}
		if base_type is types.Struct {
			for field in base_type.fields {
				if field.name == expr.rhs.name {
					return field.typ
				}
			}
		}
	}
	if expr is ast.IndexExpr {
		// For slicing (a[i..j]), the result type is the same as the source
		if expr.expr is ast.RangeExpr {
			return t.get_expr_type(expr.lhs)
		}
		// For array indexing (a[i]), get the element type
		lhs_type := t.get_expr_type(expr.lhs) or { return none }
		if lhs_type is types.Array {
			return lhs_type.elem_type
		}
		if lhs_type is types.ArrayFixed {
			return lhs_type.elem_type
		}
	}
	return none
}

// get_type_name returns the type name suitable for method lookup
fn (t &Transformer) get_type_name(typ types.Type) string {
	if typ is types.Pointer {
		return t.get_type_name(typ.base_type)
	}
	if typ is types.Struct {
		return typ.name
	}
	if typ is types.String {
		return 'string'
	}
	if typ is types.Array {
		return 'array'
	}
	if typ is types.Map {
		return 'map'
	}
	if typ is types.Primitive {
		// Get primitive type name (int, u8, etc)
		if typ.props.has(.boolean) {
			return 'bool'
		} else if typ.props.has(.integer) {
			if typ.props.has(.unsigned) {
				return 'u${typ.size}'
			} else {
				return if typ.size == 0 { 'int' } else { 'i${typ.size}' }
			}
		} else if typ.props.has(.float) {
			return 'f${typ.size}'
		}
	}
	return ''
}

// get_array_elem_type_str returns the element type name of an array variable
fn (t &Transformer) get_array_elem_type_str(expr ast.Expr) ?string {
	if expr is ast.Ident {
		// Check var_types for tracked variable type
		if var_type := t.var_types[expr.name] {
			// Strip pointer suffix for checking
			clean_type := var_type.trim_right('*')
			if clean_type.starts_with('Array_') {
				return clean_type['Array_'.len..]
			}
			// Handle strings.Builder which is an alias for []u8
			if clean_type.starts_with('strings__Builder') || clean_type == 'Builder' {
				return 'u8'
			}
		}
	}
	// Handle PrefixExpr (e.g., *ptr where ptr is pointer to array)
	if expr is ast.PrefixExpr && expr.op == .mul {
		// Dereference: check the inner expression's type
		if elem := t.get_array_elem_type_str(expr.expr) {
			return elem
		}
	}
	// Handle ArrayInitExpr directly (for inline array literals like [1, 2, 3])
	if expr is ast.ArrayInitExpr {
		arr_type := t.infer_array_type(expr) or { return none }
		if arr_type.starts_with('Array_') {
			return arr_type['Array_'.len..]
		}
	}
	// Handle CallExpr - check function return type
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		ret_type := t.get_call_return_type(expr)
		if ret_type.starts_with('Array_') {
			return ret_type['Array_'.len..]
		}
	}
	// Handle SelectorExpr method calls like expr.bytes()
	if expr is ast.SelectorExpr {
		// Check if this is a method call that returns an array
		method_name := expr.rhs.name
		if method_name in ['bytes', 'runes', 'split', 'split_any', 'fields', 'keys', 'values'] {
			// Common array-returning methods
			return t.infer_method_array_elem_type(expr)
		}
		// Check if this is a struct field access to an array field
		if field_type := t.get_struct_field_type(expr) {
			if field_type is types.Array {
				return t.type_to_c_name(field_type.elem_type)
			}
		}
	}
	// Also try getting from types.Environment
	typ := t.get_expr_type(expr) or { return none }
	// Unwrap pointer types (e.g., strings__Builder* -> strings__Builder)
	base_typ := if typ is types.Pointer { typ.base_type } else { typ }
	if base_typ is types.Array {
		return t.type_to_c_name(base_typ.elem_type)
	}
	// Check for type aliases like strings.Builder -> []u8
	if base_typ is types.Struct {
		if base_typ.name == 'strings__Builder' || base_typ.name == 'Builder' {
			return 'u8'
		}
	}
	return none
}

// get_call_return_type returns the return type of a function call
fn (t &Transformer) get_call_return_type(expr ast.Expr) string {
	mut fn_name := ''
	mut is_method := false
	mut selector_expr := ast.SelectorExpr{}
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			// Method call or module.function call
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			is_method = true
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			is_method = true
		}
	}
	if fn_name != '' {
		// Check tracked function return types first
		if ret_type := t.fn_return_types[fn_name] {
			return ret_type
		}
		// For method calls, check for known array-returning methods
		if is_method {
			if elem_type := t.infer_method_array_elem_type(selector_expr) {
				return 'Array_${elem_type}'
			}
		}
	}
	return ''
}

// infer_method_array_elem_type infers the element type for array-returning methods
fn (t &Transformer) infer_method_array_elem_type(expr ast.SelectorExpr) ?string {
	method_name := expr.rhs.name
	match method_name {
		'bytes' {
			return 'u8'
		}
		'runes' {
			return 'rune'
		}
		'split', 'split_any', 'fields' {
			return 'string'
		}
		'keys' {
			// For map.keys(), need to infer key type from map
			return none
		}
		'values' {
			// For map.values(), need to infer value type from map
			return none
		}
		else {
			return none
		}
	}
}

// get_struct_field_type returns the type of a struct field from a SelectorExpr
fn (t &Transformer) get_struct_field_type(expr ast.SelectorExpr) ?types.Type {
	// Try to get the struct type from var_types first (for local variables and receivers)
	mut struct_type_name := ''
	if expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		if lhs_name in t.var_types {
			struct_type_name = t.var_types[lhs_name].trim_right('*')
		} else if lhs_name in t.param_types {
			struct_type_name = t.param_types[lhs_name].trim_right('*')
		}
	}

	// If we have a type name, look it up in the environment
	if struct_type_name != '' {
		mut scope := t.get_current_scope() or { return none }
		obj := scope.lookup_parent(struct_type_name, 0) or { return none }
		struct_type := obj.typ()
		base_type := if struct_type is types.Pointer {
			struct_type.base_type
		} else {
			struct_type
		}
		match base_type {
			types.Struct {
				for field in base_type.fields {
					if field.name == expr.rhs.name {
						return field.typ
					}
				}
			}
			else {}
		}
	}

	// Fall back to get_expr_type for module-level lookups
	struct_type := t.get_expr_type(expr.lhs) or { return none }

	// If it's a pointer, dereference to get the struct
	base_type := if struct_type is types.Pointer {
		struct_type.base_type
	} else {
		struct_type
	}

	// Look up the field in the struct
	match base_type {
		types.Struct {
			for field in base_type.fields {
				if field.name == expr.rhs.name {
					return field.typ
				}
			}
		}
		else {}
	}
	return none
}

// infer_array_type returns the Array_T type string for an array expression
fn (t &Transformer) infer_array_type(expr ast.Expr) ?string {
	// Check for variable references with known array types
	if expr is ast.Ident {
		if var_type := t.var_types[expr.name] {
			if var_type.starts_with('Array_') {
				return var_type
			}
		}
	}
	// Check for slice expressions (arr[a..b] or arr#[a..b]) - returns same array type
	if expr is ast.IndexExpr {
		if expr.expr is ast.RangeExpr {
			// Slicing an array returns the same array type
			return t.infer_array_type(expr.lhs)
		}
	}
	// Check for function calls that return array types
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		ret_type := t.get_call_return_type(expr)
		if ret_type.starts_with('Array_') {
			return ret_type
		}
	}
	// Check for method calls like .bytes(), .split()
	if expr is ast.SelectorExpr {
		// Check if this is part of a call (method call)
		if elem_type := t.infer_method_array_elem_type(expr) {
			return 'Array_${elem_type}'
		}
		// Check for field access on modules/structs (e.g., os.args)
		if field_type := t.get_expr_type(expr) {
			if field_type is types.Array {
				elem_type_name := t.get_type_name(field_type.elem_type)
				if elem_type_name != '' {
					return 'Array_${elem_type_name}'
				}
			}
		}
	}
	// Handle InitExpr with ArrayType (e.g., []voidptr{})
	if expr is ast.InitExpr {
		match expr.typ {
			ast.Type {
				if expr.typ is ast.ArrayType {
					elem_type := t.expr_to_type_name(expr.typ.elem_type)
					if elem_type != '' {
						return 'Array_${elem_type}'
					}
				}
			}
			else {}
		}
	}
	if expr is ast.ArrayInitExpr {
		// Check if array has explicit type
		if expr.typ is ast.Type {
			if expr.typ is ast.ArrayType {
				elem_type := t.expr_to_type_name(expr.typ.elem_type)
				if elem_type != '' {
					return 'Array_${elem_type}'
				}
			}
		}
		// Infer from first element
		if expr.exprs.len > 0 {
			first := expr.exprs[0]
			if first is ast.BasicLiteral {
				if first.kind == .number {
					return 'Array_int'
				}
				if first.kind == .string {
					return 'Array_string'
				}
			}
			if first is ast.StringLiteral {
				return 'Array_string'
			}
			// Check for enum values (SelectorExpr like .trim_left)
			if first is ast.SelectorExpr {
				// Try to get type from environment
				if elem_type := t.get_expr_type(first) {
					type_name := t.type_to_c_name(elem_type)
					if type_name != '' {
						return 'Array_${type_name}'
					}
				}
				// Default to int for enum values
				return 'Array_int'
			}
			// Check for idents (could be enum values or variables)
			if first is ast.Ident {
				// First check var_types for local variables
				if var_type := t.var_types[first.name] {
					return 'Array_${var_type}'
				}
				// Then try environment for constants/globals
				if elem_type := t.get_expr_type(first) {
					type_name := t.type_to_c_name(elem_type)
					if type_name != '' {
						return 'Array_${type_name}'
					}
				}
			}
			// Check for IfExpr (ternary) - infer from branches
			if first is ast.IfExpr {
				if t.is_string_expr(first) {
					return 'Array_string'
				}
			}
		}
	}
	return none
}

// infer_map_type returns the Map_K_V type string for a map expression
fn (t &Transformer) infer_map_type(expr ast.Expr) ?string {
	// Handle InitExpr with MapType (e.g., map[int]int{})
	if expr is ast.InitExpr {
		match expr.typ {
			ast.Type {
				if expr.typ is ast.MapType {
					mt := expr.typ as ast.MapType
					key_type := t.expr_to_type_name(mt.key_type)
					value_type := t.expr_to_type_name(mt.value_type)
					if key_type != '' && value_type != '' {
						return 'Map_${key_type}_${value_type}'
					}
				}
			}
			else {}
		}
	}
	// Handle MapInitExpr (for literal maps like {'a': 1, 'b': 2})
	if expr is ast.MapInitExpr {
		// Check if map has explicit type
		match expr.typ {
			ast.Type {
				if expr.typ is ast.MapType {
					mt := expr.typ as ast.MapType
					key_type := t.expr_to_type_name(mt.key_type)
					value_type := t.expr_to_type_name(mt.value_type)
					if key_type != '' && value_type != '' {
						return 'Map_${key_type}_${value_type}'
					}
				}
			}
			else {}
		}
		// Infer from first key/value (for literal maps like {'a': 1})
		if expr.keys.len > 0 {
			mut key_type := 'int'
			mut val_type := 'int'
			first_key := expr.keys[0]
			first_val := expr.vals[0]
			if first_key is ast.StringLiteral
				|| (first_key is ast.BasicLiteral && first_key.kind == .string) {
				key_type = 'string'
			}
			if first_val is ast.StringLiteral
				|| (first_val is ast.BasicLiteral && first_val.kind == .string) {
				val_type = 'string'
			}
			return 'Map_${key_type}_${val_type}'
		}
	}
	return none
}

// infer_struct_type returns the struct type name for cast expressions like &mapnode(unsafe { nil })
fn (t &Transformer) infer_struct_type(expr ast.Expr) ?string {
	// Handle CastExpr like &mapnode(unsafe { nil }) or mapnode(...)
	if expr is ast.CastExpr {
		// Check if the type is a pointer type like &mapnode
		if expr.typ is ast.PrefixExpr {
			prefix := expr.typ as ast.PrefixExpr
			if prefix.op == .amp && prefix.expr is ast.Ident {
				return (prefix.expr as ast.Ident).name + '*'
			}
		}
		// Check if it's a simple type cast like mapnode(...)
		if expr.typ is ast.Ident {
			return (expr.typ as ast.Ident).name
		}
	}
	// Handle CallOrCastExpr which can also be a cast like &mapnode(ptr)
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.PrefixExpr {
			prefix := expr.lhs as ast.PrefixExpr
			if prefix.op == .amp && prefix.expr is ast.Ident {
				return (prefix.expr as ast.Ident).name + '*'
			}
		}
		if expr.lhs is ast.Ident {
			// Check if it's a known struct type in the scope
			name := (expr.lhs as ast.Ident).name
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(name, 0) {
					typ := obj.typ()
					if typ is types.Struct {
						return name
					}
				}
			}
		}
	}
	// Handle UnsafeExpr like unsafe { &mapnode(nil) }
	if expr is ast.UnsafeExpr {
		if expr.stmts.len > 0 {
			if last_stmt := expr.stmts[expr.stmts.len - 1] {
				if last_stmt is ast.ExprStmt {
					return t.infer_struct_type(last_stmt.expr)
				}
			}
		}
	}
	// Handle InitExpr for struct initialization like mapnode{...}
	if expr is ast.InitExpr {
		if expr.typ is ast.Ident {
			return (expr.typ as ast.Ident).name
		}
	}
	// Handle PrefixExpr like &struct_init or &mapnode(unsafe { nil })
	if expr is ast.PrefixExpr && expr.op == .amp {
		if inner_type := t.infer_struct_type(expr.expr) {
			return inner_type + '*'
		}
		// Also check for ident that is a known struct variable
		if expr.expr is ast.Ident {
			name := (expr.expr as ast.Ident).name
			if type_str := t.var_types[name] {
				return type_str + '*'
			}
		}
	}
	// Handle SelectorExpr like m.root where root is a struct pointer field
	if expr is ast.SelectorExpr {
		// Use get_expr_type to get the type of the field
		if field_type := t.get_expr_type(expr) {
			if field_type is types.Pointer {
				if field_type.base_type is types.Struct {
					struct_name := (field_type.base_type as types.Struct).name
					return struct_name + '*'
				}
			}
			if field_type is types.Struct {
				return (field_type as types.Struct).name
			}
		}
	}
	return none
}

// get_map_type_for_expr returns the Map_K_V type string for an expression if it's a map
fn (t &Transformer) get_map_type_for_expr(expr ast.Expr) ?string {
	if expr is ast.Ident {
		// Check var_types for tracked variable type
		if var_type := t.var_types[expr.name] {
			if var_type.starts_with('Map_') {
				return var_type
			}
		}
	}
	// Handle SelectorExpr (struct field access like g.stack_map or l->sym_to_got)
	if expr is ast.SelectorExpr {
		sel := expr as ast.SelectorExpr
		// Try to resolve the field type using the existing infrastructure
		if sel.lhs is ast.Ident {
			field_type := t.resolve_field_type(sel.lhs.name, sel.rhs.name)
			if field_type.starts_with('Map_') {
				return field_type
			}
		}
		// Handle nested selectors (a.b.map_field) by resolving the inner type first
		if sel.lhs is ast.SelectorExpr {
			inner_sel := sel.lhs as ast.SelectorExpr
			inner_type := t.get_selector_type_name(inner_sel)
			if inner_type != '' {
				field_type := t.resolve_struct_field_type(inner_type, sel.rhs.name)
				if field_type.starts_with('Map_') {
					return field_type
				}
			}
		}
	}
	return none
}

// get_selector_type_name returns the type name string for a SelectorExpr
fn (t &Transformer) get_selector_type_name(expr ast.SelectorExpr) string {
	if expr.lhs is ast.Ident {
		return t.resolve_field_type(expr.lhs.name, expr.rhs.name)
	}
	return ''
}

// type_to_c_name converts a types.Type to its C type name string
fn (t &Transformer) type_to_c_name(typ types.Type) string {
	match typ {
		types.Primitive {
			// Map V primitive types to C type names
			if typ.props.has(.boolean) {
				return 'bool'
			}
			if typ.props.has(.unsigned) {
				match typ.size {
					1 { return 'u8' }
					2 { return 'u16' }
					4 { return 'u32' }
					8 { return 'u64' }
					else { return 'int' }
				}
			}
			match typ.size {
				1 { return 'i8' }
				2 { return 'i16' }
				4 { return 'int' }
				8 { return 'i64' }
				else { return 'int' }
			}
		}
		types.String {
			return 'string'
		}
		types.Struct {
			return typ.name
		}
		types.Enum {
			return typ.name
		}
		types.Alias {
			return typ.name
		}
		types.NamedType {
			return string(typ)
		}
		types.Array {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_${elem_name}'
		}
		types.Pointer {
			return t.type_to_c_name(typ.base_type) + '*'
		}
		else {
			return 'int'
		}
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

// get_enum_type_name returns the enum type name for an expression, or empty string if not an enum
fn (t &Transformer) get_enum_type_name(expr ast.Expr) string {
	// First check var_types (which includes function parameters)
	if expr is ast.Ident {
		if type_name := t.var_types[expr.name] {
			if type_name != '' && type_name != 'int' && type_name != 'string' && type_name != 'bool' {
				return type_name
			}
		}
	}
	// Handle SelectorExpr - field access like p.status or p->status
	if expr is ast.SelectorExpr {
		// Try to get the field type by looking up the base type and field
		base_type := t.get_enum_type_name(expr.lhs)
		if base_type != '' {
			// If base has a type, try to resolve field type
			field_type := t.resolve_field_type(base_type, expr.rhs.name)
			if field_type != '' && field_type != 'int' && field_type != 'string'
				&& field_type != 'bool' {
				return field_type
			}
		}
		// Also check var_types for lhs.rhs pattern if lhs is ident
		if expr.lhs is ast.Ident {
			lhs_ident := expr.lhs as ast.Ident
			if lhs_type := t.var_types[lhs_ident.name] {
				field_type := t.resolve_field_type(lhs_type, expr.rhs.name)
				if field_type != '' && field_type != 'int' && field_type != 'string'
					&& field_type != 'bool' {
					return field_type
				}
			}
		}
	}
	// Try types environment as fallback
	if typ := t.get_expr_type(expr) {
		type_name := t.type_to_c_name(typ)
		if type_name != '' && type_name != 'int' {
			return type_name
		}
	}
	return ''
}

// transform_array_with_enum_context transforms an array init, resolving enum shorthand using the given enum type
fn (mut t Transformer) transform_array_with_enum_context(arr ast.ArrayInitExpr, enum_type string) ast.Expr {
	mut exprs := []ast.Expr{cap: arr.exprs.len}
	for e in arr.exprs {
		// Resolve enum shorthand before transforming
		resolved := t.resolve_enum_shorthand(e, enum_type)
		exprs << t.transform_expr(resolved)
	}
	// Now create the transformed array init
	return t.transform_array_init_with_exprs(arr, exprs)
}

// transform_array_init_with_exprs transforms an array init using already-transformed expressions
fn (mut t Transformer) transform_array_init_with_exprs(arr ast.ArrayInitExpr, exprs []ast.Expr) ast.Expr {
	// Check if this is a fixed-size array
	mut is_fixed := false
	mut elem_type_expr := ast.empty_expr
	match arr.typ {
		ast.Type {
			if arr.typ is ast.ArrayFixedType {
				is_fixed = true
			} else if arr.typ is ast.ArrayType {
				elem_type_expr = arr.typ.elem_type
			}
		}
		else {}
	}

	if is_fixed {
		return ast.ArrayInitExpr{
			typ:   arr.typ
			exprs: exprs
			init:  t.transform_expr(arr.init)
			cap:   arr.cap
			len:   arr.len
			pos:   arr.pos
		}
	}

	// Dynamic array: transform to builtin__new_array_from_c_array_noscan
	arr_len := exprs.len
	mut elem_type_name := 'int'
	sizeof_arg := if elem_type_expr !is ast.EmptyExpr {
		elem_type_name = t.expr_to_type_name(elem_type_expr)
		elem_type_expr
	} else if exprs.len > 0 {
		first := exprs[0]
		if first is ast.BasicLiteral {
			if first.kind == .number {
				elem_type_name = 'int'
			} else if first.kind == .string {
				elem_type_name = 'string'
			}
			ast.Expr(ast.Ident{
				name: elem_type_name
			})
		} else if first is ast.StringLiteral {
			elem_type_name = 'string'
			ast.Expr(ast.Ident{
				name: 'string'
			})
		} else if first is ast.SelectorExpr {
			// For qualified enum values, use int for sizeof
			elem_type_name = 'int'
			ast.Expr(ast.Ident{
				name: 'int'
			})
		} else {
			exprs[0]
		}
	} else {
		ast.Expr(ast.Ident{
			name: 'int'
		})
	}

	// Create proper array type for the inner ArrayInitExpr
	inner_array_typ := ast.Type(ast.ArrayType{
		elem_type: ast.Ident{
			name: elem_type_name
		}
	})

	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'builtin__new_array_from_c_array_noscan'
		}
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${arr_len}'
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${arr_len}'
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [sizeof_arg]
			}),
			ast.Expr(ast.ArrayInitExpr{
				typ:   inner_array_typ
				exprs: exprs
			}),
		]
		pos:  arr.pos
	}
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
	if expr is ast.BasicLiteral {
		return expr.kind == .string
	}
	if expr is ast.ComptimeExpr {
		// Compile-time expressions like @FN, @FILE, @MOD evaluate to strings
		if expr.expr is ast.Ident {
			name := (expr.expr as ast.Ident).name
			return name in ['FN', 'FILE', 'MOD', 'STRUCT', 'METHOD', 'LOCATION', 'FUNCTION']
		}
	}
	if expr is ast.CastExpr {
		// Check if casting to string type
		if expr.typ is ast.Ident {
			return expr.typ.name == 'string'
		}
	}
	if expr is ast.Ident {
		// Check for comptime string identifiers like @FN, @FILE, @MOD
		if expr.name.starts_with('@') {
			return expr.name in ['@FN', '@FILE', '@MOD', '@STRUCT', '@METHOD', '@LOCATION',
				'@FUNCTION']
		}
		// Fallback: Check tracked variable types (for loop variables, etc. that may not be in scope)
		// This is checked FIRST because local variable tracking is more reliable
		if t.var_types[expr.name] == 'string' {
			return true
		}
		// Fallback: Check tracked parameter types
		if t.param_types[expr.name] == 'string' {
			return true
		}
		// Use type environment to look up the identifier's type
		if mut scope := t.get_current_scope() {
			if obj := scope.lookup_parent(expr.name, 0) {
				typ := obj.typ()
				if typ is types.String {
					return true
				}
				// Also check for struct named 'string' (V's string type)
				if typ is types.Struct && typ.name == 'string' {
					return true
				}
			}
		}
	}
	if expr is ast.ParenExpr {
		return t.is_string_expr(expr.expr)
	}
	if expr is ast.SelectorExpr {
		// Check for module-qualified constants (e.g., os.path_separator)
		if expr.lhs is ast.Ident {
			mod_name := (expr.lhs as ast.Ident).name
			const_name := expr.rhs.name
			// Try to look up the constant in the module's scope
			if mut mod_scope := t.get_module_scope(mod_name) {
				if obj := mod_scope.lookup_parent(const_name, 0) {
					typ := obj.typ()
					if typ is types.String {
						return true
					}
					if typ is types.Struct && typ.name == 'string' {
						return true
					}
				}
			}
		}
		// Try to look up the type of the field using the environment
		if lhs_type := t.get_expr_type(expr.lhs) {
			base_type := if lhs_type is types.Pointer {
				lhs_type.base_type
			} else {
				lhs_type
			}
			if base_type is types.Struct {
				for field in base_type.fields {
					if field.name == expr.rhs.name {
						if field.typ is types.String {
							return true
						}
						if field.typ is types.Struct && field.typ.name == 'string' {
							return true
						}
					}
				}
			}
		}
		// Fallback: Check field names that are typically strings
		// Only use this for common string field names
		if expr.rhs.name in ['name', 'str', 'msg'] {
			return true
		}
	}
	if expr is ast.UnsafeExpr {
		// Check the last statement's expression inside unsafe blocks
		// e.g., unsafe { s.substr_unsafe(i, j) }
		if expr.stmts.len > 0 {
			last_stmt := expr.stmts[expr.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				return t.is_string_expr(last_stmt.expr)
			}
		}
	}
	if expr is ast.IndexExpr {
		// String slicing: s[a..b] returns string if s is string
		if expr.expr is ast.RangeExpr {
			return t.is_string_expr(expr.lhs)
		}
		// Array indexing: arr[i] where arr is []string returns string
		// Use var_types to check if the array is a string array
		if expr.lhs is ast.Ident {
			arr_name := (expr.lhs as ast.Ident).name
			if arr_type := t.var_types[arr_name] {
				if arr_type == 'Array_string' {
					return true
				}
			}
		}
		// Use get_expr_type which handles IndexExpr and returns the element type
		if elem_type := t.get_expr_type(expr) {
			if elem_type is types.String {
				return true
			}
			if elem_type is types.Struct && elem_type.name == 'string' {
				return true
			}
		}
	}
	if expr is ast.InfixExpr {
		// For + on strings, result is string
		if expr.op == .plus && t.is_string_expr(expr.lhs) {
			return true
		}
	}
	if expr is ast.CallExpr {
		// Check method calls that return string using types.Environment
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			method_name := sel.rhs.name
			// First check for module-qualified function calls (e.g., os.user_os())
			// If LHS is an Ident, it could be a module name
			if sel.lhs is ast.Ident {
				mod_name := (sel.lhs as ast.Ident).name
				// Try looking up as a module-qualified function
				if fn_type := t.env.lookup_fn(mod_name, method_name) {
					if return_type := fn_type.get_return_type() {
						if return_type is types.String {
							return true
						}
						if return_type is types.Struct && return_type.name == 'string' {
							return true
						}
					}
				}
			}
			// Try method lookup
			if receiver_type := t.get_expr_type(sel.lhs) {
				type_name := t.get_type_name(receiver_type)
				if fn_type := t.env.lookup_method(type_name, method_name) {
					if return_type := fn_type.get_return_type() {
						if return_type is types.String {
							return true
						}
						if return_type is types.Struct && return_type.name == 'string' {
							return true
						}
					}
				}
			}
			// Check for array methods that return element type (pop, first, last)
			// If receiver is []string, these methods return string
			if method_name in ['pop', 'first', 'last'] {
				if sel.lhs is ast.Ident {
					receiver_name := (sel.lhs as ast.Ident).name
					if receiver_type := t.var_types[receiver_name] {
						if receiver_type == 'Array_string' {
							return true
						}
					}
				}
			}
			// Fallback: check known string-returning methods
			if t.is_string_returning_method(method_name) {
				return true
			}
			// Also check if receiver is string and method typically returns string
			if t.is_string_expr(sel.lhs) && method_name in ['clone', 'str', 'string'] {
				return true
			}
		}
		// Check function return type using environment
		if expr.lhs is ast.Ident {
			fn_name := expr.lhs.name
			// Check for already-transformed string functions
			if fn_name.starts_with('string__') {
				// string__ prefix functions return string (string__plus, string__repeat, etc.)
				return true
			}
			// Try to find the function in the current module's scope
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(fn_name, 0) {
					typ := obj.typ()
					if typ is types.FnType {
						if return_type := typ.get_return_type() {
							if return_type is types.String {
								return true
							}
							if return_type is types.Struct && return_type.name == 'string' {
								return true
							}
						}
					}
				}
			}
			// Fallback: check if function name is known to return string
			if t.is_string_returning_method(fn_name) {
				return true
			}
			// Handle cross-module function calls like os__user_os()
			// Parse module prefix (e.g., os__user_os -> module: os, fn: user_os)
			if fn_name.contains('__') {
				parts := fn_name.split('__')
				if parts.len >= 2 {
					mod_name := parts[0]
					actual_fn := parts[1..].join('__')
					// Use environment's lookup_fn which checks the module's scope
					if fn_type := t.env.lookup_fn(mod_name, actual_fn) {
						if return_type := fn_type.get_return_type() {
							if return_type is types.String {
								return true
							}
							if return_type is types.Struct && return_type.name == 'string' {
								return true
							}
						}
					}
					// Fallback: check if function name is known to return string
					if t.is_string_returning_method(actual_fn) {
						return true
					}
				}
			}
		}
	}
	if expr is ast.CallOrCastExpr {
		// Check method calls for CallOrCastExpr (single-arg method calls like 'foo'.repeat(5))
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			method_name := sel.rhs.name
			// First check for module-qualified function calls (e.g., os.user_os())
			if sel.lhs is ast.Ident {
				mod_name := (sel.lhs as ast.Ident).name
				if fn_type := t.env.lookup_fn(mod_name, method_name) {
					if return_type := fn_type.get_return_type() {
						if return_type is types.String {
							return true
						}
						if return_type is types.Struct && return_type.name == 'string' {
							return true
						}
					}
				}
			}
			// Try method lookup
			if receiver_type := t.get_expr_type(sel.lhs) {
				type_name := t.get_type_name(receiver_type)
				if fn_type := t.env.lookup_method(type_name, method_name) {
					if return_type := fn_type.get_return_type() {
						if return_type is types.String {
							return true
						}
						if return_type is types.Struct && return_type.name == 'string' {
							return true
						}
					}
				}
			}
			// Fallback: check known string-returning methods
			if t.is_string_returning_method(method_name) {
				return true
			}
			// Also check if receiver is string and method typically returns string
			if t.is_string_expr(sel.lhs) && method_name in ['clone', 'str', 'string'] {
				return true
			}
		}
		// Check function return type for CallOrCastExpr (single-arg calls)
		if expr.lhs is ast.Ident {
			fn_name := expr.lhs.name
			// Check for already-transformed string functions
			if fn_name.starts_with('string__') {
				return true
			}
			if mut scope := t.get_current_scope() {
				if obj := scope.lookup_parent(fn_name, 0) {
					typ := obj.typ()
					if typ is types.FnType {
						if return_type := typ.get_return_type() {
							if return_type is types.String {
								return true
							}
							if return_type is types.Struct && return_type.name == 'string' {
								return true
							}
						}
					}
				}
			}
			// Fallback: check if function name is known to return string
			if t.is_string_returning_method(fn_name) {
				return true
			}
			// Handle cross-module function calls like os__user_os()
			if fn_name.contains('__') {
				parts := fn_name.split('__')
				if parts.len >= 2 {
					mod_name := parts[0]
					actual_fn := parts[1..].join('__')
					if fn_type := t.env.lookup_fn(mod_name, actual_fn) {
						if return_type := fn_type.get_return_type() {
							if return_type is types.String {
								return true
							}
							if return_type is types.Struct && return_type.name == 'string' {
								return true
							}
						}
					}
					// Fallback: check if function name is known to return string
					if t.is_string_returning_method(actual_fn) {
						return true
					}
				}
			}
		}
	}
	if expr is ast.IfExpr {
		// For ternary if-expressions, check if both branches are strings
		// Check 'then' branch (stmts - last stmt should be an expression)
		if expr.stmts.len > 0 {
			last_stmt := expr.stmts[expr.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				// Use context-aware check that looks at assignments within this block
				if !t.is_string_expr_in_block(last_stmt.expr, expr.stmts) {
					return false
				}
			}
		}
		// Check 'else' branch
		if expr.else_expr is ast.IfExpr {
			if !t.is_string_expr(expr.else_expr) {
				return false
			}
		} else if expr.else_expr !is ast.EmptyExpr {
			// else_expr could be a single expression
			if !t.is_string_expr(expr.else_expr) {
				return false
			}
		}
		// If we get here and had at least one branch, treat as string
		return expr.stmts.len > 0
	}
	// Fallback: use type environment for any expression type
	if elem_type := t.get_expr_type(expr) {
		if elem_type is types.String {
			return true
		}
		if elem_type is types.Struct && elem_type.name == 'string' {
			return true
		}
	}
	return false
}

// find_var_type_in_stmts looks for a variable assignment in a list of statements
// and returns its type if it can be inferred (used for IfExpr branch checking)
fn (t &Transformer) find_var_type_in_stmts(stmts []ast.Stmt, var_name string) string {
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			if stmt.lhs.len > 0 && stmt.rhs.len > 0 {
				assigned_name := t.get_var_name(stmt.lhs[0])
				if assigned_name == var_name {
					rhs := stmt.rhs[0]
					// Check for array types from split() and similar methods
					if array_type := t.infer_array_type(rhs) {
						return array_type
					}
				}
			}
		}
	}
	return ''
}

// is_string_expr_in_block checks if an expression is a string, with context from block statements
fn (t &Transformer) is_string_expr_in_block(expr ast.Expr, stmts []ast.Stmt) bool {
	// Handle IndexExpr into local array variables within this block
	if expr is ast.IndexExpr {
		if expr.lhs is ast.Ident {
			arr_name := (expr.lhs as ast.Ident).name
			arr_type := t.find_var_type_in_stmts(stmts, arr_name)
			if arr_type == 'Array_string' {
				return true
			}
		}
	}
	// Fall back to regular is_string_expr
	return t.is_string_expr(expr)
}

// is_string_returning_fn returns true if a function is known to return a string
fn (t &Transformer) is_string_returning_fn(fn_name string) bool {
	// Known string-returning functions
	if fn_name in ['string__plus', 'string__plus_two', 'string__substr', 'string__substr_unsafe',
		'string__repeat'] {
		return true
	}
	// String module functions generally return strings
	if fn_name.starts_with('string__') {
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
	// int/u8/etc hex() method gets converted to int__hex etc
	if fn_name.ends_with('__hex') || fn_name.ends_with('__str') {
		return true
	}
	return false
}

// is_string_returning_method returns true if a method is known to return a string
fn (t &Transformer) is_string_returning_method(method_name string) bool {
	// Common string methods that return string
	return method_name in [
		'str',
		'string',
		'to_upper',
		'to_lower',
		'capitalize',
		'uncapitalize',
		'trim',
		'trim_left',
		'trim_right',
		'trim_space',
		'strip_margin',
		'replace',
		'replace_once',
		'substr',
		'substr_unsafe',
		'repeat',
		'reverse',
		'after',
		'before',
		'all_before',
		'all_after',
		'all_before_last',
		'all_after_last',
		'join',
		'ascii_str',
		'hex',
		'clone',
		'bytestr',
		// Code generation methods that return string
		'gen',
		'name',
		// Error message methods
		'posix_get_error_msg',
		'get_error_msg',
	]
}

// is_pointer_type_expr returns true if the expression is of a pointer type
fn (t &Transformer) is_pointer_type_expr(expr ast.Expr) bool {
	if expr is ast.Ident {
		// Check var_types for tracked variable type
		if var_type := t.var_types[expr.name] {
			return var_type.ends_with('*')
		}
		// Check param_types
		if param_type := t.param_types[expr.name] {
			return param_type.ends_with('*')
		}
	}
	if expr is ast.PrefixExpr {
		// &x is a pointer
		if expr.op == .amp {
			return true
		}
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
					// Plain $else block - transform all statements
					if else_e.stmts.len == 1 {
						stmt := else_e.stmts[0]
						if stmt is ast.ExprStmt {
							return t.transform_expr(stmt.expr)
						}
					}
					// Multiple statements in $else - keep as comptime expr with transformed stmts
					return ast.ComptimeExpr{
						expr: ast.IfExpr{
							cond:      node.cond
							stmts:     t.transform_stmts(node.stmts)
							else_expr: ast.IfExpr{
								cond:  else_e.cond
								stmts: t.transform_stmts(else_e.stmts)
							}
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
