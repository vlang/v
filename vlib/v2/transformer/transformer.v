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
				result << expanded
				continue
			}
		}
		// Check for OrExpr in expression statements (e.g., println(may_fail() or { 0 }))
		if stmt is ast.ExprStmt {
			if expanded := t.try_expand_or_expr_stmt(stmt) {
				result << expanded
				continue
			}
		}
		// Check for OrExpr in return statements
		if stmt is ast.ReturnStmt {
			if expanded := t.try_expand_or_expr_return(stmt) {
				result << expanded
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
	mut rhs := []ast.Expr{cap: stmt.rhs.len}
	for i, expr in stmt.rhs {
		// Track variable -> enum type mapping
		if i < stmt.lhs.len {
			lhs_expr := stmt.lhs[i]
			// Get variable name, handling ModifierExpr (mut, shared, etc.)
			var_name := t.get_var_name(lhs_expr)
			if var_name != '' {
				enum_type := t.infer_enum_type_from_expr(expr)
				if enum_type in t.flag_enum_names {
					t.var_enum_types[var_name] = enum_type
				}
				// Track string variable assignments
				if t.is_string_expr(expr) {
					t.var_types[var_name] = 'string'
				}
				// Track array variable assignments
				if array_type := t.infer_array_type(expr) {
					t.var_types[var_name] = array_type
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
			t.var_types[decl.receiver.name] = receiver_type
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
		if var_type := t.var_types[for_in.expr.name] {
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
	// Also handle string iteration
	if for_in.expr is ast.SelectorExpr || for_in.expr is ast.CallExpr {
		// String methods like .bytes(), .split() etc. - key is int, value depends on method
		// For now, default to int for key
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
	// Transform value expressions
	mut exprs := []ast.Expr{cap: expr.exprs.len}
	for e in expr.exprs {
		exprs << t.transform_expr(e)
	}

	// Check if this is a fixed-size array
	mut is_fixed := false
	mut elem_type_expr := ast.empty_expr
	match expr.typ {
		ast.Type {
			if expr.typ is ast.ArrayFixedType {
				is_fixed = true
			} else if expr.typ is ast.ArrayType {
				elem_type_expr = expr.typ.elem_type
			}
		}
		else {}
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
	// Check for 'in' operator with arrays: elem in arr => array__contains_T(arr, elem)
	if expr.op in [.key_in, .not_in] {
		// Get the element type of the array (RHS)
		if elem_type_name := t.get_array_elem_type_str(expr.rhs) {
			// Get enum type from LHS for resolving shorthand in array
			enum_type := t.get_enum_type_name(expr.lhs)
			// Transform array with enum context if needed
			transformed_rhs := if enum_type != '' && expr.rhs is ast.ArrayInitExpr {
				t.transform_array_with_enum_context(expr.rhs as ast.ArrayInitExpr, enum_type)
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
	// Check for array append: arr << elem => builtin__array_push_noscan((array*)&arr, _MOV((T[]){ elem }))
	// If RHS is also an array, use push_many instead
	if expr.op == .left_shift {
		if elem_type_name := t.get_array_elem_type_str(expr.lhs) {
			// Check if RHS is also an array (arr << other_arr => push_many)
			rhs_is_array := t.get_array_elem_type_str(expr.rhs) != none

			// Create (array*)&arr expression
			addr_of_arr := ast.PrefixExpr{
				op:   .amp
				expr: t.transform_expr(expr.lhs)
				pos:  expr.pos
			}
			cast_to_array := ast.CastExpr{
				typ:  ast.Ident{
					name: 'array*'
				}
				expr: addr_of_arr
			}

			if rhs_is_array {
				// RHS is an array - use builtin__array_push_many
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'builtin__array_push_many'
					}
					args: [
						ast.Expr(cast_to_array),
						t.transform_expr(expr.rhs),
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
					ast.Expr(cast_to_array),
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

// get_expr_type returns the types.Type for an expression by looking it up in the environment
fn (t &Transformer) get_expr_type(expr ast.Expr) ?types.Type {
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
	return none
}

// get_array_elem_type_str returns the element type name of an array variable
fn (t &Transformer) get_array_elem_type_str(expr ast.Expr) ?string {
	if expr is ast.Ident {
		// Check var_types for tracked variable type
		if var_type := t.var_types[expr.name] {
			if var_type.starts_with('Array_') {
				return var_type['Array_'.len..]
			}
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
		if method_name in ['bytes', 'split', 'split_any', 'fields', 'keys', 'values'] {
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
	if typ is types.Array {
		return t.type_to_c_name(typ.elem_type)
	}
	return none
}

// get_call_return_type returns the return type of a function call
fn (t &Transformer) get_call_return_type(expr ast.Expr) string {
	mut fn_name := ''
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			// Method call or module.function call
			fn_name = expr.lhs.rhs.name
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			fn_name = expr.lhs.rhs.name
		}
	}
	if fn_name != '' {
		if ret_type := t.fn_return_types[fn_name] {
			return ret_type
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
			}
			exprs[0]
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
	if expr is ast.Ident {
		// Check tracked variable types
		if t.var_types[expr.name] == 'string' {
			return true
		}
		// Check environment for constants
		if elem_type := t.get_expr_type(expr) {
			if elem_type is types.String {
				return true
			}
			if elem_type is types.Struct && elem_type.name == 'string' {
				return true
			}
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
	if expr is ast.IfExpr {
		// For ternary if-expressions, check if both branches are strings
		// Check 'then' branch (stmts - last stmt should be an expression)
		if expr.stmts.len > 0 {
			last_stmt := expr.stmts[expr.stmts.len - 1]
			if last_stmt is ast.ExprStmt {
				if !t.is_string_expr(last_stmt.expr) {
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
