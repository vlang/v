// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import v2.ast
import v2.token
import v2.types

// Transformer performs AST-level transformations to simplify
// and normalize code before codegen. This avoids duplicating
// transformation logic across multiple backends (SSA, cleanc, etc.)
pub struct Transformer {
mut:
	env &types.Environment
	// Current scope for type lookups (walks up scope chain)
	scope &types.Scope = unsafe { nil }
	// Current module for scope lookups
	cur_module string
	// Temp variable counter for desugaring
	temp_counter int
	// Counter for synthesized positions (uses negative values to avoid collision)
	synth_pos_counter int = -1
	// Track needed auto-generated str functions (type_name -> elem_type for arrays)
	needed_str_fns map[string]string
	// Smart cast context stack - supports nested smart casts
	smartcast_stack []SmartcastContext
}

// SmartcastContext holds info about a single smartcast
struct SmartcastContext {
	expr    string // The expression being smart-cast (e.g., "w.valera")
	variant string // The variant type name (e.g., "int", "string", "Kek")
	sumtype string // The sum type name (e.g., "Valera")
}

pub fn Transformer.new(files []ast.File, env &types.Environment) &Transformer {
	mut t := &Transformer{
		env:            unsafe { env }
		needed_str_fns: map[string]string{}
	}
	return t
}

// push_smartcast adds a new smartcast context to the stack
fn (mut t Transformer) push_smartcast(expr string, variant string, sumtype string) {
	t.smartcast_stack << SmartcastContext{
		expr:    expr
		variant: variant
		sumtype: sumtype
	}
}

// pop_smartcast removes the most recent smartcast context from the stack
fn (mut t Transformer) pop_smartcast() {
	if t.smartcast_stack.len > 0 {
		t.smartcast_stack = t.smartcast_stack[..t.smartcast_stack.len - 1]
	}
}

// SmartcastRemoveResult holds the removed context and its original index
struct SmartcastRemoveResult {
	ctx SmartcastContext
	idx int
}

// remove_smartcast_for_expr removes the smartcast context for a specific expression
// Returns the removed context or none if not found
fn (mut t Transformer) remove_smartcast_for_expr(expr_str string) ?SmartcastContext {
	if result := t.remove_smartcast_for_expr_with_idx(expr_str) {
		return result.ctx
	}
	return none
}

// remove_smartcast_for_expr_with_idx removes the smartcast context and returns both context and original index
fn (mut t Transformer) remove_smartcast_for_expr_with_idx(expr_str string) ?SmartcastRemoveResult {
	for i := t.smartcast_stack.len - 1; i >= 0; i-- {
		if t.smartcast_stack[i].expr == expr_str {
			ctx := t.smartcast_stack[i]
			// Remove this specific context by creating new slice without this element
			mut new_stack := []SmartcastContext{cap: t.smartcast_stack.len - 1}
			for j, c in t.smartcast_stack {
				if j != i {
					new_stack << c
				}
			}
			t.smartcast_stack = new_stack
			return SmartcastRemoveResult{
				ctx: ctx
				idx: i
			}
		}
	}
	return none
}

// insert_smartcast_at inserts a smartcast context at a specific position
fn (mut t Transformer) insert_smartcast_at(idx int, ctx SmartcastContext) {
	if idx >= t.smartcast_stack.len {
		// Append at end
		t.smartcast_stack << ctx
	} else {
		// Insert at position
		mut new_stack := []SmartcastContext{cap: t.smartcast_stack.len + 1}
		for i, c in t.smartcast_stack {
			if i == idx {
				new_stack << ctx
			}
			new_stack << c
		}
		// If idx was 0 and loop didn't add, add at beginning
		if idx == 0 && new_stack.len == t.smartcast_stack.len {
			new_stack = [ctx]
			new_stack << t.smartcast_stack
		}
		t.smartcast_stack = new_stack
	}
}

// find_smartcast_for_expr finds the smartcast context that matches the given expression string
// Returns the context or none if not found
fn (t &Transformer) find_smartcast_for_expr(expr_str string) ?SmartcastContext {
	// Search from most recent to oldest (reverse order)
	for i := t.smartcast_stack.len - 1; i >= 0; i-- {
		if t.smartcast_stack[i].expr == expr_str {
			return t.smartcast_stack[i]
		}
	}
	return none
}

// has_active_smartcast returns true if there's any active smartcast context
fn (t &Transformer) has_active_smartcast() bool {
	return t.smartcast_stack.len > 0
}

// cur_smartcast_expr returns the current (most recent) smartcast expression or empty string
fn (t &Transformer) cur_smartcast_expr() string {
	if t.smartcast_stack.len > 0 {
		return t.smartcast_stack[t.smartcast_stack.len - 1].expr
	}
	return ''
}

// cur_smartcast_variant returns the current (most recent) smartcast variant or empty string
fn (t &Transformer) cur_smartcast_variant() string {
	if t.smartcast_stack.len > 0 {
		return t.smartcast_stack[t.smartcast_stack.len - 1].variant
	}
	return ''
}

// next_synth_pos returns a unique negative position for synthesized AST nodes
fn (mut t Transformer) next_synth_pos() int {
	pos := t.synth_pos_counter
	t.synth_pos_counter -= 1
	return pos
}

// register_synth_type registers a type for a synthesized node position
fn (mut t Transformer) register_synth_type(pos int, typ types.Type) {
	t.env.set_expr_type(pos, typ)
}

// open_scope creates a new nested scope
fn (mut t Transformer) open_scope() {
	t.scope = types.new_scope(t.scope)
}

// close_scope returns to the parent scope
fn (mut t Transformer) close_scope() {
	if t.scope != unsafe { nil } {
		t.scope = t.scope.parent
	}
}

// lookup_var_type looks up a variable's type in the current scope chain
fn (t &Transformer) lookup_var_type(name string) ?types.Type {
	if t.scope == unsafe { nil } {
		return none
	}
	mut scope := unsafe { t.scope }
	return scope.lookup_var_type(name)
}

// is_interface_type checks if a type is an Interface
fn (t &Transformer) is_interface_type_check(typ types.Type) bool {
	return typ is types.Interface
}

// lookup_type looks up a type by name in the module scope
fn (t &Transformer) lookup_type(name string) ?types.Type {
	// Handle qualified names like "ast__Expr" by extracting module and type name
	mut lookup_name := name
	mut lookup_module := t.cur_module
	if name.contains('__') {
		parts := name.split('__')
		if parts.len >= 2 {
			lookup_module = parts[0]
			lookup_name = parts[parts.len - 1] // Get the last part (type name)
		}
	}
	mut scope := t.get_module_scope(lookup_module) or { return none }
	obj := scope.lookup_parent(lookup_name, 0) or { return none }
	if obj is types.Type {
		return obj
	}
	return none
}

// is_flag_enum checks if a type name is a flag enum
fn (t &Transformer) is_flag_enum(type_name string) bool {
	typ := t.lookup_type(type_name) or {
		$if debug ? {
			if type_name == 'ArrayFlags' {
				eprintln('DEBUG: is_flag_enum(${type_name}) lookup failed')
			}
		}
		return false
	}
	if typ is types.Enum {
		$if debug ? {
			if type_name == 'ArrayFlags' {
				eprintln('DEBUG: is_flag_enum(${type_name}) is_flag=${typ.is_flag}')
			}
		}
		return typ.is_flag
	}
	$if debug ? {
		if type_name == 'ArrayFlags' {
			eprintln('DEBUG: is_flag_enum(${type_name}) not an Enum')
		}
	}
	return false
}

// get_sum_type_variants returns the variants for a sum type
fn (t &Transformer) get_sum_type_variants(type_name string) []string {
	typ := t.lookup_type(type_name) or { return []string{} }
	if typ is types.SumType {
		mut variants := []string{}
		for v in typ.get_variants() {
			variants << v.name()
		}
		return variants
	}
	return []string{}
}

// is_sum_type checks if a type name is a sum type
fn (t &Transformer) is_sum_type(type_name string) bool {
	typ := t.lookup_type(type_name) or { return false }
	return typ is types.SumType
}

// find_sumtype_for_variant finds the sum type that contains the given variant
// This handles nested/aliased sum types by checking all known sum types
fn (t &Transformer) find_sumtype_for_variant(variant_name string) string {
	// Common sum types to check - prioritize Expr, Type, Stmt as they're most common
	sumtypes := ['Expr', 'Type', 'Stmt', 'ast__Expr', 'ast__Type', 'ast__Stmt']
	short_variant := if variant_name.contains('__') {
		variant_name.all_after_last('__')
	} else {
		variant_name
	}

	for st in sumtypes {
		variants := t.get_sum_type_variants(st)
		for v in variants {
			v_short := if v.contains('__') { v.all_after_last('__') } else { v }
			if v == variant_name || v_short == short_variant || v_short == variant_name {
				return st
			}
		}
	}
	return ''
}

// get_fn_return_type gets the return type for a function
fn (t &Transformer) get_fn_return_type(fn_name string) ?types.Type {
	// Look up function in module scope
	mut scope := t.get_module_scope(t.cur_module) or { return none }
	obj := scope.lookup_parent(fn_name, 0) or { return none }
	if obj is types.Fn {
		fn_typ := obj.get_typ()
		if fn_typ is types.FnType {
			return fn_typ.get_return_type()
		}
	}
	return none
}

// fn_returns_result checks if a function returns a Result type
fn (t &Transformer) fn_returns_result(fn_name string) bool {
	ret_type := t.get_fn_return_type(fn_name) or { return false }
	return ret_type is types.ResultType
}

// fn_returns_option checks if a function returns an Option type
fn (t &Transformer) fn_returns_option(fn_name string) bool {
	ret_type := t.get_fn_return_type(fn_name) or { return false }
	return ret_type is types.OptionType
}

// get_fn_return_base_type gets the base type name for a function returning Result/Option
fn (t &Transformer) get_fn_return_base_type(fn_name string) string {
	ret_type := t.get_fn_return_type(fn_name) or { return '' }
	match ret_type {
		types.ResultType {
			return ret_type.base_type.name()
		}
		types.OptionType {
			return ret_type.base_type.name()
		}
		else {
			return ''
		}
	}
}

// get_method_return_type tries to get the return type for a method call.
// Returns the return type if found, none otherwise.
fn (t &Transformer) get_method_return_type(expr ast.Expr) ?types.Type {
	// Check if this is a method call (CallExpr with SelectorExpr lhs)
	if expr is ast.CallExpr {
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			method_name := sel.rhs.name
			// Get the receiver type from the checker's stored types
			receiver_pos := sel.lhs.pos()
			if receiver_pos > 0 {
				if receiver_type := t.env.get_expr_type(receiver_pos) {
					// Get the type name for method lookup
					type_name := receiver_type.name()
					// Strip pointer prefix if present
					clean_name := if type_name.starts_with('&') {
						type_name[1..]
					} else {
						type_name
					}
					// Look up the method using the environment's lookup_method
					if fn_type := t.env.lookup_method(clean_name, method_name) {
						return fn_type.get_return_type()
					}
				}
			}
		}
	}
	return none
}

// expr_returns_option checks if an expression returns an Option type by looking up
// its type from the checker's environment. Works for both function and method calls.
fn (t &Transformer) expr_returns_option(expr ast.Expr) bool {
	pos := expr.pos()
	if pos > 0 {
		if typ := t.env.get_expr_type(pos) {
			if typ is types.OptionType {
				return true
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		return ret_type is types.OptionType
	}
	return false
}

// expr_returns_result checks if an expression returns a Result type by looking up
// its type from the checker's environment. Works for both function and method calls.
fn (t &Transformer) expr_returns_result(expr ast.Expr) bool {
	pos := expr.pos()
	if pos > 0 {
		if typ := t.env.get_expr_type(pos) {
			if typ is types.ResultType {
				return true
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		return ret_type is types.ResultType
	}
	return false
}

// get_expr_base_type gets the base type name for an expression returning Result/Option
fn (t &Transformer) get_expr_base_type(expr ast.Expr) string {
	pos := expr.pos()
	if pos > 0 {
		if typ := t.env.get_expr_type(pos) {
			match typ {
				types.ResultType {
					return typ.base_type.name()
				}
				types.OptionType {
					return typ.base_type.name()
				}
				else {}
			}
		}
	}
	// Fallback: try method return type lookup
	if ret_type := t.get_method_return_type(expr) {
		match ret_type {
			types.ResultType {
				return ret_type.base_type.name()
			}
			types.OptionType {
				return ret_type.base_type.name()
			}
			else {}
		}
	}
	return ''
}

// is_interface_var checks if a variable is an interface type by looking up its type in scope
fn (t &Transformer) is_interface_var(name string) bool {
	// Special case: 'err' in or-blocks is always IError interface
	if name == 'err' {
		return true
	}
	typ := t.lookup_var_type(name) or { return false }
	return typ is types.Interface
}

// get_var_type_name returns the type name of a variable from scope lookup
fn (t &Transformer) get_var_type_name(name string) string {
	typ := t.lookup_var_type(name) or { return '' }
	return typ.name()
}

// v_type_name_to_c_name converts V-style type names to C-style names
// Examples: &char -> charptr, []int -> Array_int, &[]u8 -> Array_u8ptr
fn (t &Transformer) v_type_name_to_c_name(v_name string) string {
	mut name := v_name
	// Handle pointer prefix (&T -> Tptr)
	if name.starts_with('&') {
		inner := name[1..]
		// Recursively convert the inner type first
		inner_c := t.v_type_name_to_c_name(inner)
		return '${inner_c}ptr'
	}
	// Handle pointer suffix (*T -> Tptr) - less common in V type names
	if name.ends_with('*') {
		inner := name[..name.len - 1]
		inner_c := t.v_type_name_to_c_name(inner)
		return '${inner_c}ptr'
	}
	// Handle array type ([]T -> Array_T)
	if name.starts_with('[]') {
		elem := name[2..]
		elem_c := t.v_type_name_to_c_name(elem)
		return 'Array_${elem_c}'
	}
	// No conversion needed
	return name
}

// qualify_type_name adds module prefix to type names that need it
// e.g., "File" in ast module becomes "ast__File"
fn (t &Transformer) qualify_type_name(type_name string) string {
	// Don't qualify if already qualified (contains __) or is a primitive
	if type_name.contains('__')
		|| type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'string', 'rune', 'char', 'voidptr', 'charptr', 'byteptr', 'void'] {
		return type_name
	}
	// Don't qualify Array_ or Map_ types
	if type_name.starts_with('Array_') || type_name.starts_with('Map_') {
		return type_name
	}
	// Search all module scopes to find which module defines this type
	lock t.env.scopes {
		for mod_name, scope in t.env.scopes {
			if obj := scope.objects[type_name] {
				if obj is types.Type {
					// Found it - qualify with module name (except builtin and main)
					// main module types don't get a prefix in generated C code
					if mod_name != 'builtin' && mod_name != 'main' && mod_name != '' {
						return '${mod_name}__${type_name}'
					}
					return type_name
				}
			}
		}
	}
	return type_name
}

// is_var_enum checks if a variable's type is an enum
fn (t &Transformer) is_var_enum(name string) ?string {
	typ := t.lookup_var_type(name) or { return none }
	if typ is types.Enum {
		return typ.name
	}
	return none
}

// transform_files transforms all files and returns transformed copies
pub fn (mut t Transformer) transform_files(files []ast.File) []ast.File {
	mut result := []ast.File{cap: files.len}
	for file in files {
		result << t.transform_file(file)
	}
	// Generate auto str functions and add to the builtin file
	if t.needed_str_fns.len > 0 {
		str_fn_decls := t.generate_str_functions()
		// Find builtin file or first file to add the generated functions
		for i, file in result {
			if file.mod == 'builtin' {
				mut new_stmts := file.stmts.clone()
				for fn_decl in str_fn_decls {
					new_stmts << fn_decl
				}
				result[i] = ast.File{
					attributes: file.attributes
					mod:        file.mod
					name:       file.name
					stmts:      new_stmts
					imports:    file.imports
				}
				break
			}
		}
	}
	return result
}

fn (mut t Transformer) transform_file(file ast.File) ast.File {
	// Set current module for scope lookups
	t.cur_module = file.mod
	// Set module scope as starting point
	if scope := t.get_module_scope(file.mod) {
		t.scope = scope
	} else {
		t.scope = unsafe { nil }
	}

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
			// Check for filter/map/any/all lambda expansion
			if expanded := t.try_expand_filter_assign_stmts(stmt) {
				for exp_stmt in expanded {
					result << t.transform_stmt(exp_stmt)
				}
				continue
			}
			// Check for if-guard expression: x := if r := map[key] { r } else { default }
			if expanded := t.try_expand_if_guard_assign_stmts(stmt) {
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
			// Check for if-guard in expression statements (e.g., if attr := table[name] { ... })
			if expanded := t.try_expand_if_guard_stmt(stmt) {
				// Note: try_expand_if_guard_stmt already transforms expressions internally,
				// so we don't call transform_stmt again to avoid double transformation
				for exp_stmt in expanded {
					result << exp_stmt
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
			// Check for filter in return statements
			if expanded := t.try_expand_filter_return_stmts(stmt) {
				for exp_stmt in expanded {
					result << t.transform_stmt(exp_stmt)
				}
				continue
			}
		}
		// Check for map iteration expansion
		if stmt is ast.ForStmt {
			if expanded := t.try_expand_for_in_map(stmt) {
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

	mut lhs := []ast.Expr{cap: stmt.lhs.len}
	for _, expr in stmt.lhs {
		// Transform LHS expressions (including nested IndexExpr)
		lhs << t.transform_expr(expr)
	}
	mut rhs := []ast.Expr{cap: stmt.rhs.len}
	for _, expr in stmt.rhs {
		// Type information is already in scope from checker, no need to track here
		rhs << t.transform_expr(expr)
	}
	return ast.AssignStmt{
		op:  stmt.op
		lhs: lhs
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
// For fixed array values, cleanc will generate memcpy instead of calling the set function
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
	// For fixed array values, cleanc will intercept this and generate memcpy
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

// try_expand_if_guard_assign_stmts expands an if-guard assignment to multiple statements.
// Transforms: x := if r := map[key] { r } else { default }
// Into (for maps):
//   x := if key in map { map[key] } else { default }
// Into (for other cases):
//   r := expr
//   x := if r { r } else { default }
fn (mut t Transformer) try_expand_if_guard_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	// Check for single assignment with IfExpr RHS
	if stmt.rhs.len != 1 || stmt.lhs.len != 1 {
		return none
	}
	rhs_expr := stmt.rhs[0]
	// Check if RHS is an IfExpr with IfGuardExpr condition
	if rhs_expr !is ast.IfExpr {
		return none
	}
	if_expr := rhs_expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return none
	}
	guard := if_expr.cond as ast.IfGuardExpr

	// Extract guard variable name from LHS of the guard assignment
	mut guard_var_name := ''
	for lhs_expr in guard.stmt.lhs {
		if lhs_expr is ast.Ident {
			guard_var_name = lhs_expr.name
			break
		} else if lhs_expr is ast.ModifierExpr {
			if lhs_expr.expr is ast.Ident {
				guard_var_name = lhs_expr.expr.name
				break
			}
		}
	}
	if guard_var_name == '' || guard.stmt.rhs.len == 0 {
		return none
	}

	guard_rhs := guard.stmt.rhs[0]
	synth_pos := t.next_synth_pos()

	// Check if RHS is a map index expression - use "key in map" condition
	if guard_rhs is ast.IndexExpr {
		if _ := t.get_map_type_for_expr(guard_rhs.lhs) {
			// This is a map access - transform using "key in map" check
			// x := if key in map { map[key] } else { default }
			key_in_map := ast.InfixExpr{
				op:  .key_in
				lhs: guard_rhs.expr // the key expression
				rhs: guard_rhs.lhs  // the map expression
				pos: guard_rhs.pos
			}

			// Build new stmts for the then-branch: guard_var := map[key]; <original stmts>
			mut new_then_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
			new_then_stmts << ast.AssignStmt{
				op:  .decl_assign
				lhs: guard.stmt.lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			for s in if_expr.stmts {
				new_then_stmts << s
			}

			modified_if := ast.IfExpr{
				cond:      key_in_map
				stmts:     new_then_stmts
				else_expr: if_expr.else_expr
				pos:       synth_pos
			}

			return [
				ast.Stmt(ast.AssignStmt{
					op:  stmt.op
					lhs: stmt.lhs
					rhs: [ast.Expr(modified_if)]
					pos: stmt.pos
				}),
			]
		}
	}

	// Non-map case: use original approach
	mut stmts := []ast.Stmt{}

	// 1. Guard variable declaration: r := expr
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}

	// 2. Modified if expression with guard variable as condition
	//    x := if r { r } else { default }
	// Use synthesized position to avoid inheriting wrong type from original IfGuardExpr
	guard_ident := ast.Ident{
		name: guard_var_name
		pos:  synth_pos
	}
	modified_if := ast.IfExpr{
		cond:      guard_ident
		stmts:     if_expr.stmts
		else_expr: if_expr.else_expr
		pos:       synth_pos
	}
	stmts << ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: [ast.Expr(modified_if)]
		pos: stmt.pos
	}

	return stmts
}

// try_expand_if_guard_stmt expands a statement-level if-guard.
// Transforms: if attr := table[name] { use(attr) }
// Into: if (table[name]) { attr := table[name]; use(attr) }
// For Result types: if attr := fn_call() { use(attr) }
// Into: { _tmp := fn_call(); if (!_tmp.is_error) { attr := *(_tmp.data); use(attr) } else { else_body } }
fn (mut t Transformer) try_expand_if_guard_stmt(stmt ast.ExprStmt) ?[]ast.Stmt {
	// Check if this is an IfExpr with IfGuardExpr condition
	if stmt.expr !is ast.IfExpr {
		return none
	}
	if_expr := stmt.expr as ast.IfExpr
	if if_expr.cond !is ast.IfGuardExpr {
		return none
	}
	guard := if_expr.cond as ast.IfGuardExpr

	if guard.stmt.rhs.len == 0 {
		return none
	}

	rhs := guard.stmt.rhs[0]
	synth_pos := t.next_synth_pos()

	// Check if RHS is a call that returns Result/Option
	// First try expression-based lookup (works for both function and method calls)
	mut is_result := t.expr_returns_result(rhs)
	mut is_option := t.expr_returns_option(rhs)

	// Fallback to function name lookup for simple function calls
	if !is_result && !is_option {
		fn_name := t.get_call_fn_name(rhs)
		is_result = fn_name != '' && t.fn_returns_result(fn_name)
		is_option = fn_name != '' && t.fn_returns_option(fn_name)
	}

	if is_result || is_option {
		// Handle Result/Option if-guard
		// Generate: { _tmp := call(); if (!_tmp.is_error) { attr := extractValue(_tmp); body } else { else } }
		temp_name := t.gen_temp_name()
		temp_ident := ast.Ident{
			name: temp_name
			pos:  synth_pos
		}

		mut stmts := []ast.Stmt{}

		// 1. _tmp := call()
		stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(temp_ident)]
			rhs: [t.transform_expr(rhs)]
			pos: synth_pos
		}

		// 2. Build condition: !_tmp.is_error (for Result) or _tmp.state == 0 (for Option)
		success_cond := if is_result {
			ast.Expr(ast.PrefixExpr{
				op:   .not
				expr: ast.SelectorExpr{
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'is_error'
					}
				}
			})
		} else {
			ast.Expr(ast.InfixExpr{
				op:  .eq
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

		// 3. Build if-body: attr := _tmp.data; original_body
		// cleanc handles the cast and dereference when it sees .data on Result type
		mut if_stmts := []ast.Stmt{}
		data_access := ast.SelectorExpr{
			lhs: temp_ident
			rhs: ast.Ident{
				name: 'data'
			}
		}
		if_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: guard.stmt.lhs
			rhs: [ast.Expr(data_access)]
			pos: guard.stmt.pos
		}
		for s in if_expr.stmts {
			if_stmts << s
		}

		// 4. Build the if expression
		modified_if := ast.IfExpr{
			cond:      success_cond
			stmts:     t.transform_stmts(if_stmts)
			else_expr: t.transform_expr(if_expr.else_expr)
			pos:       synth_pos
		}
		stmts << ast.ExprStmt{
			expr: modified_if
		}

		return stmts
	}

	// Non-Result/Option if-guard
	// Check if RHS is a map lookup - these need special handling with _get_check
	// For map lookups: if x := map[key] { use(x) }
	// Transform to: { _tmp := __Map_K_V_get_check(&map, key); if (_tmp != nil) { x := *_tmp; use(x) } }
	if rhs is ast.IndexExpr {
		if map_type_str := t.get_map_type_for_expr(rhs.lhs) {
			// This is a map lookup - use _get_check pattern
			temp_name := t.gen_temp_name()
			temp_ident := ast.Ident{
				name: temp_name
				pos:  synth_pos
			}

			// 1. Generate: _tmp := __Map_K_V_get_check(&map, key)
			get_check_call := ast.CallExpr{
				lhs:  ast.Ident{
					name: '__${map_type_str}_get_check'
				}
				args: [
					ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: t.transform_expr(rhs.lhs)
					}),
					t.transform_expr(rhs.expr),
				]
			}
			temp_assign := ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(temp_ident)]
				rhs: [ast.Expr(get_check_call)]
				pos: synth_pos
			}

			// 2. Build if body: guard_var := *_tmp; original_body
			mut if_stmts := []ast.Stmt{}
			deref_tmp := ast.PrefixExpr{
				op:   .mul
				expr: temp_ident
			}
			if_stmts << ast.AssignStmt{
				op:  .decl_assign
				lhs: guard.stmt.lhs
				rhs: [ast.Expr(deref_tmp)]
				pos: guard.stmt.pos
			}
			for s in if_expr.stmts {
				if_stmts << s
			}

			// 3. Build condition: _tmp != nil
			null_check := ast.InfixExpr{
				op:  .ne
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'nil'
				}
			}

			// 4. Build the if expression
			modified_if := ast.IfExpr{
				cond:      null_check
				stmts:     t.transform_stmts(if_stmts)
				else_expr: t.transform_expr(if_expr.else_expr)
				pos:       synth_pos
			}

			return [
				ast.Stmt(temp_assign),
				ast.Stmt(ast.ExprStmt{
					expr: modified_if
				}),
			]
		}
	}

	rhs_expr := t.transform_expr(rhs)

	// For map lookups returning arrays, generate:
	// { arr := map[key]; if (arr.data != nil) { ... } }
	// This handles the case where "key exists" = "non-nil data"
	map_returns_array := t.is_map_lookup_returning_array(rhs)

	// Prepend guard variable assignment to stmts (inside the if body)
	guard_assign := ast.AssignStmt{
		op:  .decl_assign
		lhs: guard.stmt.lhs
		rhs: guard.stmt.rhs
		pos: guard.stmt.pos
	}
	mut new_stmts := []ast.Stmt{cap: if_expr.stmts.len + 1}
	new_stmts << guard_assign
	for s in if_expr.stmts {
		new_stmts << s
	}

	// Determine the condition to use
	mut cond_expr := ast.Expr(rhs_expr)
	if map_returns_array {
		// For arrays, check .data != nil (indicates key existed in map)
		// Extract guard variable name
		mut guard_var_name := ''
		for lhs_expr in guard.stmt.lhs {
			if lhs_expr is ast.Ident {
				guard_var_name = lhs_expr.name
				break
			}
		}
		// Check if it's a blank identifier - if so, use a temp variable
		is_blank := guard_var_name == '_'
		if is_blank {
			guard_var_name = t.gen_temp_name()
		}
		if guard_var_name != '' {
			// Generate: guard_var.data != nil
			// But we need to declare the variable first, so we generate:
			// { arr := map[key]; if (arr.data) { ... } }
			// Put assignment before the if, then use arr.data as condition
			// When blank, use temp variable instead of _
			temp_lhs := if is_blank {
				[
					ast.Expr(ast.Ident{
						name: guard_var_name
						pos:  synth_pos
					}),
				]
			} else {
				guard.stmt.lhs
			}
			temp_assign := ast.AssignStmt{
				op:  .decl_assign
				lhs: temp_lhs
				rhs: guard.stmt.rhs
				pos: guard.stmt.pos
			}
			// Remove the guard_assign from new_stmts since we're putting it before the if
			new_stmts = []ast.Stmt{cap: if_expr.stmts.len}
			for s in if_expr.stmts {
				new_stmts << s
			}
			// Use arr.data as condition
			cond_expr = ast.SelectorExpr{
				lhs: ast.Ident{
					name: guard_var_name
					pos:  synth_pos
				}
				rhs: ast.Ident{
					name: 'data'
				}
			}
			modified_if := ast.IfExpr{
				cond:      cond_expr
				stmts:     t.transform_stmts(new_stmts)
				else_expr: t.transform_expr(if_expr.else_expr)
				pos:       synth_pos
			}
			return [
				ast.Stmt(temp_assign),
				ast.Stmt(ast.ExprStmt{
					expr: modified_if
				}),
			]
		}
	}

	modified_if := ast.IfExpr{
		cond:      cond_expr
		stmts:     t.transform_stmts(new_stmts)
		else_expr: t.transform_expr(if_expr.else_expr)
		pos:       synth_pos
	}

	return [ast.Stmt(ast.ExprStmt{
		expr: modified_if
	})]
}

// expand_direct_or_expr_assign handles the simple case where RHS is directly an OrExpr
fn (mut t Transformer) expand_direct_or_expr_assign(stmt ast.AssignStmt, or_expr ast.OrExpr) ?[]ast.Stmt {
	// The inner expression should be a call that returns Result or Option, OR a map index
	call_expr := or_expr.expr

	// Check for map index with or block: map[key] or { fallback }
	// This is handled specially since it doesn't use Result/Option types
	if call_expr is ast.IndexExpr {
		if map_result := t.try_expand_map_index_or_assign(stmt, or_expr) {
			return map_result
		}
	}

	// Check if expression returns Result or Option using expression-based lookup
	// This works for both function calls and method calls
	mut is_result := t.expr_returns_result(call_expr)
	mut is_option := t.expr_returns_option(call_expr)

	// Fallback to function name lookup for simple function calls
	fn_name := t.get_call_fn_name(call_expr)
	if !is_result && !is_option && fn_name != '' {
		is_result = t.fn_returns_result(fn_name)
		is_option = t.fn_returns_option(fn_name)
	}

	if !is_result && !is_option {
		return none
	}

	// Get base type using expression-based lookup first, then fallback
	mut base_type := t.get_expr_base_type(call_expr)
	if base_type == '' && fn_name != '' {
		base_type = t.get_fn_return_base_type(fn_name)
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
	// err is IError type - will be looked up via scope when needed
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
		// Don't transform here - they'll be transformed when IfExpr is processed
		if_stmts << or_expr.stmts
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
		// Variable type is already tracked in scope by checker
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

// gen_filter_temp_name generates a unique temporary variable name for filter expansion
fn (mut t Transformer) gen_filter_temp_name() string {
	t.temp_counter++
	return '_filter_t${t.temp_counter}'
}

// try_expand_filter_assign_stmts expands array.filter(it ...) to a for loop.
// Transforms: filtered := arr.filter(it % 2 == 0)
// Into:
//   mut _filter_t1 := Array_T{}
//   for _filter_it in arr { if condition { array__push((array*)&_filter_t1, &_filter_it) } }
//   filtered := _filter_t1
fn (mut t Transformer) try_expand_filter_assign_stmts(stmt ast.AssignStmt) ?[]ast.Stmt {
	// Check for single assignment
	if stmt.rhs.len != 1 || stmt.lhs.len != 1 {
		return none
	}
	rhs_expr := stmt.rhs[0]
	// Check if RHS is a method call (CallOrCastExpr with SelectorExpr lhs)
	method_name, receiver_expr, filter_cond := t.get_filter_call_info(rhs_expr) or { return none }
	if method_name != 'filter' {
		return none
	}
	// Get the array type from the receiver
	array_type := t.infer_array_type(receiver_expr) or { return none }
	elem_type := array_type['Array_'.len..]

	// Generate temp variable name
	temp_name := t.gen_filter_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}
	// The 'it' variable is used inside the filter condition
	it_ident := ast.Ident{
		name: '_filter_it'
	}
	// Build the expanded statements
	mut stmts := []ast.Stmt{}

	// 1. mut _filter_t1 := []T{cap: 0} (empty array initialization with element_size set)
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: temp_ident
			}),
		]
		rhs: [
			ast.Expr(ast.ArrayInitExpr{
				typ: ast.Expr(ast.Type(ast.ArrayType{
					elem_type: ast.Ident{
						name: elem_type
					}
				}))
				cap: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}),
		]
		pos: stmt.pos
	}

	// 2. Build the for loop: for _filter_it in arr { if cond { array__push(&_filter_t1, &_filter_it) } }
	// Replace 'it' with '_filter_it' in the condition
	transformed_cond := t.replace_it_ident(filter_cond, '_filter_it')

	// Build the push call: array__push((array*)&_filter_t1, &_filter_it)
	push_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__push'
		}
		args: [
			// (array*)&_filter_t1
			ast.Expr(ast.CastExpr{
				typ:  ast.Ident{
					name: 'array*'
				}
				expr: ast.PrefixExpr{
					op:   .amp
					expr: temp_ident
				}
			}),
			// &_filter_it
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: it_ident
			}),
		]
	}

	// Build the if statement: if cond { push }
	if_stmt := ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  transformed_cond
			stmts: [ast.Stmt(ast.ExprStmt{
				expr: push_call
			})]
		}
	}

	// Build the for-in loop
	for_stmt := ast.ForStmt{
		init:  ast.ForInStmt{
			value: it_ident
			expr:  t.transform_expr(receiver_expr)
		}
		stmts: [ast.Stmt(if_stmt)]
	}
	stmts << for_stmt

	// 3. result := _filter_t1
	stmts << ast.AssignStmt{
		op:  stmt.op
		lhs: stmt.lhs
		rhs: [ast.Expr(temp_ident)]
		pos: stmt.pos
	}

	return stmts
}

// try_expand_filter_return_stmts expands return arr.filter(it ...) to statements.
// Transforms: return arr.filter(it % 2 == 0)
// Into:
//   mut _filter_t1 := Array_T{}
//   for _filter_it in arr { if condition { array__push((array*)&_filter_t1, &_filter_it) } }
//   return _filter_t1
fn (mut t Transformer) try_expand_filter_return_stmts(stmt ast.ReturnStmt) ?[]ast.Stmt {
	// Check for single expression return with filter
	if stmt.exprs.len != 1 {
		return none
	}
	rhs_expr := stmt.exprs[0]
	// Check if expr is a filter call
	method_name, receiver_expr, filter_cond := t.get_filter_call_info(rhs_expr) or { return none }
	if method_name != 'filter' {
		return none
	}
	// Get the array type from the receiver
	array_type := t.infer_array_type(receiver_expr) or { return none }
	elem_type := array_type['Array_'.len..]

	// Generate temp variable name
	temp_name := t.gen_filter_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}
	// The 'it' variable is used inside the filter condition
	it_ident := ast.Ident{
		name: '_filter_it'
	}
	// Build the expanded statements
	mut stmts := []ast.Stmt{}

	// 1. mut _filter_t1 := []T{cap: 0} (empty array initialization with element_size set)
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: temp_ident
			}),
		]
		rhs: [
			ast.Expr(ast.ArrayInitExpr{
				typ: ast.Expr(ast.Type(ast.ArrayType{
					elem_type: ast.Ident{
						name: elem_type
					}
				}))
				cap: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}),
		]
	}

	// 2. Build the for loop: for _filter_it in arr { if cond { array__push(&_filter_t1, &_filter_it) } }
	// Replace 'it' with '_filter_it' in the condition
	transformed_cond := t.replace_it_ident(filter_cond, '_filter_it')

	// Build the push call: array__push((array*)&_filter_t1, &_filter_it)
	push_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__push'
		}
		args: [
			// (array*)&_filter_t1
			ast.Expr(ast.CastExpr{
				typ:  ast.Ident{
					name: 'array*'
				}
				expr: ast.PrefixExpr{
					op:   .amp
					expr: temp_ident
				}
			}),
			// &_filter_it
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: it_ident
			}),
		]
	}

	// Build the if statement: if cond { push }
	if_stmt := ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  transformed_cond
			stmts: [ast.Stmt(ast.ExprStmt{
				expr: push_call
			})]
		}
	}

	// Build the for-in loop
	for_stmt := ast.ForStmt{
		init:  ast.ForInStmt{
			value: it_ident
			expr:  t.transform_expr(receiver_expr)
		}
		stmts: [ast.Stmt(if_stmt)]
	}
	stmts << for_stmt

	// 3. return _filter_t1
	stmts << ast.ReturnStmt{
		exprs: [ast.Expr(temp_ident)]
	}

	return stmts
}

// get_filter_call_info extracts info from a filter method call.
// Returns (method_name, receiver_expr, condition_expr) or none if not a filter call.
fn (t &Transformer) get_filter_call_info(expr ast.Expr) ?(string, ast.Expr, ast.Expr) {
	// Check for CallOrCastExpr: arr.filter(cond)
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			method_name := sel.rhs.name
			if method_name == 'filter' {
				return method_name, sel.lhs, expr.expr
			}
		}
	}
	// Check for CallExpr: arr.filter(cond)
	if expr is ast.CallExpr {
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			method_name := sel.rhs.name
			if method_name == 'filter' && expr.args.len == 1 {
				return method_name, sel.lhs, expr.args[0]
			}
		}
	}
	return none
}

// replace_it_ident replaces all occurrences of 'it' identifier with the given name
fn (t &Transformer) replace_it_ident(expr ast.Expr, new_name string) ast.Expr {
	match expr {
		ast.Ident {
			if expr.name == 'it' {
				return ast.Ident{
					name: new_name
					pos:  expr.pos
				}
			}
			return expr
		}
		ast.InfixExpr {
			return ast.InfixExpr{
				op:  expr.op
				lhs: t.replace_it_ident(expr.lhs, new_name)
				rhs: t.replace_it_ident(expr.rhs, new_name)
				pos: expr.pos
			}
		}
		ast.PrefixExpr {
			return ast.PrefixExpr{
				op:   expr.op
				expr: t.replace_it_ident(expr.expr, new_name)
				pos:  expr.pos
			}
		}
		ast.ParenExpr {
			return ast.ParenExpr{
				expr: t.replace_it_ident(expr.expr, new_name)
				pos:  expr.pos
			}
		}
		ast.CallExpr {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.replace_it_ident(arg, new_name)
			}
			return ast.CallExpr{
				lhs:  t.replace_it_ident(expr.lhs, new_name)
				args: new_args
				pos:  expr.pos
			}
		}
		ast.CallOrCastExpr {
			return ast.CallOrCastExpr{
				lhs:  t.replace_it_ident(expr.lhs, new_name)
				expr: t.replace_it_ident(expr.expr, new_name)
				pos:  expr.pos
			}
		}
		ast.SelectorExpr {
			return ast.SelectorExpr{
				lhs: t.replace_it_ident(expr.lhs, new_name)
				rhs: expr.rhs
				pos: expr.pos
			}
		}
		ast.IndexExpr {
			return ast.IndexExpr{
				lhs:  t.replace_it_ident(expr.lhs, new_name)
				expr: t.replace_it_ident(expr.expr, new_name)
			}
		}
		ast.CastExpr {
			return ast.CastExpr{
				typ:  expr.typ
				expr: t.replace_it_ident(expr.expr, new_name)
				pos:  expr.pos
			}
		}
		else {
			return expr
		}
	}
}

// gen_map_iter_temp_name generates unique temporary variable names for map iteration
fn (mut t Transformer) gen_map_iter_temp_name(suffix string) string {
	t.temp_counter++
	return '_map_${suffix}_${t.temp_counter}'
}

// type_to_c_decl_name converts a V type to a C declaration type name (with *)
// e.g., &T -> T*, []T -> Array_T, map[K]V -> Map_K_V
// Unlike type_to_c_name which returns mangled names (Tptr), this returns actual C syntax (T*)
fn (t &Transformer) type_to_c_decl_name(typ types.Type) string {
	match typ {
		types.Pointer {
			base_name := t.type_to_c_decl_name(typ.base_type)
			return '${base_name}*'
		}
		types.Array {
			elem_name := t.type_to_c_decl_name(typ.elem_type)
			return 'Array_${elem_name}'
		}
		types.Map {
			key_name := t.type_to_c_decl_name(typ.key_type)
			value_name := t.type_to_c_decl_name(typ.value_type)
			return 'Map_${key_name}_${value_name}'
		}
		types.Struct {
			// Replace . with __ for module-qualified names
			return typ.name.replace('.', '__')
		}
		types.String {
			return 'string'
		}
		types.Primitive {
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
		else {
			name := typ.name()
			// Handle pointer prefix
			if name.starts_with('&') {
				return name[1..].replace('.', '__') + '*'
			}
			return name.replace('.', '__')
		}
	}
}

// try_expand_for_in_map expands map iteration to lower-level constructs.
// Transforms: for k, v in map_expr { body }
// Into:
//   {
//       mut _map_len := map_expr.key_values.len
//       for _map_idx := 0; _map_idx < _map_len; _map_idx++ {
//           _map_delta := map_expr.key_values.len - _map_len
//           _map_len = map_expr.key_values.len
//           if _map_delta < 0 { _map_idx = -1; continue }
//           if !DenseArray__has_index(&map_expr.key_values, _map_idx) { continue }
//           k := *(KeyType*)DenseArray__key(&map_expr.key_values, _map_idx)
//           v := *(ValueType*)DenseArray__value(&map_expr.key_values, _map_idx)
//           body
//       }
//   }
fn (mut t Transformer) try_expand_for_in_map(stmt ast.ForStmt) ?[]ast.Stmt {
	// Check if this is a for-in statement
	if stmt.init !is ast.ForInStmt {
		return none
	}
	for_in := stmt.init as ast.ForInStmt

	// Get the type of the iterable expression
	iter_type := t.get_expr_type(for_in.expr) or { return none }

	// Check if it's a map type
	if iter_type !is types.Map {
		return none
	}
	map_type := iter_type as types.Map

	// Get key variable name
	mut key_name := ''
	mut key_is_blank := false
	if for_in.key !is ast.EmptyExpr {
		if for_in.key is ast.Ident {
			key_name = for_in.key.name
			key_is_blank = key_name == '_'
		} else if for_in.key is ast.ModifierExpr {
			if for_in.key.expr is ast.Ident {
				key_name = for_in.key.expr.name
				key_is_blank = key_name == '_'
			}
		}
	}

	// Get value variable name
	mut value_name := ''
	if for_in.value is ast.Ident {
		value_name = for_in.value.name
	} else if for_in.value is ast.ModifierExpr {
		if for_in.value.expr is ast.Ident {
			value_name = for_in.value.expr.name
		}
	}

	// Get C-compatible type names for key and value (using C declaration syntax with *)
	key_type_name := t.type_to_c_decl_name(map_type.key_type)
	value_type_name := t.type_to_c_decl_name(map_type.value_type)

	// Generate unique temp variable names
	idx_name := t.gen_map_iter_temp_name('idx')
	len_name := t.gen_map_iter_temp_name('len')
	delta_name := t.gen_map_iter_temp_name('delta')

	idx_ident := ast.Ident{
		name: idx_name
	}
	len_ident := ast.Ident{
		name: len_name
	}
	delta_ident := ast.Ident{
		name: delta_name
	}

	// Transform the map expression and store in temp variable to avoid multiple evaluations
	map_expr := t.transform_expr(for_in.expr)

	// Generate temp name for the map to avoid re-evaluating the map expression multiple times
	map_tmp_name := t.gen_map_iter_temp_name('map')
	map_tmp_ident := ast.Ident{
		name: map_tmp_name
	}

	// key_values selector: _map_tmp.key_values
	key_values_expr := ast.SelectorExpr{
		lhs: map_tmp_ident
		rhs: ast.Ident{
			name: 'key_values'
		}
	}

	// key_values.len selector: map_expr.key_values.len
	key_values_len_expr := ast.SelectorExpr{
		lhs: key_values_expr
		rhs: ast.Ident{
			name: 'len'
		}
	}

	mut stmts := []ast.Stmt{}

	// 0. _map_tmp := map_expr (store map in temp variable to avoid re-evaluation)
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(map_tmp_ident)]
		rhs: [ast.Expr(map_expr)]
	}

	// 1. mut _map_len := _map_tmp.key_values.len
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.ModifierExpr{
			kind: .key_mut
			expr: len_ident
		})]
		rhs: [ast.Expr(key_values_len_expr)]
	}

	// Build the inner loop body
	mut loop_body := []ast.Stmt{}

	// _map_delta := map_expr.key_values.len - _map_len
	loop_body << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(delta_ident)]
		rhs: [
			ast.Expr(ast.InfixExpr{
				op:  .minus
				lhs: key_values_len_expr
				rhs: len_ident
			}),
		]
	}

	// _map_len = map_expr.key_values.len
	loop_body << ast.AssignStmt{
		op:  .assign
		lhs: [ast.Expr(len_ident)]
		rhs: [ast.Expr(key_values_len_expr)]
	}

	// if _map_delta < 0 { _map_idx = -1; continue }
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.InfixExpr{
				op:  .lt
				lhs: delta_ident
				rhs: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}
			stmts: [
				ast.AssignStmt{
					op:  .assign
					lhs: [ast.Expr(idx_ident)]
					rhs: [
						ast.Expr(ast.PrefixExpr{
							op:   .minus
							expr: ast.BasicLiteral{
								kind:  .number
								value: '1'
							}
						}),
					]
				},
				ast.FlowControlStmt{
					op: .key_continue
				},
			]
		}
	}

	// if !DenseArray__has_index(&map_expr.key_values, _map_idx) { continue }
	has_index_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: 'DenseArray__has_index'
		}
		args: [
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: key_values_expr
			}),
			ast.Expr(idx_ident),
		]
	}
	loop_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.PrefixExpr{
				op:   .not
				expr: has_index_call
			}
			stmts: [ast.Stmt(ast.FlowControlStmt{
				op: .key_continue
			})]
		}
	}

	// k := *(KeyType*)DenseArray__key(&map_expr.key_values, _map_idx)
	// This is represented as a cast expression wrapping the call
	if !key_is_blank && key_name != '' {
		key_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__key'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		// Cast to KeyType* then dereference: *(KeyType*)call
		key_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${key_type_name}*'
			}
			expr: key_call
		}
		key_deref := ast.PrefixExpr{
			op:   .mul
			expr: key_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: key_name
			})]
			rhs: [ast.Expr(key_deref)]
		}
		// Register key variable type in scope for later string detection
		t.scope.insert(key_name, map_type.key_type)
	}

	// v := *(ValueType*)DenseArray__value(&map_expr.key_values, _map_idx)
	if value_name != '' && value_name != '_' {
		value_call := ast.CallExpr{
			lhs:  ast.Ident{
				name: 'DenseArray__value'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: key_values_expr
				}),
				ast.Expr(idx_ident),
			]
		}
		// Cast to ValueType* then dereference: *(ValueType*)call
		value_cast := ast.CastExpr{
			typ:  ast.Ident{
				name: '${value_type_name}*'
			}
			expr: value_call
		}
		value_deref := ast.PrefixExpr{
			op:   .mul
			expr: value_cast
		}
		loop_body << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: value_name
			})]
			rhs: [ast.Expr(value_deref)]
		}
		// Register value variable type in scope for later type detection
		t.scope.insert(value_name, map_type.value_type)
	}

	// Add the original body statements (NOT transformed here - transform_stmts will do it)
	for body_stmt in stmt.stmts {
		loop_body << body_stmt
	}

	// 2. Build the for loop:
	// for _map_idx := 0; _map_idx < _map_len; _map_idx++ { ... }
	for_stmt := ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})]
		}
		cond:  ast.InfixExpr{
			op:  .lt
			lhs: idx_ident
			rhs: len_ident
		}
		post:  ast.AssignStmt{
			op:  .assign
			lhs: [ast.Expr(idx_ident)]
			rhs: [
				ast.Expr(ast.InfixExpr{
					op:  .plus
					lhs: idx_ident
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '1'
					}
				}),
			]
		}
		stmts: loop_body
	}
	stmts << for_stmt

	return stmts
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
			// Check for nested OrExpr that contains control flow
			// e.g., scopes[k1] or { scopes[k2] or { return -999 } }
			if stmt.expr is ast.OrExpr {
				or_expr := stmt.expr as ast.OrExpr
				if t.or_block_has_return(or_expr.stmts) {
					return true
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
				pos:  expr.pos
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
				pos:       expr.pos
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

	// Check for map index with or block: map[key] or { fallback }
	if result := t.try_expand_map_index_or(or_expr, mut prefix_stmts) {
		return result
	}

	// Check if expression returns Result or Option using expression-based lookup
	// This works for both function calls and method calls
	mut is_result := t.expr_returns_result(call_expr)
	mut is_option := t.expr_returns_option(call_expr)

	// Fallback to function name lookup for simple function calls
	fn_name := t.get_call_fn_name(call_expr)
	if !is_result && !is_option && fn_name != '' {
		is_result = t.fn_returns_result(fn_name)
		is_option = t.fn_returns_option(fn_name)
	}

	if !is_result && !is_option {
		// Not a Result/Option call, return as-is
		return or_expr
	}

	// Get base type using expression-based lookup first, then fallback
	mut base_type := t.get_expr_base_type(call_expr)
	if base_type == '' && fn_name != '' {
		base_type = t.get_fn_return_base_type(fn_name)
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
	// err is IError type - will be looked up via scope when needed
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
		// Don't transform here - they'll be transformed when IfExpr is processed
		if_stmts << or_expr.stmts
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

// try_expand_map_index_or handles the pattern: map[key] or { fallback }
// Transforms it to use map_get_check for safe lookup with fallback.
// Returns none if not a map index expression.
fn (mut t Transformer) try_expand_map_index_or(or_expr ast.OrExpr, mut prefix_stmts []ast.Stmt) ?ast.Expr {
	// Check if the inner expression is an IndexExpr
	if or_expr.expr !is ast.IndexExpr {
		return none
	}
	index_expr := or_expr.expr as ast.IndexExpr

	// Check if the indexed expression is a map
	map_type_str := t.get_map_type_for_expr(index_expr.lhs) or { return none }

	// Generate temp variable name for the pointer result
	temp_name := t.gen_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}

	// 1. Generate: _t1 := __Map_K_V_get_check(&m, key)
	// This returns a pointer to the value, or null if not found
	get_check_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: '__${map_type_str}_get_check'
		}
		args: [
			// &m (address of map)
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: t.transform_expr(index_expr.lhs)
			}),
			// key
			t.transform_expr(index_expr.expr),
		]
	}
	prefix_stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(temp_ident)]
		rhs: [ast.Expr(get_check_call)]
	}

	// Check if or block has control flow (return/break/continue)
	has_control_flow := t.or_block_has_return(or_expr.stmts)

	// 2. Generate: if _t1 == nil { ... }
	null_check := ast.InfixExpr{
		op:  .eq
		lhs: temp_ident
		rhs: ast.Ident{
			name: 'nil'
		}
	}

	if has_control_flow {
		// Or block has control flow - use the statements directly
		// The control flow will exit, so we just dereference after
		prefix_stmts << ast.ExprStmt{
			expr: ast.IfExpr{
				cond:  null_check
				stmts: or_expr.stmts
			}
		}
	} else {
		// Or block provides a fallback value
		// We use two statements:
		// 1. result := fallback
		// 2. if ptr != nil { result = *ptr }
		result_temp := t.gen_temp_name()
		result_ident := ast.Ident{
			name: result_temp
		}

		// Declare result with fallback value
		prefix_stmts << ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(result_ident)]
			rhs: [t.get_or_block_value(or_expr.stmts)]
		}

		// If ptr != nil, overwrite with actual value
		prefix_stmts << ast.ExprStmt{
			expr: ast.IfExpr{
				cond:  ast.InfixExpr{
					op:  .ne
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'nil'
					}
				}
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .assign
						lhs: [ast.Expr(result_ident)]
						rhs: [
							ast.Expr(ast.PrefixExpr{
								op:   .mul
								expr: temp_ident
							}),
						]
					}),
				]
			}
		}
		return result_ident
	}

	// For control flow case, return the dereferenced pointer
	// (we know it's non-null because control flow would have exited)
	return ast.PrefixExpr{
		op:   .mul
		expr: temp_ident
	}
}

// try_expand_map_index_or_assign handles: var := map[key] or { fallback }
// Returns a list of statements that expand the assignment with proper map lookup.
fn (mut t Transformer) try_expand_map_index_or_assign(stmt ast.AssignStmt, or_expr ast.OrExpr) ?[]ast.Stmt {
	// Check if the inner expression is an IndexExpr
	if or_expr.expr !is ast.IndexExpr {
		return none
	}
	index_expr := or_expr.expr as ast.IndexExpr

	// Check if the indexed expression is a map
	map_type_str := t.get_map_type_for_expr(index_expr.lhs) or { return none }

	// Get the LHS variable name from the assignment
	if stmt.lhs.len != 1 {
		return none
	}

	mut stmts := []ast.Stmt{}

	// Generate temp variable name for the pointer result
	temp_name := t.gen_temp_name()
	temp_ident := ast.Ident{
		name: temp_name
	}

	// 1. Generate: _t1 := __Map_K_V_get_check(&m, key)
	get_check_call := ast.CallExpr{
		lhs:  ast.Ident{
			name: '__${map_type_str}_get_check'
		}
		args: [
			// &m (address of map)
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: t.transform_expr(index_expr.lhs)
			}),
			// key
			t.transform_expr(index_expr.expr),
		]
	}
	stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(temp_ident)]
		rhs: [ast.Expr(get_check_call)]
		pos: stmt.pos
	}

	// Check if or block has control flow (return/break/continue)
	has_control_flow := t.or_block_has_return(or_expr.stmts)

	if has_control_flow {
		// Check if or block contains a nested map or block that needs expansion
		// e.g., scopes[k1] or { scopes[k2] or { return -999 } }
		mut has_nested_or := false
		for or_stmt in or_expr.stmts {
			if or_stmt is ast.ExprStmt {
				if or_stmt.expr is ast.OrExpr {
					inner_or := or_stmt.expr as ast.OrExpr
					if inner_or.expr is ast.IndexExpr {
						has_nested_or = true
						break
					}
				}
			}
		}

		if has_nested_or {
			// For nested or blocks, we need to declare the outer variable first
			// so we can assign to it inside the nested expansion
			// Pattern:
			// 1. lhs := 0 (default)
			// 2. if ptr1 == nil { ... nested ... lhs = nested_result }
			// 3. if ptr1 != nil { lhs = *ptr1 }

			// 1. Declare lhs with zero value
			stmts << ast.AssignStmt{
				op:  stmt.op
				lhs: stmt.lhs
				rhs: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					}),
				]
				pos: stmt.pos
			}

			// 2. Expand the nested or block in the null case
			null_check := ast.InfixExpr{
				op:  .eq
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'nil'
				}
			}
			mut expanded_or_stmts := []ast.Stmt{}
			for or_stmt in or_expr.stmts {
				if or_stmt is ast.ExprStmt {
					expr_stmt := or_stmt as ast.ExprStmt
					if expr_stmt.expr is ast.OrExpr {
						inner_or := expr_stmt.expr as ast.OrExpr
						if inner_or.expr is ast.IndexExpr {
							// Create an assignment for the nested or
							inner_assign := ast.AssignStmt{
								op:  .assign
								lhs: stmt.lhs
								rhs: [ast.Expr(inner_or)]
							}
							if inner_stmts := t.try_expand_map_index_or_assign(inner_assign,
								inner_or)
							{
								expanded_or_stmts << inner_stmts
								continue
							}
						}
					}
				}
				expanded_or_stmts << or_stmt
			}
			stmts << ast.ExprStmt{
				expr: ast.IfExpr{
					cond:  null_check
					stmts: expanded_or_stmts
				}
			}

			// 3. If outer lookup succeeded, assign deref
			stmts << ast.ExprStmt{
				expr: ast.IfExpr{
					cond:  ast.InfixExpr{
						op:  .ne
						lhs: temp_ident
						rhs: ast.Ident{
							name: 'nil'
						}
					}
					stmts: [
						ast.Stmt(ast.AssignStmt{
							op:  .assign
							lhs: stmt.lhs
							rhs: [
								ast.Expr(ast.PrefixExpr{
									op:   .mul
									expr: temp_ident
								}),
							]
						}),
					]
				}
			}
		} else {
			// Simple control flow case (e.g., return inside or block)
			null_check := ast.InfixExpr{
				op:  .eq
				lhs: temp_ident
				rhs: ast.Ident{
					name: 'nil'
				}
			}
			stmts << ast.ExprStmt{
				expr: ast.IfExpr{
					cond:  null_check
					stmts: or_expr.stmts
				}
			}
			// lhs := *_t1
			stmts << ast.AssignStmt{
				op:  stmt.op
				lhs: stmt.lhs
				rhs: [
					ast.Expr(ast.PrefixExpr{
						op:   .mul
						expr: temp_ident
					}),
				]
				pos: stmt.pos
			}
		}
	} else {
		// 2b. lhs := fallback
		stmts << ast.AssignStmt{
			op:  stmt.op
			lhs: stmt.lhs
			rhs: [t.get_or_block_value(or_expr.stmts)]
			pos: stmt.pos
		}
		// 3b. if _t1 != nil { lhs = *_t1 }
		stmts << ast.ExprStmt{
			expr: ast.IfExpr{
				cond:  ast.InfixExpr{
					op:  .ne
					lhs: temp_ident
					rhs: ast.Ident{
						name: 'nil'
					}
				}
				stmts: [
					ast.Stmt(ast.AssignStmt{
						op:  .assign
						lhs: stmt.lhs
						rhs: [
							ast.Expr(ast.PrefixExpr{
								op:   .mul
								expr: temp_ident
							}),
						]
					}),
				]
			}
		}
	}

	return stmts
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
	// Save current scope
	old_scope := t.scope

	// Get the function's scope from the environment (populated by checker)
	// This contains parameter types, receiver type, and local variables
	// For methods, include receiver type in the key (e.g., "SortedMap__set")
	// The key must match how the checker generates it (using resolved/base type)
	scope_fn_name := if decl.is_method {
		// Try to get the resolved receiver type from the type system
		// This handles aliases like Builder -> []u8
		if receiver_type := t.get_expr_type(decl.receiver.typ) {
			// Get base type (unwrap pointers, aliases, etc.)
			base_type := receiver_type.base_type()
			'${base_type.name()}__${decl.name}'
		} else {
			// Fallback to AST type name if type resolution fails
			// Qualify with current module to match checker's key format
			receiver_type_name := t.get_receiver_type_name(decl.receiver.typ)
			qualified_name := if t.cur_module != '' && t.cur_module != 'main'
				&& t.cur_module != 'builtin' && !receiver_type_name.contains('__') {
				'${t.cur_module}__${receiver_type_name}'
			} else {
				receiver_type_name
			}
			'${qualified_name}__${decl.name}'
		}
	} else {
		decl.name
	}
	if fn_scope := t.env.get_fn_scope(t.cur_module, scope_fn_name) {
		t.scope = fn_scope
		$if debug ? {
			mut obj_names := []string{}
			for name, _ in fn_scope.objects {
				obj_names << name
			}
			eprintln('DEBUG: get_fn_scope(${t.cur_module}, ${scope_fn_name}) FOUND: ${obj_names}')
		}
	} else {
		// Fallback: create a new scope if function scope not found
		t.open_scope()
	}

	// Transform function body
	transformed_stmts := t.transform_stmts(decl.stmts)

	// Restore previous scope
	t.scope = old_scope

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
	// Open a child scope for loop variables
	t.open_scope()

	// Check if this is a for-in loop (init is ForInStmt)
	if stmt.init is ast.ForInStmt {
		// Infer the element type from the iterable expression and register loop variables
		if elem_type := t.get_expr_type(stmt.init.expr) {
			value_type := elem_type.value_type()
			// Register value variable if it has a name
			if stmt.init.value is ast.Ident {
				value_name := (stmt.init.value as ast.Ident).name
				if value_name != '' && value_name != '_' {
					t.scope.insert(value_name, value_type)
				}
			}
			// Register key variable if present
			key_type := elem_type.key_type()
			if stmt.init.key is ast.Ident {
				key_name := (stmt.init.key as ast.Ident).name
				if key_name != '' && key_name != '_' {
					t.scope.insert(key_name, key_type)
				}
			}
		}
	}

	result := ast.ForStmt{
		init:  t.transform_stmt(stmt.init)
		cond:  t.transform_expr(stmt.cond)
		post:  t.transform_stmt(stmt.post)
		stmts: t.transform_stmts(stmt.stmts)
	}
	t.close_scope()
	return result
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
				pos:  expr.pos
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
		ast.LockExpr {
			// Transform the statements inside lock blocks (important for map iteration expansion)
			ast.Expr(ast.LockExpr{
				lock_exprs:  expr.lock_exprs
				rlock_exprs: expr.rlock_exprs
				stmts:       t.transform_stmts(expr.stmts)
				pos:         expr.pos
			})
		}
		ast.FieldInit {
			// Transform the value inside field initializations (e.g., fn(name: expr))
			ast.Expr(ast.FieldInit{
				name:  expr.name
				value: t.transform_expr(expr.value)
			})
		}
		ast.SelectorExpr {
			t.transform_selector_expr(expr)
		}
		ast.Ident {
			// Check for smart cast on simple identifiers (e.g., if x is Type { x })
			if ctx := t.find_smartcast_for_expr(expr.name) {
				return t.apply_smartcast_direct_ctx(expr, ctx)
			}
			expr
		}
		ast.StringInterLiteral {
			// Transform interpolations, applying smart cast if needed
			t.transform_string_inter_literal(expr)
		}
		ast.AsCastExpr {
			// Transform the inner expression (important for smartcast to be applied)
			ast.Expr(ast.AsCastExpr{
				expr: t.transform_expr(expr.expr)
				typ:  expr.typ
				pos:  expr.pos
			})
		}
		else {
			expr
		}
	}
}

// transform_selector_expr transforms a selector expression, applying smart cast if applicable
fn (mut t Transformer) transform_selector_expr(expr ast.SelectorExpr) ast.Expr {
	// Check for smart cast field access: check ALL contexts in the stack
	if t.has_active_smartcast() {
		full_str := t.expr_to_string(expr)
		// First check if the ENTIRE selector matches a direct smartcast context
		// This handles cases like `sel := rhs_expr.lhs` inside `if rhs_expr.lhs is SelectorExpr`
		if direct_ctx := t.find_smartcast_for_expr(full_str) {
			// Direct access to smartcast variable - apply direct smartcast
			return t.apply_smartcast_direct_ctx(expr, direct_ctx)
		}
		// Check if LHS matches any smartcast context for field access
		lhs_str := t.expr_to_string(expr.lhs)
		if ctx := t.find_smartcast_for_expr(lhs_str) {
			// This is a field access on the smartcast variable
			// e.g., w.valera.len when w.valera is smartcast to string
			return t.apply_smartcast_field_access_ctx(expr.lhs, expr.rhs.name, ctx)
		}
	}
	// Default transformation
	return ast.SelectorExpr{
		lhs: t.transform_expr(expr.lhs)
		rhs: expr.rhs
		pos: expr.pos
	}
}

// transform_string_inter_literal transforms string interpolations, applying smart cast where needed
fn (mut t Transformer) transform_string_inter_literal(expr ast.StringInterLiteral) ast.Expr {
	mut new_inters := []ast.StringInter{cap: expr.inters.len}
	for inter in expr.inters {
		new_inters << ast.StringInter{
			format:      inter.format
			width:       inter.width
			precision:   inter.precision
			expr:        t.transform_expr(inter.expr)
			format_expr: inter.format_expr
		}
	}
	return ast.StringInterLiteral{
		kind:   expr.kind
		values: expr.values
		inters: new_inters
	}
}

// apply_smartcast_direct_ctx generates a cast expression for direct access to a smartcast variable
// For primitives: ((int)(intptr_t)v._data._int) - cast from pointer space back to value
// For structs/strings: (*((ast__Type*)v._data._Type)) - dereference pointer
fn (mut t Transformer) apply_smartcast_direct_ctx(original_expr ast.Expr, ctx SmartcastContext) ast.Expr {
	variant_full := ctx.variant
	// Extract simple variant name for _data._ accessor (strip module prefix)
	variant_simple := if variant_full.contains('__') {
		variant_full.all_after_last('__')
	} else {
		variant_full
	}
	// Use full variant name for type cast - it's already qualified if needed
	// Only add current module prefix if variant has no module prefix
	mangled_variant := if variant_full.contains('__') {
		variant_full // Already has module prefix
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${variant_full}'
	} else {
		variant_full
	}
	// For nested smartcasts, transform the base expression first
	// Remove the SPECIFIC context we're applying to avoid matching it during transform
	t.remove_smartcast_for_expr(ctx.expr)
	transformed_base := t.transform_expr(original_expr)
	t.push_smartcast(ctx.expr, ctx.variant, ctx.sumtype)
	// Create: transformed_base._data._variant (using simple name for accessor)
	data_access := ast.SelectorExpr{
		lhs: transformed_base
		rhs: ast.Ident{
			name: '_data'
		}
	}
	variant_access := ast.SelectorExpr{
		lhs: data_access
		rhs: ast.Ident{
			name: '_${variant_simple}'
		}
	}

	// For primitives, cast from pointer space back to value type
	if variant_simple in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize'] {
		// Create: ((variant)(intptr_t)variant_access)
		return ast.ParenExpr{
			expr: ast.CastExpr{
				typ:  ast.Ident{
					name: variant_simple
				}
				expr: ast.CastExpr{
					typ:  ast.Ident{
						name: 'intptr_t'
					}
					expr: variant_access
				}
			}
		}
	}

	// For structs/strings, dereference the pointer
	// Create: (mangled_variant*)variant_access
	cast_expr := ast.CastExpr{
		typ:  ast.Ident{
			name: '${mangled_variant}*'
		}
		expr: variant_access
	}
	// Create: *(cast_expr) wrapped in parens for proper precedence when accessing fields
	deref_expr := ast.PrefixExpr{
		op:   token.Token.mul
		expr: cast_expr
	}
	return ast.ParenExpr{
		expr: deref_expr
	}
}

// apply_smartcast_receiver_ctx generates a cast expression for a method call receiver on a smartcast variable
// e.g., se.lhs when smartcast to SelectorExpr -> (*((ast__SelectorExpr*)se.lhs._data._SelectorExpr))
fn (mut t Transformer) apply_smartcast_receiver_ctx(sumtype_expr ast.Expr, ctx SmartcastContext) ast.Expr {
	variant_full := ctx.variant
	// Extract simple variant name for _data._ accessor (strip module prefix)
	variant_simple := if variant_full.contains('__') {
		variant_full.all_after_last('__')
	} else {
		variant_full
	}
	// Use full variant name for type cast - it's already qualified if needed
	mangled_variant := if variant_full.contains('__') {
		variant_full // Already has module prefix
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${variant_full}'
	} else {
		variant_full
	}
	// For nested smartcasts, transform the base expression first
	// Remove the SPECIFIC context we're applying to avoid matching it during transform
	t.remove_smartcast_for_expr(ctx.expr)
	transformed_base := t.transform_expr(sumtype_expr)
	t.push_smartcast(ctx.expr, ctx.variant, ctx.sumtype)
	// Create: transformed_base._data._variant (using simple name for accessor)
	data_access := ast.SelectorExpr{
		lhs: transformed_base
		rhs: ast.Ident{
			name: '_data'
		}
	}
	variant_access := ast.SelectorExpr{
		lhs: data_access
		rhs: ast.Ident{
			name: '_${variant_simple}'
		}
	}
	// Create: (mangled_variant*)variant_access
	cast_expr := ast.CastExpr{
		typ:  ast.Ident{
			name: '${mangled_variant}*'
		}
		expr: variant_access
	}
	// Create: *(cast_expr) - dereference to get the actual value
	deref_expr := ast.PrefixExpr{
		op:   token.Token.mul
		expr: cast_expr
	}
	return ast.ParenExpr{
		expr: deref_expr
	}
}

// apply_smartcast_field_access_ctx generates a cast expression for field access on a smartcast variable
// e.g., w.valera.name when smartcast to Kek -> ((ast__Kek*)w.valera._data._Kek)->name
// For nested smartcasts, we first transform the base expression to apply outer smartcasts
fn (mut t Transformer) apply_smartcast_field_access_ctx(sumtype_expr ast.Expr, field_name string, ctx SmartcastContext) ast.Expr {
	variant_full := ctx.variant
	// Extract simple variant name for _data._ accessor (strip module prefix)
	variant_simple := if variant_full.contains('__') {
		variant_full.all_after_last('__')
	} else {
		variant_full
	}
	// Use full variant name for type cast - it's already qualified if needed
	mangled_variant := if variant_full.contains('__') {
		variant_full // Already has module prefix
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${variant_full}'
	} else {
		variant_full
	}
	// For nested smartcasts, we need to transform the base of sumtype_expr to apply outer smartcasts
	// E.g., for stmt.receiver.typ with outer smartcast on stmt, we need to transform stmt.receiver first
	// Remove the SPECIFIC context we're applying, remembering its position to restore it later
	// This preserves stack order when there are nested smartcasts (e.g., if field.typ is X inside match node { Y })
	mut original_idx := 0
	if result := t.remove_smartcast_for_expr_with_idx(ctx.expr) {
		original_idx = result.idx
	}
	transformed_base := t.transform_expr(sumtype_expr)
	// Insert context back at its original position to maintain stack order
	t.insert_smartcast_at(original_idx, SmartcastContext{
		expr:    ctx.expr
		variant: ctx.variant
		sumtype: ctx.sumtype
	})
	// Create: transformed_base._data._variant (using simple name for accessor)
	data_access := ast.SelectorExpr{
		lhs: transformed_base
		rhs: ast.Ident{
			name: '_data'
		}
	}
	variant_access := ast.SelectorExpr{
		lhs: data_access
		rhs: ast.Ident{
			name: '_${variant_simple}'
		}
	}
	// Create: (mangled_variant*)variant_access
	cast_expr := ast.CastExpr{
		typ:  ast.Ident{
			name: '${mangled_variant}*'
		}
		expr: variant_access
	}
	// Create: cast_expr->field_name (cleanc will handle pointer arrow vs dot)
	return ast.SelectorExpr{
		lhs: cast_expr
		rhs: ast.Ident{
			name: field_name
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
	// Also check for [x, y, z]! syntax - parser marks this with len: PostfixExpr{op: .not}
	if expr.len is ast.PostfixExpr {
		postfix := expr.len as ast.PostfixExpr
		if postfix.op == .not && postfix.expr is ast.EmptyExpr {
			is_fixed = true
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
			// Try to get type from scope
			var_type := t.get_var_type_name(first.name)
			if var_type != '' {
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
		} else if first is ast.CallOrCastExpr {
			// Handle cast expressions like u8(`0`) - infer element type from cast type
			if first.lhs is ast.Ident {
				cast_type := first.lhs.name
				// Check if this is a primitive type cast
				if cast_type in ['u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64', 'f32', 'f64',
					'int', 'bool', 'byte', 'rune', 'voidptr', 'charptr', 'byteptr', 'usize', 'isize',
					'string'] {
					elem_type_name = cast_type
					ast.Expr(ast.Ident{
						name: cast_type
					})
				} else {
					// Could be a struct cast - use the type name
					elem_type_name = cast_type
					ast.Expr(ast.Ident{
						name: cast_type
					})
				}
			} else {
				// Default: use int
				ast.Expr(ast.Ident{
					name: 'int'
				})
			}
		} else if first is ast.CastExpr {
			// Handle explicit CastExpr nodes
			elem_type_name = t.expr_to_type_name(first.typ)
			ast.Expr(first.typ)
		} else if first is ast.IndexExpr {
			// Handle index expressions like s[i] - try to infer element type from the indexed container
			// Also handle slice expressions like s[..i] which become IndexExpr with RangeExpr
			if first.expr is ast.RangeExpr {
				// Slicing: s[a..b] returns the same type as s
				if expr_type := t.get_expr_type(first.lhs) {
					type_name := t.type_to_c_name(expr_type)
					if type_name != '' {
						elem_type_name = type_name
						ast.Expr(ast.Ident{
							name: type_name
						})
					} else {
						ast.Expr(ast.Ident{
							name: 'int'
						})
					}
				} else {
					ast.Expr(ast.Ident{
						name: 'int'
					})
				}
			} else if expr_type := t.get_expr_type(first.lhs) {
				type_name := t.type_to_c_name(expr_type)
				if type_name == 'string' {
					// String indexing returns u8
					elem_type_name = 'u8'
					ast.Expr(ast.Ident{
						name: 'u8'
					})
				} else if type_name.starts_with('Array_') {
					// Array indexing returns element type
					arr_elem := type_name[6..] // Remove 'Array_' prefix
					elem_type_name = arr_elem
					ast.Expr(ast.Ident{
						name: arr_elem
					})
				} else {
					// Default: use int
					ast.Expr(ast.Ident{
						name: 'int'
					})
				}
			} else {
				// Could not determine type - default to int
				ast.Expr(ast.Ident{
					name: 'int'
				})
			}
		} else if first is ast.CallExpr {
			// Handle function calls - try to infer return type
			if expr_type := t.get_expr_type(first) {
				type_name := t.type_to_c_name(expr_type)
				if type_name != '' {
					elem_type_name = type_name
					ast.Expr(ast.Ident{
						name: type_name
					})
				} else {
					ast.Expr(ast.Ident{
						name: 'int'
					})
				}
			} else {
				// Try to infer from function name for common patterns
				mut fn_name := ''
				if first.lhs is ast.Ident {
					fn_name = first.lhs.name
				} else if first.lhs is ast.SelectorExpr {
					fn_name = first.lhs.rhs.name
				}
				// String methods that return string
				if fn_name in ['substr', 'substr_unsafe', 'trim', 'trim_left', 'trim_right',
					'to_upper', 'to_lower', 'replace', 'reverse', 'clone', 'repeat'] {
					elem_type_name = 'string'
					ast.Expr(ast.Ident{
						name: 'string'
					})
				} else {
					ast.Expr(ast.Ident{
						name: 'int'
					})
				}
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
	// Check if matching on a sum type
	sumtype_name := t.get_sumtype_name_for_expr(expr.expr)
	smartcast_expr := t.expr_to_string(expr.expr)

	if sumtype_name != '' {
		// Sum type match - set up smartcast context for each branch
		variants := t.get_sum_type_variants(sumtype_name)

		mut branches := []ast.MatchBranch{cap: expr.branches.len}
		for branch in expr.branches {
			if branch.cond.len > 0 {
				// Get variant name from first condition
				cond0 := branch.cond[0]
				mut variant_name := ''
				mut variant_module := ''
				if cond0 is ast.Ident {
					variant_name = cond0.name
				} else if cond0 is ast.SelectorExpr {
					// Handle module-qualified types like types.Struct
					variant_name = cond0.rhs.name
					if cond0.lhs is ast.Ident {
						variant_module = (cond0.lhs as ast.Ident).name
					}
				}

				if variant_name != '' {
					// Create fully qualified variant name
					qualified_variant := if variant_module != '' {
						'${variant_module}__${variant_name}'
					} else {
						variant_name
					}

					// Push smartcast context for this branch
					t.push_smartcast(smartcast_expr, qualified_variant, sumtype_name)

					// Transform statements with smartcast context
					transformed_stmts := t.transform_stmts(branch.stmts)

					// Pop context
					t.pop_smartcast()

					// Transform condition to tag check if we found the tag value
					mut new_conds := []ast.Expr{cap: branch.cond.len}
					for c in branch.cond {
						mut c_variant_name := ''
						if c is ast.Ident {
							c_variant_name = c.name
						} else if c is ast.SelectorExpr {
							c_variant_name = c.rhs.name
						}

						// Find tag for this condition
						mut c_tag := -1
						for i, v in variants {
							v_short := if v.contains('__') { v.all_after_last('__') } else { v }
							if v == c_variant_name || v_short == c_variant_name {
								c_tag = i
								break
							}
						}

						if c_tag >= 0 {
							// Transform to tag comparison
							new_conds << ast.BasicLiteral{
								kind:  token.Token.number
								value: '${c_tag}'
							}
						} else {
							// Keep original (shouldn't happen for valid code)
							new_conds << c
						}
					}

					branches << ast.MatchBranch{
						cond:  new_conds
						stmts: transformed_stmts
						pos:   branch.pos
					}
				} else {
					// No variant name found, just transform normally
					branches << ast.MatchBranch{
						cond:  branch.cond
						stmts: t.transform_stmts(branch.stmts)
						pos:   branch.pos
					}
				}
			} else {
				// else branch - no smartcast context
				branches << ast.MatchBranch{
					cond:  branch.cond
					stmts: t.transform_stmts(branch.stmts)
					pos:   branch.pos
				}
			}
		}

		// Transform match expression to use _tag field
		// IMPORTANT: Remove ALL smartcast contexts for this expression
		// to prevent incorrect casting. We need to access the sum type's _tag,
		// not the smartcast result's _tag.
		mut removed_contexts := []SmartcastContext{}
		for {
			existing_ctx := t.remove_smartcast_for_expr(smartcast_expr)
			if existing_ctx != none {
				removed_contexts << existing_ctx
			} else {
				break
			}
		}
		transformed_match_expr := t.transform_expr(expr.expr)
		// Re-add the contexts in reverse order (to preserve original order)
		for i := removed_contexts.len - 1; i >= 0; i-- {
			ctx := removed_contexts[i]
			t.push_smartcast(ctx.expr, ctx.variant, ctx.sumtype)
		}
		tag_access := ast.SelectorExpr{
			lhs: transformed_match_expr
			rhs: ast.Ident{
				name: '_tag'
			}
		}

		return ast.MatchExpr{
			expr:     tag_access
			branches: branches
			pos:      expr.pos
		}
	}

	// Non-sum type match - simple transformation
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
	// Get the struct type name for field type lookups
	struct_type_name := t.get_init_expr_type_name(expr.typ)

	// Transform field values recursively
	// Note: ArrayInitExpr is NOT transformed here because cleanc uses field type info
	// to determine if it's a fixed-size array (which transformer doesn't have access to)
	mut fields := []ast.FieldInit{cap: expr.fields.len}
	for field in expr.fields {
		// Check if this field is a sum type and needs wrapping
		field_type_name := t.get_struct_field_type_name(struct_type_name, field.name)
		if t.is_sum_type(field_type_name) {
			// This is a sum type field - wrap the value in sum type initialization
			if wrapped := t.wrap_sumtype_value(field.value, field_type_name) {
				fields << ast.FieldInit{
					name:  field.name
					value: wrapped
				}
				continue
			}
		}

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
	// The Error struct itself uses Error wrappers
	if base_name == 'Error' {
		return 'Error'
	}
	// Check if this type has its own msg() method using the type environment
	// Types with custom msg() need their own wrapper; types without use Error's wrapper
	if t.env.lookup_method(type_name, 'msg') != none {
		// Has custom msg() method - use full type name for wrapper
		return type_name
	}
	// No custom msg() method - use Error's wrapper
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

// is_error_type_name checks if a type implements IError
// This includes types that embed Error OR types that have msg() method
fn (t &Transformer) is_error_type_name(type_name string) bool {
	// Get base name (strip module prefix if present)
	base_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// The Error struct itself is the base error type
	if base_name == 'Error' {
		return true
	}
	// Look up the type and check if it embeds Error
	typ := t.lookup_type(type_name) or {
		// If type lookup fails, check if it has msg() method (implements IError)
		if t.env.lookup_method(type_name, 'msg') != none {
			return true
		}
		return false
	}
	if typ is types.Struct {
		for embedded in typ.embedded {
			if embedded.name == 'Error' || embedded.name.ends_with('.Error') {
				return true
			}
		}
	}
	// Also check if the type has msg() method (implements IError directly)
	if t.env.lookup_method(type_name, 'msg') != none {
		return true
	}
	return false
}

fn (mut t Transformer) transform_if_expr(expr ast.IfExpr) ast.Expr {
	// Check for compound && condition with is check: if x is Type && cond { ... }
	// Transform to nested ifs: if x is Type { if cond { ... } }
	// This allows the smart cast context to be active for the inner condition
	if expr.cond is ast.InfixExpr {
		cond := expr.cond as ast.InfixExpr
		if cond.op == .and {
			// Check if LHS is an is-check
			if cond.lhs is ast.InfixExpr {
				lhs_infix := cond.lhs as ast.InfixExpr
				if lhs_infix.op == .key_is {
					// Transform: if x is Type && rest { body } else { else_body }
					// To: if x is Type { if rest { body } else { else_body } } else { else_body }
					// The else_expr goes on both outer and inner ifs to preserve semantics:
					// - If outer is-check fails -> else
					// - If inner condition fails -> else
					inner_if := ast.IfExpr{
						cond:      cond.rhs
						stmts:     expr.stmts
						else_expr: expr.else_expr
						pos:       expr.pos
					}
					outer_if := ast.IfExpr{
						cond:      cond.lhs
						stmts:     [ast.Stmt(ast.ExprStmt{
							expr: inner_if
						})]
						else_expr: expr.else_expr // else also on outer if
						pos:       expr.pos
					}
					// Recursively transform - outer will handle smartcast
					return t.transform_if_expr(outer_if)
				}
			}
			// Check if RHS is an is-check: if cond && x is Type { ... }
			// Transform to: if cond { if x is Type { ... } else { else_body } } else { else_body }
			if cond.rhs is ast.InfixExpr {
				rhs_infix := cond.rhs as ast.InfixExpr
				if rhs_infix.op == .key_is {
					// Transform: if cond && x is Type { body } else { else_body }
					// To: if cond { if x is Type { body } else { else_body } } else { else_body }
					inner_if := ast.IfExpr{
						cond:      cond.rhs
						stmts:     expr.stmts
						else_expr: expr.else_expr
						pos:       expr.pos
					}
					outer_if := ast.IfExpr{
						cond:      cond.lhs
						stmts:     [ast.Stmt(ast.ExprStmt{
							expr: inner_if
						})]
						else_expr: expr.else_expr
						pos:       expr.pos
					}
					// Recursively transform - inner will handle smartcast
					return t.transform_if_expr(outer_if)
				}
			}
		}
	}

	// Check for sum type smart cast: if x is Type { ... }
	if expr.cond is ast.InfixExpr {
		cond := expr.cond as ast.InfixExpr
		if cond.op == .key_is {
			// Get the variant type name from RHS
			// Also extract module for qualified types like types.Type
			mut variant_name := ''
			mut variant_module := ''
			if cond.rhs is ast.Ident {
				variant_name = (cond.rhs as ast.Ident).name
			} else if cond.rhs is ast.SelectorExpr {
				// Handle module-qualified types like types.Type
				sel := cond.rhs as ast.SelectorExpr
				variant_name = sel.rhs.name
				// Extract module name (e.g., from types.Type, extract "types")
				if sel.lhs is ast.Ident {
					variant_module = (sel.lhs as ast.Ident).name
				}
			}
			if variant_name != '' {
				// Get the sum type name from LHS type
				// First try normal lookup, then fall back to finding sumtype by variant
				mut sumtype_name := t.get_sumtype_name_for_expr(cond.lhs)

				// If that failed or returned wrong sumtype, try finding by variant
				if sumtype_name == '' {
					sumtype_name = t.find_sumtype_for_variant(variant_name)
				}

				if sumtype_name != '' {
					// Find the tag value for this variant
					variants := t.get_sum_type_variants(sumtype_name)
					mut tag_value := -1
					for i, v in variants {
						// Compare both qualified and unqualified names
						// v might be "ast__SelectorExpr", variant_name might be "SelectorExpr"
						v_short := if v.contains('__') { v.all_after_last('__') } else { v }
						if v == variant_name || v_short == variant_name {
							tag_value = i
							break
						}
					}

					if tag_value >= 0 {
						// Get the string representation of the LHS expression
						smartcast_expr := t.expr_to_string(cond.lhs)

						// Create fully qualified variant name if module is specified
						// e.g., types.Type -> types__Type
						qualified_variant := if variant_module != '' {
							'${variant_module}__${variant_name}'
						} else {
							variant_name
						}

						// Transform cond.lhs BEFORE pushing the inner smartcast context
						// IMPORTANT: Remove ALL smartcast contexts for this expression
						// to prevent incorrect casting. The is-check's LHS should access
						// the sum type's _tag, not the smartcast result.
						mut removed_contexts := []SmartcastContext{}
						for {
							existing_ctx := t.remove_smartcast_for_expr(smartcast_expr)
							if existing_ctx != none {
								removed_contexts << existing_ctx
							} else {
								break
							}
						}
						transformed_lhs := t.transform_expr(cond.lhs)
						// Re-add the contexts in reverse order (to preserve original order)
						for i := removed_contexts.len - 1; i >= 0; i-- {
							ctx := removed_contexts[i]
							t.push_smartcast(ctx.expr, ctx.variant, ctx.sumtype)
						}

						// Push smart cast context for transforming body (supports nested smartcasts)
						t.push_smartcast(smartcast_expr, qualified_variant, sumtype_name)

						// Transform body with smart cast context
						transformed_stmts := t.transform_stmts(expr.stmts)

						// Pop context
						t.pop_smartcast()

						// Transform condition: v is Type -> v._tag == TAG_VALUE
						// This prevents cleanc from also applying smart cast
						// Use transformed_lhs to apply outer smartcasts to the tag check
						tag_check := ast.InfixExpr{
							op:  token.Token.eq
							lhs: ast.SelectorExpr{
								lhs: transformed_lhs
								rhs: ast.Ident{
									name: '_tag'
								}
							}
							rhs: ast.BasicLiteral{
								kind:  token.Token.number
								value: '${tag_value}'
							}
							pos: cond.pos
						}

						// Transform else_expr (may have its own smart cast context)
						return ast.IfExpr{
							cond:      tag_check
							stmts:     transformed_stmts
							else_expr: t.transform_expr(expr.else_expr)
							pos:       expr.pos
						}
					}
				}
			}
		}
	}

	// Handle if-guard expression in condition (for nested/expression-level if-guards)
	// For Option-returning expressions in if-guards, we need to expand them with temp variables.
	// Transform: if r := opt_func() { body } else { else_body }
	// To: { _tmp := opt_func(); if _tmp.state == 0 { r := _tmp.data; body } else { else_body } }
	if expr.cond is ast.IfGuardExpr {
		guard := expr.cond as ast.IfGuardExpr
		if guard.stmt.rhs.len > 0 {
			synth_pos := t.next_synth_pos()
			rhs := guard.stmt.rhs[0]

			// Check if RHS returns Option type
			mut is_option := t.expr_returns_option(rhs)
			if !is_option {
				fn_name := t.get_call_fn_name(rhs)
				is_option = fn_name != '' && t.fn_returns_option(fn_name)
			}

			if is_option {
				// Handle Option if-guard by expanding inline
				// Transform: if var := opt_call() { body }
				// To: if opt_call().state == 0 { var := opt_call().data; body }
				// Note: This calls the function twice, but is safe for method calls

				mut is_blank := false
				if guard.stmt.lhs.len == 1 && guard.stmt.lhs[0] is ast.Ident {
					if (guard.stmt.lhs[0] as ast.Ident).name == '_' {
						is_blank = true
					}
				}

				// Generate condition: call().state == 0
				call_state_check := ast.InfixExpr{
					op:  .eq
					lhs: ast.SelectorExpr{
						lhs: t.transform_expr(rhs)
						rhs: ast.Ident{
							name: 'state'
						}
					}
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}

				mut body_stmts := []ast.Stmt{}
				if !is_blank {
					// var := call().data - note: calls RHS again
					data_access := ast.SelectorExpr{
						lhs: t.transform_expr(rhs)
						rhs: ast.Ident{
							name: 'data'
						}
					}
					body_stmts << ast.AssignStmt{
						op:  .decl_assign
						lhs: guard.stmt.lhs
						rhs: [ast.Expr(data_access)]
						pos: guard.stmt.pos
					}
				}
				for s in expr.stmts {
					body_stmts << s
				}

				return ast.IfExpr{
					cond:      call_state_check
					stmts:     t.transform_stmts(body_stmts)
					else_expr: t.transform_expr(expr.else_expr)
					pos:       synth_pos
				}
			}

			// Non-option case: use simple transformation
			// For map if-guards: if r := map[key] { body } else { else_body }
			// Transform to: if (key in map) { r := map[key]; body } else { else_body }
			// For other cases: if (rhs) { r := rhs; body } else { else_body }
			mut is_blank := false
			if guard.stmt.lhs.len == 1 && guard.stmt.lhs[0] is ast.Ident {
				if (guard.stmt.lhs[0] as ast.Ident).name == '_' {
					is_blank = true
				}
			}

			// Check if RHS is a map index expression
			mut cond_expr := ast.Expr(t.transform_expr(rhs))
			if rhs is ast.IndexExpr {
				// Try to see if this is a map index
				if _ := t.get_map_type_for_expr(rhs.lhs) {
					// This is a map access - generate "key in map" check
					cond_expr = ast.InfixExpr{
						op:  .key_in
						lhs: rhs.expr // the key expression
						rhs: rhs.lhs  // the map expression
						pos: rhs.pos
					}
				}
			}

			mut new_stmts := []ast.Stmt{cap: expr.stmts.len + 1}
			if !is_blank {
				guard_assign := ast.AssignStmt{
					op:  .decl_assign
					lhs: guard.stmt.lhs
					rhs: guard.stmt.rhs
					pos: guard.stmt.pos
				}
				new_stmts << guard_assign
			}
			for s in expr.stmts {
				new_stmts << s
			}
			return ast.IfExpr{
				cond:      t.transform_expr(cond_expr)
				stmts:     t.transform_stmts(new_stmts)
				else_expr: t.transform_expr(expr.else_expr)
				pos:       synth_pos
			}
		}
	}

	// Default transformation
	return ast.IfExpr{
		cond:      t.transform_expr(expr.cond)
		stmts:     t.transform_stmts(expr.stmts)
		else_expr: t.transform_expr(expr.else_expr)
		pos:       expr.pos
	}
}

// get_sumtype_name_for_expr returns the sum type name for an expression, or empty string if not a sum type
// This function is smartcast-aware: if the expression is already smartcasted to a variant that is
// itself a sum type, it returns that sum type name.
fn (t &Transformer) get_sumtype_name_for_expr(expr ast.Expr) string {
	// First, check if this expression is currently smartcasted
	// This handles nested smartcasts like: if x.y is Type { if x.y is SubType { ... } }
	// where after the first smartcast, x.y is Type, and Type itself is a sum type
	expr_str := t.expr_to_string(expr)
	if expr_str != '' {
		if ctx := t.find_smartcast_for_expr(expr_str) {
			// The expression is smartcasted - use the variant type
			// Check if the variant is itself a sum type (for nested smartcasts)
			variant_name := ctx.variant
			// Try with and without module prefix
			if t.is_sum_type(variant_name) {
				return variant_name
			}
			// Try stripping module prefix
			variant_short := if variant_name.contains('__') {
				variant_name.all_after_last('__')
			} else {
				variant_name
			}
			if t.is_sum_type(variant_short) {
				return variant_short
			}
		}
	}

	// Look up variable type from scope
	mut type_name := if expr is ast.Ident {
		t.get_var_type_name(expr.name)
	} else if expr is ast.SelectorExpr {
		t.get_selector_type_name(expr)
	} else {
		''
	}

	// If scope lookup failed, try to get the type from the expression's position
	// This handles loop variables and other cases where the scope doesn't have the type
	if type_name == '' {
		if typ := t.env.get_expr_type(expr.pos()) {
			type_name = typ.name()
		}
	}

	if type_name != '' && t.is_sum_type(type_name) {
		return type_name
	}
	return ''
}

// expr_to_string converts an expression to its string representation for smart cast matching
fn (t &Transformer) expr_to_string(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.SelectorExpr {
		lhs_str := t.expr_to_string(expr.lhs)
		return '${lhs_str}.${expr.rhs.name}'
	}
	if expr is ast.ParenExpr {
		return t.expr_to_string(expr.expr)
	}
	return ''
}

// get_struct_field_type_name returns the type name of a field in a struct
fn (t &Transformer) get_struct_field_type_name(struct_name string, field_name string) string {
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

// wrap_sumtype_value wraps a value in sum type initialization if needed
// Returns the wrapped expression or none if the value type couldn't be determined
fn (mut t Transformer) wrap_sumtype_value(value ast.Expr, sumtype_name string) ?ast.Expr {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}

	// Determine the variant type from the value
	variant_name := t.infer_variant_type(value, variants)
	if variant_name == '' {
		return none
	}

	// Find the tag value for this variant
	mut tag_value := -1
	for i, v in variants {
		if v == variant_name {
			tag_value = i
			break
		}
	}
	if tag_value < 0 {
		return none
	}

	// Transform the value
	transformed_value := t.transform_expr(value)

	// Create: SumType{_tag: N, _data._variant: (void*)...}
	// For primitives: (void*)(intptr_t)value - stores value in pointer space
	// For structs/strings: (void*)&value - stores pointer to value
	boxed_value := if variant_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
		'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize'] {
		// Primitive - use (void*)(intptr_t) cast to store value in pointer space
		ast.Expr(ast.CastExpr{
			typ:  ast.Ident{
				name: 'voidptr'
			}
			expr: ast.CastExpr{
				typ:  ast.Ident{
					name: 'intptr_t'
				}
				expr: transformed_value
			}
		})
	} else {
		// Struct or string - take address and cast to void*
		ast.Expr(ast.CastExpr{
			typ:  ast.Ident{
				name: 'voidptr'
			}
			expr: ast.PrefixExpr{
				op:   token.Token.amp
				expr: transformed_value
			}
		})
	}

	// Create the sum type initialization with _data._variant field name
	// This generates: (SumType){._tag = N, ._data._variant = (void*)...}
	// Use short variant name for the C field (e.g., "InfixExpr" not "ast__InfixExpr")
	short_variant := if variant_name.contains('__') {
		variant_name.all_after_last('__')
	} else {
		variant_name
	}
	return ast.InitExpr{
		typ:    ast.Ident{
			name: sumtype_name
		}
		fields: [
			ast.FieldInit{
				name:  '_tag'
				value: ast.BasicLiteral{
					kind:  token.Token.number
					value: '${tag_value}'
				}
			},
			ast.FieldInit{
				name:  '_data._${short_variant}'
				value: boxed_value
			},
		]
	}
}

// infer_variant_type determines which variant of a sum type a value belongs to
fn (t &Transformer) infer_variant_type(value ast.Expr, variants []string) string {
	// Check based on expression type
	if value is ast.BasicLiteral {
		if value.kind == .number {
			// Could be int or float - check for decimal point
			if value.value.contains('.') {
				if 'f64' in variants {
					return 'f64'
				}
				if 'f32' in variants {
					return 'f32'
				}
			}
			if 'int' in variants {
				return 'int'
			}
			if 'i64' in variants {
				return 'i64'
			}
		}
		if value.kind == .string {
			if 'string' in variants {
				return 'string'
			}
		}
	}
	if value is ast.StringLiteral || value is ast.StringInterLiteral {
		if 'string' in variants {
			return 'string'
		}
	}
	if value is ast.InitExpr {
		// Struct initialization - get the struct type name
		type_name := t.get_init_expr_type_name(value.typ)
		if type_name in variants {
			return type_name
		}
		// Check with current module prefix (e.g., "InfixExpr" -> "ast__InfixExpr")
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
			mangled := '${t.cur_module}__${type_name}'
			if mangled in variants {
				return mangled
			}
		}
		// Check if any variant ends with __type_name (for imported types)
		for v in variants {
			if v.ends_with('__${type_name}') {
				return v
			}
		}
	}
	if value is ast.Ident {
		// Check scope for the variable type
		var_type := t.get_var_type_name(value.name)
		if var_type != '' && var_type in variants {
			return var_type
		}
	}
	if value is ast.CallExpr {
		// Check function return type using scope lookup
		fn_name := if value.lhs is ast.Ident {
			(value.lhs as ast.Ident).name
		} else {
			''
		}
		if fn_name != '' {
			if ret_type_obj := t.get_fn_return_type(fn_name) {
				ret_type := ret_type_obj.name()
				if ret_type in variants {
					return ret_type
				}
			}
		}
	}
	return ''
}

fn (mut t Transformer) transform_infix_expr(expr ast.InfixExpr) ast.Expr {
	// Check for string concatenation: string + string
	if expr.op == .plus {
		lhs_is_str := t.is_string_expr(expr.lhs)
		rhs_is_str := t.is_string_expr(expr.rhs)

		// Check if either side is a string literal
		lhs_is_str_literal := if expr.lhs is ast.StringLiteral {
			true
		} else if expr.lhs is ast.BasicLiteral {
			expr.lhs.kind == .string
		} else {
			false
		}
		rhs_is_str_literal := if expr.rhs is ast.StringLiteral {
			true
		} else if expr.rhs is ast.BasicLiteral {
			expr.rhs.kind == .string
		} else {
			false
		}

		// Also check if either side is a string__* call (already transformed)
		lhs_is_str_call := if expr.lhs is ast.CallExpr {
			if expr.lhs.lhs is ast.Ident {
				(expr.lhs.lhs as ast.Ident).name.starts_with('string__')
			} else {
				false
			}
		} else {
			false
		}
		rhs_is_str_call := if expr.rhs is ast.CallExpr {
			if expr.rhs.lhs is ast.Ident {
				(expr.rhs.lhs as ast.Ident).name.starts_with('string__')
			} else {
				false
			}
		} else {
			false
		}

		// Also check for string InfixExpr (chained concatenation like s1 + s2 + s3)
		lhs_is_str_infix := expr.lhs is ast.InfixExpr
			&& (expr.lhs as ast.InfixExpr).op == .plus && lhs_is_str
		rhs_is_str_infix := expr.rhs is ast.InfixExpr
			&& (expr.rhs as ast.InfixExpr).op == .plus && rhs_is_str
		// Determine if this is a string concatenation using multiple signals
		should_transform := (lhs_is_str && rhs_is_str) || (lhs_is_str_literal && (rhs_is_str || expr.rhs is ast.Ident || rhs_is_str_call)) || (rhs_is_str_literal && (lhs_is_str || expr.lhs is ast.Ident || lhs_is_str_call)) || (lhs_is_str_call && (rhs_is_str || expr.rhs is ast.Ident)) || (rhs_is_str_call && (lhs_is_str || expr.lhs is ast.Ident)) || (lhs_is_str_infix && expr.rhs is ast.Ident) // Chained: (s1 + s2) + ident
		 || (rhs_is_str_infix && expr.lhs is ast.Ident) // Chained: ident + (s1 + s2)

		// Check for chained concatenation: (s1 + s2) + s3 -> string__plus_two(s1, s2, s3)
		if expr.lhs is ast.InfixExpr && should_transform {
			lhs_infix := expr.lhs as ast.InfixExpr
			if lhs_infix.op == .plus && t.is_string_expr(lhs_infix.lhs)
				&& t.is_string_expr(lhs_infix.rhs) {
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
		if should_transform {
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
			// Check if this is a sum type variant check: sumtype_var in [TypeA, TypeB]
			// If LHS is a sum type and RHS contains variant type names, let cleanc handle it
			mut is_sumtype_variant_check := false
			if expr.rhs is ast.ArrayInitExpr {
				// Get LHS sum type name (if it is a sum type)
				lhs_sumtype := t.get_sumtype_name_for_expr(expr.lhs)
				if lhs_sumtype != '' {
					// Check if the array elements are variant type names (Idents)
					arr := expr.rhs as ast.ArrayInitExpr
					if arr.exprs.len > 0 && arr.exprs[0] is ast.Ident {
						// This looks like a sum type variant check
						// Verify that the ident names are actually variants
						variants := t.get_sum_type_variants(lhs_sumtype)
						first_ident := arr.exprs[0] as ast.Ident
						// Extract module prefix from sum type name (e.g., "ast__Expr" -> "ast__")
						mod_prefix := if lhs_sumtype.contains('__') {
							lhs_sumtype.all_before_last('__') + '__'
						} else {
							''
						}
						// Check both with and without module prefix
						variant_name := first_ident.name
						variant_mangled := mod_prefix + variant_name
						if variant_name in variants || variant_mangled in variants {
							is_sumtype_variant_check = true
						}
					}
				}
			}
			// If this is a sum type variant check, skip transformation entirely
			// and return the original expression (cleanc will handle tag checks)
			if is_sumtype_variant_check {
				// Return unchanged - cleanc will generate the tag check
				return ast.InfixExpr{
					op:  expr.op
					lhs: t.transform_expr(expr.lhs)
					rhs: expr.rhs // Don't transform RHS (keep as ArrayInitExpr)
					pos: expr.pos
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
			} else {
				// Unresolved shorthand - return as-is for cleanc to handle expansion
				return ast.InfixExpr{
					op:  expr.op
					lhs: t.transform_expr(expr.lhs)
					rhs: expr.rhs // Keep RHS as-is (ArrayInitExpr with shorthand)
					pos: expr.pos
				}
			}
		}
	}
	// Check for array append: arr << elem => builtin__array_push_noscan((array*)&arr, _MOV((T[]){ elem }))
	// If RHS is also an array, use push_many instead
	// Note: map[key] << value is handled directly in cleanc, not here
	if expr.op == .left_shift {
		// Skip transformation if LHS is a map index - cleanc handles this pattern directly
		if expr.lhs is ast.IndexExpr {
			index_expr := expr.lhs as ast.IndexExpr
			if map_type := t.get_expr_type(index_expr.lhs) {
				if map_type is types.Map {
					if map_type.value_type is types.Array {
						// Let cleanc handle map[key] << value pattern
						return ast.InfixExpr{
							lhs: t.transform_expr(expr.lhs)
							op:  expr.op
							rhs: t.transform_expr(expr.rhs)
							pos: expr.pos
						}
					}
				}
			}
		}

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
	// Check for enum shorthand in comparisons: x.op == .amp -> x.op == Token.amp
	if expr.op in [.eq, .ne] {
		// Check if RHS is enum shorthand (.member with empty LHS)
		if expr.rhs is ast.SelectorExpr {
			rhs_sel := expr.rhs as ast.SelectorExpr
			if rhs_sel.lhs is ast.EmptyExpr {
				// RHS is enum shorthand - resolve using LHS type
				enum_type := t.get_enum_type_name(expr.lhs)
				if enum_type != '' {
					resolved_rhs := t.resolve_enum_shorthand(expr.rhs, enum_type)
					return ast.InfixExpr{
						op:  expr.op
						lhs: t.transform_expr(expr.lhs)
						rhs: t.transform_expr(resolved_rhs)
						pos: expr.pos
					}
				}
			}
		}
		// Check if LHS is enum shorthand (.member with empty LHS)
		if expr.lhs is ast.SelectorExpr {
			lhs_sel := expr.lhs as ast.SelectorExpr
			if lhs_sel.lhs is ast.EmptyExpr {
				// LHS is enum shorthand - resolve using RHS type
				enum_type := t.get_enum_type_name(expr.rhs)
				if enum_type != '' {
					resolved_lhs := t.resolve_enum_shorthand(expr.lhs, enum_type)
					return ast.InfixExpr{
						op:  expr.op
						lhs: t.transform_expr(resolved_lhs)
						rhs: t.transform_expr(expr.rhs)
						pos: expr.pos
					}
				}
			}
		}
	}
	// Check for string comparisons: s1 == s2, s1 < s2, etc.
	if expr.op in [.eq, .ne, .lt, .gt, .le, .ge] {
		mut lhs_is_str := t.is_string_expr(expr.lhs)
		mut rhs_is_str := t.is_string_expr(expr.rhs)
		// Also check type environment for expression types if is_string_expr didn't find it
		if !lhs_is_str {
			if expr_type := t.get_expr_type(expr.lhs) {
				type_name := t.type_to_c_name(expr_type)
				if type_name == 'string' {
					lhs_is_str = true
				}
			}
		}
		if !rhs_is_str {
			if expr_type := t.get_expr_type(expr.rhs) {
				type_name := t.type_to_c_name(expr_type)
				if type_name == 'string' {
					rhs_is_str = true
				}
			}
		}
		// If one side is a string literal and the other is unknown (but likely a string),
		// treat as string comparison. Only do this for string literals, not other string expressions.
		lhs_is_str_literal := if expr.lhs is ast.StringLiteral {
			true
		} else if expr.lhs is ast.BasicLiteral {
			expr.lhs.kind == .string
		} else {
			false
		}
		rhs_is_str_literal := if expr.rhs is ast.StringLiteral {
			true
		} else if expr.rhs is ast.BasicLiteral {
			expr.rhs.kind == .string
		} else {
			false
		}
		// Only infer string comparison if at least one side is a string literal AND
		// the other is identified as string OR is an ident (could be loop variable)
		// Also transform if both are Ident and at least one is known to be string
		// (the other is likely also string in a comparison context)
		both_are_ident := expr.lhs is ast.Ident && expr.rhs is ast.Ident
		should_transform := (lhs_is_str && rhs_is_str)
			|| (lhs_is_str_literal && (rhs_is_str || expr.rhs is ast.Ident))
			|| (rhs_is_str_literal && (lhs_is_str || expr.lhs is ast.Ident))
			|| (both_are_ident && (lhs_is_str || rhs_is_str))
		if should_transform {
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
	// Check for struct operator overloading (e.g., time.Time - time.Time)
	// This transforms t1 - t2 into time__Time__minus(t1, t2) for structs with operator overloading
	// Only applies to specific known struct types that define operator methods
	if expr.op in [.plus, .minus, .mul, .div, .mod] {
		if lhs_type := t.get_expr_type(expr.lhs) {
			type_name := t.type_to_c_name(lhs_type)
			// Only transform known struct types with operator overloading
			// Note: Don't include primitive types, pointer types, or type aliases like Duration (i64)
			known_struct_ops := ['time__Time']
			if type_name in known_struct_ops {
				// Determine operator method name
				op_name := match expr.op {
					.plus { '__plus' }
					.minus { '__minus' }
					.mul { '__mul' }
					.div { '__div' }
					.mod { '__mod' }
					else { '' }
				}
				if op_name != '' {
					// Generate function call: Type__op(lhs, rhs)
					fn_name := '${type_name}${op_name}'
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: fn_name
						}
						args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
						pos:  expr.pos
					}
				}
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
			if t.is_flag_enum(receiver_type) {
				// Transform the method call
				return t.transform_flag_enum_method(sel.lhs, method_name, expr.args, receiver_type)
			}
		}
		// Check for smart-casted method call: se.lhs.method() when se.lhs is smartcast to Type
		if t.has_active_smartcast() {
			receiver_str := t.expr_to_string(sel.lhs)
			if ctx := t.find_smartcast_for_expr(receiver_str) {
				// Transform receiver with smart cast and keep the method call structure
				casted_receiver := t.apply_smartcast_receiver_ctx(sel.lhs, ctx)
				mut args := []ast.Expr{cap: expr.args.len}
				for arg in expr.args {
					args << t.transform_expr(arg)
				}
				return ast.CallExpr{
					lhs:  ast.SelectorExpr{
						lhs: casted_receiver
						rhs: sel.rhs
						pos: sel.pos
					}
					args: args
					pos:  expr.pos
				}
			}
		}
		// Check for interface method call: iface.method(args...)
		// Transform to: iface.method(iface._object, args...)
		if t.is_interface_receiver(sel.lhs) {
			// Transform interface method call to vtable dispatch
			// Prepend iface._object to the args list
			mut new_args := []ast.Expr{cap: expr.args.len + 1}
			new_args << ast.SelectorExpr{
				lhs: sel.lhs
				rhs: ast.Ident{
					name: '_object'
				}
			}
			for arg in expr.args {
				new_args << t.transform_expr(arg)
			}
			return ast.CallExpr{
				lhs:  expr.lhs // Keep the selector: iface.method
				args: new_args
				pos:  expr.pos
			}
		}
	}
	// Check for println/eprintln with non-string argument
	// Transform: println(arr) -> println(Array_int_str(arr))
	if expr.lhs is ast.Ident {
		fn_name := expr.lhs.name
		if fn_name in ['println', 'eprintln', 'print', 'eprint'] && expr.args.len == 1 {
			arg := expr.args[0]
			if !t.is_string_expr(arg) {
				// Get the str function name for the argument type
				if str_fn_name := t.get_str_fn_name_for_expr(arg) {
					// Transform to println(Type_str(arg))
					return ast.CallExpr{
						lhs:  expr.lhs
						args: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Ident{
									name: str_fn_name
								}
								args: [t.transform_expr(arg)]
								pos:  expr.pos
							}),
						]
						pos:  expr.pos
					}
				}
			}
		}
	}
	// Default: transform arguments and lhs recursively
	// This is important for smart cast propagation through method chains
	// e.g., stmt.name.replace() when stmt is smartcast
	mut args := []ast.Expr{cap: expr.args.len}
	for arg in expr.args {
		args << t.transform_expr(arg)
	}
	return ast.CallExpr{
		lhs:  t.transform_expr(expr.lhs)
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
			if t.is_flag_enum(receiver_type) {
				// Transform the method call
				return t.transform_flag_enum_method(sel.lhs, method_name, [expr.expr],
					receiver_type)
			}
		}
		// Check for smart-casted method call: se.lhs.method() when se.lhs is smartcast to Type
		if t.has_active_smartcast() {
			receiver_str := t.expr_to_string(sel.lhs)
			if ctx := t.find_smartcast_for_expr(receiver_str) {
				// Transform receiver with smart cast and keep the method call structure
				casted_receiver := t.apply_smartcast_receiver_ctx(sel.lhs, ctx)
				return ast.CallOrCastExpr{
					lhs:  ast.SelectorExpr{
						lhs: casted_receiver
						rhs: sel.rhs
						pos: sel.pos
					}
					expr: t.transform_expr(expr.expr)
					pos:  expr.pos
				}
			}
		}
		// Check for interface method call: iface.method(arg)
		// Transform to: iface.method(iface._object, arg) as CallExpr
		if t.is_interface_receiver(sel.lhs) {
			// Transform interface method call to vtable dispatch
			return ast.CallExpr{
				lhs:  expr.lhs // Keep the selector: iface.method
				args: [
					ast.Expr(ast.SelectorExpr{
						lhs: sel.lhs
						rhs: ast.Ident{
							name: '_object'
						}
					}),
					t.transform_expr(expr.expr),
				]
				pos:  expr.pos
			}
		}
	}
	// Check for println/eprintln with non-string argument
	// Transform: println(arr) -> println(Array_int_str(arr))
	if expr.lhs is ast.Ident {
		fn_name := expr.lhs.name
		if fn_name in ['println', 'eprintln', 'print', 'eprint'] {
			arg := expr.expr
			if !t.is_string_expr(arg) {
				// Get the str function name and record it for generation
				str_fn_info := t.get_str_fn_info_for_expr(arg)
				if str_fn_info.str_fn_name != '' {
					// Record needed str function for later generation
					t.needed_str_fns[str_fn_info.str_fn_name] = str_fn_info.elem_type
					// Transform to println(Type_str(arg)) - use CallExpr for proper function call syntax
					return ast.CallExpr{
						lhs:  expr.lhs
						args: [
							ast.Expr(ast.CallExpr{
								lhs:  ast.Ident{
									name: str_fn_info.str_fn_name
								}
								args: [t.transform_expr(arg)]
								pos:  expr.pos
							}),
						]
						pos:  expr.pos
					}
				}
			}
		}
	}
	// Default: transform lhs and expression recursively
	// This is important for smart cast propagation through method chains
	return ast.CallOrCastExpr{
		lhs:  t.transform_expr(expr.lhs)
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
			if t.is_flag_enum(lhs_name) {
				return lhs_name
			}
			// Check if lhs is a variable and rhs is a field (e.g., a.flags)
			// Look up the variable type and find the field type
			field_type := t.resolve_field_type(lhs_name, sel.rhs.name)
			if t.is_flag_enum(field_type) {
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
				if t.is_flag_enum(field_type) {
					return field_type
				}
			}
		}
	}
	// For Ident (variable), check if it's an enum type via scope lookup
	if expr is ast.Ident {
		ident := expr as ast.Ident
		if enum_type := t.is_var_enum(ident.name) {
			return enum_type
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
	// First, check if variable is already an enum type
	if _ := t.is_var_enum(var_name) {
		// Variable is already an enum, no field access needed
		return ''
	}

	// Check if variable is smartcasted - use the smartcast variant type
	if ctx := t.find_smartcast_for_expr(var_name) {
		return t.resolve_struct_field_type(ctx.variant, field_name)
	}

	// Look up variable type from scope
	var_type_name := t.get_var_type_name(var_name)
	$if debug ? {
		if field_name == 'flags' {
			eprintln('DEBUG: resolve_field_type(${var_name}, ${field_name}) var_type_name=${var_type_name}')
		}
	}
	if var_type_name != '' {
		// Strip pointer prefix/suffix for struct lookup
		mut clean_type := var_type_name
		if clean_type.starts_with('&') {
			clean_type = clean_type[1..]
		}
		if clean_type.ends_with('*') {
			clean_type = clean_type[..clean_type.len - 1]
		}
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
	// Handle qualified names like "ast__SelectorExpr" - extract module and type name
	mut lookup_name := struct_name
	mut lookup_module := ''
	if struct_name.contains('__') {
		parts := struct_name.split('__')
		if parts.len >= 2 {
			lookup_module = parts[0]
			lookup_name = parts[parts.len - 1]
		}
	}

	lock t.env.scopes {
		for scope_name, scope in t.env.scopes {
			// Try the lookup name directly in the appropriate module scope
			if lookup_module != '' && scope_name == lookup_module {
				// Look in the module scope
				if obj := scope.objects[lookup_name] {
					if obj is types.Type {
						return t.get_field_type_name(obj, field_name)
					}
				}
				// Also try fully qualified name
				if obj := scope.objects[struct_name] {
					if obj is types.Type {
						return t.get_field_type_name(obj, field_name)
					}
				}
			}
			// Try the struct name directly
			if obj := scope.objects[struct_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					return result
				}
			}
			// Try just the short name
			if obj := scope.objects[lookup_name] {
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
	if typ is types.SumType {
		return typ.get_name()
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

// is_interface_receiver checks if an expression's type is an interface type
fn (t &Transformer) is_interface_receiver(expr ast.Expr) bool {
	if expr is ast.Ident {
		// Use scope lookup to check if variable's type is an interface
		return t.is_interface_var(expr.name)
	}
	return false
}

// is_interface_cast checks if an expression is an interface cast like Calculator(value)
fn (t &Transformer) is_interface_cast(expr ast.Expr) bool {
	// Interface casts appear as CallOrCastExpr: InterfaceType(value)
	if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			// Look up the type name in the environment
			type_name := (expr.lhs as ast.Ident).name
			mut scope := t.get_current_scope() or { return false }
			obj := scope.lookup_parent(type_name, 0) or { return false }
			if obj is types.Type {
				return obj is types.Interface
			}
		}
	}
	return false
}

// is_interface_type checks if a type name corresponds to an interface type
fn (t &Transformer) is_interface_type(type_name string) bool {
	// Strip pointer suffix if present
	clean_name := if type_name.ends_with('*') { type_name[..type_name.len - 1] } else { type_name }
	// Look up in module scope
	mut scope := t.get_current_scope() or { return false }
	obj := scope.lookup_parent(clean_name, 0) or { return false }
	if obj is types.Type {
		return obj is types.Interface
	}
	return false
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
	// Handle pointer type expressions like &Type
	if expr is ast.PrefixExpr && expr.op == .amp {
		inner_type := t.get_expr_type(expr.expr) or { return none }
		return types.Pointer{
			base_type: inner_type
		}
	}
	// Handle modifier expressions like mut Type
	if expr is ast.ModifierExpr {
		return t.get_expr_type(expr.expr)
	}
	if expr is ast.Ident {
		// Look up variable type from scope
		if var_type := t.lookup_var_type(expr.name) {
			return var_type
		}
		// Fall back to current scope lookup
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
		// For array/map indexing (a[i] or m[k]), get the element/value type
		lhs_type := t.get_expr_type(expr.lhs) or { return none }
		if lhs_type is types.Array {
			return lhs_type.elem_type
		}
		if lhs_type is types.ArrayFixed {
			return lhs_type.elem_type
		}
		if lhs_type is types.Map {
			return lhs_type.value_type
		}
	}
	// Handle array type expressions (for receiver types like []Expr)
	if expr is ast.Type {
		if expr is ast.ArrayType {
			elem_type := t.get_expr_type(expr.elem_type) or { return none }
			return types.Array{
				elem_type: elem_type
			}
		}
		if expr is ast.ArrayFixedType {
			elem_type := t.get_expr_type(expr.elem_type) or { return none }
			mut len := 0
			if expr.len is ast.BasicLiteral && expr.len.kind == .number {
				len = expr.len.value.int()
			}
			return types.ArrayFixed{
				len:       len
				elem_type: elem_type
			}
		}
	}
	// Handle function calls - look up return type
	if expr is ast.CallExpr {
		mut fn_name := ''
		mut mod_name := ''
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			// Check for module.function call
			if sel.lhs is ast.Ident {
				mod_name = (sel.lhs as ast.Ident).name
				fn_name = sel.rhs.name
			}
		}
		if fn_name != '' {
			// Look up function return type from environment
			if mod_name != '' {
				if fn_type := t.env.lookup_fn(mod_name, fn_name) {
					if ret_type := fn_type.get_return_type() {
						return ret_type
					}
				}
			} else if t.cur_module != '' {
				// Try current module first
				if fn_type := t.env.lookup_fn(t.cur_module, fn_name) {
					if ret_type := fn_type.get_return_type() {
						return ret_type
					}
				}
			}
			// Try builtin
			if fn_type := t.env.lookup_fn('builtin', fn_name) {
				if ret_type := fn_type.get_return_type() {
					return ret_type
				}
			}
		}
	}
	// Handle single-arg function calls (CallOrCastExpr)
	if expr is ast.CallOrCastExpr {
		mut fn_name := ''
		mut mod_name := ''
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			if sel.lhs is ast.Ident {
				mod_name = (sel.lhs as ast.Ident).name
				fn_name = sel.rhs.name
			}
		}
		if fn_name != '' {
			if mod_name != '' {
				if fn_type := t.env.lookup_fn(mod_name, fn_name) {
					if ret_type := fn_type.get_return_type() {
						return ret_type
					}
				}
			} else if t.cur_module != '' {
				if fn_type := t.env.lookup_fn(t.cur_module, fn_name) {
					if ret_type := fn_type.get_return_type() {
						return ret_type
					}
				}
			}
		}
	}
	// Handle MapInitExpr (for literal maps like {'a': 1, 'b': 2})
	if expr is ast.MapInitExpr {
		// Helper to get string type from scope
		string_type := if mut scope := t.get_current_scope() {
			if obj := scope.lookup_parent('string', 0) {
				obj.typ()
			} else {
				types.Type(types.Primitive{
					size:  64
					props: .integer
				})
			}
		} else {
			types.Type(types.Primitive{
				size:  64
				props: .integer
			})
		}
		int_type := types.Type(types.Primitive{
			size:  64
			props: .integer
		})

		// Check if map has explicit type
		match expr.typ {
			ast.Type {
				if expr.typ is ast.MapType {
					mt := expr.typ as ast.MapType
					key_type := t.get_expr_type(mt.key_type) or { string_type }
					value_type := t.get_expr_type(mt.value_type) or { int_type }
					return types.Map{
						key_type:   key_type
						value_type: value_type
					}
				}
			}
			else {}
		}
		// Infer from first key/value (for literal maps like {'a': 1})
		if expr.keys.len > 0 {
			first_key := expr.keys[0]
			first_val := expr.vals[0]
			mut key_type := int_type
			mut val_type := int_type
			if first_key is ast.StringLiteral
				|| (first_key is ast.BasicLiteral && first_key.kind == .string) {
				key_type = string_type
			}
			if first_val is ast.StringLiteral
				|| (first_val is ast.BasicLiteral && first_val.kind == .string) {
				val_type = string_type
			}
			return types.Map{
				key_type:   key_type
				value_type: val_type
			}
		}
	}
	return none
}

// get_receiver_type_name extracts the type name from a receiver type AST expression
// This is used to build the scope key for methods (e.g., "SortedMap__set")
fn (t &Transformer) get_receiver_type_name(typ ast.Expr) string {
	if typ is ast.Ident {
		return typ.name
	}
	if typ is ast.PrefixExpr && typ.op == .amp {
		// &Type -> Type
		return t.get_receiver_type_name(typ.expr)
	}
	if typ is ast.ModifierExpr {
		// mut Type -> Type
		return t.get_receiver_type_name(typ.expr)
	}
	if typ is ast.SelectorExpr {
		// module.Type -> module__Type (but we want just Type for scope key)
		// For now, just use the rhs (type name)
		return typ.rhs.name
	}
	if typ is ast.Type {
		// Handle wrapped type variants (GenericType, etc.)
		if typ is ast.GenericType {
			// Type[T] -> Type
			return t.get_receiver_type_name(typ.name)
		}
	}
	return ''
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
		// First try to get the actual type from scope
		if typ := t.lookup_var_type(expr.name) {
			// Unwrap pointer if needed
			base_type := typ.base_type()
			if base_type is types.Array {
				// Use alias-resolving version for array_contains function naming
				// This resolves ValueID -> int, BlockID -> int, etc.
				elem_name := t.type_to_c_name_resolve_alias(base_type.elem_type)
				return t.normalize_literal_type(elem_name)
			}
			// Handle strings.Builder alias
			if base_type is types.Alias {
				if base_type.name.contains('Builder') {
					return 'u8'
				}
			}
		}
		// Fallback to string-based detection
		var_type := t.get_var_type_name(expr.name)
		if var_type != '' {
			// Handle pointer to array (&[]T) - strip the & prefix first
			mut type_to_check := var_type
			if type_to_check.starts_with('&') {
				type_to_check = type_to_check[1..]
			}
			// Now convert to C-style
			c_type := t.v_type_name_to_c_name(type_to_check)
			if c_type.starts_with('Array_') {
				elem := c_type['Array_'.len..]
				return t.normalize_literal_type(elem)
			}
			// Handle strings.Builder which is an alias for []u8
			if c_type.starts_with('strings__Builder') || c_type == 'Builder' {
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
		// Also check V-style array names like []string
		if ret_type.starts_with('[]') {
			elem := ret_type[2..]
			// Convert to C-style if needed
			return t.v_type_name_to_c_name(elem).trim_right('ptr')
		}
	}
	// Handle SelectorExpr - could be field access or method call
	if expr is ast.SelectorExpr {
		// First check if this is a struct field access to an array field
		// This should be checked before method inference since a field named 'values'
		// is different from a method called 'values()'
		if field_type := t.get_struct_field_type(expr) {
			if field_type is types.Array {
				// Use alias-resolving version for array_contains function naming
				return t.type_to_c_name_resolve_alias(field_type.elem_type)
			}
		}
		// Check if this is a method call that returns an array
		method_name := expr.rhs.name
		if method_name in ['bytes', 'runes', 'split', 'split_any', 'fields', 'keys', 'values'] {
			// Common array-returning methods
			return t.infer_method_array_elem_type(expr)
		}
	}
	// Handle IndexExpr - map lookup that returns an array (e.g., g.pending_labels[blk])
	if expr is ast.IndexExpr {
		// Check if this is a map lookup returning an array
		map_type := t.get_expr_type(expr.lhs) or { return none }
		if map_type is types.Map {
			if map_type.value_type is types.Array {
				// Use alias-resolving version for array_contains function naming
				return t.type_to_c_name_resolve_alias(map_type.value_type.elem_type)
			}
		}
	}
	// Also try getting from types.Environment
	typ := t.get_expr_type(expr) or { return none }
	// Unwrap pointer types (e.g., strings__Builder* -> strings__Builder)
	base_typ := if typ is types.Pointer { typ.base_type } else { typ }
	if base_typ is types.Array {
		// Use alias-resolving version for array_contains function naming
		return t.type_to_c_name_resolve_alias(base_typ.elem_type)
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
	mut is_module_fn := false
	mut mod_name := ''
	mut selector_expr := ast.SelectorExpr{}
	if expr is ast.CallExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			// Method call or module.function call
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			// Check if LHS is a module name (starts with lowercase and no field access)
			if selector_expr.lhs is ast.Ident {
				lhs_name := (selector_expr.lhs as ast.Ident).name
				// Check if it's a module by looking it up
				if t.get_module_scope(lhs_name) != none {
					is_module_fn = true
					mod_name = lhs_name
				} else {
					is_method = true
				}
			} else {
				is_method = true
			}
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.Ident {
			fn_name = expr.lhs.name
		} else if expr.lhs is ast.SelectorExpr {
			selector_expr = expr.lhs as ast.SelectorExpr
			fn_name = selector_expr.rhs.name
			// Check if LHS is a module name
			if selector_expr.lhs is ast.Ident {
				lhs_name := (selector_expr.lhs as ast.Ident).name
				if t.get_module_scope(lhs_name) != none {
					is_module_fn = true
					mod_name = lhs_name
				} else {
					is_method = true
				}
			} else {
				is_method = true
			}
		}
	}
	if fn_name != '' {
		// Check function return type using scope lookup
		if is_module_fn && mod_name != '' {
			// Look up function in the specific module's scope
			if mut mod_scope := t.get_module_scope(mod_name) {
				if obj := mod_scope.lookup_parent(fn_name, 0) {
					if obj is types.Fn {
						fn_typ := obj.get_typ()
						if fn_typ is types.FnType {
							if ret := fn_typ.get_return_type() {
								return ret.name()
							}
						}
					}
				}
			}
		} else {
			// Look up in current module
			if ret_type := t.get_fn_return_type(fn_name) {
				return ret_type.name()
			}
		}
		// For method calls, try to look up the method's return type from the receiver
		if is_method && selector_expr.lhs !is ast.EmptyExpr {
			// Get receiver type and look up method
			if recv_type := t.get_expr_type(selector_expr.lhs) {
				// Get the base type name for method lookup
				base_type := if recv_type is types.Pointer { recv_type.base_type } else { recv_type }
				type_name := base_type.name()
				// Look up method using environment
				if fn_typ := t.env.lookup_method(type_name, fn_name) {
					if ret := fn_typ.get_return_type() {
						return ret.name()
					}
				}
			}
		}
		// For method calls, check for known array-returning methods as fallback
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
	// Try to get the struct type from scope (for local variables and receivers)
	mut struct_type_name := ''
	if expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		lhs_type := t.get_var_type_name(lhs_name)
		if lhs_type != '' {
			// Remove pointer indicators: both V-style (&T) and C-style (T*)
			struct_type_name = lhs_type.trim_left('&').trim_right('*')
		}
	}

	// If we have a type name, look it up in the environment
	if struct_type_name != '' {
		// Types are defined at module level, not function level
		// Use lookup_type which searches module scopes
		looked_up_type := t.lookup_type(struct_type_name) or { return none }
		base_type := if looked_up_type is types.Pointer {
			looked_up_type.base_type
		} else {
			looked_up_type
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
		var_type := t.get_var_type_name(expr.name)
		if var_type != '' {
			// Convert V-style type name to C-style
			c_type := t.v_type_name_to_c_name(var_type)
			if c_type.starts_with('Array_') {
				// Normalize literal types to concrete types
				return t.normalize_array_type(c_type)
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
				// Check scope for local variables
				var_type := t.get_var_type_name(first.name)
				if var_type != '' {
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

// normalize_literal_type converts untyped literal types to their default concrete types
fn (t &Transformer) normalize_literal_type(type_name string) string {
	match type_name {
		'int_literal' { return 'int' }
		'float_literal' { return 'f64' }
		else { return type_name }
	}
}

// normalize_array_type normalizes literal types in an Array_T type name
fn (t &Transformer) normalize_array_type(array_type string) string {
	if !array_type.starts_with('Array_') {
		return array_type
	}
	elem_type := array_type['Array_'.len..]
	normalized_elem := t.normalize_literal_type(elem_type)
	return 'Array_${normalized_elem}'
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
			type_str := t.get_var_type_name(name)
			if type_str != '' {
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

// is_map_lookup_returning_array checks if an expression is a map lookup that returns an array type
fn (t &Transformer) is_map_lookup_returning_array(expr ast.Expr) bool {
	// Check if expr is an IndexExpr (map[key])
	if expr !is ast.IndexExpr {
		return false
	}
	index_expr := expr as ast.IndexExpr
	// Check if the LHS is a map type
	map_type := t.get_expr_type(index_expr.lhs) or { return false }
	if map_type is types.Map {
		// Check if the value type is an array
		return map_type.value_type is types.Array
	}
	return false
}

// get_map_type_for_expr returns the Map_K_V type string for an expression if it's a map
fn (t &Transformer) get_map_type_for_expr(expr ast.Expr) ?string {
	// Use type lookup from scope/environment
	typ := t.get_expr_type(expr) or { return none }
	if typ is types.Map {
		key_c := t.type_to_c_name(typ.key_type)
		val_c := t.type_to_c_name(typ.value_type)
		return 'Map_${key_c}_${val_c}'
	}
	// Also handle pointer to map (e.g., from mut map parameters)
	if typ is types.Pointer {
		if typ.base_type is types.Map {
			map_type := typ.base_type as types.Map
			key_c := t.type_to_c_name(map_type.key_type)
			val_c := t.type_to_c_name(map_type.value_type)
			return 'Map_${key_c}_${val_c}'
		}
	}
	return none
}

// get_selector_type_name returns the type name string for a SelectorExpr
// This function is smartcast-aware: if the LHS is smartcasted, it uses the
// smartcasted variant type to resolve the field type.
fn (t &Transformer) get_selector_type_name(expr ast.SelectorExpr) string {
	if expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		// Check if the LHS variable is currently smartcasted
		if ctx := t.find_smartcast_for_expr(lhs_name) {
			// Use the smartcasted variant type to resolve the field
			return t.resolve_struct_field_type(ctx.variant, expr.rhs.name)
		}
		return t.resolve_field_type(lhs_name, expr.rhs.name)
	}
	// Handle nested selector like a.b.c - check if a.b is smartcasted
	if expr.lhs is ast.SelectorExpr {
		lhs_str := t.expr_to_string(expr.lhs)
		if ctx := t.find_smartcast_for_expr(lhs_str) {
			// Use the smartcasted variant type to resolve the field
			return t.resolve_struct_field_type(ctx.variant, expr.rhs.name)
		}
		// Recursively get the type of the LHS and then resolve the field
		lhs_type := t.get_selector_type_name(expr.lhs as ast.SelectorExpr)
		if lhs_type != '' {
			return t.resolve_struct_field_type(lhs_type, expr.rhs.name)
		}
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
		types.Char {
			return 'char'
		}
		types.Rune {
			return 'rune'
		}
		types.Void {
			return 'void'
		}
		types.Nil {
			return 'voidptr'
		}
		types.None {
			return 'none'
		}
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Struct {
			return t.qualify_type_name(typ.name)
		}
		types.Enum {
			return t.qualify_type_name(typ.name)
		}
		types.SumType {
			return t.qualify_type_name(typ.get_name())
		}
		types.Alias {
			return t.qualify_type_name(typ.name)
		}
		types.NamedType {
			return t.qualify_type_name(string(typ))
		}
		types.Array {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_${elem_name}'
		}
		types.ArrayFixed {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_fixed_${elem_name}_${typ.len}'
		}
		types.Map {
			key_c := t.type_to_c_name(typ.key_type)
			val_c := t.type_to_c_name(typ.value_type)
			return 'Map_${key_c}_${val_c}'
		}
		types.Pointer {
			base_name := t.type_to_c_name(typ.base_type)
			// Only use Tptr naming for known pointer aliases
			if base_name == 'char' {
				return 'charptr'
			}
			if base_name == 'void' {
				return 'voidptr'
			}
			if base_name == 'u8' {
				return 'byteptr'
			}
			// For other pointer types, use mangled ptr suffix for type names
			// (This is used in map type names like Map_int_Intervalptr)
			return '${base_name}ptr'
		}
		else {
			return 'int'
		}
	}
}

// type_to_c_name_resolve_alias returns the C type name for a type, resolving simple aliases
// to their underlying primitive types. This is used for array__contains_* function naming
// where we want ValueID -> int, BlockID -> int, etc.
fn (t &Transformer) type_to_c_name_resolve_alias(typ types.Type) string {
	// If it's an alias, try to resolve to underlying type
	if typ is types.Alias {
		// Resolve to the underlying type
		base := typ.base_type
		// If base is a primitive int type, use that
		base_name := t.type_to_c_name(base)
		if base_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'bool', 'f32',
			'f64', 'rune'] {
			return base_name
		}
		// Otherwise keep the alias name
		return t.type_to_c_name(typ)
	}
	// For non-alias types, use normal type_to_c_name
	return t.type_to_c_name(typ)
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

	// Transform the receiver to apply smartcast if needed
	transformed_receiver := t.transform_expr(receiver)

	// Cast receiver to int: int(receiver)
	receiver_int := ast.CastExpr{
		typ:  ast.Ident{
			name: 'int'
		}
		expr: transformed_receiver
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
		paren_pos := t.next_synth_pos()
		t.register_synth_type(paren_pos, types.int_)
		return ast.InfixExpr{
			op:  .ne
			lhs: ast.ParenExpr{
				expr: and_expr
				pos:  paren_pos
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
		paren_pos := t.next_synth_pos()
		t.register_synth_type(paren_pos, types.int_)
		return ast.InfixExpr{
			op:  .eq
			lhs: ast.ParenExpr{
				expr: and_expr
				pos:  paren_pos
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
			pos:  paren.pos
		}
	}
	return expr
}

// get_enum_type_name returns the enum type name for an expression, or empty string if not an enum
fn (t &Transformer) get_enum_type_name(expr ast.Expr) string {
	// Check scope for variable type
	if expr is ast.Ident {
		type_name := t.get_var_type_name(expr.name)
		if type_name != '' && type_name != 'int' && type_name != 'string' && type_name != 'bool' {
			return type_name
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
		// Also check scope for lhs.rhs pattern if lhs is ident
		if expr.lhs is ast.Ident {
			lhs_ident := expr.lhs as ast.Ident
			lhs_type := t.get_var_type_name(lhs_ident.name)
			if lhs_type != '' {
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
	// Also check for [x, y, z]! syntax - parser marks this with len: PostfixExpr{op: .not}
	if arr.len is ast.PostfixExpr {
		postfix := arr.len as ast.PostfixExpr
		if postfix.op == .not && postfix.expr is ast.EmptyExpr {
			is_fixed = true
		}
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
		// Check if variable type is string via scope lookup
		var_type_name := t.get_var_type_name(expr.name)
		if var_type_name == 'string' {
			return true
		}
		// Use type environment to look up the identifier's type
		if mut scope := t.get_current_scope() {
			if obj := scope.lookup_parent(expr.name, 0) {
				typ := obj.typ()
				$if debug ? {
					eprintln('DEBUG: is_string_expr Ident ${expr.name} scope lookup typ=${typ.name()}')
				}
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
		if expr.lhs is ast.Ident {
			arr_name := (expr.lhs as ast.Ident).name
			arr_type := t.get_var_type_name(arr_name)
			if arr_type == 'Array_string' {
				return true
			}
		}
		// Use get_expr_type which handles IndexExpr and returns the element type
		if elem_type := t.get_expr_type(expr) {
			$if debug ? {
				eprintln('DEBUG: is_string_expr IndexExpr elem_type=${elem_type.name()}')
			}
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
					receiver_type := t.get_var_type_name(receiver_name)
					if receiver_type == 'Array_string' {
						return true
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
	// Check function return type using scope lookup
	if ret_type := t.get_fn_return_type(fn_name) {
		if ret_type.name() == 'string' {
			return true
		}
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
		// Check scope for variable type
		var_type := t.get_var_type_name(expr.name)
		if var_type != '' {
			// Check for both '*' suffix (C-style) and '&' prefix (V reference types)
			return var_type.ends_with('*') || var_type.starts_with('&')
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

// get_str_fn_name_for_expr returns the str function name for an expression's type.
// For example: []int -> Array_int_str, map[string]int -> Map_string_int_str
fn (t &Transformer) get_str_fn_name_for_expr(expr ast.Expr) ?string {
	// First try to infer array type
	if array_type := t.infer_array_type(expr) {
		// array_type is like 'Array_int', so append '_str'
		return '${array_type}_str'
	}
	// Try to infer map type
	if map_type := t.infer_map_type(expr) {
		// map_type is like 'Map_string_int', so append '_str'
		return '${map_type}_str'
	}
	// Try to get type from expression
	if typ := t.get_expr_type(expr) {
		return t.get_str_fn_name_for_type(typ)
	}
	// Handle ArrayInitExpr directly for inline array literals
	if expr is ast.ArrayInitExpr {
		// Get element type from first element or type annotation
		elem_type := t.get_array_init_elem_type(expr)
		if elem_type != '' {
			return 'Array_${elem_type}_str'
		}
		return 'Array_int_str' // Default fallback
	}
	return none
}

// StrFnInfo holds information about an auto-generated str function
struct StrFnInfo {
	str_fn_name string
	elem_type   string
}

// get_str_fn_info_for_expr returns the str function name and element type info for auto-generation.
// Returns StrFnInfo with str_fn_name and elem_type where elem_type is the element type for arrays.
fn (mut t Transformer) get_str_fn_info_for_expr(expr ast.Expr) StrFnInfo {
	// First try to infer array type
	if array_type := t.infer_array_type(expr) {
		// array_type is like 'Array_int'
		elem_type := array_type['Array_'.len..]
		return StrFnInfo{
			str_fn_name: '${array_type}_str'
			elem_type:   elem_type
		}
	}
	// Try to infer map type
	if map_type := t.infer_map_type(expr) {
		// map_type is like 'Map_string_int'
		return StrFnInfo{
			str_fn_name: '${map_type}_str'
			elem_type:   map_type
		}
	}
	// Handle ArrayInitExpr directly for inline array literals
	if expr is ast.ArrayInitExpr {
		elem_type := t.get_array_init_elem_type(expr)
		if elem_type != '' {
			return StrFnInfo{
				str_fn_name: 'Array_${elem_type}_str'
				elem_type:   elem_type
			}
		}
		return StrFnInfo{
			str_fn_name: 'Array_int_str'
			elem_type:   'int'
		}
	}
	return StrFnInfo{}
}

// get_str_fn_name_for_type returns the str function name for a types.Type
fn (t &Transformer) get_str_fn_name_for_type(typ types.Type) ?string {
	match typ {
		types.Array {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_${elem_name}_str'
		}
		types.Map {
			key_name := t.type_to_c_name(typ.key_type)
			val_name := t.type_to_c_name(typ.value_type)
			return 'Map_${key_name}_${val_name}_str'
		}
		types.Struct {
			return '${typ.name}_str'
		}
		types.Enum {
			return '${typ.name}_str'
		}
		types.Primitive {
			if typ.props.has(.boolean) {
				return 'bool_str'
			}
			if typ.props.has(.float) {
				if typ.size == 4 {
					return 'f32_str'
				}
				return 'f64_str'
			}
			if typ.props.has(.unsigned) {
				match typ.size {
					1 { return 'u8_str' }
					2 { return 'u16_str' }
					4 { return 'u32_str' }
					8 { return 'u64_str' }
					else { return 'int_str' }
				}
			}
			match typ.size {
				1 { return 'i8_str' }
				2 { return 'i16_str' }
				4 { return 'int_str' }
				8 { return 'i64_str' }
				else { return 'int_str' }
			}
		}
		types.Pointer {
			// For pointers, use the base type's str function
			return t.get_str_fn_name_for_type(typ.base_type)
		}
		types.Alias {
			return '${typ.name}_str'
		}
		else {
			return none
		}
	}
}

// get_array_init_elem_type returns the element type name for an ArrayInitExpr
fn (t &Transformer) get_array_init_elem_type(expr ast.ArrayInitExpr) string {
	// Check if array has explicit type
	if expr.typ is ast.Type {
		if expr.typ is ast.ArrayType {
			return t.expr_to_type_name(expr.typ.elem_type)
		}
	}
	// Infer from first element
	if expr.exprs.len > 0 {
		first := expr.exprs[0]
		if first is ast.BasicLiteral {
			if first.kind == .number {
				return 'int'
			}
			if first.kind == .string {
				return 'string'
			}
		}
		if first is ast.StringLiteral {
			return 'string'
		}
	}
	return 'int' // Default
}

// generate_str_functions generates FnDecl AST nodes for needed auto str functions.
// For arrays: generates a function that iterates over elements and calls their str methods.
fn (t &Transformer) generate_str_functions() []ast.Stmt {
	mut result := []ast.Stmt{cap: t.needed_str_fns.len}
	for fn_name, elem_type in t.needed_str_fns {
		// Generate array str function
		if fn_name.starts_with('Array_') {
			result << t.generate_array_str_fn(fn_name, elem_type)
		}
	}
	return result
}

// generate_array_str_fn generates a str function for an array type.
// Example for Array_int_str:
//   fn Array_int_str(a Array_int) string {
//       mut sb := strings__new_builder(2 + a.len * 10)
//       strings__Builder__write_string(&sb, "[")
//       for i := 0; i < a.len; i++ {
//           if i > 0 { strings__Builder__write_string(&sb, ", ") }
//           strings__Builder__write_string(&sb, int_str(a.data[i]))
//       }
//       strings__Builder__write_string(&sb, "]")
//       return strings__Builder__str(&sb)
//   }
fn (t &Transformer) generate_array_str_fn(fn_name string, elem_type string) ast.Stmt {
	// Create parameter: a Array_int
	array_type_name := fn_name[..fn_name.len - 4] // Remove '_str' suffix: 'Array_int_str' -> 'Array_int'
	param_a := ast.Parameter{
		name: 'a'
		typ:  ast.Ident{
			name: array_type_name
		}
	}

	// Get element str function name (use double underscore for method)
	elem_str_fn := '${elem_type}__str'

	// Build the function body statements
	mut body_stmts := []ast.Stmt{}

	// mut sb := strings__new_builder(2 + a.len * 10)
	body_stmts << ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: ast.Ident{
					name: 'sb'
				}
			}),
		]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: 'strings__new_builder'
				}
				args: [
					ast.Expr(ast.InfixExpr{
						op:  .plus
						lhs: ast.BasicLiteral{
							kind:  .number
							value: '2'
						}
						rhs: ast.InfixExpr{
							op:  .mul
							lhs: ast.SelectorExpr{
								lhs: ast.Ident{
									name: 'a'
								}
								rhs: ast.Ident{
									name: 'len'
								}
							}
							rhs: ast.BasicLiteral{
								kind:  .number
								value: '10'
							}
						}
					}),
				]
			}),
		]
	}

	// strings__Builder__write_string(&sb, "[")
	body_stmts << ast.ExprStmt{
		expr: ast.CallExpr{
			lhs:  ast.Ident{
				name: 'strings__Builder__write_string'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: 'sb'
					}
				}),
				ast.Expr(ast.StringLiteral{
					kind:  .v
					value: '['
				}),
			]
		}
	}

	// for i := 0; i < a.len; i++ { ... }
	// Build the for loop body
	mut for_body := []ast.Stmt{}

	// if i > 0 { strings__Builder__write_string(&sb, ", ") }
	for_body << ast.ExprStmt{
		expr: ast.IfExpr{
			cond:  ast.InfixExpr{
				op:  .gt
				lhs: ast.Ident{
					name: 'i'
				}
				rhs: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			}
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: ast.CallExpr{
						lhs:  ast.Ident{
							name: 'strings__Builder__write_string'
						}
						args: [
							ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: 'sb'
								}
							}),
							ast.Expr(ast.StringLiteral{
								kind:  .v
								value: ', '
							}),
						]
					}
				}),
			]
		}
	}

	// strings__Builder__write_string(&sb, elem_str(*(elem_type*)array__get(a, i)))
	for_body << ast.ExprStmt{
		expr: ast.CallExpr{
			lhs:  ast.Ident{
				name: 'strings__Builder__write_string'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: 'sb'
					}
				}),
				// elem_str(*(elem_type*)array__get(a, i))
				ast.Expr(ast.CallOrCastExpr{
					lhs:  ast.Ident{
						name: elem_str_fn
					}
					expr: ast.PrefixExpr{
						op:   .mul
						expr: ast.CastExpr{
							typ:  ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: elem_type
								}
							}
							expr: ast.CallExpr{
								lhs:  ast.Ident{
									name: 'array__get'
								}
								args: [
									ast.Expr(ast.Ident{
										name: 'a'
									}),
									ast.Expr(ast.Ident{
										name: 'i'
									}),
								]
							}
						}
					}
				}),
			]
		}
	}

	// for loop: for i := 0; i < a.len; i++ { ... }
	body_stmts << ast.ForStmt{
		init:  ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(ast.Ident{
				name: 'i'
			})]
			rhs: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})]
		}
		cond:  ast.InfixExpr{
			op:  .lt
			lhs: ast.Ident{
				name: 'i'
			}
			rhs: ast.SelectorExpr{
				lhs: ast.Ident{
					name: 'a'
				}
				rhs: ast.Ident{
					name: 'len'
				}
			}
		}
		post:  ast.AssignStmt{
			op:  .plus_assign
			lhs: [ast.Expr(ast.Ident{
				name: 'i'
			})]
			rhs: [ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '1'
			})]
		}
		stmts: for_body
	}

	// strings__Builder__write_string(&sb, "]")
	body_stmts << ast.ExprStmt{
		expr: ast.CallExpr{
			lhs:  ast.Ident{
				name: 'strings__Builder__write_string'
			}
			args: [
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: 'sb'
					}
				}),
				ast.Expr(ast.StringLiteral{
					kind:  .v
					value: ']'
				}),
			]
		}
	}

	// return strings__Builder__str(&sb)
	body_stmts << ast.ReturnStmt{
		exprs: [
			ast.Expr(ast.CallOrCastExpr{
				lhs:  ast.Ident{
					name: 'strings__Builder__str'
				}
				expr: ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: 'sb'
					}
				}
			}),
		]
	}

	// Create the function declaration
	return ast.FnDecl{
		name:       fn_name
		is_public:  false
		is_method:  false
		is_static:  false
		attributes: []ast.Attribute{}
		typ:        ast.FnType{
			params:      [param_a]
			return_type: ast.Ident{
				name: 'string'
			}
		}
		stmts:      body_stmts
	}
}
