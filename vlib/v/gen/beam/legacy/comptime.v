// Copyright (c) 2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module beam

import v.ast

// ComptimeMeta represents compile-time metadata for reflection
// This is a tagged union of different meta types we can iterate over
pub type ComptimeMeta = FieldMeta | MethodMeta | EnumValueMeta | AttrMeta

pub struct FieldMeta {
pub:
	name   string
	typ    ast.Type
	attrs  []ast.Attr
	is_pub bool
	is_mut bool
}

pub struct MethodMeta {
pub:
	name        string
	params      []ast.Param
	return_type ast.Type
	attrs       []ast.Attr
	is_pub      bool
}

pub struct EnumValueMeta {
pub:
	name  string
	value i64
}

pub struct AttrMeta {
pub:
	name string
	arg  string // Single arg value (V attributes have single arg)
}

// ComptimeEnv tracks the current comptime loop variable bindings
struct ComptimeEnv {
	var_name string       // "field", "method", etc.
	value    ComptimeMeta // The current iteration value
	typ      ast.Type     // The type being iterated over (e.g., App for App.methods)
	kind     ast.ComptimeForKind
}

// Push/pop comptime environment for nested $for loops
fn (mut g Gen) push_comptime_env(env ComptimeEnv) {
	g.comptime_stack << env
}

fn (mut g Gen) pop_comptime_env() {
	if g.comptime_stack.len > 0 {
		g.comptime_stack.pop()
	}
}

fn (g Gen) lookup_comptime_var(name string) ?ComptimeEnv {
	// Search from innermost scope outward
	for i := g.comptime_stack.len - 1; i >= 0; i-- {
		if g.comptime_stack[i].var_name == name {
			return g.comptime_stack[i]
		}
	}
	return none
}

// comptime_for handles $for loops - expands at compile time
fn (mut g Gen) comptime_for(node ast.ComptimeFor) {
	// Get the type we're iterating over
	typ := node.typ
	type_sym := g.table.sym(typ)

	match node.kind {
		.fields {
			g.comptime_for_fields(node, type_sym)
		}
		.methods {
			g.comptime_for_methods(node, type_sym)
		}
		.values {
			g.comptime_for_enum_values(node, type_sym)
		}
		.attributes {
			g.comptime_for_attributes(node, type_sym)
		}
		.variants {
			// Sum type variants
			g.comptime_for_variants(node, type_sym)
		}
		.params {
			// Method params - need outer method context
			g.comptime_for_params(node)
		}
	}
}

fn (mut g Gen) comptime_for_fields(node ast.ComptimeFor, type_sym &ast.TypeSymbol) {
	// Get struct fields
	if type_sym.info is ast.Struct {
		for field in type_sym.info.fields {
			meta := FieldMeta{
				name:   field.name
				typ:    field.typ
				attrs:  field.attrs
				is_pub: field.is_pub
				is_mut: field.is_mut
			}
			env := ComptimeEnv{
				var_name: node.val_var
				value:    meta
				typ:      node.typ
				kind:     .fields
			}
			g.push_comptime_env(env)

			// Generate code for this field iteration
			for stmt in node.stmts {
				g.stmt(stmt)
			}

			g.pop_comptime_env()
		}
	}
}

fn (mut g Gen) comptime_for_methods(node ast.ComptimeFor, type_sym &ast.TypeSymbol) {
	// Get methods from the type
	for method in type_sym.methods {
		meta := MethodMeta{
			name:        method.name
			params:      method.params
			return_type: method.return_type
			attrs:       method.attrs
			is_pub:      method.is_pub
		}
		env := ComptimeEnv{
			var_name: node.val_var
			value:    meta
			typ:      node.typ
			kind:     .methods
		}
		g.push_comptime_env(env)

		// Generate code for this method iteration
		for stmt in node.stmts {
			g.stmt(stmt)
		}

		g.pop_comptime_env()
	}
}

fn (mut g Gen) comptime_for_enum_values(node ast.ComptimeFor, type_sym &ast.TypeSymbol) {
	if type_sym.info is ast.Enum {
		for i, val in type_sym.info.vals {
			meta := EnumValueMeta{
				name:  val
				value: i64(i)
			}
			env := ComptimeEnv{
				var_name: node.val_var
				value:    meta
				typ:      node.typ
				kind:     .values
			}
			g.push_comptime_env(env)

			for stmt in node.stmts {
				g.stmt(stmt)
			}

			g.pop_comptime_env()
		}
	}
}

fn (mut g Gen) comptime_for_attributes(node ast.ComptimeFor, type_sym &ast.TypeSymbol) {
	// Get attributes - from struct or current field
	// Check if we're inside a field loop
	if field_env := g.lookup_comptime_var('field') {
		if field_env.value is FieldMeta {
			for attr in field_env.value.attrs {
				meta := AttrMeta{
					name: attr.name
					arg:  attr.arg
				}
				env := ComptimeEnv{
					var_name: node.val_var
					value:    meta
					typ:      node.typ
					kind:     .attributes
				}
				g.push_comptime_env(env)

				for stmt in node.stmts {
					g.stmt(stmt)
				}

				g.pop_comptime_env()
			}
		}
	} else if type_sym.info is ast.Struct {
		// Struct-level attributes
		for attr in type_sym.info.attrs {
			meta := AttrMeta{
				name: attr.name
				arg:  attr.arg
			}
			env := ComptimeEnv{
				var_name: node.val_var
				value:    meta
				typ:      node.typ
				kind:     .attributes
			}
			g.push_comptime_env(env)

			for stmt in node.stmts {
				g.stmt(stmt)
			}

			g.pop_comptime_env()
		}
	}
}

fn (mut g Gen) comptime_for_variants(node ast.ComptimeFor, type_sym &ast.TypeSymbol) {
	if type_sym.info is ast.SumType {
		for variant in type_sym.info.variants {
			// Create a field-like meta for variants
			variant_sym := g.table.sym(variant)
			meta := FieldMeta{
				name: variant_sym.name
				typ:  variant
			}
			env := ComptimeEnv{
				var_name: node.val_var
				value:    meta
				typ:      node.typ
				kind:     .variants
			}
			g.push_comptime_env(env)

			for stmt in node.stmts {
				g.stmt(stmt)
			}

			g.pop_comptime_env()
		}
	}
}

fn (mut g Gen) comptime_for_params(node ast.ComptimeFor) {
	// Iterate over method params - requires method context
	if method_env := g.lookup_comptime_var('method') {
		if method_env.value is MethodMeta {
			// Skip receiver (first param for methods)
			params := if method_env.value.params.len > 0 {
				method_env.value.params[1..]
			} else {
				[]ast.Param{}
			}

			for param in params {
				meta := FieldMeta{
					name: param.name
					typ:  param.typ
				}
				env := ComptimeEnv{
					var_name: node.val_var
					value:    meta
					typ:      node.typ
					kind:     .params
				}
				g.push_comptime_env(env)

				for stmt in node.stmts {
					g.stmt(stmt)
				}

				g.pop_comptime_env()
			}
		}
	}
}

// comptime_if evaluates $if at compile time and emits only the matching branch
fn (mut g Gen) comptime_if(node ast.IfExpr) {
	for i, branch in node.branches {
		is_else := i == node.branches.len - 1 && branch.cond is ast.EmptyExpr

		if is_else {
			// else branch - generate it
			for stmt in branch.stmts {
				g.stmt(stmt)
			}
			return
		}

		// Evaluate the comptime condition
		if g.eval_comptime_condition(branch.cond) {
			// Condition is true - generate this branch only
			for stmt in branch.stmts {
				g.stmt(stmt)
			}
			return
		}
		// Condition false - skip to next branch
	}
}

// eval_comptime_condition evaluates a compile-time condition
fn (g Gen) eval_comptime_condition(cond ast.Expr) bool {
	match cond {
		ast.InfixExpr {
			return g.eval_comptime_infix(cond)
		}
		ast.Ident {
			// Check for comptime built-in idents
			return g.eval_comptime_ident(cond)
		}
		ast.SelectorExpr {
			// field.typ, method.return_type, etc.
			return g.eval_comptime_selector_bool(cond)
		}
		ast.IsRefType {
			// $if field.typ is int { ... }
			return g.eval_comptime_is_type(cond)
		}
		else {
			return false
		}
	}
}

fn (g Gen) eval_comptime_infix(node ast.InfixExpr) bool {
	left_val := g.eval_comptime_value(node.left)
	right_val := g.eval_comptime_value(node.right)

	match node.op {
		.eq {
			return left_val == right_val
		}
		.ne {
			return left_val != right_val
		}
		.and {
			return g.eval_comptime_condition(node.left) && g.eval_comptime_condition(node.right)
		}
		.logical_or {
			return g.eval_comptime_condition(node.left) || g.eval_comptime_condition(node.right)
		}
		else {
			return false
		}
	}
}

fn (g Gen) eval_comptime_ident(node ast.Ident) bool {
	// Handle built-in comptime identifiers
	// These would be things like platform checks, but for BEAM we don't need most
	return false
}

fn (g Gen) eval_comptime_selector_bool(node ast.SelectorExpr) bool {
	// field.is_pub, method.is_pub, etc.
	if node.expr is ast.Ident {
		if env := g.lookup_comptime_var(node.expr.name) {
			match env.value {
				FieldMeta {
					match node.field_name {
						'is_pub' { return env.value.is_pub }
						'is_mut' { return env.value.is_mut }
						else {}
					}
				}
				MethodMeta {
					match node.field_name {
						'is_pub' { return env.value.is_pub }
						else {}
					}
				}
				else {}
			}
		}
	}
	return false
}

fn (g Gen) eval_comptime_is_type(node ast.IsRefType) bool {
	// $if field.typ is int { ... }
	// Get the actual type from comptime context
	if node.expr is ast.SelectorExpr {
		selector := node.expr
		if selector.expr is ast.Ident {
			if env := g.lookup_comptime_var(selector.expr.name) {
				actual_typ := g.get_comptime_type(env, selector.field_name)
				// Compare types
				return actual_typ == node.typ
			}
		}
	}
	return false
}

fn (g Gen) eval_comptime_value(expr ast.Expr) string {
	match expr {
		ast.StringLiteral {
			return expr.val
		}
		ast.SelectorExpr {
			return g.get_comptime_string_value(expr)
		}
		ast.Ident {
			// Check if it's a comptime var
			if env := g.lookup_comptime_var(expr.name) {
				match env.value {
					FieldMeta { return env.value.name }
					MethodMeta { return env.value.name }
					EnumValueMeta { return env.value.name }
					AttrMeta { return env.value.name }
				}
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (g Gen) get_comptime_string_value(node ast.SelectorExpr) string {
	if node.expr is ast.Ident {
		if env := g.lookup_comptime_var(node.expr.name) {
			match env.value {
				FieldMeta {
					match node.field_name {
						'name' { return env.value.name }
						else { return '' }
					}
				}
				MethodMeta {
					match node.field_name {
						'name' { return env.value.name }
						else { return '' }
					}
				}
				EnumValueMeta {
					match node.field_name {
						'name' { return env.value.name }
						else { return '' }
					}
				}
				AttrMeta {
					match node.field_name {
						'name' { return env.value.name }
						else { return '' }
					}
				}
			}
		}
	}
	return ''
}

fn (g Gen) get_comptime_type(env ComptimeEnv, field_name string) ast.Type {
	match env.value {
		FieldMeta {
			if field_name == 'typ' {
				return env.value.typ
			}
		}
		MethodMeta {
			match field_name {
				'typ' { return ast.void_type } // Method type itself
				'return_type' { return env.value.return_type }
				else {}
			}
		}
		else {}
	}
	return ast.void_type
}

// comptime_selector handles $() selectors: obj.$(field.name)
fn (mut g Gen) comptime_selector(node ast.ComptimeSelector) {
	// Get the field name from the comptime context
	if node.field_expr is ast.SelectorExpr {
		selector := node.field_expr
		if selector.expr is ast.Ident {
			if env := g.lookup_comptime_var(selector.expr.name) {
				if selector.field_name == 'name' {
					// obj.$(field.name) -> maps:get(field_name_atom, Obj)
					field_name := match env.value {
						FieldMeta { env.value.name }
						MethodMeta { env.value.name }
						EnumValueMeta { env.value.name }
						AttrMeta { env.value.name }
					}
					g.write('maps:get(${field_name}, ')
					g.expr(node.left)
					g.write(')')
					return
				}
			}
		}
	}

	// Fallback - shouldn't happen with valid code
	g.write('maps:get(todo, ')
	g.expr(node.left)
	g.write(')')
}

// comptime_ident_expr handles identifier expressions that reference comptime vars
fn (mut g Gen) comptime_ident_expr(ident ast.Ident) bool {
	// Check if this identifier is a comptime loop variable
	if env := g.lookup_comptime_var(ident.name) {
		// Generate a representation of the comptime value
		// This is typically used in string interpolation or println
		match env.value {
			FieldMeta {
				// field by itself - output its name
				g.write("<<\"${env.value.name}\">>")
			}
			MethodMeta {
				g.write("<<\"${env.value.name}\">>")
			}
			EnumValueMeta {
				g.write("<<\"${env.value.name}\">>")
			}
			AttrMeta {
				g.write("<<\"${env.value.name}\">>")
			}
		}
		return true
	}
	return false
}

// comptime_selector_expr handles selector expressions on comptime vars like field.name
fn (mut g Gen) comptime_selector_expr(node ast.SelectorExpr) bool {
	// Check if the expression is a comptime var
	if node.expr is ast.Ident {
		if env := g.lookup_comptime_var(node.expr.name) {
			match env.value {
				FieldMeta {
					match node.field_name {
						'name' {
							g.write("<<\"${env.value.name}\">>")
							return true
						}
						'typ' {
							// Type - output as atom
							type_sym := g.table.sym(env.value.typ)
							g.write("'${type_sym.name}'")
							return true
						}
						'is_pub' {
							g.write(if env.value.is_pub { 'true' } else { 'false' })
							return true
						}
						'is_mut' {
							g.write(if env.value.is_mut { 'true' } else { 'false' })
							return true
						}
						else {}
					}
				}
				MethodMeta {
					match node.field_name {
						'name' {
							g.write("<<\"${env.value.name}\">>")
							return true
						}
						'return_type' {
							type_sym := g.table.sym(env.value.return_type)
							g.write("'${type_sym.name}'")
							return true
						}
						'args' {
							// Return array of args - this needs special handling
							// For now, return the count
							g.write('${env.value.params.len - 1}')  // -1 for receiver
							return true
						}
						'is_pub' {
							g.write(if env.value.is_pub { 'true' } else { 'false' })
							return true
						}
						else {}
					}
				}
				EnumValueMeta {
					match node.field_name {
						'name' {
							g.write("<<\"${env.value.name}\">>")
							return true
						}
						'value' {
							g.write('${env.value.value}')
							return true
						}
						else {}
					}
				}
				AttrMeta {
					match node.field_name {
						'name' {
							g.write("<<\"${env.value.name}\">>")
							return true
						}
						else {}
					}
				}
			}
		}
	}
	return false
}
