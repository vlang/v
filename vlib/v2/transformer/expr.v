// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.types
import v2.token

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
			// Lower `&{base | field: val}` (AssocExpr) in the transformer so backends do not
			// need to deal with address-of rvalue compound expressions.
			if expr.op == .amp {
				if assoc := t.unwrap_assoc_expr(expr.expr) {
					return t.lower_assoc_expr(assoc, true)
				}
			}
			ast.Expr(ast.PrefixExpr{
				op:   expr.op
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.PostfixExpr {
			// `expr!`/`expr?` should be lowered earlier.
			// Keep codegen valid by converting them to a cast of the underlying
			// result/option expression to the checker-inferred value type.
			if expr.op in [.not, .question] {
				inner := t.transform_expr(expr.expr)
				mut type_name := ''
				if inner_type := t.get_expr_type(expr.expr) {
					match inner_type {
						types.ResultType {
							type_name = t.type_to_c_name(inner_type.base_type)
						}
						types.OptionType {
							type_name = t.type_to_c_name(inner_type.base_type)
						}
						else {}
					}
				}
				if type_name == '' {
					if typ := t.get_expr_type(expr) {
						type_name = t.type_to_c_name(typ)
					}
				}
				if type_name != '' {
					return ast.CastExpr{
						typ:  ast.Expr(ast.Ident{
							name: type_name
						})
						expr: inner
					}
				}
				inner
			} else {
				ast.Expr(ast.PostfixExpr{
					op:   expr.op
					expr: t.transform_expr(expr.expr)
					pos:  expr.pos
				})
			}
		}
		ast.CastExpr {
			// Casts to sum types must be lowered to explicit sum type initialization,
			// since C does not allow casting a variant struct to the sum type struct.
			sumtype_name := t.type_expr_name_full(expr.typ)
			if sumtype_name != '' && t.is_sum_type(sumtype_name) {
				if wrapped := t.wrap_sumtype_value(expr.expr, sumtype_name) {
					return wrapped
				}
			}
			ast.Expr(ast.CastExpr{
				typ:  expr.typ
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.IndexExpr {
			t.transform_index_expr(expr)
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
			// Normalize `unsafe { nil }` to plain `nil`.
			// This keeps pointer-null semantics and avoids backend-specific null lowering bugs.
			if t.is_unsafe_nil_expr(expr) {
				ast.Expr(ast.Ident{
					name: 'nil'
					pos:  expr.pos
				})
			} else {
				ast.Expr(ast.UnsafeExpr{
					stmts: t.transform_stmts(expr.stmts)
				})
			}
		}
		ast.LockExpr {
			// Lower to mutex lock/unlock calls wrapped in UnsafeExpr (compound expression)
			mut stmts := t.expand_lock_expr(expr)
			// When used as a value expression (GCC compound expr), the last statement's
			// value is returned. But the unlock calls come after the body, making the
			// compound expr return void. Fix: duplicate the value-producing statement
			// (last body stmt) after the unlock calls so it becomes the final expression.
			n_unlocks := expr.lock_exprs.len + expr.rlock_exprs.len
			if n_unlocks > 0 && stmts.len > n_unlocks {
				body_end := stmts.len - n_unlocks
				stmts << stmts[body_end - 1]
			}
			ast.Expr(ast.UnsafeExpr{
				stmts: stmts
			})
		}
		ast.AssocExpr {
			t.lower_assoc_expr(expr, false)
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
			if expr.name == '@VMODROOT' {
				return ast.Expr(t.vmodroot_string_literal(expr.pos))
			}
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
			transformed_inner := t.transform_expr(expr.expr)
			target_type := t.type_expr_to_c_name(expr.typ)
			// If smartcast transformation already produced a concrete cast/deref to
			// the requested type, keep that expression and drop the redundant `as`.
			if target_type != '' && t.expr_is_casted_to_type(transformed_inner, target_type) {
				return transformed_inner
			}
			// Otherwise keep AsCastExpr for backend lowering.
			ast.Expr(ast.AsCastExpr{
				expr: transformed_inner
				typ:  expr.typ
				pos:  expr.pos
			})
		}
		ast.OrExpr {
			// OrExpr in expression context (e.g., nested, in return, in for-loop condition)
			mut prefix_stmts := []ast.Stmt{}
			result_expr := t.expand_single_or_expr(expr, mut prefix_stmts)
			if prefix_stmts.len > 0 {
				// Wrap in UnsafeExpr — cleanc emits as GCC compound expression ({ ... })
				prefix_stmts << ast.ExprStmt{
					expr: result_expr
				}
				ast.Expr(ast.UnsafeExpr{
					stmts: prefix_stmts
				})
			} else {
				result_expr
			}
		}
		ast.IfGuardExpr {
			// IfGuardExpr should only appear as IfExpr condition, handled by transform_if_expr.
			// If it somehow reaches here standalone, just evaluate the RHS.
			if expr.stmt.rhs.len > 0 {
				return t.transform_expr(expr.stmt.rhs[0])
			}
			expr
		}
		ast.GenericArgs {
			// Disambiguate `x[y]` parsed as GenericArgs: if lhs is not callable and there
			// is a single argument, this is an index expression.
			if expr.args.len == 1 {
				if lhs_type := t.get_expr_type(expr.lhs) {
					if !t.is_callable_type(lhs_type) {
						return t.transform_index_expr(ast.IndexExpr{
							lhs:      expr.lhs
							expr:     expr.args[0]
							is_gated: false
							pos:      expr.pos
						})
					}
				}
			}
			// Resolve generic specialization token: Foo[int] -> Foo_int
			lhs_name := expr.lhs.name()
			mut parts := []string{cap: expr.args.len}
			for arg in expr.args {
				parts << arg.name()
			}
			concrete_name := if parts.len > 0 {
				'${lhs_name}_${parts.join('_')}'
			} else {
				lhs_name
			}
			ast.Expr(ast.Ident{
				name: concrete_name
				pos:  expr.pos
			})
		}
		ast.GenericArgOrIndexExpr {
			// Disambiguate parser ambiguity `x[y]`:
			// - callable lhs => generic arg specialization token (`x_T`)
			// - otherwise => normal index expression
			if lhs_type := t.get_expr_type(expr.lhs) {
				if t.is_callable_type(lhs_type) {
					lhs_name := expr.lhs.name()
					arg_name := expr.expr.name()
					return ast.Expr(ast.Ident{
						name: '${lhs_name}_${arg_name}'
						pos:  expr.pos
					})
				}
			}
			return t.transform_index_expr(ast.IndexExpr{
				lhs:      expr.lhs
				expr:     expr.expr
				is_gated: false
				pos:      expr.pos
			})
		}
		ast.ModifierExpr {
			ast.Expr(ast.ModifierExpr{
				kind: expr.kind
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.FnLiteral {
			ast.Expr(ast.FnLiteral{
				typ:           expr.typ
				captured_vars: expr.captured_vars
				stmts:         t.transform_stmts(expr.stmts)
				pos:           expr.pos
			})
		}
		ast.LambdaExpr {
			ast.Expr(ast.LambdaExpr{
				args: expr.args
				expr: t.transform_expr(expr.expr)
				pos:  expr.pos
			})
		}
		ast.KeywordOperator {
			if expr.op == .key_typeof && expr.exprs.len > 0 {
				type_name := t.resolve_typeof_expr(expr.exprs[0])
				if type_name != '' {
					return ast.StringLiteral{
						kind:  .v
						value: quote_v_string_literal(type_name)
						pos:   expr.pos
					}
				}
			}
			expr
		}
		else {
			expr
		}
	}
}

fn (mut t Transformer) transform_index_expr(expr ast.IndexExpr) ast.Expr {
	// Lower slices in transformer so backends do not need slice-specific type logic.
	if expr.expr is ast.RangeExpr {
		lhs := t.transform_expr(expr.lhs)
		return t.transform_slice_index_expr(lhs, expr.lhs, expr.expr, expr.is_gated)
	}

	// Keep gated indexing as-is (`arr#[i]`).
	if expr.is_gated {
		return ast.IndexExpr{
			lhs:      t.transform_expr(expr.lhs)
			expr:     t.transform_expr(expr.expr)
			is_gated: expr.is_gated
			pos:      expr.pos
		}
	}

	// Lower map reads `m[key]` to `map__get(&m, &key, &zero)` in transformer so backends
	// do not need map-specific IndexExpr logic.
	mut map_expr_type_opt := t.get_expr_type(expr.lhs)
	if map_expr_type_opt == none && expr.lhs is ast.Ident {
		map_expr_type_opt = t.lookup_var_type((expr.lhs as ast.Ident).name)
	}
	if map_expr_typ := map_expr_type_opt {
		if map_type := t.unwrap_map_type(map_expr_typ) {
			synth_pos := t.next_synth_pos()

			mut stmts := []ast.Stmt{}

			// Map arg: map__get expects a pointer to the map.
			mut map_arg := ast.Expr(ast.empty_expr)
			lhs_val := t.transform_expr(expr.lhs)
			if t.is_pointer_type(map_expr_typ) {
				map_arg = lhs_val
			} else if t.can_take_address_expr(lhs_val) {
				map_arg = ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: lhs_val
				})
			} else {
				map_tmp := t.gen_temp_name()
				map_ident := ast.Ident{
					name: map_tmp
					pos:  synth_pos
				}
				t.register_temp_var(map_tmp, map_expr_typ)
				mut map_lhs := []ast.Expr{cap: 1}
				map_lhs << ast.Expr(map_ident)
				mut map_rhs := []ast.Expr{cap: 1}
				map_rhs << lhs_val
				stmts << ast.Stmt(ast.AssignStmt{
					op:  .decl_assign
					lhs: map_lhs
					rhs: map_rhs
					pos: synth_pos
				})
				map_arg = ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: map_ident
				})
			}

			// Key temp (so we can take its address safely for the duration of the whole expression).
			key_tmp := t.gen_temp_name()
			key_ident := ast.Ident{
				name: key_tmp
				pos:  synth_pos
			}
			t.register_temp_var(key_tmp, map_type.key_type)
			mut key_lhs := []ast.Expr{cap: 1}
			key_lhs << ast.Expr(key_ident)
			mut key_rhs := []ast.Expr{cap: 1}
			key_rhs << t.transform_expr(expr.expr)
			stmts << ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: key_lhs
				rhs: key_rhs
				pos: synth_pos
			})

			// Zero/default temp.
			zero_tmp := t.gen_temp_name()
			zero_ident := ast.Ident{
				name: zero_tmp
				pos:  synth_pos
			}
			t.register_temp_var(zero_tmp, map_type.value_type)
			mut zero_lhs := []ast.Expr{cap: 1}
			zero_lhs << ast.Expr(zero_ident)
			mut zero_rhs := []ast.Expr{cap: 1}
			zero_rhs << t.zero_value_expr_for_type(map_type.value_type)
			stmts << ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: zero_lhs
				rhs: zero_rhs
				pos: synth_pos
			})

			get_call := ast.CallExpr{
				lhs:  ast.Ident{
					name: 'map__get'
				}
				args: [
					map_arg,
					t.voidptr_cast(ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: key_ident
					})),
					t.voidptr_cast(ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: zero_ident
					})),
				]
			}
			cast_ptr_type := ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: t.type_to_ast_type_expr(map_type.value_type)
			})
			typed_ptr := ast.Expr(ast.CastExpr{
				typ:  cast_ptr_type
				expr: get_call
			})
			deref_expr := ast.Expr(ast.PrefixExpr{
				op:   .mul
				expr: typed_ptr
			})
			stmts << ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.ParenExpr{
					expr: deref_expr
				})
			})

			return ast.Expr(ast.UnsafeExpr{
				stmts: stmts
			})
		}
	}

	return ast.IndexExpr{
		lhs:      t.transform_expr(expr.lhs)
		expr:     t.transform_expr(expr.expr)
		is_gated: expr.is_gated
		pos:      expr.pos
	}
}

fn (mut t Transformer) transform_slice_index_expr(lhs ast.Expr, orig_lhs ast.Expr, range ast.RangeExpr, is_gated bool) ast.Expr {
	start_expr := if range.start is ast.EmptyExpr {
		ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '0'
		})
	} else {
		t.transform_expr(range.start)
	}

	// Build end expression for lowering target calls:
	// `a..b` -> b, `a...b` -> b + 1, `a..` -> lhs.len.
	mut end_expr := ast.Expr(ast.empty_expr)
	if range.end is ast.EmptyExpr {
		end_expr = t.synth_selector(lhs, 'len', types.Type(types.int_))
	} else {
		end_expr = t.transform_expr(range.end)
		if range.op == .ellipsis {
			end_expr = ast.Expr(ast.InfixExpr{
				op:  .plus
				lhs: end_expr
				rhs: ast.BasicLiteral{
					kind:  .number
					value: '1'
				}
			})
		}
	}

	// Prefer semantic string detection over position-based type tags.
	// Expression positions are often shared with parent index/slice nodes,
	// which can incorrectly stamp the source as array-like.
	if t.is_string_expr(orig_lhs) || t.is_string_expr(lhs) {
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: 'string__substr'
			}
			args: [lhs, start_expr, end_expr]
		}
	}

	if lhs_type := t.get_expr_type(orig_lhs) {
		match lhs_type {
			types.String {
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'string__substr'
					}
					args: [lhs, start_expr, end_expr]
				}
			}
			types.Alias {
				if lhs_type.name == 'string' || lhs_type.base_type is types.String {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'string__substr'
						}
						args: [lhs, start_expr, end_expr]
					}
				}
			}
			types.Array {
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'array__slice'
					}
					args: [lhs, start_expr, end_expr]
				}
			}
			types.ArrayFixed {
				elem_c_name := lhs_type.elem_type.name()
				return ast.CallExpr{
					lhs:  ast.Ident{
						name: 'new_array_from_c_array'
					}
					args: [
						end_expr,
						end_expr,
						ast.Expr(ast.KeywordOperator{
							op:    .key_sizeof
							exprs: [
								ast.Expr(ast.Ident{
									name: elem_c_name
								}),
							]
						}),
						lhs,
					]
				}
			}
			types.Pointer {
				if lhs_type.base_type is types.Array {
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'array__slice'
						}
						args: [
							ast.Expr(ast.PrefixExpr{
								op:   .mul
								expr: lhs
							}),
							start_expr,
							end_expr,
						]
					}
				}
			}
			else {}
		}
	}
	// Fallback when env type lookup misses selector/if-guard positions.
	if t.infer_expr_type(orig_lhs) == 'string' {
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: 'string__substr'
			}
			args: [lhs, start_expr, end_expr]
		}
	}

	// Type lookup failed; default to array__slice (most common case).
	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__slice'
		}
		args: [lhs, start_expr, end_expr]
	}
}

// transform_selector_expr transforms a selector expression, applying smart cast if applicable
fn (mut t Transformer) transform_selector_expr(expr ast.SelectorExpr) ast.Expr {
	// typeof(x).name -> string literal with V type name
	if expr.lhs is ast.KeywordOperator && expr.lhs.op == .key_typeof && expr.rhs.name == 'name' {
		if expr.lhs.exprs.len > 0 {
			type_name := t.resolve_typeof_expr(expr.lhs.exprs[0])
			if type_name != '' {
				return ast.StringLiteral{
					kind:  .v
					value: quote_v_string_literal(type_name)
					pos:   expr.pos
				}
			}
		}
	}
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
	// Handle module-qualified enum value access: module.EnumType.value -> module__EnumType__value
	// Also handle nested module references: rand.seed.time_seed_array -> seed__time_seed_array
	if expr.lhs is ast.SelectorExpr {
		lhs_sel := expr.lhs as ast.SelectorExpr
		if lhs_sel.lhs is ast.Ident {
			module_name := lhs_sel.lhs.name
			type_name := lhs_sel.rhs.name
			qualified := '${module_name}__${type_name}'
			if typ := t.lookup_type(qualified) {
				if typ is types.Enum {
					return ast.Ident{
						name: '${qualified}__${expr.rhs.name}'
						pos:  expr.pos
					}
				}
			}
			// Nested module reference: rand.seed.time_seed_array -> seed__time_seed_array
			// Only resolve when both the outer ident is a module AND the inner name
			// is also a sub-module (not a variable like os.args.len).
			if t.is_module_ident(module_name) {
				sub_mod := lhs_sel.rhs.name
				fn_name := expr.rhs.name
				// Check if sub_mod is actually a module scope
				if t.get_module_scope(sub_mod) != none {
					return ast.Ident{
						name: '${sub_mod}__${fn_name}'
						pos:  expr.pos
					}
				}
				// Try full qualified name (module__sub_mod as scope key)
				full_mod := '${module_name}__${sub_mod}'
				if t.get_module_scope(full_mod) != none {
					return ast.Ident{
						name: '${full_mod}__${fn_name}'
						pos:  expr.pos
					}
				}
			}
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
fn (mut t Transformer) transform_match_expr(expr ast.MatchExpr) ast.Expr {
	// Check if matching on a sum type
	mut sumtype_name := t.get_sumtype_name_for_expr(expr.expr)
	smartcast_expr := t.expr_to_string(expr.expr)

	// Verify that it's actually a sum type match by checking branch conditions.
	// If conditions are string/number literals, it's NOT a sum type match even if
	// the expression type looks like a sum type.
	if sumtype_name != '' && expr.branches.len > 0 {
		first_branch := expr.branches[0]
		if first_branch.cond.len > 0 {
			first_cond := first_branch.cond[0]
			if first_cond is ast.BasicLiteral || first_cond is ast.StringLiteral
				|| first_cond is ast.StringInterLiteral {
				sumtype_name = ''
			}
		}
	}

	if sumtype_name != '' {
		// Sum type match - set up smartcast context for each branch
		variants := t.get_sum_type_variants(sumtype_name)

		mut branches := []ast.MatchBranch{cap: expr.branches.len}
		for branch in expr.branches {
			if branch.cond.len > 0 {
				mut cond_tags := []int{cap: branch.cond.len}
				mut cond_variants := []string{cap: branch.cond.len}
				mut cond_variants_full := []string{cap: branch.cond.len}
				mut can_split_branch := true

				for c in branch.cond {
					mut c_variant_name := ''
					mut c_variant_name_full := ''
					mut c_variant_module := ''

					if c is ast.Ident {
						c_variant_name = c.name
						c_variant_name_full = if t.cur_module != '' && t.cur_module != 'main'
							&& t.cur_module != 'builtin' {
							'${t.cur_module}__${c.name}'
						} else {
							c.name
						}
					} else if c is ast.SelectorExpr {
						c_variant_name = c.rhs.name
						if c.lhs is ast.Ident {
							c_variant_module = (c.lhs as ast.Ident).name
							c_variant_name_full = '${c_variant_module}__${c.rhs.name}'
						} else {
							c_variant_name_full = c.rhs.name
						}
					} else if c is ast.Type {
						// Handle type variants like []ast.Attribute
						c_variant_name = t.type_variant_name(c)
						c_variant_name_full = t.type_variant_name_full(c)
					}

					if c_variant_name == '' {
						can_split_branch = false
						break
					}

					qualified_variant := if c_variant_module != ''
						&& !c_variant_name.starts_with('Array_')
						&& !c_variant_name.starts_with('Map_') {
						'${c_variant_module}__${c_variant_name}'
					} else {
						c_variant_name
					}
					qualified_variant_full := if c_variant_name_full != ''
						&& c_variant_name_full != c_variant_name {
						c_variant_name_full
					} else if c_variant_module != '' {
						'${c_variant_module}__${c_variant_name}'
					} else {
						c_variant_name
					}

					mut c_tag := -1
					for i, v in variants {
						v_short := if v.contains('__') { v.all_after_last('__') } else { v }
						// For array types, also try matching with [] prefix
						if v == c_variant_name || v_short == c_variant_name {
							c_tag = i
							break
						}
						// Handle array variant matching:
						// c_variant_name is 'Array_Attribute' or 'Array_ast__Attribute' (C format)
						// v is '[]Attribute' or '[]ast__Attribute' (V format from types.Array.name())
						if c_variant_name.starts_with('Array_') && v.starts_with('[]') {
							c_elem := c_variant_name[6..] // Strip 'Array_'
							v_elem := v[2..] // Strip '[]'
							c_elem_short := if c_elem.contains('__') {
								c_elem.all_after_last('__')
							} else {
								c_elem
							}
							v_elem_short := if v_elem.contains('__') {
								v_elem.all_after_last('__')
							} else {
								v_elem
							}
							if c_elem == v_elem || c_elem_short == v_elem_short {
								c_tag = i
								break
							}
						}
						// Handle fixed array variant matching
						if c_variant_name.starts_with('Array_fixed_') && v.starts_with('[') {
							// TODO: implement fixed array matching if needed
						}
						// Handle map variant matching
						if c_variant_name.starts_with('Map_') && v.starts_with('map[') {
							// TODO: implement map matching if needed
						}
					}

					if c_tag < 0 {
						can_split_branch = false
						break
					}
					cond_tags << c_tag
					cond_variants << qualified_variant
					cond_variants_full << qualified_variant_full
				}

				if can_split_branch && cond_tags.len > 0 {
					// When a branch has multiple sum variants, each condition needs its own
					// smartcast context. Splitting preserves correct dispatch in branch bodies.
					for i, c_tag in cond_tags {
						t.push_smartcast_full(smartcast_expr, cond_variants[i], cond_variants_full[i],
							sumtype_name)
						mut transformed_stmts := t.transform_stmts(branch.stmts)
						if t.sumtype_return_wrap != '' && transformed_stmts.len > 0 {
							last_idx := transformed_stmts.len - 1
							if transformed_stmts[last_idx] is ast.ExprStmt {
								last_expr := (transformed_stmts[last_idx] as ast.ExprStmt).expr
								if wrapped := t.wrap_sumtype_value_transformed(last_expr,
									t.sumtype_return_wrap)
								{
									transformed_stmts[last_idx] = ast.Stmt(ast.ExprStmt{
										expr: wrapped
									})
								}
							}
						}
						t.pop_smartcast()

						branches << ast.MatchBranch{
							cond:  [
								ast.Expr(ast.BasicLiteral{
									kind:  token.Token.number
									value: '${c_tag}'
								}),
							]
							stmts: transformed_stmts
							pos:   branch.pos
						}
					}
				} else {
					// No variant name found, just transform normally
					mut fallback_stmts := t.transform_stmts(branch.stmts)
					if t.sumtype_return_wrap != '' && fallback_stmts.len > 0 {
						last_idx := fallback_stmts.len - 1
						if fallback_stmts[last_idx] is ast.ExprStmt {
							last_expr := (fallback_stmts[last_idx] as ast.ExprStmt).expr
							if wrapped := t.wrap_sumtype_value_transformed(last_expr,
								t.sumtype_return_wrap)
							{
								fallback_stmts[last_idx] = ast.Stmt(ast.ExprStmt{
									expr: wrapped
								})
							}
						}
					}
					branches << ast.MatchBranch{
						cond:  branch.cond
						stmts: fallback_stmts
						pos:   branch.pos
					}
				}
			} else {
				// else branch - no smartcast context
				mut else_stmts := t.transform_stmts(branch.stmts)
				if t.sumtype_return_wrap != '' && else_stmts.len > 0 {
					last_idx := else_stmts.len - 1
					if else_stmts[last_idx] is ast.ExprStmt {
						last_expr := (else_stmts[last_idx] as ast.ExprStmt).expr
						if wrapped := t.wrap_sumtype_value_transformed(last_expr, t.sumtype_return_wrap) {
							else_stmts[last_idx] = ast.Stmt(ast.ExprStmt{
								expr: wrapped
							})
						}
					}
				}
				branches << ast.MatchBranch{
					cond:  branch.cond
					stmts: else_stmts
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
			if existing_ctx := t.remove_smartcast_for_expr(smartcast_expr) {
				removed_contexts << existing_ctx
			} else {
				break
			}
		}
		transformed_match_expr := t.transform_expr(expr.expr)
		// Re-add the contexts in reverse order (to preserve original order)
		for i := removed_contexts.len - 1; i >= 0; i-- {
			ctx := removed_contexts[i]
			t.push_smartcast_full(ctx.expr, ctx.variant, ctx.variant_full, ctx.sumtype)
		}
		tag_access := t.synth_selector(transformed_match_expr, '_tag', types.Type(types.int_))

		return t.lower_match_expr_to_if(tag_access, branches)
	}

	// Non-sum type match - simple transformation
	// Determine if match expression is an enum type so we can resolve shorthands (.red → Color__red)
	mut enum_type_name := ''
	if typ := t.get_expr_type(expr.expr) {
		c_name := t.type_to_c_name(typ)
		if c_name != '' {
			if resolved := t.lookup_type(c_name) {
				if resolved is types.Enum {
					enum_type_name = c_name
				}
			}
		}
	}
	mut branches := []ast.MatchBranch{cap: expr.branches.len}
	for branch in expr.branches {
		mut conds := branch.cond.clone()
		if enum_type_name != '' {
			conds = []ast.Expr{cap: branch.cond.len}
			for c in branch.cond {
				conds << t.resolve_enum_shorthand(c, enum_type_name)
			}
		}
		branches << ast.MatchBranch{
			cond:  conds
			stmts: t.transform_stmts(branch.stmts)
			pos:   branch.pos
		}
	}
	return t.lower_match_expr_to_if(t.transform_expr(expr.expr), branches)
}

// lower_match_expr_to_if converts a transformed match expression into a nested IfExpr chain.
// Backends only need to support IfExpr after this lowering.
fn (mut t Transformer) lower_match_expr_to_if(match_expr ast.Expr, branches []ast.MatchBranch) ast.Expr {
	is_match_true := match_expr is ast.BasicLiteral && match_expr.kind == .key_true
	is_match_false := match_expr is ast.BasicLiteral && match_expr.kind == .key_false

	mut current := ast.Expr(ast.empty_expr)
	for i := branches.len - 1; i >= 0; i-- {
		branch := branches[i]
		if branch.cond.len == 0 {
			current = ast.Expr(ast.IfExpr{
				cond:      ast.empty_expr
				stmts:     branch.stmts
				else_expr: current
			})
			continue
		}

		branch_cond := t.build_match_branch_cond(match_expr, branch.cond, is_match_true,
			is_match_false)
		current = ast.Expr(ast.IfExpr{
			cond:      branch_cond
			stmts:     branch.stmts
			else_expr: current
		})
	}
	return current
}

fn (mut t Transformer) transform_if_expr(expr ast.IfExpr) ast.Expr {
	// Normalize long && chains containing `is` checks so smartcast lowering can
	// apply to patterns like `a && x is T && b`.
	if expr.cond is ast.InfixExpr && expr.cond.op == token.Token.and {
		terms := t.flatten_and_terms(expr.cond)
		if terms.len > 2 {
			mut is_idx := -1
			for i, term in terms {
				if term is ast.InfixExpr && term.op in [.key_is, .eq]
					&& t.smartcast_context_from_is_check(term) != none {
					is_idx = i
					break
				}
			}
			if is_idx >= 0 {
				pre_terms := if is_idx > 0 { terms[..is_idx] } else { []ast.Expr{} }
				post_terms := if is_idx + 1 < terms.len { terms[is_idx + 1..] } else { []ast.Expr{} }
				mut inner_cond := terms[is_idx]
				if post_terms.len > 0 {
					inner_cond = ast.Expr(ast.InfixExpr{
						op:  token.Token.and
						lhs: inner_cond
						rhs: t.join_and_terms(post_terms)
					})
				}
				inner_if := ast.IfExpr{
					cond:      inner_cond
					stmts:     expr.stmts
					else_expr: expr.else_expr
					pos:       expr.pos
				}
				if pre_terms.len > 0 {
					outer_if := ast.IfExpr{
						cond:      t.join_and_terms(pre_terms)
						stmts:     [ast.Stmt(ast.ExprStmt{
							expr: inner_if
						})]
						else_expr: expr.else_expr
						pos:       expr.pos
					}
					return t.transform_if_expr(outer_if)
				}
			}
		}
	}

	// Check for compound && condition with is check: if x is Type && cond { ... }
	// Transform to nested ifs: if x is Type { if cond { ... } }
	// This allows the smart cast context to be active for the inner condition
	if expr.cond is ast.InfixExpr {
		cond := expr.cond as ast.InfixExpr
		if cond.op == .and {
			// Check if LHS is an is-check
			if cond.lhs is ast.InfixExpr {
				lhs_infix := cond.lhs as ast.InfixExpr
				if lhs_infix.op in [.key_is, .eq]
					&& t.smartcast_context_from_is_check(lhs_infix) != none {
					// Transform: if x is Type && rest { body } else { else_body }
					// Handle directly to ensure else_body is transformed WITHOUT smartcast
					// Get variant info from lhs_infix (the is-check)
					mut variant_name := ''
					mut variant_module := ''
					if lhs_infix.rhs is ast.Ident {
						variant_name = (lhs_infix.rhs as ast.Ident).name
					} else if lhs_infix.rhs is ast.SelectorExpr {
						sel := lhs_infix.rhs as ast.SelectorExpr
						variant_name = sel.rhs.name
						if sel.lhs is ast.Ident {
							variant_module = (sel.lhs as ast.Ident).name
						}
					}
					if variant_name != '' {
						mut sumtype_name := t.get_sumtype_name_for_expr(lhs_infix.lhs)
						if sumtype_name == '' {
							sumtype_name = t.find_sumtype_for_variant(variant_name)
						}
						if sumtype_name != '' {
							variants := t.get_sum_type_variants(sumtype_name)
							mut tag_value := -1
							for i, v in variants {
								v_short := if v.contains('__') {
									v.all_after_last('__')
								} else {
									v
								}
								if v == variant_name || v_short == variant_name {
									tag_value = i
									break
								}
							}
							if tag_value >= 0 {
								smartcast_expr := t.expr_to_string(lhs_infix.lhs)
								qualified_variant := if variant_module != '' {
									'${variant_module}__${variant_name}'
								} else {
									variant_name
								}
								// For full variant name (type casts), always include module prefix
								qualified_variant_full := if variant_module != '' {
									'${variant_module}__${variant_name}'
								} else if t.cur_module != '' && t.cur_module != 'main'
									&& t.cur_module != 'builtin' {
									'${t.cur_module}__${variant_name}'
								} else {
									variant_name
								}
								// Transform LHS for tag check. Remove non-sum-type smartcasts
								// (e.g., EmptyExpr = u8) to avoid accessing ._tag on a non-struct.
								mut removed_for_tag2 := []SmartcastRemoveResult{}
								if lhs_infix.lhs is ast.Ident {
									lhs_id_name := (lhs_infix.lhs as ast.Ident).name
									for {
										if existing := t.remove_smartcast_for_expr_with_idx(lhs_id_name) {
											if t.is_sum_type(existing.ctx.variant) {
												t.insert_smartcast_at(existing.idx, existing.ctx)
												break
											}
											removed_for_tag2 << existing
										} else {
											break
										}
									}
								}
								transformed_lhs := t.transform_expr(lhs_infix.lhs)
								for i2 := removed_for_tag2.len - 1; i2 >= 0; i2-- {
									entry2 := removed_for_tag2[i2]
									t.insert_smartcast_at(entry2.idx, entry2.ctx)
								}
								// Push smartcast for body and inner condition
								t.push_smartcast_full(smartcast_expr, qualified_variant,
									qualified_variant_full, sumtype_name)
								// Check if inner condition (rest) is also an is-check
								mut inner_cond_lowered := false
								mut inner_tag_check := ast.Expr(ast.BasicLiteral{
									kind:  token.Token.number
									value: '0'
								})
								mut inner_smartcast_pushed := false
								if cond.rhs is ast.InfixExpr
									&& (cond.rhs as ast.InfixExpr).op in [.key_is, .eq] && t.smartcast_context_from_is_check(cond.rhs as ast.InfixExpr) != none {
									orig_inner := cond.rhs as ast.InfixExpr
									mut inner_vname := ''
									mut inner_vmodule := ''
									if orig_inner.rhs is ast.Ident {
										inner_vname = (orig_inner.rhs as ast.Ident).name
									} else if orig_inner.rhs is ast.SelectorExpr {
										inner_sel := orig_inner.rhs as ast.SelectorExpr
										inner_vname = inner_sel.rhs.name
										if inner_sel.lhs is ast.Ident {
											inner_vmodule = (inner_sel.lhs as ast.Ident).name
										}
									}
									if inner_vname != '' {
										// Get sum type from ORIGINAL expression (before smartcast)
										mut inner_stype := t.get_sumtype_name_for_expr(orig_inner.lhs)
										if inner_stype == '' {
											inner_stype = t.find_sumtype_for_variant(inner_vname)
										}
										if inner_stype != '' {
											inner_variants := t.get_sum_type_variants(inner_stype)
											mut inner_tag := -1
											for iv, iv_name in inner_variants {
												iv_short := if iv_name.contains('__') {
													iv_name.all_after_last('__')
												} else {
													iv_name
												}
												if iv_name == inner_vname || iv_short == inner_vname {
													inner_tag = iv
													break
												}
											}
											if inner_tag >= 0 {
												// Transform inner LHS with outer smartcast
												transformed_inner_lhs := t.transform_expr(orig_inner.lhs)
												inner_tag_check = ast.Expr(ast.InfixExpr{
													op:  token.Token.eq
													lhs: t.synth_selector(transformed_inner_lhs,
														'_tag', types.Type(types.int_))
													rhs: ast.BasicLiteral{
														kind:  token.Token.number
														value: '${inner_tag}'
													}
												})
												// Push inner smartcast for body
												inner_qv := if inner_vmodule != '' {
													'${inner_vmodule}__${inner_vname}'
												} else {
													inner_vname
												}
												inner_qvf := if inner_vmodule != '' {
													'${inner_vmodule}__${inner_vname}'
												} else if t.cur_module != ''
													&& t.cur_module != 'main'
													&& t.cur_module != 'builtin' {
													'${t.cur_module}__${inner_vname}'
												} else {
													inner_vname
												}
												inner_se := t.expr_to_string(orig_inner.lhs)
												t.push_smartcast_full(inner_se, inner_qv,
													inner_qvf, inner_stype)
												inner_smartcast_pushed = true
												inner_cond_lowered = true
											}
										}
									}
								}
								// Transform inner condition with smartcast (if not already lowered)
								transformed_rest := if inner_cond_lowered {
									inner_tag_check
								} else {
									t.transform_expr(cond.rhs)
								}
								// Transform body with smartcast(s)
								transformed_body := t.transform_stmts(expr.stmts)
								// Pop inner smartcast if pushed
								if inner_smartcast_pushed {
									t.pop_smartcast()
								}
								// Pop outer smartcast BEFORE transforming else
								t.pop_smartcast()
								// Transform else WITHOUT smartcast
								transformed_else := t.transform_expr(expr.else_expr)
								// Build tag check
								tag_check := ast.InfixExpr{
									op:  token.Token.eq
									lhs: t.synth_selector(transformed_lhs, '_tag', types.Type(types.int_))
									rhs: ast.BasicLiteral{
										kind:  token.Token.number
										value: '${tag_value}'
									}
									pos: lhs_infix.pos
								}
								// Build inner if (with already-transformed components)
								inner_if := ast.IfExpr{
									cond:      transformed_rest
									stmts:     transformed_body
									else_expr: transformed_else
									pos:       expr.pos
								}
								// Build outer if
								return ast.IfExpr{
									cond:      tag_check
									stmts:     [
										ast.Stmt(ast.ExprStmt{
											expr: inner_if
										}),
									]
									else_expr: transformed_else
									pos:       expr.pos
								}
							}
						}
					}
					// Fallback: Even without tag info, we need to handle smartcast correctly.
					// The outer condition (lhs_infix) is an is-check that should push smartcast.
					// Transform else_expr FIRST without smartcast, use pre-transformed version.
					transformed_else_fallback := t.transform_expr(expr.else_expr)
					// For the outer condition, transform LHS (for nested smartcasts)
					transformed_outer_lhs := t.transform_expr(lhs_infix.lhs)
					// We can't generate tag check without knowing the tag, so just keep the is-check
					// (cleanc will handle it), but we need smartcast for body/inner condition
					smartcast_expr := t.expr_to_string(lhs_infix.lhs)
					// Get variant name for smartcast context
					mut fallback_variant := variant_name
					if variant_module != '' {
						fallback_variant = '${variant_module}__${variant_name}'
					}
					// For full variant name (type casts), always include module prefix
					fallback_variant_full := if variant_module != '' {
						'${variant_module}__${variant_name}'
					} else if t.cur_module != '' && t.cur_module != 'main'
						&& t.cur_module != 'builtin' {
						'${t.cur_module}__${variant_name}'
					} else {
						variant_name
					}
					// Use empty sumtype name since we couldn't find it
					t.push_smartcast_full(smartcast_expr, fallback_variant, fallback_variant_full,
						'')
					// Transform inner condition and body with smartcast
					transformed_rest_fallback := t.transform_expr(cond.rhs)
					transformed_body_fallback := t.transform_stmts(expr.stmts)
					// Pop smartcast before using pre-transformed else
					t.pop_smartcast()
					inner_if := ast.IfExpr{
						cond:      transformed_rest_fallback
						stmts:     transformed_body_fallback
						else_expr: transformed_else_fallback
						pos:       expr.pos
					}
					// Keep original is-check condition (let cleanc handle it)
					outer_if := ast.IfExpr{
						cond:      ast.InfixExpr{
							op:  lhs_infix.op
							lhs: transformed_outer_lhs
							rhs: lhs_infix.rhs
							pos: lhs_infix.pos
						}
						stmts:     [ast.Stmt(ast.ExprStmt{
							expr: inner_if
						})]
						else_expr: transformed_else_fallback
						pos:       expr.pos
					}
					return outer_if
				}
			}
			// Check if RHS is an is-check: if cond && x is Type { ... }
			// Transform to: if cond { if x is Type { ... } else { else_body } } else { else_body }
			if cond.rhs is ast.InfixExpr {
				rhs_infix := cond.rhs as ast.InfixExpr
				if rhs_infix.op in [.key_is, .eq]
					&& t.smartcast_context_from_is_check(rhs_infix) != none {
					// Transform: if cond && x is Type { body } else { else_body }
					// To: if cond { if x is Type { body } else { else_body } } else { else_body }
					inner_if := ast.IfExpr{
						cond:      ast.Expr(cond.rhs)
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
		if cond.op in [.key_is, .eq] && t.smartcast_context_from_is_check(cond) != none {
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
						// For full variant name (type casts), always include module prefix
						qualified_variant_full := if variant_module != '' {
							'${variant_module}__${variant_name}'
						} else if t.cur_module != '' && t.cur_module != 'main'
							&& t.cur_module != 'builtin' {
							'${t.cur_module}__${variant_name}'
						} else {
							variant_name
						}

						// Transform cond.lhs WITH active smartcast contexts for nested sum types.
						// For nested smartcasts (e.g., match x { Number { if x is Point } }),
						// we need the outer smartcast (x -> Number) to be applied so that
						// the tag check accesses the inner sum type's tag, not the outer's.
						// e.g., (*((Number*)(x._data._Number)))._tag == 1, not x._tag == 1
						// BUT: if the outer smartcast's variant is NOT a sum type (e.g., EmptyExpr = u8),
						// then applying it would dereference to a non-struct and ._tag access fails.
						// In that case, remove the smartcast for the tag check.
						mut removed_for_tag := []SmartcastRemoveResult{}
						if cond.lhs is ast.Ident {
							lhs_ident_name := (cond.lhs as ast.Ident).name
							for {
								if existing := t.remove_smartcast_for_expr_with_idx(lhs_ident_name) {
									if t.is_sum_type(existing.ctx.variant) {
										// Variant is itself a sum type - restore and keep for nested access
										t.insert_smartcast_at(existing.idx, existing.ctx)
										break
									}
									removed_for_tag << existing
								} else {
									break
								}
							}
						}
						transformed_lhs := t.transform_expr(cond.lhs)
						// Restore removed smartcasts
						for i := removed_for_tag.len - 1; i >= 0; i-- {
							entry := removed_for_tag[i]
							t.insert_smartcast_at(entry.idx, entry.ctx)
						}

						// Push smart cast context for transforming body (supports nested smartcasts)
						t.push_smartcast_full(smartcast_expr, qualified_variant, qualified_variant_full,
							sumtype_name)

						// Transform body with smart cast context
						transformed_stmts := t.transform_stmts(expr.stmts)

						// Pop context
						t.pop_smartcast()

						// Transform condition: v is Type -> v._tag == TAG_VALUE
						// This prevents cleanc from also applying smart cast
						// Use transformed_lhs to apply outer smartcasts to the tag check
						tag_check := ast.InfixExpr{
							op:  token.Token.eq
							lhs: t.synth_selector(transformed_lhs, '_tag', types.Type(types.int_))
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

			// Check if RHS returns Result or Option type
			mut is_result := t.expr_returns_result(rhs)
			mut is_option := t.expr_returns_option(rhs)
			if !is_result && !is_option {
				fn_name := t.get_call_fn_name(rhs)
				is_result = fn_name != '' && t.fn_returns_result(fn_name)
				is_option = fn_name != '' && t.fn_returns_option(fn_name)
			}
			// Native backends (arm64/x64) don't use Option/Result structs -
			// functions return raw values (0 for none). Use temp variable +
			// truthiness check to avoid double-calling the function.
			if t.pref != unsafe { nil } && (t.pref.backend == .arm64 || t.pref.backend == .x64) {
				if is_option || is_result {
					temp_name := t.gen_temp_name()
					temp_ident := ast.Ident{
						name: temp_name
						pos:  synth_pos
					}
					// 1. _tmp := call()
					temp_assign := ast.AssignStmt{
						op:  .decl_assign
						lhs: [ast.Expr(temp_ident)]
						rhs: [t.transform_expr(rhs)]
						pos: synth_pos
					}
					// 2. Body: guard_var := _tmp; original_body
					mut body_stmts := []ast.Stmt{}
					mut is_blank := false
					if guard.stmt.lhs.len == 1 {
						lhs0 := guard.stmt.lhs[0]
						if lhs0 is ast.Ident {
							if lhs0.name == '_' {
								is_blank = true
							}
						}
					}
					if !is_blank {
						body_stmts << ast.AssignStmt{
							op:  .decl_assign
							lhs: guard.stmt.lhs
							rhs: [ast.Expr(temp_ident)]
							pos: guard.stmt.pos
						}
					}
					for s in expr.stmts {
						body_stmts << s
					}
					// 3. Condition: truthiness of _tmp (0 = none for native backends)
					modified_if := ast.IfExpr{
						cond:      temp_ident
						stmts:     t.transform_stmts(body_stmts)
						else_expr: t.transform_expr(expr.else_expr)
						pos:       synth_pos
					}
					if orig_type := t.get_expr_type(ast.Expr(expr)) {
						t.register_synth_type(synth_pos, orig_type)
					}
					return ast.UnsafeExpr{
						stmts: [ast.Stmt(temp_assign), ast.ExprStmt{
							expr: modified_if
						}]
					}
				}
				is_result = false
				is_option = false
			}

			if is_result {
				// Handle Result if-guard using temp variable pattern
				// Transform: if var := result_call() { body } else { else_body }
				// To: { _tmp := result_call(); if !_tmp.is_error { var := _tmp.data; body } else { else_body } }
				temp_name := t.gen_temp_name()
				temp_ident := ast.Ident{
					name: temp_name
					pos:  synth_pos
				}

				mut is_blank := false
				if guard.stmt.lhs.len == 1 {
					lhs0 := guard.stmt.lhs[0]
					if lhs0 is ast.Ident {
						if lhs0.name == '_' {
							is_blank = true
						}
					}
				}

				// 1. _tmp := result_call()
				temp_assign := ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(temp_ident)]
					rhs: [t.transform_expr(rhs)]
					pos: synth_pos
				}

				// 2. Condition: !_tmp.is_error
				success_cond := ast.PrefixExpr{
					op:   .not
					expr: t.synth_selector(temp_ident, 'is_error', types.Type(types.bool_))
				}

				// 3. Body: var := _tmp.data; original_body
				mut body_stmts := []ast.Stmt{}
				if !is_blank {
					data_access := t.synth_selector(temp_ident, 'data', types.Type(types.voidptr_))
					body_stmts << ast.AssignStmt{
						op:  .decl_assign
						lhs: guard.stmt.lhs
						rhs: [data_access]
						pos: guard.stmt.pos
					}
				}
				for s in expr.stmts {
					body_stmts << s
				}

				modified_if := ast.IfExpr{
					cond:      success_cond
					stmts:     t.transform_stmts(body_stmts)
					else_expr: t.transform_expr(expr.else_expr)
					pos:       synth_pos
				}
				// Propagate the original IfExpr type to the synthesized node
				if orig_type := t.get_expr_type(ast.Expr(expr)) {
					t.register_synth_type(synth_pos, orig_type)
				}

				// Wrap temp assignment + if in UnsafeExpr (compound expression)
				return ast.UnsafeExpr{
					stmts: [ast.Stmt(temp_assign), ast.ExprStmt{
						expr: modified_if
					}]
				}
			}

			if is_option {
				// Handle Option if-guard using a temp variable to avoid taking
				// addresses of rvalue wrappers when extracting `.data`.
				// Transform: if var := opt_call() { body } else { else_body }
				// To: { _tmp := opt_call(); if _tmp.state == 0 { var := _tmp.data; body } else { else_body } }
				temp_name := t.gen_temp_name()
				temp_ident := ast.Ident{
					name: temp_name
					pos:  synth_pos
				}
				mut is_blank := false
				if guard.stmt.lhs.len == 1 {
					lhs0 := guard.stmt.lhs[0]
					if lhs0 is ast.Ident {
						if lhs0.name == '_' {
							is_blank = true
						}
					}
				}

				temp_assign := ast.AssignStmt{
					op:  .decl_assign
					lhs: [ast.Expr(temp_ident)]
					rhs: [t.transform_expr(rhs)]
					pos: synth_pos
				}

				opt_success_cond := ast.InfixExpr{
					op:  .eq
					lhs: t.synth_selector(temp_ident, 'state', types.Type(types.int_))
					rhs: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				}

				mut body_stmts := []ast.Stmt{}
				if !is_blank {
					data_access := t.synth_selector(temp_ident, 'data', types.Type(types.voidptr_))
					body_stmts << ast.AssignStmt{
						op:  .decl_assign
						lhs: guard.stmt.lhs
						rhs: [data_access]
						pos: guard.stmt.pos
					}
				}
				for s in expr.stmts {
					body_stmts << s
				}

				modified_if := ast.IfExpr{
					cond:      opt_success_cond
					stmts:     t.transform_stmts(body_stmts)
					else_expr: t.transform_expr(expr.else_expr)
					pos:       synth_pos
				}
				// Propagate the original IfExpr type to the synthesized node
				if orig_type := t.get_expr_type(ast.Expr(expr)) {
					t.register_synth_type(synth_pos, orig_type)
				}

				return ast.UnsafeExpr{
					stmts: [ast.Stmt(temp_assign), ast.ExprStmt{
						expr: modified_if
					}]
				}
			}

			// Non-option case: use simple transformation
			// For map if-guards: if r := map[key] { body } else { else_body }
			// Transform to: if (key in map) { r := map[key]; body } else { else_body }
			// For other cases: if (rhs) { r := rhs; body } else { else_body }
			mut is_blank := false
			if guard.stmt.lhs.len == 1 {
				lhs0 := guard.stmt.lhs[0]
				if lhs0 is ast.Ident {
					if lhs0.name == '_' {
						is_blank = true
					}
				}
			}

			// Check if RHS is a map or array index expression
			mut cond_expr := ast.Expr(t.transform_expr(rhs))
			if rhs is ast.IndexExpr {
				// Try to see if this is a map index
				if _ := t.get_map_type_for_expr(rhs.lhs) {
					// This is a map access - generate "key in map" check
					cond_expr = ast.Expr(ast.InfixExpr{
						op:  .key_in
						lhs: rhs.expr // the key expression
						rhs: rhs.lhs  // the map expression
						pos: rhs.pos
					})
				} else {
					// This is an array access - generate bounds check: index < array.len
					cond_expr = ast.Expr(ast.InfixExpr{
						op:  .lt
						lhs: t.transform_expr(rhs.expr) // the index expression
						rhs: t.synth_selector(t.transform_expr(rhs.lhs), 'len', types.Type(types.int_))
						pos: rhs.pos
					})
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
			result_if := ast.IfExpr{
				cond:      t.transform_expr(cond_expr)
				stmts:     t.transform_stmts(new_stmts)
				else_expr: t.transform_expr(expr.else_expr)
				pos:       synth_pos
			}
			// Propagate the original IfExpr type to the synthesized node
			if orig_type := t.get_expr_type(ast.Expr(expr)) {
				t.register_synth_type(synth_pos, orig_type)
			}
			return result_if
		}
	}

	// Default transformation
	mut body_smartcasts := []SmartcastContext{}
	mut seen_smartcasts := map[string]bool{}
	for term in t.flatten_and_terms(expr.cond) {
		if term is ast.InfixExpr {
			if ctx := t.smartcast_context_from_is_check(term) {
				key := '${ctx.expr}|${ctx.variant}|${ctx.variant_full}'
				if key !in seen_smartcasts {
					seen_smartcasts[key] = true
					body_smartcasts << ctx
				}
			}
		}
	}
	for ctx in body_smartcasts {
		t.push_smartcast_full(ctx.expr, ctx.variant, ctx.variant_full, ctx.sumtype)
	}
	transformed_stmts := t.transform_stmts(expr.stmts)
	for _ in body_smartcasts {
		t.pop_smartcast()
	}
	transformed_if := ast.IfExpr{
		cond:      t.transform_expr(expr.cond)
		stmts:     transformed_stmts
		else_expr: t.transform_expr(expr.else_expr)
		pos:       expr.pos
	}
	// Lower value-position IfExpr: hoist to a temp variable assignment.
	// This eliminates expression-valued IfExpr so backends don't need statement-expressions.
	// A value-position if has an else branch and its body ends with an ExprStmt (producing a value).
	// Skip lowering when:
	// - The IfExpr is directly inside an ExprStmt (statement position, not value)
	// - The IfExpr is already the RHS of a decl_assign (cleanc handles this efficiently)
	if !t.skip_if_value_lowering && transformed_if.else_expr !is ast.EmptyExpr
		&& t.if_expr_is_value(transformed_if) {
		return t.lower_if_expr_value(transformed_if)
	}
	return transformed_if
}

// get_sumtype_name_for_expr returns the sum type name for an expression, or empty string if not a sum type
// This function is smartcast-aware: if the expression is already smartcasted to a variant that is
// itself a sum type, it returns that sum type name.
fn (mut t Transformer) transform_infix_expr(expr ast.InfixExpr) ast.Expr {
	// Preserve short-circuit semantics while making smartcasts available to all
	// following terms in `&&` chains (including multiple `is` checks).
	if expr.op == .and {
		terms := t.flatten_and_terms(expr)
		if terms.len > 1 {
			mut transformed_terms := []ast.Expr{cap: terms.len}
			mut pushed := 0
			mut changed := false
			for term in terms {
				if term is ast.InfixExpr {
					if ctx := t.smartcast_context_from_is_check(term) {
						transformed_terms << t.transform_expr(term)
						t.push_smartcast_full(ctx.expr, ctx.variant, ctx.variant_full,
							ctx.sumtype)
						pushed++
						changed = true
						continue
					}
				}
				transformed_terms << t.transform_expr(term)
			}
			for _ in 0 .. pushed {
				t.pop_smartcast()
			}
			if changed {
				return t.join_and_terms(transformed_terms)
			}
		}
	}

	// Lower sum type checks/comparisons to tag comparisons.
	// This also handles parser/checker-lowered `!is` that appears as `!=` with type RHS.
	if expr.op in [.key_is, .not_is, .eq, .ne] {
		mut variant_name := ''
		mut variant_module := ''
		if expr.rhs is ast.Ident {
			variant_name = (expr.rhs as ast.Ident).name
		} else if expr.rhs is ast.SelectorExpr {
			sel := expr.rhs as ast.SelectorExpr
			variant_name = sel.rhs.name
			if sel.lhs is ast.Ident {
				variant_module = (sel.lhs as ast.Ident).name
			}
		}
		if variant_name != '' {
			mut sumtype_name := t.get_sumtype_name_for_expr(expr.lhs)
			if sumtype_name == '' {
				sumtype_name = t.find_sumtype_for_variant(variant_name)
			}
			if sumtype_name != '' {
				variants := t.get_sum_type_variants(sumtype_name)
				mut tag_value := -1
				for i, v in variants {
					v_short := if v.contains('__') { v.all_after_last('__') } else { v }
					mangled_variant := if variant_module != '' {
						'${variant_module}__${variant_name}'
					} else {
						variant_name
					}
					if v == variant_name || v_short == variant_name || v == mangled_variant
						|| v_short == mangled_variant {
						tag_value = i
						break
					}
				}
				if tag_value >= 0 {
					transformed_lhs := t.transform_expr(expr.lhs)
					cmp_op := if expr.op in [.key_is, .eq] {
						token.Token.eq
					} else {
						token.Token.ne
					}
					return ast.InfixExpr{
						op:  cmp_op
						lhs: t.synth_selector(transformed_lhs, '_tag', types.Type(types.int_))
						rhs: ast.BasicLiteral{
							kind:  token.Token.number
							value: '${tag_value}'
						}
						pos: expr.pos
					}
				}
			}
		}
	}

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
	// Check for 'in' operator with arrays: elem in arr => Array_T_contains(arr, elem)
	if expr.op in [.key_in, .not_in] {
		// Map membership: key in map -> map__exists(&map, &key)
		if rhs_type := t.get_expr_type(expr.rhs) {
			if map_typ := t.unwrap_map_type(rhs_type) {
				mut map_ptr := ast.Expr(ast.empty_expr)
				rhs_trans := t.transform_expr(expr.rhs)
				if rhs_type is types.Pointer {
					map_ptr = rhs_trans
				} else if t.can_take_address_expr(rhs_trans) {
					map_ptr = ast.Expr(ast.PrefixExpr{
						op:   .amp
						expr: rhs_trans
					})
				} else {
					map_ptr = t.addr_of_expr_with_temp(expr.rhs, map_typ)
				}
				key_ptr := t.addr_of_expr_with_temp(expr.lhs, map_typ.key_type)
				exists_call := ast.CallExpr{
					lhs:  ast.Ident{
						name: 'map__exists'
					}
					args: [map_ptr, key_ptr]
					pos:  expr.pos
				}
				if expr.op == .not_in {
					return ast.PrefixExpr{
						op:   .not
						expr: exists_call
						pos:  expr.pos
					}
				}
				return exists_call
			}
		}
		// For inline array literals, expand to a chain of equality checks
		// instead of a contains() call — simpler and works for all backends.
		if expr.rhs is ast.ArrayInitExpr {
			arr := expr.rhs as ast.ArrayInitExpr
			if arr.exprs.len > 0 {
				lhs_trans := t.transform_expr(expr.lhs)
				// Get enum type from LHS for resolving shorthand in array elements
				enum_type := t.get_enum_type_name(expr.lhs)
				// Build: lhs == arr[0] || lhs == arr[1] || ...
				mut chain := ast.Expr(ast.InfixExpr{
					op:  .eq
					lhs: lhs_trans
					rhs: if enum_type != '' {
						t.resolve_enum_shorthand(t.transform_expr(arr.exprs[0]), enum_type)
					} else {
						t.transform_expr(arr.exprs[0])
					}
					pos: expr.pos
				})
				for i := 1; i < arr.exprs.len; i++ {
					elem := if enum_type != '' {
						t.resolve_enum_shorthand(t.transform_expr(arr.exprs[i]), enum_type)
					} else {
						t.transform_expr(arr.exprs[i])
					}
					chain = ast.Expr(ast.InfixExpr{
						op:  .logical_or
						lhs: chain
						rhs: ast.Expr(ast.InfixExpr{
							op:  .eq
							lhs: lhs_trans
							rhs: elem
							pos: expr.pos
						})
						pos: expr.pos
					})
				}
				if expr.op == .not_in {
					return ast.PrefixExpr{
						op:   .not
						expr: chain
						pos:  expr.pos
					}
				}
				return chain
			}
		}
		// Array membership: elem in arr -> Array_T_contains(arr, elem)
		if arr_info := t.get_array_method_info(expr.rhs) {
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
				mut method_info := arr_info
				if enum_type != '' {
					enum_c_name := t.v_type_name_to_c_name(enum_type)
					if enum_c_name != '' {
						method_info = ArrayMethodInfo{
							array_type: 'Array_${enum_c_name}'
							elem_type:  enum_c_name
							is_fixed:   false
						}
					}
				}
				contains_fn_name := t.register_needed_array_method(method_info, 'contains')
				// Transform array with enum context if needed
				transformed_rhs := if enum_type != '' && expr.rhs is ast.ArrayInitExpr {
					t.transform_array_with_enum_context(expr.rhs as ast.ArrayInitExpr,
						enum_type)
				} else {
					t.transform_expr(expr.rhs)
				}
				contains_call := ast.CallExpr{
					lhs:  ast.Ident{
						name: contains_fn_name
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
	// Note: map[key] << value is handled at the statement level
	// by try_transform_map_index_push in transform_stmts.
	if expr.op == .left_shift {
		if elem_type_name := t.get_array_elem_type_str(expr.lhs) {
			// Use push_many only when RHS element type matches the LHS element type.
			// This avoids mis-lowering [][]T << []T, which must stay a single push.
			mut rhs_is_array := false
			if rhs_elem_type := t.array_value_elem_type(expr.rhs) {
				rhs_is_array = t.array_elem_types_compatible(elem_type_name, rhs_elem_type)
			} else {
				lhs_is_nested_array := elem_type_name.starts_with('Array_')
					|| elem_type_name.starts_with('Array_fixed_')
				rhs_is_array = t.is_array_value_expr(expr.rhs) && !lhs_is_nested_array
			}

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
				// When RHS contains a call expression, introduce a temporary variable
				// to avoid evaluating the call twice (once for .data, once for .len).
				// The VarDecl is hoisted via pending_stmts before the current statement.
				if t.contains_call_expr(expr.rhs) {
					t.temp_counter++
					tmp_name := '_pm_t${t.temp_counter}'
					tmp_ident := ast.Ident{
						name: tmp_name
					}
					if rhs_type := t.get_expr_type(expr.rhs) {
						t.register_temp_var(tmp_name, rhs_type)
					}
					t.pending_stmts << ast.Stmt(ast.AssignStmt{
						op:  .decl_assign
						lhs: [ast.Expr(tmp_ident)]
						rhs: [rhs_transformed]
					})
					return ast.CallExpr{
						lhs:  ast.Ident{
							name: 'array__push_many'
						}
						args: [
							arr_ptr_expr,
							t.synth_selector(ast.Expr(tmp_ident), 'data', types.Type(types.voidptr_)),
							t.synth_selector(ast.Expr(tmp_ident), 'len', types.Type(types.int_)),
						]
						pos:  expr.pos
					}
				}
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
						t.synth_selector(rhs_for_selector, 'data', types.Type(types.voidptr_)),
						t.synth_selector(rhs_for_selector, 'len', types.Type(types.int_)),
					]
					pos:  expr.pos
				}
			}

			// Create (T[]){ elem } expression for single element push
			// Note: cleanc will add _MOV wrapper when generating ArrayInitExpr
			transformed_rhs := t.transform_expr(expr.rhs)
			// Wrap in sumtype if the array element type is a sumtype
			push_elem := if t.is_sum_type(elem_type_name) {
				t.wrap_sumtype_value_transformed(transformed_rhs, elem_type_name) or {
					transformed_rhs
				}
			} else {
				transformed_rhs
			}
			arr_literal := ast.ArrayInitExpr{
				typ:   ast.Expr(ast.Type(ast.ArrayType{
					elem_type: ast.Ident{
						name: elem_type_name
					}
				}))
				exprs: [push_elem]
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
				lhs_is_str = t.type_is_string(expr_type)
			}
		}
		if !rhs_is_str {
			if expr_type := t.get_expr_type(expr.rhs) {
				rhs_is_str = t.type_is_string(expr_type)
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
		// Also transform if one side is a SelectorExpr and the other is a string literal
		// (field access compared with string literal is almost always string comparison)
		both_are_ident := expr.lhs is ast.Ident && expr.rhs is ast.Ident
		should_transform := (lhs_is_str && rhs_is_str) || (lhs_is_str_literal && (rhs_is_str
			|| expr.rhs is ast.Ident || expr.rhs is ast.SelectorExpr))
			|| (rhs_is_str_literal && (lhs_is_str || expr.lhs is ast.Ident
			|| expr.lhs is ast.SelectorExpr))
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
		lhs_arr_type := t.get_array_type_str(expr.lhs)
		rhs_arr_type := t.get_array_type_str(expr.rhs)
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
		// Check for map comparisons: map1 == map2 or map1 != map2
		// Exclude pointer-to-map types (those should be pointer comparisons)
		if expr.op in [.eq, .ne] {
			mut is_lhs_ptr_map := false
			if lhs_type := t.get_expr_type(expr.lhs) {
				if lhs_type is types.Pointer {
					is_lhs_ptr_map = true
				}
			}
			mut is_rhs_ptr_map := false
			if rhs_type := t.get_expr_type(expr.rhs) {
				if rhs_type is types.Pointer {
					is_rhs_ptr_map = true
				}
			}
			if !is_lhs_ptr_map && !is_rhs_ptr_map {
				lhs_map_type := t.get_map_type_for_expr(expr.lhs)
				rhs_map_type := t.get_map_type_for_expr(expr.rhs)
				if lhs_map_type != none || rhs_map_type != none {
					map_type_name := lhs_map_type or { rhs_map_type or { 'map' } }
					eq_fn := '${map_type_name}_map_eq'
					map_eq_call := ast.CallExpr{
						lhs:  ast.Ident{
							name: eq_fn
						}
						args: [t.transform_expr(expr.lhs), t.transform_expr(expr.rhs)]
						pos:  expr.pos
					}
					if expr.op == .ne {
						return ast.PrefixExpr{
							op:   .not
							expr: map_eq_call
							pos:  expr.pos
						}
					}
					return map_eq_call
				}
			}
		}
	}
	// Check for struct operator overloading (e.g., time.Time - time.Time)
	// This transforms t1 - t2 into time__Time__minus(t1, t2) for structs with operator overloading
	// Only applies to specific known struct types that define operator methods
	if expr.op in [.plus, .minus, .mul, .div, .mod] {
		if lhs_type := t.get_expr_type(expr.lhs) {
			match lhs_type {
				types.Struct {
					type_name := t.type_to_c_name(lhs_type)
					// Only transform known struct types with operator overloading
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
								args: [t.transform_expr(expr.lhs),
									t.transform_expr(expr.rhs)]
								pos:  expr.pos
							}
						}
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

// resolve_field_type looks up the type of a field on a variable
// e.g., for a.flags where a is Array, returns 'ArrayFlags'
fn (mut t Transformer) transform_comptime_expr(expr ast.ComptimeExpr) ast.Expr {
	// The inner expression should be an IfExpr for $if
	inner := expr.expr
	if inner is ast.IfExpr {
		return t.eval_comptime_if(inner)
	}
	if inner is ast.Ident {
		if inner.name in ['VMODROOT', '@VMODROOT'] {
			return ast.Expr(t.vmodroot_string_literal(expr.pos))
		}
	}
	// For other comptime expressions, just return them transformed
	return ast.ComptimeExpr{
		expr: t.transform_expr(inner)
		pos:  expr.pos
	}
}

// eval_comptime_if evaluates a compile-time $if and returns the selected branch expression
