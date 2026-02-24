// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.types
import v2.token

fn (mut t Transformer) synth_selector(lhs ast.Expr, field_name string, typ types.Type) ast.Expr {
	pos := t.next_synth_pos()
	t.register_synth_type(pos, typ)
	return ast.Expr(ast.SelectorExpr{
		lhs: lhs
		rhs: ast.Ident{
			name: field_name
		}
		pos: pos
	})
}

// synth_selector_from_struct creates a typed SelectorExpr by looking up the field type
// from the named struct in the environment. Falls back to a pos-only synth node if
// the field type cannot be resolved.
fn (mut t Transformer) synth_selector_from_struct(lhs ast.Expr, field_name string, struct_name string) ast.Expr {
	pos := t.next_synth_pos()
	if field_typ := t.lookup_struct_field_type(struct_name, field_name) {
		t.register_synth_type(pos, field_typ)
	}
	return ast.Expr(ast.SelectorExpr{
		lhs: lhs
		rhs: ast.Ident{
			name: field_name
		}
		pos: pos
	})
}

// lookup_struct_field_type returns the raw types.Type for a struct field.
fn (t &Transformer) lookup_struct_field_type(struct_name string, field_name string) ?types.Type {
	mut sname := struct_name
	mut mod := ''
	if struct_name.contains('__') {
		mod = struct_name.all_before_last('__')
		sname = struct_name.all_after_last('__')
	}
	lock t.env.scopes {
		// Try module scope first if qualified
		if mod != '' {
			if scope := t.env.scopes[mod] {
				if obj := scope.objects[sname] {
					if obj is types.Type {
						typ := types.Type(obj)
						if typ is types.Struct {
							for field in typ.fields {
								if field.name == field_name {
									return field.typ
								}
							}
						}
					}
				}
			}
		}
		// Fallback: scan all scopes
		scope_names := t.env.scopes.keys()
		for scope_name in scope_names {
			scope := t.env.scopes[scope_name] or { continue }
			for name in [struct_name, sname] {
				if obj := scope.objects[name] {
					if obj is types.Type {
						typ := types.Type(obj)
						if typ is types.Struct {
							for field in typ.fields {
								if field.name == field_name {
									return field.typ
								}
							}
						}
					}
				}
			}
		}
	}
	return none
}

// open_scope creates a new nested scope
fn (mut t Transformer) apply_smartcast_field_access_ctx(sumtype_expr ast.Expr, field_name string, ctx SmartcastContext) ast.Expr {
	// variant (short name) is used for union member access
	// variant_full (full name) is used for type cast
	variant_short := ctx.variant
	// Extract simple variant name for _data._ accessor (strip module prefix)
	// But preserve composite type prefixes like Array_, Map_, Array_fixed_
	variant_simple := if variant_short.starts_with('Array_') || variant_short.starts_with('Map_') {
		// For composite types, use the short name to match union member
		variant_short
	} else if variant_short.contains('__') {
		variant_short.all_after_last('__')
	} else {
		variant_short
	}
	// Use full variant name for type cast from context
	mangled_variant := if ctx.variant_full != '' {
		ctx.variant_full
	} else if variant_short.contains('__') {
		variant_short // Already has module prefix
	} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		'${t.cur_module}__${variant_short}'
	} else {
		variant_short
	}
	// For nested smartcasts, we need to transform the base of sumtype_expr to apply outer smartcasts
	// E.g., for stmt.receiver.typ with outer smartcast on stmt, we need to transform stmt.receiver first.
	// Temporarily remove this exact context to avoid applying it recursively.
	removed_ctxs := t.remove_matching_smartcasts(ctx)
	transformed_base := t.transform_expr(sumtype_expr)
	t.restore_smartcasts(removed_ctxs)
	if t.expr_is_casted_to_type(transformed_base, '${mangled_variant}*') {
		return t.synth_selector_from_struct(transformed_base, field_name, mangled_variant)
	}
	// Already concretely casted to this variant by an outer smartcast context.
	if t.expr_is_casted_to_type(transformed_base, mangled_variant) {
		return t.synth_selector_from_struct(transformed_base, field_name, mangled_variant)
	}
	// Create data access.
	// For native backends (arm64/x64): _data is a plain i64 (void pointer) in the SSA struct.
	// No union variant sub-field exists, so just use _data directly.
	// For C backends: _data is a union, so access _data._variant for the specific member.
	is_native_backend := t.pref != unsafe { nil }
		&& (t.pref.backend == .arm64 || t.pref.backend == .x64)
	data_access := t.synth_selector(transformed_base, '_data', types.Type(types.voidptr_))
	variant_access := if is_native_backend {
		data_access
	} else {
		t.synth_selector(data_access, '_${variant_simple}', types.Type(types.voidptr_))
	}
	// Create: (mangled_variant*)variant_access
	cast_expr := ast.CastExpr{
		typ:  ast.Ident{
			name: '${mangled_variant}*'
		}
		expr: variant_access
	}
	// Create: cast_expr->field_name (cleanc will handle pointer arrow vs dot)
	return t.synth_selector_from_struct(ast.Expr(cast_expr), field_name, mangled_variant)
}

fn (mut t Transformer) transform_array_init_expr(expr ast.ArrayInitExpr) ast.Expr {
	// Transform value expressions
	mut exprs := []ast.Expr{cap: expr.exprs.len}
	for e in expr.exprs {
		exprs << t.transform_expr(e)
	}

	// Check if this is a fixed-size array
	mut is_fixed := false
	mut array_typ := expr.typ
	mut elem_type_expr := ast.empty_expr
	// Check for ArrayFixedType or ArrayType (expr.typ is ast.Type sum type)
	if expr.typ is ast.Type {
		if expr.typ is ast.ArrayFixedType {
			is_fixed = true
		} else if expr.typ is ast.ArrayType {
			elem_type_expr = expr.typ.elem_type
		}
	}
	// For untyped `[]` literals, use checker-inferred type from context (assign/call/return).
	if array_typ is ast.EmptyExpr {
		if inferred := t.get_expr_type(ast.Expr(expr)) {
			inferred_base := t.unwrap_alias_and_pointer_type(inferred)
			match inferred_base {
				types.Array {
					array_typ = t.type_to_ast_type_expr(inferred_base)
					elem_type_expr = t.type_to_ast_type_expr(inferred_base.elem_type)
				}
				types.ArrayFixed {
					array_typ = t.type_to_ast_type_expr(inferred_base)
					elem_type_expr = t.type_to_ast_type_expr(inferred_base.elem_type)
					is_fixed = true
				}
				else {}
			}
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
			typ:   array_typ
			exprs: exprs
			init:  t.transform_expr(expr.init)
			cap:   if expr.cap !is ast.EmptyExpr { t.transform_expr(expr.cap) } else { expr.cap }
			len:   if expr.len !is ast.EmptyExpr { t.transform_expr(expr.len) } else { expr.len }
			pos:   expr.pos
		}
	}

	// Dynamic array: transform to builtin__new_array_from_c_array_noscan(len, cap, sizeof(elem), values)
	arr_len := exprs.len

	// Handle empty dynamic arrays: lower to __new_array_with_default_noscan(len, cap, sizeof(elem), init)
	if arr_len == 0 {
		sizeof_expr := if elem_type_expr !is ast.EmptyExpr {
			elem_type_expr
		} else {
			ast.Expr(ast.Ident{
				name: 'int'
			})
		}
		len_expr := ast.Expr(if expr.len !is ast.EmptyExpr {
			t.transform_expr(expr.len)
		} else {
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		})
		cap_expr := ast.Expr(if expr.cap !is ast.EmptyExpr {
			t.transform_expr(expr.cap)
		} else {
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		})
		init_expr := ast.Expr(if expr.init !is ast.EmptyExpr {
			t.transform_expr(expr.init)
		} else {
			ast.Expr(ast.Ident{
				name: 'nil'
			})
		})
		// If init expression uses `index`, expand to a for-loop that assigns each element.
		if expr.init !is ast.EmptyExpr && t.expr_contains_ident_named(init_expr, 'index') {
			return t.expand_array_init_with_index(len_expr, cap_expr, sizeof_expr, init_expr,
				expr.pos)
		}
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: '__new_array_with_default_noscan'
			}
			args: [
				len_expr,
				cap_expr,
				ast.Expr(ast.KeywordOperator{
					op:    .key_sizeof
					exprs: [sizeof_expr]
				}),
				init_expr,
			]
			pos:  expr.pos
		}
	}

	// Determine element type name and sizeof argument
	// First, try to get the array type from the type checker's annotations
	mut elem_type_name := 'int'
	mut elem_type_expr_resolved := elem_type_expr
	if elem_type_expr_resolved is ast.EmptyExpr && exprs.len > 0 {
		if arr_type := t.env.get_expr_type(expr.pos.id) {
			match arr_type {
				types.Array {
					tn := t.type_to_c_name(arr_type.elem_type)
					if tn != '' {
						elem_type_name = tn
						elem_type_expr_resolved = ast.Expr(ast.Ident{
							name: tn
						})
					}
				}
				types.ArrayFixed {
					tn := t.type_to_c_name(arr_type.elem_type)
					if tn != '' {
						elem_type_name = tn
						elem_type_expr_resolved = ast.Expr(ast.Ident{
							name: tn
						})
					}
				}
				else {}
			}
		}
		// If env lookup failed, try getting element type from the ORIGINAL (untransformed)
		// first expression, which preserves CastExpr and other type-annotated nodes
		if elem_type_expr_resolved is ast.EmptyExpr {
			orig_first := expr.exprs[0]
			if elem_type := t.get_expr_type(orig_first) {
				tn := t.type_to_c_name(elem_type)
				if tn != '' {
					elem_type_name = tn
					elem_type_expr_resolved = ast.Expr(ast.Ident{
						name: tn
					})
				}
			} else {
			}
			// If still not resolved, check if first expr is a CallExpr and look up its return type
			if elem_type_expr_resolved is ast.EmptyExpr {
				first := exprs[0]
				if first is ast.CallExpr || first is ast.CallOrCastExpr {
					if ret_type := t.get_method_return_type(first) {
						tn := t.type_to_c_name(ret_type)
						if tn != '' {
							elem_type_name = tn
							elem_type_expr_resolved = ast.Expr(ast.Ident{
								name: tn
							})
						}
					} else if first is ast.CallExpr {
						// Try looking up by function name for plain function calls
						fn_name := if first.lhs is ast.Ident {
							first.lhs.name
						} else {
							''
						}
						if fn_name != '' {
							if ret_type2 := t.get_fn_return_type(fn_name) {
								tn := t.type_to_c_name(ret_type2)
								if tn != '' {
									elem_type_name = tn
									elem_type_expr_resolved = ast.Expr(ast.Ident{
										name: tn
									})
								}
							}
						}
					}
				}
			}
		}
	}
	sizeof_arg := if elem_type_expr_resolved !is ast.EmptyExpr {
		elem_type_name = t.expr_to_type_name(elem_type_expr_resolved)
		elem_type_expr_resolved
	} else if exprs.len > 0 {
		// Infer from first element
		first := exprs[0]
		if first is ast.BasicLiteral {
			if first.kind == .number {
				if first.value.contains('.') || first.value.contains('e')
					|| first.value.contains('E') {
					elem_type_name = 'f64'
				} else {
					elem_type_name = 'int'
				}
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
			// Extract first.lhs to avoid double smartcast in if-guard expansions
			first_lhs := first.lhs
			mut idx_sizeof := ast.Expr(ast.Ident{
				name: 'int'
			})
			if first.expr is ast.RangeExpr {
				// Slicing: s[a..b] returns the same type as s
				if expr_type := t.get_expr_type(first_lhs) {
					type_name := t.type_to_c_name(expr_type)
					if type_name != '' {
						elem_type_name = type_name
						idx_sizeof = ast.Expr(ast.Ident{
							name: type_name
						})
					}
				}
			} else if expr_type := t.get_expr_type(first_lhs) {
				type_name := t.type_to_c_name(expr_type)
				if type_name == 'string' {
					// String indexing returns u8
					elem_type_name = 'u8'
					idx_sizeof = ast.Expr(ast.Ident{
						name: 'u8'
					})
				} else if type_name.starts_with('Array_') {
					// Array indexing returns element type
					arr_elem := type_name[6..] // Remove 'Array_' prefix
					elem_type_name = arr_elem
					idx_sizeof = ast.Expr(ast.Ident{
						name: arr_elem
					})
				}
			}
			idx_sizeof
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
				// Dynamic array construction functions return 'array' type
				if fn_name in ['builtin__new_array_from_c_array_noscan',
					'builtin__new_array_from_c_array', '__new_array_with_default_noscan',
					'new_array_from_c_array'] {
					elem_type_name = 'array'
					ast.Expr(ast.Ident{
						name: 'array'
					})
				} else if fn_name in ['substr', 'substr_unsafe', 'trim', 'trim_left', 'trim_right',
					'to_upper', 'to_lower', 'replace', 'reverse', 'clone', 'repeat'] {
					// String methods that return string
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
		} else if first is ast.InitExpr {
			// Struct literal - get the type name from the struct type
			init_type_name := t.expr_to_type_name(first.typ)
			if init_type_name != '' {
				elem_type_name = init_type_name
				ast.Expr(ast.Ident{
					name: init_type_name
				})
			} else {
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
				typ:   ast.Expr(inner_array_typ)
				exprs: exprs
			}),
		]
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_map_init_expr(expr ast.MapInitExpr) ast.Expr {
	// Determine key/value types from the explicit map type when available.
	mut key_type_expr := ast.Expr(ast.Ident{
		name: 'int'
	})
	mut val_type_expr := ast.Expr(ast.Ident{
		name: 'int'
	})
	mut key_type_name := 'int'
	mut have_explicit_map_type := false
	match expr.typ {
		ast.Type {
			if expr.typ is ast.MapType {
				mt := expr.typ as ast.MapType
				key_type_expr = mt.key_type
				val_type_expr = mt.value_type
				key_type_name = t.expr_to_type_name(mt.key_type)
				have_explicit_map_type = true
			}
		}
		else {}
	}
	// Empty map literals `{}` rely on checker-provided expected type.
	// Use the inferred map type from the environment when the AST node doesn't carry one.
	if !have_explicit_map_type {
		if inferred := t.get_expr_type(ast.Expr(expr)) {
			if inferred_map := t.unwrap_map_type(inferred) {
				key_type_expr = t.type_to_ast_type_expr(inferred_map.key_type)
				val_type_expr = t.type_to_ast_type_expr(inferred_map.value_type)
				key_type_name = t.type_to_c_name(inferred_map.key_type)
			}
		}
	}

	// Transform key and value expressions (if any).
	mut keys := []ast.Expr{cap: expr.keys.len}
	mut vals := []ast.Expr{cap: expr.vals.len}
	for k in expr.keys {
		keys << t.transform_expr(k)
	}
	for v in expr.vals {
		vals << t.transform_expr(v)
	}

	// Infer map type from first entry when the checker didn't provide one.
	if key_type_name == 'int' && keys.len > 0 {
		first_key := keys[0]
		first_val := vals[0]
		if first_key is ast.BasicLiteral && first_key.kind == .string {
			key_type_name = 'string'
			key_type_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
		} else if first_key is ast.StringLiteral {
			key_type_name = 'string'
			key_type_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
		}
		if first_val is ast.BasicLiteral && first_val.kind == .string {
			val_type_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
		} else if first_val is ast.StringLiteral {
			val_type_expr = ast.Expr(ast.Ident{
				name: 'string'
			})
		}
	}

	hash_fn, eq_fn, clone_fn, free_fn := map_runtime_key_fns_from_type_name(key_type_name)

	// Empty map literal `{}`: lower to `new_map(sizeof(K), sizeof(V), &hash, &eq, &clone, &free)`.
	if keys.len == 0 {
		return ast.CallExpr{
			lhs:  ast.Ident{
				name: 'new_map'
			}
			args: [
				ast.Expr(ast.KeywordOperator{
					op:    .key_sizeof
					exprs: [key_type_expr]
				}),
				ast.Expr(ast.KeywordOperator{
					op:    .key_sizeof
					exprs: [val_type_expr]
				}),
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: hash_fn
					}
				}),
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: eq_fn
					}
				}),
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: clone_fn
					}
				}),
				ast.Expr(ast.PrefixExpr{
					op:   .amp
					expr: ast.Ident{
						name: free_fn
					}
				}),
			]
			pos:  expr.pos
		}
	}

	n := keys.len

	// Create array types for keys and values.
	key_array_typ := ast.Type(ast.ArrayType{
		elem_type: key_type_expr
	})
	val_array_typ := ast.Type(ast.ArrayType{
		elem_type: val_type_expr
	})

	// new_map_init_noscan_value(hash_fn, eq_fn, clone_fn, free_fn, n, key_size, val_size, keys, vals)
	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'new_map_init_noscan_value'
		}
		args: [
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: hash_fn
				}
			}),
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: eq_fn
				}
			}),
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: clone_fn
				}
			}),
			ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: ast.Ident{
					name: free_fn
				}
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${n}'
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [key_type_expr]
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [val_type_expr]
			}),
			ast.Expr(ast.ArrayInitExpr{
				typ:   ast.Expr(key_array_typ)
				exprs: keys
			}),
			ast.Expr(ast.ArrayInitExpr{
				typ:   ast.Expr(val_array_typ)
				exprs: vals
			}),
		]
		pos:  expr.pos
	}
}

fn (mut t Transformer) transform_init_expr(expr ast.InitExpr) ast.Expr {
	// Typed empty map init: `map[K]V{}`.
	// Lower here so backends do not need to special-case map InitExpr nodes.
	if expr.fields.len == 0 {
		match expr.typ {
			ast.Type {
				if expr.typ is ast.MapType {
					mt := expr.typ as ast.MapType
					key_type_name := t.expr_to_type_name(mt.key_type)
					hash_fn, eq_fn, clone_fn, free_fn := map_runtime_key_fns_from_type_name(key_type_name)
					return ast.Expr(ast.CallExpr{
						lhs:  ast.Ident{
							name: 'new_map'
						}
						args: [
							ast.Expr(ast.KeywordOperator{
								op:    .key_sizeof
								exprs: [mt.key_type]
							}),
							ast.Expr(ast.KeywordOperator{
								op:    .key_sizeof
								exprs: [mt.value_type]
							}),
							ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: hash_fn
								}
							}),
							ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: eq_fn
								}
							}),
							ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: clone_fn
								}
							}),
							ast.Expr(ast.PrefixExpr{
								op:   .amp
								expr: ast.Ident{
									name: free_fn
								}
							}),
						]
						pos:  expr.pos
					})
				}
			}
			else {}
		}
	}

	// Get the struct type name for field type lookups
	struct_type_name := t.get_init_expr_type_name(expr.typ)

	// Transform field values recursively
	// Note: ArrayInitExpr is NOT transformed here because cleanc uses field type info
	// to determine if it's a fixed-size array (which transformer doesn't have access to)
	mut fields := []ast.FieldInit{cap: expr.fields.len}
	for field in expr.fields {
		// Check if this field is a sum type and needs wrapping
		mut field_type_name := t.get_struct_field_type_name(struct_type_name, field.name)
		if field_type_name == '' {
			// Fallback to direct type lookup from the init expression type.
			field_type_name = t.get_init_expr_field_type_name(expr.typ, field.name)
		}
		if t.is_sum_type(field_type_name) {
			// If the value is a variable whose declared type is already this sum type
			// (e.g., `expr: ast.Expr` used in `GenericArgOrIndexExpr{expr: expr}`),
			// skip wrapping. At the C level, the variable is already a tagged union
			// of the correct type, so wrapping would produce invalid C.
			if field.value is ast.Ident {
				if var_type := t.lookup_var_type(field.value.name) {
					if var_type is types.SumType {
						var_st_name := types.sum_type_name(var_type)
						if var_st_name == field_type_name
							|| match_sumtype_variant_name(var_st_name, [field_type_name]) != '' {
							// Variable is already the target sum type. Remove any
							// smartcast context temporarily so transform_expr returns
							// the raw variable (tagged union value) without deref.
							if sc_ctx := t.find_smartcast_for_expr(field.value.name) {
								removed := t.remove_matching_smartcasts(sc_ctx)
								transformed_direct := t.transform_expr(field.value)
								t.restore_smartcasts(removed)
								fields << ast.FieldInit{
									name:  field.name
									value: transformed_direct
								}
							} else {
								fields << ast.FieldInit{
									name:  field.name
									value: t.transform_expr(field.value)
								}
							}
							continue
						}
					}
				}
			}
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
			// If the array has len/cap but no literal elements (e.g., []int{len: 4}),
			// use the normal transform_expr path which handles __new_array_with_default_noscan
			if field.value.exprs.len == 0
				&& (field.value.len !is ast.EmptyExpr || field.value.cap !is ast.EmptyExpr) {
				t.transform_expr(field.value)
			} else {
				// Transform array elements with sumtype wrapping if needed.
				elem_sumtype := t.get_field_array_elem_sumtype_name(struct_type_name,
					field.name)
				mut new_exprs := []ast.Expr{cap: field.value.exprs.len}
				for e in field.value.exprs {
					transformed := t.transform_expr(e)
					if elem_sumtype != '' {
						if wrapped := t.wrap_sumtype_value_transformed(transformed, elem_sumtype) {
							new_exprs << wrapped
							continue
						}
					}
					new_exprs << transformed
				}
				// Set elem type from struct field so
				// transform_array_init_with_exprs uses the correct element type.
				// This is critical when elements were wrapped in a sum type above,
				// as the C array type must match the wrapped (sum type) elements.
				mut arr_with_type := field.value
				elem_c_name := t.get_field_array_elem_c_name(struct_type_name, field.name)
				if elem_c_name != '' {
					arr_with_type = ast.ArrayInitExpr{
						typ:   ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Ident{
								name: elem_c_name
							}
						}))
						exprs: arr_with_type.exprs
						init:  arr_with_type.init
						cap:   arr_with_type.cap
						len:   arr_with_type.len
						pos:   arr_with_type.pos
					}
				}
				// Use transform_array_init_with_exprs which handles both fixed and dynamic:
				// - Fixed arrays stay as ArrayInitExpr for cleanc
				// - Dynamic arrays are lowered to builtin__new_array_from_c_array_noscan
				t.transform_array_init_with_exprs(arr_with_type, new_exprs)
			}
		} else {
			t.transform_expr(field.value)
		}
		final_value := t.deref_init_field_value_if_needed(transformed_value, field_type_name)
		fields << ast.FieldInit{
			name:  field.name
			value: final_value
		}
	}
	fields = t.add_missing_struct_field_defaults(struct_type_name, fields)

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

fn (t &Transformer) deref_init_field_value_if_needed(value ast.Expr, expected_field_type_name string) ast.Expr {
	if expected_field_type_name == '' {
		return value
	}
	// Pointer-typed fields already expect an address.
	if expected_field_type_name.starts_with('&') || expected_field_type_name.ends_with('*') {
		return value
	}
	mut expected_base_c := ''
	if expected_typ := t.lookup_type(expected_field_type_name) {
		if expected_typ is types.Pointer {
			return value
		}
		expected_base := t.unwrap_alias_and_pointer_type(expected_typ)
		expected_base_c = t.type_to_c_name(expected_base)
	}
	value_typ := t.get_expr_type(value) or { return value }
	if value_typ is types.Pointer {
		value_base := t.unwrap_alias_and_pointer_type(value_typ.base_type)
		value_base_c := t.type_to_c_name(value_base)
		value_short := if value_base_c.contains('__') {
			value_base_c.all_after_last('__')
		} else {
			value_base_c
		}
		if expected_base_c != '' {
			expected_short := if expected_base_c.contains('__') {
				expected_base_c.all_after_last('__')
			} else {
				expected_base_c
			}
			if expected_base_c == value_base_c || expected_short == value_short {
				return ast.PrefixExpr{
					op:   .mul
					expr: value
				}
			}
			return value
		}
		mut expected_c := t.v_type_name_to_c_name(expected_field_type_name)
		if expected_c.ends_with('*') {
			expected_c = expected_c[..expected_c.len - 1]
		}
		expected_short := if expected_c.contains('__') {
			expected_c.all_after_last('__')
		} else {
			expected_c
		}
		raw_expected_short := if expected_field_type_name.contains('__') {
			expected_field_type_name.all_after_last('__')
		} else {
			expected_field_type_name
		}
		if expected_c == value_base_c || expected_short == value_short
			|| raw_expected_short == value_short {
			return ast.PrefixExpr{
				op:   .mul
				expr: value
			}
		}
	}
	return value
}

fn (t &Transformer) get_init_expr_field_type_name(init_typ_expr ast.Expr, field_name string) string {
	init_typ := t.get_expr_type(init_typ_expr) or { return '' }
	base_typ := t.unwrap_alias_and_pointer_type(init_typ)
	if base_typ is types.Struct {
		for field in base_typ.fields {
			if field.name == field_name {
				return t.type_to_name(field.typ)
			}
		}
	}
	return ''
}

fn (mut t Transformer) add_missing_struct_field_defaults(struct_name string, fields []ast.FieldInit) []ast.FieldInit {
	if struct_name == '' {
		return fields
	}
	struct_type := t.lookup_type(struct_name) or { return fields }
	base_type := t.unwrap_alias_and_pointer_type(struct_type)
	if base_type !is types.Struct {
		return fields
	}
	struct_info := base_type as types.Struct
	mut existing := map[string]bool{}
	mut positional_idx := 0
	for field in fields {
		if field.name == '' {
			// Positional field — map it to the corresponding struct field name
			if positional_idx < struct_info.fields.len {
				existing[struct_info.fields[positional_idx].name] = true
			}
			positional_idx++
		} else {
			existing[field.name] = true
		}
	}
	mut out := []ast.FieldInit{cap: fields.len}
	for field in fields {
		out << field
	}
	for struct_field in struct_info.fields {
		if struct_field.name in existing {
			continue
		}
		if struct_field.default_expr !is ast.EmptyExpr
			&& t.is_supported_struct_default_expr(struct_field.default_expr) {
			resolved_default := t.resolve_expr_with_expected_type(struct_field.default_expr,
				struct_field.typ)
			out << ast.FieldInit{
				name:  struct_field.name
				value: t.transform_struct_field_default_expr(struct_name, resolved_default)
			}
			continue
		}
		field_type := t.unwrap_alias_and_pointer_type(struct_field.typ)
		if field_type is types.Map {
			map_init := ast.Expr(ast.MapInitExpr{
				typ: t.type_to_ast_type_expr(field_type)
			})
			out << ast.FieldInit{
				name:  struct_field.name
				value: t.transform_expr(map_init)
			}
			continue
		}
		if field_type is types.Array {
			array_init := ast.Expr(ast.ArrayInitExpr{
				typ: t.type_to_ast_type_expr(field_type)
			})
			out << ast.FieldInit{
				name:  struct_field.name
				value: t.transform_expr(array_init)
			}
			continue
		}
		if field_type is types.OptionType {
			option_none := ast.Expr(ast.InitExpr{
				typ:    t.type_to_ast_type_expr(field_type)
				fields: [
					ast.FieldInit{
						name:  'state'
						value: ast.BasicLiteral{
							kind:  token.Token.number
							value: '2'
						}
					},
				]
			})
			out << ast.FieldInit{
				name:  struct_field.name
				value: t.transform_expr(option_none)
			}
			continue
		}
		if field_type is types.String {
			out << ast.FieldInit{
				name:  struct_field.name
				value: ast.StringLiteral{
					kind:  .v
					value: "''"
				}
			}
		}
	}
	return out
}

fn (mut t Transformer) transform_struct_field_default_expr(struct_name string, expr ast.Expr) ast.Expr {
	if struct_name.contains('__') {
		module_name := struct_name.all_before_last('__')
		if module_name != '' && module_name != t.cur_module {
			// Cross-module const defaults like `ast.empty_expr` must be qualified.
			if expr is ast.Ident {
				return ast.SelectorExpr{
					lhs: ast.Ident{
						name: module_name
					}
					rhs: expr
				}
			}
			old_module := t.cur_module
			t.cur_module = module_name
			transformed := t.transform_expr(expr)
			t.cur_module = old_module
			return transformed
		}
	}
	return t.transform_expr(expr)
}

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
fn (t &Transformer) get_struct_field_type_name(struct_name string, field_name string) string {
	// Look up the struct type in scopes.
	// When the struct name is not module-qualified, prefer the current module scope
	// to avoid collisions (e.g., ast.FnType vs types.FnType both named "FnType").
	lock t.env.scopes {
		// First: try the current module scope for non-qualified names.
		if !struct_name.contains('__') && t.cur_module != '' && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			if cur_scope := t.env.scopes[t.cur_module] {
				if obj := cur_scope.objects[struct_name] {
					if obj is types.Type {
						result := t.get_field_type_name(obj, field_name)
						if result != '' {
							return result
						}
					}
				}
			}
		}
		scope_names := t.env.scopes.keys()
		for scope_name in scope_names {
			scope := t.env.scopes[scope_name] or { continue }
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
			// Try stripping module prefix for cross-module types (e.g., "ast__CallExpr" -> "CallExpr")
			if struct_name.contains('__') {
				short_name := struct_name.all_after_last('__')
				if obj := scope.objects[short_name] {
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
		scope_names := t.env.scopes.keys()
		for scope_name in scope_names {
			scope := t.env.scopes[scope_name] or { continue }
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

// get_field_array_elem_sumtype_name returns the sum type name of the array element type
// for a struct field, if the field is an array of sum types. Returns '' otherwise.
fn (t &Transformer) get_field_array_elem_sumtype_name(struct_name string, field_name string) string {
	lock t.env.scopes {
		scope_names := t.env.scopes.keys()
		for scope_name in scope_names {
			scope := t.env.scopes[scope_name] or { continue }
			// Try direct name and short name (for cross-module types like ast__CallExpr)
			names := if struct_name.contains('__') {
				[struct_name, struct_name.all_after_last('__')]
			} else {
				[struct_name]
			}
			for name in names {
				if obj := scope.objects[name] {
					if obj is types.Type {
						if obj is types.Struct {
							for field in obj.fields {
								if field.name == field_name {
									if field.typ is types.Array {
										field_arr := field.typ as types.Array
										elem_name := t.type_to_name(field_arr.elem_type)
										if t.is_sum_type(elem_name) {
											return elem_name
										}
									}
									return ''
								}
							}
						}
					}
				}
			}
		}
	}
	return ''
}

// get_field_array_elem_c_name returns the C type name for the element type of an array field
fn (t &Transformer) get_field_array_elem_c_name(struct_name string, field_name string) string {
	lock t.env.scopes {
		scope_names := t.env.scopes.keys()
		for scope_name in scope_names {
			scope := t.env.scopes[scope_name] or { continue }
			if obj := scope.objects[struct_name] {
				if obj is types.Type {
					if obj is types.Struct {
						for field in obj.fields {
							if field.name == field_name {
								if field.typ is types.Array {
									field_arr := field.typ as types.Array
									return t.type_to_c_name(field_arr.elem_type)
								}
								return ''
							}
						}
					}
				}
			}
		}
	}
	return ''
}

// type_to_name converts a Type to its name string
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

// expand_array_init_with_index expands `[]T{len: n, init: expr_using_index}` into:
//   mut _awi_tN = __new_array_with_default_noscan(len, cap, sizeof(T), nil)
//   for (_v_index = 0; _v_index < _awi_tN.len; _v_index++) {
//       ((T*)_awi_tN.data)[_v_index] = init_expr  (with `index` renamed to `_v_index`)
//   }
//   <returns _awi_tN ident>
fn (mut t Transformer) expand_array_init_with_index(len_expr ast.Expr, cap_expr ast.Expr, sizeof_expr ast.Expr, init_expr ast.Expr, pos token.Pos) ast.Expr {
	t.temp_counter++
	arr_name := '_awi_t${t.temp_counter}'
	arr_ident := ast.Ident{
		name: arr_name
	}
	idx_ident := ast.Ident{
		name: '_v_index'
	}

	// 1. mut _awi_tN = __new_array_with_default_noscan(len, cap, sizeof(T), nil)
	init_stmt := ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [
			ast.Expr(ast.ModifierExpr{
				kind: .key_mut
				expr: arr_ident
			}),
		]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: '__new_array_with_default_noscan'
				}
				args: [
					len_expr,
					cap_expr,
					ast.Expr(ast.KeywordOperator{
						op:    .key_sizeof
						exprs: [sizeof_expr]
					}),
					ast.Expr(ast.Ident{
						name: 'nil'
					}),
				]
			}),
		]
	})

	// 2. Build assignment: ((T*)_awi_tN.data)[_v_index] = init_expr
	//    with `index` renamed to `_v_index`
	renamed_init := t.replace_ident_named(init_expr, 'index', '_v_index')
	arr_data := t.synth_selector(arr_ident, 'data', types.Type(types.voidptr_))
	// Use a simple Ident for the cast type name so cleanc renders it as
	// ((ElemType*)data)[idx] without decomposing compound type expressions.
	cast_type_name := t.expr_to_type_name(sizeof_expr)
	elem_assign := ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.IndexExpr{
				lhs:  ast.CastExpr{
					typ:  ast.Ident{
						name: '${cast_type_name}*'
					}
					expr: arr_data
				}
				expr: idx_ident
			}),
		]
		rhs: [renamed_init]
	})

	// 3. for (int _v_index = 0; _v_index < _awi_tN.len; _v_index++) { ... }
	for_stmt := ast.Stmt(ast.ForStmt{
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
			rhs: t.synth_selector(arr_ident, 'len', types.Type(types.int_))
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
		stmts: [elem_assign]
	})

	// Emit init + for loop as pending statements
	t.pending_stmts << init_stmt
	t.pending_stmts << for_stmt

	// Return the temp array ident as the expression value
	return arr_ident
}

// replace_ident_named replaces all occurrences of an identifier named `old_name`
// with a new identifier named `new_name` in an expression tree.
fn (t &Transformer) replace_ident_named(expr ast.Expr, old_name string, new_name string) ast.Expr {
	match expr {
		ast.Ident {
			if expr.name == old_name {
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
				lhs: t.replace_ident_named(expr.lhs, old_name, new_name)
				rhs: t.replace_ident_named(expr.rhs, old_name, new_name)
				pos: expr.pos
			}
		}
		ast.PrefixExpr {
			return ast.PrefixExpr{
				op:   expr.op
				expr: t.replace_ident_named(expr.expr, old_name, new_name)
				pos:  expr.pos
			}
		}
		ast.ParenExpr {
			return ast.ParenExpr{
				expr: t.replace_ident_named(expr.expr, old_name, new_name)
				pos:  expr.pos
			}
		}
		ast.CallExpr {
			mut new_args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				new_args << t.replace_ident_named(arg, old_name, new_name)
			}
			return ast.CallExpr{
				lhs:  t.replace_ident_named(expr.lhs, old_name, new_name)
				args: new_args
				pos:  expr.pos
			}
		}
		ast.CastExpr {
			return ast.CastExpr{
				typ:  expr.typ
				expr: t.replace_ident_named(expr.expr, old_name, new_name)
				pos:  expr.pos
			}
		}
		ast.IndexExpr {
			return ast.IndexExpr{
				lhs:  t.replace_ident_named(expr.lhs, old_name, new_name)
				expr: t.replace_ident_named(expr.expr, old_name, new_name)
				pos:  expr.pos
			}
		}
		ast.SelectorExpr {
			return ast.SelectorExpr{
				lhs: t.replace_ident_named(expr.lhs, old_name, new_name)
				rhs: expr.rhs
				pos: expr.pos
			}
		}
		ast.ModifierExpr {
			return ast.ModifierExpr{
				kind: expr.kind
				expr: t.replace_ident_named(expr.expr, old_name, new_name)
				pos:  expr.pos
			}
		}
		else {
			return expr
		}
	}
}

// get_array_type_str returns the Array_T type string for an array expression using checker type info.
