// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module transformer

import v2.ast
import v2.types
import v2.token

fn is_numeric_literal_expr(expr ast.Expr) bool {
	match expr {
		ast.BasicLiteral {
			return expr.kind == .number
		}
		ast.PrefixExpr {
			return expr.op == .minus && expr.expr is ast.BasicLiteral && expr.expr.kind == .number
		}
		ast.ParenExpr {
			return is_numeric_literal_expr(expr.expr)
		}
		else {
			return false
		}
	}
}

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
fn transformer_object_type(obj types.Object) ?types.Type {
	match obj {
		types.Type {
			return types.Type(obj)
		}
		types.TypeObject {
			return obj.typ
		}
		else {}
	}

	return none
}

fn (t &Transformer) lookup_struct_field_type(struct_name string, field_name string) ?types.Type {
	if struct_name.len == 0 || field_name.len == 0 || struct_name.len > 1024
		|| field_name.len > 1024 || !transformer_string_has_valid_data(struct_name)
		|| !transformer_string_has_valid_data(field_name) {
		return none
	}
	if field_type := t.lookup_struct_field_generic_decl_type(struct_name, field_name) {
		return field_type
	}
	mut sname := struct_name
	mut mod := ''
	dunder := struct_name.last_index('__') or { -1 }
	if dunder >= 0 {
		mod = struct_name[..dunder]
		sname = struct_name[dunder + 2..]
		if field_type := t.cached_struct_field_types[struct_field_lookup_cache_key(mod, sname,
			field_name)]
		{
			return field_type
		}
		if field_type := t.cached_struct_field_types[struct_field_generic_decl_key(struct_name,
			field_name)]
		{
			return field_type
		}
	} else if dot := struct_name.last_index('.') {
		mod = struct_name[..dot]
		sname = struct_name[dot + 1..]
		if field_type := t.cached_struct_field_types[struct_field_lookup_cache_key(mod, sname,
			field_name)]
		{
			return field_type
		}
		dot_name := struct_name.replace('.', '__')
		if field_type := t.cached_struct_field_types[struct_field_generic_decl_key(dot_name,
			field_name)]
		{
			return field_type
		}
	} else if t.cur_module != '' {
		if field_type := t.cached_struct_field_types[struct_field_lookup_cache_key(t.cur_module,
			sname, field_name)]
		{
			return field_type
		}
	}
	// Try module scope first if qualified
	if mod != '' {
		if scope := t.cached_scopes[mod] {
			if obj := scope.objects[sname] {
				if typ := transformer_object_type(obj) {
					if typ is types.Struct {
						if field_type := t.struct_field_type(typ, field_name) {
							return field_type
						}
					}
				}
			}
		}
	}
	// For unqualified names, try the current module first to avoid
	// collisions (e.g., ast.OptionType vs types.OptionType).
	if mod == '' && t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		if cur_scope := t.cached_scopes[t.cur_module] {
			if obj := cur_scope.objects[sname] {
				if typ := transformer_object_type(obj) {
					if typ is types.Struct {
						if field_type := t.struct_field_type(typ, field_name) {
							return field_type
						}
					}
				}
			}
		}
	}
	// Fallback: scan all scopes
	for sk in t.cached_scope_keys {
		scope := t.cached_scopes[sk] or { continue }
		if obj := scope.objects[struct_name] {
			if typ := transformer_object_type(obj) {
				if typ is types.Struct {
					if field_type := t.struct_field_type(typ, field_name) {
						return field_type
					}
				}
			}
		}
		if sname != struct_name {
			if obj := scope.objects[sname] {
				if typ := transformer_object_type(obj) {
					if typ is types.Struct {
						if field_type := t.struct_field_type(typ, field_name) {
							return field_type
						}
					}
				}
			}
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_field_generic_decl_type(struct_name string, field_name string) ?types.Type {
	if struct_name == '' || field_name == '' {
		return none
	}
	if field_type := t.struct_field_generic_decl_type_for_candidate(struct_name, field_name) {
		return field_type
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		if short_name != '' && short_name != struct_name {
			if field_type := t.struct_field_generic_decl_type_for_candidate(short_name, field_name) {
				return field_type
			}
		}
	}
	if struct_name.index_u8(`.`) >= 0 {
		dot_name := struct_name.replace('.', '__')
		if dot_name != struct_name {
			if field_type := t.struct_field_generic_decl_type_for_candidate(dot_name, field_name) {
				return field_type
			}
		}
		short_name := struct_name.all_after_last('.')
		if short_name != '' && short_name != struct_name && short_name != dot_name {
			if field_type := t.struct_field_generic_decl_type_for_candidate(short_name, field_name) {
				return field_type
			}
		}
	}
	return none
}

fn (t &Transformer) struct_field_generic_decl_type_for_candidate(struct_name string, field_name string) ?types.Type {
	key := struct_field_generic_decl_key(struct_name, field_name)
	field_type := t.struct_field_generic_decl_types[key] or { return none }
	return field_type
}

fn (t &Transformer) lookup_struct_field_generic_decl_bindings(struct_name string, field_name string) ?map[string]types.Type {
	if struct_name == '' || field_name == '' {
		return none
	}
	if bindings := t.struct_field_generic_decl_bindings_for_candidate(struct_name, field_name) {
		return bindings
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		if short_name != '' && short_name != struct_name {
			if bindings := t.struct_field_generic_decl_bindings_for_candidate(short_name,
				field_name)
			{
				return bindings
			}
		}
	}
	if struct_name.index_u8(`.`) >= 0 {
		dot_name := struct_name.replace('.', '__')
		if dot_name != struct_name {
			if bindings := t.struct_field_generic_decl_bindings_for_candidate(dot_name, field_name) {
				return bindings
			}
		}
		short_name := struct_name.all_after_last('.')
		if short_name != '' && short_name != struct_name && short_name != dot_name {
			if bindings := t.struct_field_generic_decl_bindings_for_candidate(short_name,
				field_name)
			{
				return bindings
			}
		}
	}
	return none
}

fn (t &Transformer) struct_field_generic_decl_bindings_for_candidate(struct_name string, field_name string) ?map[string]types.Type {
	key := struct_field_generic_decl_key(struct_name, field_name)
	if bindings := t.struct_field_generic_decl_bindings[key] {
		return bindings.clone()
	}
	return none
}

fn struct_field_lookup_candidates(struct_name string) []string {
	mut candidates := []string{}
	if struct_name != '' {
		candidates << struct_name
		if struct_name.contains('__') {
			short_name := struct_name.all_after_last('__')
			if short_name != '' && short_name !in candidates {
				candidates << short_name
			}
		}
		if struct_name.contains('.') {
			dot_name := struct_name.replace('.', '__')
			if dot_name !in candidates {
				candidates << dot_name
			}
			short_name := struct_name.all_after_last('.')
			if short_name != '' && short_name !in candidates {
				candidates << short_name
			}
		}
	}
	return candidates
}

fn embedded_type_name_matches(embedded_name string, field_name string) bool {
	if embedded_name == field_name {
		return true
	}
	if embedded_name.contains('__') && embedded_name.all_after_last('__') == field_name {
		return true
	}
	if embedded_name.contains('.') && embedded_name.all_after_last('.') == field_name {
		return true
	}
	return false
}

fn (t &Transformer) live_embedded_struct_type(embedded types.Struct) types.Struct {
	if embedded.name != '' {
		if live_type := t.lookup_struct_type_any_module(embedded.name) {
			return live_type
		}
		if live_type := t.lookup_type(embedded.name) {
			if live_type is types.Struct {
				return live_type
			}
		}
		if embedded.name.contains('__') {
			short_name := embedded.name.all_after_last('__')
			if live_type := t.lookup_struct_type_any_module(short_name) {
				return live_type
			}
			if live_type := t.lookup_type(short_name) {
				if live_type is types.Struct {
					return live_type
				}
			}
		}
	}
	return embedded
}

fn (t &Transformer) struct_field_type(struct_type types.Struct, field_name string) ?types.Type {
	mut seen := map[string]bool{}
	return t.struct_field_type_inner(struct_type, field_name, mut seen)
}

fn (t &Transformer) struct_field_type_inner(struct_type types.Struct, field_name string, mut seen map[string]bool) ?types.Type {
	if struct_type.name != '' {
		if struct_type.name in seen {
			return none
		}
		seen[struct_type.name] = true
	}
	for field in struct_type.fields {
		if !transformer_string_has_valid_data(field.name) {
			continue
		}
		if field.name == field_name {
			return field.typ
		}
	}
	for embedded in struct_type.embedded {
		live_embedded := t.live_embedded_struct_type(embedded)
		if embedded_type_name_matches(live_embedded.name, field_name) {
			return types.Type(live_embedded)
		}
		if field_type := t.struct_field_type_inner(live_embedded, field_name, mut seen) {
			return field_type
		}
	}
	return none
}

// open_scope creates a new nested scope
fn (mut t Transformer) apply_smartcast_field_access_ctx(sumtype_expr ast.Expr, field_name string, ctx SmartcastContext) ast.Expr {
	// variant (short name) is used for union member access
	// variant_full (full name) is used for type cast
	variant_short := ctx.variant
	// Extract simple variant name for _data._ accessor
	// Union fields use: _Null (same-module Ident), _time__Time (cross-module SelectorExpr),
	// _Array_json2__Any (composite). Only strip module prefix for same-module types.
	// Union fields use the name as seen from the declaring module:
	// same-module Ident → _Null, cross-module SelectorExpr → _time__Time.
	// Strip module prefix when the variant's module matches the sumtype's module,
	// because those variants were declared as Idents (no module prefix in the union).
	sumtype_module := if ctx.sumtype.contains('__') {
		ctx.sumtype.all_before_last('__')
	} else {
		''
	}
	variant_simple := if variant_short.starts_with('Array_') || variant_short.starts_with('Map_') {
		// For composite types, use the short name to match union member
		variant_short
	} else if variant_short.contains('__') {
		mod_prefix := variant_short.all_before_last('__')
		if mod_prefix == sumtype_module {
			variant_short.all_after_last('__')
		} else {
			variant_short
		}
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
	// For interface smartcasts, use _object instead of _data
	is_interface_ctx := ctx.sumtype.starts_with('__iface__')
	if is_interface_ctx {
		object_access := t.synth_selector(transformed_base, '_object', types.Type(types.voidptr_))
		cast_expr := ast.CastExpr{
			typ:  ast.Ident{
				name: '${mangled_variant}*'
			}
			expr: object_access
		}
		return t.synth_selector_from_struct(ast.ParenExpr{
			expr: cast_expr
		}, field_name, mangled_variant)
	}
	// Create data access.
	// For native backends (arm64/x64): _data is a plain i64 (void pointer) in the SSA struct.
	// No union variant sub-field exists, so just use _data directly.
	// For C backends: _data is a union, so access _data._variant for the specific member.
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	data_access := t.synth_selector(transformed_base, '_data', types.Type(types.voidptr_))
	variant_access := if is_native_backend {
		data_access
	} else {
		t.synth_selector(data_access, '_${variant_simple}', types.Type(types.voidptr_))
	}
	if t.is_eval_backend() {
		return t.synth_selector_from_struct(variant_access, field_name, mangled_variant)
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
	if !expr_array_has_valid_data(expr.exprs) {
		return ast.Expr(expr)
	}
	is_native_backend := t.pref != unsafe { nil } && t.is_native_be
	native_interface_elem_type := if is_native_backend {
		t.get_interface_array_init_concrete_type(&expr) or { '' }
	} else {
		''
	}
	// Transform value expressions
	mut exprs := []ast.Expr{cap: expr.exprs.len}
	for e in expr.exprs {
		if native_interface_elem_type != '' {
			if inner := t.get_interface_cast_inner_expr(e) {
				exprs << t.transform_expr(inner)
				continue
			}
		}
		exprs << t.transform_expr(e)
	}

	// Check if this is a fixed-size array
	mut is_fixed := false
	mut array_typ := expr.typ
	mut elem_type_expr := ast.empty_expr
	has_fixed_marker := array_init_has_fixed_len_marker(expr)
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
		if !has_fixed_marker {
			if literal_type := t.get_array_init_expr_type(expr) {
				if literal_type is types.Array {
					array_typ = t.type_to_ast_type_expr(literal_type)
					elem_type_expr = t.type_to_ast_type_expr(literal_type.elem_type)
				}
			}
		}
	}
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
	if native_interface_elem_type != '' {
		elem_type_expr = ast.Expr(ast.Ident{
			name: native_interface_elem_type
		})
		array_typ = ast.Expr(ast.Type(ast.ArrayType{
			elem_type: elem_type_expr
		}))
	}
	// Also check for [x, y, z]! syntax - parser marks this with len: PostfixExpr{op: .not}
	if has_fixed_marker {
		is_fixed = true
		if expr.typ is ast.EmptyExpr {
			array_typ = ast.Expr(ast.empty_expr)
		}
		if array_typ is ast.Type && array_typ is ast.ArrayType {
			array_typ = ast.Expr(ast.Type(ast.ArrayFixedType{
				len:       ast.BasicLiteral{
					kind:  .number
					value: '${expr.exprs.len}'
				}
				elem_type: elem_type_expr
			}))
		} else if array_typ is ast.EmptyExpr {
			if elem_type := t.get_array_init_elem_expr_type(if expr.exprs.len > 0 {
				expr.exprs[0]
			} else {
				expr.init
			})
			{
				array_typ = ast.Expr(ast.Type(ast.ArrayFixedType{
					len:       ast.BasicLiteral{
						kind:  .number
						value: '${expr.exprs.len}'
					}
					elem_type: t.type_to_ast_type_expr(elem_type)
				}))
			} else if elem_type_expr !is ast.EmptyExpr {
				array_typ = ast.Expr(ast.Type(ast.ArrayFixedType{
					len:       ast.BasicLiteral{
						kind:  .number
						value: '${expr.exprs.len}'
					}
					elem_type: elem_type_expr
				}))
			}
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

	if t.is_eval_backend() {
		return ast.ArrayInitExpr{
			typ:         array_typ
			exprs:       exprs
			init:        if expr.init !is ast.EmptyExpr {
				t.transform_expr(expr.init)
			} else {
				expr.init
			}
			cap:         if expr.cap !is ast.EmptyExpr {
				t.transform_expr(expr.cap)
			} else {
				expr.cap
			}
			len:         if expr.len !is ast.EmptyExpr {
				t.transform_expr(expr.len)
			} else {
				expr.len
			}
			update_expr: if expr.update_expr !is ast.EmptyExpr {
				t.transform_expr(expr.update_expr)
			} else {
				expr.update_expr
			}
			pos:         expr.pos
		}
	}

	// Spread syntax `[...base, e1, e2]` — lower to
	// builtin__new_array_from_array_and_c_array(array__clone_to_depth(&base, depth),
	//   n, sizeof(elem), {e1, e2}).
	if expr.update_expr !is ast.EmptyExpr {
		return t.transform_array_spread_expr(expr, exprs, elem_type_expr)
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
		mut init_expr := ast.Expr(if expr.init !is ast.EmptyExpr {
			t.transform_expr(expr.init)
		} else {
			ast.Expr(ast.Ident{
				name: 'nil'
			})
		})
		if expr.init !is ast.EmptyExpr && is_numeric_literal_expr(init_expr) {
			init_expr = ast.Expr(ast.CastExpr{
				typ:  sizeof_expr
				expr: init_expr
			})
		}
		// If init expression uses `index`, expand to a for-loop that assigns each element.
		if expr.init !is ast.EmptyExpr && t.expr_contains_ident_named(init_expr, 'index') {
			return t.expand_array_init_with_index(len_expr, cap_expr, sizeof_expr, init_expr,
				expr.pos)
		}
		// When element type is a reference type (array or map) and no explicit init,
		// synthesize a proper default so inner elements get initialized correctly
		// (not zero-filled via NULL, which leaves element_size=0 or hash_fn=null).
		// sizeof_expr holds elem_type_expr before smartcasting, so use it to avoid issues.
		elem_is_nested_array := elem_type_expr is ast.Type && elem_type_expr is ast.ArrayType
		elem_is_map := elem_type_expr is ast.Type && elem_type_expr is ast.MapType
		if expr.init is ast.EmptyExpr && elem_is_nested_array {
			init_expr = ast.Expr(t.transform_array_init_expr(ast.ArrayInitExpr{
				typ: sizeof_expr
			}))
		}
		if expr.init is ast.EmptyExpr && elem_is_map {
			init_expr = ast.Expr(t.transform_map_init_expr(ast.MapInitExpr{
				typ: sizeof_expr
			}))
		}
		// When element type is a struct with map fields and no explicit init,
		// synthesize a struct default and use for-loop expansion so each element
		// gets its own map allocation (memcpy would share internal pointers).
		if expr.init is ast.EmptyExpr && !elem_is_nested_array && !elem_is_map {
			if elem_type := t.get_expr_type(elem_type_expr) {
				elem_base := t.unwrap_alias_and_pointer_type(elem_type)
				if elem_base is types.Struct {
					mut field_inits := []ast.FieldInit{}
					for field in elem_base.fields {
						if field.typ is types.Map {
							map_type_expr := t.type_to_ast_type_expr(field.typ)
							map_init := t.transform_map_init_expr(ast.MapInitExpr{
								typ: map_type_expr
							})
							field_inits << ast.FieldInit{
								name:  field.name
								value: map_init
							}
						}
					}
					if field_inits.len > 0 {
						struct_init := ast.Expr(ast.InitExpr{
							typ:    elem_type_expr
							fields: field_inits
						})
						return t.expand_array_init_with_index(len_expr, cap_expr, sizeof_expr,
							struct_init, expr.pos)
					}
				}
			}
		}
		// When init value is an array, use __new_array_with_array_default for deep cloning
		// (shallow memcpy would share data pointers between all elements)
		mut init_is_array := expr.init is ast.ArrayInitExpr
		if !init_is_array && elem_is_nested_array && expr.init is ast.EmptyExpr {
			init_is_array = true
		}
		if !init_is_array {
			if init_type := t.get_expr_type(expr.init) {
				init_base := t.unwrap_alias_and_pointer_type(init_type)
				init_is_array = init_base is types.Array
			}
		}
		if init_is_array {
			return ast.CallExpr{
				lhs:  ast.Ident{
					name: '__new_array_with_array_default'
				}
				args: [
					len_expr,
					cap_expr,
					ast.Expr(ast.KeywordOperator{
						op:    .key_sizeof
						exprs: [sizeof_expr]
					}),
					init_expr,
					// depth parameter for clone_to_depth
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '3'
					}),
				]
				pos:  expr.pos
			}
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

	// Create proper array type for the inner ArrayInitExpr.
	// Use the resolved elem_type expression when available (preserves structured AST
	// type info like ArrayType, PrefixExpr for &T, etc.). Only fall back to the mangled
	// name string when no structured expression exists — the name-based path can lose
	// type structure (e.g., 'Array_int*' gets misinterpreted as ptr(array) in SSA).
	inner_elem_type := if elem_type_expr_resolved !is ast.EmptyExpr {
		elem_type_expr_resolved
	} else {
		ast.Expr(ast.Ident{
			name: elem_type_name
		})
	}
	inner_array_typ := ast.Type(ast.ArrayType{
		elem_type: inner_elem_type
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

fn (mut t Transformer) transform_array_spread_expr(expr ast.ArrayInitExpr, exprs []ast.Expr, hint_elem_type_expr ast.Expr) ast.Expr {
	update_expr := t.transform_expr(expr.update_expr)
	// Resolve element type from the base array.
	mut elem_type_expr := hint_elem_type_expr
	mut clone_depth := 0
	if base_type := t.get_expr_type(expr.update_expr) {
		base_unwrapped := t.unwrap_alias_and_pointer_type(base_type)
		if base_unwrapped is types.Array {
			if elem_type_expr is ast.EmptyExpr {
				elem_type_expr = t.type_to_ast_type_expr(base_unwrapped.elem_type)
			}
			// Match V2 `.clone()` semantics for nested arrays: deep-clone inner
			// arrays so `[...nested]` doesn't share inner storage with `nested`.
			nesting := t.get_array_nesting_depth(base_type)
			if nesting > 1 {
				clone_depth = nesting - 1
			}
		}
	}
	sizeof_arg := if elem_type_expr !is ast.EmptyExpr {
		elem_type_expr
	} else {
		ast.Expr(ast.Ident{
			name: 'int'
		})
	}
	cloned_base := ast.Expr(ast.CallExpr{
		lhs:  ast.Ident{
			name: 'array__clone_to_depth'
		}
		args: [
			update_expr,
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${clone_depth}'
			}),
		]
		pos:  expr.pos
	})
	new_count := exprs.len
	mut data_arg := ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.Ident{
			name: 'voidptr'
		})
		expr: ast.Expr(ast.BasicLiteral{
			kind:  .number
			value: '0'
		})
	})
	if new_count > 0 {
		inner_array_typ := ast.Type(ast.ArrayType{
			elem_type: sizeof_arg
		})
		data_arg = ast.Expr(ast.ArrayInitExpr{
			typ:   ast.Expr(inner_array_typ)
			exprs: exprs
		})
	}
	return ast.CallExpr{
		lhs:  ast.Ident{
			name: 'builtin__new_array_from_array_and_c_array'
		}
		args: [
			cloned_base,
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '${new_count}'
			}),
			ast.Expr(ast.KeywordOperator{
				op:    .key_sizeof
				exprs: [sizeof_arg]
			}),
			data_arg,
		]
		pos:  expr.pos
	}
}

fn array_expr_with_expected_type(expr ast.Expr, expected_type ast.Expr) ast.Expr {
	if expr is ast.ArrayInitExpr {
		return ast.Expr(array_init_with_expected_type(expr, expected_type))
	}
	return expr
}

fn array_init_with_expected_type(expr ast.ArrayInitExpr, expected_type ast.Expr) ast.ArrayInitExpr {
	if expected_type is ast.EmptyExpr {
		return expr
	}
	mut rewritten_exprs := []ast.Expr{cap: expr.exprs.len}
	mut elem_type := ast.Expr(ast.empty_expr)
	if expected_type is ast.Type {
		expected_ast_type := expected_type as ast.Type
		if expected_ast_type is ast.ArrayType {
			array_type := expected_ast_type as ast.ArrayType
			elem_type = array_type.elem_type
		}
	}
	for item in expr.exprs {
		if item is ast.ArrayInitExpr && elem_type !is ast.EmptyExpr {
			rewritten_exprs << ast.Expr(array_init_with_expected_type(item, elem_type))
		} else {
			rewritten_exprs << item
		}
	}
	return ast.ArrayInitExpr{
		typ:   expected_type
		exprs: rewritten_exprs
		init:  expr.init
		cap:   expr.cap
		len:   expr.len
		pos:   expr.pos
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
			expr_typ := expr.typ as ast.Type
			if expr_typ is ast.MapType {
				mt := expr_typ as ast.MapType
				key_type_expr = mt.key_type
				val_type_expr = mt.value_type
				key_type_name = t.expr_to_type_name(mt.key_type)
				have_explicit_map_type = true
			}
		}
		ast.Ident {
			if explicit_map_typ := t.lookup_type(expr.typ.name) {
				if explicit_map := t.unwrap_map_type(explicit_map_typ) {
					key_type_expr = t.type_to_ast_type_expr(explicit_map.key_type)
					val_type_expr = t.type_to_ast_type_expr(explicit_map.value_type)
					key_type_name = t.type_to_c_name(explicit_map.key_type)
					have_explicit_map_type = true
				}
			}
		}
		ast.SelectorExpr {
			explicit_map_name := t.expr_to_type_name(expr.typ)
			if explicit_map_name != '' {
				if explicit_map_typ := t.lookup_type(explicit_map_name) {
					if explicit_map := t.unwrap_map_type(explicit_map_typ) {
						key_type_expr = t.type_to_ast_type_expr(explicit_map.key_type)
						val_type_expr = t.type_to_ast_type_expr(explicit_map.value_type)
						key_type_name = t.type_to_c_name(explicit_map.key_type)
						have_explicit_map_type = true
					}
				}
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
		vals << t.transform_expr(array_expr_with_expected_type(v, val_type_expr))
	}

	if keys.len > 0 && key_type_name != '' && key_type_name != 'int' {
		mut needs_enum_key_resolution := false
		for key_expr in keys {
			if key_expr is ast.SelectorExpr && key_expr.lhs is ast.EmptyExpr {
				needs_enum_key_resolution = true
				break
			}
		}
		if needs_enum_key_resolution {
			for i, key_expr in keys {
				keys[i] = t.transform_expr(t.resolve_enum_shorthand(key_expr, key_type_name))
			}
		}
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

	if t.is_eval_backend() {
		return ast.MapInitExpr{
			typ:  if expr.typ !is ast.EmptyExpr { t.transform_expr(expr.typ) } else { expr.typ }
			keys: keys
			vals: vals
			pos:  expr.pos
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
				expr_typ := expr.typ as ast.Type
				if expr_typ is ast.MapType {
					if t.is_eval_backend() {
						return ast.Expr(ast.MapInitExpr{
							typ:  ast.Expr(ast.Type(expr.typ))
							keys: []ast.Expr{}
							vals: []ast.Expr{}
							pos:  expr.pos
						})
					}
					mt := expr_typ as ast.MapType
					key_type_name := t.expr_to_type_name(mt.key_type)
					hash_fn, eq_fn, clone_fn, free_fn :=
						map_runtime_key_fns_from_type_name(key_type_name)
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

	mut init_typ_expr := expr.typ
	if concrete_typ_expr := t.concrete_generic_struct_type_expr(expr.typ) {
		init_typ_expr = concrete_typ_expr
	}
	typed_expr := ast.InitExpr{
		typ: init_typ_expr
		pos: expr.pos
	}
	if expr.pos.id != 0 {
		if init_typ := t.type_from_init_expr(typed_expr) {
			t.synth_types[expr.pos.id] = init_typ
		}
	}
	// Get the struct type name for field type lookups
	struct_type_name := t.get_init_expr_type_name(init_typ_expr)

	// Transform field values recursively
	// Note: ArrayInitExpr is NOT transformed here because cleanc uses field type info
	// to determine if it's a fixed-size array (which transformer doesn't have access to)
	mut fields := []ast.FieldInit{cap: expr.fields.len}
	positional_field_names := t.struct_positional_field_names(struct_type_name)
	mut positional_idx := 0
	for field in expr.fields {
		mut lookup_field_name := field.name
		if lookup_field_name == '' {
			if positional_idx < positional_field_names.len {
				lookup_field_name = positional_field_names[positional_idx]
			}
			positional_idx++
		}
		// Check if this field is a sum type and needs wrapping
		mut field_type_name := t.get_struct_field_type_name(struct_type_name, lookup_field_name)
		mut expected_field_type := types.Type(types.int_)
		mut has_expected_field_type := false
		mut pretransformed_value := ast.empty_expr
		mut has_pretransformed_value := false
		if direct_type := t.lookup_struct_field_type(struct_type_name, lookup_field_name) {
			expected_field_type = direct_type
			has_expected_field_type = true
			field_type_name = t.type_to_c_name(direct_type)
		} else if direct_type := t.get_init_expr_field_type(init_typ_expr, lookup_field_name) {
			expected_field_type = direct_type
			has_expected_field_type = true
			field_type_name = t.type_to_c_name(direct_type)
		} else if field_type_name != '' {
			if expected_typ := t.lookup_type(field_type_name) {
				expected_field_type = expected_typ
				has_expected_field_type = true
			}
		} else {
			// Fallback to direct type lookup from the init expression type.
			field_type_name = t.get_init_expr_field_type_name(expr.typ, lookup_field_name)
			if field_type_name != '' {
				if expected_typ := t.lookup_type(field_type_name) {
					expected_field_type = expected_typ
					has_expected_field_type = true
				}
			}
		}
		mut field_value := field.value
		is_sumtype_field := t.is_sum_type(field_type_name)
		mut use_direct_sumtype_value := false
		mut direct_sumtype_value := ast.empty_expr
		if has_expected_field_type {
			field_value = t.resolve_expr_with_expected_type(field_value, expected_field_type)
			fv_pos := field_value.pos()
			if !is_sumtype_field && fv_pos.id != 0 {
				t.synth_types[fv_pos.id] = expected_field_type
			}
			if t.is_eval_backend() {
				if expected_field_type is types.OptionType
					|| expected_field_type is types.ResultType {
					base_type_name := t.type_to_c_name(expected_field_type.base_type())
					if t.is_sum_type(base_type_name) {
						if wrapped := t.wrap_sumtype_value(field_value, base_type_name) {
							pretransformed_value = wrapped
							has_pretransformed_value = true
						}
					}
				}
			}
		}
		if is_sumtype_field {
			if direct_value := t.transform_declared_sumtype_value(field_value, field_type_name) {
				direct_sumtype_value = direct_value
				use_direct_sumtype_value = true
			}
			// This is a sum type field - wrap the value in sum type initialization
			if !use_direct_sumtype_value {
				if wrapped := t.wrap_sumtype_value(field_value, field_type_name) {
					fields << ast.FieldInit{
						name:  field.name
						value: wrapped
					}
					continue
				}
			}
		}

		transformed_value := if has_pretransformed_value {
			pretransformed_value
		} else if use_direct_sumtype_value {
			direct_sumtype_value
		} else if field_value is ast.ArrayInitExpr {
			arr_value := field_value as ast.ArrayInitExpr
			// If the array has len/cap but no literal elements (e.g., []int{len: 4}),
			// use the normal transform_expr path which handles __new_array_with_default_noscan
			if arr_value.exprs.len == 0
				&& (arr_value.len !is ast.EmptyExpr || arr_value.cap !is ast.EmptyExpr) {
				t.transform_expr(field_value)
			} else {
				// Transform array elements with sumtype wrapping if needed.
				elem_sumtype := t.get_field_array_elem_sumtype_name(struct_type_name,
					lookup_field_name)
				elem_interface_typ := t.get_field_array_elem_interface_type(struct_type_name,
					lookup_field_name)
				mut new_exprs := []ast.Expr{cap: arr_value.exprs.len}
				for e in arr_value.exprs {
					mut transformed := t.transform_expr(e)
					if elem_sumtype != '' {
						if wrapped := t.wrap_sumtype_value_transformed(transformed, elem_sumtype) {
							new_exprs << wrapped
							continue
						}
					}
					if iface_typ := elem_interface_typ {
						if !t.is_interface_cast(transformed) {
							transformed = ast.Expr(ast.CallOrCastExpr{
								lhs:  t.type_to_ast_type_expr(iface_typ)
								expr: transformed
							})
						}
					}
					new_exprs << transformed
				}
				// Set elem type from struct field so
				// transform_array_init_with_exprs uses the correct element type.
				// This is critical when elements were wrapped in a sum type above,
				// as the C array type must match the wrapped (sum type) elements.
				mut arr_with_type := arr_value
				elem_c_name := t.get_field_array_elem_c_name(struct_type_name, lookup_field_name)
				if elem_c_name != '' {
					arr_with_type = ast.ArrayInitExpr{
						typ:   ast.Expr(ast.Type(ast.ArrayType{
							elem_type: ast.Ident{
								name: elem_c_name
							}
						}))
						exprs: arr_value.exprs
						init:  arr_value.init
						cap:   arr_value.cap
						len:   arr_value.len
						pos:   arr_value.pos
					}
				}
				// Use transform_array_init_with_exprs which handles both fixed and dynamic:
				// - Fixed arrays stay as ArrayInitExpr for cleanc
				// - Dynamic arrays are lowered to builtin__new_array_from_c_array_noscan
				t.transform_array_init_with_exprs(arr_with_type, new_exprs)
			}
		} else {
			t.transform_expr(field_value)
		}
		final_value := if has_expected_field_type {
			t.deref_init_field_value_if_needed(transformed_value, expected_field_type)
		} else {
			transformed_value
		}
		field_name := t.rewrite_embedded_default_field_name(struct_type_name, field.name)
		fields << ast.FieldInit{
			name:  field_name
			value: final_value
		}
	}
	fields = t.add_missing_struct_field_defaults(struct_type_name, fields)

	return ast.InitExpr{
		typ:    init_typ_expr
		fields: fields
		pos:    expr.pos
	}
}

fn (t &Transformer) struct_positional_field_names(struct_name string) []string {
	if struct_name == '' {
		return []string{}
	}
	struct_type := t.lookup_type(struct_name) or { return []string{} }
	base_type := t.unwrap_alias_and_pointer_type(struct_type)
	if base_type !is types.Struct {
		return []string{}
	}
	info := base_type as types.Struct
	mut names := []string{cap: info.fields.len}
	for field in info.fields {
		names << field.name
	}
	return names
}

fn (t &Transformer) type_resolves_to_pointer(typ types.Type) bool {
	mut cur := typ
	for {
		match cur {
			types.Pointer {
				return true
			}
			types.Alias {
				cur = t.live_alias_base_type(cur) or { return false }
			}
			else {
				return false
			}
		}
	}
	return false
}

fn (t &Transformer) deref_init_field_value_if_needed(value ast.Expr, expected types.Type) ast.Expr {
	if t.type_resolves_to_pointer(expected) {
		return value
	}
	expected_base := t.unwrap_alias_and_pointer_type(expected)
	expected_base_c := t.type_to_c_name(expected_base)
	value_typ := t.get_expr_type(value) or { return value }
	if value_typ is types.Pointer {
		value_base := t.unwrap_alias_and_pointer_type(value_typ.base_type)
		value_base_c := t.type_to_c_name(value_base)
		value_short := if value_base_c.contains('__') {
			value_base_c.all_after_last('__')
		} else {
			value_base_c
		}
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
	}
	return value
}

fn (t &Transformer) get_init_expr_field_type(init_typ_expr ast.Expr, field_name string) ?types.Type {
	if !transformer_string_has_valid_data(field_name) {
		return none
	}
	if field_typ := t.generic_init_expr_field_type(init_typ_expr, field_name) {
		return field_typ
	}
	init_typ := t.get_expr_type(init_typ_expr) or { return none }
	base_typ := t.unwrap_alias_and_pointer_type(init_typ)
	if base_typ is types.Struct {
		if field_typ := t.lookup_struct_field_type(base_typ.name, field_name) {
			return field_typ
		}
	}
	return none
}

fn (t &Transformer) get_init_expr_field_type_name(init_typ_expr ast.Expr, field_name string) string {
	if !transformer_string_has_valid_data(field_name) {
		return ''
	}
	if field_typ := t.generic_init_expr_field_type(init_typ_expr, field_name) {
		return t.type_to_name(field_typ)
	}
	init_typ := t.get_expr_type(init_typ_expr) or { return '' }
	base_typ := t.unwrap_alias_and_pointer_type(init_typ)
	if base_typ is types.Struct {
		if field_typ := t.lookup_struct_field_type(base_typ.name, field_name) {
			return t.type_to_name(field_typ)
		}
	}
	return ''
}

fn (t &Transformer) generic_init_expr_field_type(init_typ_expr ast.Expr, field_name string) ?types.Type {
	mut lhs := ast.empty_expr
	mut args := []ast.Expr{}
	match init_typ_expr {
		ast.GenericArgs {
			lhs = init_typ_expr.lhs
			args = init_typ_expr.args.clone()
		}
		ast.GenericArgOrIndexExpr {
			lhs = init_typ_expr.lhs
			args = [init_typ_expr.expr]
		}
		ast.IndexExpr {
			lhs = init_typ_expr.lhs
			args = [init_typ_expr.expr]
		}
		else {
			return none
		}
	}

	base := t.lookup_type_from_expr(lhs) or { return none }
	if base is types.Struct {
		field := struct_field_by_name(base, field_name) or { return none }
		bindings := t.generic_type_arg_bindings(base.generic_params, args) or { return field.typ }
		return substitute_type(field.typ, bindings)
	}
	return none
}

fn (mut t Transformer) add_missing_struct_field_defaults(struct_name string, fields []ast.FieldInit) []ast.FieldInit {
	if struct_name == '' {
		return fields
	}
	struct_type := t.lookup_type(struct_name) or {
		if struct_name.contains('Scope') || struct_name.contains('DenseArray')
			|| struct_name.contains('Env') {
		}
		return fields
	}
	base_type := t.unwrap_alias_and_pointer_type(struct_type)
	if base_type !is types.Struct {
		if struct_name.contains('Scope') || struct_name.contains('DenseArray')
			|| struct_name.contains('Env') {
		}
		return fields
	}
	struct_info := base_type as types.Struct
	mut existing := map[string]bool{}
	mut positional_idx := 0
	for field in fields {
		if field.name == '' {
			// Positional field — map it to the corresponding struct field name
			if positional_idx < struct_info.fields.len {
				positional_name := struct_info.fields[positional_idx].name
				existing[positional_name] = true
				existing[t.rewrite_embedded_default_field_name(struct_name, positional_name)] = true
			}
			positional_idx++
		} else {
			existing[field.name] = true
			existing[t.rewrite_embedded_default_field_name(struct_name, field.name)] = true
		}
	}
	mut out := []ast.FieldInit{cap: fields.len}
	for field in fields {
		out << field
	}
	for struct_field in struct_info.fields {
		field_name := t.rewrite_embedded_default_field_name(struct_name, struct_field.name)
		if struct_field.name in existing || field_name in existing {
			continue
		}
		if struct_field.default_expr !is ast.EmptyExpr
			&& t.is_supported_struct_default_expr(struct_field.default_expr) {
			out << ast.FieldInit{
				name:  field_name
				value: t.transform_struct_field_default_value(struct_name,
					struct_field.default_expr, struct_field.typ)
			}
			continue
		}
		if t.is_pointer_type(struct_field.typ) {
			continue
		}
		field_type := t.unwrap_alias_and_pointer_type(struct_field.typ)
		if field_type is types.Map {
			if struct_name.contains('Scope') || struct_name.contains('Env') {
			}
			map_init := ast.Expr(ast.MapInitExpr{
				typ: t.type_to_ast_type_expr(field_type)
			})
			out << ast.FieldInit{
				name:  field_name
				value: t.transform_expr(map_init)
			}
			continue
		}
		if field_type is types.Array {
			array_init := ast.Expr(ast.ArrayInitExpr{
				typ: t.type_to_ast_type_expr(field_type)
			})
			out << ast.FieldInit{
				name:  field_name
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
				name:  field_name
				value: t.transform_expr(option_none)
			}
			continue
		}
		if field_type is types.String {
			out << ast.FieldInit{
				name:  field_name
				value: ast.StringLiteral{
					kind:  .v
					value: "''"
				}
			}
		}
	}
	// Also fill defaults for fields from embedded structs.
	for emb in struct_info.embedded {
		resolved_emb := t.resolve_embedded_struct_for_default_fields(struct_name, emb)
		for struct_field in resolved_emb.fields {
			raw_field_name := embedded_default_field_name_with_owner(emb.name, struct_field.name)
			field_name := t.embedded_default_field_name(struct_name, emb.name, struct_field.name)
			if struct_field.name in existing || raw_field_name in existing || field_name in existing {
				continue
			}
			if struct_field.default_expr !is ast.EmptyExpr
				&& t.is_supported_struct_default_expr(struct_field.default_expr) {
				mut emb_struct_name := struct_name
				if resolved_emb.name != '' {
					emb_struct_name = t.type_to_c_name(types.Type(resolved_emb))
				}
				out << ast.FieldInit{
					name:  field_name
					value: t.transform_struct_field_default_value(emb_struct_name,
						struct_field.default_expr, struct_field.typ)
				}
				continue
			}
			if t.is_pointer_type(struct_field.typ) {
				continue
			}
			field_type := t.unwrap_alias_and_pointer_type(struct_field.typ)
			if field_type is types.Map {
				map_init := ast.Expr(ast.MapInitExpr{
					typ: t.type_to_ast_type_expr(field_type)
				})
				out << ast.FieldInit{
					name:  field_name
					value: t.transform_expr(map_init)
				}
				continue
			}
			if field_type is types.Array {
				array_init := ast.Expr(ast.ArrayInitExpr{
					typ: t.type_to_ast_type_expr(field_type)
				})
				out << ast.FieldInit{
					name:  field_name
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
					name:  field_name
					value: t.transform_expr(option_none)
				}
				continue
			}
			if field_type is types.String {
				out << ast.FieldInit{
					name:  field_name
					value: ast.StringLiteral{
						kind:  .v
						value: "''"
					}
				}
			}
		}
		if resolved_emb.fields.len == 0 {
			if info := t.resolve_embedded_decl_for_default_fields(struct_name, emb) {
				out = t.add_missing_embedded_decl_defaults(struct_name, emb, info, out, existing)
			}
		}
	}
	return out
}

fn embedded_default_field_name_with_owner(embedded_name string, field_name string) string {
	if field_name.contains('.') {
		return field_name
	}
	owner := embedded_concrete_owner_name_from_type_name(embedded_name)
	if owner == '' {
		return field_name
	}
	return '${owner}.${field_name}'
}

fn (t &Transformer) embedded_default_field_name(parent_struct_name string, embedded_name string, field_name string) string {
	if field_name.contains('.') {
		return t.rewrite_embedded_default_field_name(parent_struct_name, field_name)
	}
	mut owner := embedded_concrete_owner_name_from_type_name(embedded_name)
	if concrete_owner := t.concrete_embedded_owner_name(parent_struct_name, owner) {
		owner = concrete_owner
	}
	if owner == '' {
		return field_name
	}
	return '${owner}.${field_name}'
}

fn (t &Transformer) resolve_embedded_struct_for_default_fields(parent_struct_name string, embedded types.Struct) types.Struct {
	owner := embedded_concrete_owner_name_from_type_name(embedded.name)
	if concrete_owner := t.concrete_embedded_owner_name(parent_struct_name, owner) {
		if concrete_owner != owner {
			module_name := embedded_struct_module_name(embedded.name, owner)
			if resolved := t.lookup_struct_type_in_module(module_name, concrete_owner) {
				return resolved
			}
		}
	}
	return t.live_embedded_struct_type(embedded)
}

fn (t &Transformer) resolve_embedded_decl_for_default_fields(parent_struct_name string, embedded types.Struct) ?StructDefaultDeclInfo {
	owner := embedded_concrete_owner_name_from_type_name(embedded.name)
	if concrete_owner := t.concrete_embedded_owner_name(parent_struct_name, owner) {
		if concrete_owner != owner {
			module_name := embedded_struct_module_name(embedded.name, owner)
			if module_name != '' {
				if info := t.lookup_struct_default_decl_info('${module_name}__${concrete_owner}') {
					return info
				}
			}
			if info := t.lookup_struct_default_decl_info(concrete_owner) {
				return info
			}
		}
	}
	if info := t.lookup_struct_default_decl_info(embedded.name) {
		return info
	}
	if embedded.name.contains('__') {
		return t.lookup_struct_default_decl_info(embedded.name.all_after_last('__'))
	}
	return none
}

fn (t &Transformer) lookup_struct_default_decl_info(name string) ?StructDefaultDeclInfo {
	if name == '' {
		return none
	}
	return t.struct_default_decl_infos[name] or { return none }
}

fn (mut t Transformer) add_missing_embedded_decl_defaults(parent_struct_name string, embedded types.Struct, info StructDefaultDeclInfo, fields []ast.FieldInit, existing map[string]bool) []ast.FieldInit {
	mut out := fields.clone()
	base_owner := embedded_concrete_owner_name_from_type_name(embedded.name)
	mut concrete_owner := base_owner
	if rewritten_owner := t.concrete_embedded_owner_name(parent_struct_name, base_owner) {
		concrete_owner = rewritten_owner
	}
	emb_struct_name := generic_struct_decl_c_name(info.decl, info.module_name)
	for field in info.decl.fields {
		if field.name == '' {
			continue
		}
		raw_field_name := '${base_owner}.${field.name}'
		field_name := '${concrete_owner}.${field.name}'
		if raw_field_name in existing || field_name in existing {
			continue
		}
		if field.value !is ast.EmptyExpr && t.is_supported_struct_default_expr(field.value) {
			field_type := t.struct_decl_field_type(info, field) or { continue }
			out << ast.FieldInit{
				name:  field_name
				value: t.transform_struct_field_default_value(emb_struct_name, field.value,
					field_type)
			}
			continue
		}
		field_type := t.struct_decl_field_type(info, field) or { continue }
		if t.is_pointer_type(field_type) {
			continue
		}
		base_type := t.unwrap_alias_and_pointer_type(field_type)
		if base_type is types.Map {
			map_init := ast.Expr(ast.MapInitExpr{
				typ: t.type_to_ast_type_expr(base_type)
			})
			out << ast.FieldInit{
				name:  field_name
				value: t.transform_expr(map_init)
			}
			continue
		}
		if base_type is types.Array {
			array_init := ast.Expr(ast.ArrayInitExpr{
				typ: t.type_to_ast_type_expr(base_type)
			})
			out << ast.FieldInit{
				name:  field_name
				value: t.transform_expr(array_init)
			}
			continue
		}
		if base_type is types.OptionType {
			option_none := ast.Expr(ast.InitExpr{
				typ:    t.type_to_ast_type_expr(base_type)
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
				name:  field_name
				value: t.transform_expr(option_none)
			}
			continue
		}
		if base_type is types.String {
			out << ast.FieldInit{
				name:  field_name
				value: ast.StringLiteral{
					kind:  .v
					value: "''"
				}
			}
		}
	}
	return out
}

fn (mut t Transformer) struct_decl_field_type(info StructDefaultDeclInfo, field ast.FieldDecl) ?types.Type {
	old_module := t.cur_module
	old_scope := t.scope
	t.cur_module = info.module_name
	if scope := t.get_module_scope(info.module_name) {
		t.scope = scope
	} else {
		t.scope = unsafe { nil }
	}
	field_type := t.lookup_type_from_expr(field.typ) or {
		t.cur_module = old_module
		t.scope = old_scope
		return none
	}
	t.cur_module = old_module
	t.scope = old_scope
	return field_type
}

fn embedded_struct_module_name(type_name string, owner string) string {
	if type_name == '' || owner == '' {
		return ''
	}
	suffix := '__${owner}'
	if type_name.ends_with(suffix) {
		return type_name[..type_name.len - suffix.len]
	}
	if type_name.contains('__') {
		return type_name.all_before_last('__')
	}
	return ''
}

fn (t &Transformer) lookup_struct_type_in_module(module_name string, type_name string) ?types.Struct {
	if module_name != '' {
		for candidate in [module_name, module_name.replace('__', '.')] {
			if scope := t.get_module_scope(candidate) {
				if typ := scope.lookup_type(type_name) {
					if typ is types.Struct {
						return typ
					}
				}
			}
		}
	}
	if typ := t.lookup_type(type_name) {
		if typ is types.Struct {
			return typ
		}
	}
	return none
}

fn (mut t Transformer) transform_struct_field_default_value(struct_name string, expr ast.Expr, expected types.Type) ast.Expr {
	resolved := t.resolve_expr_with_expected_type(expr, expected)
	base := t.unwrap_alias_and_pointer_type(expected)
	if base is types.Interface && !t.is_interface_cast(resolved) {
		return t.transform_expr(ast.CallOrCastExpr{
			lhs:  t.type_to_ast_type_expr(expected)
			expr: resolved
			pos:  resolved.pos()
		})
	}
	return t.transform_struct_field_default_expr(struct_name, resolved)
}

fn (mut t Transformer) transform_struct_field_default_expr(struct_name string, expr ast.Expr) ast.Expr {
	if struct_name.contains('__') {
		module_name := struct_name.all_before_last('__')
		if module_name != '' && module_name != t.cur_module {
			// Cross-module const defaults like `ast.empty_expr` must be qualified.
			if expr is ast.Ident {
				if expr.name.contains('__') {
					return expr
				}
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
	if generic_type_name := t.generic_init_type_name(typ) {
		return t.qualify_type_name(generic_type_name)
	}
	if typ is ast.Ident {
		if synth_typ := t.get_synth_type(typ.pos) {
			return t.type_to_c_name(synth_typ)
		}
		base_name := typ.name
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
			if scope := t.cached_scopes[t.cur_module] {
				if obj := scope.objects[base_name] {
					if obj is types.Type {
						return '${t.cur_module}__${base_name}'
					}
				}
			}
			if t.lookup_method_cached('${t.cur_module}__${base_name}', 'msg') != none {
				return '${t.cur_module}__${base_name}'
			}
		}
		return t.qualify_type_name(base_name)
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
	// For qualified names (e.g. "ast__CallExpr"), try the module scope directly first.
	dunder := struct_name.index('__') or { -1 }
	if dunder >= 0 {
		mod_name := struct_name[..dunder]
		last_dunder := struct_name.last_index('__') or { dunder }
		short_name := struct_name[last_dunder + 2..]
		if mod_scope := t.cached_scopes[mod_name] {
			if obj := mod_scope.objects[short_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
			// Also try the full qualified name
			if obj := mod_scope.objects[struct_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
		}
	}
	// Prefer the current module scope for non-qualified names
	// to avoid collisions (e.g., ast.FnType vs types.FnType both named "FnType").
	if dunder < 0 && t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		if cur_scope := t.cached_scopes[t.cur_module] {
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
	// Fallback: scan all scopes
	scope_keys2 := t.cached_scopes.keys()
	for sk in scope_keys2 {
		scope := t.cached_scopes[sk] or { continue }
		if obj := scope.objects[struct_name] {
			if obj is types.Type {
				return t.get_field_type_name(obj, field_name)
			}
		}
		if dunder >= 0 {
			short_name := struct_name[struct_name.last_index('__') or { dunder } + 2..]
			if obj := scope.objects[short_name] {
				if obj is types.Type {
					return t.get_field_type_name(obj, field_name)
				}
			}
		} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
			mangled := '${t.cur_module}__${struct_name}'
			if obj := scope.objects[mangled] {
				if obj is types.Type {
					return t.get_field_type_name(obj, field_name)
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
	dunder := struct_name.index('__') or { -1 }
	if dunder >= 0 {
		lookup_module = struct_name[..dunder]
		last_dunder := struct_name.last_index('__') or { dunder }
		lookup_name = struct_name[last_dunder + 2..]
	}
	// Fast path: try the target module scope directly
	if lookup_module != '' {
		if mod_scope := t.cached_scopes[lookup_module] {
			if obj := mod_scope.objects[lookup_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
			if obj := mod_scope.objects[struct_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
		}
	}
	// Try current module scope
	if t.cur_module != '' {
		if cur_scope := t.cached_scopes[t.cur_module] {
			if obj := cur_scope.objects[struct_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
			if obj := cur_scope.objects[lookup_name] {
				if obj is types.Type {
					result := t.get_field_type_name(obj, field_name)
					if result != '' {
						return result
					}
				}
			}
		}
	}
	// Fallback: scan all scopes
	scope_keys3 := t.cached_scopes.keys()
	for sk in scope_keys3 {
		scope := t.cached_scopes[sk] or { continue }
		if obj := scope.objects[struct_name] {
			if obj is types.Type {
				return t.get_field_type_name(obj, field_name)
			}
		}
		if obj := scope.objects[lookup_name] {
			if obj is types.Type {
				return t.get_field_type_name(obj, field_name)
			}
		}
	}
	return ''
}

// get_field_type_name gets the type name of a field from a Type
fn (t &Transformer) get_field_type_name(typ types.Type, field_name string) string {
	if typ is types.Struct {
		if field_type := t.struct_field_type(typ, field_name) {
			return t.type_to_name(field_type)
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
	// Compute short name once outside the loop
	dunder := struct_name.index('__') or { -1 }
	short_name := if dunder >= 0 {
		last_dunder := struct_name.last_index('__') or { dunder }
		struct_name[last_dunder + 2..]
	} else {
		struct_name
	}
	// Try module scope directly first for qualified names
	if dunder >= 0 {
		mod_name := struct_name[..dunder]
		if mod_scope := t.cached_scopes[mod_name] {
			if obj := mod_scope.objects[short_name] {
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
	scope_keys4 := t.cached_scopes.keys()
	for sk in scope_keys4 {
		scope := t.cached_scopes[sk] or { continue }
		if obj := scope.objects[struct_name] {
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
		if dunder >= 0 {
			if obj := scope.objects[short_name] {
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
	return ''
}

fn (t &Transformer) get_field_array_elem_interface_type(struct_name string, field_name string) ?types.Type {
	field_typ := t.lookup_struct_field_type(struct_name, field_name) or { return none }
	match field_typ {
		types.Array {
			et := field_typ.elem_type
			if et is types.Interface {
				return et
			}
			if et is types.Alias && et.base_type is types.Interface {
				return et
			}
		}
		types.Alias {
			if field_typ.base_type is types.Array {
				et := field_typ.base_type.elem_type
				if et is types.Interface {
					return et
				}
				if et is types.Alias && et.base_type is types.Interface {
					return et
				}
			}
		}
		else {}
	}

	return none
}

// get_field_array_elem_c_name returns the C type name for the element type of an array field
fn (t &Transformer) get_field_array_elem_c_name(struct_name string, field_name string) string {
	field_typ := t.lookup_struct_field_type(struct_name, field_name) or { return '' }
	if field_typ is types.Array {
		return t.type_to_c_name(field_typ.elem_type)
	}
	return ''
}

// type_to_name converts a Type to its name string
fn (t &Transformer) get_struct_field_type(expr ast.SelectorExpr) ?types.Type {
	// Try to get the struct type from scope (for local variables and receivers)
	mut struct_type_name := ''
	if !transformer_string_has_valid_data(expr.rhs.name) {
		return none
	}
	if smartcast_type := t.smartcast_type_for_expr(expr.lhs) {
		if field_typ := t.field_type_from_receiver_type(smartcast_type, expr.rhs.name) {
			return field_typ
		}
	}
	if expr.lhs is ast.Ident {
		lhs_name := expr.lhs.name
		if !transformer_string_has_valid_data(lhs_name) {
			return none
		}
		if lhs_type := t.lookup_var_type(lhs_name) {
			if field_typ := t.field_type_from_receiver_type(lhs_type, expr.rhs.name) {
				return field_typ
			}
			base_type := lhs_type.base_type()
			if base_type is types.Struct {
				if field_typ := t.lookup_struct_field_type(base_type.name, expr.rhs.name) {
					return field_typ
				}
			}
			struct_type_name = t.type_to_name(base_type)
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
				if field_typ := t.lookup_struct_field_type(base_type.name, expr.rhs.name) {
					return field_typ
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
			if field_typ := t.lookup_struct_field_type(base_type.name, expr.rhs.name) {
				return field_typ
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
fn (mut t Transformer) expand_array_init_with_index(len_expr ast.Expr, cap_expr ast.Expr, sizeof_expr ast.Expr, init_expr ast.Expr, _pos token.Pos) ast.Expr {
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
