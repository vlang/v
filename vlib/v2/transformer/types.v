// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import strconv
import v2.ast
import v2.token
import v2.types
import strings

struct SumtypeConstructorPayload {
	variant_name string
	variant_type types.Type
	value        ast.Expr
	should_deref bool
}

// register_synth_type registers a type for a synthesized node position.
// Accumulates in synth_types map for deferred application (thread-safe).
fn (mut t Transformer) register_synth_type(pos token.Pos, typ types.Type) {
	t.synth_types[pos.id] = typ
}

fn (mut t Transformer) with_synth_type(expr ast.Expr, typ types.Type) ast.Expr {
	pos := t.next_synth_pos()
	t.register_synth_type(pos, typ)
	return match expr {
		ast.Ident {
			ast.Expr(ast.Ident{
				name: expr.name
				pos:  pos
			})
		}
		ast.SelectorExpr {
			ast.Expr(ast.SelectorExpr{
				lhs: expr.lhs
				rhs: expr.rhs
				pos: pos
			})
		}
		ast.IndexExpr {
			ast.Expr(ast.IndexExpr{
				lhs:      expr.lhs
				expr:     expr.expr
				is_gated: expr.is_gated
				pos:      pos
			})
		}
		ast.CallExpr {
			ast.Expr(ast.CallExpr{
				lhs:  expr.lhs
				args: expr.args
				pos:  pos
			})
		}
		ast.CallOrCastExpr {
			ast.Expr(ast.CallOrCastExpr{
				lhs:  expr.lhs
				expr: expr.expr
				pos:  pos
			})
		}
		else {
			ast.Expr(ast.ParenExpr{
				expr: expr
				pos:  pos
			})
		}
	}
}

fn (mut t Transformer) transform_expr_without_smartcast_as_type(expr ast.Expr, expr_str string, typ types.Type) ast.Expr {
	transformed := t.transform_expr_without_smartcast(expr, expr_str)
	return t.with_synth_type(transformed, typ)
}

// lookup_method_cached looks up a method by receiver type name and method name
// using cached_methods (lock-free) instead of env.lookup_method.
fn (t &Transformer) lookup_method_cached(type_name string, method_name string) ?types.FnType {
	methods := t.cached_methods[type_name] or { return none }
	for method in methods {
		if method.get_name() == method_name {
			typ := method.get_typ()
			if typ is types.FnType {
				return typ
			}
		}
	}
	return none
}

// lookup_fn_cached looks up a function by module and name
// using cached_scopes (lock-free) instead of env.lookup_fn.
fn (t &Transformer) lookup_fn_cached(module_name string, fn_name string) ?types.FnType {
	if scope := t.cached_scopes[module_name] {
		obj := scope.objects[fn_name] or { return none }
		if obj is types.Fn {
			typ := obj.get_typ()
			if typ is types.FnType {
				return typ
			}
		}
		return none
	}
	if module_name != '' {
		for key, scope in t.cached_scopes {
			short_key := if key.contains('.') {
				key.all_after_last('.')
			} else if key.contains('__') {
				key.all_after_last('__')
			} else {
				key
			}
			if short_key != module_name {
				continue
			}
			obj := scope.objects[fn_name] or { continue }
			if obj is types.Fn {
				typ := obj.get_typ()
				if typ is types.FnType {
					return typ
				}
			}
		}
	}
	return none
}

// register_generated_fn_scope creates a function scope for a transformer-generated function
// (e.g. Array_int_contains, Array_string_str) and registers parameter types so cleanc
// can resolve them via scope lookup instead of falling back to string-based inference.
fn (mut t Transformer) register_generated_fn_scope(fn_name string, module_name string, params []ast.Parameter) {
	parent := t.get_module_scope(module_name) or { return }
	mut fn_scope := types.new_scope(parent)
	for param in params {
		type_name := t.expr_to_type_name(param.typ)
		if type_name == '' {
			continue
		}
		if param_type := t.c_name_to_type(type_name) {
			fn_scope.insert(param.name, types.Object(param_type))
		}
	}
	key := if module_name == '' { fn_name } else { '${module_name}__${fn_name}' }
	t.cached_fn_scopes[key] = fn_scope
}

// c_name_to_type converts a C-style type name (e.g. "Array_int", "int", "string") to types.Type.
fn (t &Transformer) c_name_to_type(name string) ?types.Type {
	// Handle Array_* prefix
	if name.starts_with('Array_fixed_') {
		// Fixed arrays: Array_fixed_int_3 → types.ArrayFixed
		payload := name['Array_fixed_'.len..]
		if payload.contains('_') {
			elem_name := payload.all_before_last('_')
			len_str := payload.all_after_last('_')
			elem_type := t.c_name_to_type(elem_name) or { return none }
			return types.ArrayFixed{
				elem_type: elem_type
				len:       int(strconv.parse_int(len_str, 0, 64) or { 0 })
			}
		}
		return none
	}
	if name.starts_with('Array_') {
		elem_name := name['Array_'.len..]
		elem_type := t.c_name_to_type(elem_name) or { return none }
		return types.Array{
			elem_type: elem_type
		}
	}
	// Handle Map_KEY_VALUE prefix (e.g. Map_string_json2__Any)
	if name.starts_with('Map_string_') {
		value_name := name['Map_string_'.len..]
		value_type := t.c_name_to_type(value_name) or { return none }
		return types.Map{
			key_type:   types.string_
			value_type: value_type
		}
	}
	// Primitives and well-known types
	return match name {
		'int' {
			types.Type(types.int_)
		}
		'i8' {
			types.Type(types.Primitive{
				size:  8
				props: .integer
			})
		}
		'i16' {
			types.Type(types.Primitive{
				size:  16
				props: .integer
			})
		}
		'i32' {
			types.Type(types.Primitive{
				size:  32
				props: .integer
			})
		}
		'i64' {
			types.Type(types.Primitive{
				size:  64
				props: .integer
			})
		}
		'u8', 'byte' {
			types.Type(types.Primitive{
				size:  8
				props: .integer | .unsigned
			})
		}
		'u16' {
			types.Type(types.Primitive{
				size:  16
				props: .integer | .unsigned
			})
		}
		'u32' {
			types.Type(types.Primitive{
				size:  32
				props: .integer | .unsigned
			})
		}
		'u64' {
			types.Type(types.Primitive{
				size:  64
				props: .integer | .unsigned
			})
		}
		'f32' {
			types.Type(types.Primitive{
				size:  32
				props: .float
			})
		}
		'f64' {
			types.Type(types.Primitive{
				size:  64
				props: .float
			})
		}
		'bool' {
			types.Type(types.bool_)
		}
		'string' {
			types.Type(types.string_)
		}
		'int_literal' {
			types.Type(types.Primitive{
				props: .untyped | .integer
			})
		}
		'float_literal' {
			types.Type(types.Primitive{
				props: .untyped | .float
			})
		}
		else {
			// Try looking up by name in module scopes
			if typ := t.lookup_type(name) {
				typ
			} else {
				t.synthetic_struct_type_from_c_name(name)
			}
		}
	}
}

fn (t &Transformer) synthetic_struct_type_from_c_name(name string) ?types.Type {
	if name == '' || t.is_sum_type(name) || t.is_interface_type(name) {
		return none
	}
	short_name := if name.contains('__') { name.all_after_last('__') } else { name }
	if short_name.len == 0 || short_name[0] < `A` || short_name[0] > `Z` {
		return none
	}
	return types.Type(types.Struct{
		name: name
	})
}

// lookup_var_type looks up a variable's type in the current scope chain
fn (t &Transformer) lookup_var_type(name string) ?types.Type {
	if typ := t.decl_type_overrides[name] {
		return typ
	}
	if t.scope == unsafe { nil } {
		return none
	}
	mut scope := unsafe { t.scope }
	if obj := scope.lookup_parent(name, 0) {
		if obj is types.Module || obj is types.Fn {
			return none
		}
		if obj is types.Type && name_looks_like_type_symbol(name) {
			return none
		}
		return obj.typ()
	}
	return none
}

fn name_looks_like_type_symbol(name string) bool {
	if name == '' {
		return false
	}
	if name[0] >= `A` && name[0] <= `Z` {
		return true
	}
	return name in ['bool', 'string', 'rune', 'byte', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
		'u32', 'u64', 'f32', 'f64', 'usize', 'isize', 'void', 'voidptr', 'charptr', 'byteptr',
		'nil']
}

fn (t &Transformer) is_callable_type(typ types.Type) bool {
	match typ {
		types.FnType {
			return true
		}
		types.Alias {
			return t.is_callable_type(typ.base_type)
		}
		types.Pointer {
			return t.is_callable_type(typ.base_type)
		}
		else {
			return false
		}
	}
}

// is_fn_ident checks if an Ident refers to a function (for detecting function pointer args
// in .filter()/.map() calls, e.g., arr.filter(my_predicate))
fn (t &Transformer) is_fn_ident(ident ast.Ident) bool {
	if typ := t.get_expr_type(ident) {
		return t.is_callable_type(typ)
	}
	// Fallback: check if the name exists as a registered function
	return t.lookup_fn_cached('', ident.name) != none
		|| t.lookup_fn_cached('builtin', ident.name) != none
}

// is_interface_type checks if a type is an Interface
fn (t &Transformer) is_interface_type_check(typ types.Type) bool {
	return typ is types.Interface
}

// is_type_valid does a quick sanity check on an extracted Type sumtype.
// SSA sumtype layout: {i64 _tag, i64 _data} = 16 bytes total.
// For large variants (> 8 bytes), _data is a heap pointer to variant data.
// In v3 (ARM64-compiled), extracting nested sumtype (Type from Object) from
// map[string]Object can produce a Type with _data=0 (null heap pointer).
// Calling .name() on such a Type crashes when the match dispatch dereferences null.
fn is_type_valid(typ types.Type) bool {
	// Read _data field at offset 8 (second i64 in the sumtype struct).
	// For both C backend (union after tag) and ARM64 backend ({i64,i64}),
	// offset 8 contains either inline data or a heap pointer.
	data := unsafe { *(&u64(&u8(&typ) + 8)) }
	if data == 0 {
		// _data is null/zero. This is valid ONLY for tiny type-alias variants
		// stored inline (Char, Nil, None, Void, Rune, String, ISize, USize)
		// which have no fields or a single u8 field. For these, zero _data
		// means the variant value is 0/false/nil which is valid.
		// But for Alias (tag=0, 32B), Array (tag=1, 16B), etc., zero _data
		// means a null heap pointer → invalid.
		// Read the tag to distinguish:
		tag := unsafe { *(&u64(&typ)) }
		// Small inline-safe variants whose zero value is valid:
		// We conservatively allow tag values for Primitive and the type aliases.
		// Primitive is tag 14 (size=2B, fits inline). Type alias tags depend on
		// declaration order. Rather than enumerating, just check: if tag < 24
		// (valid variant range) and it's not one of the known large variants,
		// allow it. Known large variants that need non-null _data:
		// Alias=0, Array=1, ArrayFixed=2, Channel=3, Enum=5, FnType=6,
		// Interface=8, Map=9, OptionType=13, Pointer=14(?), ResultType=15(?),
		// Struct=18, SumType=19, Thread=20, Tuple=21
		// Actually, the tag numbering depends on the order in the Type declaration.
		// Type = Alias(0) | Array(1) | ArrayFixed(2) | Channel(3) | Char(4) |
		//        Enum(5) | FnType(6) | ISize(7) | Interface(8) | Map(9) |
		//        NamedType(10) | Nil(11) | None(12) | OptionType(13) | Pointer(14) |
		//        Primitive(15) | ResultType(16) | Rune(17) | String(18) | Struct(19) |
		//        SumType(20) | Thread(21) | Tuple(22) | USize(23) | Void(24)
		// Small (inline-safe with zero _data): Char(4), ISize(7), Nil(11), None(12),
		//   Primitive(15), Rune(17), String(18), USize(23), Void(24)
		// Large (need non-null _data): everything else
		if tag == 4 || tag == 7 || tag == 11 || tag == 12 || tag == 15 || tag == 17 || tag == 18
			|| tag == 23 || tag == 24 {
			return true // small inline variant, zero _data is OK
		}
		return false // large variant with null heap pointer
	}
	return true
}

// lookup_type looks up a type by name in the module scope
fn (t &Transformer) lookup_type(name string) ?types.Type {
	// Handle qualified names like "ast__Expr" by extracting module and type name
	normalized_name := name.replace('.', '__')
	mut lookup_name := normalized_name
	mut lookup_module := t.cur_module
	// Use index_of instead of split to avoid array allocation
	dunder := normalized_name.index('__') or { -1 }
	if dunder >= 0 {
		lookup_module = normalized_name[..dunder]
		// Get the last segment after '__' (handles multi-part like "a__b__C")
		last_dunder := normalized_name.last_index('__') or { dunder }
		lookup_name = normalized_name[last_dunder + 2..]
	}
	if scope := t.get_module_scope(lookup_module) {
		if obj := scope.lookup_parent(lookup_name, 0) {
			if obj is types.Type {
				if !is_type_valid(obj) {
					return none
				}
				return obj
			}
		}
	}
	if lookup_module != '' {
		for mod_name, scope in t.cached_scopes {
			mod_short := if mod_name.contains('.') {
				mod_name.all_after_last('.')
			} else if mod_name.contains('__') {
				mod_name.all_after_last('__')
			} else {
				mod_name
			}
			if mod_short != lookup_module {
				continue
			}
			if obj := scope.lookup_parent(lookup_name, 0) {
				if obj is types.Type {
					if !is_type_valid(obj) {
						continue
					}
					return obj
				}
			}
		}
	}
	if dunder < 0 {
		for _, fallback_scope in t.cached_scopes {
			if fallback_obj := fallback_scope.objects[lookup_name] {
				if fallback_obj is types.Type {
					if !is_type_valid(fallback_obj) {
						continue
					}
					return fallback_obj
				}
			}
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_type_any_module(name string) ?types.Struct {
	if typ := t.lookup_type(name) {
		if typ is types.Struct {
			return typ
		}
	}
	for _, scope in t.cached_scopes {
		if obj := scope.objects[name] {
			if obj is types.Type {
				if obj is types.Struct {
					return obj
				}
			}
		}
	}
	return none
}

fn (t &Transformer) struct_implements_name(st types.Struct, target string) bool {
	for impl_name in st.implements {
		if impl_name == target || impl_name.all_after_last('__') == target {
			return true
		}
	}
	if st.name != '' && st.implements.len == 0 {
		if live_type := t.lookup_struct_type_any_module(st.name) {
			if live_type.name != st.name {
				return t.struct_implements_name(live_type, target)
			}
			for impl_name in live_type.implements {
				if impl_name == target || impl_name.all_after_last('__') == target {
					return true
				}
			}
		}
	}
	return false
}

fn (mut t Transformer) register_needed_clone_struct(st types.Struct) string {
	fn_name := '${t.type_to_c_name(types.Type(st))}__clone'
	t.needed_clone_fns[fn_name] = st.name
	return fn_name
}

fn (t &Transformer) clone_fn_name_for_type(typ types.Type) ?string {
	mut base := typ
	for base is types.Pointer {
		base = (base as types.Pointer).base_type
	}
	base = types.resolve_alias(base)
	if base is types.Struct {
		st := base as types.Struct
		if !t.struct_implements_name(st, 'IClone') {
			return none
		}
		fn_name := '${t.type_to_c_name(types.Type(st))}__clone'
		short_name := if st.name.contains('__') { st.name.all_after_last('__') } else { st.name }
		if t.lookup_method_cached(st.name, 'clone') != none
			|| (short_name != st.name && t.lookup_method_cached(short_name, 'clone') != none) {
			return fn_name
		}
		return fn_name
	}
	return none
}

fn (mut t Transformer) auto_clone_fn_name_for_type(typ types.Type) ?string {
	fn_name := t.clone_fn_name_for_type(typ) or { return none }
	mut base := typ
	for base is types.Pointer {
		base = (base as types.Pointer).base_type
	}
	base = types.resolve_alias(base)
	if base is types.Struct {
		st := base as types.Struct
		short_name := if st.name.contains('__') { st.name.all_after_last('__') } else { st.name }
		if t.lookup_method_cached(st.name, 'clone') == none
			&& (short_name == st.name || t.lookup_method_cached(short_name, 'clone') == none) {
			t.needed_clone_fns[fn_name] = st.name
		}
	}
	return fn_name
}

// is_flag_enum checks if a type name is a flag enum
fn (t &Transformer) is_flag_enum(type_name string) bool {
	typ := t.lookup_type(type_name) or { return false }
	if typ is types.Enum {
		return typ.is_flag
	}
	return false
}

// type_expr_to_variant_name converts a type AST expression (like []Any or
// map[string]Any) to the mangled variant name used in sum type definitions
// (e.g. Array_json2__Any, Map_string_json2__Any).
fn (t &Transformer) type_expr_to_variant_name(e ast.Expr) string {
	if e is ast.Type {
		return t.type_to_variant_name(e)
	}
	if e is ast.Ident {
		name := (e as ast.Ident).name
		if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !name.contains('__') && !t.is_builtin_type_name(name) {
			return '${t.cur_module}__${name}'
		}
		return name
	}
	if e is ast.SelectorExpr {
		sel := e as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			return '${(sel.lhs as ast.Ident).name}__${sel.rhs.name}'
		}
	}
	return ''
}

fn (t &Transformer) is_builtin_type_name(name string) bool {
	return name in ['string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'bool', 'rune', 'usize', 'isize', 'voidptr', 'byteptr', 'charptr']
}

// variant_name_to_c converts V-style variant names to C-style for union member access.
// e.g. "[]json2__Any" → "Array_json2__Any", "map[string]json2__Any" → "Map_string_json2__Any"
fn (t &Transformer) variant_name_to_c(name string) string {
	if name.starts_with('[]') {
		return 'Array_${name[2..]}'
	}
	if name.starts_with('map[') {
		// "map[K]V" → "Map_K_V"
		bracket_end := name.index_u8(`]`)
		if bracket_end > 0 {
			key := name[4..bracket_end]
			val := name[bracket_end + 1..]
			return 'Map_${key}_${val}'
		}
	}
	return name
}

fn (t &Transformer) type_to_variant_name(e ast.Type) string {
	match e {
		ast.ArrayType {
			elem := t.type_expr_to_variant_name(e.elem_type)
			if elem != '' {
				return '[]${elem}'
			}
		}
		ast.MapType {
			key := t.type_expr_to_variant_name(e.key_type)
			val := t.type_expr_to_variant_name(e.value_type)
			if key != '' && val != '' {
				return 'map[${key}]${val}'
			}
		}
		else {}
	}

	return ''
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

// transform_sumtype_type_name lowers `receiver.type_name()` on a sum type into
// a match expression on `receiver._tag` that returns the variant display name.
fn (mut t Transformer) transform_sumtype_type_name(receiver ast.Expr) ?ast.Expr {
	mut sumtype_name := t.get_sumtype_name_for_expr(receiver)
	// Fallback: if name-based lookup fails, try direct type lookup
	if sumtype_name == '' {
		if receiver is ast.Ident {
			typ := t.lookup_var_type(receiver.name) or { return none }
			if typ is types.SumType {
				sumtype_name = typ.get_name()
			}
		}
	}
	if sumtype_name == '' {
		return none
	}
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}

	transformed_receiver := t.transform_expr(receiver)
	tag_access := t.synth_selector(transformed_receiver, '_tag', types.Type(types.int_))

	mut branches := []ast.MatchBranch{cap: variants.len + 1}
	for i, variant in variants {
		// Convert internal name (e.g. "ast__SelectorExpr") to display name ("ast.SelectorExpr")
		display_name := variant.replace('__', '.')
		branches << ast.MatchBranch{
			cond:  [
				ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '${i}'
				}),
			]
			stmts: [
				ast.Stmt(ast.ExprStmt{
					expr: ast.StringLiteral{
						value: display_name
						kind:  .v
					}
				}),
			]
		}
	}
	// else branch returns empty string
	branches << ast.MatchBranch{
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: ast.StringLiteral{
					value: ''
					kind:  .v
				}
			}),
		]
	}

	return t.lower_match_expr_to_if(tag_access, branches)
}

// is_sum_type checks if a type name is a sum type
fn (t &Transformer) is_sum_type(type_name string) bool {
	typ := t.lookup_type(type_name) or { return false }
	return typ is types.SumType
}

fn (t &Transformer) normalize_sumtype_name(type_name string) string {
	if type_name == '' {
		return ''
	}
	mut name := type_name.replace('.', '__')
	if t.is_sum_type(name) {
		return name
	}
	if !name.contains('__') {
		qualified := t.qualify_type_name(name)
		if t.is_sum_type(qualified) {
			return qualified
		}
	}
	return name
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
	// Fallback: search sum types in the current module scope for user-defined types.
	// Only search the current module to avoid cross-module matches that would
	// incorrectly trigger smartcasting (e.g., types.Type containing Alias).
	cur_mod := t.cur_module
	scope := t.get_module_scope(cur_mod) or { return '' }
	for obj_name, obj in scope.objects {
		if obj is types.Type {
			if obj is types.SumType {
				st_name := if cur_mod != '' && cur_mod != 'main' && cur_mod != 'builtin' {
					'${cur_mod}__${obj_name}'
				} else {
					obj_name
				}
				variants := t.get_sum_type_variants(st_name)
				if variants.len == 0 {
					// Try short name if qualified name didn't work
					inner_variants := t.get_sum_type_variants(obj_name)
					for v in inner_variants {
						v_short := if v.contains('__') {
							v.all_after_last('__')
						} else {
							v
						}
						if v == variant_name || v_short == short_variant || v_short == variant_name {
							return obj_name
						}
					}
					continue
				}
				for v in variants {
					v_short := if v.contains('__') {
						v.all_after_last('__')
					} else {
						v
					}
					if v == variant_name || v_short == short_variant || v_short == variant_name {
						return st_name
					}
				}
			}
		}
	}
	return ''
}

// get_var_type_name returns the type name of a variable from scope lookup
fn (t &Transformer) get_var_type_name(name string) string {
	if typ := t.decl_type_overrides[name] {
		return t.type_to_name(typ)
	}
	if t.scope == unsafe { nil } {
		return ''
	}
	mut scope := unsafe { t.scope }
	for ; scope != unsafe { nil }; scope = scope.parent {
		obj := scope.objects[name] or { continue }
		if obj is types.Module {
			continue
		}
		typ := obj.typ()
		if obj is types.Const && typ is types.Alias {
			// Imported const copies can still carry the zero-value Type until
			// their source module finishes pending-const resolution.
			return ''
		}
		return t.type_to_name(typ)
	}
	return ''
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
	if type_name.contains('.') {
		return type_name.replace('.', '__')
	}
	// Don't qualify if already qualified (contains __) or is a primitive
	if type_name.contains('__')
		|| type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'string', 'rune', 'char', 'voidptr', 'charptr', 'byteptr', 'void'] {
		return type_name
	}
	// Don't qualify Array_ or Map_ types
	if type_name.starts_with('Array_') || type_name.starts_with('Map_') {
		return type_name
	}
	// Search all module scopes to find which module defines this type.
	// Check main and builtin first to avoid ambiguity when the same type
	// name exists in multiple modules (e.g. Coord in main and term).
	for priority_mod in ['main', 'builtin', ''] {
		if scope := t.cached_scopes[priority_mod] {
			if obj := scope.objects[type_name] {
				if obj is types.Type {
					return type_name
				}
			}
		}
	}
	scope_keys := t.cached_scopes.keys()
	for mod_name in scope_keys {
		if mod_name in ['main', 'builtin', ''] {
			continue
		}
		scope := t.cached_scopes[mod_name] or { continue }
		if obj := scope.objects[type_name] {
			if obj is types.Type {
				return '${mod_name}__${type_name}'
			}
		}
	}
	return type_name
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
		types.ArrayFixed {
			elem_name := t.type_to_c_decl_name(typ.elem_type)
			return 'Array_fixed_${elem_name}_${typ.len}'
		}
		types.Map {
			key_name := t.type_to_c_decl_name(typ.key_type)
			value_name := t.type_to_c_decl_name(typ.value_type)
			return 'Map_${key_name}_${value_name}'
		}
		types.Tuple {
			tuple_types := typ.get_types()
			mut names := []string{cap: tuple_types.len}
			for elem_type in tuple_types {
				names << t.type_to_c_name(elem_type)
			}
			return 'Tuple_${names.join('_')}'
		}
		types.Struct {
			// Replace . with __ for module-qualified names
			return typ.name.replace('.', '__')
		}
		types.String {
			return 'string'
		}
		types.Primitive {
			if typ.props.has(types.Properties.boolean) {
				return 'bool'
			}
			if typ.props.has(types.Properties.unsigned) {
				match typ.size {
					8 { return 'u8' }
					16 { return 'u16' }
					32 { return 'u32' }
					64 { return 'u64' }
					else { return 'int' }
				}
			}
			match typ.size {
				8 { return 'i8' }
				16 { return 'i16' }
				32, 0 { return 'int' }
				64 { return 'i64' }
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

// expr_to_type_name extracts a type name from a type expression
fn (t &Transformer) expr_to_type_name(expr ast.Expr) string {
	if expr is ast.Ident {
		name := expr.name
		// Add module prefix for non-builtin types when inside a non-main/builtin module.
		if name !in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr', 'void', 'nil']
			&& !name.contains('__') && t.cur_module != '' && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			return '${t.cur_module}__${name}'
		}
		return name
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

fn (t &Transformer) unwrap_map_type(typ types.Type) ?types.Map {
	mut cur := typ
	for {
		if cur is types.Pointer {
			ptr := cur as types.Pointer
			cur = ptr.base_type
			continue
		}
		if cur is types.Alias {
			alias_typ := cur as types.Alias
			cur = alias_typ.base_type
			continue
		}
		break
	}
	if cur is types.Map {
		return cur as types.Map
	}
	return none
}

fn map_key_type_needs_expr_fallback(typ types.Type) bool {
	if typ.name().starts_with('_option_') || typ.name().starts_with('_result_') {
		return true
	}
	return match typ {
		types.OptionType, types.ResultType, types.Void, types.None {
			true
		}
		else {
			false
		}
	}
}

fn map_key_expr_fallback_is_usable(typ types.Type) bool {
	if typ.name().starts_with('_option_') || typ.name().starts_with('_result_') {
		return false
	}
	return match typ {
		types.OptionType, types.ResultType, types.Void, types.None {
			false
		}
		else {
			true
		}
	}
}

fn (mut t Transformer) map_key_expr_type(expr ast.Expr) ?types.Type {
	if typ := t.get_expr_type(expr) {
		return typ
	}
	if typ := t.resolve_expr_type(expr) {
		return typ
	}
	if typ := t.infer_prop_type(expr) {
		return typ
	}
	if expr is ast.InfixExpr && expr.op == .plus
		&& (t.is_string_expr(expr.lhs) || t.is_string_expr(expr.rhs)) {
		return types.Type(types.string_)
	}
	return none
}

fn (mut t Transformer) map_key_type_for_expr(expr ast.Expr, map_type types.Map) types.Type {
	key_type := map_type.key_type
	if !map_key_type_needs_expr_fallback(key_type) {
		return key_type
	}
	if typ := t.map_key_expr_type(expr) {
		if map_key_expr_fallback_is_usable(typ) {
			return typ
		}
	}
	return key_type
}

fn (mut t Transformer) map_index_expr_from_guard_rhs(expr ast.Expr) ?ast.IndexExpr {
	mut index_expr := ast.IndexExpr{}
	if expr is ast.IndexExpr {
		index_expr = expr
	} else if expr is ast.GenericArgOrIndexExpr {
		if lhs_type := t.get_expr_type(expr.lhs) {
			if t.is_callable_type(lhs_type) {
				return none
			}
		}
		index_expr = ast.IndexExpr{
			lhs:      expr.lhs
			expr:     expr.expr
			is_gated: false
			pos:      expr.pos
		}
	} else if expr is ast.GenericArgs {
		if expr.args.len != 1 {
			return none
		}
		if lhs_type := t.get_expr_type(expr.lhs) {
			if t.is_callable_type(lhs_type) {
				return none
			}
		}
		index_expr = ast.IndexExpr{
			lhs:      expr.lhs
			expr:     expr.args[0]
			is_gated: false
			pos:      expr.pos
		}
	} else {
		return none
	}
	if _ := t.get_map_type_for_expr(index_expr.lhs) {
		return index_expr
	}
	return none
}

fn (t &Transformer) declared_sumtype_name_for_expr(expr ast.Expr) string {
	mut typ := types.Type(types.void_)
	mut found := false
	if resolved_type := t.resolve_expr_type(expr) {
		base := t.unwrap_alias_and_pointer_type(resolved_type)
		if base is types.SumType {
			type_name := t.type_to_c_name(base)
			if type_name != '' {
				return type_name
			}
		}
	}
	match expr {
		ast.SelectorExpr {
			if field_type := t.get_struct_field_type(expr) {
				typ = field_type
				found = true
			}
		}
		ast.IndexExpr {
			mut lhs_type_opt := t.get_expr_type(expr.lhs)
			if lhs_type_opt == none && expr.lhs is ast.Ident {
				lhs_type_opt = t.lookup_var_type(expr.lhs.name)
			}
			if lhs_type := lhs_type_opt {
				base := t.unwrap_alias_and_pointer_type(lhs_type)
				match base {
					types.Array {
						typ = base.elem_type
						found = true
					}
					types.ArrayFixed {
						typ = base.elem_type
						found = true
					}
					types.Map {
						typ = base.value_type
						found = true
					}
					else {}
				}
			}
		}
		else {}
	}

	if !found {
		return ''
	}
	base := t.unwrap_alias_and_pointer_type(typ)
	type_name := t.type_to_c_name(base)
	if type_name != '' && base is types.SumType {
		return type_name
	}
	return ''
}

fn map_int_key_width_from_type_name(type_name string) int {
	// Strings are handled separately.
	if type_name.contains('*') || type_name.ends_with('ptr') {
		// Pointer-sized keys are 8 bytes on all supported v2 targets for now.
		// Note: v2 cleanc currently assumes 64-bit pointers during self-hosting.
		return 8
	}
	return match type_name {
		'i8', 'u8', 'byte', 'bool', 'char' { 1 }
		'i16', 'u16' { 2 }
		'i64', 'u64', 'f64', 'usize', 'isize' { 8 }
		else { 4 }
	}
}

fn map_runtime_key_fns_from_type_name(key_type_name string) (string, string, string, string) {
	if key_type_name == 'string' {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	width := map_int_key_width_from_type_name(key_type_name)
	return 'map_hash_int_${width}', 'map_eq_int_${width}', 'map_clone_int_${width}', 'map_free_nop'
}

fn (t &Transformer) zero_value_expr_for_type(typ types.Type) ast.Expr {
	// Keep this conservative and allocation-free. It is primarily used for
	// default map index reads `m[key]` when the key is missing.
	match typ {
		types.OptionType {
			return t.none_value_expr_for_option_type(typ)
		}
		types.String {
			return ast.Expr(ast.StringLiteral{
				kind:  .v
				value: "''"
			})
		}
		types.Alias {
			if typ.name == 'string' {
				return ast.Expr(ast.StringLiteral{
					kind:  .v
					value: "''"
				})
			}
		}
		else {}
	}

	match typ {
		types.Primitive {
			if typ.props.has(types.Properties.boolean) {
				return ast.Expr(ast.BasicLiteral{
					kind:  .key_false
					value: 'false'
				})
			}
			// For primitives larger than 32 bits (i64, u64, f64),
			// wrap in a cast so cleanc emits the correct type.
			if typ.size > 32 && typ.props.has(.integer) {
				prim_name := if typ.props.has(.unsigned) {
					'u${typ.size}'
				} else {
					'i${typ.size}'
				}
				return ast.Expr(ast.CastExpr{
					typ:  ast.Ident{
						name: prim_name
					}
					expr: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				})
			} else if typ.size > 32 && typ.props.has(.float) {
				return ast.Expr(ast.CastExpr{
					typ:  ast.Ident{
						name: 'f${typ.size}'
					}
					expr: ast.BasicLiteral{
						kind:  .number
						value: '0'
					}
				})
			}
			return ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '0'
			})
		}
		types.Pointer, types.Nil {
			return ast.Expr(ast.Ident{
				name: 'nil'
			})
		}
		types.Enum {
			return ast.Expr(ast.CastExpr{
				typ:  t.type_to_ast_type_expr(typ)
				expr: ast.BasicLiteral{
					kind:  .number
					value: '0'
				}
			})
		}
		else {}
	}

	// For aggregates, use a compound-literal zero init: `((T){0})`.
	type_name := t.type_to_c_name(typ)
	if type_name != '' {
		return ast.Expr(ast.InitExpr{
			typ: ast.Ident{
				name: type_name
			}
		})
	}
	return ast.Expr(ast.InitExpr{
		typ: t.type_to_ast_type_expr(typ)
	})
}

fn (t &Transformer) none_value_expr_for_option_type(typ types.OptionType) ast.Expr {
	return ast.Expr(ast.InitExpr{
		typ:    t.type_to_ast_type_expr(types.Type(typ))
		fields: [
			ast.FieldInit{
				name:  'state'
				value: ast.Expr(ast.BasicLiteral{
					kind:  .number
					value: '2'
				})
			},
		]
	})
}

fn (t &Transformer) unwrap_array_or_string_type(typ types.Type) ?types.Type {
	mut cur := typ
	for {
		if cur is types.Pointer {
			ptr := cur as types.Pointer
			cur = ptr.base_type
			continue
		}
		if cur is types.Alias {
			alias_typ := cur as types.Alias
			cur = alias_typ.base_type
			continue
		}
		break
	}
	match cur {
		types.Array, types.String {
			return cur
		}
		else {}
	}

	return none
}

fn (t &Transformer) type_expr_to_c_name(typ ast.Expr) string {
	match typ {
		ast.Ident {
			return typ.name.replace('.', '__')
		}
		ast.SelectorExpr {
			if typ.lhs is ast.Ident {
				return '${(typ.lhs as ast.Ident).name}__${typ.rhs.name}'
			}
			return typ.rhs.name
		}
		ast.PrefixExpr {
			if typ.op == .amp {
				base := t.type_expr_to_c_name(typ.expr)
				if base != '' {
					return '${base}*'
				}
			}
			return t.type_expr_to_c_name(typ.expr)
		}
		ast.ModifierExpr {
			return t.type_expr_to_c_name(typ.expr)
		}
		ast.Type {
			// Handle composite types like []ast.Attribute, [3]int, map[string]int
			return t.type_variant_name(typ)
		}
		else {}
	}

	return typ.name().replace('.', '__')
}

fn (t &Transformer) type_names_match(actual string, target string) bool {
	if actual == target {
		return true
	}
	actual_base := actual.trim_right('*')
	target_base := target.trim_right('*')
	actual_short := if actual_base.contains('__') {
		actual_base.all_after_last('__')
	} else {
		actual_base
	}
	target_short := if target_base.contains('__') {
		target_base.all_after_last('__')
	} else {
		target_base
	}
	return actual_short == target_short
}

fn (t &Transformer) expr_is_casted_to_type(expr ast.Expr, target string) bool {
	match expr {
		ast.ParenExpr {
			return t.expr_is_casted_to_type(expr.expr, target)
		}
		ast.ModifierExpr {
			return t.expr_is_casted_to_type(expr.expr, target)
		}
		ast.CastExpr {
			cast_type := t.type_expr_to_c_name(expr.typ)
			return t.type_names_match(cast_type, target)
		}
		ast.CallOrCastExpr {
			cast_type := t.type_expr_to_c_name(expr.lhs)
			if cast_type != '' {
				return t.type_names_match(cast_type, target)
			}
			return t.expr_is_casted_to_type(expr.expr, target)
		}
		ast.PrefixExpr {
			if expr.op == .mul && expr.expr is ast.CastExpr {
				cast_type := t.type_expr_to_c_name(expr.expr.typ)
				return t.type_names_match(cast_type, '${target}*')
					|| t.type_names_match(cast_type, '${target}ptr')
			}
			return t.expr_is_casted_to_type(expr.expr, target)
		}
		else {}
	}

	return false
}

fn (mut t Transformer) resolve_expr_with_expected_type(expr ast.Expr, expected types.Type) ast.Expr {
	base := t.unwrap_alias_and_pointer_type(expected)
	match expr {
		ast.ArrayInitExpr {
			if expr.typ is ast.EmptyExpr && (base is types.Array || base is types.ArrayFixed) {
				return ast.ArrayInitExpr{
					typ:   t.type_to_ast_type_expr(base)
					exprs: expr.exprs
					init:  expr.init
					cap:   expr.cap
					len:   expr.len
					pos:   expr.pos
				}
			}
		}
		ast.MapInitExpr {
			if expr.typ is ast.EmptyExpr && base is types.Map {
				return ast.MapInitExpr{
					typ:  t.type_to_ast_type_expr(base)
					keys: expr.keys
					vals: expr.vals
					pos:  expr.pos
				}
			}
		}
		ast.InitExpr {
			if expr.typ is ast.EmptyExpr {
				return ast.InitExpr{
					typ:    t.type_to_ast_type_expr(base)
					fields: expr.fields
					pos:    expr.pos
				}
			}
		}
		else {}
	}

	match expected {
		types.OptionType {
			match expr {
				ast.Keyword {
					if expr.tok == .key_none {
						return t.none_value_expr_for_option_type(expected)
					}
				}
				ast.Ident {
					if expr.name == 'none' {
						return t.none_value_expr_for_option_type(expected)
					}
				}
				ast.Type {
					if expr is ast.NoneType {
						return t.none_value_expr_for_option_type(expected)
					}
				}
				else {}
			}

			if expr_type := t.get_expr_type(expr) {
				if expr_type is types.OptionType {
					return expr
				}
			}
			return ast.Expr(ast.CastExpr{
				typ:  t.type_to_ast_type_expr(expected)
				expr: expr
				pos:  expr.pos()
			})
		}
		types.ResultType {
			if expr_type := t.get_expr_type(expr) {
				if expr_type is types.ResultType {
					return expr
				}
			}
			return ast.Expr(ast.CastExpr{
				typ:  t.type_to_ast_type_expr(expected)
				expr: expr
				pos:  expr.pos()
			})
		}
		else {}
	}

	if base is types.Enum {
		enum_name := t.type_to_c_name(base)
		return t.resolve_enum_shorthand(expr, enum_name)
	}
	return expr
}

fn (mut t Transformer) resolve_expr_with_inferred_enum_type(expr ast.Expr) ast.Expr {
	if typ := t.get_expr_type(expr) {
		base := t.unwrap_alias_and_pointer_type(typ)
		if base is types.Enum {
			enum_name := t.type_to_c_name(base)
			return t.resolve_enum_shorthand(expr, enum_name)
		}
	}
	return expr
}

fn is_generic_placeholder_type_name(name string) bool {
	return name in ['T', 'K', 'V'] || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`)
}

fn (t &Transformer) type_to_ast_type_expr(typ types.Type) ast.Expr {
	match typ {
		types.Map {
			return ast.Expr(ast.Type(ast.MapType{
				key_type:   t.type_to_ast_type_expr(typ.key_type)
				value_type: t.type_to_ast_type_expr(typ.value_type)
			}))
		}
		types.Array {
			return ast.Expr(ast.Type(ast.ArrayType{
				elem_type: t.type_to_ast_type_expr(typ.elem_type)
			}))
		}
		types.ArrayFixed {
			return ast.Expr(ast.Type(ast.ArrayFixedType{
				len:       ast.BasicLiteral{
					kind:  .number
					value: '${typ.len}'
				}
				elem_type: t.type_to_ast_type_expr(typ.elem_type)
			}))
		}
		types.Pointer {
			return ast.Expr(ast.PrefixExpr{
				op:   .amp
				expr: t.type_to_ast_type_expr(typ.base_type)
			})
		}
		types.OptionType {
			return ast.Expr(ast.Type(ast.OptionType{
				base_type: t.type_to_ast_type_expr(typ.base_type)
			}))
		}
		types.ResultType {
			return ast.Expr(ast.Type(ast.ResultType{
				base_type: t.type_to_ast_type_expr(typ.base_type)
			}))
		}
		types.Alias {
			return ast.Expr(ast.Ident{
				name: typ.name
			})
		}
		else {
			raw_name := typ.name()
			if is_generic_placeholder_type_name(raw_name) {
				return ast.Expr(ast.Ident{
					name: raw_name
				})
			}
			type_name := t.type_to_c_name(typ)
			return ast.Expr(ast.Ident{
				name: type_name
			})
		}
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
	if t.lookup_method_cached(type_name, 'msg') != none {
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
		if t.lookup_method_cached(type_name, 'msg') != none {
			return true
		}
		return false
	}
	if typ is types.Struct {
		for embedded in typ.embedded {
			if embedded.name == 'Error' || embedded.name.ends_with('.Error')
				|| embedded.name == 'MessageError' || embedded.name.ends_with('.MessageError')
				|| embedded.name.ends_with('__MessageError') {
				return true
			}
		}
	}
	// Also check if the type has msg() method (implements IError directly)
	if t.lookup_method_cached(type_name, 'msg') != none {
		return true
	}
	return false
}

fn (t &Transformer) is_ierror_type(typ types.Type) bool {
	match typ {
		types.Interface {
			return typ.name == 'IError' || typ.name.ends_with('__IError')
		}
		types.NamedType {
			name := string(typ)
			return name == 'IError' || name.ends_with('__IError')
		}
		else {
			return false
		}
	}
}

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
	// Unwrap ModifierExpr (e.g. `mut value` in `if mut value is Type`)
	unwrapped_expr := if expr is ast.ModifierExpr { expr.expr } else { expr }
	if direct_type := t.resolve_expr_type(unwrapped_expr) {
		direct_base := t.unwrap_alias_and_pointer_type(direct_type)
		if direct_base is types.SumType {
			direct_name := t.type_to_c_name(direct_base)
			if direct_name != '' && t.is_sum_type(direct_name) {
				return direct_name
			}
		}
	}
	mut type_name := if unwrapped_expr is ast.Ident {
		t.get_var_type_name(unwrapped_expr.name)
	} else if unwrapped_expr is ast.SelectorExpr {
		t.get_selector_type_name(unwrapped_expr)
	} else {
		''
	}

	// If scope lookup failed, try to get the type from the expression's position
	// This handles loop variables and other cases where the scope doesn't have the type
	if type_name == '' && expr_has_valid_data(unwrapped_expr) {
		if typ := t.env.get_expr_type(unwrapped_expr.pos().id) {
			type_name = typ.name()
		}
	}

	if type_name != '' && t.is_sum_type(type_name) {
		return type_name
	}
	// If unqualified lookup failed, try the env type for a direct SumType check.
	// Then search all module scopes for a matching SumType definition.
	// This handles cross-module types like types.Type being accessed from
	// another module where the short name isn't in scope.
	if type_name != '' && !type_name.contains('__') {
		// Use the variable's actual type object (via scope) to get variant count
		// for disambiguation when multiple modules define a SumType with the same name.
		mut var_variant_count := 0
		if unwrapped_expr is ast.Ident {
			if raw_type := t.lookup_var_type(unwrapped_expr.name) {
				if raw_type is types.SumType {
					var_variant_count = raw_type.variants.len
				}
			}
		}
		scope_keys := t.cached_scopes.keys()
		for mod_name in scope_keys {
			if mod_name in ['main', 'builtin', ''] {
				continue
			}
			scope := t.cached_scopes[mod_name] or { continue }
			if obj := scope.objects[type_name] {
				if obj is types.Type {
					typ := types.Type(obj)
					if typ is types.SumType {
						// Disambiguate: if we know the variable's variant count,
						// match against the definition's variant count.
						if var_variant_count > 0 && typ.variants.len != var_variant_count {
							continue
						}
						return '${mod_name}__${type_name}'
					}
				}
			}
		}
	}
	// Check if it's a pointer to a sum type (e.g., &Object for a method receiver)
	if type_name != '' {
		mut stripped := type_name
		if stripped.starts_with('&') {
			stripped = stripped[1..]
		}
		if stripped.ends_with('*') {
			stripped = stripped[..stripped.len - 1]
		}
		if stripped != type_name && t.is_sum_type(stripped) {
			return stripped
		}
	}
	return ''
}

// type_variant_name extracts a variant name from a Type expression for sumtype matching
// Returns C-compatible names: []ast.Attribute -> 'Array_Attribute' (short name for union member matching)
fn (t &Transformer) type_variant_name(typ ast.Type) string {
	if typ is ast.ArrayType {
		// []Type -> 'Array_' + full element type name (matches generated union member)
		elem_name := t.type_expr_name_full(typ.elem_type)
		return 'Array_${elem_name}'
	}
	if typ is ast.ArrayFixedType {
		// [N]Type -> 'Array_fixed_' + short element type name + '_' + N
		elem_name := t.type_expr_name_full(typ.elem_type)
		mut len_str := '0'
		if typ.len is ast.BasicLiteral {
			len_str = typ.len.value
		}
		return 'Array_fixed_${elem_name}_${len_str}'
	}
	if typ is ast.MapType {
		// map[K]V -> 'Map_K_V'
		key_name := t.type_expr_name_full(typ.key_type)
		val_name := t.type_expr_name_full(typ.value_type)
		return 'Map_${key_name}_${val_name}'
	}
	if typ is ast.GenericType {
		// Foo[Bar] -> Foo (used for sumtype matching)
		return t.type_expr_name(typ.name)
	}
	// Other `ast.Type` variants are matched by their variant struct names.
	return match typ {
		ast.AnonStructType { 'AnonStructType' }
		ast.ChannelType { 'ChannelType' }
		ast.FnType { 'FnType' }
		ast.NilType { 'NilType' }
		ast.NoneType { 'NoneType' }
		ast.OptionType { 'OptionType' }
		ast.ResultType { 'ResultType' }
		ast.ThreadType { 'ThreadType' }
		ast.TupleType { 'TupleType' }
		else { 'Type' }
	}
}

// type_variant_name_full extracts full variant name for type casts (with module prefix)
// []ast.Attribute -> 'Array_ast__Attribute' (for typedef name)
fn (t &Transformer) type_variant_name_full(typ ast.Type) string {
	if typ is ast.ArrayType {
		elem_name := t.type_expr_name_full(typ.elem_type)
		return 'Array_${elem_name}'
	}
	if typ is ast.ArrayFixedType {
		elem_name := t.type_expr_name_full(typ.elem_type)
		mut len_str := '0'
		if typ.len is ast.BasicLiteral {
			len_str = typ.len.value
		}
		return 'Array_fixed_${elem_name}_${len_str}'
	}
	if typ is ast.MapType {
		key_name := t.type_expr_name_full(typ.key_type)
		val_name := t.type_expr_name_full(typ.value_type)
		return 'Map_${key_name}_${val_name}'
	}
	return t.type_expr_name_full(typ)
}

// type_expr_name extracts the short type name from a type expression
fn (t &Transformer) type_expr_name(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.SelectorExpr {
		// ast.Attribute -> 'Attribute' (use short name for matching)
		return expr.rhs.name
	}
	if expr is ast.Type {
		return t.type_variant_name(expr)
	}
	return ''
}

// type_expr_name_full extracts the full type name with module prefix (for C mangling)
fn (t &Transformer) type_expr_name_full(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.SelectorExpr {
		// ast.Attribute -> 'ast__Attribute' (full name with module prefix for C)
		if expr.lhs is ast.Ident {
			mod := (expr.lhs as ast.Ident).name
			return '${mod}__${expr.rhs.name}'
		}
		return expr.rhs.name
	}
	if expr is ast.Type {
		return t.type_variant_name(expr)
	}
	return ''
}

fn (t &Transformer) sumtype_expr_needs_variant_inference(value ast.Expr) bool {
	return value is ast.InitExpr || value is ast.ArrayInitExpr || value is ast.MapInitExpr
		|| value is ast.BasicLiteral || value is ast.StringLiteral
		|| value is ast.StringInterLiteral || value is ast.CastExpr
}

fn (mut t Transformer) sumtype_variant_from_expr_type(value ast.Expr, variants []string) (string, bool) {
	mut typ := types.Type(types.void_)
	mut found := false
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			typ = var_type
			found = true
		}
	} else if value is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(value) {
			typ = field_type
			found = true
		}
	}
	if !found {
		return '', false
	}
	mut is_pointer := false
	if typ is types.Pointer {
		is_pointer = true
		typ = typ.base_type
	}
	base := t.unwrap_alias_and_pointer_type(typ)
	base_c_name := t.type_to_c_name(base)
	if matched := t.match_variant(base_c_name, variants) {
		return matched, is_pointer
	}
	constructor_name := t.type_constructor_name(base)
	if constructor_name != '' {
		if matched := t.match_variant(constructor_name, variants) {
			return matched, is_pointer
		}
	}
	return '', false
}

// get_struct_field_type_name returns the type name of a field in a struct
fn (mut t Transformer) wrap_sumtype_value(value ast.Expr, sumtype_name string) ?ast.Expr {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}
	value_key := t.expr_to_string(value)
	if value_key != '' {
		if ctx := t.find_smartcast_for_expr(value_key) {
			if t.is_same_sumtype_name(ctx.sumtype, sumtype_name) {
				if matched := t.match_variant(ctx.variant_full, variants) {
					return t.build_sumtype_init(t.apply_smartcast_direct_ctx(value, ctx), matched,
						sumtype_name)
				}
				if matched := t.match_variant(ctx.variant, variants) {
					return t.build_sumtype_init(t.apply_smartcast_direct_ctx(value, ctx), matched,
						sumtype_name)
				}
			}
			if matched := t.match_variant(ctx.variant_full, variants) {
				return t.build_sumtype_init(t.apply_smartcast_direct_ctx(value, ctx), matched,
					sumtype_name)
			}
			if matched := t.match_variant(ctx.variant, variants) {
				return t.build_sumtype_init(t.apply_smartcast_direct_ctx(value, ctx), matched,
					sumtype_name)
			}
		}
	}
	declared_sumtype := t.declared_sumtype_name_for_expr(value)
	if declared_sumtype != '' && t.is_same_sumtype_name(declared_sumtype, sumtype_name) {
		return none
	}
	if (sumtype_name == 'Expr' || sumtype_name.ends_with('__Expr')) && value is ast.SelectorExpr
		&& value.rhs.name == 'expr' {
		return none
	}
	// Determine the variant type from the checker's type info
	mut typ := types.Type(types.void_)
	mut has_expr_type := false
	if expr_type := t.get_expr_type(value) {
		typ = expr_type
		has_expr_type = true
	}
	c_name := if has_expr_type { t.type_to_c_name(typ) } else { '' }
	input_is_target_sumtype := c_name != '' && t.is_same_sumtype_name(c_name, sumtype_name)
	// If the value's type IS the target sum type, no wrapping needed —
	// the value is already the correct type (not a variant that needs wrapping).
	if input_is_target_sumtype && !t.sumtype_expr_needs_variant_inference(value) {
		return none
	}
	mut variant_name := ''
	mut should_deref_pointer_value := false
	// For Ident expressions, also check the variable's declared type.
	// In multi-variant match arms, the checker may narrow to the first variant
	// (e.g., Primitive) even though the runtime value could be any variant.
	// The declared variable type is more reliable in this case.
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			mut checked_type := var_type
			if var_type is types.Pointer {
				checked_type = var_type.base_type
				should_deref_pointer_value = true
			} else if value.name in t.cur_fn_mut_params {
				should_deref_pointer_value = true
			}
			var_c_name := t.type_to_c_name(checked_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
			if matched := t.match_variant(var_c_name, variants) {
				variant_name = matched
			}
		}
	}
	if has_expr_type && variant_name == '' && c_name != '' && c_name != 'void'
		&& !input_is_target_sumtype {
		variant_name = t.match_variant(c_name, variants) or { '' }
	}
	if variant_name == '' {
		inferred_variant, is_pointer := t.sumtype_variant_from_expr_type(value, variants)
		if inferred_variant != '' {
			variant_name = inferred_variant
			should_deref_pointer_value = is_pointer
		}
	}
	// Fallback: use the type's V constructor name (e.g., types.Void → 'Void')
	// This handles cases where type_to_c_name returns a C name (like 'void', 'string')
	// that doesn't match the variant constructor name (like 'Void', 'String').
	if has_expr_type && variant_name == '' && !input_is_target_sumtype {
		constructor_name := t.type_constructor_name(typ)
		if constructor_name != '' {
			variant_name = t.match_variant(constructor_name, variants) or { '' }
		}
	}
	if variant_name == '' {
		return none
	}
	// Transform the value then wrap
	mut transformed_value := t.transform_expr(value)
	if should_deref_pointer_value {
		transformed_value = ast.PrefixExpr{
			op:   .mul
			expr: transformed_value
		}
	}
	return t.build_sumtype_init(transformed_value, variant_name, sumtype_name)
}

fn (mut t Transformer) wrap_transformed_variant_for_sumtype(value ast.Expr, sumtype_name string) ?ast.Expr {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}
	candidate := if value is ast.ModifierExpr { value.expr } else { value }
	mut variant_name := ''
	mut should_deref_pointer_value := false
	if candidate is ast.Ident {
		if var_type := t.lookup_var_type(candidate.name) {
			mut checked_type := var_type
			if var_type is types.Pointer {
				checked_type = var_type.base_type
				should_deref_pointer_value = true
			}
			var_c_name := t.type_to_c_name(checked_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
			variant_name = t.match_variant(var_c_name, variants) or { '' }
		}
	}
	if variant_name == '' {
		if inferred_type := t.infer_decl_type_from_transformed_expr(candidate) {
			inferred_c_name := t.type_to_c_name(inferred_type)
			if !t.is_same_sumtype_name(inferred_c_name, sumtype_name) {
				variant_name = t.match_variant(inferred_c_name, variants) or { '' }
			}
		}
	}
	if variant_name == '' {
		return none
	}
	mut wrapped_value := candidate
	if should_deref_pointer_value {
		wrapped_value = ast.PrefixExpr{
			op:   .mul
			expr: wrapped_value
		}
	}
	return t.build_sumtype_init(wrapped_value, variant_name, sumtype_name)
}

// wrap_sumtype_value_transformed wraps an already-transformed expression in sum type init
fn (mut t Transformer) wrap_sumtype_value_transformed(value ast.Expr, sumtype_name string) ?ast.Expr {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}
	declared_sumtype := t.declared_sumtype_name_for_expr(value)
	if declared_sumtype != '' && t.is_same_sumtype_name(declared_sumtype, sumtype_name) {
		return none
	}
	if (sumtype_name == 'Expr' || sumtype_name.ends_with('__Expr')) && value is ast.SelectorExpr
		&& value.rhs.name == 'expr' {
		return none
	}
	// For Ident expressions, check if the variable's declared type IS the target sum type.
	mut variant_name := ''
	mut should_deref_pointer_value := false
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			mut checked_type := var_type
			if var_type is types.Pointer {
				checked_type = var_type.base_type
				should_deref_pointer_value = true
			} else if value.name in t.cur_fn_mut_params {
				should_deref_pointer_value = true
			}
			var_c_name := t.type_to_c_name(checked_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
			if matched := t.match_variant(var_c_name, variants) {
				variant_name = matched
			}
		}
	}
	// Try checker-provided type first (works for original expressions with valid pos.id)
	mut input_is_target_sumtype2 := false
	if typ := t.get_expr_type(value) {
		c_name := t.type_to_c_name(typ)
		input_is_target_sumtype2 = t.is_same_sumtype_name(c_name, sumtype_name)
		// If the value's type IS the target sum type, no wrapping needed.
		if input_is_target_sumtype2 && !t.sumtype_expr_needs_variant_inference(value) {
			return none
		}
		if variant_name == '' && c_name != '' && c_name != 'void' && !input_is_target_sumtype2 {
			variant_name = t.match_variant(c_name, variants) or { '' }
		}
		// Fallback: use type constructor name (e.g., Void, Struct, Primitive)
		if variant_name == '' && !input_is_target_sumtype2 {
			constructor_name := t.type_constructor_name(typ)
			if constructor_name != '' {
				variant_name = t.match_variant(constructor_name, variants) or { '' }
			}
		}
	}
	// Fallback: infer variant from expression structure (needed for already-transformed
	// expressions that have lost their position IDs after transformation)
	if variant_name == '' {
		if inferred_type := t.infer_decl_type_from_transformed_expr(value) {
			inferred_c_name := t.type_to_c_name(inferred_type)
			if inferred_c_name != '' {
				variant_name = t.match_variant(inferred_c_name, variants) or { '' }
			}
		}
		if value is ast.InitExpr {
			type_name := t.get_init_expr_type_name(value.typ)
			matched := t.match_variant(type_name, variants) or { '' }
			if matched != '' {
				variant_name = matched
			} else if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
				// Try with module prefix
				mangled := '${t.cur_module}__${type_name}'
				variant_name = t.match_variant(mangled, variants) or { '' }
			}
		}
		if variant_name == '' && value is ast.BasicLiteral {
			if value.kind == .number {
				variant_name = if value.value.contains('.') {
					match_sumtype_variant_name('f64', variants)
				} else {
					match_sumtype_variant_name('int', variants)
				}
			} else if value.kind == .string {
				variant_name = match_sumtype_variant_name('string', variants)
			}
		}
		if variant_name == '' && (value is ast.StringLiteral || value is ast.StringInterLiteral) {
			variant_name = match_sumtype_variant_name('string', variants)
		}
		if variant_name == '' && value is ast.Ident {
			var_type_name := t.get_var_type_name(value.name)
			if var_type_name != '' {
				variant_name = t.match_variant(var_type_name, variants) or { '' }
			}
		}
		if variant_name == '' && value is ast.CastExpr {
			variant_name = t.match_variant(t.type_expr_name_full(value.typ), variants) or { '' }
		}
		if variant_name == '' && value is ast.CallExpr {
			variant_name = match_sumtype_variant_name(t.get_call_return_type(ast.Expr(value)),
				variants)
		}
	}
	if variant_name == '' {
		return none
	}
	// Value is already transformed, just wrap it
	mut wrapped_value := value
	if should_deref_pointer_value {
		wrapped_value = ast.PrefixExpr{
			op:   .mul
			expr: wrapped_value
		}
	}
	return t.build_sumtype_init(wrapped_value, variant_name, sumtype_name)
}

fn is_sumtype_inline_primitive_variant(variant_name string) bool {
	return variant_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
}

fn (mut t Transformer) type_for_sumtype_constructor_value(value ast.Expr) ?types.Type {
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			return var_type
		}
	}
	if value is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(value) {
			return field_type
		}
	}
	if expr_type := t.get_expr_type(value) {
		return expr_type
	}
	return none
}

fn (mut t Transformer) sumtype_constructor_payload(value ast.Expr, sumtype_name string) ?SumtypeConstructorPayload {
	variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 {
		return none
	}
	mut payload_value := unwrap_paren_expr(value)
	mut should_deref := false
	if payload_value is ast.PrefixExpr && payload_value.op == .amp {
		payload_value = unwrap_paren_expr(payload_value.expr)
	}
	mut payload_type := t.type_for_sumtype_constructor_value(payload_value) or {
		if fallback_type := t.get_expr_type(value) {
			fallback_type
		} else {
			return none
		}
	}
	if payload_type is types.Pointer {
		payload_type = payload_type.base_type
		should_deref = true
	}
	if payload_value is ast.Ident && payload_value.name in t.cur_fn_mut_params {
		should_deref = true
	}
	base_type := t.unwrap_alias_and_pointer_type(payload_type)
	base_c_name := t.type_to_c_name(base_type)
	mut variant_name := t.match_variant(base_c_name, variants) or { '' }
	if variant_name == '' {
		constructor_name := t.type_constructor_name(base_type)
		if constructor_name != '' {
			variant_name = t.match_variant(constructor_name, variants) or { '' }
		}
	}
	if variant_name == '' {
		return none
	}
	variant_type := if resolved := t.lookup_type(variant_name) {
		resolved
	} else {
		base_type
	}
	return SumtypeConstructorPayload{
		variant_name: variant_name
		variant_type: variant_type
		value:        payload_value
		should_deref: should_deref
	}
}

fn (mut t Transformer) malloc_cast_expr(typ_expr ast.Expr, ptr_type types.Type, pos token.Pos) ast.Expr {
	cast_pos := t.next_synth_pos()
	t.register_synth_type(cast_pos, ptr_type)
	return ast.Expr(ast.CastExpr{
		typ:  ast.Expr(ast.PrefixExpr{
			op:   .amp
			expr: typ_expr
			pos:  cast_pos
		})
		expr: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'C'
					pos:  pos
				})
				rhs: ast.Ident{
					name: 'malloc'
					pos:  pos
				}
				pos: pos
			})
			args: [
				ast.Expr(ast.KeywordOperator{
					op:    .key_sizeof
					exprs: [typ_expr]
					pos:   pos
				}),
			]
			pos:  pos
		})
		pos:  cast_pos
	})
}

fn (mut t Transformer) lower_addr_of_sumtype_constructor(expr ast.PrefixExpr) ?ast.Expr {
	if expr.op != .amp || expr.expr !is ast.CallOrCastExpr {
		return none
	}
	cast_expr := expr.expr as ast.CallOrCastExpr
	if !t.call_or_cast_lhs_is_type(cast_expr.lhs) {
		return none
	}
	sumtype_name := t.type_expr_name_full(cast_expr.lhs)
	if sumtype_name == '' || !t.is_sum_type(sumtype_name) {
		return none
	}
	sumtype_type := t.lookup_type(sumtype_name) or { return none }
	payload := t.sumtype_constructor_payload(cast_expr.expr, sumtype_name) or { return none }

	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut payload_value := t.transform_expr(payload.value)
	payload_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if payload.should_deref {
		payload_value = ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: payload_value
			pos:  expr.pos
		})
	}

	mut stmts := []ast.Stmt{cap: payload_pending.len + 5}
	for stmt in payload_pending {
		stmts << stmt
	}
	mut sum_payload_value := payload_value
	if !is_sumtype_inline_primitive_variant(payload.variant_name) {
		payload_ptr_name := t.gen_temp_name()
		payload_ptr_ident := ast.Ident{
			name: payload_ptr_name
			pos:  expr.pos
		}
		payload_ptr_type := types.Type(types.Pointer{
			base_type: payload.variant_type
		})
		t.register_temp_var(payload_ptr_name, payload_ptr_type)
		payload_type_expr := t.type_to_ast_type_expr(payload.variant_type)
		stmts << ast.Stmt(ast.AssignStmt{
			op:  .decl_assign
			lhs: [ast.Expr(payload_ptr_ident)]
			rhs: [t.malloc_cast_expr(payload_type_expr, payload_ptr_type, expr.pos)]
			pos: expr.pos
		})
		stmts << ast.Stmt(ast.AssignStmt{
			op:  .assign
			lhs: [
				ast.Expr(ast.PrefixExpr{
					op:   .mul
					expr: ast.Expr(payload_ptr_ident)
					pos:  expr.pos
				}),
			]
			rhs: [payload_value]
			pos: expr.pos
		})
		sum_payload_value = ast.Expr(ast.PrefixExpr{
			op:   .mul
			expr: ast.Expr(payload_ptr_ident)
			pos:  expr.pos
		})
	}

	sum_init := t.build_sumtype_init(sum_payload_value, payload.variant_name, sumtype_name) or {
		return none
	}
	sum_ptr_name := t.gen_temp_name()
	sum_ptr_ident := ast.Ident{
		name: sum_ptr_name
		pos:  expr.pos
	}
	sum_ptr_type := types.Type(types.Pointer{
		base_type: sumtype_type
	})
	t.register_temp_var(sum_ptr_name, sum_ptr_type)
	sum_type_expr := t.type_to_ast_type_expr(sumtype_type)
	stmts << ast.Stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(sum_ptr_ident)]
		rhs: [t.malloc_cast_expr(sum_type_expr, sum_ptr_type, expr.pos)]
		pos: expr.pos
	})
	stmts << ast.Stmt(ast.AssignStmt{
		op:  .assign
		lhs: [
			ast.Expr(ast.PrefixExpr{
				op:   .mul
				expr: ast.Expr(sum_ptr_ident)
				pos:  expr.pos
			}),
		]
		rhs: [sum_init]
		pos: expr.pos
	})
	stmts << ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(sum_ptr_ident)
	})
	return ast.Expr(ast.UnsafeExpr{
		stmts: stmts
		pos:   expr.pos
	})
}

// build_sumtype_init creates a sum type initialization expression
fn (t &Transformer) build_sumtype_init(transformed_value ast.Expr, variant_name string, sumtype_name string) ?ast.Expr {
	mut qualified_sumtype_name := sumtype_name
	if !qualified_sumtype_name.contains('__') {
		qualified_sumtype_name = t.qualify_type_name(sumtype_name)
	}
	mut variants := t.get_sum_type_variants(sumtype_name)
	if variants.len == 0 && qualified_sumtype_name != sumtype_name {
		variants = t.get_sum_type_variants(qualified_sumtype_name)
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

	// Eval executes the transformed AST directly, so it needs the real payload value.
	// Native/C backends still need the C-style pointer boxing layout.
	boxed_value := if t.is_eval_backend() {
		transformed_value
	} else {
		// Create: SumType{_tag: N, _data._variant: (void*)...}
		// For primitives: (void*)(intptr_t)value - stores value in pointer space
		// For structs/strings: (void*)&value - stores pointer to value
		// Only direct scalar variants are boxed inline in pointer space.
		// Metadata carriers like `types.Type.Primitive` are regular structs whose payload
		// is read through `_data` as a pointer, so they must not be treated as scalars.
		mut is_primitive_variant := is_sumtype_inline_primitive_variant(variant_name)
		if is_primitive_variant {
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
	}

	// Create the sum type initialization with _data._variant field name
	// This generates: (SumType){._tag = N, ._data._variant = (void*)...}
	// Convert variant name to C field name matching the union declaration:
	// - "ast__InfixExpr" → "InfixExpr" (strip module prefix)
	// - "[]ast__Attribute" → "Array_ast__Attribute" (array variant)
	// - "map[K]V" → "Map_K_V" (map variant)
	variant_field := if variant_name.starts_with('[]') {
		'Array_${variant_name[2..]}'
	} else if variant_name.starts_with('map[') {
		// map[K]V → Map_K_V
		inner := variant_name[4..] // after 'map['
		if bracket_idx := inner.index(']') {
			key := inner[..bracket_idx]
			val := inner[bracket_idx + 1..]
			'Map_${key}_${val}'
		} else {
			variant_name
		}
	} else if variant_name.contains('__') {
		sumtype_variant_union_field_name(qualified_sumtype_name, variant_name)
	} else {
		variant_name
	}
	return ast.InitExpr{
		typ:    ast.Ident{
			name: qualified_sumtype_name
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
				name:  '_data._${variant_field}'
				value: boxed_value
			},
		]
	}
}

fn match_sumtype_variant_name(candidate string, variants []string) string {
	if candidate.len == 0 {
		return ''
	}
	if candidate in variants {
		return candidate
	}
	cand_short := if candidate.contains('__') { candidate.all_after_last('__') } else { candidate }
	for v in variants {
		v_short := if v.contains('__') { v.all_after_last('__') } else { v }
		if cand_short == v_short || cand_short == v {
			return v
		}
	}
	return ''
}

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
	if typ is types.Tuple {
		return t.type_to_c_name(typ)
	}
	if typ is types.String {
		return 'string'
	}
	if typ is types.Char {
		return 'char'
	}
	if typ is types.Rune {
		return 'rune'
	}
	if typ is types.ISize {
		return 'isize'
	}
	if typ is types.USize {
		return 'usize'
	}
	if typ is types.Void {
		return 'void'
	}
	if typ is types.Nil {
		return 'nil'
	}
	if typ is types.None {
		return 'none'
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
		return types.sum_type_name(typ)
	}
	inner := typ.base_type()
	if typ is types.OptionType {
		return '_option_' + t.type_to_name(inner)
	}
	if typ is types.ResultType {
		return '_result_' + t.type_to_name(inner)
	}
	if typ is types.Pointer {
		return t.type_to_name(inner) + '*'
	}
	if typ is types.FnType {
		return 'FnType'
	}
	return ''
}

// infer_expr_type tries to infer the type name of an expression
fn (t &Transformer) infer_expr_type(expr ast.Expr) string {
	if recv_type := t.get_expr_type(expr) {
		return t.type_to_name(recv_type)
	}
	return ''
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
	return t.cached_scopes[t.cur_module] or { return none }
}

// get_module_scope returns the scope for a specific module
fn (t &Transformer) get_module_scope(module_name string) ?&types.Scope {
	return t.cached_scopes[module_name] or { return none }
}

// resolve_module_name resolves a module alias to its real module name via scope lookup.
// Returns the full module name (e.g., 'rand' for alias 'rand', 'seed' for sub-module 'seed').
fn (t &Transformer) resolve_module_name(name string) ?string {
	if t.scope != unsafe { nil } {
		mut scope := unsafe { t.scope }
		if obj := scope.lookup_parent(name, 0) {
			if obj is types.Module {
				mod := unsafe { &types.Module(&obj) }
				if mod.name != '' {
					return mod.name
				}
				return name
			}
		}
	}
	// Fallback: check current module scope
	if t.cur_module != '' {
		if mut mod_scope := t.get_module_scope(t.cur_module) {
			if obj := mod_scope.lookup_parent(name, 0) {
				if obj is types.Module {
					mod := unsafe { &types.Module(&obj) }
					if mod.name != '' {
						return mod.name
					}
					return name
				}
			}
		}
	}
	return none
}

fn (t &Transformer) selector_module_name(name string) ?string {
	if t.scope != unsafe { nil } {
		mut scope := unsafe { t.scope }
		if obj := scope.lookup_parent(name, 0) {
			if obj is types.Module {
				mod := unsafe { &types.Module(&obj) }
				if mod.name != '' {
					return mod.name
				}
				return name
			}
			return none
		}
	}
	if _ := t.cached_scopes[name] {
		return name
	}
	if mod_name := t.cur_import_aliases[name] {
		return mod_name
	}
	return t.resolve_module_name(name)
}

// is_module_ident checks if an identifier refers to a module
fn (t &Transformer) is_module_ident(name string) bool {
	return t.resolve_module_name(name) != none
}

// type_is_string checks if a type is string, including &string (Pointer to String)
fn (t &Transformer) type_is_string(typ types.Type) bool {
	if typ is types.String {
		return true
	}
	if typ is types.Struct && (typ as types.Struct).name == 'string' {
		return true
	}
	if typ is types.Pointer {
		return t.type_is_string(typ.base_type)
	}
	return false
}

// is_ptr_to_string_expr returns true if the expression has type &string (pointer to string)
fn (t &Transformer) is_ptr_to_string_expr(expr ast.Expr) bool {
	if expr_type := t.get_expr_type(expr) {
		if expr_type is types.Pointer {
			return t.type_is_string(expr_type.base_type)
		}
	}
	// Also check scope for identifiers
	if expr is ast.Ident {
		if mut scope := t.get_current_scope() {
			if obj := scope.lookup_parent(expr.name, 0) {
				typ := obj.typ()
				if typ is types.Pointer {
					return t.type_is_string(typ.base_type)
				}
			}
		}
	}
	return false
}

// expr_has_valid_data checks if an Expr sum type has a valid data pointer.
// On ARM64 backend, sum types are represented as (tag: u64, data_ptr: u64).
// Default-initialized Expr{} has data_ptr=0 (NULL), which crashes on field access.
fn stmt_has_valid_data(stmt ast.Stmt) bool {
	// On ARM64 backend: word1 = data pointer; NULL means corrupt/default-initialized.
	// On C backend: word1 = first 8 bytes of inline variant data; usually non-zero for valid statements.
	word1 := unsafe { (&u64(&stmt))[1] }
	return word1 != 0
}

fn expr_has_valid_data(expr ast.Expr) bool {
	// On ARM64 backend: word1 = data pointer; NULL means corrupt/default-initialized.
	// On C backend: word1 = first 8 bytes of inline variant data; usually non-zero for valid expressions.
	word1 := unsafe { (&u64(&expr))[1] }
	return word1 != 0
}

// get_expr_type returns the types.Type for an expression by looking it up in the environment
fn (t &Transformer) get_expr_type(expr ast.Expr) ?types.Type {
	// Handle specific expression types that may not have pos info or need special handling
	// These checks go BEFORE pos() to avoid dereferencing corrupt sum type data pointers
	// in ARM64-compiled binaries.
	if expr is ast.Ident {
		// Try pos-based lookup first for Idents (faster)
		pos := expr.pos
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return typ
			}
		}
		if typ := t.lookup_var_type(expr.name) {
			return typ
		}
		return none
	}
	// For UnsafeExpr, look at the type of the inner expression
	if expr is ast.UnsafeExpr {
		// Try pos-based lookup first
		if expr.pos.is_valid() {
			if typ := t.env.get_expr_type(expr.pos.id) {
				return typ
			}
		}
		if expr.stmts.len > 0 {
			last := expr.stmts[expr.stmts.len - 1]
			if last is ast.ExprStmt {
				if typ := t.get_expr_type(last.expr) {
					return typ
				}
			}
		}
		return none
	}
	// For CallExpr/CallOrCastExpr, try pos then function return type lookup
	if expr is ast.CallExpr {
		pos := expr.pos
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return typ
			}
		}
		if typ := t.resolve_call_return_type(expr) {
			return typ
		}
		return none
	}
	if expr is ast.CallOrCastExpr {
		pos := expr.pos
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return typ
			}
		}
		if typ := t.resolve_call_return_type(expr) {
			return typ
		}
		return none
	}
	// For other expression types, use the generic pos() dispatch
	// Guard against corrupt sum type data in ARM64-compiled binaries
	if !expr_has_valid_data(expr) {
		return none
	}
	pos := expr.pos()
	if pos.id != 0 {
		if typ := t.env.get_expr_type(pos.id) {
			return typ
		}
		// Check synth_types for types registered during the current transform pass
		// (they haven't been applied to env yet).
		if typ := t.synth_types[pos.id] {
			return typ
		}
	}
	return none
}

fn sanitize_generic_token_part(name string) string {
	if name == '' {
		return 'Type'
	}
	mut out := strings.new_builder(name.len)
	mut wrote_sep := false
	for ch in name {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) {
			out.write_u8(ch)
			wrote_sep = false
		} else if !wrote_sep {
			out.write_u8(`_`)
			wrote_sep = true
		}
	}
	mut tok := out.str().trim('_')
	if tok == '' {
		tok = 'Type'
	}
	return tok
}

fn (t &Transformer) generic_specialization_token_from_type(typ types.Type) string {
	match typ {
		types.Array {
			return 'Array_' + t.generic_specialization_token_from_type(typ.elem_type)
		}
		types.ArrayFixed {
			return 'Array_fixed_' + t.generic_specialization_token_from_type(typ.elem_type) + '_' +
				typ.len.str()
		}
		types.Map {
			return 'Map_' + t.generic_specialization_token_from_type(typ.key_type) + '_' +
				t.generic_specialization_token_from_type(typ.value_type)
		}
		types.OptionType {
			return 'Option_' + t.generic_specialization_token_from_type(typ.base_type)
		}
		types.ResultType {
			return 'Result_' + t.generic_specialization_token_from_type(typ.base_type)
		}
		types.Pointer {
			return t.generic_specialization_token_from_type(typ.base_type) + 'ptr'
		}
		types.Alias {
			if typ.name != '' {
				return sanitize_generic_token_part(typ.name)
			}
			return t.generic_specialization_token_from_type(typ.base_type)
		}
		else {
			type_name := typ.name()
			if type_name == '' {
				return 'Type'
			}
			return sanitize_generic_token_part(type_name)
		}
	}
}

fn (t &Transformer) generic_specialization_token(expr ast.Expr) string {
	if concrete := t.get_expr_type(expr) {
		return t.generic_specialization_token_from_type(concrete)
	}
	match expr {
		ast.PrefixExpr {
			if expr.op == .amp {
				return t.generic_specialization_token(expr.expr) + 'ptr'
			}
			return t.generic_specialization_token(expr.expr)
		}
		ast.Ident {
			return sanitize_generic_token_part(expr.name)
		}
		ast.SelectorExpr {
			return sanitize_generic_token_part(t.type_expr_to_c_name(expr))
		}
		ast.Type {
			return sanitize_generic_token_part(expr.name())
		}
		else {
			return sanitize_generic_token_part(expr.name())
		}
	}
}

fn generic_placeholder_name(expr ast.Expr) ?string {
	if expr is ast.Ident {
		if expr.name in ['T', 'U', 'V', 'K', 'W'] {
			return expr.name
		}
	}
	return none
}

fn (t &Transformer) generic_specialization_suffix(args []ast.Expr) string {
	if args.len == 0 {
		return ''
	}
	mut all_placeholders := true
	mut placeholder_parts := []string{cap: args.len}
	for arg in args {
		if name := generic_placeholder_name(arg) {
			placeholder_parts << name
		} else {
			all_placeholders = false
			break
		}
	}
	if all_placeholders {
		return '_' + placeholder_parts.join('_')
	}
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << t.generic_specialization_token(arg)
	}
	return '_T_' + parts.join('_')
}

fn (mut t Transformer) specialize_generic_callable_expr(lhs ast.Expr, args []ast.Expr, pos token.Pos) ast.Expr {
	suffix := t.generic_specialization_suffix(args)
	match lhs {
		ast.Ident {
			return ast.Expr(ast.Ident{
				name: lhs.name + suffix
				pos:  pos
			})
		}
		ast.SelectorExpr {
			return ast.Expr(ast.SelectorExpr{
				lhs: t.transform_expr(lhs.lhs)
				rhs: ast.Ident{
					name: lhs.rhs.name + suffix
					pos:  lhs.rhs.pos
				}
				pos: pos
			})
		}
		else {
			return ast.Expr(ast.Ident{
				name: lhs.name() + suffix
				pos:  pos
			})
		}
	}
}

// get_receiver_type_name extracts the type name from a receiver type AST expression
// This is used to build the scope key for methods (e.g., "SortedMap__set")
fn (t &Transformer) get_receiver_type_name(typ ast.Expr) string {
	if typ is ast.Ident {
		return typ.name
	}
	if typ is ast.PrefixExpr && typ.op == token.Token.amp {
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
		if typ.props.has(types.Properties.boolean) {
			return 'bool'
		} else if typ.props.has(types.Properties.integer) {
			if typ.props.has(types.Properties.unsigned) {
				return 'u${typ.size}'
			} else {
				return if typ.size == 0 { 'int' } else { 'i${typ.size}' }
			}
		} else if typ.props.has(types.Properties.float) {
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
				// Use array_elem_type_name_for_helpers which handles Alias types safely
				// (type_to_c_name_resolve_alias accesses .base_type which is malformed in self-host mode)
				elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
				return t.normalize_literal_type(elem_name)
			}
			if base_type is types.ArrayFixed {
				elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
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
		var_type_name := t.get_var_type_name(expr.name)
		if var_type_name != '' {
			// Handle pointer to array (&[]T) - strip the & prefix first
			mut type_to_check := var_type_name
			if type_to_check.starts_with('&') {
				type_to_check = type_to_check[1..]
			}
			// Now convert to C-style
			c_type := t.v_type_name_to_c_name(type_to_check)
			if c_type.starts_with('Array_') {
				if c_type.starts_with('Array_fixed_') {
					payload := c_type['Array_fixed_'.len..]
					if payload.contains('_') {
						elem := payload.all_before_last('_')
						return t.normalize_literal_type(elem)
					}
				}
				elem := c_type['Array_'.len..]
				return t.normalize_literal_type(elem)
			}
			// Handle strings.Builder which is an alias for []u8
			if c_type.starts_with('strings__Builder') || c_type == 'Builder' {
				return 'u8'
			}
		}
	}
	if expr is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(expr) {
			if array_type := t.get_array_type_str_from_type(field_type) {
				if array_type.starts_with('Array_') {
					return array_type['Array_'.len..]
				}
			}
		}
	}
	// Handle PrefixExpr (e.g., *ptr where ptr is pointer to array)
	if expr is ast.PrefixExpr && expr.op == token.Token.mul {
		// Dereference: check the inner expression's type
		if elem := t.get_array_elem_type_str(expr.expr) {
			return elem
		}
	}
	// Handle ArrayInitExpr directly (for inline array literals like [1, 2, 3])
	if expr is ast.ArrayInitExpr {
		arr_type := t.get_array_type_str(expr) or { return none }
		if arr_type.starts_with('Array_') {
			if arr_type.starts_with('Array_fixed_') {
				payload := arr_type['Array_fixed_'.len..]
				if payload.contains('_') {
					return payload.all_before_last('_')
				}
			}
			return arr_type['Array_'.len..]
		}
	}
	// Handle CallExpr - check function return type
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		ret_type := t.get_call_return_type(expr)
		if ret_type.starts_with('Array_') {
			if ret_type.starts_with('Array_fixed_') {
				payload := ret_type['Array_fixed_'.len..]
				if payload.contains('_') {
					return payload.all_before_last('_')
				}
			}
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
				return t.array_elem_type_name_for_helpers(field_type.elem_type)
			}
			if field_type is types.ArrayFixed {
				return t.array_elem_type_name_for_helpers(field_type.elem_type)
			}
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
			if map_type.value_type is types.ArrayFixed {
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

fn normalize_array_elem_type_name(type_name string) string {
	mut name := type_name.trim_space()
	if name == '' {
		return ''
	}
	if name.starts_with('&') {
		name = name[1..]
	}
	for name.ends_with('*') && name.len > 0 {
		name = name[..name.len - 1]
	}
	return name
}

fn (t &Transformer) canonical_array_elem_type_name(type_name string) string {
	name := normalize_array_elem_type_name(type_name)
	if name == '' {
		return ''
	}
	if name.starts_with('Array_fixed_') {
		payload := name['Array_fixed_'.len..]
		last_underscore := payload.last_index('_') or { return name }
		elem := t.canonical_array_elem_type_name(payload[..last_underscore])
		return 'Array_fixed_${elem}_${payload[last_underscore + 1..]}'
	}
	if name.starts_with('Array_') {
		elem := t.canonical_array_elem_type_name(name['Array_'.len..])
		return 'Array_${elem}'
	}
	if typ := t.c_name_to_type(name) {
		if typ is types.Alias {
			resolved := t.type_to_c_name_resolve_alias(typ)
			if resolved != '' {
				return normalize_array_elem_type_name(resolved)
			}
		}
		base := t.unwrap_alias_and_pointer_type(typ)
		if base is types.Alias {
			resolved := t.type_to_c_name_resolve_alias(base)
			if resolved != '' {
				return normalize_array_elem_type_name(resolved)
			}
		}
	}
	return name
}

fn short_array_elem_type_name(type_name string) string {
	name := normalize_array_elem_type_name(type_name)
	if name.contains('__') {
		return name.all_after_last('__')
	}
	return name
}

fn (t &Transformer) array_elem_types_compatible(lhs_elem string, rhs_elem string) bool {
	left := normalize_array_elem_type_name(lhs_elem)
	right := normalize_array_elem_type_name(rhs_elem)
	if left == '' || right == '' {
		return false
	}
	if left in ['array', '__generic_array__'] || right in ['array', '__generic_array__'] {
		return true
	}
	if left == right {
		return true
	}
	canonical_left := t.canonical_array_elem_type_name(left)
	canonical_right := t.canonical_array_elem_type_name(right)
	if canonical_left != '' && canonical_left == canonical_right {
		return true
	}
	left_is_array_like := left.starts_with('Array_') || left.starts_with('Array_fixed_')
	right_is_array_like := right.starts_with('Array_') || right.starts_with('Array_fixed_')
	if left_is_array_like != right_is_array_like {
		return false
	}
	if short_array_elem_type_name(left) == short_array_elem_type_name(right) {
		return true
	}
	return false
}

fn (t &Transformer) array_value_elem_type(expr ast.Expr) ?string {
	if elem_type := t.get_array_elem_type_str(expr) {
		return elem_type
	}
	if typ := t.get_expr_type(expr) {
		base := t.unwrap_alias_and_pointer_type(typ)
		if base is types.Array {
			return t.normalize_literal_type(t.array_elem_type_name_for_helpers(base.elem_type))
		}
		if base is types.ArrayFixed {
			return t.normalize_literal_type(t.array_elem_type_name_for_helpers(base.elem_type))
		}
	}
	// PostfixExpr `!` or `?` (error/option propagation) unwraps the inner expression.
	// e.g., `parse_ipv4(address)!` → inner `parse_ipv4(address)` returns `![]u8`,
	// so the unwrapped result is `[]u8`.
	if expr is ast.PostfixExpr && expr.op in [.not, .question] {
		// Try direct recursion (checker may annotate the inner expr with the array type)
		if elem := t.array_value_elem_type(expr.expr) {
			return elem
		}
		// For calls returning Result/Option, the call return type is _result_Array_T.
		// Strip the wrapper prefix to get the underlying array type.
		if expr.expr is ast.CallExpr || expr.expr is ast.CallOrCastExpr {
			ret_type := t.get_call_return_type(expr.expr)
			mut base := ret_type
			if base.starts_with('_result_') {
				base = base['_result_'.len..]
			} else if base.starts_with('_option_') {
				base = base['_option_'.len..]
			}
			if base.starts_with('Array_') {
				return base['Array_'.len..]
			}
		}
	}
	if expr is ast.CallExpr || expr is ast.CallOrCastExpr {
		ret_type := t.get_call_return_type(expr)
		if ret_type.starts_with('Array_fixed_') {
			payload := ret_type['Array_fixed_'.len..]
			if payload.contains('_') {
				return payload.all_before_last('_')
			}
		}
		if ret_type.starts_with('Array_') {
			return ret_type['Array_'.len..]
		}
	}
	if typ := t.get_expr_type(expr) {
		mut base_type := typ.base_type()
		if base_type is types.Alias {
			base_type = (base_type as types.Alias).base_type
		}
		match base_type {
			types.Array {
				arr_type := base_type as types.Array
				return t.type_to_c_name_resolve_alias(arr_type.elem_type)
			}
			types.ArrayFixed {
				arr_fixed_type := base_type as types.ArrayFixed
				return t.type_to_c_name_resolve_alias(arr_fixed_type.elem_type)
			}
			else {}
		}
	}
	return none
}

// array_elem_needs_deep_eq returns true if the array element type is a struct or map,
// meaning memcmp-based comparison won't work (strings, maps, nested structs need deep comparison).
fn (t &Transformer) array_elem_needs_deep_eq(expr ast.Expr) bool {
	recv_type := t.get_expr_type(expr) or { return false }
	base := t.unwrap_alias_and_pointer_type(recv_type)
	if base is types.Array {
		elem_base := t.unwrap_alias_and_pointer_type(base.elem_type)
		return elem_base is types.Struct || elem_base is types.Map
	}
	return false
}

// get_struct_field_type returns the type of a struct field from a SelectorExpr
fn (t &Transformer) get_array_type_str(expr ast.Expr) ?string {
	// Check for array element type overrides first (e.g. from .map(fn_name) expansion
	// where the checker incorrectly types the result as []voidptr).
	if expr is ast.Ident {
		if override := t.array_elem_type_overrides[expr.name] {
			return 'Array_${override}'
		}
	}
	if expr is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(expr) {
			if array_type := t.get_array_type_str_from_type(field_type) {
				return array_type
			}
		}
	}
	recv_type := t.get_expr_type(expr) or { return none }
	return t.get_array_type_str_from_type(recv_type)
}

fn (t &Transformer) array_method_elem_type_name(expr ast.Expr) ?string {
	mut receiver := ast.Expr(ast.empty_expr)
	mut method_name := ''
	if expr is ast.CallExpr {
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			receiver = sel.lhs
			method_name = sel.rhs.name
		}
	} else if expr is ast.CallOrCastExpr {
		if expr.lhs is ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			receiver = sel.lhs
			method_name = sel.rhs.name
		}
	}
	if method_name !in ['last', 'first', 'pop', 'pop_left'] {
		return none
	}
	array_type := t.get_array_type_str(receiver) or { return none }
	if !array_type.starts_with('Array_') {
		return none
	}
	return array_type['Array_'.len..]
}

fn (mut t Transformer) with_typed_array_method_receiver(expr ast.Expr, elem_type types.Type) ast.Expr {
	array_type := types.Type(types.Array{
		elem_type: elem_type
	})
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident
				&& expr.lhs.name in ['array__last', 'array__first', 'array__pop', 'array__pop_left']
				&& expr.args.len > 0 {
				mut args := expr.args.clone()
				args[0] = t.with_callsite_array_type(args[0], array_type)
				return ast.Expr(ast.CallExpr{
					lhs:  expr.lhs
					args: args
					pos:  expr.pos
				})
			}
			if expr.lhs is ast.SelectorExpr {
				sel := expr.lhs as ast.SelectorExpr
				if sel.rhs.name in ['last', 'first', 'pop', 'pop_left'] {
					typed_receiver := t.with_callsite_array_type(sel.lhs, array_type)
					return ast.Expr(ast.CallExpr{
						lhs:  ast.SelectorExpr{
							lhs: typed_receiver
							rhs: sel.rhs
							pos: sel.pos
						}
						args: expr.args
						pos:  expr.pos
					})
				}
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.SelectorExpr {
				sel := expr.lhs as ast.SelectorExpr
				if sel.rhs.name in ['last', 'first', 'pop', 'pop_left'] {
					typed_receiver := t.with_callsite_array_type(sel.lhs, array_type)
					return ast.Expr(ast.CallOrCastExpr{
						lhs:  ast.SelectorExpr{
							lhs: typed_receiver
							rhs: sel.rhs
							pos: sel.pos
						}
						expr: expr.expr
						pos:  expr.pos
					})
				}
			}
		}
		else {}
	}

	return expr
}

fn (mut t Transformer) with_callsite_array_type(expr ast.Expr, array_type types.Type) ast.Expr {
	pos := expr.pos()
	if pos.is_valid() {
		t.register_synth_type(pos, array_type)
		return ast.Expr(ast.ParenExpr{
			expr: expr
			pos:  pos
		})
	}
	return t.with_synth_type(ast.Expr(ast.ParenExpr{
		expr: expr
	}), array_type)
}

fn (t &Transformer) get_array_type_str_from_type(typ types.Type) ?string {
	recv_type := typ
	base := t.unwrap_alias_and_pointer_type(recv_type)
	if base is types.Alias {
		alias_base := t.unwrap_alias_and_pointer_type(base.base_type)
		if alias_base is types.Array {
			elem_type := t.array_elem_type_name_for_helpers(alias_base.elem_type)
			if elem_type != '' && elem_type != 'void' {
				return 'Array_${elem_type}'
			}
		}
		if alias_base is types.ArrayFixed {
			elem_type := t.array_elem_type_name_for_helpers(alias_base.elem_type)
			if elem_type != '' && elem_type != 'void' {
				return 'Array_fixed_${elem_type}_${alias_base.len}'
			}
		}
	}
	if base is types.Array {
		elem_type := t.array_elem_type_name_for_helpers(base.elem_type)
		if elem_type != '' && elem_type != 'void' {
			return 'Array_${elem_type}'
		}
	}
	if base is types.ArrayFixed {
		elem_type := t.array_elem_type_name_for_helpers(base.elem_type)
		if elem_type != '' && elem_type != 'void' {
			return 'Array_fixed_${elem_type}_${base.len}'
		}
	}
	return none
}

fn (t &Transformer) unwrap_alias_and_pointer_type(typ types.Type) types.Type {
	mut cur := typ
	for cur is types.Pointer {
		ptr := cur as types.Pointer
		cur = ptr.base_type
	}
	return cur
}

// get_array_nesting_depth returns the nesting depth of an array type.
// []int → 1, [][]int → 2, [][][]int → 3, non-array → 0
fn (t &Transformer) get_array_nesting_depth(typ types.Type) int {
	base := t.unwrap_alias_and_pointer_type(typ)
	if base is types.Array {
		return 1 + t.get_array_nesting_depth(base.elem_type)
	}
	return 0
}

fn (t &Transformer) array_elem_type_name_for_helpers(elem_type types.Type) string {
	if elem_type is types.FnType {
		return 'voidptr'
	}
	if elem_type is types.Alias {
		// Use type_to_c_name which uses the safe .name field
		// (type_to_c_name_resolve_alias accesses .base_type which is malformed in self-host mode)
		result := t.type_to_c_name(elem_type)
		return result
	}
	result := t.type_to_c_name(elem_type)
	return result
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

// get_map_type_for_expr returns the Map_K_V type string for an expression if it's a map.
// Unwraps aliases and pointers (e.g., mut map parameters) before checking.
fn (t &Transformer) get_map_type_for_expr(expr ast.Expr) ?string {
	typ := t.get_expr_type(expr) or { return none }
	unwrapped := t.unwrap_alias_and_pointer_type(typ)
	// Also unwrap aliases (unwrap_alias_and_pointer_type only handles pointers)
	base := if unwrapped is types.Alias {
		(unwrapped as types.Alias).base_type
	} else {
		unwrapped
	}
	if base is types.Map {
		key_c := t.type_to_c_name(base.key_type)
		val_c := t.type_to_c_name(base.value_type)
		if key_c != '' && val_c != '' {
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

// type_constructor_name returns the V type constructor name for a types.Type variant.
// This is the name of the sum type VARIANT (e.g., 'Void', 'Struct', 'Primitive'),
// NOT the C type name (e.g., 'void', 'Foo', 'int').
// Used for sum type variant matching when type_to_c_name produces names that
// don't match variant constructor names.
fn (t &Transformer) type_constructor_name(typ types.Type) string {
	match typ {
		types.Alias { return 'Alias' }
		types.Array { return 'Array' }
		types.ArrayFixed { return 'ArrayFixed' }
		types.Channel { return 'Channel' }
		types.Char { return 'Char' }
		types.Enum { return 'Enum' }
		types.FnType { return 'FnType' }
		types.ISize { return 'ISize' }
		types.Interface { return 'Interface' }
		types.Map { return 'Map' }
		types.NamedType { return 'NamedType' }
		types.Nil { return 'Nil' }
		types.None { return 'None' }
		types.OptionType { return 'OptionType' }
		types.Pointer { return 'Pointer' }
		types.Primitive { return 'Primitive' }
		types.ResultType { return 'ResultType' }
		types.Rune { return 'Rune' }
		types.String { return 'String' }
		types.Struct { return 'Struct' }
		types.SumType { return 'SumType' }
		types.Thread { return 'Thread' }
		types.Tuple { return 'Tuple' }
		types.USize { return 'USize' }
		types.Void { return 'Void' }
	}
}

// type_to_c_name converts a types.Type to its C type name string
fn (t &Transformer) type_to_c_name(typ types.Type) string {
	match typ {
		types.Primitive {
			// Map V primitive types to C type names
			if typ.props.has(types.Properties.boolean) {
				return 'bool'
			}
			if typ.props.has(types.Properties.float) {
				match typ.size {
					32 { return 'f32' }
					64 { return 'f64' }
					else { return 'f64' } // default float is f64
				}
			}
			if typ.props.has(types.Properties.unsigned) {
				match typ.size {
					8 { return 'u8' }
					16 { return 'u16' }
					32 { return 'u32' }
					64 { return 'u64' }
					else { return 'int' }
				}
			}
			match typ.size {
				8 { return 'i8' }
				16 { return 'i16' }
				32, 0 { return 'int' }
				64 { return 'i64' }
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
		types.Interface {
			return t.qualify_type_name(typ.name)
		}
		types.SumType {
			return t.qualify_type_name(types.sum_type_name(typ))
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
		types.Tuple {
			tuple_types := typ.get_types()
			mut names := []string{cap: tuple_types.len}
			for elem_type in tuple_types {
				names << t.type_to_c_name(elem_type)
			}
			return 'Tuple_${names.join('_')}'
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
		types.Channel {
			return 'chan'
		}
		types.FnType {
			return 'voidptr'
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
		primitives := ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'bool', 'f32',
			'f64', 'rune']
		// First check if the alias name itself is already a primitive
		// (in self-host mode, 'int' is stored as Alias with malformed .base_type)
		alias_name := t.type_to_c_name(typ)
		if alias_name in primitives {
			return alias_name
		}
		// Resolve to the underlying type
		base := typ.base_type
		// If base is a primitive int type, use that
		base_name := t.type_to_c_name(base)
		if base_name in primitives {
			return base_name
		}
		// Otherwise keep the alias name
		return alias_name
	}
	// For non-alias types, use normal type_to_c_name
	return t.type_to_c_name(typ)
}

// get_enum_type_name returns the enum type name for an expression, or empty string if not an enum
fn (t &Transformer) get_enum_type_name(expr ast.Expr) string {
	// Check scope for variable type
	if expr is ast.Ident {
		type_name := t.get_var_type_name(expr.name)
		if type_name != ''
			&& type_name !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'string', 'bool', 'rune', 'voidptr', 'byteptr', 'charptr', 'nil'] {
			return type_name
		}
	}
	// Handle SelectorExpr - field access like p.status or p->status
	if expr is ast.SelectorExpr {
		if field_type := t.get_struct_field_type(expr) {
			base := t.unwrap_alias_and_pointer_type(field_type)
			if base is types.Enum {
				return t.type_to_c_name(base)
			}
		}
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
			// First try with variable name directly - this enables smartcast lookup
			// e.g. for `expr.op == .mul` after `expr is PrefixExpr`, find_smartcast_for_expr("expr")
			// resolves to PrefixExpr, then PrefixExpr.op -> token__Token
			field_type_via_var := t.resolve_field_type(lhs_ident.name, expr.rhs.name)
			if field_type_via_var != '' && field_type_via_var != 'int'
				&& field_type_via_var != 'string' && field_type_via_var != 'bool' {
				return field_type_via_var
			}
			// Fall back to type-based lookup
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
		base := t.unwrap_alias_and_pointer_type(typ)
		if base is types.Enum {
			return t.type_to_c_name(base)
		}
	}
	return ''
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
					if array_type := t.get_array_type_str(rhs) {
						return array_type
					}
				}
			}
		}
	}
	return ''
}

fn (t &Transformer) is_pointer_type(typ types.Type) bool {
	match typ {
		types.Pointer {
			return true
		}
		types.Alias {
			return t.is_pointer_type(typ.base_type)
		}
		else {
			return false
		}
	}
}

// is_pointer_type_expr returns true if the expression is of a pointer type
fn (t &Transformer) is_pointer_type_expr(expr ast.Expr) bool {
	if expr is ast.Ident {
		if expr.name in t.cur_fn_mut_params {
			return true
		}
		if typ := t.lookup_var_type(expr.name) {
			return t.is_pointer_type(typ)
		}
		// Fallback to string-based check for partial type info.
		var_type_name := t.get_var_type_name(expr.name)
		if var_type_name != '' {
			// Check for both '*' suffix (C-style) and '&' prefix (V reference types)
			return var_type_name.ends_with('*') || var_type_name.starts_with('&')
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

// get_str_fn_name_for_type returns the str function name for a types.Type
fn (t &Transformer) get_str_fn_name_for_type(typ types.Type) ?string {
	match typ {
		types.Array {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_${elem_name}_str'
		}
		types.ArrayFixed {
			elem_name := t.type_to_c_name(typ.elem_type)
			return 'Array_fixed_${elem_name}_${typ.len}_str'
		}
		types.Map {
			key_name := t.type_to_c_name(typ.key_type)
			val_name := t.type_to_c_name(typ.value_type)
			return 'Map_${key_name}_${val_name}_str'
		}
		types.Struct {
			return '${typ.name}__str'
		}
		types.SumType {
			return '${t.type_to_c_name(typ)}__str'
		}
		types.Enum {
			return '${typ.name}__str'
		}
		types.NamedType {
			name := string(typ)
			if name == '' {
				return none
			}
			if name.len == 1 && name[0] >= `A` && name[0] <= `Z` {
				return none
			}
			if resolved := t.lookup_type(name) {
				if resolved is types.NamedType {
					if string(resolved) == name {
						return none
					}
				}
				return t.get_str_fn_name_for_type(resolved)
			}
			return '${t.type_to_c_name(typ)}__str'
		}
		types.Primitive {
			if typ.props.has(types.Properties.boolean) {
				return 'bool__str'
			}
			if typ.props.has(types.Properties.float) {
				if typ.size == 4 {
					return 'f32__str'
				}
				return 'f64__str'
			}
			if typ.props.has(types.Properties.unsigned) {
				match typ.size {
					1 { return 'u8__str' }
					2 { return 'u16__str' }
					4 { return 'u32__str' }
					8 { return 'u64__str' }
					else { return 'int__str' }
				}
			}
			match typ.size {
				1 { return 'i8__str' }
				2 { return 'i16__str' }
				4 { return 'int__str' }
				8 { return 'i64__str' }
				else { return 'int__str' }
			}
		}
		types.Pointer {
			// For pointers, use the base type's str function
			return t.get_str_fn_name_for_type(typ.base_type)
		}
		types.Alias {
			// Recurse to base type — aliases to primitives (e.g., ValueID = int)
			// don't have their own str() functions.
			return t.get_str_fn_name_for_type(typ.base_type)
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

// resolve_typeof_expr resolves typeof(expr) to a V type name string.
fn (t &Transformer) resolve_typeof_expr(expr ast.Expr) string {
	if raw_type := t.get_expr_type(expr) {
		return t.types_type_to_v(raw_type)
	}
	return ''
}

// c_name_to_v_name converts a C-mangled name (e.g. "os__File") to V format ("os.File").
// Strips "builtin__" and "main__" prefixes since V doesn't show them in typeof.
fn c_name_to_v_name(name string) string {
	if name.starts_with('builtin__') {
		return name['builtin__'.len..]
	}
	if name.starts_with('main__') {
		return name['main__'.len..]
	}
	// Convert module__Name to module.Name
	if idx := name.index('__') {
		return name[..idx] + '.' + name[idx + 2..]
	}
	return name
}

// types_type_to_v converts a types.Type to a V type name string (e.g. "map[rune]int", "[]string").
// Used for typeof(expr) which needs V syntax, not C type names.
fn (t &Transformer) types_type_to_v(typ types.Type) string {
	match typ {
		types.Primitive {
			if typ.props.has(.integer) {
				if typ.props.has(.untyped) {
					return 'int'
				}
				size := if typ.size == 0 { 32 } else { int(typ.size) }
				is_signed := !typ.props.has(.unsigned)
				return if is_signed {
					match size {
						8 { 'i8' }
						16 { 'i16' }
						32 { 'int' }
						64 { 'i64' }
						else { 'int' }
					}
				} else {
					match size {
						8 { 'u8' }
						16 { 'u16' }
						32 { 'u32' }
						else { 'u64' }
					}
				}
			} else if typ.props.has(.float) {
				if typ.props.has(.untyped) {
					return 'f64'
				}
				return if typ.size == 32 { 'f32' } else { 'f64' }
			} else if typ.props.has(.boolean) {
				return 'bool'
			}
			return 'int'
		}
		types.Pointer {
			base := t.types_type_to_v(typ.base_type)
			return '&' + base
		}
		types.Array {
			elem := t.types_type_to_v(typ.elem_type)
			return '[]' + elem
		}
		types.ArrayFixed {
			elem := t.types_type_to_v(typ.elem_type)
			return '[' + typ.len.str() + ']' + elem
		}
		types.Struct {
			return c_name_to_v_name(typ.name)
		}
		types.String {
			return 'string'
		}
		types.Alias {
			return c_name_to_v_name(typ.name)
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
		types.Enum {
			return c_name_to_v_name(typ.name)
		}
		types.Interface {
			return c_name_to_v_name(typ.name)
		}
		types.SumType {
			return c_name_to_v_name(types.sum_type_name(typ))
		}
		types.Map {
			key := t.types_type_to_v(typ.key_type)
			val := t.types_type_to_v(typ.value_type)
			return 'map[' + key + ']' + val
		}
		types.OptionType {
			base := t.types_type_to_v(typ.base_type)
			return '?' + base
		}
		types.ResultType {
			base := t.types_type_to_v(typ.base_type)
			return '!' + base
		}
		types.FnType {
			return 'fn ()'
		}
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Nil {
			return 'voidptr'
		}
		types.None {
			return 'void'
		}
		else {
			return 'int'
		}
	}
}

// type_sizeof returns the byte size of a type for the native (arm64/x64) backend.
// is_memcmp_safe_type checks if a type can be compared with memcmp.
// Primitives and fixed arrays of primitives are safe. Types containing
// heap pointers (dynamic arrays, strings, maps, structs) are not.
fn (t &Transformer) is_memcmp_safe_type(typ types.Type) bool {
	match typ {
		types.Primitive { return true }
		types.ArrayFixed { return t.is_memcmp_safe_type(typ.elem_type) }
		types.Alias { return t.is_memcmp_safe_type(typ.base_type) }
		types.Enum { return true }
		else { return false }
	}
}

fn (t &Transformer) type_sizeof(typ types.Type) int {
	match typ {
		types.Primitive {
			if typ.size > 0 {
				return int(typ.size) / 8
			}
			// bool has size 0, treat as 1 byte
			return 1
		}
		types.Pointer {
			return 8
		}
		types.Array {
			return 32 // dynamic array struct: ptr(8) + 5*i32(20) padded to 32
		}
		types.ArrayFixed {
			return typ.len * t.type_sizeof(typ.elem_type)
		}
		types.Map {
			return 120
		}
		types.Alias {
			return t.type_sizeof(typ.base_type)
		}
		types.Struct {
			return 8 // fallback; structs need field-level computation
		}
		else {
			return 8
		}
	}
}
