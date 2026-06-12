// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module transformer

import strconv
import v2.ast
import v2.token
import v2.types
import strings

// register_synth_type registers a type for a synthesized node position.
// Accumulates in synth_types map for deferred application (thread-safe).
fn (mut t Transformer) register_synth_type(pos token.Pos, typ types.Type) {
	t.synth_types[pos.id] = typ
}

fn (t &Transformer) get_synth_type(pos token.Pos) ?types.Type {
	if pos.id == 0 {
		return none
	}
	typ := t.synth_types[pos.id] or { return none }
	return t.normalize_type(typ)
}

// lookup_method_cached looks up a method by receiver type name and method name
// using cached_methods (lock-free) instead of env.lookup_method.
fn (t &Transformer) lookup_method_cached(type_name string, method_name string) ?types.FnType {
	// O(1) via the precomputed base-name index (built in build_cached_method_base_index).
	// Equivalent to the old linear scan: match by generic base name, first FnType wins.
	if typ := t.cached_method_base_index['${type_name}#${method_name}'] {
		if typ is types.FnType {
			return typ
		}
		return none
	}
	base_method_name := generic_base_name_without_specialization(method_name)
	if base_method_name == method_name {
		return none
	}
	typ := t.cached_method_base_index['${type_name}#${base_method_name}'] or { return none }
	if typ is types.FnType {
		return typ
	}
	return none
}

// lookup_fn_cached looks up a function by module and name
// using cached_scopes (lock-free) instead of env.lookup_fn.
fn (t &Transformer) lookup_fn_cached(module_name string, fn_name string) ?types.FnType {
	typ := t.cached_fn_type_index['${module_name}#${fn_name}'] or { return none }
	if typ is types.FnType {
		return typ
	}
	return none
}

// register_generated_fn_scope creates a function scope for a transformer-generated function
// (e.g. Array_int_contains, Array_string_str) and registers parameter types so cleanc
// can resolve them via scope lookup instead of falling back to string-based inference.
fn value_object_from_type(typ types.Type) types.Object {
	return types.TypeObject{
		typ: typ
	}
}

fn (mut t Transformer) register_generated_fn_scope(fn_name string, module_name string, params []ast.Parameter) {
	parent := t.get_module_scope(module_name) or { return }
	mut fn_scope := types.new_scope(parent)
	for param in params {
		type_name := t.expr_to_type_name(param.typ)
		if type_name == '' {
			continue
		}
		if param_type := t.c_name_to_type(type_name) {
			fn_scope.insert(param.name, value_object_from_type(param_type))
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
	if name.ends_with('ptr') && name !in ['byteptr', 'charptr', 'voidptr'] {
		if typ := t.lookup_type(name) {
			return typ
		}
		base_name := name[..name.len - 'ptr'.len]
		base_type := t.c_name_to_type(base_name) or { return none }
		return types.Pointer{
			base_type: base_type
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
		'char', 'rune', 'usize', 'isize', 'void', 'nil', 'none', 'byteptr', 'charptr', 'voidptr' {
			types.builtin_type(name) or { return none }
		}
		else {
			// Try looking up by name in module scopes
			t.lookup_type(name)
		}
	}
}

// lookup_var_type looks up a variable's type in the current scope chain
fn (t &Transformer) lookup_var_type(name string) ?types.Type {
	if t.scope == unsafe { nil } {
		if t.fn_root_scope != unsafe { nil } {
			if typ := t.fn_root_scope.lookup_var_type(name) {
				return t.normalize_type(typ)
			}
		}
		if t.cur_module != '' {
			if scope := t.get_module_scope(t.cur_module) {
				if typ := scope.lookup_var_type(name) {
					return t.normalize_type(typ)
				}
			}
		}
		if typ := t.lookup_imported_var_type(name) {
			return typ
		}
		return none
	}
	mut scope := unsafe { t.scope }
	if typ := scope.lookup_var_type(name) {
		return t.normalize_type(typ)
	}
	if t.fn_root_scope != unsafe { nil } {
		if typ := t.fn_root_scope.lookup_var_type(name) {
			return t.normalize_type(typ)
		}
	}
	if t.cur_module != '' {
		if mod_scope := t.get_module_scope(t.cur_module) {
			if typ := mod_scope.lookup_var_type(name) {
				return t.normalize_type(typ)
			}
		}
	}
	if typ := t.lookup_imported_var_type(name) {
		return typ
	}
	return none
}

fn (t &Transformer) lookup_imported_var_type(name string) ?types.Type {
	if name == '' || name.contains('__') || t.cur_module == '' {
		return none
	}
	// The imported-module scope list is precomputed once in cache_env_maps
	// (build_cached_imported_module_scopes); see the field comment in
	// transformer.v. Iterate only the imports rather than rescanning and
	// sorting the whole module symbol table on every lookup. The "found in
	// exactly one import" / ambiguity semantics are order-independent, so the
	// dropped per-call sort does not change the result.
	scopes := t.cached_imported_module_scopes[t.cur_module] or { return none }
	mut found_type := types.Type(types.void_)
	mut found := false
	for module_scope in scopes {
		typ := module_scope.lookup_var_type(name) or { continue }
		if found {
			return none
		}
		found_type = t.normalize_type(typ)
		found = true
	}
	if found {
		return found_type
	}
	return none
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

fn (t &Transformer) live_alias_base_type(alias types.Alias) ?types.Type {
	if transformer_string_has_valid_data(alias.name) && alias.name != '' {
		if live_type := t.lookup_type(alias.name) {
			if live_type is types.Alias {
				if types.type_has_valid_payload(live_type.base_type) {
					return live_type.base_type
				}
			} else {
				return live_type
			}
		}
	}
	if types.type_has_valid_payload(alias.base_type) {
		return alias.base_type
	}
	return none
}

fn (t &Transformer) normalize_type(typ types.Type) types.Type {
	if !types.type_has_valid_payload(typ) {
		return typ
	}
	if typ is types.Alias {
		if base_type := t.live_alias_base_type(typ) {
			return types.Type(types.Alias{
				name:      typ.name
				base_type: base_type
			})
		}
	}
	return typ
}

fn (t &Transformer) type_from_init_expr(expr ast.InitExpr) ?types.Type {
	if generic_type_name := t.generic_init_type_name(expr.typ) {
		if typ := t.lookup_type(generic_type_name) {
			return t.normalize_type(typ)
		}
		return types.Type(types.Struct{
			name: generic_type_name
		})
	}
	if typ := t.type_from_param_type_expr(expr.typ, []) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	if typ := t.get_expr_type(expr.typ) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	type_name := t.expr_to_type_name(expr.typ)
	if type_name == '' {
		return none
	}
	if typ := t.c_name_to_type(type_name) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	if typ := t.lookup_type(type_name) {
		normalized := t.normalize_type(typ)
		if t.type_to_c_name(normalized) != '' {
			return normalized
		}
	}
	c_type_name := t.v_type_name_to_c_name(type_name)
	return types.Type(types.Struct{
		name: if c_type_name != '' { c_type_name } else { type_name }
	})
}

fn (t &Transformer) generic_init_type_name(expr ast.Expr) ?string {
	match expr {
		ast.GenericArgs {
			base_name := t.expr_to_type_name(expr.lhs)
			suffix := t.generic_specialization_suffix(expr.args)
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		ast.GenericArgOrIndexExpr {
			base_name := t.expr_to_type_name(expr.lhs)
			suffix := t.generic_specialization_suffix([expr.expr])
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		ast.IndexExpr {
			base_name := t.expr_to_type_name(expr.lhs)
			suffix := t.generic_specialization_suffix([expr.expr])
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		else {}
	}

	return none
}

// lookup_type looks up a type by name in the module scope
fn (t &Transformer) lookup_type(name string) ?types.Type {
	if name.len == 0 || name.len > 1024 || !transformer_string_has_valid_data(name) {
		return none
	}
	// Handle qualified names like "ast__Expr" by extracting module and type name
	mut lookup_name := name
	mut lookup_module := t.cur_module
	// Use index_of instead of split to avoid array allocation
	dunder := name.index('__') or { -1 }
	if dunder >= 0 {
		lookup_module = name[..dunder]
		// Get the last segment after '__' (handles multi-part like "a__b__C")
		last_dunder := name.last_index('__') or { dunder }
		lookup_name = name[last_dunder + 2..]
	} else if dot := name.last_index('.') {
		lookup_module = name[..dot]
		lookup_name = name[dot + 1..]
	}
	mut scope := t.get_module_scope(lookup_module) or { return none }
	typ := scope.lookup_type_parent(lookup_name, 0) or { return none }
	if !is_type_valid(typ) {
		return none
	}
	return typ
}

fn (t &Transformer) lookup_struct_type_any_module(name string) ?types.Struct {
	mut fallback := types.Struct{}
	mut has_fallback := false
	if typ := t.lookup_type(name) {
		if typ is types.Struct {
			if typ.fields.len > 0 {
				return typ
			}
			fallback = typ
			has_fallback = true
		}
	}
	for _, scope in t.cached_scopes {
		if typ := scope.lookup_type(name) {
			if typ is types.Struct {
				if typ.fields.len > 0 {
					return typ
				}
				if !has_fallback {
					fallback = typ
					has_fallback = true
				}
			}
		}
	}
	if has_fallback {
		return fallback
	}
	return none
}

fn (t &Transformer) live_struct_type_from_type(typ types.Type) ?types.Struct {
	base_type := t.unwrap_alias_and_pointer_type(typ)
	if base_type !is types.Struct {
		return none
	}
	struct_type := base_type as types.Struct
	if struct_type.name != '' {
		if live_type := t.lookup_struct_type_any_module(struct_type.name) {
			return live_type
		}
	}
	return struct_type
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

fn transformer_selector_expr_parts(expr ast.SelectorExpr) []string {
	mut parts := []string{}
	collect_transformer_selector_expr_parts(ast.Expr(expr), mut parts)
	return parts
}

fn collect_transformer_selector_expr_parts(expr ast.Expr, mut parts []string) bool {
	match expr {
		ast.Ident {
			parts << expr.name
			return true
		}
		ast.SelectorExpr {
			if !collect_transformer_selector_expr_parts(expr.lhs, mut parts) {
				return false
			}
			parts << expr.rhs.name
			return true
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) selector_type_name(expr ast.SelectorExpr, full bool) string {
	parts := transformer_selector_expr_parts(expr)
	if parts.len == 0 {
		return ''
	}
	if !full || parts.len == 1 {
		return parts[parts.len - 1]
	}
	mut module_parts := parts[..parts.len - 1].clone()
	if resolved_module := t.cur_import_aliases[module_parts[0]] {
		mut resolved_parts := resolved_module.split('.')
		if module_parts.len > 1 {
			resolved_parts << module_parts[1..]
		}
		module_parts = resolved_parts.clone()
	}
	module_name := module_parts.join('__')
	type_name := parts[parts.len - 1]
	return '${module_name}__${type_name}'
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
		return t.selector_type_name(e, true)
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

fn (t &Transformer) get_sum_type_variants(type_name string) []string {
	if variants := t.sum_type_decl_variant_names[type_name] {
		return variants.clone()
	}
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

fn (mut t Transformer) collect_sum_type_decl_variant_names(files []ast.File) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_import_aliases := t.cur_import_aliases.clone()
	t.sum_type_decl_variant_names = map[string][]string{}
	for file in files {
		t.cur_file_name = file.name
		t.cur_module = file.mod
		t.cur_import_aliases = import_aliases_for_generic_collect(file.imports)
		if scope := t.get_module_scope(file.mod) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		for stmt in file.stmts {
			if stmt is ast.TypeDecl {
				t.cache_sum_type_decl_variant_names(file.mod, stmt)
			}
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) collect_sum_type_decl_variant_names_from_flat(flat &ast.FlatAst) {
	old_module := t.cur_module
	old_file := t.cur_file_name
	old_scope := t.scope
	old_import_aliases := t.cur_import_aliases.clone()
	t.sum_type_decl_variant_names = map[string][]string{}
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		t.cur_file_name = fc.name()
		t.cur_module = fc.mod()
		t.cur_import_aliases = flat_import_aliases_for_generic_collect(flat, fi)
		if scope := t.get_module_scope(t.cur_module) {
			t.scope = scope
		} else {
			t.scope = unsafe { nil }
		}
		stmts := fc.stmts()
		for si in 0 .. stmts.len() {
			stmt := stmts.at(si)
			if stmt.kind() == .stmt_type_decl {
				t.cache_sum_type_decl_variant_names(fc.mod(), stmt.type_decl())
			}
		}
	}
	t.cur_module = old_module
	t.cur_file_name = old_file
	t.scope = old_scope
	t.cur_import_aliases = old_import_aliases.clone()
}

fn (mut t Transformer) cache_sum_type_decl_variant_names(module_name string, decl ast.TypeDecl) {
	if decl.name == '' || decl.variants.len == 0 || decl.generic_params.len > 0 {
		return
	}
	mut variants := []string{cap: decl.variants.len}
	for variant in decl.variants {
		if sum_type_decl_variant_has_open_placeholder(variant) {
			return
		}
		variant_name := t.sum_type_decl_variant_name(variant)
		if variant_name == '' {
			return
		}
		variants << variant_name
	}
	if variants.len == 0 {
		return
	}
	for key in sum_type_decl_cache_keys(module_name, decl.name) {
		t.sum_type_decl_variant_names[key] = variants
	}
}

fn (t &Transformer) sum_type_decl_variant_name(variant ast.Expr) string {
	match variant {
		ast.GenericArgs {
			base_name := t.type_expr_name_full(variant.lhs)
			suffix := t.generic_specialization_suffix(variant.args)
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		ast.GenericArgOrIndexExpr {
			base_name := t.type_expr_name_full(variant.lhs)
			suffix := t.generic_specialization_suffix([variant.expr])
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		ast.IndexExpr {
			base_name := t.type_expr_name_full(variant.lhs)
			suffix := t.generic_specialization_suffix([variant.expr])
			if base_name != '' && suffix != '' {
				return base_name + suffix
			}
		}
		ast.Type {
			if variant is ast.GenericType {
				base_name := t.type_expr_name_full(variant.name)
				suffix := t.generic_specialization_suffix(variant.params)
				if base_name != '' && suffix != '' {
					return base_name + suffix
				}
			}
		}
		else {}
	}

	return t.type_expr_name_full(variant)
}

fn sum_type_decl_variant_has_open_placeholder(variant ast.Expr) bool {
	match variant {
		ast.GenericArgs {
			for arg in variant.args {
				if generic_type_expr_has_open_placeholder(arg) {
					return true
				}
			}
			return false
		}
		ast.GenericArgOrIndexExpr, ast.IndexExpr {
			return generic_type_expr_has_open_placeholder(variant.expr)
		}
		ast.Type {
			if variant is ast.GenericType {
				for param in variant.params {
					if generic_type_expr_has_open_placeholder(param) {
						return true
					}
				}
				return false
			}
			return generic_type_node_has_open_placeholder(variant)
		}
		else {
			return generic_type_expr_has_open_placeholder(variant)
		}
	}
}

fn sum_type_decl_cache_keys(module_name string, name string) []string {
	c_name := name.replace('.', '__')
	if c_name == '' {
		return []string{}
	}
	if c_name.contains('__') {
		return [c_name]
	}
	mut keys := []string{}
	if module_name == '' || module_name == 'main' || module_name == 'builtin' {
		keys << c_name
	}
	if module_name != '' {
		keys << '${module_name.replace('.', '__')}__${c_name}'
	}
	if keys.len == 0 {
		keys << c_name
	}
	return keys
}

fn sum_type_variant_match_name(name string) string {
	c_name := name.replace('.', '__')
	if c_name.starts_with('Array_') || c_name.starts_with('Array_fixed_')
		|| c_name.starts_with('Map_') || c_name.starts_with('_option_')
		|| c_name.starts_with('_result_') {
		return c_name
	}
	if c_name.contains('__') {
		return c_name.all_after_last('__')
	}
	return c_name
}

fn sum_type_variant_name_with_module(module_name string, variant_name string) string {
	if module_name == '' || variant_name == '' || variant_name.contains('__')
		|| variant_name.starts_with('Array_') || variant_name.starts_with('Array_fixed_')
		|| variant_name.starts_with('Map_') {
		return variant_name
	}
	return '${module_name}__${variant_name}'
}

fn qualify_sum_type_variant_for_match(sum_type_name string, variant_name string) string {
	if variant_name == '' || variant_name.contains('__') || !sum_type_name.contains('__')
		|| variant_name.starts_with('Array_') || variant_name.starts_with('Array_fixed_')
		|| variant_name.starts_with('Map_') || variant_name.starts_with('_option_')
		|| variant_name.starts_with('_result_') {
		return variant_name
	}
	return '${sum_type_name.all_before_last('__')}__${variant_name}'
}

fn sum_type_variant_matches(variant string, target string) bool {
	c_variant := variant.replace('.', '__')
	c_target := target.replace('.', '__')
	if c_variant == c_target {
		return true
	}
	if c_variant == '' || c_target == '' {
		return false
	}
	if c_variant.contains('__') && c_target.contains('__') {
		return false
	}
	variant_short := sum_type_variant_match_name(c_variant)
	target_short := sum_type_variant_match_name(c_target)
	return variant_short == target_short || c_target.ends_with('__${variant_short}')
}

fn sum_type_variant_matches_for_sumtype(sum_type_name string, variant string, target string) bool {
	return sum_type_variant_matches(qualify_sum_type_variant_for_match(sum_type_name, variant),
		target)
}

// transform_sumtype_type_name lowers `receiver.type_name()` on a sum type into
// a match expression on `receiver._tag` that returns the variant display name.
fn (mut t Transformer) transform_sumtype_type_name(receiver ast.Expr) ?ast.Expr {
	mut sumtype_name := t.get_sumtype_name_for_expr(receiver)
	if sumtype_name == '' {
		if receiver_type := t.get_expr_type(receiver) {
			base_type := t.unwrap_alias_and_pointer_type(receiver_type)
			if base_type is types.SumType {
				sumtype_name = types.sum_type_name(base_type)
			}
		}
	}
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

// find_sumtype_for_variant finds the sum type that contains the given variant
// This handles nested/aliased sum types by checking all known sum types
fn (t &Transformer) find_sumtype_for_variant(variant_name string) string {
	// Common sum types to check - prioritize Expr, Type, Stmt as they're most common
	sumtypes := ['Expr', 'Type', 'Stmt', 'ast__Expr', 'ast__Type', 'ast__Stmt']
	for st in sumtypes {
		variants := t.get_sum_type_variants(st)
		for v in variants {
			if sum_type_variant_matches_for_sumtype(st, v, variant_name) {
				return st
			}
		}
	}
	// Fallback: search sum types in the current module scope for user-defined types.
	// Only search the current module to avoid cross-module matches that would
	// incorrectly trigger smartcasting (e.g., types.Type containing Alias).
	cur_mod := if t.cur_module == '' { 'main' } else { t.cur_module }
	scope := t.get_module_scope(cur_mod) or { return '' }
	for type_name, typ in scope.types {
		if typ is types.SumType {
			st_name := if cur_mod != '' && cur_mod != 'main' && cur_mod != 'builtin' {
				'${cur_mod}__${type_name}'
			} else {
				type_name
			}
			variants := typ.get_variants()
			for v in variants {
				if sum_type_variant_matches_for_sumtype(st_name, v.name(), variant_name) {
					return st_name
				}
			}
		}
	}
	return ''
}

// get_var_type_name returns the type name of a variable from scope lookup
fn (t &Transformer) get_var_type_name(name string) string {
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
	if v_name.len == 0 || !transformer_string_has_valid_data(v_name) {
		return ''
	}
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
	if name.contains('.') {
		return name.replace('.', '__')
	}
	// No conversion needed
	return name
}

// qualify_type_name adds module prefix to type names that need it
// e.g., "File" in ast module becomes "ast__File"
fn (t &Transformer) qualify_type_name(type_name string) string {
	if type_name.len == 0 || !transformer_string_has_valid_data(type_name) {
		return ''
	}
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
	if !types.type_has_valid_payload(typ) {
		return ''
	}
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
			if !transformer_string_has_valid_data(typ.name) {
				return ''
			}
			return typ.name.replace('.', '__')
		}
		types.String {
			return 'string'
		}
		types.Primitive {
			if typ.props.has(types.Properties.boolean) {
				return 'bool'
			}
			if typ.props.has(types.Properties.float) {
				match typ.size {
					32 { return 'f32' }
					64 { return 'f64' }
					else { return 'f64' }
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
		else {
			name := typ.name()
			if !transformer_string_has_valid_data(name) {
				return ''
			}
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
		if typ := t.get_synth_type(expr.pos) {
			c_name := t.type_to_c_name(typ)
			if c_name != '' {
				return c_name
			}
		}
		name := expr.name
		// Add module prefix for non-builtin types when inside a non-main/builtin module.
		if name !in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune', 'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'voidptr', 'charptr', 'byteptr', 'void', 'nil']
			&& !name.contains('__') && t.cur_module != '' && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			if builtin_scope := t.cached_scopes['builtin'] {
				if obj := builtin_scope.objects[name] {
					if _ := transformer_object_type(obj) {
						return name
					}
				}
			}
			return '${t.cur_module}__${name}'
		}
		return name
	}
	if expr is ast.SelectorExpr {
		return t.selector_type_name(expr, true)
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
		if expr is ast.ArrayFixedType {
			elem_type := t.expr_to_type_name(expr.elem_type)
			if elem_type == '' {
				return ''
			}
			len := if expr.len is ast.BasicLiteral {
				(expr.len as ast.BasicLiteral).value
			} else {
				''
			}
			if len == '' {
				return ''
			}
			return 'Array_fixed_${elem_type}_${len}'
		}
		if expr is ast.PointerType {
			base_type := t.expr_to_type_name(expr.base_type)
			return pointer_type_c_name(base_type)
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

fn pointer_type_c_name(base_type string) string {
	if base_type == '' {
		return ''
	}
	return match base_type {
		'char' { 'charptr' }
		'void' { 'voidptr' }
		'u8', 'byte' { 'byteptr' }
		else { '${base_type}ptr' }
	}
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

fn (t &Transformer) field_type_from_receiver_type(receiver_type types.Type, field_name string) ?types.Type {
	mut cur := receiver_type
	for {
		if !transformer_type_has_safe_payload(cur) {
			return none
		}
		if cur is types.Pointer {
			cur = cur.base_type
			continue
		}
		if cur is types.Alias {
			if field_typ := t.lookup_struct_field_type(cur.name, field_name) {
				return field_typ
			}
			cur = cur.base_type
			continue
		}
		break
	}
	if field_name == 'flags' {
		base := t.unwrap_alias_and_pointer_type(cur)
		match base {
			types.Struct {
				if base.name in ['array', 'builtin__array'] {
					return t.lookup_type('ArrayFlags')
				}
			}
			types.Array {
				return t.lookup_type('ArrayFlags')
			}
			else {}
		}

		c_name := t.type_to_c_name(base)
		if c_name.starts_with('Array_') || c_name in ['array', 'builtin__array'] {
			return t.lookup_type('ArrayFlags')
		}
	}
	if cur is types.Struct {
		if field_typ := t.lookup_struct_field_generic_decl_type(cur.name, field_name) {
			return field_typ
		}
		if field_typ := t.struct_field_type(cur, field_name) {
			return field_typ
		}
	}
	if cur is types.SumType {
		return none
	}
	c_name := t.type_to_c_name(cur)
	if c_name != '' {
		if field_typ := t.lookup_struct_field_generic_decl_type(c_name, field_name) {
			return field_typ
		}
		if field_typ := t.lookup_struct_field_type(c_name, field_name) {
			return field_typ
		}
	}
	if cur is types.Struct {
		return none
	}
	type_name := t.type_to_name(cur)
	if type_name != '' && type_name != c_name {
		if field_typ := t.lookup_struct_field_generic_decl_type(type_name, field_name) {
			return field_typ
		}
		if field_typ := t.lookup_struct_field_type(type_name, field_name) {
			return field_typ
		}
	}
	return none
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
			base_name := types.type_name(typ.base_type)
			if base_name != '' && base_name != typ.name {
				return t.zero_value_expr_for_type(typ.base_type)
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
		types.Array {
			elem_type := t.type_to_ast_type_expr(typ.elem_type)
			return ast.Expr(ast.CallExpr{
				lhs:  ast.Ident{
					name: '__new_array_with_default_noscan'
				}
				args: [
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					}),
					ast.Expr(ast.BasicLiteral{
						kind:  .number
						value: '0'
					}),
					ast.Expr(ast.KeywordOperator{
						op:    .key_sizeof
						exprs: [elem_type]
					}),
					ast.Expr(ast.Ident{
						name: 'nil'
					}),
				]
			})
		}
		else {}
	}

	// For aggregates, use a compound-literal zero init: `((T){0})`.
	return ast.Expr(ast.InitExpr{
		typ: t.type_to_ast_type_expr(typ)
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

fn (mut t Transformer) resolve_expr_with_expected_wrapper_type(expr ast.Expr, expected types.Type, is_option bool) ast.Expr {
	expr_pos := expr.pos()
	match expr {
		ast.Keyword {
			if expr.tok == .key_none {
				return ast.Expr(ast.CastExpr{
					typ:  t.type_to_ast_type_expr(expected)
					expr: expr
					pos:  expr_pos
				})
			}
		}
		ast.Ident {
			if expr.name == 'none' {
				return ast.Expr(ast.CastExpr{
					typ:  t.type_to_ast_type_expr(expected)
					expr: expr
					pos:  expr_pos
				})
			}
		}
		ast.Type {
			if expr is ast.NoneType {
				return ast.Expr(ast.CastExpr{
					typ:  t.type_to_ast_type_expr(expected)
					expr: ast.Expr(ast.Type(expr))
					pos:  expr_pos
				})
			}
		}
		else {}
	}

	if expr_type := t.get_expr_type(expr) {
		if is_option {
			if expr_type is types.OptionType {
				return expr
			}
		} else {
			if expr_type is types.ResultType {
				return expr
			}
		}
	}
	return ast.Expr(ast.CastExpr{
		typ:  t.type_to_ast_type_expr(expected)
		expr: expr
		pos:  expr_pos
	})
}

fn (mut t Transformer) resolve_expr_with_expected_type(expr ast.Expr, expected types.Type) ast.Expr {
	base := t.unwrap_alias_and_pointer_type(expected)
	match expr {
		ast.ArrayInitExpr {
			if expr.typ is ast.EmptyExpr && (base is types.Array || base is types.ArrayFixed) {
				return ast.ArrayInitExpr{
					typ:         t.type_to_ast_type_expr(base)
					exprs:       expr.exprs
					init:        expr.init
					cap:         expr.cap
					len:         expr.len
					update_expr: expr.update_expr
					pos:         expr.pos
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
			return t.resolve_expr_with_expected_wrapper_type(expr, expected, true)
		}
		types.ResultType {
			return t.resolve_expr_with_expected_wrapper_type(expr, expected, false)
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
			return ast.Expr(ast.Type(ast.PointerType{
				base_type: t.type_to_ast_type_expr(typ.base_type)
				lifetime:  typ.lifetime
			}))
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
			type_name := t.type_to_c_name(typ)
			return ast.Expr(ast.Ident{
				name: type_name
			})
		}
	}
}

// get_error_wrapper_type returns the wrapper type name for IError interface methods.
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
	if typ := t.lookup_type(type_name) {
		if typ is types.Struct {
			for embedded in typ.embedded {
				embedded_name := if embedded.name.contains('__') {
					embedded.name.all_after_last('__')
				} else {
					embedded.name
				}
				if embedded_name == 'Error' {
					return type_name
				}
			}
		}
	}
	// No custom msg() method and no embedded Error - use Error's wrapper.
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
			if embedded.name == 'Error' || embedded.name.ends_with('.Error') {
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
	mut type_name := if unwrapped_expr is ast.Ident {
		t.get_var_type_name(unwrapped_expr.name)
	} else if unwrapped_expr is ast.SelectorExpr {
		t.get_selector_type_name(unwrapped_expr)
	} else {
		''
	}
	if type_name.len > 1024 || !transformer_string_has_valid_data(type_name) {
		return ''
	}

	// If scope lookup failed, try to get the type from the expression's position
	// This handles loop variables and other cases where the scope doesn't have the type
	if type_name == '' && expr_has_valid_data(unwrapped_expr) {
		if typ := t.env.get_expr_type(unwrapped_expr.pos().id) {
			type_name = typ.name()
			if type_name.len > 1024 || !transformer_string_has_valid_data(type_name) {
				return ''
			}
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
	if typ is ast.PointerType {
		base_name := t.type_expr_name(typ.base_type)
		return '${base_name}ptr'
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
		ast.PointerType { 'PointerType' }
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
	if typ is ast.PointerType {
		base_name := t.type_expr_name_full(typ.base_type)
		return '${base_name}ptr'
	}
	return t.type_expr_name_full(typ)
}

// type_expr_name extracts the short type name from a type expression
fn (t &Transformer) type_expr_name(expr ast.Expr) string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.LifetimeExpr {
		return '^' + expr.name
	}
	if expr is ast.SelectorExpr {
		// ast.Attribute -> 'Attribute' (use short name for matching)
		return t.selector_type_name(expr, false)
	}
	if expr is ast.Type {
		return t.type_variant_name(expr)
	}
	return ''
}

// type_expr_name_full extracts the full type name with module prefix (for C mangling)
fn (t &Transformer) type_expr_name_full(expr ast.Expr) string {
	if expr is ast.Ident {
		return t.qualify_type_name(expr.name)
	}
	if expr is ast.LifetimeExpr {
		return 'lt__' + expr.name
	}
	if expr is ast.SelectorExpr {
		// ast.Attribute -> 'ast__Attribute' (full name with module prefix for C)
		return t.selector_type_name(expr, true)
	}
	if expr is ast.Type {
		return t.type_variant_name(expr)
	}
	return ''
}

fn (t &Transformer) sumtype_expr_needs_variant_inference(value ast.Expr) bool {
	return value is ast.InitExpr || value is ast.ArrayInitExpr || value is ast.MapInitExpr
		|| value is ast.BasicLiteral || value is ast.StringLiteral
		|| value is ast.StringInterLiteral || value is ast.CastExpr || value is ast.AsCastExpr
}

fn (t &Transformer) pointer_type_name_base(type_name string) string {
	name := type_name.trim_space()
	if name == '' || name in ['voidptr', 'charptr', 'byteptr'] {
		return ''
	}
	if name.ends_with('*') {
		base_name := name.trim_right('*')
		if base_name != '' {
			return base_name
		}
	}
	if name.ends_with('ptr') {
		base_name := name[..name.len - 3]
		if base_name != '' {
			return base_name
		}
	}
	return ''
}

fn (t &Transformer) sumtype_pointer_base(type_name string) string {
	base_name := t.pointer_type_name_base(type_name)
	if t.is_sum_type(base_name) {
		return base_name
	}
	return ''
}

fn (t &Transformer) expr_type_name_hint(value ast.Expr) string {
	match value {
		ast.ParenExpr {
			return t.expr_type_name_hint(value.expr)
		}
		ast.ModifierExpr {
			return t.expr_type_name_hint(value.expr)
		}
		ast.CastExpr {
			return t.type_expr_to_c_name(value.typ)
		}
		ast.AsCastExpr {
			return t.type_expr_to_c_name(value.typ)
		}
		ast.PrefixExpr {
			if value.op == .mul {
				inner_type := t.expr_type_name_hint(value.expr)
				base_name := t.pointer_type_name_base(inner_type)
				if base_name != '' {
					return base_name
				}
			}
			return t.expr_type_name_hint(value.expr)
		}
		ast.CallOrCastExpr {
			if t.call_or_cast_lhs_is_type(value.lhs) {
				return t.type_expr_to_c_name(value.lhs)
			}
		}
		else {}
	}

	return ''
}

fn (t &Transformer) unwrap_addr_of_value_expr(value ast.Expr) ?ast.Expr {
	match value {
		ast.PrefixExpr {
			if value.op == .amp {
				return value.expr
			}
		}
		ast.ParenExpr {
			return t.unwrap_addr_of_value_expr(value.expr)
		}
		ast.ModifierExpr {
			return t.unwrap_addr_of_value_expr(value.expr)
		}
		ast.CastExpr {
			cast_type := t.type_expr_to_c_name(value.typ)
			base_name := t.pointer_type_name_base(cast_type)
			if base_name != '' {
				if inner_type := t.get_expr_type(value.expr) {
					inner_name := t.type_to_c_name(inner_type)
					if t.type_names_match(inner_name, base_name) {
						return value.expr
					}
				}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) expr_is_pointer_to_sumtype(value ast.Expr, sumtype_name string) bool {
	if typ := t.get_expr_type(value) {
		type_name := t.type_to_c_name(typ)
		base_name := t.pointer_type_name_base(type_name)
		if base_name != '' {
			return t.is_same_sumtype_name(base_name, sumtype_name)
		}
	}
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			type_name := t.type_to_c_name(var_type)
			base_name := t.pointer_type_name_base(type_name)
			if base_name != '' {
				return t.is_same_sumtype_name(base_name, sumtype_name)
			}
		}
	}
	return false
}

fn (mut t Transformer) wrap_array_push_elem_value(value ast.Expr, elem_type_name string) ast.Expr {
	if wrapped := t.wrap_sumtype_value_transformed(value, elem_type_name) {
		return wrapped
	}
	sumtype_name := t.sumtype_pointer_base(elem_type_name)
	if sumtype_name == '' {
		return value
	}
	if t.expr_is_pointer_to_sumtype(value, sumtype_name) {
		return value
	}
	value_expr := t.unwrap_addr_of_value_expr(value) or { value }
	wrapped_value := t.wrap_sumtype_value_transformed(value_expr, sumtype_name) or { return value }
	return ast.PrefixExpr{
		op:   .amp
		expr: wrapped_value
	}
}

fn (t &Transformer) generic_placeholder_from_type_expr(expr ast.Expr) ?string {
	match expr {
		ast.Ident {
			if expr.name in t.cur_fn_generic_params {
				return expr.name
			}
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					return t.generic_placeholder_from_type_expr(expr.elem_type)
				}
				ast.ArrayFixedType {
					return t.generic_placeholder_from_type_expr(expr.elem_type)
				}
				ast.MapType {
					if placeholder := t.generic_placeholder_from_type_expr(expr.value_type) {
						return placeholder
					}
					return t.generic_placeholder_from_type_expr(expr.key_type)
				}
				ast.OptionType {
					return t.generic_placeholder_from_type_expr(expr.base_type)
				}
				ast.ResultType {
					return t.generic_placeholder_from_type_expr(expr.base_type)
				}
				ast.PointerType {
					return t.generic_placeholder_from_type_expr(expr.base_type)
				}
				ast.GenericType {
					for param in expr.params {
						if placeholder := t.generic_placeholder_from_type_expr(param) {
							return placeholder
						}
					}
				}
				else {}
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) expr_uses_current_generic_param(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name in t.generic_var_type_params
		}
		ast.IndexExpr {
			return t.expr_uses_current_generic_param(expr.lhs)
		}
		ast.ParenExpr {
			return t.expr_uses_current_generic_param(expr.expr)
		}
		ast.ModifierExpr {
			return t.expr_uses_current_generic_param(expr.expr)
		}
		ast.CastExpr {
			if t.generic_placeholder_from_type_expr(expr.typ) != none {
				return true
			}
			return t.expr_uses_current_generic_param(expr.expr)
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) type_uses_current_generic_param(typ types.Type) bool {
	return t.type_uses_current_generic_param_inner(typ, 0)
}

fn (t &Transformer) type_uses_current_generic_param_inner(typ types.Type, depth int) bool {
	if depth > 32 || !types.type_has_valid_payload(typ) {
		return false
	}
	match typ {
		types.NamedType {
			name := string(typ)
			if !transformer_string_has_valid_data(name) {
				return false
			}
			for param in t.cur_fn_generic_params {
				if transformer_string_has_valid_data(param) && param == name {
					return true
				}
			}
			return false
		}
		types.Array {
			return t.type_uses_current_generic_param_inner(typ.elem_type, depth + 1)
		}
		types.ArrayFixed {
			return t.type_uses_current_generic_param_inner(typ.elem_type, depth + 1)
		}
		types.Map {
			return t.type_uses_current_generic_param_inner(typ.key_type, depth + 1)
				|| t.type_uses_current_generic_param_inner(typ.value_type, depth + 1)
		}
		types.OptionType {
			return t.type_uses_current_generic_param_inner(typ.base_type, depth + 1)
		}
		types.ResultType {
			return t.type_uses_current_generic_param_inner(typ.base_type, depth + 1)
		}
		types.Pointer {
			return t.type_uses_current_generic_param_inner(typ.base_type, depth + 1)
		}
		types.Alias {
			return t.type_uses_current_generic_param_inner(typ.base_type, depth + 1)
		}
		types.FnType {
			for param_type in typ.get_param_types() {
				if t.type_uses_current_generic_param_inner(param_type, depth + 1) {
					return true
				}
			}
			if ret_type := typ.get_return_type() {
				return t.type_uses_current_generic_param_inner(ret_type, depth + 1)
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) expr_uses_current_generic_type(expr ast.Expr) bool {
	if t.expr_uses_current_generic_param(expr) {
		return true
	}
	if typ := t.get_expr_type(expr) {
		return t.type_uses_current_generic_param(typ)
	}
	return false
}

// get_struct_field_type_name returns the type name of a field in a struct
fn (t &Transformer) init_expr_sumtype_variant_name(init_expr ast.InitExpr, variants []string, sumtype_name string) string {
	init_type_name := t.get_init_expr_type_name(init_expr.typ)
	if init_type_name == '' || t.is_same_sumtype_name(init_type_name, sumtype_name) {
		return ''
	}
	if variant_name := t.match_variant(init_type_name, variants) {
		return variant_name
	}
	if t.cur_module != '' && t.cur_module != 'main' && t.cur_module != 'builtin' {
		mangled := '${t.cur_module}__${init_type_name}'
		if variant_name := t.match_variant(mangled, variants) {
			return variant_name
		}
	}
	return ''
}

fn (mut t Transformer) wrap_sumtype_value(value ast.Expr, sumtype_name string) ?ast.Expr {
	info := t.sumtype_wrap_info_for_name(sumtype_name) or { return none }
	return t.wrap_sumtype_value_with_variants(value, info.name, info.variants)
}

fn (mut t Transformer) wrap_sumtype_value_with_variants(value ast.Expr, sumtype_name string, variants []string) ?ast.Expr {
	if t.expr_uses_current_generic_param(value) {
		return none
	}
	if variants.len == 0 {
		return none
	}
	mut init_variant_name := ''
	if value is ast.InitExpr {
		init_type_name := t.get_init_expr_type_name(value.typ)
		if t.is_same_sumtype_name(init_type_name, sumtype_name) {
			return none
		}
		init_variant_name = t.init_expr_sumtype_variant_name(value, variants, sumtype_name)
	}
	if declared_type := t.declared_expr_type_for_method_receiver(value) {
		declared_c_name := t.type_to_c_name(declared_type)
		if t.is_same_sumtype_name(declared_c_name, sumtype_name) && init_variant_name == '' {
			return none
		}
	}
	// Determine the variant type from the checker's type info
	typ := t.get_expr_type(value) or { return none }
	c_name := t.type_to_c_name(typ)
	input_is_target_sumtype := t.is_same_sumtype_name(c_name, sumtype_name)
	// If the value's type IS the target sum type, no wrapping needed —
	// the value is already the correct type (not a variant that needs wrapping).
	if input_is_target_sumtype && init_variant_name == ''
		&& !t.sumtype_expr_needs_variant_inference(value) {
		return none
	}
	// For Ident expressions, also check the variable's declared type.
	// In multi-variant match arms, the checker may narrow to the first variant
	// (e.g., Primitive) even though the runtime value could be any variant.
	// The declared variable type is more reliable in this case.
	if value is ast.Ident {
		if var_type := t.lookup_var_type(value.name) {
			var_c_name := t.type_to_c_name(var_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
		}
	}
	mut variant_name := init_variant_name
	if c_name != '' && c_name != 'void' && !input_is_target_sumtype {
		variant_name = t.match_variant(c_name, variants) or { '' }
	}
	// Fallback: use the type's V constructor name (e.g., types.Void → 'Void')
	// This handles cases where type_to_c_name returns a C name (like 'void', 'string')
	// that doesn't match the variant constructor name (like 'Void', 'String').
	if variant_name == '' && !input_is_target_sumtype {
		constructor_name := t.type_constructor_name(typ)
		if constructor_name != '' {
			variant_name = t.match_variant(constructor_name, variants) or { '' }
		}
	}
	if variant_name == '' {
		return none
	}
	// Transform the value then wrap
	transformed_value := t.transform_expr(value)
	return t.build_sumtype_init_with_variants(transformed_value, variant_name, sumtype_name,
		variants)
}

fn (mut t Transformer) transform_declared_sumtype_value(value ast.Expr, sumtype_name string) ?ast.Expr {
	declared_type := t.declared_expr_type_for_method_receiver(value) or { return none }
	declared_c_name := t.type_to_c_name(declared_type)
	if !t.is_same_sumtype_name(declared_c_name, sumtype_name) {
		return none
	}
	value_key := t.expr_to_string(value)
	if value_key != '' {
		if ctx := t.find_smartcast_for_expr(value_key) {
			if t.is_same_sumtype_name(ctx.sumtype, sumtype_name) {
				removed := t.remove_matching_smartcasts(ctx)
				transformed := t.transform_expr(value)
				t.restore_smartcasts(removed)
				return transformed
			}
		}
	}
	return t.transform_expr(value)
}

// wrap_sumtype_value_transformed wraps an already-transformed expression in sum type init
fn (mut t Transformer) wrap_sumtype_value_transformed(value ast.Expr, sumtype_name string) ?ast.Expr {
	info := t.sumtype_wrap_info_for_name(sumtype_name) or { return none }
	return t.wrap_sumtype_value_transformed_with_variants(value, info.name, info.variants)
}

fn (mut t Transformer) wrap_sumtype_value_transformed_with_variants(value ast.Expr, sumtype_name string, variants []string) ?ast.Expr {
	if t.expr_uses_current_generic_param(value) {
		return none
	}
	if variants.len == 0 {
		return none
	}
	mut init_variant_name := ''
	if value is ast.InitExpr {
		init_value := value as ast.InitExpr
		init_type_name := t.get_init_expr_type_name(init_value.typ)
		if t.is_same_sumtype_name(init_type_name, sumtype_name) {
			return none
		}
		init_variant_name = t.init_expr_sumtype_variant_name(init_value, variants, sumtype_name)
	}
	if declared_type := t.declared_expr_type_for_method_receiver(value) {
		declared_c_name := t.type_to_c_name(declared_type)
		if t.is_same_sumtype_name(declared_c_name, sumtype_name) && init_variant_name == '' {
			return none
		}
	}
	// For Ident expressions, check if the variable's declared type IS the target sum type.
	if value is ast.Ident {
		ident_value := value as ast.Ident
		if var_type := t.lookup_var_type(ident_value.name) {
			var_c_name := t.type_to_c_name(var_type)
			if t.is_same_sumtype_name(var_c_name, sumtype_name) {
				return none
			}
		}
	}
	mut variant_name := init_variant_name
	// Try checker-provided type first (works for original expressions with valid pos.id)
	mut input_is_target_sumtype2 := false
	if typ := t.get_expr_type(value) {
		c_name := t.type_to_c_name(typ)
		input_is_target_sumtype2 = t.is_same_sumtype_name(c_name, sumtype_name)
		// If the value's type IS the target sum type, no wrapping needed.
		if input_is_target_sumtype2 && init_variant_name == ''
			&& !t.sumtype_expr_needs_variant_inference(value) {
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
		if value is ast.InitExpr {
			init_value := value as ast.InitExpr
			variant_name = t.init_expr_sumtype_variant_name(init_value, variants, sumtype_name)
		}
		if variant_name == '' && value is ast.BasicLiteral {
			lit_value := value as ast.BasicLiteral
			if lit_value.kind == .number {
				variant_name = if lit_value.value.contains('.') {
					match_sumtype_variant_name('f64', variants)
				} else {
					match_sumtype_variant_name('int', variants)
				}
			} else if lit_value.kind == .string {
				variant_name = match_sumtype_variant_name('string', variants)
			}
		}
		if variant_name == '' && (value is ast.StringLiteral || value is ast.StringInterLiteral) {
			variant_name = match_sumtype_variant_name('string', variants)
		}
		if variant_name == '' && value is ast.Ident {
			ident_value := value as ast.Ident
			var_type_name := t.get_var_type_name(ident_value.name)
			if var_type_name != '' {
				variant_name = t.match_variant(var_type_name, variants) or { '' }
			}
		}
		if variant_name == '' && value is ast.CastExpr {
			cast_value := value as ast.CastExpr
			variant_name = t.match_variant(t.type_expr_name_full(cast_value.typ), variants) or {
				''
			}
		}
		if variant_name == '' && value is ast.AsCastExpr {
			as_value := value as ast.AsCastExpr
			variant_name = t.match_variant(t.type_expr_name_full(as_value.typ), variants) or { '' }
		}
		if variant_name == '' {
			hint_type := t.expr_type_name_hint(value)
			if hint_type != '' {
				variant_name = t.match_variant(hint_type, variants) or { '' }
			}
		}
		if variant_name == '' && value is ast.CallExpr {
			call_value := value as ast.CallExpr
			variant_name = match_sumtype_variant_name(t.get_call_return_type(ast.Expr(call_value)),
				variants)
		}
	}
	if variant_name == '' {
		return none
	}
	// Value is already transformed, just wrap it
	return t.build_sumtype_init_with_variants(value, variant_name, sumtype_name, variants)
}

// build_sumtype_init creates a sum type initialization expression
fn (t &Transformer) build_sumtype_init(transformed_value ast.Expr, variant_name string, sumtype_name string) ?ast.Expr {
	info := t.sumtype_wrap_info_for_name(sumtype_name) or { return none }
	return t.build_sumtype_init_with_variants(transformed_value, variant_name, info.name,
		info.variants)
}

fn (t &Transformer) build_sumtype_init_with_variants(transformed_value ast.Expr, variant_name string, sumtype_name string, variants []string) ?ast.Expr {
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
		is_direct_variant := t.sumtype_variant_init_data_is_direct(variant_name)
		if is_direct_variant {
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
			payload_type := if transformed_value is ast.InitExpr {
				transformed_value.typ
			} else {
				ast.Expr(ast.Ident{
					name: variant_name
				})
			}
			// Struct or string payloads must outlive the expression that created them.
			// Copy them before storing the payload pointer in the sumtype.
			ast.Expr(ast.CastExpr{
				typ:  ast.Ident{
					name: 'voidptr'
				}
				expr: ast.CallExpr{
					lhs:  ast.Expr(ast.Ident{
						name: 'memdup'
					})
					args: [
						ast.Expr(ast.PrefixExpr{
							op:   token.Token.amp
							expr: transformed_value
						}),
						ast.Expr(ast.KeywordOperator{
							op:    token.Token.key_sizeof
							exprs: [payload_type]
						}),
					]
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
	short_variant := if variant_name.starts_with('[]') {
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

fn match_sumtype_variant_name(candidate string, variants []string) string {
	if candidate.len == 0 {
		return ''
	}
	if candidate in variants {
		return candidate
	}
	for v in variants {
		if sum_type_variant_matches(v, candidate) {
			return v
		}
	}
	return ''
}

fn (t &Transformer) type_to_name(typ types.Type) string {
	match typ {
		types.Enum {
			return typ.name
		}
		types.Struct {
			return typ.name
		}
		types.Alias {
			return typ.name
		}
		types.NamedType {
			return string(typ)
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
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Void {
			return 'void'
		}
		types.Nil {
			return 'nil'
		}
		types.None {
			return 'none'
		}
		types.Map {
			// Convert Map type to 'Map_K_V' format
			key_type := t.type_to_name(typ.key_type)
			value_type := t.type_to_name(typ.value_type)
			if key_type != '' && value_type != '' {
				return 'Map_${key_type}_${value_type}'
			}
		}
		types.Primitive {
			return t.type_to_c_name(typ)
		}
		types.SumType {
			return types.sum_type_name(typ)
		}
		types.OptionType {
			return '_option_' + t.type_to_name(typ.base_type)
		}
		types.ResultType {
			return '_result_' + t.type_to_name(typ.base_type)
		}
		types.Pointer {
			return t.type_to_name(typ.base_type) + '*'
		}
		types.FnType {
			return 'FnType'
		}
		else {}
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
				if obj.name != '' {
					return obj.name
				}
				return name
			}
			return none
		}
	}
	// Fallback: check current module scope
	if t.cur_module != '' {
		if mut mod_scope := t.get_module_scope(t.cur_module) {
			if obj := mod_scope.lookup_parent(name, 0) {
				if obj is types.Module {
					if obj.name != '' {
						return obj.name
					}
					return name
				}
			}
		}
	}
	return none
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
fn sumtype_payload_word_is_valid(tag_word u64, data_word u64) bool {
	if data_word == 0 {
		return false
	}
	// Native v2 backends use `(tag, data_ptr)` for sumtypes. If the first word
	// looks like a small tag, the payload must be a real pointer, not a leaked
	// enum/default value like `3`.
	if tag_word < 256 {
		return transformer_data_ptr_has_valid_address(data_word)
	}
	return true
}

fn transformer_data_ptr_has_valid_address(ptr u64) bool {
	return ptr >= 0x10000 && ptr < 0x0000800000000000
}

fn stmt_has_valid_data(stmt ast.Stmt) bool {
	// On native v2 backends: word0 = tag, word1 = data pointer.
	// On C backend: word0 is pointer-like inline representation data.
	tag_word := unsafe { (&u64(&stmt))[0] }
	data_word := unsafe { (&u64(&stmt))[1] }
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

fn stmt_array_has_valid_data(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return true
	}
	if stmts.len < 0 || stmts.cap < stmts.len || stmts.len > 1_000_000 {
		return false
	}
	if stmts.data == unsafe { nil } {
		return false
	}
	data_word := u64(voidptr(stmts.data))
	return transformer_data_ptr_has_valid_address(data_word)
}

fn expr_has_valid_data(expr ast.Expr) bool {
	// On native v2 backends: word0 = tag, word1 = data pointer.
	// On C backend: word0 is pointer-like inline representation data.
	tag_word := unsafe { (&u64(&expr))[0] }
	data_word := unsafe { (&u64(&expr))[1] }
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

fn expr_has_valid_data_ref(expr &ast.Expr) bool {
	// Read from the caller-owned Expr storage. ARM64-generated code can
	// miscompile taking the address of a sumtype parameter by value.
	tag_word := unsafe { (&u64(expr))[0] }
	data_word := unsafe { (&u64(expr))[1] }
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

fn expr_array_has_valid_data(exprs []ast.Expr) bool {
	if exprs.len == 0 {
		return true
	}
	if exprs.len < 0 || exprs.cap < exprs.len || exprs.len > 1_000_000 {
		return false
	}
	if exprs.data == unsafe { nil } {
		return false
	}
	data_word := u64(voidptr(exprs.data))
	return transformer_data_ptr_has_valid_address(data_word)
}

// get_expr_type returns the types.Type for an expression by looking it up in the environment
fn (t &Transformer) get_expr_type(expr ast.Expr) ?types.Type {
	if expr is ast.StringLiteral && expr.kind == .c {
		if typ := types.builtin_type('charptr') {
			return typ
		}
	}
	// Handle specific expression types that may not have pos info or need special handling
	// These checks go BEFORE pos() to avoid dereferencing corrupt sum type data pointers
	// in ARM64-compiled binaries.
	if expr is ast.Ident {
		if typ := t.smartcast_type_for_expr(expr) {
			return typ
		}
		if typ := t.lookup_local_decl_type(expr.name) {
			return typ
		}
		if typ := t.lookup_var_type(expr.name) {
			return typ
		}
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		return none
	}
	// For UnsafeExpr, look at the type of the inner expression
	if expr is ast.UnsafeExpr {
		if typ := t.get_synth_type(expr.pos) {
			return typ
		}
		if expr.stmts.len > 0 {
			last := expr.stmts[expr.stmts.len - 1]
			if last is ast.ExprStmt {
				if typ := t.get_expr_type(last.expr) {
					return typ
				}
			}
		}
		if expr.pos.is_valid() {
			if typ := t.env.get_expr_type(expr.pos.id) {
				return t.normalize_type(typ)
			}
		}
		return none
	}
	// For CallExpr/CallOrCastExpr, try pos then function return type lookup
	if expr is ast.CallExpr {
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if typ := t.generic_call_concrete_return_type(expr) {
			return t.normalize_type(typ)
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		if typ := t.resolve_call_return_type(expr) {
			return t.normalize_type(typ)
		}
		return none
	}
	if expr is ast.CallOrCastExpr {
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if typ := t.generic_call_concrete_return_type(expr) {
			return t.normalize_type(typ)
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		if t.call_or_cast_lhs_is_type(expr.lhs) {
			if typ := t.type_from_param_type_expr(expr.lhs, []) {
				return t.normalize_type(typ)
			}
		}
		if typ := t.resolve_call_return_type(expr) {
			return t.normalize_type(typ)
		}
		return none
	}
	if expr is ast.CastExpr {
		if typ := t.type_from_param_type_expr(expr.typ, []) {
			return t.normalize_type(typ)
		}
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		return none
	}
	if expr is ast.AsCastExpr {
		if typ := t.type_from_param_type_expr(expr.typ, []) {
			return t.normalize_type(typ)
		}
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		return none
	}
	if expr is ast.PrefixExpr && expr.op == .mul {
		if typ := t.deref_address_of_cast_type(expr) {
			return t.normalize_type(typ)
		}
		if inner_typ := t.get_expr_type(expr.expr) {
			if inner_typ is types.Pointer {
				return t.normalize_type(inner_typ.base_type)
			}
		}
	}
	if expr is ast.ArrayInitExpr {
		pos := expr.pos
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if literal_type := t.get_array_init_expr_type(expr) {
			if expr.typ !is ast.EmptyExpr && !clone_type_contains_generic_placeholder(literal_type) {
				return literal_type
			}
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				concrete_type := concrete_literal_array_type(t.normalize_type(typ))
				if literal_type := t.get_array_init_expr_type(expr) {
					if array_type_has_float_elem(literal_type)
						&& !array_type_has_float_elem(concrete_type) {
						return literal_type
					}
				}
				return concrete_type
			}
		}
		return t.get_array_init_expr_type(expr)
	}
	if expr is ast.InitExpr {
		if typ := t.type_from_init_expr(expr) {
			return typ
		}
	}
	if expr is ast.SelectorExpr {
		if typ := t.smartcast_type_for_expr(expr) {
			return typ
		}
		if expr.rhs.name == 'name' && expr.lhs is ast.Ident
			&& (expr.lhs.name in t.cur_fn_generic_params
			|| expr.lhs.name in t.generic_var_type_params) {
			return types.Type(types.string_)
		}
		pos := expr.pos
		if lhs_type := t.get_expr_type(expr.lhs) {
			if field_typ := t.field_type_from_receiver_type(lhs_type, expr.rhs.name) {
				return t.normalize_type(substitute_type(field_typ, t.cur_monomorphized_fn_bindings))
			}
		}
		if typ := t.get_synth_type(pos) {
			return typ
		}
		if pos.is_valid() {
			if typ := t.env.get_expr_type(pos.id) {
				return t.normalize_type(typ)
			}
		}
		return none
	}
	if expr is ast.IndexExpr {
		if expr.expr is ast.RangeExpr {
			if lhs_type := t.get_expr_type(expr.lhs) {
				return t.normalize_type(lhs_type)
			}
		}
	}
	if expr is ast.IndexExpr && expr.expr !is ast.RangeExpr {
		if lhs_type := t.map_index_lhs_type(expr.lhs) {
			if map_type := t.unwrap_map_type(lhs_type) {
				return map_type.value_type
			}
			mut base_type := lhs_type
			for {
				if base_type is types.Pointer {
					base_type = base_type.base_type
					continue
				}
				if base_type is types.Alias {
					base_type = t.live_alias_base_type(base_type) or { break }
					continue
				}
				break
			}
			if base_type is types.Array {
				return t.normalize_type(base_type.elem_type)
			}
			if base_type is types.ArrayFixed {
				return t.normalize_type(base_type.elem_type)
			}
			if t.is_string_iterable_type(base_type) {
				return string_iter_value_type()
			}
		}
	}
	// For other expression types, use the generic pos() dispatch
	// Guard against corrupt sum type data in ARM64-compiled binaries
	if !expr_has_valid_data(expr) {
		return none
	}
	pos := expr.pos()
	if typ := t.get_synth_type(pos) {
		return typ
	}
	if pos.is_valid() {
		if typ := t.env.get_expr_type(pos.id) {
			return t.normalize_type(typ)
		}
	}
	return none
}

fn (t &Transformer) declared_expr_type_for_method_receiver(expr ast.Expr) ?types.Type {
	return t.declared_expr_type_for_method_receiver_inner(expr, t.expr_to_string(expr))
}

fn (t &Transformer) declared_expr_type_for_method_receiver_inner(expr ast.Expr, ignored_smartcast_expr string) ?types.Type {
	expr_str := t.expr_to_string(expr)
	if expr_str != '' && expr_str != ignored_smartcast_expr {
		if ctx := t.find_smartcast_for_expr(expr_str) {
			if ctx.variant_full != '' {
				if typ := t.c_name_to_type(ctx.variant_full) {
					return typ
				}
			}
			if ctx.variant != '' {
				if typ := t.c_name_to_type(ctx.variant) {
					return typ
				}
			}
		}
	}
	if expr is ast.Ident {
		if typ := t.lookup_local_decl_type(expr.name) {
			return typ
		}
		if typ := t.lookup_var_type(expr.name) {
			return typ
		}
		return t.get_expr_type(expr)
	}
	if expr is ast.SelectorExpr {
		if lhs_type := t.declared_expr_type_for_method_receiver_inner(expr.lhs,
			ignored_smartcast_expr)
		{
			if field_typ := t.field_type_from_receiver_type(lhs_type, expr.rhs.name) {
				return substitute_type(field_typ, t.cur_monomorphized_fn_bindings)
			}
		}
		return t.get_expr_type(expr)
	}
	if expr is ast.IndexExpr && expr.expr !is ast.RangeExpr {
		if lhs_type := t.declared_expr_type_for_method_receiver_inner(expr.lhs,
			ignored_smartcast_expr)
		{
			mut base_type := lhs_type
			for {
				if base_type is types.Pointer {
					base_type = base_type.base_type
					continue
				}
				if base_type is types.Alias {
					base_type = base_type.base_type
					continue
				}
				break
			}
			if base_type is types.Array {
				return base_type.elem_type
			}
			if base_type is types.ArrayFixed {
				return base_type.elem_type
			}
			if base_type is types.Map {
				return base_type.value_type
			}
			if t.is_string_iterable_type(base_type) {
				return string_iter_value_type()
			}
		}
		return t.get_expr_type(expr)
	}
	if expr is ast.ParenExpr {
		return t.declared_expr_type_for_method_receiver_inner(expr.expr, ignored_smartcast_expr)
	}
	if expr is ast.ModifierExpr {
		return t.declared_expr_type_for_method_receiver_inner(expr.expr, ignored_smartcast_expr)
	}
	return t.get_expr_type(expr)
}

fn (t &Transformer) get_array_init_expr_type(expr ast.ArrayInitExpr) ?types.Type {
	is_fixed := array_init_has_fixed_len_marker(expr)
	if expr.typ !is ast.EmptyExpr {
		if typ := t.type_from_param_type_expr(expr.typ, []) {
			if is_fixed {
				base := t.unwrap_alias_and_pointer_type(typ)
				if base is types.Array {
					return types.Type(types.ArrayFixed{
						len:       expr.exprs.len
						elem_type: base.elem_type
					})
				}
			}
			return typ
		}
	}
	for elem in expr.exprs {
		if typ := t.get_array_init_elem_expr_type(elem) {
			if is_fixed {
				return types.Type(types.ArrayFixed{
					len:       expr.exprs.len
					elem_type: typ
				})
			}
			return types.Type(types.Array{
				elem_type: typ
			})
		}
	}
	return none
}

fn (t &Transformer) get_array_init_elem_expr_type(expr ast.Expr) ?types.Type {
	match expr {
		ast.ArrayInitExpr {
			return t.get_array_init_expr_type(expr)
		}
		ast.StringLiteral, ast.StringInterLiteral {
			return types.Type(types.string_)
		}
		ast.BasicLiteral {
			match expr.kind {
				.string {
					return types.Type(types.string_)
				}
				.key_true, .key_false {
					return types.Type(types.bool_)
				}
				.number {
					if expr.value.contains('.') || expr.value.contains('e')
						|| expr.value.contains('E') {
						return types.Type(types.f64_)
					}
					return types.Type(types.int_)
				}
				.char {
					return types.Type(types.int_)
				}
				else {}
			}
		}
		else {}
	}

	if typ := t.get_expr_type(expr) {
		return concrete_literal_array_elem_type(typ)
	}

	return none
}

fn concrete_literal_array_elem_type(typ types.Type) types.Type {
	if typ is types.Primitive && typ.props.has(.untyped) {
		if typ.props.has(.float) {
			return types.Type(types.f64_)
		}
		return types.Type(types.int_)
	}
	return typ
}

fn normalize_generic_concrete_type(typ types.Type) types.Type {
	if typ is types.Primitive && typ.props.has(.untyped) {
		if typ.props.has(.float) {
			return types.Type(types.f64_)
		}
		return types.Type(types.int_)
	}
	return match typ.name() {
		'int_literal' {
			types.Type(types.int_)
		}
		'float_literal' {
			types.Type(types.f64_)
		}
		else {
			typ
		}
	}
}

fn array_type_has_float_elem(typ types.Type) bool {
	elem_type := if typ is types.Array {
		typ.elem_type
	} else if typ is types.ArrayFixed {
		typ.elem_type
	} else {
		return false
	}
	return elem_type is types.Primitive && elem_type.props.has(.float)
}

fn concrete_literal_array_type(typ types.Type) types.Type {
	if typ is types.Array {
		return types.Type(types.Array{
			elem_type: concrete_literal_array_elem_type(typ.elem_type)
		})
	}
	if typ is types.ArrayFixed {
		return types.Type(types.ArrayFixed{
			len:       typ.len
			elem_type: concrete_literal_array_elem_type(typ.elem_type)
		})
	}
	return typ
}

fn array_init_has_fixed_len_marker(expr ast.ArrayInitExpr) bool {
	if expr.len is ast.PostfixExpr {
		postfix := expr.len as ast.PostfixExpr
		return postfix.op == .not && postfix.expr is ast.EmptyExpr
	}
	return false
}

fn sanitize_generic_token_part(name string) string {
	if name == '' || name.len > 1024 || !transformer_string_has_valid_data(name) {
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
	normalized := normalize_generic_concrete_type(typ)
	if normalized.name() != typ.name() {
		return t.generic_specialization_token_from_type(normalized)
	}
	match normalized {
		types.Array {
			return 'Array_' + t.generic_specialization_token_from_type(normalized.elem_type)
		}
		types.ArrayFixed {
			return 'Array_fixed_' + t.generic_specialization_token_from_type(normalized.elem_type) +
				'_' + normalized.len.str()
		}
		types.Map {
			return 'Map_' + t.generic_specialization_token_from_type(normalized.key_type) + '_' +
				t.generic_specialization_token_from_type(normalized.value_type)
		}
		types.OptionType {
			return 'Option_' + t.generic_specialization_token_from_type(normalized.base_type)
		}
		types.ResultType {
			return 'Result_' + t.generic_specialization_token_from_type(normalized.base_type)
		}
		types.Pointer {
			return t.generic_specialization_token_from_type(normalized.base_type) + 'ptr'
		}
		types.Alias {
			if normalized.name != '' {
				return sanitize_generic_token_part(normalized.name)
			}
			return t.generic_specialization_token_from_type(normalized.base_type)
		}
		else {
			if !transformer_type_has_safe_payload(normalized) {
				return 'Type'
			}
			type_name := normalized.name()
			if type_name == '' || type_name.len > 1024
				|| !transformer_string_has_valid_data(type_name) {
				return 'Type'
			}
			return sanitize_generic_token_part(type_name)
		}
	}
}

fn (t &Transformer) generic_specialization_suffix_from_bindings(generic_params []string, bindings map[string]types.Type) string {
	if generic_params.len == 0 {
		return ''
	}
	mut all_placeholders := true
	mut parts := []string{cap: generic_params.len}
	for param_name in generic_params {
		concrete := bindings[param_name] or { return '' }
		parts << t.generic_specialization_token_from_type(concrete)
		if concrete.name() != param_name {
			all_placeholders = false
		}
	}
	if parts.len == 0 {
		return ''
	}
	if all_placeholders {
		return '_' + generic_params.join('_')
	}
	return '_T_' + parts.join('_')
}

fn (t &Transformer) generic_specialization_token(expr ast.Expr) string {
	if expr is ast.Ident {
		if concrete := t.cur_monomorphized_fn_bindings[expr.name] {
			return t.generic_specialization_token_from_type(concrete)
		}
	}
	if concrete := t.get_expr_type(expr) {
		return t.generic_specialization_token_from_type(concrete)
	}
	match expr {
		ast.Ident {
			return sanitize_generic_token_part(expr.name)
		}
		ast.SelectorExpr {
			return sanitize_generic_token_part(expr.name())
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return t.generic_specialization_token(expr.expr) + 'ptr'
			}
			return sanitize_generic_token_part(ast.Expr(expr).name())
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					return 'Array_' + t.generic_specialization_token(expr.elem_type)
				}
				ast.ArrayFixedType {
					mut len_str := '0'
					if expr.len is ast.BasicLiteral {
						len_str = expr.len.value
					}
					return 'Array_fixed_' + t.generic_specialization_token(expr.elem_type) +
						'_${len_str}'
				}
				ast.MapType {
					return 'Map_' + t.generic_specialization_token(expr.key_type) + '_' +
						t.generic_specialization_token(expr.value_type)
				}
				ast.OptionType {
					return 'Option_' + t.generic_specialization_token(expr.base_type)
				}
				ast.ResultType {
					return 'Result_' + t.generic_specialization_token(expr.base_type)
				}
				ast.PointerType {
					return t.generic_specialization_token(expr.base_type) + 'ptr'
				}
				else {
					type_name := t.expr_to_type_name(ast.Expr(expr))
					if type_name != '' {
						return sanitize_generic_token_part(type_name)
					}
				}
			}

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
		if typ is ast.PointerType {
			return t.get_receiver_type_name(typ.base_type)
		}
		if typ is ast.GenericType {
			// Type[T] -> Type
			return t.get_receiver_type_name(typ.name)
		}
	}
	return ''
}

fn (t &Transformer) get_receiver_type_name_cursor(typ ast.Cursor) string {
	if !typ.is_valid() {
		return ''
	}
	match typ.kind() {
		.expr_ident {
			return typ.name()
		}
		.expr_prefix {
			op := unsafe { token.Token(int(typ.aux())) }
			if op == .amp {
				return t.get_receiver_type_name_cursor(typ.edge(0))
			}
		}
		.expr_modifier, .typ_pointer, .typ_generic {
			return t.get_receiver_type_name_cursor(typ.edge(0))
		}
		.expr_selector {
			rhs := typ.edge(1)
			if rhs.is_valid() {
				return rhs.name()
			}
		}
		else {}
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
			base_type := t.unwrap_alias_and_pointer_type(typ)
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
		}
		if typ := t.get_expr_type(expr) {
			base_type := t.unwrap_alias_and_pointer_type(typ)
			if base_type is types.Array {
				elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
				return t.normalize_literal_type(elem_name)
			}
			if base_type is types.ArrayFixed {
				elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
				return t.normalize_literal_type(elem_name)
			}
		}
		// Fallback to string-based detection
		var_type_name := t.get_var_type_name(expr.name)
		if var_type_name != '' {
			if resolved_type := t.lookup_type(var_type_name) {
				base_type := t.unwrap_alias_and_pointer_type(resolved_type)
				if base_type is types.Array {
					elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
					return t.normalize_literal_type(elem_name)
				}
				if base_type is types.ArrayFixed {
					elem_name := t.array_elem_type_name_for_helpers(base_type.elem_type)
					return t.normalize_literal_type(elem_name)
				}
			}
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
			base_type := t.unwrap_alias_and_pointer_type(field_type)
			if base_type is types.Array {
				return t.array_elem_type_name_for_helpers(base_type.elem_type)
			}
			if base_type is types.ArrayFixed {
				return t.array_elem_type_name_for_helpers(base_type.elem_type)
			}
		}
	}
	// Handle IndexExpr - map lookup that returns an array (e.g., g.pending_labels[blk])
	if expr is ast.IndexExpr {
		// Check if this is a map lookup returning an array
		map_expr_type := t.get_expr_type(expr.lhs) or { return none }
		if map_type := t.unwrap_map_type(map_expr_type) {
			value_type := t.unwrap_alias_and_pointer_type(map_type.value_type)
			if value_type is types.Array {
				return t.array_elem_type_name_for_helpers(value_type.elem_type)
			}
			if value_type is types.ArrayFixed {
				return t.array_elem_type_name_for_helpers(value_type.elem_type)
			}
		}
	}
	// Also try getting from types.Environment
	typ := t.get_expr_type(expr) or { return none }
	base_typ := t.unwrap_alias_and_pointer_type(typ)
	if base_typ is types.Array {
		return t.array_elem_type_name_for_helpers(base_typ.elem_type)
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
	if name.contains('.') {
		name = name.replace('.', '__')
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

fn (t &Transformer) array_init_value_elem_type(expr ast.ArrayInitExpr) ?string {
	if expr.exprs.len == 0 {
		return none
	}
	first_expr := expr.exprs[0]
	if first_expr is ast.ArrayInitExpr {
		if inner_elem := t.array_value_elem_type(first_expr) {
			return 'Array_${inner_elem}'
		}
	}
	if typ := t.get_expr_type(first_expr) {
		type_name := t.type_to_c_name(typ)
		if type_name != '' && type_name != 'void' {
			return t.normalize_literal_type(type_name)
		}
	}
	match first_expr {
		ast.StringLiteral {
			return 'string'
		}
		ast.BasicLiteral {
			if first_expr.kind == .number {
				return 'int'
			}
		}
		else {}
	}

	return none
}

fn (t &Transformer) append_rhs_is_array_value_compatible(lhs_elem string, rhs ast.Expr) bool {
	rhs_elem_type := t.array_value_elem_type(rhs) or {
		lhs_is_nested_array := lhs_elem.starts_with('Array_')
			|| lhs_elem.starts_with('Array_fixed_')
		return t.is_array_value_expr(rhs) && !lhs_is_nested_array
	}
	if !t.array_elem_types_compatible(lhs_elem, rhs_elem_type) {
		return false
	}
	if rhs is ast.ArrayInitExpr {
		if literal_elem_type := t.array_init_value_elem_type(rhs) {
			return t.array_elem_types_compatible(lhs_elem, literal_elem_type)
		}
	}
	return true
}

fn (t &Transformer) single_nested_array_append_value(rhs ast.Expr, lhs_elem string) ?ast.Expr {
	if !(lhs_elem.starts_with('Array_') || lhs_elem.starts_with('Array_fixed_')) {
		return none
	}
	if rhs !is ast.ArrayInitExpr {
		return none
	}
	literal := rhs as ast.ArrayInitExpr
	if literal.exprs.len != 1 {
		return none
	}
	value := literal.exprs[0]
	value_type := t.get_expr_type(value) or { return none }
	value_type_name := t.type_to_c_name_resolve_alias(value_type)
	if !t.array_elem_types_compatible(lhs_elem, value_type_name) {
		return none
	}
	return value
}

fn (t &Transformer) array_value_elem_type(expr ast.Expr) ?string {
	if elem_type := t.get_array_elem_type_str(expr) {
		return elem_type
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
	recv_type := t.get_expr_type(expr) or { return none }
	base := t.unwrap_alias_and_pointer_type(recv_type)
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
	mut seen := map[string]bool{}
	for {
		if !types.type_has_valid_payload(cur) {
			break
		}
		if cur is types.Pointer {
			ptr := cur as types.Pointer
			cur = ptr.base_type
			continue
		}
		if cur is types.Alias {
			key := 'alias:${cur.name}'
			if key in seen {
				break
			}
			seen[key] = true
			cur = t.live_alias_base_type(cur) or { break }
			continue
		}
		if cur is types.NamedType {
			name := string(cur)
			key := 'named:${name}'
			if key in seen {
				break
			}
			seen[key] = true
			if resolved := t.lookup_type(name) {
				cur = resolved
				continue
			}
		}
		break
	}
	return cur
}

fn (t &Transformer) unwrap_alias_type(typ types.Type) types.Type {
	mut cur := typ
	mut seen := map[string]bool{}
	for {
		if cur is types.Alias {
			key := 'alias:${cur.name}'
			if key in seen {
				break
			}
			seen[key] = true
			cur = t.live_alias_base_type(cur) or { break }
			continue
		}
		if cur is types.NamedType {
			name := string(cur)
			key := 'named:${name}'
			if key in seen {
				break
			}
			seen[key] = true
			if resolved := t.lookup_type(name) {
				cur = resolved
				continue
			}
		}
		break
	}
	return cur
}

fn (t &Transformer) sumtype_variant_data_is_direct(typ types.Type) bool {
	base := t.unwrap_alias_type(typ)
	return match base {
		types.Primitive, types.Char, types.Enum, types.FnType, types.Interface, types.ISize,
		types.Nil, types.None, types.Pointer, types.Rune, types.USize, types.Void {
			true
		}
		else {
			false
		}
	}
}

fn (t &Transformer) sumtype_variant_init_data_is_direct(variant_name string) bool {
	if variant_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'bool', 'rune',
		'byte', 'usize', 'isize', 'char', 'void', 'nil', 'none', 'byteptr', 'charptr', 'voidptr'] {
		return true
	}
	variant_type := t.c_name_to_type(variant_name) or { return false }
	return t.type_fits_inline_sumtype_data(variant_type, variant_name)
}

fn (t &Transformer) type_fits_inline_sumtype_data(typ types.Type, variant_name string) bool {
	match typ {
		types.Alias {
			base := t.live_alias_base_type(typ) or { return false }
			return t.type_fits_inline_sumtype_data(base, variant_name)
		}
		types.Primitive, types.Char, types.Enum, types.ISize, types.Nil, types.None, types.Rune,
		types.USize, types.Void {
			return true
		}
		types.Pointer {
			return variant_name.ends_with('ptr')
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) smartcast_variant_data_is_direct(ctx SmartcastContext) bool {
	if ctx.variant_full != '' {
		if typ := t.lookup_type(ctx.variant_full) {
			return t.sumtype_variant_data_is_direct(typ)
		}
	}
	if ctx.variant != '' {
		if typ := t.lookup_type(ctx.variant) {
			return t.sumtype_variant_data_is_direct(typ)
		}
	}
	return false
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
	if !types.type_has_valid_payload(typ) {
		return ''
	}
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
		base := t.live_alias_base_type(typ) or { return alias_name }
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
		if t.is_enum_type_name(type_name) {
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
			if t.is_enum_type_name(field_type) {
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
			if t.is_enum_type_name(field_type_via_var) {
				return field_type_via_var
			}
			// Fall back to type-based lookup
			lhs_type := t.get_var_type_name(lhs_ident.name)
			if lhs_type != '' {
				field_type := t.resolve_field_type(lhs_type, expr.rhs.name)
				if t.is_enum_type_name(field_type) {
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

fn generic_type_param_name(name string) bool {
	return name in ['T', 'U', 'V', 'K', 'W']
}

fn (t &Transformer) is_enum_type_name(type_name string) bool {
	if type_name == '' || generic_type_param_name(type_name) {
		return false
	}
	if typ := t.lookup_type(type_name) {
		return typ is types.Enum
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
	if !is_type_valid(typ) {
		return false
	}
	match typ {
		types.Pointer {
			return true
		}
		types.Alias {
			if !is_type_valid(typ.base_type) {
				if resolved := t.lookup_type(typ.name) {
					if resolved is types.Alias {
						if is_type_valid(resolved.base_type) {
							return t.is_pointer_type(resolved.base_type)
						}
						return false
					}
					return t.is_pointer_type(resolved)
				}
				return false
			}
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
		if expr.name == t.cur_fn_recv_param && t.cur_fn_recv_is_ptr {
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

fn (t &Transformer) array_append_lhs_uses_local_array_storage(expr ast.Expr) bool {
	if expr is ast.ParenExpr {
		return t.array_append_lhs_uses_local_array_storage(expr.expr)
	}
	if expr is ast.ModifierExpr {
		return t.array_append_lhs_uses_local_array_storage(expr.expr)
	}
	if expr !is ast.Ident {
		return false
	}
	ident := expr as ast.Ident
	decl_type := t.active_local_decl_type_for_expr(ast.Expr(ident)) or { return false }
	if decl_type !is types.Array {
		return false
	}
	lookup_type := t.lookup_var_type(ident.name) or { return false }
	return lookup_type is types.Pointer && lookup_type.base_type is types.Array
}

// get_str_fn_name_for_type returns the str function name for a types.Type
fn (t &Transformer) get_str_fn_name_for_type(typ types.Type) ?string {
	if !types.type_has_valid_payload(typ) {
		return none
	}
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
			if !transformer_string_has_valid_data(typ.name) {
				return none
			}
			return '${typ.name}__str'
		}
		types.SumType {
			return '${t.type_to_c_name(typ)}__str'
		}
		types.Enum {
			if !transformer_string_has_valid_data(typ.name) {
				return none
			}
			return '${typ.name}__str'
		}
		types.Rune {
			return 'rune__str'
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
					8 { return 'u8__str' }
					16 { return 'u16__str' }
					32 { return 'u32__str' }
					64 { return 'u64__str' }
					else { return 'int__str' }
				}
			}
			match typ.size {
				8 { return 'i8__str' }
				16 { return 'i16__str' }
				32, 0 { return 'int__str' }
				64 { return 'i64__str' }
				else { return 'int__str' }
			}
		}
		types.Pointer {
			if !types.type_has_valid_payload(typ.base_type) {
				return none
			}
			// For pointers, use the base type's str function
			return t.get_str_fn_name_for_type(typ.base_type)
		}
		types.Alias {
			// Recurse to base type — aliases to primitives (e.g., ValueID = int)
			// don't have their own str() functions.
			base_type := t.live_alias_base_type(typ) or { return none }
			return t.get_str_fn_name_for_type(base_type)
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

fn typeof_type_idx(type_name string) int {
	return match type_name {
		'void' { 0 }
		'voidptr' { 1 }
		'byteptr' { 2 }
		'charptr' { 3 }
		'i8' { 4 }
		'i16' { 5 }
		'i32' { 6 }
		'int' { 7 }
		'i64' { 8 }
		'isize' { 9 }
		'u8', 'byte' { 10 }
		'u16' { 11 }
		'u32' { 12 }
		'u64' { 13 }
		'usize' { 14 }
		'f32' { 15 }
		'f64' { 16 }
		'char' { 17 }
		'bool' { 18 }
		'none' { 19 }
		'string' { 20 }
		'rune' { 21 }
		else { 0 }
	}
}

fn typeof_idx_literal(type_name string, pos token.Pos) ast.BasicLiteral {
	return ast.BasicLiteral{
		value: typeof_type_idx(type_name).str()
		kind:  .number
		pos:   pos
	}
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
			if typ.lifetime != '' {
				return '&^${typ.lifetime} ' + base
			}
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
		types.NamedType {
			return c_name_to_v_name(string(typ))
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
