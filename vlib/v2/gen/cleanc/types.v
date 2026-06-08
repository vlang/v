// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types
import strings

fn (g &Gen) result_value_type(result_type string) string {
	if !result_type.starts_with('_result_') {
		return ''
	}
	payload := result_type['_result_'.len..]
	if payload.contains('_T_') {
		if ptr_base := g.result_payload_generic_struct_pointer_base(payload) {
			return '${ptr_base}*'
		}
		// Generic specialization names can end in `ptr` because a generic
		// argument is a pointer, while the payload itself is still the
		// specialized typedef (for example Foo_T_Barptr).
		return payload
	}
	return unmangle_c_ptr_type(payload)
}

fn (g &Gen) result_payload_generic_struct_pointer_base(payload string) ?string {
	if !payload.ends_with('ptr') {
		return none
	}
	if g.generic_struct_instance_c_name_exists(payload) {
		return none
	}
	base := payload[..payload.len - 3]
	if g.generic_struct_instance_c_name_exists(base) {
		return base
	}
	return none
}

fn (g &Gen) generic_struct_instance_c_name_exists(c_name string) bool {
	for _, instances in g.generic_struct_instances {
		for inst in instances {
			if inst.c_name == c_name {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) result_value_c_type(result_type string) string {
	return g.option_result_payload_c_type(g.result_value_type(result_type))
}

fn option_value_type(option_type string) string {
	if !option_type.starts_with('_option_') {
		return ''
	}
	return unmangle_c_ptr_type(option_type['_option_'.len..])
}

fn is_ierror_interface_name(name string) bool {
	return name == 'IError' || name == 'builtin__IError' || name.ends_with('__IError')
}

fn wrapper_alias_shadowed_by_qualified_payload(name string, prefix string, aliases map[string]bool) bool {
	if !name.starts_with(prefix) {
		return false
	}
	payload := name[prefix.len..]
	if payload == '' || payload.contains('__') || payload.starts_with('struct_') {
		return false
	}
	suffix := '__${payload}'
	for other, _ in aliases {
		if other == name || !other.starts_with(prefix) {
			continue
		}
		other_payload := other[prefix.len..]
		if other_payload.contains('__') && other_payload.ends_with(suffix) {
			return true
		}
	}
	return false
}

fn (g &Gen) should_skip_shadowed_option_result_alias(name string) bool {
	if name.starts_with('_option_') {
		return wrapper_alias_shadowed_by_qualified_payload(name, '_option_', g.option_aliases)
	}
	if name.starts_with('_result_') {
		return wrapper_alias_shadowed_by_qualified_payload(name, '_result_', g.result_aliases)
	}
	return false
}

fn (g &Gen) option_result_payload_alias_component(base_type string) string {
	base := base_type.trim_space()
	if base == 'void' || base.ends_with('__void') {
		return 'void'
	}
	if base == 'voidptr' || base == 'void*' || base.ends_with('__voidptr')
		|| base.ends_with('__void*') {
		return 'voidptr'
	}
	if base.contains('__') {
		short := base.all_after_last('__')
		if short in primitive_types || short in ['bool', 'string', 'rune'] {
			return short
		}
	}
	if base.starts_with('struct ') {
		return 'struct_' + mangle_alias_component(base['struct '.len..].trim_space())
	}
	if !base.contains('__') && base in g.c_struct_types && base !in g.typedef_c_types {
		return 'struct_' + mangle_alias_component(base)
	}
	return mangle_alias_component(base)
}

fn c_struct_decl_c_name(name string) string {
	if name.starts_with('C.') {
		return name['C.'.len..]
	}
	return name
}

fn overloaded_arithmetic_method_part(node ast.InfixExpr) string {
	return match node.op {
		.plus { 'op_plus' }
		.minus { 'op_minus' }
		.mul { 'op_mul' }
		.div { 'op_div' }
		.mod { 'op_mod' }
		else { '' }
	}
}

fn overloaded_arithmetic_operand_value_type(typ string) string {
	mut t := typ.trim_space()
	if t.ends_with('*') {
		t = t[..t.len - 1].trim_space()
	}
	if t == '' || t == 'void' || t in primitive_types || t == 'string' || t.ends_with('*')
		|| t.ends_with('ptr') {
		return ''
	}
	return t
}

fn (mut g Gen) overloaded_arithmetic_method_for_infix(node ast.InfixExpr, lhs_type string, rhs_type string) (string, string) {
	op_name := overloaded_arithmetic_method_part(node)
	if op_name == '' {
		return '', ''
	}
	lhs_value_type := overloaded_arithmetic_operand_value_type(lhs_type)
	rhs_value_type := overloaded_arithmetic_operand_value_type(rhs_type)
	if lhs_value_type == '' || lhs_value_type != rhs_value_type {
		return '', ''
	}
	method_fn := '${lhs_value_type}__${op_name}'
	if method_fn !in g.fn_return_types && method_fn !in g.fn_param_is_ptr {
		return '', ''
	}
	return method_fn, lhs_value_type
}

fn (mut g Gen) overloaded_arithmetic_result_type(node ast.InfixExpr) string {
	lhs_type := g.get_expr_type(node.lhs)
	rhs_type := g.get_expr_type(node.rhs)
	method_fn, operand_type := g.overloaded_arithmetic_method_for_infix(node, lhs_type, rhs_type)
	if method_fn == '' {
		return ''
	}
	if ret := g.fn_return_types[method_fn] {
		return ret
	}
	return operand_type
}

fn (mut g Gen) overloaded_arithmetic_arg_is_pointer(expr ast.Expr, operand_type string) bool {
	unwrapped := g.unwrap_parens(expr)
	if unwrapped is ast.CallExpr {
		if ret := g.get_call_return_type(unwrapped.lhs, unwrapped.args) {
			return ret.ends_with('*')
				&& overloaded_arithmetic_operand_value_type(ret) == operand_type
		}
		return false
	}
	if unwrapped is ast.InfixExpr {
		return false
	}
	return g.expr_produces_pointer(expr)
}

fn (mut g Gen) gen_overloaded_arithmetic_arg(expr ast.Expr, expr_type string, operand_type string) {
	unwrapped := g.unwrap_parens(expr)
	if g.overloaded_arithmetic_arg_is_pointer(expr, operand_type)
		&& expr_type.trim_space().ends_with('*')
		&& overloaded_arithmetic_operand_value_type(expr_type) == operand_type {
		g.sb.write_string('(*')
		g.expr(expr)
		g.sb.write_string(')')
		return
	}
	if unwrapped is ast.Ident && unwrapped.name in g.cur_fn_mut_params {
		g.sb.write_string('*')
		g.expr(expr)
		return
	}
	g.expr(expr)
}

fn (g &Gen) resolve_active_generic_type(name string) ?types.Type {
	if name == '' || g.active_generic_types.len == 0 {
		return none
	}
	if concrete := g.active_generic_types[name] {
		if concrete.name() == name || type_contains_generic_placeholder(concrete) {
			return none
		}
		return concrete
	}
	return none
}

fn (g &Gen) concrete_c_type_from_active_generic(typ types.Type) ?string {
	match typ {
		types.NamedType {
			name := string(typ)
			if is_generic_placeholder_type_name(name) {
				concrete := g.resolve_active_generic_type(name) or { return none }
				return g.types_type_to_c(concrete)
			}
		}
		types.Pointer {
			base := g.concrete_c_type_from_active_generic(typ.base_type) or { return none }
			return base + '*'
		}
		types.Array {
			elem := g.concrete_c_type_from_active_generic(typ.elem_type) or { return none }
			return 'Array_' + mangle_alias_component(elem)
		}
		types.ArrayFixed {
			elem := g.concrete_c_type_from_active_generic(typ.elem_type) or { return none }
			return 'Array_fixed_' + mangle_alias_component(elem) + '_' + typ.len.str()
		}
		types.Map {
			key := g.concrete_c_type_from_active_generic(typ.key_type) or {
				g.types_type_to_c(typ.key_type)
			}
			val := g.concrete_c_type_from_active_generic(typ.value_type) or {
				g.types_type_to_c(typ.value_type)
			}
			return 'Map_' + mangle_alias_component(key) + '_' + mangle_alias_component(val)
		}
		types.OptionType {
			base := g.concrete_c_type_from_active_generic(typ.base_type) or { return none }
			return '_option_' + g.option_result_payload_alias_component(base)
		}
		types.ResultType {
			base := g.concrete_c_type_from_active_generic(typ.base_type) or { return none }
			return '_result_' + g.option_result_payload_alias_component(base)
		}
		else {}
	}

	return none
}

fn (g &Gen) index_elem_c_type_from_raw(typ types.Type) ?string {
	match typ {
		types.Array {
			return g.types_type_to_c(typ.elem_type)
		}
		types.ArrayFixed {
			return g.types_type_to_c(typ.elem_type)
		}
		types.Map {
			return g.types_type_to_c(typ.value_type)
		}
		types.Pointer {
			return g.index_elem_c_type_from_raw(typ.base_type)
		}
		types.Alias {
			return g.index_elem_c_type_from_raw(typ.base_type)
		}
		else {}
	}

	return none
}

fn type_contains_generic_placeholder(typ types.Type) bool {
	mut seen := map[string]bool{}
	return type_contains_generic_placeholder_inner(typ, mut seen)
}

fn type_contains_generic_placeholder_inner(typ types.Type, mut seen map[string]bool) bool {
	if !types.type_has_valid_payload(typ) {
		return false
	}
	match typ {
		types.NamedType {
			return is_generic_placeholder_type_name(string(typ))
		}
		types.Array {
			return type_contains_generic_placeholder_inner(typ.elem_type, mut seen)
		}
		types.ArrayFixed {
			return type_contains_generic_placeholder_inner(typ.elem_type, mut seen)
		}
		types.Map {
			return type_contains_generic_placeholder_inner(typ.key_type, mut seen)
				|| type_contains_generic_placeholder_inner(typ.value_type, mut seen)
		}
		types.OptionType {
			return type_contains_generic_placeholder_inner(typ.base_type, mut seen)
		}
		types.ResultType {
			return type_contains_generic_placeholder_inner(typ.base_type, mut seen)
		}
		types.Pointer {
			return type_contains_generic_placeholder_inner(typ.base_type, mut seen)
		}
		types.Alias {
			key := 'alias:${typ.name}'
			if key in seen {
				return false
			}
			seen[key] = true
			return type_contains_generic_placeholder_inner(typ.base_type, mut seen)
		}
		types.FnType {
			for param_type in typ.get_param_types() {
				if type_contains_generic_placeholder_inner(param_type, mut seen) {
					return true
				}
			}
			if ret := typ.get_return_type() {
				return type_contains_generic_placeholder_inner(ret, mut seen)
			}
			return false
		}
		types.Struct {
			key := 'struct:${typ.name}'
			if typ.name != '' {
				if key in seen {
					return false
				}
				seen[key] = true
			}
			if typ.generic_params.len == 0 || typ.name.contains('_T_') {
				return false
			}
			for field in typ.fields {
				if type_contains_generic_placeholder_inner(field.typ, mut seen) {
					return true
				}
			}
			for embedded in typ.embedded {
				if type_contains_generic_placeholder_inner(types.Type(embedded), mut seen) {
					return true
				}
			}
			if typ.fields.len == 0 && typ.embedded.len == 0 {
				for param_name in typ.generic_params {
					if !param_name.starts_with('^') && is_generic_placeholder_type_name(param_name) {
						return true
					}
				}
			}
			return false
		}
		else {
			return false
		}
	}
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

fn (g &Gen) generic_specialization_token_from_type(typ types.Type) string {
	normalized := normalize_generic_concrete_type(typ)
	if normalized.name() != typ.name() {
		return g.generic_specialization_token_from_type(normalized)
	}
	match typ {
		types.Array {
			return 'Array_' + g.generic_specialization_token_from_type(typ.elem_type)
		}
		types.ArrayFixed {
			return 'Array_fixed_' + g.generic_specialization_token_from_type(typ.elem_type) + '_' +
				typ.len.str()
		}
		types.Map {
			return 'Map_' + g.generic_specialization_token_from_type(typ.key_type) + '_' +
				g.generic_specialization_token_from_type(typ.value_type)
		}
		types.OptionType {
			return 'Option_' + g.generic_specialization_token_from_type(typ.base_type)
		}
		types.ResultType {
			return 'Result_' + g.generic_specialization_token_from_type(typ.base_type)
		}
		types.Pointer {
			return g.generic_specialization_token_from_type(typ.base_type) + 'ptr'
		}
		types.Alias {
			if typ.name != '' {
				return sanitize_generic_token_part(typ.name)
			}
			return g.generic_specialization_token_from_type(typ.base_type)
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

fn normalize_generic_concrete_type(typ types.Type) types.Type {
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

fn generic_concrete_type_is_comptime_metadata(typ types.Type) bool {
	if !type_has_valid_data(typ) {
		return true
	}
	normalized := normalize_generic_concrete_type(typ)
	match normalized {
		types.NamedType {
			return string(normalized) == 'Type'
		}
		types.Struct {
			return normalized.name in ['Type', '__type_info', '__field_info', '__method_info',
				'__function_param_info', '__enum_value_info']
		}
		types.Array {
			return generic_concrete_type_is_comptime_metadata(normalized.elem_type)
		}
		types.ArrayFixed {
			return generic_concrete_type_is_comptime_metadata(normalized.elem_type)
		}
		types.Map {
			return generic_concrete_type_is_comptime_metadata(normalized.key_type)
				|| generic_concrete_type_is_comptime_metadata(normalized.value_type)
		}
		types.OptionType {
			return generic_concrete_type_is_comptime_metadata(normalized.base_type)
		}
		types.ResultType {
			return generic_concrete_type_is_comptime_metadata(normalized.base_type)
		}
		types.Pointer {
			return generic_concrete_type_is_comptime_metadata(normalized.base_type)
		}
		else {
			return false
		}
	}
}

fn generic_concrete_type_is_runtime_specializable(typ types.Type) bool {
	if !type_has_valid_data(typ) || typ is types.Void || typ is types.None || typ is types.Nil {
		return false
	}
	concrete := normalize_generic_concrete_type(typ)
	if concrete.name() == '' || type_contains_generic_placeholder(concrete) {
		return false
	}
	return !generic_concrete_type_is_comptime_metadata(concrete)
}

// unmangle_c_ptr_type converts mangled pointer type names back to C pointer types.
// e.g. FILEptr -> FILE*, voidptr -> void*, charptr -> char*
// Preserves composite type names (Array_, Map_, _option_, _result_) that happen to end in 'ptr'.
fn unmangle_c_ptr_type(name string) string {
	if name == 'voidptr' {
		return 'void*'
	}
	if name == 'charptr' {
		return 'char*'
	}
	if name == 'byteptr' {
		return 'u8*'
	}
	if name.contains('_T_') {
		return name
	}
	if name.len > 3 && name.ends_with('ptr') {
		// Don't unmangle composite type names - they are typedef'd aliases
		if name.starts_with('Array_') || name.starts_with('Map_') || name.starts_with('Tuple_')
			|| name.starts_with('_option_') || name.starts_with('_result_') {
			return name
		}
		base := name[..name.len - 3]
		// Any mangled pointer type (created by mangle_alias_component replacing * with ptr)
		// should be unmangled back to base*
		return '${base}*'
	}
	return name
}

fn unmangle_alias_component_to_c(name string) string {
	if name.starts_with('Array_') || name.starts_with('Array_fixed_') || name.starts_with('Map_')
		|| name.starts_with('_option_') || name.starts_with('_result_') {
		return name
	}
	return unmangle_c_ptr_type(name)
}

fn is_builtin_specialization_base(name string) bool {
	return name in primitive_types || name in ['string', 'chan', 'Error', 'IError']
}

fn (g &Gen) normalize_builtin_qualified_c_type(type_name string) string {
	mut base := type_name.trim_space()
	if base == '' {
		return type_name
	}
	mut suffix := ''
	for base.ends_with('*') {
		suffix += '*'
		base = base[..base.len - 1].trim_space()
	}
	if !base.contains('__') {
		return type_name
	}
	mod_name := base.all_before_last('__')
	short_name := base.all_after_last('__')
	if is_builtin_specialization_base(short_name)
		&& !g.c_type_name_declared_in_module(mod_name, short_name) {
		return short_name + suffix
	}
	return type_name
}

fn (g &Gen) c_type_from_possible_specialization_token(name string) string {
	if name == '' || name.starts_with('Array_') || name.starts_with('Array_fixed_')
		|| name.starts_with('Map_') || name.starts_with('_option_') || name.starts_with('_result_') {
		return name
	}
	if name == 'voidptr' || name == 'charptr' || name == 'byteptr' {
		return unmangle_c_ptr_type(name)
	}
	// Don't strip `_ptr` suffix from user-declared fn type aliases like
	// `Fn_sqlite3_syscall_ptr` — they are real identifiers, not mangled pointers.
	if name in g.fn_type_aliases {
		return name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_type_aliases {
			return qualified
		}
	}
	mut base := name
	mut ptr_suffix := ''
	for base.len > 3 && base.ends_with('ptr') {
		ptr_suffix += '*'
		base = base[..base.len - 3]
	}
	resolved_base := g.c_name_from_specialization_token_base(base)
	if ptr_suffix != '' {
		return resolved_base + ptr_suffix
	}
	return resolved_base
}

fn (g &Gen) should_map_vec_generic_to_simd(lhs_name string) bool {
	base_name := if lhs_name.contains('__') { lhs_name.all_after_last('__') } else { lhs_name }
	if base_name !in ['Vec2', 'Vec4'] {
		return false
	}
	// The math.vec module defines Vec2/Vec4 as ordinary generic structs.
	// SIMD aliases are emitted for explicit aliases like `type SimdFloat4 = vec.Vec4[f32]`;
	// mapping the module's own Vec2/Vec4 definitions changes their function bodies.
	return g.cur_module != 'vec'
}

fn (g &Gen) vec_suffix_alias_declared(mapped string) bool {
	if mapped.contains('__') {
		return g.module_declares_type(mapped.all_before_last('__'), mapped.all_after_last('__'))
	}
	module_name := if g.cur_module == '' { 'main' } else { g.cur_module }
	return g.module_declares_type(module_name, mapped)
}

fn (g &Gen) vec_type_suffix_alias_to_c(name string) ?string {
	if g.cur_module == 'vec' && name in ['Vec2', 'Vec4', 'vec__Vec2', 'vec__Vec4'] {
		return none
	}
	if name.ends_with('Vec4') {
		mut mapped := name[..name.len - 'Vec4'.len] + 'SimdFloat4'
		if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
			&& g.cur_module != 'builtin' {
			mapped = '${g.cur_module}__${mapped}'
		}
		if g.vec_suffix_alias_declared(mapped) {
			return mapped
		}
		return none
	}
	if name.ends_with('Vec2') {
		mut mapped := name[..name.len - 'Vec2'.len] + 'SimdFloat2'
		if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
			&& g.cur_module != 'builtin' {
			mapped = '${g.cur_module}__${mapped}'
		}
		if g.vec_suffix_alias_declared(mapped) {
			return mapped
		}
		return none
	}
	return none
}

fn vec_simd_alias_name(base_name string, arg_type string) string {
	if base_name == 'Vec4' {
		return if arg_type.starts_with('u') {
			'SimdU32_4'
		} else if arg_type.starts_with('f') {
			'SimdFloat4'
		} else {
			'SimdInt4'
		}
	}
	if base_name == 'Vec2' {
		return if arg_type.starts_with('u') {
			'SimdUint2'
		} else if arg_type.starts_with('f') {
			'SimdFloat2'
		} else {
			'SimdI32_2'
		}
	}
	return ''
}

fn (g &Gen) module_declares_type(module_name string, type_name string) bool {
	if module_name == '' || type_name == '' {
		return false
	}
	qualified := type_decl_name_in_module(type_name, module_name)
	if type_decl_module_key(module_name, qualified) in g.declared_type_names_by_mod {
		return true
	}
	return g.c_type_name_declared_in_module(module_name, type_name)
}

fn (g &Gen) local_vec_simd_alias_to_c(base_name string, arg_type string) ?string {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin'
		|| g.cur_module == 'vec' {
		return none
	}
	alias_name := vec_simd_alias_name(base_name, arg_type)
	if alias_name == '' || !g.module_declares_type(g.cur_module, alias_name) {
		return none
	}
	return '${g.cur_module}__${alias_name}'
}

fn (mut g Gen) generic_placeholder_c_type_from_name(name string) ?string {
	if name == '' {
		return none
	}
	mut ptr_suffix := ''
	mut base := name.trim_space()
	for base.ends_with('*') {
		ptr_suffix += '*'
		base = base[..base.len - 1].trim_space()
	}
	if base.contains('__') {
		base = base.all_after_last('__')
	}
	if !is_generic_placeholder_type_name(base) {
		return none
	}
	if concrete := g.resolve_active_generic_type(base) {
		return g.types_type_to_c(concrete) + ptr_suffix
	}
	return 'f64' + ptr_suffix
}

fn map_key_type_candidates() []string {
	return ['string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune',
		'char', 'bool', 'f32', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr']
}

fn (mut g Gen) infer_map_type_info_from_alias_name(map_name string) ?MapTypeInfo {
	if !map_name.starts_with('Map_') || map_name.len <= 'Map_'.len {
		return none
	}
	rest := map_name['Map_'.len..]
	if rest.starts_with('Array_fixed_') {
		mut best_key := ''
		mut best_value := ''
		for i := 1; i < rest.len; i++ {
			if rest[i] != `_` {
				continue
			}
			key := rest[..i]
			value := rest[i + 1..]
			if value == '' {
				continue
			}
			_, arr_len := g.parse_fixed_array_type(key)
			if arr_len > 0 {
				best_key = key
				best_value = value
			}
		}
		if best_key != '' {
			info := MapTypeInfo{
				key_c_type:   best_key
				value_c_type: unmangle_alias_component_to_c(best_value)
			}
			g.collected_map_types[map_name] = info
			return info
		}
	}
	mut key_types := map_key_type_candidates()
	for _, info in g.collected_map_types {
		if info.key_c_type != '' && info.key_c_type !in key_types {
			key_types << info.key_c_type
		}
	}
	for key_c in key_types {
		key_alias := mangle_alias_component(key_c)
		prefix := key_alias + '_'
		if !rest.starts_with(prefix) {
			continue
		}
		val_alias := rest[prefix.len..]
		if val_alias == '' {
			continue
		}
		info := MapTypeInfo{
			key_c_type:   key_c
			value_c_type: unmangle_alias_component_to_c(val_alias)
		}
		g.collected_map_types[map_name] = info
		return info
	}
	sep := rest.index('_') or { return none }
	val_alias := rest[sep + 1..]
	if val_alias == '' {
		return none
	}
	info := MapTypeInfo{
		key_c_type:   unmangle_alias_component_to_c(rest[..sep])
		value_c_type: unmangle_alias_component_to_c(val_alias)
	}
	g.collected_map_types[map_name] = info
	return info
}

fn (mut g Gen) ensure_map_type_info(map_name string) ?MapTypeInfo {
	if info := g.collected_map_types[map_name] {
		return info
	}
	return g.infer_map_type_info_from_alias_name(map_name)
}

// collect_typedef_c_types scans all files for @[typedef] C struct declarations
// and records their names. This must run before any type resolution so that
// expr_type_to_c can emit these types without a 'struct' prefix.
fn (mut g Gen) collect_typedef_c_types() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			stmts := g.flat.file_cursor(i).stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_struct_decl {
					continue
				}
				decl := stmt.struct_decl()
				if decl.language == .c {
					c_name := c_struct_decl_c_name(decl.name)
					g.c_struct_types[c_name] = true
					if decl.attributes.has('typedef') {
						g.typedef_c_types[c_name] = true
					}
				}
			}
		}
		return
	}
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language == .c {
					c_name := c_struct_decl_c_name(stmt.name)
					g.c_struct_types[c_name] = true
					if stmt.attributes.has('typedef') {
						g.typedef_c_types[c_name] = true
					}
				}
			}
		}
	}
}

fn (mut g Gen) collect_module_type_names() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			module_name := flat_file_module_name(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_struct_decl {
						decl := stmt.struct_decl()
						g.record_declared_type_name(decl.name, module_name)
						if decl.language != .v {
							continue
						}
						struct_name := g.get_struct_name(decl)
						for field in decl.fields {
							field_type := g.expr_type_to_c(field.typ)
							full_key := decl.name + '.' + field.name
							struct_key := struct_name + '.' + field.name
							g.struct_field_types[full_key] = field_type
							g.struct_field_types[struct_key] = field_type
							if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
								fixed_typ := field.typ as ast.ArrayFixedType
								elem_type := g.expr_type_to_c(fixed_typ.elem_type)
								g.fixed_array_fields[full_key] = true
								g.fixed_array_fields[struct_key] = true
								g.fixed_array_field_elem[full_key] = elem_type
								g.fixed_array_field_elem[struct_key] = elem_type
							}
						}
						for emb in decl.embedded {
							emb_c_type := g.expr_type_to_c(emb)
							emb_name := embedded_owner_field_name(emb_c_type)
							if emb_name == '' {
								continue
							}
							full_key := decl.name + '.' + emb_name
							struct_key := struct_name + '.' + emb_name
							if full_key !in g.struct_field_types {
								g.struct_field_types[full_key] = emb_c_type
							}
							if struct_key !in g.struct_field_types {
								g.struct_field_types[struct_key] = emb_c_type
							}
						}
					}
					.stmt_enum_decl {
						decl := stmt.enum_decl(false)
						g.record_declared_type_name(decl.name, module_name)
						enum_name := g.get_enum_name(decl)
						if is_generic_placeholder_c_type_name(enum_name) {
							continue
						}
						mut fields := map[string]bool{}
						if enum_name in g.enum_type_fields {
							fields = g.enum_type_fields[enum_name].clone()
						}
						for field in decl.fields {
							if field.name !in g.enum_value_to_enum {
								g.enum_value_to_enum[field.name] = enum_name
							}
							fields[field.name] = true
						}
						mut cloned_fields := fields.clone()
						g.enum_type_fields[enum_name] = cloned_fields.move()
						if enum_name.contains('__') {
							short_name := enum_name.all_after_last('__')
							mut short_cloned_fields := fields.clone()
							g.enum_type_fields[short_name] = short_cloned_fields.move()
						}
					}
					.stmt_type_decl {
						decl := stmt.type_decl()
						g.record_declared_type_name(decl.name, module_name)
						if decl.language == .c {
							continue
						}
					}
					.stmt_interface_decl {
						decl := stmt.interface_decl()
						g.record_declared_type_name(decl.name, module_name)
					}
					else {}
				}
			}
		}
		return
	}
	for file in g.files {
		g.set_file_module(file)
		module_name := file_module_name(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			match stmt {
				ast.StructDecl {
					g.record_declared_type_name(stmt.name, module_name)
					if stmt.language != .v {
						continue
					}
					struct_name := g.get_struct_name(stmt)
					for field in stmt.fields {
						field_type := g.expr_type_to_c(field.typ)
						full_key := stmt.name + '.' + field.name
						struct_key := struct_name + '.' + field.name
						g.struct_field_types[full_key] = field_type
						g.struct_field_types[struct_key] = field_type
						if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
							fixed_typ := field.typ as ast.ArrayFixedType
							elem_type := g.expr_type_to_c(fixed_typ.elem_type)
							g.fixed_array_fields[full_key] = true
							g.fixed_array_fields[struct_key] = true
							g.fixed_array_field_elem[full_key] = elem_type
							g.fixed_array_field_elem[struct_key] = elem_type
						}
					}
					// Register each embedded struct as a synthetic field under the
					// outer struct so `outer.EmbedName` (explicit embed access) can
					// be resolved by `lookup_struct_field_type_by_name`. Without
					// this, type inference for `outer.EmbedName.f` falls back to
					// `int`, which breaks if-expression temp typing among others.
					for emb in stmt.embedded {
						emb_c_type := g.expr_type_to_c(emb)
						emb_name := embedded_owner_field_name(emb_c_type)
						if emb_name == '' {
							continue
						}
						full_key := stmt.name + '.' + emb_name
						struct_key := struct_name + '.' + emb_name
						if full_key !in g.struct_field_types {
							g.struct_field_types[full_key] = emb_c_type
						}
						if struct_key !in g.struct_field_types {
							g.struct_field_types[struct_key] = emb_c_type
						}
					}
				}
				ast.EnumDecl {
					g.record_declared_type_name(stmt.name, module_name)
					enum_name := g.get_enum_name(stmt)
					if is_generic_placeholder_c_type_name(enum_name) {
						continue
					}
					mut fields := map[string]bool{}
					if enum_name in g.enum_type_fields {
						fields = g.enum_type_fields[enum_name].clone()
					}
					for field in stmt.fields {
						if field.name !in g.enum_value_to_enum {
							g.enum_value_to_enum[field.name] = enum_name
						}
						fields[field.name] = true
					}
					mut cloned_fields := fields.clone()
					g.enum_type_fields[enum_name] = cloned_fields.move()
					if enum_name.contains('__') {
						short_name := enum_name.all_after_last('__')
						mut short_cloned_fields := fields.clone()
						g.enum_type_fields[short_name] = short_cloned_fields.move()
					}
				}
				ast.TypeDecl {
					g.record_declared_type_name(stmt.name, module_name)
					if stmt.language == .c {
						continue
					}
				}
				ast.InterfaceDecl {
					g.record_declared_type_name(stmt.name, module_name)
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) record_declared_type_name(decl_name string, module_name string) {
	if decl_name == '' {
		return
	}
	alias_name := type_decl_name_in_module(decl_name, module_name)
	g.declared_type_names_all[alias_name] = true
	g.declared_type_names_by_mod[type_decl_module_key(module_name, alias_name)] = true
	if (module_name == '' || module_name == 'main' || module_name == 'builtin')
		&& decl_name != alias_name {
		g.declared_type_names_all[decl_name] = true
		g.declared_type_names_by_mod[type_decl_module_key(module_name, decl_name)] = true
	}
}

fn (mut g Gen) collect_runtime_aliases() {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_fn_decl {
						// Type aliases live only in the signature (receiver /
						// param / return types); decode the body-less signature
						// so cleanc's type-alias collection never rehydrates a fn
						// body on the default build path.
						g.collect_decl_type_aliases_from_stmt(ast.Stmt(stmt.fn_decl_signature()))
					}
					.stmt_struct_decl, .stmt_interface_decl, .stmt_type_decl, .stmt_global_decl {
						g.collect_decl_type_aliases_from_stmt(stmt.stmt())
					}
					else {}
				}
			}
		}
	} else {
		for file in g.files {
			g.set_file_module(file)
			for stmt in file.stmts {
				g.collect_decl_type_aliases_from_stmt(stmt)
			}
		}
	}
	// Also use type-checker output so aliases used only in expressions are captured.
	if g.env != unsafe { nil } {
		for _, typ in g.env.expr_types {
			if !type_has_valid_data(typ) || typ is types.Void {
				continue
			}
			if typ is types.Alias {
				continue
			}
			g.collect_aliases_from_type(typ)
		}
	}
}

fn tuple_payload_type_name(ret_type string) string {
	if ret_type.starts_with('_option_') {
		payload := option_value_type(ret_type)
		if payload.starts_with('Tuple_') {
			return payload
		}
		return ''
	}
	if ret_type.starts_with('_result_') {
		payload := result_type_value_name(ret_type)
		if payload.starts_with('Tuple_') {
			return payload
		}
		return ''
	}
	if ret_type.starts_with('Tuple_') {
		return ret_type
	}
	return ''
}

fn result_type_value_name(result_type string) string {
	if !result_type.starts_with('_result_') {
		return ''
	}
	return unmangle_c_ptr_type(result_type['_result_'.len..])
}

fn normalize_signature_type_name(typ string, fallback string) string {
	if typ.trim_space() == '' {
		return fallback
	}
	return typ
}

fn (mut g Gen) collect_aliases_from_type(t types.Type) {
	if !type_has_valid_data(t) {
		return
	}
	match t {
		types.Array {
			g.collect_aliases_from_type(t.elem_type)
			elem := mangle_alias_component(g.types_type_to_c(t.elem_type))
			g.register_alias_type('Array_' + elem)
		}
		types.ArrayFixed {
			g.collect_aliases_from_type(t.elem_type)
			elem := mangle_alias_component(g.types_type_to_c(t.elem_type))
			fixed_name := 'Array_fixed_' + elem + '_' + t.len.str()
			g.register_alias_type(fixed_name)
			g.collected_fixed_array_types[fixed_name] = FixedArrayInfo{
				elem_type: g.types_type_to_c(t.elem_type)
				size:      t.len
			}
		}
		types.Map {
			g.collect_aliases_from_type(t.key_type)
			g.collect_aliases_from_type(t.value_type)
			key := mangle_alias_component(g.types_type_to_c(t.key_type))
			val := mangle_alias_component(g.types_type_to_c(t.value_type))
			map_name := 'Map_' + key + '_' + val
			g.register_alias_type(map_name)
			g.collected_map_types[map_name] = MapTypeInfo{
				key_c_type:   g.types_type_to_c(t.key_type)
				value_c_type: g.types_type_to_c(t.value_type)
			}
		}
		types.OptionType {
			g.collect_aliases_from_type(t.base_type)
			base := g.option_result_payload_alias_component(g.types_type_to_c(t.base_type))
			g.register_alias_type('_option_' + base)
		}
		types.ResultType {
			g.collect_aliases_from_type(t.base_type)
			base := g.option_result_payload_alias_component(g.types_type_to_c(t.base_type))
			g.register_alias_type('_result_' + base)
		}
		types.Alias {
			// Alias payloads from self-host env caches can be malformed.
			// Decls are collected from AST, so skip runtime alias recursion here.
			return
		}
		types.Pointer {
			g.collect_aliases_from_type(t.base_type)
		}
		else {}
	}
}

fn (mut g Gen) collect_decl_type_aliases_from_stmt(stmt ast.Stmt) {
	if !stmt_has_valid_data(stmt) {
		return
	}
	match stmt {
		ast.StructDecl {
			if stmt.language == .c {
				return
			}
			for emb in stmt.embedded {
				_ = g.expr_type_to_c(emb)
			}
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		ast.InterfaceDecl {
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		ast.TypeDecl {
			if stmt.language == .c {
				return
			}
			if stmt.base_type !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.base_type)
			}
			for variant in stmt.variants {
				_ = g.expr_type_to_c(variant)
			}
		}
		ast.FnDecl {
			if stmt.language == .js {
				return
			}
			if stmt.language == .c && stmt.stmts.len == 0 {
				return
			}
			if g.generic_fn_param_names(stmt).len > 0 {
				return
			}
			if receiver_generic_param_names(stmt).len > 0 {
				return
			}
			if stmt.is_method && stmt.receiver.typ !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.receiver.typ)
			}
			for param in stmt.typ.params {
				_ = g.expr_type_to_c(param.typ)
			}
			if stmt.typ.return_type !is ast.EmptyExpr {
				_ = g.expr_type_to_c(stmt.typ.return_type)
			}
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				if field.typ !is ast.EmptyExpr {
					_ = g.expr_type_to_c(field.typ)
				}
			}
		}
		else {}
	}
}

fn is_generic_placeholder_type_name(name string) bool {
	return name in ['T', 'K', 'V'] || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`)
}

fn (mut g Gen) type_expr_has_metadata(expr ast.Expr) bool {
	if _ := g.get_expr_type_from_env(expr) {
		return true
	}
	if g.env != unsafe { nil } && expr_has_valid_data(expr) {
		pos := expr.pos()
		if pos.id != 0 {
			if typ := g.env.get_expr_type(pos.id) {
				return type_has_valid_data(typ)
			}
		}
	}
	return false
}

fn (g &Gen) qualify_module_local_type_name(type_name string) string {
	mut mod_name := g.cur_module
	if (mod_name == '' || mod_name == 'main') && g.cur_fn_c_name.contains('__') {
		fn_prefix := g.cur_fn_c_name.all_before('__')
		if !('body_${fn_prefix}' in g.emitted_types || 'enum_${fn_prefix}' in g.emitted_types
			|| 'alias_${fn_prefix}' in g.emitted_types) {
			mod_name = fn_prefix
		}
	}
	if type_name == '' || type_name.contains('__') || type_name.starts_with('Array_')
		|| type_name.starts_with('Map_') || type_name.starts_with('_option_')
		|| type_name.starts_with('_result_') || type_name in g.c_struct_types
		|| type_name in g.typedef_c_types || is_known_external_c_type_name(type_name)
		|| mod_name == '' || mod_name == 'main' || mod_name == 'builtin' {
		return type_name
	}
	qualified := '${mod_name}__${type_name}'
	if 'body_${qualified}' in g.emitted_types || 'enum_${qualified}' in g.emitted_types
		|| 'alias_${qualified}' in g.emitted_types || '${qualified}__msg' in g.fn_return_types {
		return qualified
	}
	if 'body_${qualified}' in g.pending_late_body_keys {
		return qualified
	}
	if mut module_scope := g.env_scope(mod_name) {
		if _ := module_scope.lookup_type(type_name) {
			return qualified
		}
	}
	if obj := g.lookup_module_scope_object(type_name) {
		if obj is types.Type {
			return qualified
		}
	}
	if g.cur_fn_c_name.contains('__') && !type_name.contains(' ')
		&& !type_name.starts_with('struct ') {
		return qualified
	}
	return type_name
}

fn (g &Gen) qualify_module_local_generic_c_name(raw_name string) string {
	mut name := raw_name.trim_space()
	if name == '' {
		return name
	}
	mut suffix := ''
	for name.ends_with('*') {
		name = name[..name.len - 1].trim_space()
		suffix += '*'
	}
	if name.starts_with('struct ') {
		name = name['struct '.len..].trim_space()
	}
	mut qualified := name
	if name.starts_with('Array_fixed_') {
		rest := name['Array_fixed_'.len..]
		last_underscore := rest.last_index_u8(`_`)
		if last_underscore >= 0 {
			elem :=
				g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(rest[..last_underscore]))
			qualified = 'Array_fixed_' + mangle_alias_component(elem) + '_' + rest[last_underscore +
				1..]
		}
	} else if name.starts_with('Array_') {
		elem :=
			g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(name['Array_'.len..]))
		qualified = 'Array_' + mangle_alias_component(elem)
	} else if name.starts_with('Map_') {
		if info := g.collected_map_types[name] {
			if info.key_c_type != '' && info.value_c_type != '' {
				qualified = 'Map_' + mangle_alias_component(info.key_c_type) + '_' +
					mangle_alias_component(info.value_c_type)
			}
		} else {
			rest := name['Map_'.len..]
			key, value := g.parse_map_kv_types(rest)
			if key != '' && value != '' {
				key_name :=
					g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(key))
				value_name :=
					g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(value))
				qualified = 'Map_' + mangle_alias_component(key_name) + '_' +
					mangle_alias_component(value_name)
			}
		}
	} else if name.starts_with('Tuple_') {
		mut parts := []string{}
		for part in name['Tuple_'.len..].split('_') {
			if part == '' {
				continue
			}
			part_name := g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(part))
			parts << mangle_alias_component(part_name)
		}
		qualified = 'Tuple_' + parts.join('_')
	} else if name.starts_with('_option_') {
		base :=
			g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(name['_option_'.len..]))
		qualified = '_option_' + g.option_result_payload_alias_component(base)
	} else if name.starts_with('_result_') {
		base :=
			g.qualify_module_local_generic_c_name(unmangle_alias_component_to_c(name['_result_'.len..]))
		qualified = '_result_' + g.option_result_payload_alias_component(base)
	} else {
		qualified = g.qualify_module_local_type_name(name)
	}
	return qualified + suffix
}

fn non_lifetime_generic_args(args []ast.Expr) []ast.Expr {
	mut out := []ast.Expr{cap: args.len}
	for arg in args {
		if arg is ast.LifetimeExpr {
			continue
		}
		out << arg
	}
	return out
}

fn runtime_generic_param_names(params []string) []string {
	mut out := []string{cap: params.len}
	for param in params {
		if param.starts_with('^') {
			continue
		}
		out << param
	}
	return out
}

fn runtime_generic_args(args []ast.Expr) []ast.Expr {
	return non_lifetime_generic_args(args)
}

fn (g &Gen) option_result_payload_invalid(val_type string) bool {
	if val_type == '' {
		return true
	}
	if val_type == 'void' {
		return false
	}
	return is_generic_placeholder_c_type_name(val_type)
		|| g.option_result_payload_is_unbound_generic_struct(val_type)
}

fn (g &Gen) option_result_payload_is_unbound_generic_struct(val_type string) bool {
	base := val_type.trim_space().trim_right('*')
	if base == '' || base.contains('_T_') {
		return false
	}
	if base in g.generic_struct_bindings || base in g.generic_struct_instances {
		return false
	}
	typ := g.lookup_type_by_c_name_const(base) or { return false }
	if typ is types.Struct {
		return typ.generic_params.len > 0
	}
	return false
}

fn (g &Gen) option_result_payload_c_type(val_type string) string {
	if val_type.starts_with('struct_') {
		c_name := val_type['struct_'.len..]
		if c_name in g.c_struct_types && c_name !in g.typedef_c_types {
			return 'struct ${c_name}'
		}
	}
	return val_type
}

fn (g &Gen) option_result_payload_ready(val_type string) bool {
	if g.option_result_payload_invalid(val_type) || val_type == 'void' {
		return false
	}
	if !g.has_emitted_ierror_body() {
		return false
	}
	payload_type := g.option_result_payload_c_type(val_type)
	if payload_type.starts_with('struct ') {
		return true
	}
	if val_type.starts_with('_option_') {
		return val_type in g.emitted_option_structs
	}
	if val_type.starts_with('_result_') {
		return val_type in g.emitted_result_structs
	}
	if val_type in primitive_types || val_type in ['bool', 'char', 'void*', 'u8*', 'char*'] {
		return true
	}
	// `string` is a struct payload and needs its body emitted before sizeof(string).
	if val_type == 'string' || val_type == 'builtin__string' {
		string_body_key := 'body_string'
		builtin_string_body_key := 'body_builtin__string'
		return string_body_key in g.emitted_types || builtin_string_body_key in g.emitted_types
	}
	if val_type == 'IError' || val_type == 'builtin__IError' {
		return true
	}
	if val_type.ends_with('*') {
		return true
	}
	if val_type in g.typedef_c_types || is_known_external_c_type_name(val_type) {
		return true
	}
	if val_type.starts_with('Array_fixed_') {
		body_key := 'body_${val_type}'
		alias_key := 'alias_${val_type}'
		return body_key in g.emitted_types || alias_key in g.emitted_types
	}
	if val_type == 'array' || val_type.starts_with('Array_') || val_type in g.array_aliases {
		return 'body_array' in g.emitted_types
	}
	if val_type == 'map' || val_type.starts_with('Map_') || val_type in g.map_aliases {
		return 'body_map' in g.emitted_types
	}
	body_key := 'body_${val_type}'
	enum_key := 'enum_${val_type}'
	alias_key := 'alias_${val_type}'
	if body_key in g.emitted_types || enum_key in g.emitted_types || alias_key in g.emitted_types {
		return true
	}
	return false
}

fn (mut g Gen) register_alias_type(name string) {
	if !is_c_identifier_like(name) {
		return
	}
	if !g.alias_type_belongs_to_emit_modules(name) {
		return
	}
	if name.starts_with('Array_') || name.starts_with('Array_fixed_') {
		g.array_aliases[name] = true
		return
	}
	if name.starts_with('Map_') {
		g.map_aliases[name] = true
		return
	}
	if name.starts_with('_result_') {
		val_type := g.result_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			return
		}
		if val_type.starts_with('_option_') || val_type.starts_with('_result_') {
			g.register_alias_type(val_type)
		}
		g.result_aliases[name] = true
		// Also register the payload type as an array/map alias if applicable.
		if val_type.starts_with('Array_') {
			g.array_aliases[val_type] = true
		} else if val_type.starts_with('Map_') {
			g.map_aliases[val_type] = true
		}
		return
	}
	if name.starts_with('_option_') {
		val_type := option_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			return
		}
		if val_type.starts_with('_option_') || val_type.starts_with('_result_') {
			g.register_alias_type(val_type)
		}
		g.option_aliases[name] = true
		if val_type.starts_with('Array_') {
			g.array_aliases[val_type] = true
		} else if val_type.starts_with('Map_') {
			g.map_aliases[val_type] = true
		}
	}
}

fn (g &Gen) alias_type_belongs_to_emit_modules(name string) bool {
	if g.cache_bundle_name.len == 0 || (g.emit_modules.len == 0 && g.type_modules.len == 0) {
		return true
	}
	prefixes := g.module_prefixes_in_c_name(name)
	for prefix in prefixes {
		if prefix != 'builtin' && prefix !in g.type_modules && prefix !in g.emit_modules {
			return false
		}
	}
	if prefixes.len == 0 && !g.unqualified_alias_type_belongs_to_files(name) {
		return false
	}
	return true
}

fn (g &Gen) module_prefixes_in_c_name(name string) []string {
	if name.starts_with('Array_fixed_') {
		rest := name['Array_fixed_'.len..]
		last_underscore := rest.last_index_u8(`_`)
		if last_underscore < 0 {
			return []string{}
		}
		return g.module_prefixes_in_c_name(unmangle_alias_component_to_c(rest[..last_underscore]))
	}
	if name.starts_with('Array_') {
		return g.module_prefixes_in_c_name(unmangle_alias_component_to_c(name['Array_'.len..]))
	}
	if name.starts_with('Map_') {
		rest := name['Map_'.len..]
		key, value := g.parse_map_kv_types(rest)
		if key == '' || value == '' {
			return g.module_prefixes_in_c_name(unmangle_alias_component_to_c(rest))
		}
		mut prefixes := g.module_prefixes_in_c_name(unmangle_alias_component_to_c(key))
		prefixes << g.module_prefixes_in_c_name(unmangle_alias_component_to_c(value))
		return prefixes
	}
	if name.starts_with('Tuple_') {
		mut prefixes := []string{}
		for part in name['Tuple_'.len..].split('_') {
			prefixes << g.module_prefixes_in_c_name(unmangle_alias_component_to_c(part))
		}
		return prefixes
	}
	if name.starts_with('_option_') {
		return g.module_prefixes_in_c_name(unmangle_alias_component_to_c(name['_option_'.len..]))
	}
	if name.starts_with('_result_') {
		return g.module_prefixes_in_c_name(unmangle_alias_component_to_c(name['_result_'.len..]))
	}
	if name.contains('_T_') {
		base := name.all_before('_T_')
		suffix := name.all_after('_T_')
		mut prefixes := g.module_prefixes_in_c_name(base)
		prefixes << g.module_prefixes_in_c_name(unmangle_alias_component_to_c(suffix))
		return prefixes
	}
	parts := name.split('__')
	if parts.len < 2 {
		return []string{}
	}
	mut prefixes := []string{cap: parts.len - 1}
	for i := 0; i < parts.len - 1; i++ {
		prefix := parts[i]
		if prefix != '' {
			prefixes << prefix
		}
	}
	return prefixes
}

fn (g &Gen) unqualified_alias_type_belongs_to_files(name string) bool {
	if name.starts_with('Array_fixed_') {
		rest := name['Array_fixed_'.len..]
		last_underscore := rest.last_index_u8(`_`)
		if last_underscore < 0 {
			return true
		}
		return g.unqualified_alias_component_belongs_to_files(rest[..last_underscore])
	}
	if name.starts_with('Array_') {
		return g.unqualified_alias_component_belongs_to_files(name['Array_'.len..])
	}
	if name.starts_with('Map_') {
		rest := name['Map_'.len..]
		key, value := g.parse_map_kv_types(rest)
		if key == '' || value == '' {
			return g.unqualified_alias_component_belongs_to_files(rest)
		}
		return g.unqualified_alias_component_belongs_to_files(key)
			&& g.unqualified_alias_component_belongs_to_files(value)
	}
	if name.starts_with('Tuple_') {
		if name in g.tuple_aliases {
			return true
		}
		for part in name['Tuple_'.len..].split('_') {
			if part != '' && !g.unqualified_alias_component_belongs_to_files(part) {
				return false
			}
		}
		return true
	}
	if name.starts_with('_option_') {
		return g.unqualified_alias_component_belongs_to_files(name['_option_'.len..])
	}
	if name.starts_with('_result_') {
		return g.unqualified_alias_component_belongs_to_files(name['_result_'.len..])
	}
	return g.unqualified_alias_component_belongs_to_files(name)
}

fn (g &Gen) unqualified_alias_component_belongs_to_files(raw_name string) bool {
	mut name := raw_name.trim_space()
	if name == '' {
		return true
	}
	unmangled := unmangle_alias_component_to_c(name)
	if unmangled != name {
		return g.unqualified_alias_component_belongs_to_files(unmangled)
	}
	for name.ends_with('*') {
		name = name[..name.len - 1].trim_space()
	}
	if name == '' || name in primitive_types
		|| name in ['string', 'array', 'map', 'chan', 'None__', 'IError']
		|| is_generic_placeholder_type_name(name) {
		return true
	}
	if name.starts_with('Array_') || name.starts_with('Array_fixed_') || name.starts_with('Map_')
		|| name.starts_with('Tuple_') || name.starts_with('_option_')
		|| name.starts_with('_result_') {
		return g.unqualified_alias_type_belongs_to_files(name)
	}
	if name.contains('__') {
		return true
	}
	if name in g.c_struct_types || name in g.typedef_c_types || is_known_external_c_type_name(name) {
		return true
	}
	return g.unqualified_type_name_declared_in_files(name)
}

fn is_known_external_c_type_name(name string) bool {
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'pthread_condattr_t', 'pthread_rwlockattr_t',
		'atomic_uintptr_t', 'jmp_buf', 'sigjmp_buf', 'sigset_t', 'size_t', 'ssize_t', 'off_t',
		'mode_t', 'pid_t', 'uid_t', 'gid_t', 'time_t', 'clock_t', 'socklen_t', 'dev_t', 'ino_t',
		'nlink_t', 'blksize_t', 'blkcnt_t', 'cc_t', 'speed_t', 'tcflag_t', 'fd_set',
		'mach_timebase_info_data_t']
}

fn (g &Gen) unqualified_type_name_declared_in_files(name string) bool {
	if g.declared_type_names_all.len > 0 || g.declared_type_names_by_mod.len > 0 {
		if g.declared_type_name_belongs_to_lookup_files(name) {
			return true
		}
	} else if g.scan_unqualified_type_name_declared_in_files(name) {
		return true
	}
	if g.unqualified_type_name_declared_in_type_modules(name) {
		return true
	}
	return false
}

fn (g &Gen) declared_type_name_belongs_to_lookup_files(name string) bool {
	if g.cache_bundle_name.len == 0 || (g.type_modules.len == 0 && g.emit_modules.len == 0) {
		return name in g.declared_type_names_all
	}
	if type_decl_module_key('builtin', name) in g.declared_type_names_by_mod {
		return true
	}
	if g.type_modules.len > 0 {
		for module_name, _ in g.type_modules {
			if type_decl_module_key(module_name, name) in g.declared_type_names_by_mod {
				return true
			}
		}
	}
	if g.emit_modules.len > 0 {
		for module_name, _ in g.emit_modules {
			if type_decl_module_key(module_name, name) in g.declared_type_names_by_mod {
				return true
			}
		}
	}
	return false
}

fn (g &Gen) scan_unqualified_type_name_declared_in_files(name string) bool {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			module_name := flat_file_module_name(fc)
			if !g.type_lookup_file_belongs_to_cache(module_name) {
				continue
			}
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				match stmt.kind() {
					.stmt_struct_decl, .stmt_enum_decl, .stmt_interface_decl, .stmt_type_decl {
						if type_decl_name_matches_cache_alias(stmt.name(), module_name, name) {
							return true
						}
					}
					else {}
				}
			}
		}
		if g.unqualified_type_name_declared_in_type_modules(name) {
			return true
		}
		return false
	}
	for file in g.files {
		module_name := file_module_name(file)
		if !g.type_lookup_file_belongs_to_cache(module_name) {
			continue
		}
		for stmt in file.stmts {
			match stmt {
				ast.StructDecl {
					if type_decl_name_matches_cache_alias(stmt.name, module_name, name) {
						return true
					}
				}
				ast.EnumDecl {
					if type_decl_name_matches_cache_alias(stmt.name, module_name, name) {
						return true
					}
				}
				ast.InterfaceDecl {
					if type_decl_name_matches_cache_alias(stmt.name, module_name, name) {
						return true
					}
				}
				ast.TypeDecl {
					if type_decl_name_matches_cache_alias(stmt.name, module_name, name) {
						return true
					}
				}
				else {}
			}
		}
	}
	if g.unqualified_type_name_declared_in_type_modules(name) {
		return true
	}
	return false
}

fn (g &Gen) unqualified_type_name_declared_in_type_modules(name string) bool {
	if g.env == unsafe { nil } {
		return false
	}
	mut modules := []string{}
	if g.type_modules.len > 0 {
		modules = g.type_modules.keys()
	} else if g.emit_modules.len > 0 {
		modules = g.emit_modules.keys()
	}
	for module_name in modules {
		if module_name != '' && module_name != 'main' && module_name != 'builtin' {
			continue
		}
		if scope := g.env_scope(module_name) {
			obj := scope.lookup_parent(name, 0) or { continue }
			typ := obj.typ()
			match typ {
				types.Struct, types.Enum, types.Interface, types.SumType, types.Alias {
					return true
				}
				else {}
			}
		}
	}
	return false
}

fn (g &Gen) type_lookup_file_belongs_to_cache(module_name string) bool {
	if g.cache_bundle_name.len == 0 || (g.type_modules.len == 0 && g.emit_modules.len == 0) {
		return true
	}
	if module_name == 'builtin' {
		return true
	}
	return module_name in g.type_modules || module_name in g.emit_modules
}

fn type_decl_name_matches_cache_alias(decl_name string, module_name string, alias_name string) bool {
	if type_decl_name_in_module(decl_name, module_name) == alias_name {
		return true
	}
	return (module_name == '' || module_name == 'main' || module_name == 'builtin')
		&& decl_name == alias_name
}

fn type_decl_module_key(module_name string, alias_name string) string {
	return '${module_name}::${alias_name}'
}

fn file_module_name(file ast.File) string {
	for stmt in file.stmts {
		if stmt is ast.ModuleStmt {
			return stmt.name.replace('.', '_')
		}
	}
	if file.mod != '' {
		return file.mod.replace('.', '_')
	}
	return 'main'
}

fn type_decl_name_in_module(name string, module_name string) string {
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}__${name}'
	}
	return name
}

fn (mut g Gen) is_pointer_type(e ast.Expr) bool {
	if e is ast.PrefixExpr {
		return e.op == .amp
	}
	if e is ast.ModifierExpr {
		return g.is_pointer_type(e.expr)
	}
	if e is ast.Ident {
		if e.name in ['voidptr', 'charptr', 'byteptr'] || e.name.ends_with('ptr') {
			return true
		}
	}
	if raw_type := g.get_raw_type(e) {
		match raw_type {
			types.Pointer {
				return true
			}
			types.Alias {
				if raw_type.base_type is types.Pointer {
					return true
				}
			}
			else {}
		}
	}
	c_name := g.expr_type_to_c(e)
	if c_name.ends_with('*') {
		return true
	}
	return false
}

fn (mut g Gen) get_type_decl_name(node ast.TypeDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_type_alias(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	// System-provided typedefs should not be redefined by generated builtin aliases.
	if name in ['intptr_t', 'uintptr_t'] {
		return
	}
	alias_key := 'alias_${name}'
	if alias_key in g.emitted_types {
		return
	}
	g.emitted_types[alias_key] = true

	// Check if base type is a function type - needs special syntax
	if node.base_type is ast.Type {
		if node.base_type is ast.FnType {
			fn_type := node.base_type as ast.FnType
			g.fn_type_aliases[name] = true
			mut ret_type := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret_type = g.expr_type_to_c(fn_type.return_type)
			}
			ret_type = normalize_signature_type_name(ret_type, 'void')
			ret_type = g.c_callback_return_type_from_v(ret_type)
			g.sb.write_string('typedef ${ret_type} (*${name})(')
			for i, param in fn_type.params {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string(g.fn_type_alias_param_c_type(node, param))
			}
			g.sb.writeln(');')
			return
		}
		if node.base_type is ast.GenericType {
			if g.emit_vec_generic_alias_type(name, node.base_type) {
				return
			}
		}
	}
	base_type := g.expr_type_to_c(node.base_type)
	if base_type != '' && base_type != name {
		g.alias_base_types[name] = base_type
	}
	if base_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize'] {
		g.primitive_type_aliases[name] = true
	}
	if base_type == 'array' || base_type.starts_with('Array_') || base_type in g.array_aliases {
		g.array_aliases[name] = true
	}
	g.sb.writeln('typedef ${base_type} ${name};')
	if '${name}_str' !in g.fn_return_types && '${name}__str' !in g.fn_return_types {
		if str_fn := g.get_str_fn_for_type(base_type) {
			g.sb.writeln('#define ${name}_str(v) ${str_fn}(v)')
			g.sb.writeln('#define ${name}__str(v) ${str_fn}(v)')
		}
	}
}

fn (mut g Gen) fn_type_alias_param_c_type(node ast.TypeDecl, param ast.Parameter) string {
	param_name := param.typ.name()
	mut is_generic_param := false
	for generic_param in node.generic_params {
		if generic_param.name() == param_name {
			is_generic_param = true
			break
		}
	}
	// Generic function-type aliases are used as erased callbacks in C. A plain
	// placeholder would otherwise fall back to f64, producing unusable typedefs
	// like `fn (mut T)` -> `f64*` instead of the ABI-compatible `void*`.
	if is_generic_param {
		return 'void*'
	}
	param_type := g.expr_type_to_c(param.typ)
	if param.is_mut {
		return '${param_type}*'
	}
	return param_type
}

fn generic_type_base_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			return expr.rhs.name
		}
		else {
			return ''
		}
	}
}

fn (mut g Gen) emit_vec_generic_alias_type(alias_name string, gen_type ast.GenericType) bool {
	base_name := generic_type_base_name(gen_type.name)
	if base_name !in ['Vec2', 'Vec4'] {
		return false
	}
	arg_type := if gen_type.params.len > 0 { g.expr_type_to_c(gen_type.params[0]) } else { 'f32' }
	lanes := if base_name == 'Vec4' { 4 } else { 2 }
	vector_size := lanes * 4
	mut scalar_type := 'int'
	mut fallback_field_type := 'i32'
	if arg_type.starts_with('f') {
		scalar_type = 'float'
		fallback_field_type = 'f32'
	} else if arg_type.starts_with('u') {
		scalar_type = 'unsigned int'
		fallback_field_type = 'u32'
	}
	g.sb.writeln('#if defined(__clang__)')
	g.sb.writeln('typedef ${scalar_type} ${alias_name} __attribute__((ext_vector_type(${lanes})));')
	g.sb.writeln('#elif defined(__GNUC__)')
	g.sb.writeln('typedef ${scalar_type} ${alias_name} __attribute__((vector_size(${vector_size})));')
	g.sb.writeln('#else')
	if lanes == 4 {
		g.sb.writeln('typedef struct ${alias_name} { ${fallback_field_type} x; ${fallback_field_type} y; ${fallback_field_type} z; ${fallback_field_type} w; } ${alias_name};')
	} else {
		g.sb.writeln('typedef struct ${alias_name} { ${fallback_field_type} x; ${fallback_field_type} y; } ${alias_name};')
	}
	g.sb.writeln('#endif')
	return true
}

fn (g &Gen) is_enum_type(name string) bool {
	// Check emitted_types for enum_Name or enum_module__Name
	enum_key := 'enum_${name}'
	if enum_key in g.emitted_types {
		return true
	}
	qualified := g.get_qualified_name(name)
	qualified_enum_key := 'enum_${qualified}'
	if qualified_enum_key in g.emitted_types {
		return true
	}
	// Also check the types.Environment
	if g.env != unsafe { nil } {
		mut found := false
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(name, 0) {
				if obj is types.Type && obj is types.Enum {
					found = true
				}
			}
		}
		if found {
			return true
		}
	}
	return false
}

fn (g &Gen) get_qualified_name(name string) string {
	if name.contains('__') {
		return name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${name}'
	}
	return name
}

fn (g &Gen) normalize_enum_name(name string) string {
	if g.cur_module != '' {
		double_prefix := '${g.cur_module}__${g.cur_module}__'
		if name.starts_with(double_prefix) {
			return name[g.cur_module.len + 2..]
		}
	}
	// When the name has a module prefix (e.g., sync__ChanState) but the emitted
	// enum uses the short name (e.g., ChanState — a builtin type referenced from
	// the sync module), strip the prefix so references match the typedef.
	if name.contains('__') {
		short_name := name.all_after_last('__')
		if short_name in g.enum_type_fields && name !in g.enum_type_fields {
			return short_name
		}
		if 'enum_${short_name}' in g.emitted_types && 'enum_${name}' !in g.emitted_types {
			return short_name
		}
	}
	return name
}

fn (g &Gen) enum_member_c_name(enum_name string, field_name string) string {
	mut actual_field_name := field_name
	if !actual_field_name.starts_with('@') && g.enum_has_field(enum_name, '@${actual_field_name}') {
		actual_field_name = '@${actual_field_name}'
	}
	return '${g.normalize_enum_name(enum_name)}__${escape_c_keyword(mangle_alias_component(actual_field_name))}'
}

fn (g &Gen) enum_has_field(enum_name string, field_name string) bool {
	if enum_name in g.enum_type_fields {
		return field_name in g.enum_type_fields[enum_name]
	}
	normalized := g.normalize_enum_name(enum_name)
	if normalized in g.enum_type_fields {
		return field_name in g.enum_type_fields[normalized]
	}
	if enum_name.contains('__') {
		short_name := enum_name.all_after_last('__')
		if short_name in g.enum_type_fields {
			return field_name in g.enum_type_fields[short_name]
		}
	}
	return false
}

fn (g &Gen) is_type_name(name string) bool {
	if name in primitive_types {
		return true
	}
	// Check if it's a known emitted type (enum, struct, alias, sum type, interface)
	qualified := g.get_qualified_name(name)
	enum_key := 'enum_${name}'
	qualified_enum_key := 'enum_${qualified}'
	if enum_key in g.emitted_types || qualified_enum_key in g.emitted_types {
		return true
	}
	body_key := 'body_${name}'
	qualified_body_key := 'body_${qualified}'
	if body_key in g.emitted_types || qualified_body_key in g.emitted_types {
		return true
	}
	alias_key := 'alias_${name}'
	qualified_alias_key := 'alias_${qualified}'
	if alias_key in g.emitted_types || qualified_alias_key in g.emitted_types {
		return true
	}
	if name in g.emitted_types || qualified in g.emitted_types {
		return true
	}
	return false
}

fn (mut g Gen) c_type_from_type_name(type_name string) string {
	name := type_name.trim_space()
	if name == '' {
		return 'int'
	}
	if name.starts_with('tuple (') && name.ends_with(')') {
		inner := name['tuple ('.len..name.len - 1]
		mut elem_types := []string{}
		for part in split_top_level_csv(inner) {
			elem_types << g.c_type_from_type_name(part)
		}
		return g.register_tuple_alias(elem_types)
	}
	if name.starts_with('&') {
		return g.c_type_from_type_name(name[1..]) + '*'
	}
	if name.starts_with('[]') {
		elem := g.c_type_from_type_name(name[2..])
		return 'Array_' + mangle_alias_component(elem)
	}
	if name.contains('::') {
		return name.replace('::', '__')
	}
	return name.replace('.', '__')
}

fn (mut g Gen) register_tuple_alias(elem_types []string) string {
	if elem_types.len == 0 {
		return 'int'
	}
	mut parts := []string{cap: elem_types.len}
	for elem in elem_types {
		parts << mangle_alias_component(elem)
	}
	name := 'Tuple_' + parts.join('_')
	if name !in g.tuple_aliases {
		cloned_elem_types := elem_types.clone()
		g.tuple_aliases[name] = cloned_elem_types
	}
	return name
}

fn (g &Gen) is_tuple_alias(t string) bool {
	return t in g.tuple_aliases
}

fn (g &Gen) tuple_field_type_ready(field_type string) bool {
	typ := field_type.trim_space()
	if typ == '' || typ == 'void' {
		return false
	}
	if typ in primitive_types || typ in ['bool', 'char', 'void*', 'u8*', 'char*'] {
		return true
	}
	if typ == 'string' || typ == 'builtin__string' {
		return 'body_string' in g.emitted_types || 'body_builtin__string' in g.emitted_types
	}
	if typ.ends_with('*') {
		return true
	}
	if typ.starts_with('Array_fixed_') {
		return 'body_${typ}' in g.emitted_types || 'alias_${typ}' in g.emitted_types
	}
	if typ == 'array' || typ.starts_with('Array_') || typ in g.array_aliases {
		return 'body_array' in g.emitted_types
	}
	if typ == 'map' || typ.starts_with('Map_') || typ in g.map_aliases {
		return 'body_map' in g.emitted_types
	}
	// Pointer typedefs like 'Appptr' end with 'ptr' — only treat as ready pointer
	// after ruling out Array_/Map_ prefixes which are by-value struct types.
	if typ.ends_with('ptr') {
		return true
	}
	return 'body_${typ}' in g.emitted_types || 'enum_${typ}' in g.emitted_types
		|| 'alias_${typ}' in g.emitted_types
}

fn (mut g Gen) emit_tuple_aliases() {
	mut names := g.tuple_aliases.keys()
	names.sort()
	for name in names {
		body_key := 'body_${name}'
		if body_key in g.emitted_types {
			continue
		}
		field_types := g.tuple_aliases[name]
		mut ready := true
		for field_type in field_types {
			if !g.tuple_field_type_ready(field_type) {
				ready = false
				break
			}
		}
		if !ready {
			continue
		}
		g.emitted_types[body_key] = true
		g.sb.writeln('typedef struct ${name} {')
		for i, field_type in field_types {
			g.sb.writeln('\t${field_type} arg${i};')
		}
		g.sb.writeln('} ${name};')
	}
}

fn (g &Gen) should_use_memcmp_eq(lhs_type string, rhs_type string) bool {
	if lhs_type == '' || rhs_type == '' {
		return false
	}
	mut t := lhs_type
	if lhs_type != rhs_type {
		// Resolve aliases: ui__Color (alias for gg__Color) should match gg__Color
		lhs_base := g.alias_base_c_type(lhs_type) or { lhs_type }
		rhs_base := g.alias_base_c_type(rhs_type) or { rhs_type }
		if lhs_base != rhs_base && lhs_type != rhs_base && rhs_type != lhs_base {
			return false
		}
		t = lhs_base
	}
	if t in primitive_types || t == 'string' {
		return false
	}
	if g.is_enum_type(t) || g.is_enum_type(rhs_type) {
		return false
	}
	if base_type := g.alias_base_c_type(t) {
		if g.is_enum_type(base_type) {
			return false
		}
	}
	if t.ends_with('*') || t.ends_with('ptr') {
		return false
	}
	if t.starts_with('Array_') || t.starts_with('Map_') {
		return false
	}
	return true
}

// is_known_struct_type checks if a type name resolves to a struct (directly or through aliases).
fn (mut g Gen) is_known_struct_type(type_name string) bool {
	if g.lookup_struct_type_by_c_name(type_name).fields.len > 0 {
		return true
	}
	// Resolve through aliases (e.g. ui__Color -> gg__Color)
	if base := g.alias_base_c_type(type_name) {
		if g.lookup_struct_type_by_c_name(base).fields.len > 0 {
			return true
		}
	}
	return false
}

// struct_has_ref_fields checks if a struct has any reference-type fields
// (string, array, map, or nested structs containing them) that require deep
// comparison instead of memcmp.
fn (g &Gen) struct_has_ref_fields(s types.Struct) bool {
	mut seen := map[string]bool{}
	if s.name != '' {
		seen[s.name] = true
	}
	for field in s.fields {
		if g.type_has_ref_fields(field.typ, mut seen) {
			return true
		}
	}
	return false
}

fn (g &Gen) type_has_ref_fields(t types.Type, mut seen map[string]bool) bool {
	match t {
		types.String, types.Array, types.Map, types.OptionType {
			return true
		}
		types.Struct {
			if t.name != '' {
				if t.name in seen {
					return false
				}
				seen[t.name] = true
			}
			for field in t.fields {
				if g.type_has_ref_fields(field.typ, mut seen) {
					return true
				}
			}
		}
		types.Alias {
			if t.name != '' {
				if t.name in seen {
					return false
				}
				seen[t.name] = true
			}
			if type_has_valid_data(t.base_type) {
				return g.type_has_ref_fields(t.base_type, mut seen)
			}
		}
		else {}
	}

	return false
}

fn (mut g Gen) gen_type_field_eq_expr(t types.Type, c_type string, lhs string, rhs string) string {
	match t {
		types.String {
			return 'string__eq(${lhs}, ${rhs})'
		}
		types.Array {
			return '__v2_array_eq(${lhs}, ${rhs})'
		}
		types.Map {
			map_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			if map_type.starts_with('Map_') {
				return '${map_type}_map_eq(${lhs}, ${rhs})'
			}
			return 'map_map_eq(${lhs}, ${rhs})'
		}
		types.OptionType {
			option_c_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			mut value_c_type := option_value_type(option_c_type)
			if value_c_type == '' {
				value_c_type = g.types_type_to_c(t.base_type)
			}
			if value_c_type == '' || value_c_type == 'void' {
				return '${lhs}.state == ${rhs}.state'
			}
			lpayload := '(*(${value_c_type}*)(((u8*)(&${lhs}.err)) + sizeof(IError)))'
			rpayload := '(*(${value_c_type}*)(((u8*)(&${rhs}.err)) + sizeof(IError)))'
			payload_eq := g.gen_type_field_eq_expr(t.base_type, value_c_type, lpayload, rpayload)
			return '(${lhs}.state == ${rhs}.state && (${lhs}.state != 0 || ${payload_eq}))'
		}
		types.Struct {
			field_c_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			if g.struct_has_ref_fields(t) {
				return g.gen_struct_field_eq_expr(t, lhs, rhs)
			}
			return 'memcmp(&${lhs}, &${rhs}, sizeof(${field_c_type})) == 0'
		}
		types.Alias {
			if type_has_valid_data(t.base_type) {
				return g.gen_type_field_eq_expr(t.base_type, c_type, lhs, rhs)
			}
			field_c_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			return 'memcmp(&${lhs}, &${rhs}, sizeof(${field_c_type})) == 0'
		}
		types.Primitive, types.Pointer, types.Rune, types.Char, types.ISize, types.USize,
		types.Enum {
			return '${lhs} == ${rhs}'
		}
		types.NamedType {
			field_c_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			if field_c_type == 'string' {
				return 'string__eq(${lhs}, ${rhs})'
			}
			if field_c_type in primitive_types || field_c_type.ends_with('*')
				|| field_c_type.ends_with('ptr') || g.is_enum_type(field_c_type) {
				return '${lhs} == ${rhs}'
			}
			return 'memcmp(&${lhs}, &${rhs}, sizeof(${field_c_type})) == 0'
		}
		else {
			field_c_type := if c_type != '' { c_type } else { g.types_type_to_c(t) }
			return 'memcmp(&${lhs}, &${rhs}, sizeof(${field_c_type})) == 0'
		}
	}
}

// gen_struct_field_eq_expr generates an inline field-by-field equality expression
// for structs with reference-type fields.
fn (mut g Gen) gen_struct_field_eq_expr(s types.Struct, va string, vb string) string {
	mut parts := []string{}
	for field in s.fields {
		fname := escape_c_keyword(field.name)
		c_type := g.types_type_to_c(field.typ)
		parts << g.gen_type_field_eq_expr(field.typ, c_type, '${va}.${fname}', '${vb}.${fname}')
	}
	if parts.len == 0 {
		return '1'
	}
	return parts.join(' && ')
}

fn (mut g Gen) method_receiver_base_type(expr ast.Expr) string {
	unwrapped_expr := strip_expr_wrappers(expr)
	if unwrapped_expr is ast.PrefixExpr && unwrapped_expr.op in [.amp, .mul] {
		return g.method_receiver_base_type(unwrapped_expr.expr)
	}
	direct_type := g.direct_known_c_type_for_expr(expr)
	if direct_type != '' && direct_type != 'int' {
		base := strip_pointer_type_name(direct_type)
		if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
			return base
		}
	}
	// For InitExpr (struct literal), use the explicit type from the AST
	// rather than pos.id lookup (which may return a different pre-transformer type).
	if expr is ast.InitExpr {
		init_type := g.expr_type_to_c(expr.typ)
		if init_type != '' && init_type != 'int' {
			return init_type
		}
	}
	if expr is ast.Ident {
		if local_type := g.get_local_var_c_type(expr.name) {
			mut base := local_type
			if base.ends_with('*') {
				base = base[..base.len - 1]
			}
			if base != '' && base != 'int' {
				return base
			}
		}
	}
	if expr is ast.SelectorExpr {
		lhs_struct_name := g.selector_struct_name(expr.lhs)
		if lhs_struct_name != '' {
			if declared_field_type := g.lookup_struct_field_type_by_name(lhs_struct_name,
				expr.rhs.name)
			{
				base := strip_pointer_type_name(declared_field_type)
				if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
					return base
				}
			}
		}
		field_type := g.selector_field_type(expr)
		if field_type != '' && field_type != 'int' {
			mut base := field_type
			if base.ends_with('*') {
				base = base[..base.len - 1]
			}
			if base in ['voidptr', 'void*'] {
				return 'void'
			}
			return base
		}
	}
	// Fast path: env pos.id O(1) lookup (covers most non-Ident receivers).
	if g.env != unsafe { nil } {
		pos := expr.pos()
		if pos.is_valid() {
			if raw_type := g.env.get_expr_type(pos.id) {
				if type_has_valid_data(raw_type) {
					match raw_type {
						types.Pointer {
							return g.types_type_to_c(raw_type.base_type)
						}
						types.Alias {
							// Use the alias name itself (e.g. strings__Builder, ssa__TypeID)
							return raw_type.name
						}
						else {
							c := g.types_type_to_c(raw_type)
							if c != '' && c != 'int' && c != 'void' {
								return c
							}
						}
					}
				}
			}
		}
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Pointer {
				return g.types_type_to_c(raw_type.base_type)
			}
			else {
				raw_c_type := g.types_type_to_c(raw_type)
				if raw_c_type != 'void' {
					return raw_c_type
				}
			}
		}
	}
	mut receiver_type := g.get_expr_type(expr)
	if receiver_type.ends_with('*') {
		receiver_type = receiver_type[..receiver_type.len - 1]
	}
	if receiver_type in ['voidptr', 'void*'] {
		return 'void'
	}
	return receiver_type
}

struct EmbeddedFieldLookupInfo {
	owner      string
	field_type string
}

fn embedded_owner_field_name(type_name string) string {
	base := strip_pointer_type_name(type_name)
	if base.contains('__') {
		return base.all_after_last('__')
	}
	return base
}

fn (mut g Gen) lookup_embedded_field_info_in_struct(st types.Struct, field_name string) ?EmbeddedFieldLookupInfo {
	for embedded in st.embedded {
		embedded_c_type := g.types_type_to_c(embedded)
		owner := embedded_owner_field_name(embedded_c_type)
		// Use live lookup if the embedded copy has stale (empty) fields
		live_emb := if embedded.fields.len == 0 && embedded_c_type != '' {
			g.lookup_struct_type_by_c_name(embedded_c_type)
		} else {
			embedded
		}
		for field in live_emb.fields {
			if field.name == field_name {
				return EmbeddedFieldLookupInfo{
					owner:      owner
					field_type: g.types_type_to_c(field.typ)
				}
			}
		}
		if nested := g.lookup_embedded_field_info_in_struct(live_emb, field_name) {
			return EmbeddedFieldLookupInfo{
				owner:      owner
				field_type: nested.field_type
			}
		}
	}
	return none
}

fn (mut g Gen) find_struct_decl_info_in_flat(c_name string, saved_module string, current_module_only bool, exact bool) ?StructDeclInfo {
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		if current_module_only && g.cur_module != saved_module {
			continue
		}
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_struct_decl {
				continue
			}
			decl := stmt.struct_decl()
			if decl.language != .v {
				continue
			}
			struct_name := g.get_struct_name(decl)
			matches := if exact {
				struct_name == c_name
			} else {
				short_type_name(struct_name) == c_name
			}
			if matches {
				return StructDeclInfo{
					decl:      decl
					mod:       g.cur_module
					file_name: fc.name()
				}
			}
		}
	}
	return none
}

fn (mut g Gen) find_struct_decl_info_by_c_name(c_name string) ?StructDeclInfo {
	if c_name == '' {
		return none
	}
	if cached := g.struct_decl_info_cache[c_name] {
		return cached
	}
	if c_name in g.struct_decl_info_miss {
		return none
	}
	saved_module := g.cur_module
	saved_file_name := g.cur_file_name
	defer {
		g.cur_module = saved_module
		g.cur_file_name = saved_file_name
	}
	if g.has_flat() {
		if info := g.find_struct_decl_info_in_flat(c_name, saved_module, false, true) {
			g.struct_decl_info_cache[c_name] = info
			return info
		}
		if info := g.find_struct_decl_info_in_flat(c_name, saved_module, true, false) {
			g.struct_decl_info_cache[c_name] = info
			return info
		}
		if info := g.find_struct_decl_info_in_flat(c_name, saved_module, false, false) {
			g.struct_decl_info_cache[c_name] = info
			return info
		}
		g.struct_decl_info_miss[c_name] = true
		return none
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language != .v {
					continue
				}
				struct_name := g.get_struct_name(stmt)
				if struct_name == c_name {
					info := StructDeclInfo{
						decl:      stmt
						mod:       g.cur_module
						file_name: file.name
					}
					g.struct_decl_info_cache[c_name] = info
					return info
				}
			}
		}
	}
	for file in g.files {
		g.set_file_module(file)
		if g.cur_module != saved_module {
			continue
		}
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language != .v {
					continue
				}
				struct_name := g.get_struct_name(stmt)
				if short_type_name(struct_name) == c_name {
					info := StructDeclInfo{
						decl:      stmt
						mod:       g.cur_module
						file_name: file.name
					}
					g.struct_decl_info_cache[c_name] = info
					return info
				}
			}
		}
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language != .v {
					continue
				}
				struct_name := g.get_struct_name(stmt)
				if short_type_name(struct_name) == c_name {
					info := StructDeclInfo{
						decl:      stmt
						mod:       g.cur_module
						file_name: file.name
					}
					g.struct_decl_info_cache[c_name] = info
					return info
				}
			}
		}
	}
	g.struct_decl_info_miss[c_name] = true
	return none
}

fn (mut g Gen) lookup_embedded_field_info_in_decl(info StructDeclInfo, field_name string) ?EmbeddedFieldLookupInfo {
	saved_module := g.cur_module
	saved_file_name := g.cur_file_name
	defer {
		g.cur_module = saved_module
		g.cur_file_name = saved_file_name
	}
	g.set_struct_info_context(info)
	for emb in info.decl.embedded {
		embedded_c_type := g.expr_type_to_c(emb)
		owner := embedded_owner_field_name(embedded_c_type)
		if embedded_info := g.find_struct_decl_info_by_c_name(embedded_c_type) {
			g.set_struct_info_context(embedded_info)
			for field in embedded_info.decl.fields {
				if field.name == field_name {
					return EmbeddedFieldLookupInfo{
						owner:      owner
						field_type: g.expr_type_to_c(field.typ)
					}
				}
			}
			if nested := g.lookup_embedded_field_info_in_decl(embedded_info, field_name) {
				return EmbeddedFieldLookupInfo{
					owner:      owner
					field_type: nested.field_type
				}
			}
			g.set_struct_info_context(info)
		}
	}
	return none
}

fn (mut g Gen) has_struct_field(c_type_name string, field_name string) bool {
	// Special-case `string` which is types.String, not types.Struct.
	// Its fields (str, len, is_lit) are defined in the builtin struct.
	if c_type_name == 'string' {
		return field_name in ['str', 'len', 'is_lit']
	}
	s := g.lookup_struct_type_by_c_name(c_type_name)
	for f in s.fields {
		if f.name == field_name {
			return true
		}
	}
	return false
}

fn (mut g Gen) lookup_embedded_field_info(struct_name string, field_name string) ?EmbeddedFieldLookupInfo {
	if struct_name == '' || field_name == '' {
		return none
	}
	mut base_name := strip_pointer_type_name(struct_name)
	mut struct_type := g.lookup_struct_type_by_c_name(base_name)
	if struct_type.fields.len == 0 && struct_type.embedded.len == 0 {
		if alias_base := g.alias_base_c_type(base_name) {
			base_name = alias_base
			struct_type = g.lookup_struct_type_by_c_name(base_name)
		}
	}
	// If the struct has a direct field with this name, don't route through embedded
	for f in struct_type.fields {
		if f.name == field_name {
			return none
		}
	}
	if struct_type.fields.len == 0 && struct_type.embedded.len == 0 {
		if decl_info := g.find_struct_decl_info_by_c_name(base_name) {
			// Also check AST direct fields
			for f in decl_info.decl.fields {
				if f.name == field_name {
					return none
				}
			}
			return g.lookup_embedded_field_info_in_decl(decl_info, field_name)
		}
		return none
	}
	if info := g.lookup_embedded_field_info_in_struct(struct_type, field_name) {
		return info
	}
	if decl_info := g.find_struct_decl_info_by_c_name(base_name) {
		return g.lookup_embedded_field_info_in_decl(decl_info, field_name)
	}
	return none
}

fn (mut g Gen) channel_elem_type_from_expr(expr ast.Expr) ?string {
	if raw := g.get_raw_type(expr) {
		if elem_type := raw.channel_elem_type() {
			return g.types_type_to_c(elem_type)
		}
	}
	return none
}

fn (g &Gen) types_type_to_c(t types.Type) string {
	if !type_has_valid_data(t) {
		return 'int /*corrupt type*/'
	}
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
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
			} else if t.props.has(.float) {
				if t.props.has(.untyped) {
					return 'f64'
				}
				return if t.size == 32 { 'f32' } else { 'f64' }
			} else if t.props.has(.boolean) {
				return 'bool'
			}
			return 'int'
		}
		types.Pointer {
			base := g.types_type_to_c(t.base_type)
			return base + '*'
		}
		types.Array {
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_' + mangle_alias_component(elem)
		}
		types.ArrayFixed {
			elem := g.types_type_to_c(t.elem_type)
			return 'Array_fixed_' + mangle_alias_component(elem) + '_' + t.len.str()
		}
		types.Channel {
			return 'chan'
		}
		types.Struct {
			name := g.c_type_from_possible_specialization_token(t.name)
			if name.ends_with('*') {
				return name
			}
			if !name.contains('__') && name in g.c_struct_types && name !in g.typedef_c_types {
				return 'struct ${name}'
			}
			// C struct types need the 'struct' keyword in C
			if !name.contains('__')
				&& name in ['tm', 'timespec', 'timeval', 'stat', 'dirent', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'addrinfo', 'msghdr', 'iovec', 'pollfd', 'rusage', 'rlimit', 'sigaction', 'winsize', 'utsname'] {
				return 'struct ${name}'
			}
			if imported_name := g.imported_symbol_c_type(name) {
				return imported_name
			}
			return name
		}
		types.String {
			return 'string'
		}
		types.NamedType {
			name := string(t)
			if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
				'f32', 'f64', 'usize', 'isize', 'bool', 'string', 'chan', 'Error', 'IError'] {
				return name
			}
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			if name == 'byteptr' {
				return 'u8*'
			}
			specialized_name := g.c_type_from_possible_specialization_token(name)
			if specialized_name != name {
				return specialized_name
			}
			unmangled_name := unmangle_c_ptr_type(name)
			if unmangled_name != name {
				return unmangled_name
			}
			if mapped := g.vec_type_suffix_alias_to_c(name) {
				return mapped
			}
			if name in ['SimdFloat4', 'SimdInt4', 'SimdU32_4', 'SimdFloat2', 'SimdUint2', 'SimdI32_2']
				&& g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				return '${g.cur_module}__${name}'
			}
			if is_generic_placeholder_type_name(name) {
				if concrete := g.resolve_active_generic_type(name) {
					return g.types_type_to_c(concrete)
				}
				return 'f64'
			}
			if imported_name := g.imported_symbol_c_type(name) {
				return imported_name
			}
			if g.is_module_local_type(name) {
				return '${g.cur_module}__${name}'
			}
			return name
		}
		types.Alias {
			return g.c_type_from_possible_specialization_token(t.name)
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
			return t.name
		}
		types.Interface {
			if is_ierror_interface_name(t.name) {
				return 'IError'
			}
			return t.name
		}
		types.SumType {
			return types.sum_type_name(t)
		}
		types.Map {
			key := g.types_type_to_c(t.key_type)
			val := g.types_type_to_c(t.value_type)
			return 'Map_' + mangle_alias_component(key) + '_' + mangle_alias_component(val)
		}
		types.OptionType {
			base := g.types_type_to_c(t.base_type)
			return '_option_' + g.option_result_payload_alias_component(base)
		}
		types.ResultType {
			base := g.types_type_to_c(t.base_type)
			return '_result_' + g.option_result_payload_alias_component(base)
		}
		types.FnType {
			return 'void*'
		}
		types.ISize {
			return 'isize'
		}
		types.USize {
			return 'usize'
		}
		types.Nil {
			return 'void*'
		}
		types.None {
			return 'void'
		}
		else {
			return 'int'
		}
	}
}

// types_type_to_v converts a types.Type to a V type name string (e.g. "map[rune]int", "[]string").
// Used for typeof(expr).name which needs V syntax, not C type names.
fn (g &Gen) types_type_to_v(t types.Type) string {
	if !type_has_valid_data(t) {
		return 'int /*corrupt type*/'
	}
	match t {
		types.Primitive {
			if t.props.has(.integer) {
				if t.props.has(.untyped) {
					return 'int'
				}
				size := if t.size == 0 { 32 } else { int(t.size) }
				is_signed := !t.props.has(.unsigned)
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
			} else if t.props.has(.float) {
				if t.props.has(.untyped) {
					return 'f64'
				}
				return if t.size == 32 { 'f32' } else { 'f64' }
			} else if t.props.has(.boolean) {
				return 'bool'
			}
			return 'int'
		}
		types.Pointer {
			base := g.types_type_to_v(t.base_type)
			return '&' + base
		}
		types.Array {
			elem := g.types_type_to_v(t.elem_type)
			return '[]' + elem
		}
		types.ArrayFixed {
			elem := g.types_type_to_v(t.elem_type)
			return '[' + t.len.str() + ']' + elem
		}
		types.Struct {
			return c_name_to_v_name(t.name)
		}
		types.String {
			return 'string'
		}
		types.NamedType {
			return c_name_to_v_name(string(t))
		}
		types.Alias {
			return c_name_to_v_name(t.name)
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
			return c_name_to_v_name(t.name)
		}
		types.Interface {
			return c_name_to_v_name(t.name)
		}
		types.SumType {
			return c_name_to_v_name(types.sum_type_name(t))
		}
		types.Map {
			key := g.types_type_to_v(t.key_type)
			val := g.types_type_to_v(t.value_type)
			return 'map[' + key + ']' + val
		}
		types.OptionType {
			base := g.types_type_to_v(t.base_type)
			return '?' + base
		}
		types.ResultType {
			base := g.types_type_to_v(t.base_type)
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

// get_expr_type_from_env retrieves the C type string for an expression from the Environment
fn (g &Gen) get_expr_type_from_env(e ast.Expr) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	if !expr_has_valid_data(e) {
		return none
	}
	pos := e.pos()
	if pos.id != 0 {
		if typ := g.env.get_expr_type(pos.id) {
			// Self-hosting can leave alias payloads (e.g. voidptr, MapHashFn)
			// that propagate incorrect types to variable declarations.
			// Skip aliases here; selector_field_type uses get_env_c_type instead.
			if typ is types.Alias {
				return none
			}
			return g.types_type_to_c(typ)
		}
	}
	return none
}

// get_env_c_type retrieves the C type string for an expression from the Environment
// without filtering aliases.  This is safe for selector field types where the alias
// (e.g. strings__Builder, ssa__TypeID) is the correct type for the field.
// Pointer-like aliases (voidptr, charptr, byteptr) are unmangled to their C pointer
// form (void*, char*, u8*) so downstream pointer detection (-> vs .) works correctly.
fn (g &Gen) get_env_c_type(e ast.Expr) ?string {
	if g.env == unsafe { nil } {
		return none
	}
	if !expr_has_valid_data(e) {
		return none
	}
	pos := e.pos()
	if pos.id != 0 {
		if typ := g.env.get_expr_type(pos.id) {
			c := g.types_type_to_c(typ)
			return unmangle_c_ptr_type(c)
		}
	}
	return none
}

// receiver_type_to_scope_name converts a receiver type AST expression to
// the V-style name used by the checker for scope keys.
// This must match what the checker computes: receiver_type.base_type().name()
fn (g &Gen) receiver_type_to_scope_name(typ ast.Expr) string {
	// Strip receiver modifiers (e.g. `mut Type`, `shared Type`)
	if typ is ast.ModifierExpr {
		return g.receiver_type_to_scope_name(typ.expr)
	}
	// Strip pointer/reference prefix (e.g. &[]string -> []string)
	if typ is ast.PrefixExpr {
		if typ.op == .amp {
			return g.receiver_type_to_scope_name(typ.expr)
		}
	}
	if typ is ast.Type {
		if typ is ast.PointerType {
			return g.receiver_type_to_scope_name(typ.base_type)
		}
		// Array type: []T -> "[]T"
		if typ is ast.ArrayType {
			elem := g.receiver_type_to_scope_name(typ.elem_type)
			return '[]${elem}'
		}
		// Map type: map[K]V -> "map[K]V"
		if typ is ast.MapType {
			key := g.receiver_type_to_scope_name(typ.key_type)
			val := g.receiver_type_to_scope_name(typ.value_type)
			return 'map[${key}]${val}'
		}
	}
	// Ident: bare type name (e.g. "Builder", "string")
	if typ is ast.Ident {
		return typ.name
	}
	// Selector: module.Type -> just use Type
	if typ is ast.SelectorExpr {
		return typ.rhs.name
	}
	// Fallback
	return ''
}

fn (mut g Gen) remember_runtime_local_type(name string, typ string) {
	if name == '' || name == '_' || typ == '' {
		return
	}
	g.runtime_local_types[name] = typ
	g.runtime_decl_types[name] = typ
	// Invalidate negative cache entry if it was previously marked as non-local.
	if name in g.not_local_var_cache {
		g.not_local_var_cache.delete(name)
	}
}

fn (mut g Gen) remember_runtime_current_local_type(name string, typ string) {
	if name == '' || name == '_' || typ == '' {
		return
	}
	g.runtime_local_types[name] = typ
	if name in g.not_local_var_cache {
		g.not_local_var_cache.delete(name)
	}
}

fn (g &Gen) get_runtime_decl_type(name string) ?string {
	if typ := g.runtime_decl_types[name] {
		return typ
	}
	return none
}

// get_local_var_c_type looks up a local variable's C type string from the function scope
fn (mut g Gen) get_local_var_c_type(name string) ?string {
	if local_typ := g.runtime_local_types[name] {
		return local_typ
	}
	// Negative cache: name was already looked up and is not a local variable.
	if name in g.not_local_var_cache {
		return none
	}
	if mut fn_scope := g.ensure_cur_fn_scope() {
		if obj := fn_scope.lookup_parent(name, 0) {
			if obj is types.Module || obj is types.Const || obj is types.Global || obj is types.Fn {
				g.not_local_var_cache[name] = true
				return none
			}
			c := g.types_type_to_c(obj.typ())
			// Cache the scope-resolved type for future lookups (avoids repeat scope walks).
			if c != '' {
				g.remember_runtime_current_local_type(name, c)
			}
			return c
		}
	}
	g.not_local_var_cache[name] = true
	return none
}

fn builtin_string_field_c_type(field_name string) string {
	return match field_name {
		'str' { 'u8*' }
		'len' { 'int' }
		'is_lit' { 'int' }
		else { '' }
	}
}

fn type_is_string_or_string_ptr(typ types.Type) bool {
	match typ {
		types.String {
			return true
		}
		types.Pointer {
			return type_is_string_or_string_ptr(typ.base_type)
		}
		types.Alias {
			return typ.name == 'string' || type_is_string_or_string_ptr(typ.base_type)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) direct_known_c_type_for_expr(expr ast.Expr) string {
	match expr {
		ast.ParenExpr {
			return g.direct_known_c_type_for_expr(expr.expr)
		}
		ast.ModifierExpr {
			return g.direct_known_c_type_for_expr(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .mul {
				ptr_type := g.direct_known_c_type_for_expr(expr.expr)
				if ptr_type != '' {
					return strip_one_pointer_type_name(ptr_type)
				}
			}
		}
		ast.CastExpr {
			return g.expr_type_to_c(expr.typ)
		}
		ast.AsCastExpr {
			return g.expr_type_to_c(expr.typ)
		}
		ast.CallOrCastExpr {
			if g.call_or_cast_lhs_is_type(expr.lhs) {
				return g.expr_type_to_c(expr.lhs)
			}
		}
		else {}
	}

	if expr is ast.SelectorExpr {
		if global_type := g.global_var_type_for_selector(expr) {
			return global_type
		}
	}
	unwrapped := strip_expr_wrappers(expr)
	if unwrapped is ast.Ident {
		name := unwrapped.name
		if local_type := g.get_local_var_c_type(name) {
			return local_type
		}
		if const_type := g.const_types[name] {
			return const_type
		}
		if global_type := g.global_var_type_for_ident(name) {
			return global_type
		}
		if g.cur_module != '' {
			qualified := '${g.cur_module}__${name}'
			if const_type := g.const_types[qualified] {
				return const_type
			}
			if global_type := g.global_var_types[qualified] {
				return global_type
			}
		}
		if global_type := g.global_var_types[name] {
			return global_type
		}
	}
	return ''
}

fn (mut g Gen) global_var_type_for_selector(sel ast.SelectorExpr) ?string {
	parts := cleanc_selector_expr_parts(sel)
	if parts.len != 2 || parts[1] == '' {
		return none
	}
	mod_name := g.resolve_module_name(parts[0])
	if mod_name == '' {
		return none
	}
	qualified := module_storage_c_name(mod_name, parts[1])
	if global_type := g.global_var_types[qualified] {
		return global_type
	}
	return none
}

fn (g &Gen) global_var_type_for_ident(name string) ?string {
	if name == '' {
		return none
	}
	if global_type := g.global_var_types[name] {
		return global_type
	}
	qualified := module_storage_c_name(g.cur_module, name)
	if qualified in g.module_storage_vars {
		if global_type := g.global_var_types[qualified] {
			return global_type
		}
	}
	if name.contains('__') {
		return none
	}
	return none
}

fn (mut g Gen) builtin_string_field_lhs_type(lhs ast.Expr, field_name string) string {
	if builtin_string_field_c_type(field_name) == '' {
		return ''
	}
	direct_type := g.direct_known_c_type_for_expr(lhs).trim_space()
	if direct_type.trim_right('*') == 'string' {
		return direct_type
	}
	if raw_type := g.get_raw_type(lhs) {
		if type_is_string_or_string_ptr(raw_type) {
			raw_c_type := g.types_type_to_c(raw_type).trim_space()
			if raw_c_type != '' {
				return raw_c_type
			}
			return 'string'
		}
	}
	lhs_type := g.get_expr_type(lhs).trim_space()
	if lhs_type.trim_right('*') == 'string' {
		return lhs_type
	}
	return ''
}

fn is_float_like_c_type_name(typ string) bool {
	return typ in ['f32', 'f64', 'float_literal']
}

fn promote_numeric_c_types(lhs string, rhs string) string {
	if is_float_like_c_type_name(lhs) || is_float_like_c_type_name(rhs) {
		if lhs in ['f64', 'float_literal'] || rhs in ['f64', 'float_literal'] {
			return 'f64'
		}
		if lhs == 'f32' || rhs == 'f32' {
			return 'f32'
		}
		return 'f64'
	}
	if lhs != '' && lhs !in ['int', 'int_literal'] {
		return lhs
	}
	if rhs != '' && rhs !in ['int', 'int_literal'] {
		return rhs
	}
	return 'int'
}

fn (mut g Gen) infer_numeric_expr_type(node ast.Expr) string {
	match node {
		ast.Ident {
			return g.get_expr_type(node)
		}
		ast.BasicLiteral {
			if node.kind == .number {
				if node.value.contains('.') || node.value.contains('e') || node.value.contains('E') {
					return 'f64'
				}
				return 'int'
			}
			return ''
		}
		ast.SelectorExpr {
			field_type := g.selector_field_type(node)
			if field_type != '' {
				return field_type
			}
			return g.get_expr_type(node)
		}
		ast.ParenExpr {
			return g.infer_numeric_expr_type(node.expr)
		}
		ast.PrefixExpr {
			return g.infer_numeric_expr_type(node.expr)
		}
		ast.InfixExpr {
			if node.op in [.plus, .minus, .mul, .div, .mod] {
				lhs_t := g.infer_numeric_expr_type(node.lhs)
				rhs_t := g.infer_numeric_expr_type(node.rhs)
				return promote_numeric_c_types(lhs_t, rhs_t)
			}
			return ''
		}
		else {
			return g.get_expr_type(node)
		}
	}
}

// get_expr_type returns the C type string for an expression
fn (mut g Gen) get_ident_expr_type(node ast.Ident) string {
	if local_type := g.get_local_var_c_type(node.name) {
		return local_type
	}
	if node.name == 'err' {
		return 'IError'
	}
	if global_type := g.global_var_type_for_ident(node.name) {
		return global_type
	}
	// Env pos.id O(1) lookup.
	if t := g.get_expr_type_from_env(node) {
		return t
	}
	// get_local_var_c_type already checked fn scope + runtime_local_types.
	// Only module and builtin scope fallbacks remain.
	if g.env != unsafe { nil } {
		if g.cur_module != '' {
			if mut mod_scope := g.env_scope(g.cur_module) {
				if obj := mod_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						raw := obj.typ()
						if raw is types.Alias {
							// For struct-valued aliases (like Color = gg.Color),
							// return the alias C type directly.
							alias_c := g.types_type_to_c(raw)
							if alias_c != '' && alias_c != 'int'
								&& alias_c !in ['voidptr', 'void*', 'charptr', 'char*', 'byteptr', 'u8*'] {
								return alias_c
							}
						} else {
							typ_name := g.types_type_to_c(raw)
							if typ_name != '' {
								return typ_name
							}
						}
					}
				}
			}
		}
		if g.cur_module != 'builtin' {
			if mut builtin_scope := g.env_scope('builtin') {
				if obj := builtin_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						raw := obj.typ()
						if raw !is types.Alias {
							typ_name := g.types_type_to_c(raw)
							if typ_name != '' {
								return typ_name
							}
						}
					}
				}
			}
		}
	}
	// Module-qualified ident: transformer mangles `module.name` -> `module__name`.
	// Split and look up the short name in the module's scope.
	if node.name.contains('__') {
		parts := node.name.split('__')
		if parts.len == 2 {
			mod_name := parts[0]
			short_name := parts[1]
			if mut target_scope := g.env_scope(mod_name) {
				if obj := target_scope.lookup_parent(short_name, 0) {
					if obj !is types.Module {
						raw := obj.typ()
						typ_name := g.types_type_to_c(raw)
						if typ_name != '' && typ_name != 'int' {
							return typ_name
						}
					}
				}
			}
		}
	}
	// Ident already tried env + all scopes above; avoid redundant second lookup below.
	return 'int'
}

fn (mut g Gen) get_expr_type_from_env_call_checked(node ast.Expr) ?string {
	if node is ast.CallExpr {
		if node.lhs is ast.Ident
			&& node.lhs.name in ['array__pop', 'array__pop_left', 'array__first', 'array__last']
			&& node.args.len > 0 {
			elem_type := g.infer_array_elem_type_from_expr(node.args[0])
			if elem_type != '' {
				return elem_type
			}
		} else if node.lhs is ast.SelectorExpr
			&& node.lhs.rhs.name in ['pop', 'pop_left', 'first', 'last'] {
			arr_expr := if node.args.len > 0 { node.args[0] } else { node.lhs.lhs }
			elem_type := g.infer_array_elem_type_from_expr(arr_expr)
			if elem_type != '' {
				return elem_type
			}
		}
		if ret := g.get_call_return_type(node.lhs, node.args) {
			if ret != '' {
				return ret
			}
		}
	} else if node is ast.CallOrCastExpr {
		if node.expr !is ast.EmptyExpr && g.call_or_cast_lhs_is_type(node.lhs) {
			cast_t := g.expr_type_to_c(node.lhs)
			if cast_t != '' && cast_t != 'int' {
				return cast_t
			}
		} else {
			call_args := if node.expr is ast.EmptyExpr {
				[]ast.Expr{}
			} else {
				[node.expr]
			}
			if ret := g.get_call_return_type(node.lhs, call_args) {
				if ret != '' {
					return ret
				}
			}
		}
	}
	return none
}

fn (mut g Gen) get_expr_type_from_env_index_checked(node ast.IndexExpr, t string) ?string {
	if node.expr is ast.RangeExpr {
		return none
	}
	if t.starts_with('Array_') && !t.starts_with('Array_fixed_') {
		if lhs_raw := g.get_raw_type(node.lhs) {
			if lhs_raw is types.Array {
				raw_elem := g.types_type_to_c(lhs_raw.elem_type)
				if raw_elem != '' && raw_elem != t {
					return raw_elem
				}
			} else if lhs_raw is types.Pointer && lhs_raw.base_type is types.Array {
				raw_elem := g.types_type_to_c(lhs_raw.base_type.elem_type)
				if raw_elem != '' && raw_elem != t {
					return raw_elem
				}
			}
		}
		return t
	}
	if t.starts_with('Array_fixed_') {
		lhs_type := g.get_expr_type(node.lhs).trim_right('*')
		if lhs_type.starts_with('Array_fixed_') {
			mut fixed_t := if t.ends_with('*') { t[..t.len - 1] } else { t }
			rest := fixed_t['Array_fixed_'.len..]
			last_underscore := rest.last_index('_') or { -1 }
			if last_underscore > 0 {
				return rest[..last_underscore]
			}
		}
		return t
	}
	if t in ['void*', 'voidptr'] {
		if elem_type := g.index_expr_elem_type_from_lhs(node) {
			return elem_type
		}
		container_type := g.get_expr_type(node.lhs)
		if container_type.starts_with('Array_fixed_') {
			rest := container_type['Array_fixed_'.len..]
			last_underscore := rest.last_index('_') or { -1 }
			if last_underscore > 0 {
				return rest[..last_underscore]
			}
		}
		return t
	}
	return none
}

fn (mut g Gen) get_expr_type_from_env_selector_checked(node ast.SelectorExpr, t string) ?string {
	if t in ['void*', 'voidptr'] {
		field_type := g.selector_field_type(node)
		if field_type != '' && field_type !in ['void*', 'voidptr'] {
			return field_type
		}
		return t
	}
	if t.starts_with('_result_') || t.starts_with('_option_') {
		field_type := g.selector_field_type(node)
		if field_type != '' && field_type != t {
			return field_type
		}
		return none
	}
	if g.active_generic_types.len > 0 {
		field_type := g.selector_field_type(node)
		if field_type != '' && field_type != t {
			return field_type
		}
		return t
	}
	if t == 'int' {
		field_type := g.selector_field_type(node)
		if field_type != '' && field_type != 'int' {
			return field_type
		}
		return none
	}
	field_type := g.selector_field_type(node)
	if field_type != '' && field_type != t && !is_generic_placeholder_c_type_name(field_type) {
		return field_type
	}
	return t
}

fn (mut g Gen) get_expr_type_from_env_infix_checked(node ast.InfixExpr, t string) ?string {
	if t in ['void*', 'voidptr'] && node.op in [.plus, .minus, .mul, .div, .mod] {
		return none
	}
	if t.starts_with('_result_') || t.starts_with('_option_') {
		lhs_type := g.get_expr_type(node.lhs)
		rhs_type := g.get_expr_type(node.rhs)
		if lhs_type.ends_with('*')
			|| lhs_type in ['void*', 'u8*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
			return lhs_type
		}
		if rhs_type.ends_with('*')
			|| rhs_type in ['void*', 'u8*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
			return rhs_type
		}
		return none
	}
	if t == 'int' && node.op in [.plus, .minus, .mul, .div, .mod] {
		numeric_type := g.infer_numeric_expr_type(node)
		if numeric_type != '' && numeric_type !in ['int', 'int_literal'] {
			return numeric_type
		}
		return none
	}
	return t
}

fn (mut g Gen) get_expr_type_from_env_checked(node ast.Expr, t string) ?string {
	if checked := g.get_expr_type_from_env_call_checked(node) {
		return checked
	}
	if t == 'bool' && node is ast.BasicLiteral && node.kind == .number {
		return 'int'
	}
	match node {
		ast.IndexExpr {
			if checked := g.get_expr_type_from_env_index_checked(node, t) {
				return checked
			}
		}
		ast.SelectorExpr {
			return g.get_expr_type_from_env_selector_checked(node, t) or { none }
		}
		ast.InfixExpr {
			return g.get_expr_type_from_env_infix_checked(node, t) or { none }
		}
		else {}
	}

	return t
}

fn (mut g Gen) get_expr_type(node ast.Expr) string {
	if node is ast.StringLiteral || node is ast.StringInterLiteral {
		if node is ast.StringLiteral && node.kind == .c {
			return 'char*'
		}
		return 'string'
	}
	if !expr_has_valid_data(node) {
		return ''
	}
	// For identifiers, check local/parameter types first (authoritative),
	// then fall back to env position lookup.
	if node is ast.Ident {
		return g.get_ident_expr_type(node)
	}
	// For IndexExpr on pointer-to-pointer or pointer-to-string types, prefer raw-type-based
	// inference over env (env may store the wrong type, e.g. char instead of char*).
	if node is ast.IndexExpr {
		if g.comptime_method_var != '' {
			if comptime_t := g.comptime_method_args_index_type(node) {
				return comptime_t
			}
		}
		if node.expr is ast.RangeExpr {
			if lhs_raw := g.get_raw_type(node.lhs) {
				match lhs_raw {
					types.Array {
						return g.types_type_to_c(lhs_raw)
					}
					types.Pointer {
						if lhs_raw.base_type is types.Array {
							return g.types_type_to_c(lhs_raw.base_type)
						}
					}
					else {}
				}
			}
			return g.get_expr_type(node.lhs)
		}
		if elem_type := g.index_expr_elem_type_from_lhs(node) {
			if elem_type == 'u8' || elem_type.starts_with('Array_fixed_') {
				return elem_type
			}
		}
		// Cross-check: if LHS has a known local C type ending in **, derive element type
		if node.lhs is ast.Ident {
			if mut fn_scope := g.ensure_cur_fn_scope() {
				if obj := fn_scope.lookup_parent(node.lhs.name, 0) {
					raw_type := obj.typ()
					if !type_contains_generic_placeholder(raw_type)
						|| g.active_generic_types.len > 0 {
						if elem_type := g.index_elem_c_type_from_raw(raw_type) {
							return elem_type
						}
					}
				}
			}
		}
		if node.lhs is ast.Ident {
			if local_type := g.get_local_var_c_type(node.lhs.name) {
				if local_type.ends_with('**') {
					return local_type[..local_type.len - 1]
				}
				if raw_type := g.resolve_c_type_to_raw(local_type) {
					if elem_type := g.index_elem_c_type_from_raw(raw_type) {
						return elem_type
					}
				}
				elem_type := g.array_alias_elem_type_from_c_type(local_type)
				if elem_type != '' {
					return elem_type
				}
			}
		}
		if lhs_raw := g.get_raw_type(node.lhs) {
			if lhs_raw is types.Pointer {
				if lhs_raw.base_type is types.Pointer || lhs_raw.base_type is types.String {
					return g.types_type_to_c(lhs_raw.base_type)
				}
				if lhs_raw.base_type is types.ArrayFixed {
					return g.types_type_to_c(lhs_raw.base_type.elem_type)
				}
			}
		}
		if g.active_generic_types.len > 0 {
			if raw_type := g.get_raw_type(node) {
				if concrete_type := g.concrete_c_type_from_active_generic(raw_type) {
					return concrete_type
				}
			}
		}
	}
	// Tuple: always infer from element types, env position can be wrong.
	if node is ast.Tuple {
		mut elem_types := []string{cap: node.exprs.len}
		for expr in node.exprs {
			elem_types << g.get_expr_type(expr)
		}
		return g.register_tuple_alias(elem_types)
	}
	// Comptime field selector type resolution
	if g.comptime_field_var != '' && node is ast.SelectorExpr {
		ct_type := g.get_comptime_selector_type(node)
		if ct_type != '' {
			return ct_type
		}
	}
	if node is ast.SelectorExpr {
		if global_type := g.global_var_type_for_selector(node) {
			return global_type
		}
	}
	// Generic type parameter access: T.name is emitted as a static V string.
	if node is ast.SelectorExpr && node.rhs.name == 'name' && node.lhs is ast.Ident {
		lhs_name := node.lhs.name
		if is_generic_placeholder_type_name(lhs_name) {
			return 'string'
		}
	}
	// SelectorExpr .argN on a tuple-typed LHS: extract the N-th field type
	// from the tuple alias. Transformer expands `a, b := call()` to
	// `_tuple_tN := call(); a := _tuple_tN.arg0; b := _tuple_tN.arg1`,
	// and the env may return the wrong type for these synthetic selectors.
	if node is ast.SelectorExpr {
		field_name := node.rhs.name
		if field_name.starts_with('arg') {
			lhs_type := g.get_expr_type(node.lhs)
			if lhs_type.starts_with('Tuple_') {
				if field_types := g.tuple_aliases[lhs_type] {
					idx := field_name['arg'.len..].int()
					if idx >= 0 && idx < field_types.len {
						return field_types[idx]
					}
				}
			}
		}
	}
	if node is ast.InitExpr {
		typ_name := g.expr_type_to_c(node.typ)
		if g.type_expr_has_metadata(node.typ) {
			return typ_name
		}
		if node.typ is ast.Ident {
			type_ident := node.typ as ast.Ident
			if is_generic_placeholder_type_name(type_ident.name) {
				return typ_name
			}
		}
		return g.qualify_module_local_type_name(typ_name)
	}
	if node is ast.PrefixExpr && node.op == .amp && node.expr is ast.InitExpr {
		init_expr := node.expr as ast.InitExpr
		mut typ := g.expr_type_to_c(init_expr.typ)
		mut is_generic_init := false
		metadata_backed := g.type_expr_has_metadata(init_expr.typ)
		if init_expr.typ is ast.Ident {
			type_ident := init_expr.typ as ast.Ident
			is_generic_init = is_generic_placeholder_type_name(type_ident.name)
		}
		if !is_generic_init && !metadata_backed {
			typ = g.qualify_module_local_type_name(typ)
		}
		return typ + '*'
	}
	if node is ast.PrefixExpr && node.op == .mul {
		// A dereference expression is typed from its operand.  Env metadata can
		// still carry the operand pointer type for the whole expression.
		if deref_type := g.deref_address_of_cast_result_type(node) {
			return deref_type
		}
		inner_type := g.get_expr_type(node.expr)
		if inner_type.ends_with('*') {
			return inner_type[..inner_type.len - 1]
		}
		if inner_type != '' {
			return inner_type
		}
	}
	if node is ast.UnsafeExpr {
		if unsafe_type := g.unsafe_expr_result_type(node) {
			return unsafe_type
		}
	}
	if node is ast.CallExpr {
		if node.lhs is ast.Ident && node.lhs.name == 'array__slice' && node.args.len > 0 {
			first_arg_type := g.get_expr_type(node.args[0]).trim_right('*')
			if g.expr_resolves_to_string(node.args[0], first_arg_type) {
				return 'string'
			}
		}
		array_method_elem_type := g.infer_array_method_elem_type(node)
		if array_method_elem_type != '' {
			return array_method_elem_type
		}
		if node.args.len == 1 {
			if cast_type := g.fn_type_alias_cast_type(node.lhs) {
				return cast_type
			}
		}
		if ret := g.get_call_return_type(node.lhs, node.args) {
			return ret
		}
		fn_ptr_ret := g.fn_pointer_return_type(node.lhs)
		if fn_ptr_ret != '' {
			return fn_ptr_ret
		}
	}
	if node is ast.CallOrCastExpr && !g.call_or_cast_lhs_is_type(node.lhs) {
		if ret := g.get_call_return_type(node.lhs, [node.expr]) {
			return ret
		}
	}
	if node is ast.AsCastExpr {
		cast_type := g.expr_type_to_c(node.typ)
		if cast_type != '' {
			return cast_type
		}
	}
	if node is ast.InfixExpr {
		overloaded_ret := g.overloaded_arithmetic_result_type(node)
		if overloaded_ret != '' {
			return overloaded_ret
		}
	}
	// Try environment lookup
	if t := g.get_expr_type_from_env(node) {
		if checked := g.get_expr_type_from_env_checked(node, t) {
			return checked
		}
	}
	// Fallback inference
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true || node.kind == .key_false {
				return 'bool'
			}
			return 'int'
		}
		ast.StringLiteral {
			return 'string'
		}
		ast.SelectorExpr {
			field_type := g.selector_field_type(node)
			if field_type != '' {
				return field_type
			}
			return 'int'
		}
		ast.InfixExpr {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .and, .logical_or] {
				return 'bool'
			}
			if node.op in [.plus, .minus, .mul, .div, .mod] {
				numeric_type := g.infer_numeric_expr_type(node)
				if numeric_type != '' && numeric_type != 'int_literal' {
					return numeric_type
				}
			}
			lhs_t := g.get_expr_type(node.lhs)
			rhs_t := g.get_expr_type(node.rhs)
			if node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .left_shift,
				.right_shift] {
				if lhs_t.ends_with('Fn') || rhs_t.ends_with('Fn') {
					return 'u64'
				}
			}
			return lhs_t
		}
		ast.ParenExpr {
			return g.get_expr_type(node.expr)
		}
		ast.PrefixExpr {
			if node.op == .arrow {
				if elem_type := g.channel_elem_type_from_expr(node.expr) {
					return elem_type
				}
				return 'void*'
			}
			if node.op == .mul {
				// Dereference: *(T*)(x) -> T
				if deref_type := g.deref_address_of_cast_result_type(node) {
					return deref_type
				}
				inner_t := g.get_expr_type(node.expr)
				if inner_t.ends_with('*') {
					return inner_t[..inner_t.len - 1]
				}
				return inner_t
			}
			if node.op == .amp {
				if node.expr is ast.SelectorExpr {
					field_type := g.c_pointer_cast_selector_field_type(node.expr)
					if field_type != '' {
						return field_type
					}
					c_typedef := g.c_typedef_for_interface_object_access(node.expr)
					if c_typedef != '' {
						if field_type2 := g.lookup_struct_field_type_by_name(c_typedef,
							node.expr.rhs.name)
						{
							return field_type2
						}
						return 'void*'
					}
				}
				return g.get_expr_type(node.expr) + '*'
			}
			return g.get_expr_type(node.expr)
		}
		ast.UnsafeExpr {
			// Infer from last statement in the block
			if node.stmts.len > 0 {
				last := node.stmts[node.stmts.len - 1]
				if last is ast.ExprStmt {
					return g.get_expr_type(last.expr)
				}
			}
			return 'int'
		}
		ast.IfExpr {
			return g.get_if_expr_type(&node)
		}
		ast.IndexExpr {
			unwrapped_lhs := strip_expr_wrappers(node.lhs)
			if unwrapped_lhs is ast.CallExpr {
				call_name := g.resolve_call_name(unwrapped_lhs.lhs, unwrapped_lhs.args.len)
				if call_name in ['new_array_from_c_array', 'builtin__new_array_from_c_array',
					'builtin__new_array_from_c_array_noscan'] {
					elem_type := g.infer_array_elem_type_from_expr(unwrapped_lhs)
					if elem_type != '' && elem_type != 'array' && elem_type != 'int'
						&& elem_type != 'void' {
						return elem_type
					}
				}
			}
			if node.lhs is ast.SelectorExpr {
				elem_type := g.fixed_array_selector_elem_type(node.lhs)
				if elem_type != '' {
					return elem_type
				}
			}
			// Try to get element type from LHS type
			if raw_type := g.get_raw_type(node.lhs) {
				raw_lhs_expr := strip_expr_wrappers(node.lhs)
				match raw_type {
					types.Array {
						elem_type := g.types_type_to_c(raw_type.elem_type)
						if raw_lhs_expr is ast.ArrayInitExpr {
							value_elem := g.infer_array_init_value_elem_type(raw_lhs_expr)
							specialized := specialized_generic_elem_type_from_value(elem_type,
								value_elem)
							if specialized != '' {
								return specialized
							}
						}
						return elem_type
					}
					types.ArrayFixed {
						return g.types_type_to_c(raw_type.elem_type)
					}
					types.Map {
						return g.types_type_to_c(raw_type.value_type)
					}
					types.Alias {
						// Avoid alias payload dereference in self-host fallback path.
					}
					types.Pointer {
						match raw_type.base_type {
							types.Pointer {
								return g.types_type_to_c(raw_type.base_type)
							}
							types.Array {
								elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
								if raw_lhs_expr is ast.ArrayInitExpr {
									value_elem := g.infer_array_init_value_elem_type(raw_lhs_expr)
									specialized := specialized_generic_elem_type_from_value(elem_type,
										value_elem)
									if specialized != '' {
										return specialized
									}
								}
								return elem_type
							}
							types.ArrayFixed {
								return g.types_type_to_c(raw_type.base_type.elem_type)
							}
							types.Map {
								return g.types_type_to_c(raw_type.base_type.value_type)
							}
							types.Alias {
								// Avoid nested alias payload dereference in self-host fallback path.
							}
							types.String {
								return 'string'
							}
							else {}
						}
					}
					types.String {
						return 'u8'
					}
					else {}
				}
			}
			lhs_type := g.get_expr_type(node.lhs)
			// Cross-check: if LHS is a pointer type (e.g. char**), derive element type
			if lhs_type.ends_with('**') {
				return lhs_type[..lhs_type.len - 1]
			}
			if lhs_type.starts_with('Map_') {
				if info := g.ensure_map_type_info(lhs_type) {
					return info.value_c_type
				}
			}
			if lhs_type.starts_with('Array_fixed_') {
				if info := g.collected_fixed_array_types[lhs_type] {
					return info.elem_type
				}
				// Extract element type from mangled name: Array_fixed_TYPE_SIZE
				trailing := lhs_type['Array_fixed_'.len..]
				last_underscore := trailing.last_index_u8(`_`)
				if last_underscore > 0 {
					return trailing[..last_underscore]
				}
			} else {
				unwrapped_lhs2 := strip_expr_wrappers(node.lhs)
				elem := array_alias_elem_type(lhs_type)
				if elem != '' {
					if elem == 'int' && unwrapped_lhs2 is ast.CallExpr {
						elem_type := g.infer_array_elem_type_from_expr(unwrapped_lhs2)
						if elem_type != '' && elem_type != 'array' && elem_type != 'int' {
							return elem_type
						}
					}
					return elem
				}
				if lhs_type == 'array' && unwrapped_lhs2 is ast.CallExpr {
					elem_type := g.infer_array_elem_type_from_expr(unwrapped_lhs2)
					if elem_type != '' && elem_type != 'array' && elem_type != 'int' {
						return elem_type
					}
				}
			}
			return 'int'
		}
		ast.InitExpr {
			typ_name := g.expr_type_to_c(node.typ)
			if g.type_expr_has_metadata(node.typ) {
				return typ_name
			}
			return g.qualify_module_local_type_name(typ_name)
		}
		ast.ArrayInitExpr {
			is_fixed_marker := array_init_has_fixed_len_marker(node)
			mut elem := g.extract_array_elem_type(node.typ)
			value_elem := g.infer_array_init_value_elem_type(node)
			specialized := specialized_generic_elem_type_from_value(elem, value_elem)
			if specialized != '' {
				elem = specialized
			}
			if (elem == '' || elem == 'int') && value_elem != '' && value_elem != 'int' {
				elem = value_elem
			}
			if elem != '' {
				if g.is_dynamic_array_type(node.typ) && !is_fixed_marker {
					return 'Array_' + elem
				}
				// Get size from type annotation if available, fallback to exprs.len
				mut fixed_size := node.exprs.len
				if node.typ is ast.Type && node.typ is ast.ArrayFixedType {
					fixed_typ := node.typ as ast.ArrayFixedType
					resolved_size := g.expr_to_int_str_with_env(fixed_typ.len)
					if resolved_size != '0' {
						fixed_size = resolved_size.int()
					}
				}
				if fixed_size == 0 && node.exprs.len > 0 {
					fixed_size = node.exprs.len
				}
				// For zero-size fixed arrays with init (e.g. [3][3]int{init:...}),
				// use ArrayFixedType-based size from the parent type annotation
				if fixed_size == 0 && node.len !is ast.EmptyExpr {
					resolved_size2 := g.expr_to_int_str_with_env(node.len)
					if resolved_size2 != '0' {
						fixed_size = resolved_size2.int()
					}
				}
				fixed_name := 'Array_fixed_' + mangle_alias_component(elem) + '_' + fixed_size.str()
				g.register_alias_type(fixed_name)
				g.collected_fixed_array_types[fixed_name] = FixedArrayInfo{
					elem_type: elem
					size:      fixed_size
				}
				return fixed_name
			}
			// No type annotation: infer element type from first expression for fixed arrays
			if node.exprs.len > 0 {
				elem = g.get_expr_type(node.exprs[0])
				if elem == 'int_literal' {
					elem = 'int'
				}
				if elem == 'float_literal' {
					elem = 'f64'
				}
				if elem != '' {
					fixed_name := 'Array_fixed_' + mangle_alias_component(elem) + '_' +
						node.exprs.len.str()
					g.register_alias_type(fixed_name)
					g.collected_fixed_array_types[fixed_name] = FixedArrayInfo{
						elem_type: elem
						size:      node.exprs.len
					}
					return fixed_name
				}
			}
			return 'array'
		}
		ast.CallExpr {
			fn_ptr_ret := g.fn_pointer_return_type(node.lhs)
			if fn_ptr_ret != '' {
				return fn_ptr_ret
			}
			if node.lhs is ast.Ident
				&& node.lhs.name in ['array__get_u64', 'array__get_u32', 'array__get_u16', 'array__get_u8']
				&& node.args.len > 0 {
				receiver_expr := if node.args[0] is ast.PrefixExpr
					&& (node.args[0] as ast.PrefixExpr).op == .mul {
					(node.args[0] as ast.PrefixExpr).expr
				} else {
					node.args[0]
				}
				if g.get_expr_type(receiver_expr).trim_right('*') == 'binary__DecodeState' {
					return '_result_' + node.lhs.name['array__get_'.len..]
				}
			}
			array_method_elem_type := g.infer_array_method_elem_type(node)
			if array_method_elem_type != '' {
				return array_method_elem_type
			}
			if ret := g.get_call_return_type(node.lhs, node.args) {
				return ret
			}
			return 'int'
		}
		ast.CallOrCastExpr {
			if node.expr !is ast.EmptyExpr && g.call_or_cast_lhs_is_type(node.lhs) {
				return g.expr_type_to_c(node.lhs)
			}
			call_args := if node.expr is ast.EmptyExpr {
				[]ast.Expr{}
			} else {
				[node.expr]
			}
			if ret := g.get_call_return_type(node.lhs, call_args) {
				return ret
			}
			return 'int'
		}
		ast.CastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.AsCastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.StringInterLiteral {
			return 'string'
		}
		ast.Tuple {
			mut elem_types := []string{cap: node.exprs.len}
			for expr in node.exprs {
				elem_types << g.get_expr_type(expr)
			}
			return g.register_tuple_alias(elem_types)
		}
		else {
			return 'int'
		}
	}
}

fn (mut g Gen) unsafe_expr_result_type(node ast.UnsafeExpr) ?string {
	if node.stmts.len == 0 {
		return none
	}
	last := node.stmts[node.stmts.len - 1]
	if last is ast.ExprStmt {
		if last.expr is ast.SelectorExpr && last.expr.rhs.name == 'data'
			&& last.expr.lhs is ast.Ident {
			result_name := last.expr.lhs.name
			for stmt in node.stmts[..node.stmts.len - 1] {
				if stmt is ast.AssignStmt && stmt.op == .decl_assign && stmt.lhs.len == 1 {
					stmt_lhs := stmt.lhs[0]
					if stmt_lhs is ast.Ident && stmt_lhs.name == result_name {
						wrapper_type := g.get_expr_type(stmt.rhs[0])
						if wrapper_type.starts_with('_result_') {
							value_type := g.result_value_type(wrapper_type)
							if value_type != '' && value_type != 'void' {
								return value_type
							}
						} else if wrapper_type.starts_with('_option_') {
							value_type := option_value_type(wrapper_type)
							if value_type != '' && value_type != 'void' {
								return value_type
							}
						}
					}
				}
			}
		}
		result_type := g.get_expr_type(last.expr)
		if result_type != '' && result_type != 'int' && result_type != 'void' {
			return result_type
		}
	}
	return none
}

fn (mut g Gen) deref_address_of_cast_result_type(node ast.PrefixExpr) ?string {
	if node.op != .mul {
		return none
	}
	inner := g.unwrap_parens(node.expr)
	match inner {
		ast.CastExpr {
			target_type := g.expr_type_to_c(inner.typ)
			if is_type_name_pointer_like(target_type) {
				return strip_one_pointer_type_name(target_type)
			}
		}
		ast.PrefixExpr {
			if inner.op != .amp {
				return none
			}
			return g.cast_target_value_type(inner.expr)
		}
		else {}
	}

	return none
}

fn (mut g Gen) cast_target_value_type(expr ast.Expr) ?string {
	unwrapped := g.unwrap_parens(expr)
	match unwrapped {
		ast.CallOrCastExpr {
			if unwrapped.expr !is ast.EmptyExpr && g.call_or_cast_lhs_is_type(unwrapped.lhs) {
				typ := g.expr_type_to_c(unwrapped.lhs)
				if typ != '' {
					return typ
				}
			}
		}
		ast.CallExpr {
			if unwrapped.args.len == 1 && g.call_or_cast_lhs_is_type(unwrapped.lhs) {
				typ := g.expr_type_to_c(unwrapped.lhs)
				if typ != '' {
					return typ
				}
			}
		}
		ast.CastExpr {
			typ := g.expr_type_to_c(unwrapped.typ)
			if typ != '' {
				return typ
			}
		}
		else {}
	}

	return none
}

// comptime_method_args_index_type detects index/slice expressions on the comptime
// `method.args` selector (e.g. `method.args[i]`, `method.args[1..]`,
// `(method.args[1..])[i]`) and returns the corresponding element/container C type.
// Returns none when the expression does not chain back to `method.args`.
fn (g &Gen) comptime_method_args_index_type(node ast.IndexExpr) ?string {
	if g.comptime_method_var == '' {
		return none
	}
	is_slice := node.expr is ast.RangeExpr
	mut cur := strip_expr_wrappers(node.lhs)
	for {
		if cur is ast.SelectorExpr {
			if cur.lhs is ast.Ident && cur.lhs.name == g.comptime_method_var
				&& cur.rhs.name == 'args' {
				return if is_slice { 'Array_FunctionParam' } else { 'FunctionParam' }
			}
			return none
		}
		if cur is ast.IndexExpr {
			cur = strip_expr_wrappers(cur.lhs)
			continue
		}
		// Transformer lowers `method.args[lo..hi]` to
		// `array__slice[_ni](method.args, lo, hi)`. Drill into the first arg.
		if cur is ast.CallExpr {
			if cur.lhs is ast.Ident
				&& cur.lhs.name in ['array__slice', 'array__slice_ni', 'builtin__array__slice', 'builtin__array__slice_ni']
				&& cur.args.len > 0 {
				cur = strip_expr_wrappers(cur.args[0])
				continue
			}
			return none
		}
		return none
	}
	return none
}

fn (mut g Gen) index_expr_elem_type_from_lhs(node ast.IndexExpr) ?string {
	if node.expr is ast.RangeExpr {
		return none
	}
	if node.lhs is ast.Ident {
		if local_type := g.get_local_var_c_type(node.lhs.name) {
			local_c_type := local_type.trim_space()
			if local_c_type.ends_with('*') {
				return none
			}
			lhs_type := local_c_type.trim_right('*')
			if lhs_type == 'string' {
				return 'u8'
			}
			elem := g.array_alias_elem_type_from_c_type(lhs_type)
			if elem != '' {
				return elem
			}
			if lhs_type != '' && lhs_type != 'int' {
				return none
			}
		}
	}
	if node.lhs is ast.SelectorExpr {
		field_type := g.selector_field_type(node.lhs).trim_space().trim_right('*')
		if field_type != '' {
			elem := g.array_alias_elem_type_from_c_type(field_type)
			if elem != '' {
				return elem
			}
		}
	}
	if raw_type := g.get_raw_type(node.lhs) {
		match raw_type {
			types.Array {
				return g.types_type_to_c(raw_type.elem_type)
			}
			types.Pointer {
				if raw_type.base_type is types.Array {
					return g.types_type_to_c(raw_type.base_type.elem_type)
				}
			}
			types.String {
				return 'u8'
			}
			types.Alias {
				if raw_type.base_type is types.String {
					return 'u8'
				}
			}
			else {}
		}
	}
	lhs_type := g.get_expr_type(node.lhs)
	if lhs_type == 'string' {
		return 'u8'
	}
	elem := array_alias_elem_type(lhs_type)
	if elem != '' {
		return elem
	}
	return none
}

// expr_type_to_c converts an AST type expression to a C type string
fn (mut g Gen) vec_generic_type_to_c(lhs_name string, arg_type string) string {
	if !g.should_map_vec_generic_to_simd(lhs_name) {
		return ''
	}
	base_name := if lhs_name.contains('__') { lhs_name.all_after_last('__') } else { lhs_name }
	if local_alias := g.local_vec_simd_alias_to_c(base_name, arg_type) {
		return local_alias
	}
	prefix := if lhs_name.contains('__') {
		lhs_name.all_before_last('__') + '__'
	} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		g.cur_module + '__'
	} else {
		''
	}
	alias_name := vec_simd_alias_name(base_name, arg_type)
	if alias_name != '' {
		return prefix + alias_name
	}
	return ''
}

fn cleanc_selector_expr_parts(expr ast.SelectorExpr) []string {
	mut parts := []string{}
	collect_cleanc_selector_expr_parts(ast.Expr(expr), mut parts)
	return parts
}

fn collect_cleanc_selector_expr_parts(expr ast.Expr, mut parts []string) bool {
	match expr {
		ast.Ident {
			parts << expr.name
			return true
		}
		ast.SelectorExpr {
			if !collect_cleanc_selector_expr_parts(expr.lhs, mut parts) {
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

fn (mut g Gen) resolve_active_generic_expr_params(params []ast.Expr) []ast.Expr {
	if g.active_generic_types.len == 0 {
		return params.clone()
	}
	mut resolved := params.clone()
	for i, param in params {
		param_name := param.name()
		if is_generic_placeholder_type_name(param_name) {
			if concrete := g.active_generic_types[param_name] {
				resolved[i] = ast.Expr(ast.Ident{
					name: g.types_type_to_c(concrete)
					pos:  param.pos()
				})
			}
		}
	}
	return resolved
}

fn (mut g Gen) generic_type_lhs_to_c(e ast.Expr) string {
	if e is ast.Ident {
		name := e.name
		if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
			'f32', 'f64', 'usize', 'isize'] {
			return name
		}
		if name == 'bool' {
			return 'bool'
		}
		if name == 'string' {
			return 'string'
		}
		if name == 'chan' {
			return 'chan'
		}
		if name == 'voidptr' {
			return 'void*'
		}
		if name == 'charptr' {
			return 'char*'
		}
		if name == 'byteptr' {
			return 'u8*'
		}
		if concrete := g.resolve_active_generic_type(name) {
			return g.types_type_to_c(concrete)
		}
		if g.is_module_local_type(name) {
			return g.cur_module + '__' + name
		}
		if name.starts_with('[]') || name.starts_with('map[') || name.starts_with('&') {
			return g.c_type_from_type_name(name)
		}
		if name == 'Error' && !g.c_type_name_declared_in_module(g.cur_module, name) {
			return 'Error'
		}
		if name == 'IError' {
			return 'IError'
		}
		specialized_name := g.c_type_from_possible_specialization_token(name)
		if specialized_name != name {
			return specialized_name
		}
		if generic_type := g.generic_placeholder_c_type_from_name(name) {
			return generic_type
		}
		if imported_name := g.imported_symbol_c_type(name) {
			return imported_name
		}
		if name.starts_with('C.') {
			return name.all_after('C.')
		}
		if name.contains('.') {
			return name.replace('.', '__')
		}
		unmangled_name := unmangle_c_ptr_type(name)
		if unmangled_name != name {
			return unmangled_name
		}
		if mapped := g.vec_type_suffix_alias_to_c(name) {
			g.register_alias_type(mapped)
			return mapped
		}
		if name in ['SimdFloat4', 'SimdInt4', 'SimdU32_4', 'SimdFloat2', 'SimdUint2', 'SimdI32_2']
			&& g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			return '${g.cur_module}__${name}'
		}
		return name
	}
	return g.expr_type_to_c(e)
}

fn (mut g Gen) expr_type_to_c(e ast.Expr) string {
	if !expr_has_valid_data(e) {
		return 'void'
	}
	match e {
		ast.Ident {
			name := e.name
			if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
				'f32', 'f64', 'usize', 'isize'] {
				return name
			}
			if name == 'bool' {
				return 'bool'
			}
			if name == 'string' {
				return 'string'
			}
			if name == 'chan' {
				return 'chan'
			}
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			if name == 'byteptr' {
				return 'u8*'
			}
			if concrete := g.resolve_active_generic_type(name) {
				return g.types_type_to_c(concrete)
			}
			if env_type := g.get_expr_type_from_env(e) {
				env_c := env_type.trim_space()
				if env_c != '' && env_c != 'int' {
					return env_c
				}
			}
			if raw_type := g.get_raw_type(e) {
				raw_c := g.types_type_to_c(raw_type).trim_space()
				if raw_c != '' && raw_c != 'int' {
					return raw_c
				}
			}
			if g.is_module_local_type(name) {
				return g.cur_module + '__' + name
			}
			if name.starts_with('[]') || name.starts_with('map[') || name.starts_with('&') {
				return g.c_type_from_type_name(name)
			}
			if name == 'Error' && !g.c_type_name_declared_in_module(g.cur_module, name) {
				return 'Error'
			}
			if name == 'IError' {
				return 'IError'
			}
			specialized_name := g.c_type_from_possible_specialization_token(name)
			if specialized_name != name {
				return specialized_name
			}
			if generic_type := g.generic_placeholder_c_type_from_name(name) {
				return generic_type
			}
			if imported_name := g.imported_symbol_c_type(name) {
				return imported_name
			}
			if name.starts_with('C.') {
				return name.all_after('C.')
			}
			if name.contains('.') {
				return name.replace('.', '__')
			}
			// Detect mangled pointer aliases from transformer/checker
			// (e.g. FILEptr -> FILE*, viper__Appptr -> viper__App*).
			unmangled_name := unmangle_c_ptr_type(name)
			if unmangled_name != name {
				return unmangled_name
			}
			if mapped := g.vec_type_suffix_alias_to_c(name) {
				g.register_alias_type(mapped)
				return mapped
			}
			if name in ['SimdFloat4', 'SimdInt4', 'SimdU32_4', 'SimdFloat2', 'SimdUint2', 'SimdI32_2']
				&& g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
				return '${g.cur_module}__${name}'
			}
			g.register_alias_type(name)
			return name
		}
		ast.PrefixExpr {
			if e.op == .amp {
				return g.expr_type_to_c(e.expr) + '*'
			}
			if e.op == .ellipsis {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.expr))
				array_type := 'Array_' + elem_type
				g.register_alias_type(array_type)
				return array_type
			}
			return 'void*'
		}
		ast.SelectorExpr {
			parts := cleanc_selector_expr_parts(e)
			if parts.len >= 2 {
				// C interop types: C.FILE -> FILE, C.tm -> struct tm
				if parts.len == 2 && parts[0] == 'C' {
					name := parts[1]
					// Known C typedefs don't need 'struct' prefix
					if name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t',
						'pthread_cond_t', 'pthread_rwlock_t', 'pthread_attr_t', 'pthread_condattr_t',
						'pthread_rwlockattr_t', 'atomic_uintptr_t', 'jmp_buf', 'sigjmp_buf',
						'sigset_t', 'size_t', 'ssize_t', 'off_t', 'mode_t', 'pid_t', 'uid_t', 'gid_t',
						'time_t', 'clock_t', 'socklen_t', 'dev_t', 'ino_t', 'nlink_t', 'blksize_t',
						'blkcnt_t', 'cc_t', 'speed_t', 'tcflag_t', 'fd_set',
						'mach_timebase_info_data_t'] {
						return name
					}
					// @[typedef] C structs are typedefs, not raw structs
					if name in g.typedef_c_types {
						return name
					}
					return 'struct ' + name
				}
				module_name := parts[parts.len - 2]
				type_name := parts[parts.len - 1]
				mod_name := g.resolve_module_name(module_name)
				mut qualified := mod_name + '__' + type_name
				if type_name !in ['Vec2', 'Vec4'] {
					if mapped := g.vec_type_suffix_alias_to_c(qualified) {
						qualified = mapped
					}
				}
				return qualified
			}
			return g.expr_type_to_c(e.lhs) + '__' + e.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			lhs_name := g.generic_type_lhs_to_c(e.lhs)
			if alias_name := g.fn_type_alias_name_for_base_expr(e.lhs) {
				return alias_name
			}
			if e.expr is ast.LifetimeExpr {
				return lhs_name
			}
			resolved_params := g.resolve_active_generic_expr_params([e.expr])
			resolved_expr := resolved_params[0]
			arg_type := g.expr_type_to_c(resolved_expr)
			mapped := g.vec_generic_type_to_c(lhs_name, arg_type)
			if mapped != '' {
				g.register_alias_type(mapped)
				return mapped
			}
			// Record binding and resolve multi-instantiation
			arg_name := resolved_expr.name()
			if !is_generic_placeholder_type_name(arg_name) {
				struct_base := if lhs_name.contains('__') {
					lhs_name.all_after_last('__')
				} else {
					lhs_name
				}
				g.record_generic_struct_bindings(struct_base, lhs_name, resolved_params)
			}
			return g.resolve_generic_struct_c_name(lhs_name, resolved_params)
		}
		ast.GenericArgs {
			lhs_name := g.generic_type_lhs_to_c(e.lhs)
			if alias_name := g.fn_type_alias_name_for_base_expr(e.lhs) {
				return alias_name
			}
			concrete_args := non_lifetime_generic_args(e.args)
			if concrete_args.len > 0 {
				resolved_args := g.resolve_active_generic_expr_params(concrete_args)
				arg_type := g.expr_type_to_c(resolved_args[0])
				mapped := g.vec_generic_type_to_c(lhs_name, arg_type)
				if mapped != '' {
					g.register_alias_type(mapped)
					return mapped
				}
				// Record binding for multi-instantiation (e.g. LinkedList[StructFieldInfo])
				mut all_concrete := true
				for a in resolved_args {
					if is_generic_placeholder_type_name(a.name()) {
						all_concrete = false
						break
					}
				}
				if all_concrete {
					struct_base := if lhs_name.contains('__') {
						lhs_name.all_after_last('__')
					} else {
						lhs_name
					}
					g.record_generic_struct_bindings(struct_base, lhs_name, resolved_args)
				}
				return g.resolve_generic_struct_c_name(lhs_name, resolved_args)
			}
			return lhs_name
		}
		ast.EmptyExpr {
			return 'void'
		}
		ast.Type {
			if e is ast.ArrayType {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.elem_type))
				array_type := 'Array_' + elem_type
				g.register_alias_type(array_type)
				return array_type
			}
			if e is ast.ArrayFixedType {
				elem_type := mangle_alias_component(g.expr_type_to_c(e.elem_type))
				size_str := g.expr_to_int_str_with_env(e.len)
				fixed_type := 'Array_fixed_' + elem_type + '_' + size_str
				g.register_alias_type(fixed_type)
				g.collected_fixed_array_types[fixed_type] = FixedArrayInfo{
					elem_type: g.expr_type_to_c(e.elem_type)
					size:      size_str.int()
				}
				return fixed_type
			}
			if e is ast.ChannelType {
				return 'chan'
			}
			if e is ast.TupleType {
				mut elem_types := []string{cap: e.types.len}
				for t in e.types {
					elem_types << g.expr_type_to_c(t)
				}
				return g.register_tuple_alias(elem_types)
			}
			if e is ast.MapType {
				key_c := g.expr_type_to_c(e.key_type)
				value_c := g.expr_type_to_c(e.value_type)
				key_type := mangle_alias_component(key_c)
				value_type := mangle_alias_component(value_c)
				map_type := 'Map_' + key_type + '_' + value_type
				g.register_alias_type(map_type)
				g.collected_map_types[map_type] = MapTypeInfo{
					key_c_type:   key_c
					value_c_type: value_c
				}
				return map_type
			}
			if e is ast.OptionType {
				base_type :=
					g.option_result_payload_alias_component(g.qualify_module_local_generic_c_name(g.expr_type_to_c(e.base_type)))
				option_type := g.qualify_module_local_generic_c_name('_option_' + base_type)
				g.register_alias_type(option_type)
				return option_type
			}
			if e is ast.PointerType {
				return g.expr_type_to_c(e.base_type) + '*'
			}
			if e is ast.ResultType {
				base_type :=
					g.option_result_payload_alias_component(g.qualify_module_local_generic_c_name(g.expr_type_to_c(e.base_type)))
				result_type := g.qualify_module_local_generic_c_name('_result_' + base_type)
				g.register_alias_type(result_type)
				return result_type
			}
			if e is ast.FnType {
				if e.return_type !is ast.EmptyExpr {
					_ = g.expr_type_to_c(e.return_type)
				}
				for param in e.params {
					_ = g.expr_type_to_c(param.typ)
				}
				return 'void*'
			}
			if e is ast.NilType {
				return 'void*'
			}
			if e is ast.NoneType {
				return 'None__'
			}
			if e is ast.GenericType {
				// Generic struct type like LinkedList[ValueInfo] or Node[T].
				base_name := g.generic_type_lhs_to_c(e.name)
				concrete_params := non_lifetime_generic_args(e.params)
				if concrete_params.len > 0 {
					arg_type := g.expr_type_to_c(concrete_params[0])
					mapped := g.vec_generic_type_to_c(base_name, arg_type)
					if mapped != '' {
						g.register_alias_type(mapped)
						return mapped
					}
					struct_base := if base_name.contains('__') {
						base_name.all_after_last('__')
					} else {
						base_name
					}
					// Resolve placeholder params (e.g., T → StructFieldInfo) via active_generic_types
					mut resolved_params := concrete_params.clone()
					if g.active_generic_types.len > 0 {
						for i, p in concrete_params {
							pname := p.name()
							if is_generic_placeholder_type_name(pname) {
								if concrete := g.active_generic_types[pname] {
									c_name := g.types_type_to_c(concrete)
									resolved_params[i] = ast.Expr(ast.Ident{
										name: c_name
									})
								}
							}
						}
					}
					g.record_generic_struct_bindings(struct_base, base_name, resolved_params)
					return g.resolve_generic_struct_c_name(base_name, resolved_params)
				}
				return base_name
			}
			return 'int'
		}
		ast.ModifierExpr {
			// Handle shared/mut modifiers: unwrap and use the inner type
			return g.expr_type_to_c(e.expr)
		}
		ast.CallOrCastExpr {
			if e.expr !is ast.EmptyExpr && g.call_or_cast_lhs_is_type(e.lhs) {
				return g.expr_type_to_c(e.lhs)
			}
			call_args := if e.expr is ast.EmptyExpr {
				[]ast.Expr{}
			} else {
				[e.expr]
			}
			if ret := g.get_call_return_type(e.lhs, call_args) {
				return ret
			}
			return 'int'
		}
		else {
			return 'int'
		}
	}
}

fn (mut g Gen) decl_expr_type_to_c(e ast.Expr) string {
	saved_fn_scope := g.cur_fn_scope
	saved_fn_scope_miss_key := g.cur_fn_scope_miss_key
	saved_runtime_local_types := g.runtime_local_types.clone()
	saved_runtime_decl_types := g.runtime_decl_types.clone()
	saved_not_local_var_cache := g.not_local_var_cache.clone()
	defer {
		g.cur_fn_scope = saved_fn_scope
		g.cur_fn_scope_miss_key = saved_fn_scope_miss_key
		g.runtime_local_types = saved_runtime_local_types.clone()
		g.runtime_decl_types = saved_runtime_decl_types.clone()
		g.not_local_var_cache = saved_not_local_var_cache.clone()
	}
	g.cur_fn_scope = unsafe { nil }
	g.cur_fn_scope_miss_key = ''
	g.runtime_local_types = map[string]string{}
	g.runtime_decl_types = map[string]string{}
	g.not_local_var_cache = map[string]bool{}
	return g.expr_type_to_c(e)
}

fn (mut g Gen) signature_expr_type_to_c(e ast.Expr) string {
	c_name := g.expr_type_to_c(e)
	raw_type := g.get_raw_type(e) or { return c_name }
	return g.specialized_generic_struct_signature_type(c_name, raw_type) or { c_name }
}

fn (mut g Gen) specialized_generic_struct_signature_type(c_name string, raw_type types.Type) ?string {
	if c_name == '' || c_name.ends_with('*') || raw_type !is types.Struct {
		return none
	}
	raw_struct := raw_type as types.Struct
	if raw_struct.fields.len == 0 {
		return none
	}
	instances := g.generic_struct_instances_for_signature_type(c_name)
	if instances.len == 0 {
		return none
	}
	if instances.len == 1 {
		return instances[0].c_name
	}
	template := g.find_generic_struct_node(c_name) or { return none }
	for inst in instances {
		if g.generic_struct_instance_matches_signature_fields(template, raw_struct, inst) {
			return inst.c_name
		}
	}
	return none
}

fn (g &Gen) generic_struct_instances_for_signature_type(c_name string) []GenericStructInstance {
	mut instances := g.generic_struct_instances[c_name]
	if instances.len == 0 && c_name.contains('__') {
		instances = g.generic_struct_instances[c_name.all_after_last('__')]
	}
	if instances.len == 0 && !c_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
		&& g.cur_module != 'builtin' {
		instances = g.generic_struct_instances['${g.cur_module}__${c_name}']
	}
	return instances
}

fn (mut g Gen) generic_struct_instance_matches_signature_fields(template ast.StructDecl, raw_struct types.Struct, inst GenericStructInstance) bool {
	if template.fields.len == 0 || template.fields.len != raw_struct.fields.len {
		return false
	}
	prev_active := g.active_generic_types.clone()
	g.active_generic_types = inst.bindings.clone()
	defer {
		g.active_generic_types = prev_active.clone()
	}
	for i, field in template.fields {
		if i >= raw_struct.fields.len {
			return false
		}
		raw_field := raw_struct.fields[i]
		if field.name != raw_field.name {
			return false
		}
		expected := normalize_signature_type_name(g.expr_type_to_c(field.typ), '')
		actual := normalize_signature_type_name(g.types_type_to_c(raw_field.typ), '')
		if expected == '' || actual == '' || expected != actual {
			return false
		}
	}
	return true
}

fn (g &Gen) current_fn_module_local_type_name(type_name string) ?string {
	if type_name == '' || type_name.contains('__') || g.cur_fn_c_name == '' {
		return none
	}
	fn_module := module_prefix_from_fn_name(g.cur_fn_c_name)
	if fn_module == '' || fn_module == g.cur_module || fn_module == 'main' || fn_module == 'builtin' {
		return none
	}
	if fn_module[0] < `a` || fn_module[0] > `z` || !g.source_module_exists(fn_module) {
		return none
	}
	qualified := '${fn_module}__${type_name}'
	if 'body_${qualified}' in g.emitted_types || 'enum_${qualified}' in g.emitted_types
		|| 'alias_${qualified}' in g.emitted_types
		|| 'body_${qualified}' in g.pending_late_body_keys {
		return qualified
	}
	if g.c_type_name_declared_in_module(fn_module, type_name) {
		return qualified
	}
	if mut module_scope := g.env_scope(fn_module) {
		if _ := module_scope.lookup_type(type_name) {
			return qualified
		}
	}
	return none
}

// is_c_type_name checks if a name refers to a C type (struct, typedef) vs a C function.
fn (g &Gen) is_c_type_name(name string) bool {
	if name in g.typedef_c_types {
		return true
	}
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'pthread_condattr_t', 'pthread_rwlockattr_t',
		'atomic_uintptr_t', 'stat', 'tm', 'timespec', 'timeval', 'dirent', 'termios', 'sockaddr',
		'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'fd_set', 'mach_timebase_info_data_t',
		'FONScontext', 'FONSparams']
}

// record_generic_struct_bindings records the concrete type bindings for a
// generic struct instantiation (e.g. LinkedList[ValueInfo] → {T: ValueInfo}).
// These bindings are used when emitting methods on the generic struct.
fn (mut g Gen) record_generic_struct_bindings(struct_base_name string, struct_c_name string, concrete_params []ast.Expr) {
	// Find the struct decl to get generic param names.
	filtered_params := runtime_generic_args(concrete_params)
	runtime_param_names := g.generic_struct_runtime_param_names(struct_base_name, struct_c_name)
	if runtime_param_names.len == 0 || runtime_param_names.len != filtered_params.len {
		return
	}
	// Check that all concrete params are non-placeholder types.
	mut bindings := map[string]types.Type{}
	mut param_c_names := []string{cap: filtered_params.len}
	for i, param_name in runtime_param_names {
		concrete_expr := filtered_params[i]
		if is_generic_placeholder_type_name(concrete_expr.name()) {
			return
		}
		concrete_c_name := g.expr_type_to_c(concrete_expr)
		param_c_names << mangle_alias_component(concrete_c_name)
		if concrete_type := g.concrete_type_from_call_arg_c_name(concrete_c_name) {
			bindings[param_name] = concrete_type
		}
	}
	if bindings.len != runtime_param_names.len {
		return
	}
	if !g.generic_specialization_belongs_to_emit_modules(bindings) {
		return
	}
	params_key := param_c_names.join('_')
	bindings_key := g.generic_struct_bindings_key(runtime_param_names, bindings)

	// Record in multi-instantiation map
	mut instances := g.generic_struct_instances[struct_c_name]
	mut already_exists := false
	for inst in instances {
		if g.generic_struct_instance_matches(inst, params_key, bindings_key, runtime_param_names)
			|| g.generic_struct_instance_bindings_match(inst, bindings, runtime_param_names) {
			already_exists = true
			break
		}
	}
	if !already_exists {
		mut force_suffixed_instance := false
		for param_c_name in param_c_names {
			param_struct := g.lookup_struct_type(param_c_name)
			if param_struct.generic_params.len > 0 {
				force_suffixed_instance = true
				break
			}
		}
		inst_c_name := if instances.len == 0 && !force_suffixed_instance {
			struct_c_name
		} else {
			'${struct_c_name}_T_${params_key}'
		}
		instances << GenericStructInstance{
			params_key: params_key
			bindings:   bindings.clone()
			c_name:     inst_c_name
		}
		g.generic_struct_instances[struct_c_name] = instances
	}

	// Keep existing generic_struct_bindings for backward compat (primary binding)
	if struct_c_name !in g.generic_struct_bindings {
		g.generic_struct_bindings[struct_c_name] = bindings.clone()
	}
}

// record_generic_struct_bindings_with_parent records bindings for a generic struct
// by resolving placeholder params through the parent struct's known bindings.
fn (mut g Gen) record_generic_struct_bindings_with_parent(struct_base_name string, struct_c_name string, concrete_params []ast.Expr, parent_bindings map[string]types.Type) {
	filtered_params := runtime_generic_args(concrete_params)
	runtime_param_names := g.generic_struct_runtime_param_names(struct_base_name, struct_c_name)
	if runtime_param_names.len == 0 || runtime_param_names.len != filtered_params.len {
		return
	}
	mut bindings := map[string]types.Type{}
	mut param_c_names := []string{cap: filtered_params.len}
	for i, param_name in runtime_param_names {
		concrete_expr := filtered_params[i]
		expr_name := concrete_expr.name()
		if is_generic_placeholder_type_name(expr_name) {
			if parent_type := parent_bindings[expr_name] {
				parent_c_name := g.types_type_to_c(parent_type)
				mut resolved_parent_type := parent_type
				if concrete_parent_type := g.concrete_type_from_c_name(parent_c_name) {
					resolved_parent_type = concrete_parent_type
				}
				bindings[param_name] = resolved_parent_type
				param_c_names << mangle_alias_component(parent_c_name)
				continue
			}
			return
		}
		resolved_expr := g.substitute_generic_expr_with_parent(concrete_expr, parent_bindings)
		concrete_c_name := g.expr_type_to_c(resolved_expr)
		param_c_names << mangle_alias_component(concrete_c_name)
		if concrete_type := g.concrete_type_from_call_arg_c_name(concrete_c_name) {
			bindings[param_name] = concrete_type
		}
	}
	if bindings.len != runtime_param_names.len {
		return
	}
	if !g.generic_specialization_belongs_to_emit_modules(bindings) {
		return
	}
	params_key := param_c_names.join('_')
	bindings_key := g.generic_struct_bindings_key(runtime_param_names, bindings)

	mut instances := g.generic_struct_instances[struct_c_name]
	mut already_exists := false
	for inst in instances {
		if g.generic_struct_instance_matches(inst, params_key, bindings_key, runtime_param_names)
			|| g.generic_struct_instance_bindings_match(inst, bindings, runtime_param_names) {
			already_exists = true
			break
		}
	}
	if !already_exists {
		mut force_suffixed_instance := false
		for param_c_name in param_c_names {
			param_struct := g.lookup_struct_type(param_c_name)
			if param_struct.generic_params.len > 0 {
				force_suffixed_instance = true
				break
			}
		}
		inst_c_name := if instances.len == 0 && !force_suffixed_instance {
			struct_c_name
		} else {
			'${struct_c_name}_T_${params_key}'
		}
		instances << GenericStructInstance{
			params_key: params_key
			bindings:   bindings.clone()
			c_name:     inst_c_name
		}
		g.generic_struct_instances[struct_c_name] = instances
	}

	if struct_c_name !in g.generic_struct_bindings {
		g.generic_struct_bindings[struct_c_name] = bindings.clone()
	}
}

fn (g &Gen) generic_struct_bindings_key(param_names []string, bindings map[string]types.Type) string {
	mut parts := []string{cap: param_names.len}
	for param_name in param_names {
		concrete := bindings[param_name] or { return '' }
		if !types.type_has_valid_payload(concrete) {
			return ''
		}
		parts << g.generic_specialization_token_from_type(concrete)
	}
	return parts.join('_')
}

fn (g &Gen) generic_struct_instance_matches(inst GenericStructInstance, params_key string, bindings_key string, param_names []string) bool {
	if inst.params_key == params_key {
		return true
	}
	if bindings_key == '' {
		return false
	}
	return g.generic_struct_bindings_key(param_names, inst.bindings) == bindings_key
}

fn (g &Gen) generic_struct_instance_bindings_match(inst GenericStructInstance, bindings map[string]types.Type, param_names []string) bool {
	for param_name in param_names {
		inst_type := inst.bindings[param_name] or { return false }
		binding_type := bindings[param_name] or { return false }
		if g.generic_binding_types_match(inst_type, binding_type) {
			continue
		}
		return false
	}
	return true
}

fn (g &Gen) generic_binding_types_match(a types.Type, b types.Type) bool {
	a_token := g.generic_specialization_token_from_type(a)
	b_token := g.generic_specialization_token_from_type(b)
	if a_token == b_token {
		return true
	}
	a_name := a.name()
	b_name := b.name()
	if a_name == '' || b_name == '' {
		return false
	}
	if a_name == b_name {
		return true
	}
	if a_name.contains('__') && b_name.contains('__') {
		return false
	}
	return short_type_name(a_name) == short_type_name(b_name)
}

fn (mut g Gen) generic_struct_params_bindings_key(concrete_params []ast.Expr) string {
	runtime_params := runtime_generic_args(concrete_params)
	mut parts := []string{cap: runtime_params.len}
	for param in runtime_params {
		concrete_c_name := g.expr_type_to_c(param)
		concrete := g.concrete_type_from_call_arg_c_name(concrete_c_name) or { return '' }
		if !types.type_has_valid_payload(concrete) {
			return ''
		}
		parts << g.generic_specialization_token_from_type(concrete)
	}
	return parts.join('_')
}

fn (mut g Gen) substitute_generic_expr_with_parent(expr ast.Expr, parent_bindings map[string]types.Type) ast.Expr {
	match expr {
		ast.Ident {
			if is_generic_placeholder_type_name(expr.name) {
				if concrete := parent_bindings[expr.name] {
					return ast.Expr(ast.Ident{
						name: g.types_type_to_c(concrete)
						pos:  expr.pos
					})
				}
			}
			return expr
		}
		ast.Type {
			if expr is ast.GenericType {
				mut params := []ast.Expr{cap: expr.params.len}
				for param in expr.params {
					params << g.substitute_generic_expr_with_parent(param, parent_bindings)
				}
				return ast.Expr(ast.Type(ast.GenericType{
					name:   expr.name
					params: params
				}))
			}
			if expr is ast.PointerType {
				return ast.Expr(ast.Type(ast.PointerType{
					base_type: g.substitute_generic_expr_with_parent(expr.base_type,
						parent_bindings)
					lifetime:  expr.lifetime
				}))
			}
			return expr
		}
		ast.GenericArgOrIndexExpr {
			return ast.Expr(ast.GenericArgOrIndexExpr{
				lhs:  expr.lhs
				expr: g.substitute_generic_expr_with_parent(expr.expr, parent_bindings)
				pos:  expr.pos
			})
		}
		ast.GenericArgs {
			mut args := []ast.Expr{cap: expr.args.len}
			for arg in expr.args {
				args << g.substitute_generic_expr_with_parent(arg, parent_bindings)
			}
			return ast.Expr(ast.GenericArgs{
				lhs:  expr.lhs
				args: args
				pos:  expr.pos
			})
		}
		else {
			return expr
		}
	}
}

fn (mut g Gen) generic_struct_runtime_param_names(struct_base_name string, struct_c_name string) []string {
	mut env_struct := g.lookup_struct_type_by_c_name(struct_c_name)
	if env_struct.generic_params.len == 0 {
		env_struct = g.lookup_struct_type(struct_base_name)
	}
	return runtime_generic_param_names(env_struct.generic_params)
}

// resolve_generic_struct_c_name returns the correct C struct name for a generic
// struct instantiation with specific type parameters. If the instantiation matches
// a non-primary instance, returns the suffixed name.
fn (mut g Gen) resolve_generic_struct_c_name(base_name string, concrete_params []ast.Expr) string {
	mut lookup_base_name := base_name
	mut instances := g.generic_struct_instances[lookup_base_name]
	if instances.len == 0 && base_name.contains('__') {
		short_base_name := base_name.all_after_last('__')
		instances = g.generic_struct_instances[short_base_name]
		if instances.len > 0 {
			lookup_base_name = short_base_name
		}
	}
	if instances.len == 0 && !base_name.contains('__') && g.cur_module != ''
		&& g.cur_module != 'main' && g.cur_module != 'builtin' {
		qualified_base_name := '${g.cur_module}__${base_name}'
		instances = g.generic_struct_instances[qualified_base_name]
		if instances.len > 0 {
			lookup_base_name = qualified_base_name
		}
	}
	if instances.len == 1 {
		inst := instances[0]
		body_key := 'body_${inst.c_name}'
		if g.pass5_start_pos > 0 && body_key !in g.emitted_types
			&& body_key !in g.pending_late_body_keys {
			g.emit_late_generic_struct(lookup_base_name, inst)
		}
		return inst.c_name
	}
	if instances.len == 0 {
		return base_name
	}
	runtime_params := runtime_generic_args(concrete_params)
	mut param_c_names := []string{cap: runtime_params.len}
	for p in runtime_params {
		param_c_names << mangle_alias_component(g.expr_type_to_c(p))
	}
	params_key := param_c_names.join('_')
	bindings_key := g.generic_struct_params_bindings_key(concrete_params)
	for inst in instances {
		if inst.params_key == params_key {
			body_key := 'body_${inst.c_name}'
			if g.pass5_start_pos > 0 && body_key !in g.emitted_types
				&& body_key !in g.pending_late_body_keys {
				g.emit_late_generic_struct(lookup_base_name, inst)
			}
			return inst.c_name
		}
	}
	if bindings_key != '' {
		struct_base := if lookup_base_name.contains('__') {
			lookup_base_name.all_after_last('__')
		} else {
			lookup_base_name
		}
		param_names := g.generic_struct_runtime_param_names(struct_base, lookup_base_name)
		for inst in instances {
			if g.generic_struct_instance_matches(inst, params_key, bindings_key, param_names) {
				body_key := 'body_${inst.c_name}'
				if g.pass5_start_pos > 0 && body_key !in g.emitted_types
					&& body_key !in g.pending_late_body_keys {
					g.emit_late_generic_struct(lookup_base_name, inst)
				}
				return inst.c_name
			}
		}
	}
	return base_name
}

// emit_late_generic_struct generates a struct definition for a non-primary generic
// struct instantiation and adds it to late_struct_defs for insertion before option/result
// wrapper emission.
fn (mut g Gen) emit_late_generic_struct(base_name string, inst GenericStructInstance) {
	body_key := 'body_${inst.c_name}'
	if body_key in g.emitted_types || body_key in g.pending_late_body_keys {
		return
	}
	// Mark as pending — not yet in g.sb, so option_result_payload_ready won't see it.
	// It will be moved to emitted_types when late_struct_defs is flushed.
	g.pending_late_body_keys[body_key] = true
	// Find the struct AST node by base name
	struct_node := g.find_generic_struct_node(base_name) or { return }
	keyword := if struct_node.is_union { 'union' } else { 'struct' }
	// Generate struct body with this instantiation's bindings
	prev_active := g.active_generic_types.clone()
	g.active_generic_types = inst.bindings.clone()
	mut def := strings.new_builder(256)
	// Emit typedef forward declarations so self-referential pointer fields
	// (e.g. linked-list `next` pointers) and other generic-instance field
	// types can be resolved without `struct` tag inside the body.
	// Note: don't mark these in g.emitted_types — per-bundle workers
	// inherit the map and would skip their own pass 1 typedef otherwise.
	// Duplicate typedefs of identical struct shape are valid C11.
	def.writeln('typedef ${keyword} ${inst.c_name} ${inst.c_name};')
	mut seen_field_typedef := map[string]bool{}
	for field in struct_node.fields {
		ft := g.qualify_owner_local_field_type(inst.c_name, g.expr_type_to_c(field.typ))
		fbase := ft.trim_right('*')
		if fbase != inst.c_name && fbase.contains('_T_') && !fbase.starts_with('Array_')
			&& !fbase.starts_with('Map_') && fbase !in seen_field_typedef {
			seen_field_typedef[fbase] = true
			def.writeln('typedef struct ${fbase} ${fbase};')
		}
	}
	def.writeln('${keyword} ${inst.c_name} {')
	for field in struct_node.fields {
		field_type := g.qualify_owner_local_field_type(inst.c_name, g.expr_type_to_c(field.typ))
		field_name := if field.name.len > 0 { field.name } else { 'value' }
		def.writeln('\t${field_type} ${field_name};')
		g.struct_field_types['${inst.c_name}.${field_name}'] = field_type
	}
	g.struct_field_lookup_cache = map[string]string{}
	g.struct_field_lookup_miss = map[string]bool{}
	if struct_node.fields.len == 0 {
		def.writeln('\tu8 _dummy;')
	}
	def.writeln('};')
	// Don't emit str macro here — fn_return_types may not be fully populated yet
	// (this runs during pass 4). The str macro will be emitted in gen_finalize(),
	// after pass 4 has populated fn_return_types.
	g.late_generic_str_instances << inst.c_name
	def.writeln('')
	g.active_generic_types = prev_active.clone()
	g.late_struct_defs << def.str()
}

// find_generic_struct_node finds the AST StructDecl for a given C struct name.
fn (mut g Gen) find_generic_struct_node(c_name string) ?ast.StructDecl {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_struct_decl {
					continue
				}
				decl := stmt.struct_decl()
				if decl.generic_params.len > 0 {
					name := g.get_struct_name(decl)
					if name == c_name {
						return decl
					}
				}
			}
		}
		return none
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.generic_params.len > 0 {
					name := g.get_struct_name(stmt)
					if name == c_name {
						return stmt
					}
				}
			}
		}
	}
	return none
}

fn is_comptime_selector_rhs_name(name string) bool {
	return name == '__comptime_selector__' || name == 'TODO: comptime selector'
}

// get_comptime_selector_type returns the C type for a comptime field selector expression.
fn (mut g Gen) get_comptime_selector_type(node ast.SelectorExpr) string {
	rhs_name := node.rhs.name
	// val.$(field.name) → type of that field
	if is_comptime_selector_rhs_name(rhs_name) {
		return g.comptime_field_type
	}
	// field.name → string
	if node.lhs is ast.Ident && node.lhs.name == g.comptime_field_var {
		match rhs_name {
			'name' {
				return 'string'
			}
			'typ' {
				return 'int'
			}
			'unaliased_typ' {
				return 'int'
			}
			'attrs' {
				return 'Array_string'
			}
			'is_pub', 'is_mut', 'is_embed', 'is_shared', 'is_atomic', 'is_option', 'is_array',
			'is_map', 'is_chan', 'is_enum', 'is_struct', 'is_alias' {
				return 'bool'
			}
			else {}
		}
	}
	// field.name.str → char*, field.name.len → int
	if node.lhs is ast.SelectorExpr {
		inner := node.lhs as ast.SelectorExpr
		if inner.lhs is ast.Ident && inner.lhs.name == g.comptime_field_var
			&& inner.rhs.name == 'name' {
			match rhs_name {
				'str' { return 'char*' }
				'len' { return 'int' }
				else {}
			}
		}
	}
	return ''
}

// alias_resolves_to_string reports whether `c_name` is a type alias whose
// chain of base types ultimately resolves to the `string` named type.
fn (mut g Gen) alias_resolves_to_string(c_name string) bool {
	if c_name == 'string' {
		return true
	}
	typ := g.lookup_type_by_c_name(c_name) or { return false }
	return g.type_resolves_to_string(typ, 0)
}

fn (mut g Gen) type_resolves_to_string(typ types.Type, depth int) bool {
	if depth > 16 {
		return false
	}
	if typ is types.String {
		return true
	}
	if typ is types.NamedType {
		return string(typ) == 'string'
	}
	if typ is types.Alias {
		return g.type_resolves_to_string(typ.base_type, depth + 1)
	}
	return false
}

// lookup_type_by_c_name resolves a C type name to a types.Type from the Environment.
fn (mut g Gen) lookup_type_by_c_name(c_name string) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	// Try module-scoped lookup: json2__ValueInfo → look in json2 scope for ValueInfo
	if c_name.contains('__') {
		parts := c_name.split('__')
		if parts.len == 2 {
			mod_name := parts[0]
			type_name := parts[1]
			if scope := g.env_scope(mod_name) {
				if typ := scope.lookup_type(type_name) {
					return typ
				}
			}
		}
	}
	mod_name := if g.cur_module != '' { g.cur_module } else { 'main' }
	short_name := if c_name.contains('__') { c_name.all_after_last('__') } else { c_name }
	mut modules_to_try := []string{}
	if !c_name.contains('__') && mod_name != 'main' && mod_name != 'builtin' {
		modules_to_try << 'main'
		modules_to_try << 'builtin'
	}
	modules_to_try << mod_name
	if 'main' !in modules_to_try {
		modules_to_try << 'main'
	}
	if 'builtin' !in modules_to_try {
		modules_to_try << 'builtin'
	}
	mut tried := map[string]bool{}
	for try_mod in modules_to_try {
		if tried[try_mod] {
			continue
		}
		tried[try_mod] = true
		if scope := g.env_scope(try_mod) {
			if typ := scope.lookup_type(short_name) {
				return typ
			}
		}
	}
	return none
}

fn (g &Gen) generic_struct_primary_instance_is_concrete(c_name string) bool {
	if c_name !in g.generic_struct_bindings {
		return false
	}
	bindings := g.generic_struct_bindings[c_name].clone()
	if bindings.len == 0 {
		return false
	}
	for _, concrete in bindings {
		if concrete.name() == '' || type_contains_generic_placeholder(concrete) {
			return false
		}
	}
	return true
}

fn (mut g Gen) concrete_type_from_c_name(c_name string) ?types.Type {
	clean_name := c_name.trim_space().trim_right('*')
	if clean_name == '' {
		return none
	}
	if g.generic_struct_primary_instance_is_concrete(clean_name) {
		return types.Type(types.Struct{
			name: clean_name
		})
	}
	if clean_name.contains('_T_') {
		base_name := clean_name.all_before('_T_')
		if base_name != '' && (base_name in g.generic_struct_instances
			|| base_name in g.generic_struct_bindings
			|| g.lookup_struct_type_by_c_name(base_name).generic_params.len > 0) {
			return normalize_generic_concrete_type(types.Type(types.Struct{
				name: clean_name
			}))
		}
	}
	if concrete := g.lookup_type_by_c_name(clean_name) {
		if type_contains_generic_placeholder(concrete) {
			return types.Type(types.Struct{
				name: clean_name
			})
		}
		return normalize_generic_concrete_type(concrete)
	}
	if primitive := resolve_primitive_type_name(clean_name) {
		return normalize_generic_concrete_type(primitive)
	}
	if raw := g.resolve_c_type_to_raw(clean_name) {
		return normalize_generic_concrete_type(raw)
	}
	if clean_name == 'int' {
		return none
	}
	return normalize_generic_concrete_type(types.Type(types.Struct{
		name: clean_name
	}))
}

fn (g &Gen) env_scope(module_name string) ?&types.Scope {
	if g.env == unsafe { nil } {
		return none
	}
	// Cache scope pointers to avoid repeated lock acquisition in Environment.get_scope.
	if cached_ptr := g.cached_env_scopes[module_name] {
		if cached_ptr == unsafe { nil } {
			return none
		}
		return unsafe { &types.Scope(cached_ptr) }
	}
	if scope := g.env.get_scope(module_name) {
		unsafe {
			mut self := g
			self.cached_env_scopes[module_name] = voidptr(scope)
		}
		return scope
	}
	mut null_ptr := unsafe { nil }
	unsafe {
		mut self := g
		self.cached_env_scopes[module_name] = null_ptr
	}
	return none
}

fn (g &Gen) lookup_module_scope_object(name string) ?types.Object {
	if g.env == unsafe { nil } {
		return none
	}
	if g.cur_module == '' {
		return none
	}
	if mut module_scope := g.env_scope(g.cur_module) {
		if obj := module_scope.lookup(name) {
			return obj
		}
	}
	return none
}

fn (g &Gen) is_module_local_type(name string) bool {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin' {
		return false
	}
	if name in primitive_types {
		return false
	}
	if name in ['bool', 'string', 'voidptr', 'charptr', 'byteptr'] {
		return false
	}
	if name.contains('__') || name.starts_with('Array_') || name.starts_with('Array_fixed_')
		|| name.starts_with('Map_') || name.starts_with('_result_') || name.starts_with('_option_') {
		return false
	}
	if _ := g.imported_symbol_c_type(name) {
		return false
	}
	if mut module_scope := g.env_scope(g.cur_module) {
		if _ := module_scope.lookup_type(name) {
			return true
		}
	}
	return false
}

fn (g &Gen) imported_symbol_c_type(name string) ?string {
	if name == '' {
		return none
	}
	mut symbol_name := name
	if name.contains('__') {
		prefix := '${g.cur_module}__'
		if g.cur_module == '' || !name.starts_with(prefix) {
			return none
		}
		symbol_name = name[prefix.len..]
	}
	if mod_name := g.imported_symbols_index['${g.cur_file_name}\x01${symbol_name}'] {
		if mod_name == '' || mod_name == g.cur_module {
			return none
		}
		return '${mod_name}__${symbol_name}'
	}
	if g.imported_symbols_index.len == 0 {
		return g.scan_imported_symbol_c_type(symbol_name)
	}
	return none
}

fn (g &Gen) scan_imported_symbol_c_type(symbol_name string) ?string {
	if symbol_name == '' {
		return none
	}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			if g.cur_file_name != '' && fc.name() != g.cur_file_name {
				continue
			}
			imports := fc.imports()
			for j in 0 .. imports.len() {
				import_stmt := imports.at(j).import_stmt()
				if import_stmt.symbols.len == 0 {
					continue
				}
				for symbol in import_stmt.symbols {
					if symbol.name() != symbol_name {
						continue
					}
					mod_name := import_stmt.name.all_after_last('.').replace('.', '_')
					if mod_name == '' || mod_name == g.cur_module {
						return none
					}
					return '${mod_name}__${symbol_name}'
				}
			}
		}
		return none
	}
	for file in g.files {
		if g.cur_file_name != '' && file.name != g.cur_file_name {
			continue
		}
		for import_stmt in file.imports {
			if import_stmt.symbols.len == 0 {
				continue
			}
			for symbol in import_stmt.symbols {
				if symbol.name() != symbol_name {
					continue
				}
				mod_name := import_stmt.name.all_after_last('.').replace('.', '_')
				if mod_name == '' || mod_name == g.cur_module {
					return none
				}
				return '${mod_name}__${symbol_name}'
			}
		}
	}
	return none
}

fn (g &Gen) is_module_local_const_or_global(name string) bool {
	if g.cur_module == '' {
		return false
	}
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Const || obj is types.Global
	}
	return false
}

fn (g &Gen) is_module_local_fn(name string) bool {
	if g.cur_module == '' || g.cur_module == 'main' || g.cur_module == 'builtin' {
		return false
	}
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Fn
	}
	sanitized := sanitize_fn_ident(name)
	if sanitized != name {
		if obj := g.lookup_module_scope_object(sanitized) {
			return obj is types.Fn
		}
	}
	return false
}

// get_raw_type returns the raw types.Type for an expression from the Environment
fn (mut g Gen) get_raw_type(node ast.Expr) ?types.Type {
	if typ := g.get_raw_type_inner(node) {
		if type_has_valid_data(typ) {
			return typ
		}
	}
	return none
}

fn (mut g Gen) get_raw_type_inner(node ast.Expr) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	if !expr_has_valid_data(node) {
		return none
	}
	if node is ast.InitExpr && g.active_generic_types.len > 0 {
		if node.typ is ast.Ident {
			if concrete := g.resolve_active_generic_type(node.typ.name) {
				return concrete
			}
		}
	}
	// Fast path: env pos.id O(1) lookup for non-compound expressions.
	if node !is ast.Ident && node !is ast.SelectorExpr && node !is ast.IndexExpr {
		pos := node.pos()
		if pos.is_valid() {
			if typ := g.env.get_expr_type(pos.id) {
				if type_has_valid_data(typ) {
					return typ
				}
			}
			return none
		}
	}
	// For identifiers, current C-scope locals override broader checker scopes.
	// Synthesized option/result guards and lowered loops can reuse a name from an
	// outer scope, while only runtime_local_types knows the active C declaration.
	if node is ast.Ident {
		// Fast path: if is_module_ident_cache already knows this is a module, skip scope walks.
		if cached := g.is_module_ident_cache[node.name] {
			if cached {
				return none
			}
		}
		if local_type := g.runtime_local_types[node.name] {
			if resolved := g.resolve_c_type_to_raw(local_type) {
				return resolved
			}
		}
		if mut fn_scope := g.ensure_cur_fn_scope() {
			if obj := fn_scope.lookup_parent(node.name, 0) {
				if obj is types.Module {
					return none
				}
				return obj.typ()
			}
		}
		// Fallback to module scopes when function scope chain misses the symbol.
		if g.cur_module != '' {
			if mut mod_scope := g.env_scope(g.cur_module) {
				if obj := mod_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						return obj.typ()
					}
				}
			}
		}
		if g.cur_module != 'builtin' {
			if mut builtin_scope := g.env_scope('builtin') {
				if obj := builtin_scope.lookup_parent(node.name, 0) {
					if obj !is types.Module {
						return obj.typ()
					}
				}
			}
		}
	}
	// For SelectorExpr, resolve through struct field lookup (more reliable than position)
	if node is ast.SelectorExpr {
		if g.comptime_field_var != '' && is_comptime_selector_rhs_name(node.rhs.name)
			&& type_has_valid_data(g.comptime_field_raw_type) {
			return g.comptime_field_raw_type
		}
		// Module selector (e.g. os.args): resolve from module scope first.
		if node.lhs is ast.Ident {
			// Use is_module_ident cache to quickly check if LHS is a module.
			is_mod := g.is_module_ident(node.lhs.name)
			if is_mod {
				// LHS is a module — look up the RHS in the module's scope.
				if mut fn_scope := g.ensure_cur_fn_scope() {
					if obj := fn_scope.lookup_parent(node.lhs.name, 0) {
						if obj is types.Module {
							if rhs_obj := obj.lookup(node.rhs.name) {
								if rhs_obj !is types.Module {
									return rhs_obj.typ()
								}
							}
						}
					}
				}
				if g.cur_module != '' {
					if mut mod_scope := g.env_scope(g.cur_module) {
						if obj := mod_scope.lookup_parent(node.lhs.name, 0) {
							if obj is types.Module {
								if rhs_obj := obj.lookup(node.rhs.name) {
									if rhs_obj !is types.Module {
										return rhs_obj.typ()
									}
								}
							}
						}
					}
				}
			}
		}
		if lhs_type := g.get_raw_type(node.lhs) {
			if !type_has_valid_data(lhs_type) {
				return none
			}
			if field_type := selector_struct_field_type_from_type(lhs_type, node.rhs.name) {
				return field_type
			}
			// When LHS is a sum type and has been narrowed via `is` check, look up
			// the field on the narrowed variant type instead.
			if lhs_type is types.SumType {
				if narrowed_c := g.get_expr_type_from_env(node.lhs) {
					if narrowed_type := g.resolve_c_type_to_raw(narrowed_c) {
						if field_type := selector_struct_field_type_from_type(narrowed_type,
							node.rhs.name)
						{
							return field_type
						}
					}
				}
			}
			if vector_field_index(node.rhs.name) >= 0 {
				mut lhs_c_type := g.types_type_to_c(lhs_type)
				if lhs_c_type == '' || lhs_c_type == 'int' {
					lhs_c_type = g.get_expr_type(node.lhs)
				}
				elem_type_name := vector_elem_type_for_name(lhs_c_type)
				if elem_type_name != '' {
					if elem_type := resolve_primitive_type_name(elem_type_name) {
						return elem_type
					}
				}
			}
			// Built-in fields on array/map/string types
			base_lhs := match lhs_type {
				types.Pointer { lhs_type.base_type }
				types.Alias { lhs_type.base_type }
				else { lhs_type }
			}

			if base_lhs is types.Array || base_lhs is types.ArrayFixed || base_lhs is types.Map
				|| base_lhs is types.String {
				if node.rhs.name == 'len' {
					return types.int_
				}
			}
			// Interface types stored in variables may have empty fields.
			// Look up the populated interface declaration from the scope.
			iface_name := match lhs_type {
				types.Interface {
					lhs_type.name
				}
				types.Pointer {
					if lhs_type.base_type is types.Interface {
						lhs_type.base_type.name
					} else {
						''
					}
				}
				else {
					''
				}
			}

			if iface_name != '' {
				short_name := if iface_name.contains('__') {
					iface_name.all_after_last('__')
				} else {
					iface_name
				}
				for mod in ['builtin', g.cur_module] {
					if mod == '' {
						continue
					}
					if mut scope := g.env_scope(mod) {
						if obj := scope.lookup_parent(short_name, 0) {
							if obj is types.Type {
								if field_type := selector_struct_field_type_from_type(obj,
									node.rhs.name)
								{
									return field_type
								}
							}
						}
					}
				}
			}
		}
	}
	// For IndexExpr, resolve through the LHS container type
	if node is ast.IndexExpr {
		if lhs_type := g.get_raw_type(node.lhs) {
			if lhs_type is types.Array {
				return lhs_type.elem_type
			}
			if lhs_type is types.ArrayFixed {
				return lhs_type.elem_type
			}
			if lhs_type is types.Map {
				return lhs_type.value_type
			}
			if lhs_type is types.Pointer {
				// Pointer to array: indexing dereferences then indexes, return element type
				if lhs_type.base_type is types.Array {
					return lhs_type.base_type.elem_type
				}
				if lhs_type.base_type is types.ArrayFixed {
					return lhs_type.base_type.elem_type
				}
				return lhs_type.base_type
			}
		}
	}
	// For UnsafeExpr, infer type from the last statement in the block.
	if node is ast.UnsafeExpr {
		if node.stmts.len > 0 {
			last := node.stmts[node.stmts.len - 1]
			if last is ast.ExprStmt {
				if typ := g.get_raw_type(last.expr) {
					return typ
				}
				return none
			}
		}
	}
	// Try environment lookup by position
	if !expr_has_valid_data(node) {
		return none
	}
	pos := node.pos()
	if pos.is_valid() {
		if typ := g.env.get_expr_type(pos.id) {
			if type_has_valid_data(typ) {
				return typ
			}
		}
		return none
	}
	return none
}

// resolve_primitive_type_name converts a primitive C type name to a types.Type.
// Returns none if the name is not a known primitive type.
fn resolve_primitive_type_name(name string) ?types.Type {
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
		'isize' {
			types.Type(types.Alias{
				name:      'isize'
				base_type: types.Type(types.Primitive{
					size:  64
					props: .integer
				})
			})
		}
		'usize' {
			types.Type(types.Alias{
				name:      'usize'
				base_type: types.Type(types.Primitive{
					size:  64
					props: .integer | .unsigned
				})
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
			none
		}
	}
}

// resolve_c_type_to_raw converts a C type name (e.g. "types__Deferred") back to a types.Type
// by looking up the type name in the appropriate module scope.
fn (mut g Gen) resolve_c_type_to_raw(c_type string) ?types.Type {
	if g.env == unsafe { nil } {
		return none
	}
	mut type_name := c_type.trim_right('*')
	is_ptr := c_type.ends_with('*')
	if type_name.starts_with('Array_fixed_') {
		payload := type_name['Array_fixed_'.len..]
		last_underscore := payload.last_index('_') or { return none }
		elem_c := unmangle_c_ptr_type(payload[..last_underscore])
		len := payload[last_underscore + 1..].int()
		elem_type := g.resolve_c_type_to_raw(elem_c) or { return none }
		raw := types.Type(types.ArrayFixed{
			len:       len
			elem_type: elem_type
		})
		if is_ptr {
			return types.Pointer{
				base_type: raw
			}
		}
		return raw
	}
	// Handle Array_* → types.Array{elem_type: ...}
	if type_name.starts_with('Array_') {
		elem_c := unmangle_c_ptr_type(type_name['Array_'.len..])
		elem_type := g.resolve_c_type_to_raw(elem_c) or { return none }
		raw := types.Type(types.Array{
			elem_type: elem_type
		})
		if is_ptr {
			return types.Pointer{
				base_type: raw
			}
		}
		return raw
	}
	if type_name.starts_with('Map_') {
		if info := g.ensure_map_type_info(type_name) {
			key_type := g.resolve_c_type_to_raw(info.key_c_type) or { return none }
			value_type := g.resolve_c_type_to_raw(info.value_c_type) or { return none }
			return types.Map{
				key_type:   key_type
				value_type: value_type
			}
		}
	}
	// Handle primitive type names (int, string, bool, etc.) directly without scope lookup.
	if prim_raw := resolve_primitive_type_name(type_name) {
		if is_ptr {
			return types.Pointer{
				base_type: prim_raw
			}
		}
		return prim_raw
	}
	// Pointer-like type aliases: voidptr → void*, charptr → char*, byteptr → u8*
	if type_name in ['void', 'voidptr', 'void*'] {
		return types.Pointer{
			base_type: types.void_
		}
	}
	if type_name in ['charptr', 'char*'] {
		// char is not available outside types module; use a scope lookup fallback.
		if scope := g.env_scope('builtin') {
			if obj := scope.objects['char'] {
				return types.Pointer{
					base_type: obj.typ()
				}
			}
		}
		return none
	}
	if type_name in ['byteptr', 'u8*'] {
		return types.Pointer{
			base_type: types.Primitive{
				props: .integer | .unsigned
				size:  8
			}
		}
	}
	mut mod_name := g.cur_module
	if type_name.contains('__') {
		mod_name = type_name.all_before_last('__')
		type_name = type_name.all_after_last('__')
	}
	if scope := g.env_scope(mod_name) {
		if obj := scope.objects[type_name] {
			raw := obj.typ()
			if is_ptr {
				return types.Pointer{
					base_type: raw
				}
			}
			return raw
		}
	}
	if mod_name != 'builtin' {
		if scope := g.env_scope('builtin') {
			if obj := scope.objects[type_name] {
				raw := obj.typ()
				if is_ptr {
					return types.Pointer{
						base_type: raw
					}
				}
				return raw
			}
		}
	}
	return none
}

fn (mut g Gen) expr_pointer_return_type(expr ast.Expr) string {
	unwrapped := strip_expr_wrappers(expr)
	match unwrapped {
		ast.CallExpr {
			if ret := g.get_call_return_type(unwrapped.lhs, unwrapped.args) {
				return ret
			}
		}
		else {}
	}

	return ''
}

fn (mut g Gen) field_type_name(e ast.Expr) string {
	direct_name := g.expr_type_to_c(e)
	if direct_name != '' && direct_name != 'int' {
		return direct_name
	}
	if raw_type := g.get_raw_type(e) {
		raw_name := g.types_type_to_c(raw_type)
		if raw_name != '' && raw_name != 'int' {
			return raw_name
		}
	}
	match e {
		ast.Ident {
			if g.is_module_local_type(e.name) {
				return '${g.cur_module}__${e.name}'
			}
			return e.name
		}
		ast.SelectorExpr {
			if e.lhs is ast.Ident {
				return '${e.lhs.name}__${e.rhs.name}'
			}
			return ''
		}
		ast.PrefixExpr {
			return g.field_type_name(e.expr)
		}
		ast.Type {
			if e is ast.ArrayType {
				return g.field_type_name(e.elem_type)
			}
			if e is ast.ArrayFixedType {
				return g.field_type_name(e.elem_type)
			}
			if e is ast.MapType {
				return 'map'
			}
			if e is ast.OptionType {
				return g.field_type_name(e.base_type)
			}
			if e is ast.ResultType {
				return g.field_type_name(e.base_type)
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (mut g Gen) lookup_struct_type(struct_name string) types.Struct {
	if g.env == unsafe { nil } {
		return types.Struct{}
	}
	mod_name := if g.cur_module != '' { g.cur_module } else { 'main' }
	mut out := types.Struct{}
	if scope := g.env_scope(mod_name) {
		if obj := scope.objects[struct_name] {
			typ := obj.typ()
			if typ is types.Struct {
				out = typ
			}
		}
	}
	return out
}

// lookup_union_variant_struct resolves a union variant type name (which may be
// a type alias like GgRect -> gg.Rect) to the underlying Struct, following
// aliases until a Struct is found.
fn (mut g Gen) lookup_union_variant_struct(type_name string) types.Struct {
	if g.env == unsafe { nil } {
		return types.Struct{}
	}
	// Try to look up the type, following aliases
	mut mod_name := ''
	mut short_name := type_name
	if idx := type_name.index('__') {
		mod_name = type_name[..idx]
		short_name = type_name[idx + 2..]
	}
	mut modules_to_try := []string{}
	if mod_name != '' {
		modules_to_try << mod_name
	}
	cur_mod := if g.cur_module != '' { g.cur_module } else { 'main' }
	modules_to_try << cur_mod
	modules_to_try << 'main'
	modules_to_try << 'builtin'
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			file_mod := flat_file_module_name(g.flat.file_cursor(i))
			if file_mod !in modules_to_try {
				modules_to_try << file_mod
			}
		}
	} else {
		for file in g.files {
			if file.mod !in modules_to_try {
				modules_to_try << file.mod
			}
		}
	}
	mut tried := map[string]bool{}
	for try_mod in modules_to_try {
		if tried[try_mod] {
			continue
		}
		tried[try_mod] = true
		if scope := g.env_scope(try_mod) {
			if obj := scope.lookup_parent(short_name, 0) {
				return g.resolve_type_to_struct(obj.typ())
			}
		}
	}
	return types.Struct{}
}

// resolve_type_to_struct follows alias chains to find the underlying Struct.
fn (g &Gen) resolve_type_to_struct(t types.Type) types.Struct {
	if t is types.Struct {
		return t
	}
	if t is types.Alias {
		return g.resolve_type_to_struct(t.base_type)
	}
	return types.Struct{}
}

fn (mut g Gen) set_struct_info_full_context(info StructDeclInfo) {
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			if fc.name() == info.file_name {
				g.set_file_cursor_module(fc)
				return
			}
		}
		g.set_struct_info_context(info)
		return
	}
	for file in g.files {
		if file.name == info.file_name {
			g.set_file_module(file)
			return
		}
	}
	g.set_struct_info_context(info)
}

// types_type_to_c converts a types.Type to a C type string
fn selector_struct_field_type_from_type(t types.Type, field_name string) ?types.Type {
	if !type_has_valid_data(t) {
		return none
	}
	match t {
		types.Struct {
			for field in t.fields {
				if field.name == field_name {
					return field.typ
				}
			}
			return none
		}
		types.Interface {
			for field in t.fields {
				if field.name == field_name {
					return field.typ
				}
			}
			return none
		}
		types.Pointer {
			if field_type := selector_struct_field_type_from_type(t.base_type, field_name) {
				return field_type
			}
			return none
		}
		types.Alias {
			if field_type := selector_struct_field_type_from_type(t.base_type, field_name) {
				return field_type
			}
			return none
		}
		else {
			return none
		}
	}
}

fn array_alias_field_type_by_name(type_name string, field_name string) ?string {
	base_name := strip_pointer_type_name(type_name)
	if base_name !in ['array', 'strings__Builder', 'Builder'] && !base_name.starts_with('Array_') {
		return none
	}
	return match field_name {
		'data' { 'void*' }
		'offset', 'len', 'cap', 'element_size' { 'int' }
		'flags' { 'ArrayFlags' }
		else { none }
	}
}

fn (mut g Gen) lookup_struct_field_type_by_name(struct_name string, field_name string) ?string {
	if struct_name == '' || field_name == '' {
		return none
	}
	use_cache := g.active_generic_types.len == 0
	cache_key := if use_cache { '${g.cur_module}|${struct_name}.${field_name}' } else { '' }
	if use_cache {
		if cached := g.struct_field_lookup_cache[cache_key] {
			return cached
		}
		if cache_key in g.struct_field_lookup_miss {
			return none
		}
	}
	mut candidates := []string{}
	candidates << struct_name
	base_name := strip_pointer_type_name(struct_name)
	if field_type := array_alias_field_type_by_name(base_name, field_name) {
		if use_cache {
			g.struct_field_lookup_cache[cache_key] = field_type
		}
		return field_type
	}
	if base_name != '' && base_name !in candidates {
		candidates << base_name
	}
	if base_name != '' && !base_name.contains('__') {
		qualified_name := g.qualify_module_local_type_name(base_name)
		if qualified_name != '' && qualified_name != base_name && qualified_name !in candidates {
			candidates << qualified_name
		}
	}
	for candidate in candidates {
		full_key := '${candidate}.${field_name}'
		if field_type := g.struct_field_types[full_key] {
			if field_type != '' {
				if use_cache {
					g.struct_field_lookup_cache[cache_key] = field_type
				}
				return field_type
			}
		}
		short_name := short_type_name(candidate)
		if short_name != '' && short_name != candidate {
			short_key := '${short_name}.${field_name}'
			if field_type := g.struct_field_types[short_key] {
				if field_type != '' {
					if use_cache {
						g.struct_field_lookup_cache[cache_key] = field_type
					}
					return field_type
				}
			}
		}
		if info := g.lookup_embedded_field_info(candidate, field_name) {
			if info.field_type != '' {
				if use_cache {
					g.struct_field_lookup_cache[cache_key] = info.field_type
				}
				return info.field_type
			}
		}
	}
	if use_cache {
		g.struct_field_lookup_miss[cache_key] = true
	}
	return none
}

fn (mut g Gen) selector_declared_field_type(sel ast.SelectorExpr) string {
	rhs := sel.rhs.name
	if rhs == '' {
		return ''
	}
	lhs_struct_name := g.selector_struct_name(sel.lhs)
	if lhs_struct_name != '' {
		if g.active_generic_types.len > 0 || lhs_struct_name in g.generic_struct_bindings
			|| lhs_struct_name in g.generic_struct_instances || lhs_struct_name.contains('_T_') {
			if field_type := g.lookup_struct_field_type_by_name(lhs_struct_name, rhs) {
				return field_type
			}
		}
		if field_type := g.lookup_struct_decl_field_type_by_name(lhs_struct_name, rhs) {
			return field_type
		}
		if field_type := g.lookup_struct_field_type_by_name(lhs_struct_name, rhs) {
			return field_type
		}
	}
	lhs_expr_type := g.get_expr_type(sel.lhs)
	if lhs_expr_type != '' && lhs_expr_type != 'int' {
		if field_type := g.lookup_struct_field_type_by_name(lhs_expr_type, rhs) {
			return field_type
		}
	}
	mut raw_resolved := ''
	if lhs_raw := g.get_raw_type(sel.lhs) {
		if field_type := selector_struct_field_type_from_type(lhs_raw, rhs) {
			resolved := g.types_type_to_c(field_type)
			if resolved != '' && resolved !in ['int', 'array'] {
				return resolved
			}
			raw_resolved = resolved
		}
	}
	return raw_resolved
}

fn (mut g Gen) selector_storage_field_type(sel ast.SelectorExpr) string {
	rhs := sel.rhs.name
	if rhs == '' {
		return ''
	}
	if sel.lhs is ast.Ident {
		if local_type := g.get_local_var_c_type(sel.lhs.name) {
			base := strip_pointer_type_name(local_type)
			if field_type := g.lookup_struct_decl_field_type_by_name(base, rhs) {
				return field_type
			}
		}
	}
	if raw_lhs := g.get_raw_type(sel.lhs) {
		lhs_type := g.types_type_to_c(raw_lhs)
		if field_type := g.lookup_struct_decl_field_type_by_name(strip_pointer_type_name(lhs_type),
			rhs)
		{
			return field_type
		}
	}
	return g.selector_declared_field_type(sel)
}

fn (mut g Gen) lookup_struct_decl_field_type_by_name(struct_name string, field_name string) ?string {
	if struct_name == '' || field_name == '' {
		return none
	}
	base_name := strip_pointer_type_name(struct_name)
	use_cache := g.active_generic_types.len == 0
	cache_key := if use_cache { 'decl|${g.cur_module}|${base_name}.${field_name}' } else { '' }
	if use_cache {
		if cached := g.struct_field_lookup_cache[cache_key] {
			return cached
		}
		if cache_key in g.struct_field_lookup_miss {
			return none
		}
	}
	if decl_info := g.find_struct_decl_info_by_c_name(base_name) {
		old_file := g.cur_file_name
		old_module := g.cur_module
		old_import_modules := g.cur_import_modules.clone()
		old_resolved_modules := g.resolved_module_names.clone()
		old_module_ident_cache := g.is_module_ident_cache.clone()
		g.set_struct_info_full_context(decl_info)
		for field in decl_info.decl.fields {
			escaped_name := escape_c_keyword(field.name)
			if field.name != field_name && escaped_name != field_name {
				continue
			}
			field_type := g.expr_type_to_c(field.typ)
			g.cur_file_name = old_file
			g.cur_module = old_module
			g.cur_import_modules = old_import_modules.clone()
			g.resolved_module_names = old_resolved_modules.clone()
			g.is_module_ident_cache = old_module_ident_cache.clone()
			if use_cache {
				g.struct_field_lookup_cache[cache_key] = field_type
			}
			return field_type
		}
		g.cur_file_name = old_file
		g.cur_module = old_module
		g.cur_import_modules = old_import_modules.clone()
		g.resolved_module_names = old_resolved_modules.clone()
		g.is_module_ident_cache = old_module_ident_cache.clone()
	}
	if use_cache {
		g.struct_field_lookup_miss[cache_key] = true
	}
	return none
}

fn (mut g Gen) selector_generated_field_type(sel ast.SelectorExpr) string {
	rhs := sel.rhs.name
	if rhs == '' {
		return ''
	}
	mut candidates := []string{}
	lhs_direct_type := g.direct_known_c_type_for_expr(sel.lhs)
	if lhs_direct_type != '' {
		candidates << lhs_direct_type
	}
	if sel.lhs is ast.SelectorExpr {
		lhs_field_type := g.selector_generated_field_type(sel.lhs)
		if lhs_field_type != '' {
			candidates << lhs_field_type
		}
	}
	lhs_struct_name := g.selector_struct_name(sel.lhs)
	if lhs_struct_name != '' {
		candidates << lhs_struct_name
	}
	lhs_expr_type := g.get_expr_type(sel.lhs)
	if lhs_expr_type != '' && lhs_expr_type != 'int' {
		candidates << lhs_expr_type
	}
	if lhs_raw := g.get_raw_type(sel.lhs) {
		lhs_raw_type := g.types_type_to_c(lhs_raw)
		if lhs_raw_type != '' && lhs_raw_type != 'int' {
			candidates << lhs_raw_type
		}
	}
	mut seen := map[string]bool{}
	for candidate in candidates {
		base := strip_pointer_type_name(candidate)
		if base == '' || base == 'int' || seen[base] {
			continue
		}
		seen[base] = true
		if field_type := g.lookup_struct_field_type_by_name(base, rhs) {
			return field_type
		}
	}
	return ''
}

fn (mut g Gen) selector_sum_common_field_type(sel ast.SelectorExpr) string {
	if sel.rhs.name.starts_with('_') {
		return ''
	}
	mut lhs_sum_type := ''
	if local_type := g.local_var_c_type_for_expr(sel.lhs) {
		lhs_sum_type = g.resolve_sum_type_name_for_common_field(local_type)
	}
	if lhs_sum_type == '' {
		if raw_lhs := g.get_raw_type(sel.lhs) {
			match raw_lhs {
				types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs)
				}
				types.Pointer {
					if raw_lhs.base_type is types.SumType {
						lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
					}
				}
				types.Alias {
					if raw_lhs.base_type is types.SumType {
						lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
					}
				}
				else {}
			}
		}
	}
	if lhs_sum_type == '' {
		lhs_type := g.get_expr_type(sel.lhs)
		lhs_sum_type = g.resolve_sum_type_name_for_common_field(lhs_type)
	}
	if lhs_sum_type == '' {
		return ''
	}
	infos := g.sum_common_field_infos(lhs_sum_type, sel.rhs.name, []string{}) or { return '' }
	if infos.len == 0 {
		return ''
	}
	return infos[0].field_type
}

fn selector_field_type_cache_key(sel ast.SelectorExpr) string {
	if sel.pos.id == 0 || sel.rhs.name == '' {
		return ''
	}
	return '${sel.pos.id}|${sel.rhs.name}'
}

fn (mut g Gen) cache_selector_field_type(cache_key string, typ string) string {
	if cache_key == '' {
		return typ
	}
	if typ == '' {
		g.selector_field_type_miss[cache_key] = true
	} else {
		g.selector_field_type_cache[cache_key] = typ
	}
	return typ
}

fn (mut g Gen) selector_field_type(sel ast.SelectorExpr) string {
	// Fast path: use the type checker's env pos.id lookup (O(1) array access).
	// Uses get_env_c_type (alias-preserving) since alias types like
	// strings__Builder, ssa__TypeID are correct for struct field types.
	// Skip _data and _result_/_option_ internal fields (.data, .err, .is_error, .state):
	// the checker stores incorrect types for these on wrapper structs.
	rhs := sel.rhs.name
	cache_key := if g.active_generic_types.len == 0 {
		selector_field_type_cache_key(sel)
	} else {
		''
	}
	if cache_key != '' {
		if cached := g.selector_field_type_cache[cache_key] {
			return cached
		}
		if cache_key in g.selector_field_type_miss {
			return ''
		}
	}
	lhs_type0 := g.builtin_string_field_lhs_type(sel.lhs, rhs).trim_right('*')
	if lhs_type0 == 'string' {
		return g.cache_selector_field_type(cache_key, builtin_string_field_c_type(rhs))
	}
	mut env_type := ''
	if !rhs.starts_with('_') && rhs !in ['data', 'err', 'is_error', 'state'] {
		if t := g.get_env_c_type(sel) {
			// Nested selector chains can be reduced to `int` in env metadata.
			// Keep resolving through raw/field information in that case.
			if t != 'int' && t != 'void' && !t.starts_with('_result_') && !t.starts_with('_option_') {
				// In specialized generic functions, the env may return the substituted
				// generic type param (e.g. Slack for T) instead of the actual struct
				// field type (e.g. ValueInfo). Cross-check against struct_field_types.
				if g.active_generic_types.len > 0 {
					mut lhs_struct := g.selector_struct_name(sel.lhs)
					if lhs_struct != '' {
						if field_t := g.lookup_struct_field_type_by_name(lhs_struct, rhs) {
							if field_t != t {
								return g.cache_selector_field_type(cache_key, field_t)
							}
						}
					}
					generic_lhs_struct := g.resolve_generic_struct_field_name(sel.lhs)
					if generic_lhs_struct != '' && generic_lhs_struct != lhs_struct {
						if field_t := g.lookup_struct_field_type_by_name(generic_lhs_struct, rhs) {
							if field_t != t {
								return g.cache_selector_field_type(cache_key, field_t)
							}
						}
					}
				}
				return g.cache_selector_field_type(cache_key, t)
			}
			env_type = t
		}
	}
	// Handle _result_/_option_ wrapper field access (.data, .err, .is_error, .state)
	if rhs in ['data', 'err', 'is_error', 'state'] {
		lhs_type := g.get_expr_type(sel.lhs)
		if lhs_type.starts_with('_result_') {
			if rhs == 'is_error' {
				return g.cache_selector_field_type(cache_key, 'bool')
			}
			if rhs == 'err' {
				return g.cache_selector_field_type(cache_key, 'IError')
			}
			if rhs == 'data' {
				base := g.result_value_type(lhs_type)
				if base != '' && base != 'void' {
					return g.cache_selector_field_type(cache_key, base)
				}
			}
		}
		if lhs_type.starts_with('_option_') {
			if rhs == 'state' {
				return g.cache_selector_field_type(cache_key, 'u8')
			}
			if rhs == 'err' {
				return g.cache_selector_field_type(cache_key, 'IError')
			}
			if rhs == 'data' {
				base := option_value_type(lhs_type)
				if base != '' && base != 'void' {
					return g.cache_selector_field_type(cache_key, base)
				}
			}
		}
	}
	if data_field := g.selector_interface_data_field(sel) {
		return g.cache_selector_field_type(cache_key, data_field.c_type)
	}
	if raw_type := g.get_raw_type(sel) {
		resolved := g.types_type_to_c(raw_type)
		if resolved != '' && resolved != 'void'
			&& ((!resolved.starts_with('_result_') && !resolved.starts_with('_option_'))
			|| rhs in ['data', 'err', 'is_error', 'state']) {
			return g.cache_selector_field_type(cache_key, resolved)
		}
	}
	lhs_struct_name := g.selector_struct_name(sel.lhs)
	if lhs_struct_name != '' {
		if field_type := g.lookup_struct_field_type_by_name(lhs_struct_name, rhs) {
			return g.cache_selector_field_type(cache_key, field_type)
		}
	}
	lhs_expr_type := if lhs_type0 != '' { lhs_type0 } else { g.get_expr_type(sel.lhs) }
	if lhs_expr_type != '' && lhs_expr_type != 'int' {
		if field_type := g.lookup_struct_field_type_by_name(lhs_expr_type, rhs) {
			return g.cache_selector_field_type(cache_key, field_type)
		}
	}
	lane_type := g.selector_vector_lane_type(sel)
	if lane_type != '' {
		return g.cache_selector_field_type(cache_key, lane_type)
	}
	generated_field_type := g.selector_generated_field_type(sel)
	if generated_field_type != '' {
		return g.cache_selector_field_type(cache_key, generated_field_type)
	}
	sum_common_field_type := g.selector_sum_common_field_type(sel)
	if sum_common_field_type != '' {
		return g.cache_selector_field_type(cache_key, sum_common_field_type)
	}
	if env_type.starts_with('_result_') || env_type.starts_with('_option_') {
		return g.cache_selector_field_type(cache_key, '')
	}
	if env_type == 'void' || env_type == 'int' {
		return g.cache_selector_field_type(cache_key, '')
	}
	return g.cache_selector_field_type(cache_key, env_type)
}

fn (mut g Gen) selector_vector_lane_type(sel ast.SelectorExpr) string {
	if vector_field_index(sel.rhs.name) < 0 {
		return ''
	}
	mut lhs_type := g.get_expr_type(sel.lhs)
	if lhs_type == '' || lhs_type == 'int' {
		if raw := g.get_raw_type(sel.lhs) {
			lhs_type = g.types_type_to_c(raw)
		}
	}
	elem_type := vector_elem_type_for_name(lhs_type)
	if elem_type != '' {
		return elem_type
	}
	if sel.lhs is ast.SelectorExpr {
		inner_type := g.selector_field_type(sel.lhs)
		if inner_type != '' {
			inner_elem_type := vector_elem_type_for_name(inner_type)
			if inner_elem_type != '' {
				return inner_elem_type
			}
		}
	}
	return ''
}

fn (mut g Gen) selector_struct_name(expr ast.Expr) string {
	direct_type := g.direct_known_c_type_for_expr(expr)
	if direct_type != '' {
		base := strip_pointer_type_name(direct_type)
		if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
			return base
		}
	}
	if expr is ast.Ident {
		if local_type := g.get_local_var_c_type(expr.name) {
			base := strip_pointer_type_name(local_type)
			if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
				return base
			}
		}
	}
	if expr is ast.SelectorExpr {
		field_type := g.selector_generated_field_type(expr)
		if field_type != '' {
			return strip_pointer_type_name(field_type)
		}
	}
	if raw_type := g.get_raw_type(expr) {
		if !type_has_valid_data(raw_type) {
			return g.get_expr_type(expr).trim_right('*')
		}
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					return raw_type.base_type.name
				}
				if raw_type.base_type is types.Alias {
					return raw_type.base_type.name
				}
			}
			types.Struct {
				return raw_type.name
			}
			types.Alias {
				return raw_type.name
			}
			else {}
		}
	}
	return g.get_expr_type(expr).trim_right('*')
}

// resolve_generic_struct_field_name returns the specialized struct name for field lookups.
// When active_generic_types are set and the struct is generic, resolves to the
// specialized instance name (e.g. json2__LinkedList_T_json2__StructFieldInfo).
fn (mut g Gen) resolve_generic_struct_field_name(expr ast.Expr) string {
	base := g.selector_struct_name(expr)
	if base == '' || g.active_generic_types.len == 0 {
		return base
	}
	// Check if this struct has multi-instantiation
	c_name := base.replace('.', '__')
	if instances := g.generic_struct_instances[c_name] {
		if instances.len > 1 {
			// Build the params_key from active_generic_types
			// Find the struct's generic params to know the order
			env_struct := g.lookup_struct_type(c_name.all_after_last('__'))
			runtime_param_names := runtime_generic_param_names(env_struct.generic_params)
			if runtime_param_names.len > 0 {
				mut param_c_names := []string{cap: runtime_param_names.len}
				for param_name in runtime_param_names {
					if concrete := g.active_generic_types[param_name] {
						param_c_names << mangle_alias_component(g.types_type_to_c(concrete))
					} else {
						return base // can't resolve, use base
					}
				}
				params_key := param_c_names.join('_')
				for inst in instances {
					if inst.params_key == params_key {
						return inst.c_name
					}
				}
			}
		}
	}
	return base
}

fn is_type_name_pointer_like(name string) bool {
	trimmed := name.trim_space()
	return trimmed.starts_with('&') || trimmed.ends_with('*') || trimmed.ends_with('ptr')
}

fn strip_pointer_type_name(name string) string {
	mut out := name.trim_space()
	if out.starts_with('&') {
		out = out[1..]
	}
	for out.ends_with('*') {
		out = out[..out.len - 1].trim_space()
	}
	return unmangle_c_ptr_type(out)
}

fn strip_one_pointer_type_name(name string) string {
	mut out := name.trim_space()
	if out.starts_with('&') {
		return unmangle_c_ptr_type(out[1..].trim_space())
	}
	if out.ends_with('*') {
		return unmangle_c_ptr_type(out[..out.len - 1].trim_space())
	}
	return unmangle_c_ptr_type(out)
}

fn short_type_name(name string) string {
	if name.contains('__') {
		return name.all_after_last('__')
	}
	return name
}

fn is_generic_placeholder_c_type_name(name string) bool {
	mut base := unmangle_c_ptr_type(name)
	base = base.trim_right('*')
	if base.contains('__') {
		base = base.all_after_last('__')
	}
	return is_generic_placeholder_type_name(base)
}

fn (mut g Gen) c_pointer_cast_selector_field_type(sel ast.SelectorExpr) string {
	target_type := match sel.lhs {
		ast.CallOrCastExpr {
			if !g.call_or_cast_lhs_is_type(sel.lhs.lhs) {
				return ''
			}
			g.expr_type_to_c(sel.lhs.lhs)
		}
		ast.CallExpr {
			if sel.lhs.args.len != 1 || !g.call_or_cast_lhs_is_type(sel.lhs.lhs) {
				return ''
			}
			g.expr_type_to_c(sel.lhs.lhs)
		}
		else {
			return ''
		}
	}

	if target_type == '' || target_type == 'int' {
		return ''
	}
	if field_type := g.lookup_struct_field_type_by_name(target_type, sel.rhs.name) {
		return field_type
	}
	if t := g.get_env_c_type(sel) {
		if t != '' && t != 'int' {
			return t
		}
	}
	return ''
}

fn (mut g Gen) c_typedef_for_interface_object_access(sel ast.SelectorExpr) string {
	if sel.rhs.name != '_object' || sel.lhs !is ast.Ident {
		return ''
	}
	lhs_type := g.get_expr_type(sel.lhs).trim_right('*')
	if lhs_type == '' {
		return ''
	}
	mut names := g.typedef_c_types.keys()
	names.sort()
	for name in names {
		if name == lhs_type || name.ends_with('__${lhs_type}') || name.ends_with(lhs_type) {
			return name
		}
	}
	mut field_keys := g.struct_field_types.keys()
	field_keys.sort()
	for key in field_keys {
		if !key.ends_with('._object') {
			continue
		}
		struct_name := key.all_before_last('._object')
		if struct_name == lhs_type || struct_name.ends_with('__${lhs_type}')
			|| struct_name.ends_with(lhs_type) {
			return struct_name
		}
	}
	return ''
}

fn ierror_wrapper_base_from_ident(name string) string {
	if !name.starts_with('IError_') {
		return ''
	}
	prefix_len := 'IError_'.len
	if name.ends_with('_type_name_wrapper') {
		return name[prefix_len..name.len - '_type_name_wrapper'.len]
	}
	if name.ends_with('_msg_wrapper') {
		return name[prefix_len..name.len - '_msg_wrapper'.len]
	}
	if name.ends_with('_code_wrapper') {
		return name[prefix_len..name.len - '_code_wrapper'.len]
	}
	return ''
}
