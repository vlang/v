// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

fn (g &Gen) result_value_type(result_type string) string {
	if !result_type.starts_with('_result_') {
		return ''
	}
	return unmangle_c_ptr_type(result_type['_result_'.len..])
}

fn option_value_type(option_type string) string {
	if !option_type.starts_with('_option_') {
		return ''
	}
	return unmangle_c_ptr_type(option_type['_option_'.len..])
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
	if name.len > 3 && name.ends_with('ptr') {
		// Don't unmangle composite type names - they are typedef'd aliases
		if name.starts_with('Array_') || name.starts_with('Map_') || name.starts_with('_option_')
			|| name.starts_with('_result_') {
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

fn map_key_type_candidates() []string {
	return ['string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte', 'rune',
		'char', 'bool', 'f32', 'f64', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr']
}

fn (mut g Gen) infer_map_type_info_from_alias_name(map_name string) ?MapTypeInfo {
	if !map_name.starts_with('Map_') || map_name.len <= 'Map_'.len {
		return none
	}
	rest := map_name['Map_'.len..]
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

fn (mut g Gen) collect_module_type_names() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if !stmt_has_valid_data(stmt) {
				continue
			}
			match stmt {
				ast.StructDecl {
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
				}
				ast.EnumDecl {
					enum_name := g.get_enum_name(stmt)
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
					if stmt.language == .c {
						continue
					}
				}
				ast.InterfaceDecl {}
				else {}
			}
		}
	}
}

fn (mut g Gen) collect_runtime_aliases() {
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			g.collect_decl_type_aliases_from_stmt(stmt)
		}
	}
	// Also use type-checker output so aliases used only in expressions are captured.
	if g.env != unsafe { nil } {
		for typ in g.env.expr_type_values {
			if typ is types.Void {
				continue
			}
			// Skip top-level aliases from env cache; declarations are collected
			// from the AST path above.
			if typ is types.Alias {
				continue
			}
			g.collect_aliases_from_type(typ)
		}
		for typ in g.env.expr_type_neg_values {
			if typ is types.Void {
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
			base := mangle_alias_component(g.types_type_to_c(t.base_type))
			g.register_alias_type('_option_' + base)
		}
		types.ResultType {
			g.collect_aliases_from_type(t.base_type)
			base := mangle_alias_component(g.types_type_to_c(t.base_type))
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
	return name in ['T', 'K', 'V', 'Type'] || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`)
}

fn (g &Gen) option_result_payload_invalid(val_type string) bool {
	if val_type == '' {
		return true
	}
	if val_type == 'void' {
		return false
	}
	return is_generic_placeholder_type_name(val_type)
}

fn (g &Gen) option_result_payload_ready(val_type string) bool {
	if g.option_result_payload_invalid(val_type) || val_type == 'void' {
		return false
	}
	ierror_body_key := 'body_IError'
	ierror_builtin_body_key := 'body_builtin__IError'
	if ierror_body_key !in g.emitted_types && ierror_builtin_body_key !in g.emitted_types {
		return false
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
	if val_type.starts_with('Array_fixed_') {
		body_key := 'body_${val_type}'
		alias_key := 'alias_${val_type}'
		return body_key in g.emitted_types || alias_key in g.emitted_types
	}
	if val_type.starts_with('Array_') {
		return true
	}
	if val_type.starts_with('Map_') {
		return true
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
		g.result_aliases[name] = true
		return
	}
	if name.starts_with('_option_') {
		val_type := option_value_type(name)
		if g.option_result_payload_invalid(val_type) {
			return
		}
		g.option_aliases[name] = true
	}
}

fn (g &Gen) is_pointer_type(e ast.Expr) bool {
	if e is ast.PrefixExpr {
		return e.op == .amp
	}
	if e is ast.Ident {
		return e.name in ['voidptr', 'charptr', 'byteptr']
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
			mut ret_type := 'void'
			if fn_type.return_type !is ast.EmptyExpr {
				ret_type = g.expr_type_to_c(fn_type.return_type)
			}
			g.sb.write_string('typedef ${ret_type} (*${name})(')
			for i, param in fn_type.params {
				if i > 0 {
					g.sb.write_string(', ')
				}
				param_type := g.expr_type_to_c(param.typ)
				if param.is_mut {
					g.sb.write_string('${param_type}*')
				} else {
					g.sb.write_string(param_type)
				}
			}
			g.sb.writeln(');')
			return
		}
	}
	base_type := g.expr_type_to_c(node.base_type)
	if base_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize'] {
		g.primitive_type_aliases[name] = true
	}
	if base_type == 'array' || base_type.starts_with('Array_') || base_type in g.array_aliases {
		g.array_aliases[name] = true
	}
	g.sb.writeln('typedef ${base_type} ${name};')
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
	return name
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

fn (mut g Gen) emit_tuple_aliases() {
	mut names := g.tuple_aliases.keys()
	names.sort()
	for name in names {
		body_key := 'body_${name}'
		if body_key in g.emitted_types {
			continue
		}
		g.emitted_types[body_key] = true
		field_types := g.tuple_aliases[name]
		g.sb.writeln('typedef struct ${name} {')
		for i, field_type in field_types {
			g.sb.writeln('\t${field_type} arg${i};')
		}
		g.sb.writeln('} ${name};')
	}
}

fn (g &Gen) should_use_memcmp_eq(lhs_type string, rhs_type string) bool {
	if lhs_type == '' || rhs_type == '' || lhs_type != rhs_type {
		return false
	}
	if lhs_type in primitive_types || lhs_type == 'string' {
		return false
	}
	if lhs_type.ends_with('*') || lhs_type.ends_with('ptr') {
		return false
	}
	if lhs_type.starts_with('Array_') || lhs_type.starts_with('Map_') {
		return false
	}
	return true
}

// struct_has_ref_fields checks if a struct has any reference-type fields
// (string, array, map) that require deep comparison instead of memcmp.
fn (g &Gen) struct_has_ref_fields(s types.Struct) bool {
	for field in s.fields {
		match field.typ {
			types.String, types.Array, types.Map { return true }
			else {}
		}
	}
	return false
}

// gen_struct_field_eq_expr generates an inline field-by-field equality expression
// for structs with reference-type fields.
fn (mut g Gen) gen_struct_field_eq_expr(s types.Struct, va string, vb string) string {
	mut parts := []string{}
	for field in s.fields {
		fname := field.name
		match field.typ {
			types.String {
				parts << 'string__eq(${va}.${fname}, ${vb}.${fname})'
			}
			types.Array {
				parts << '__v2_array_eq(${va}.${fname}, ${vb}.${fname})'
			}
			types.Map {
				c_type := g.types_type_to_c(field.typ)
				if c_type.starts_with('Map_') {
					parts << '${c_type}_map_eq(${va}.${fname}, ${vb}.${fname})'
				} else {
					parts << 'map_map_eq(${va}.${fname}, ${vb}.${fname})'
				}
			}
			else {
				parts << '${va}.${fname} == ${vb}.${fname}'
			}
		}
	}
	if parts.len == 0 {
		return '1'
	}
	return parts.join(' && ')
}

fn (mut g Gen) method_receiver_base_type(expr ast.Expr) string {
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
	// Fast path: env pos.id O(1) lookup (covers most non-Ident receivers).
	if g.env != unsafe { nil } {
		pos := expr.pos()
		if pos.is_valid() {
			if raw_type := g.env.get_expr_type(pos.id) {
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
						if c != '' && c != 'int' {
							return c
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
				return g.types_type_to_c(raw_type)
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

fn (g &Gen) types_type_to_c(t types.Type) string {
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
		types.Struct {
			name := t.name
			// C struct types need the 'struct' keyword in C
			if !name.contains('__')
				&& name in ['tm', 'timespec', 'timeval', 'stat', 'dirent', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'addrinfo', 'msghdr', 'iovec', 'pollfd', 'rusage', 'rlimit', 'sigaction', 'winsize', 'utsname'] {
				return 'struct ${name}'
			}
			return name
		}
		types.String {
			return 'string'
		}
		types.Alias {
			return t.name
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
			return '_option_' + mangle_alias_component(base)
		}
		types.ResultType {
			base := g.types_type_to_c(t.base_type)
			return '_result_' + mangle_alias_component(base)
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
	// Invalidate negative cache entry if it was previously marked as non-local.
	if name in g.not_local_var_cache {
		g.not_local_var_cache.delete(name)
	}
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
				g.runtime_local_types[name] = c
			}
			return c
		}
	}
	g.not_local_var_cache[name] = true
	return none
}

// get_expr_type returns the C type string for an expression
fn (mut g Gen) get_expr_type(node ast.Expr) string {
	if !expr_has_valid_data(node) {
		return ''
	}
	// For identifiers, check local/parameter types first (authoritative),
	// then fall back to env position lookup.
	if node is ast.Ident {
		if node.name == 'err' {
			return 'IError'
		}
		if local_type := g.get_local_var_c_type(node.name) {
			return local_type
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
		// Ident already tried env + all scopes above; avoid redundant second lookup below.
		return 'int'
	}
	// For IndexExpr on pointer-to-pointer or pointer-to-string types, prefer raw-type-based
	// inference over env (env may store the wrong type, e.g. char instead of char*).
	if node is ast.IndexExpr {
		if lhs_raw := g.get_raw_type(node.lhs) {
			if lhs_raw is types.Pointer {
				if lhs_raw.base_type is types.Pointer || lhs_raw.base_type is types.String {
					return g.types_type_to_c(lhs_raw.base_type)
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
	// Try environment lookup
	if t := g.get_expr_type_from_env(node) {
		// For array element-returning methods, prefer element type inference over the
		// generic void* return type from function metadata.
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
			if ret := g.get_call_return_type(node.lhs, node.args.len) {
				if ret != '' {
					return ret
				}
			}
		}
		// For arithmetic InfixExpr, env may return void* when it should be a
		// numeric type.  Fall through to operand-based inference.
		if t in ['void*', 'voidptr'] && node is ast.InfixExpr
			&& node.op in [.plus, .minus, .mul, .div, .mod] {
			// fall through to InfixExpr handler below
		} else if t == 'bool' && node is ast.BasicLiteral && node.kind == .number {
			// Numeric literals mistyped as bool by env (e.g. `1 in map` context).
			return 'int'
		} else if t in ['void*', 'voidptr'] && node is ast.IndexExpr {
			// For IndexExpr, env returns void* for fixed-array element access.
			// Try to infer element type from the container type.
			container_type := g.get_expr_type(node.lhs)
			if container_type.starts_with('Array_fixed_') {
				// Array_fixed_u16_8 => element type is u16
				rest := container_type['Array_fixed_'.len..]
				last_underscore := rest.last_index('_') or { -1 }
				if last_underscore > 0 {
					elem := rest[..last_underscore]
					return elem
				}
			}
			return t
		} else {
			return t
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
			if node.op == .mul {
				// Dereference: *(T*)(x) -> T
				inner_t := g.get_expr_type(node.expr)
				if inner_t.ends_with('*') {
					return inner_t[..inner_t.len - 1]
				}
				return inner_t
			}
			if node.op == .amp {
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
			return g.get_if_expr_type(node)
		}
		ast.IndexExpr {
			if node.lhs is ast.SelectorExpr {
				elem_type := g.fixed_array_selector_elem_type(node.lhs)
				if elem_type != '' {
					return elem_type
				}
			}
			// Try to get element type from LHS type
			if raw_type := g.get_raw_type(node.lhs) {
				match raw_type {
					types.Array {
						return g.types_type_to_c(raw_type.elem_type)
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
								return g.types_type_to_c(raw_type.base_type.elem_type)
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
				elem := array_alias_elem_type(lhs_type)
				if elem != '' {
					return elem
				}
			}
			return 'int'
		}
		ast.InitExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.ArrayInitExpr {
			mut elem := g.extract_array_elem_type(node.typ)
			if elem != '' {
				if g.is_dynamic_array_type(node.typ) {
					return 'Array_' + elem
				}
				// Get size from type annotation if available, fallback to exprs.len
				mut fixed_size := node.exprs.len
				if node.typ is ast.Type && node.typ is ast.ArrayFixedType {
					fixed_typ := node.typ as ast.ArrayFixedType
					if fixed_typ.len is ast.BasicLiteral && fixed_typ.len.kind == .number {
						fixed_size = fixed_typ.len.value.int()
					}
				}
				if fixed_size == 0 && node.exprs.len > 0 {
					fixed_size = node.exprs.len
				}
				// For zero-size fixed arrays with init (e.g. [3][3]int{init:...}),
				// use ArrayFixedType-based size from the parent type annotation
				if fixed_size == 0 && node.len !is ast.EmptyExpr {
					if node.len is ast.BasicLiteral && node.len.kind == .number {
						fixed_size = node.len.value.int()
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
				if elem != '' && elem != 'int_literal' {
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
			if ret := g.get_call_return_type(node.lhs, node.args.len) {
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

// expr_type_to_c converts an AST type expression to a C type string
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
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			if name == 'byteptr' {
				return 'u8*'
			}
			// Detect mangled pointer types from transformer (e.g. FILEptr -> FILE*)
			if name.len > 3 && name.ends_with('ptr') {
				base := name[..name.len - 3]
				if base in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t',
					'pthread_cond_t', 'pthread_rwlock_t', 'pthread_attr_t'] {
					return base + '*'
				}
			}
			if g.is_module_local_type(name) {
				return g.cur_module + '__' + name
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
			if e.lhs is ast.Ident {
				// C interop types: C.FILE -> FILE, C.tm -> struct tm
				if e.lhs.name == 'C' {
					name := e.rhs.name
					// Known C typedefs don't need 'struct' prefix
					if name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t',
						'pthread_cond_t', 'pthread_rwlock_t', 'pthread_attr_t', 'jmp_buf',
						'sigjmp_buf', 'sigset_t', 'size_t', 'ssize_t', 'off_t', 'mode_t', 'pid_t',
						'uid_t', 'gid_t', 'time_t', 'clock_t', 'socklen_t', 'dev_t', 'ino_t',
						'nlink_t', 'blksize_t', 'blkcnt_t', 'cc_t', 'speed_t', 'tcflag_t', 'fd_set',
						'mach_timebase_info_data_t'] {
						return name
					}
					return 'struct ' + name
				}
				return e.lhs.name + '__' + e.rhs.name
			}
			return g.expr_type_to_c(e.lhs) + '__' + e.rhs.name
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
				size_str := expr_to_int_str(e.len)
				fixed_type := 'Array_fixed_' + elem_type + '_' + size_str
				g.register_alias_type(fixed_type)
				g.collected_fixed_array_types[fixed_type] = FixedArrayInfo{
					elem_type: g.expr_type_to_c(e.elem_type)
					size:      size_str.int()
				}
				return fixed_type
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
				base_type := mangle_alias_component(g.expr_type_to_c(e.base_type))
				option_type := '_option_' + base_type
				g.register_alias_type(option_type)
				return option_type
			}
			if e is ast.ResultType {
				base_type := mangle_alias_component(g.expr_type_to_c(e.base_type))
				result_type := '_result_' + base_type
				g.register_alias_type(result_type)
				return result_type
			}
			if e is ast.FnType {
				return 'void*'
			}
			if e is ast.NilType {
				return 'void*'
			}
			if e is ast.NoneType {
				return 'None__'
			}
			return 'int'
		}
		ast.ModifierExpr {
			// Handle shared/mut modifiers: unwrap and use the inner type
			return g.expr_type_to_c(e.expr)
		}
		else {
			return 'int'
		}
	}
}

// is_c_type_name checks if a name refers to a C type (struct, typedef) vs a C function.
fn (g &Gen) is_c_type_name(name string) bool {
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'stat', 'tm', 'timespec', 'timeval', 'dirent',
		'termios', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'fd_set',
		'mach_timebase_info_data_t']
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
	if obj := g.lookup_module_scope_object(name) {
		return obj is types.Type
	}
	return false
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
	if g.env == unsafe { nil } {
		return none
	}
	if !expr_has_valid_data(node) {
		return none
	}
	// Fast path: env pos.id O(1) lookup for non-compound expressions.
	if node !is ast.Ident && node !is ast.SelectorExpr && node !is ast.IndexExpr {
		pos := node.pos()
		if pos.is_valid() {
			return g.env.get_expr_type(pos.id)
		}
	}
	// For identifiers, check function scope first
	if node is ast.Ident {
		// Fast path: if is_module_ident_cache already knows this is a module, skip scope walks.
		if cached := g.is_module_ident_cache[node.name] {
			if cached {
				return none
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
		// Fallback: try runtime_local_types to resolve for-loop vars and other locals
		// not found in the scope chain (scope may not include transformer-generated vars).
		if local_type := g.runtime_local_types[node.name] {
			resolved := g.resolve_c_type_to_raw(local_type)
			if resolved != none {
				return resolved
			}
		}
	}
	// For SelectorExpr, resolve through struct field lookup (more reliable than position)
	if node is ast.SelectorExpr {
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
			if field_type := selector_struct_field_type_from_type(lhs_type, node.rhs.name) {
				return field_type
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
				return lhs_type.base_type
			}
		}
	}
	// For UnsafeExpr, infer type from the last statement in the block.
	if node is ast.UnsafeExpr {
		if node.stmts.len > 0 {
			last := node.stmts[node.stmts.len - 1]
			if last is ast.ExprStmt {
				return g.get_raw_type(last.expr)
			}
		}
	}
	// Try environment lookup by position
	if !expr_has_valid_data(node) {
		return none
	}
	pos := node.pos()
	if pos.is_valid() {
		return g.env.get_expr_type(pos.id)
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
			if ret := g.get_call_return_type(unwrapped.lhs, unwrapped.args.len) {
				return ret
			}
		}
		else {}
	}
	return ''
}

fn (g &Gen) field_type_name(e ast.Expr) string {
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

// types_type_to_c converts a types.Type to a C type string
fn selector_struct_field_type_from_type(t types.Type, field_name string) ?types.Type {
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
			return selector_struct_field_type_from_type(t.base_type, field_name)
		}
		types.Alias {
			return selector_struct_field_type_from_type(t.base_type, field_name)
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) selector_field_type(sel ast.SelectorExpr) string {
	// Fast path: use the type checker's env pos.id lookup (O(1) array access).
	// Uses get_env_c_type (alias-preserving) since alias types like
	// strings__Builder, ssa__TypeID are correct for struct field types.
	// Skip _data and _result_/_option_ internal fields (.data, .err, .is_error, .state):
	// the checker stores incorrect types for these on wrapper structs.
	rhs := sel.rhs.name
	if !rhs.starts_with('_') && rhs !in ['data', 'err', 'is_error', 'state'] {
		if t := g.get_env_c_type(sel) {
			return t
		}
	}
	// Handle _result_/_option_ wrapper field access (.data, .err, .is_error, .state)
	if rhs in ['data', 'err', 'is_error', 'state'] {
		lhs_type := g.get_expr_type(sel.lhs)
		if lhs_type.starts_with('_result_') {
			if rhs == 'is_error' {
				return 'bool'
			}
			if rhs == 'err' {
				return 'IError'
			}
			if rhs == 'data' {
				base := g.result_value_type(lhs_type)
				if base != '' && base != 'void' {
					return base
				}
			}
		}
		if lhs_type.starts_with('_option_') {
			if rhs == 'state' {
				return 'u8'
			}
			if rhs == 'err' {
				return 'IError'
			}
			if rhs == 'data' {
				base := option_value_type(lhs_type)
				if base != '' && base != 'void' {
					return base
				}
			}
		}
	}
	if raw_type := g.get_raw_type(sel) {
		resolved := g.types_type_to_c(raw_type)
		if resolved != '' {
			return resolved
		}
	}
	return ''
}

fn (mut g Gen) selector_struct_name(expr ast.Expr) string {
	if raw_type := g.get_raw_type(expr) {
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
