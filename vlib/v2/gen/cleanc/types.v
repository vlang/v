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
	return unmangle_c_ptr_type(result_type['_result_'.len..])
}

fn option_value_type(option_type string) string {
	if !option_type.starts_with('_option_') {
		return ''
	}
	return unmangle_c_ptr_type(option_type['_option_'.len..])
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

fn type_contains_generic_placeholder(typ types.Type) bool {
	match typ {
		types.NamedType {
			return is_generic_placeholder_type_name(string(typ))
		}
		types.Array {
			return type_contains_generic_placeholder(typ.elem_type)
		}
		types.ArrayFixed {
			return type_contains_generic_placeholder(typ.elem_type)
		}
		types.Map {
			return type_contains_generic_placeholder(typ.key_type)
				|| type_contains_generic_placeholder(typ.value_type)
		}
		types.OptionType {
			return type_contains_generic_placeholder(typ.base_type)
		}
		types.ResultType {
			return type_contains_generic_placeholder(typ.base_type)
		}
		types.Pointer {
			return type_contains_generic_placeholder(typ.base_type)
		}
		types.Alias {
			return type_contains_generic_placeholder(typ.base_type)
		}
		types.FnType {
			for param_type in typ.get_param_types() {
				if type_contains_generic_placeholder(param_type) {
					return true
				}
			}
			if ret := typ.get_return_type() {
				return type_contains_generic_placeholder(ret)
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

// collect_typedef_c_types scans all files for @[typedef] C struct declarations
// and records their names. This must run before any type resolution so that
// expr_type_to_c can emit these types without a 'struct' prefix.
fn (mut g Gen) collect_typedef_c_types() {
	for file in g.files {
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language == .c && stmt.attributes.has('typedef') {
					g.typedef_c_types[stmt.name] = true
				}
			}
		}
	}
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
			if !type_has_valid_data(typ) || typ is types.Void {
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
	return name in ['T', 'K', 'V'] || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`)
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
	if !g.has_emitted_ierror_body() {
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
		g.option_aliases[name] = true
		if val_type.starts_with('Array_') {
			g.array_aliases[val_type] = true
		} else if val_type.starts_with('Map_') {
			g.map_aliases[val_type] = true
		}
	}
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
							if c != '' && c != 'int' {
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

fn (mut g Gen) find_struct_decl_info_by_c_name(c_name string) ?StructDeclInfo {
	if c_name == '' {
		return none
	}
	saved_module := g.cur_module
	defer {
		g.cur_module = saved_module
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.language != .v {
					continue
				}
				struct_name := g.get_struct_name(stmt)
				if struct_name == c_name || short_type_name(struct_name) == c_name {
					return StructDeclInfo{
						decl: stmt
						mod:  g.cur_module
					}
				}
			}
		}
	}
	return none
}

fn (mut g Gen) lookup_embedded_field_info_in_decl(info StructDeclInfo, field_name string) ?EmbeddedFieldLookupInfo {
	saved_module := g.cur_module
	defer {
		g.cur_module = saved_module
	}
	g.cur_module = info.mod
	for emb in info.decl.embedded {
		embedded_c_type := g.expr_type_to_c(emb)
		owner := embedded_owner_field_name(embedded_c_type)
		if embedded_info := g.find_struct_decl_info_by_c_name(embedded_c_type) {
			g.cur_module = embedded_info.mod
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
			g.cur_module = info.mod
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
		types.NamedType {
			name := string(t)
			if name in ['int', 'i64', 'i32', 'i16', 'i8', 'u64', 'u32', 'u16', 'u8', 'byte', 'rune',
				'f32', 'f64', 'usize', 'isize', 'bool', 'string'] {
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
			unmangled_name := unmangle_c_ptr_type(name)
			if unmangled_name != name {
				return unmangled_name
			}
			if name.ends_with('Vec4') {
				mut mapped := name[..name.len - 'Vec4'.len] + 'SimdFloat4'
				if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
					&& g.cur_module != 'builtin' {
					mapped = '${g.cur_module}__${mapped}'
				}
				return mapped
			}
			if name.ends_with('Vec2') {
				mut mapped := name[..name.len - 'Vec2'.len] + 'SimdFloat2'
				if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
					&& g.cur_module != 'builtin' {
					mapped = '${g.cur_module}__${mapped}'
				}
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
			if g.is_module_local_type(name) {
				return '${g.cur_module}__${name}'
			}
			return name
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
fn (mut g Gen) get_expr_type(node ast.Expr) string {
	if !expr_has_valid_data(node) {
		return ''
	}
	// For identifiers, check local/parameter types first (authoritative),
	// then fall back to env position lookup.
	if node is ast.Ident {
		if local_type := g.get_local_var_c_type(node.name) {
			return local_type
		}
		if node.name == 'err' {
			return 'IError'
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
		// Module-qualified ident: transformer mangles `module.name` → `module__name`.
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
	// For IndexExpr on pointer-to-pointer or pointer-to-string types, prefer raw-type-based
	// inference over env (env may store the wrong type, e.g. char instead of char*).
	if node is ast.IndexExpr {
		// Cross-check: if LHS has a known local C type ending in **, derive element type
		if node.lhs is ast.Ident {
			if local_type := g.get_local_var_c_type(node.lhs.name) {
				if local_type.ends_with('**') {
					return local_type[..local_type.len - 1]
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
		// For arithmetic InfixExpr, env may return void* when it should be a
		// numeric type.  Fall through to operand-based inference.
		if t in ['void*', 'voidptr'] && node is ast.InfixExpr
			&& node.op in [.plus, .minus, .mul, .div, .mod] {
			// fall through to InfixExpr handler below
		} else if t == 'bool' && node is ast.BasicLiteral && node.kind == .number {
			// Numeric literals mistyped as bool by env (e.g. `1 in map` context).
			return 'int'
		} else if node is ast.IndexExpr && node.expr !is ast.RangeExpr && t.starts_with('Array_')
			&& !t.starts_with('Array_fixed_') {
			// Env may return the container type instead of the element type for IndexExpr.
			// Cross-check against raw type of the LHS container: if the LHS is an Array
			// whose element type is known, prefer that.
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
		} else if node is ast.IndexExpr && node.expr !is ast.RangeExpr
			&& t.starts_with('Array_fixed_') {
			// Env returned the container type for an IndexExpr on a fixed array.
			// Extract the element type: Array_fixed_u32_8 => u32, Array_fixed_u32_8* => u32
			mut fixed_t := if t.ends_with('*') { t[..t.len - 1] } else { t }
			rest := fixed_t['Array_fixed_'.len..]
			last_underscore := rest.last_index('_') or { -1 }
			if last_underscore > 0 {
				return rest[..last_underscore]
			}
			return t
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
		} else if node is ast.SelectorExpr
			&& (t.starts_with('_result_') || t.starts_with('_option_')) {
			// Selector env metadata can incorrectly attach wrapper types to ordinary
			// fields like `s.str` / `s.is_lit`. Re-resolve the field against the
			// struct instead of trusting the env entry.
			field_type := g.selector_field_type(node)
			if field_type != '' && field_type != t {
				return field_type
			}
		} else if node is ast.InfixExpr && (t.starts_with('_result_') || t.starts_with('_option_')) {
			// Pointer arithmetic can inherit bogus wrapper types from env metadata
			// even when the operands are plain pointer + integer expressions.
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
		} else if t == 'int' && node is ast.SelectorExpr {
			// Selector env types can degrade to `int` for nested field chains
			// (e.g. a.x.x). Prefer field-resolution fallback when available.
			field_type := g.selector_field_type(node)
			if field_type != '' && field_type != 'int' {
				return field_type
			}
		} else if t == 'int' && node is ast.InfixExpr
			&& node.op in [.plus, .minus, .mul, .div, .mod] {
			// Arithmetic env types can also degrade to `int` when selector
			// operands lose their field types. Re-infer numerics from operands.
			numeric_type := g.infer_numeric_expr_type(node)
			if numeric_type != '' && numeric_type !in ['int', 'int_literal'] {
				return numeric_type
			}
		} else if node is ast.SelectorExpr && g.active_generic_types.len > 0 {
			// In specialized generic functions, the env may return the
			// substituted generic type param (e.g. Slack) instead of the actual
			// struct field type (e.g. ValueInfo). Cross-check against struct fields.
			field_type := g.selector_field_type(node)
			if field_type != '' && field_type != t {
				return field_type
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
			return g.get_if_expr_type(&node)
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

// expr_type_to_c converts an AST type expression to a C type string
fn (mut g Gen) vec_generic_type_to_c(lhs_name string, arg_type string) string {
	base_name := if lhs_name.contains('__') { lhs_name.all_after_last('__') } else { lhs_name }
	prefix := if lhs_name.contains('__') {
		lhs_name.all_before_last('__') + '__'
	} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		g.cur_module + '__'
	} else {
		''
	}
	if base_name == 'Vec4' {
		return if arg_type.starts_with('u') {
			prefix + 'SimdU32_4'
		} else if arg_type.starts_with('f') {
			prefix + 'SimdFloat4'
		} else {
			prefix + 'SimdInt4'
		}
	}
	if base_name == 'Vec2' {
		return if arg_type.starts_with('u') {
			prefix + 'SimdUint2'
		} else if arg_type.starts_with('f') {
			prefix + 'SimdFloat2'
		} else {
			prefix + 'SimdI32_2'
		}
	}
	return ''
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
			if name == 'voidptr' {
				return 'void*'
			}
			if name == 'charptr' {
				return 'char*'
			}
			if name == 'byteptr' {
				return 'u8*'
			}
			// Detect mangled pointer aliases from transformer/checker
			// (e.g. FILEptr -> FILE*, viper__Appptr -> viper__App*).
			unmangled_name := unmangle_c_ptr_type(name)
			if unmangled_name != name {
				return unmangled_name
			}
			if name.ends_with('Vec4') {
				mut mapped := name[..name.len - 'Vec4'.len] + 'SimdFloat4'
				if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
					&& g.cur_module != 'builtin' {
					mapped = '${g.cur_module}__${mapped}'
				}
				g.register_alias_type(mapped)
				return mapped
			}
			if name.ends_with('Vec2') {
				mut mapped := name[..name.len - 'Vec2'.len] + 'SimdFloat2'
				if !mapped.contains('__') && g.cur_module != '' && g.cur_module != 'main'
					&& g.cur_module != 'builtin' {
					mapped = '${g.cur_module}__${mapped}'
				}
				g.register_alias_type(mapped)
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
					// @[typedef] C structs are typedefs, not raw structs
					if name in g.typedef_c_types {
						return name
					}
					return 'struct ' + name
				}
				mod_name := g.resolve_module_name(e.lhs.name)
				mut qualified := mod_name + '__' + e.rhs.name
				if qualified.ends_with('Vec4') {
					qualified = qualified[..qualified.len - 'Vec4'.len] + 'SimdFloat4'
				} else if qualified.ends_with('Vec2') {
					qualified = qualified[..qualified.len - 'Vec2'.len] + 'SimdFloat2'
				}
				return qualified
			}
			return g.expr_type_to_c(e.lhs) + '__' + e.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			lhs_name := g.expr_type_to_c(e.lhs)
			arg_type := g.expr_type_to_c(e.expr)
			mapped := g.vec_generic_type_to_c(lhs_name, arg_type)
			if mapped != '' {
				g.register_alias_type(mapped)
				return mapped
			}
			// Record binding and resolve multi-instantiation
			arg_name := e.expr.name()
			if !is_generic_placeholder_type_name(arg_name) {
				struct_base := if lhs_name.contains('__') {
					lhs_name.all_after_last('__')
				} else {
					lhs_name
				}
				g.record_generic_struct_bindings(struct_base, lhs_name, [e.expr])
			}
			return g.resolve_generic_struct_c_name(lhs_name, [e.expr])
		}
		ast.GenericArgs {
			lhs_name := g.expr_type_to_c(e.lhs)
			if e.args.len > 0 {
				arg_type := g.expr_type_to_c(e.args[0])
				mapped := g.vec_generic_type_to_c(lhs_name, arg_type)
				if mapped != '' {
					g.register_alias_type(mapped)
					return mapped
				}
				// Record binding for multi-instantiation (e.g. LinkedList[StructFieldInfo])
				mut all_concrete := true
				for a in e.args {
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
					g.record_generic_struct_bindings(struct_base, lhs_name, e.args)
				}
				return g.resolve_generic_struct_c_name(lhs_name, e.args)
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
			if e is ast.GenericType {
				// Generic struct type like LinkedList[ValueInfo] or Node[T].
				base_name := g.expr_type_to_c(e.name)
				if e.params.len > 0 {
					struct_base := if base_name.contains('__') {
						base_name.all_after_last('__')
					} else {
						base_name
					}
					// Resolve placeholder params (e.g., T → StructFieldInfo) via active_generic_types
					mut resolved_params := e.params.clone()
					if g.active_generic_types.len > 0 {
						for i, p in e.params {
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

// is_c_type_name checks if a name refers to a C type (struct, typedef) vs a C function.
fn (g &Gen) is_c_type_name(name string) bool {
	if name in g.typedef_c_types {
		return true
	}
	return name in ['FILE', 'DIR', 'va_list', 'pthread_t', 'pthread_mutex_t', 'pthread_cond_t',
		'pthread_rwlock_t', 'pthread_attr_t', 'atomic_uintptr_t', 'stat', 'tm', 'timespec', 'timeval',
		'dirent', 'termios', 'sockaddr', 'sockaddr_in', 'sockaddr_in6', 'sockaddr_un', 'fd_set',
		'mach_timebase_info_data_t', 'FONScontext', 'FONSparams']
}

// record_generic_struct_bindings records the concrete type bindings for a
// generic struct instantiation (e.g. LinkedList[ValueInfo] → {T: ValueInfo}).
// These bindings are used when emitting methods on the generic struct.
fn (mut g Gen) record_generic_struct_bindings(struct_base_name string, struct_c_name string, concrete_params []ast.Expr) {
	// Find the struct decl to get generic param names.
	env_struct := g.lookup_struct_type(struct_base_name)
	generic_param_names := env_struct.generic_params
	if generic_param_names.len == 0 || generic_param_names.len != concrete_params.len {
		return
	}
	// Check that all concrete params are non-placeholder types.
	mut bindings := map[string]types.Type{}
	mut param_c_names := []string{cap: concrete_params.len}
	for i, param_name in generic_param_names {
		concrete_expr := concrete_params[i]
		if is_generic_placeholder_type_name(concrete_expr.name()) {
			return
		}
		concrete_c_name := g.expr_type_to_c(concrete_expr)
		param_c_names << concrete_c_name
		if concrete_type := g.lookup_type_by_c_name(concrete_c_name) {
			bindings[param_name] = concrete_type
		}
	}
	if bindings.len != generic_param_names.len {
		return
	}
	params_key := param_c_names.join('_')

	// Record in multi-instantiation map
	mut instances := g.generic_struct_instances[struct_c_name]
	mut already_exists := false
	for inst in instances {
		if inst.params_key == params_key {
			already_exists = true
			break
		}
	}
	if !already_exists {
		inst_c_name := if instances.len == 0 {
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
	env_struct := g.lookup_struct_type(struct_base_name)
	generic_param_names := env_struct.generic_params
	if generic_param_names.len == 0 || generic_param_names.len != concrete_params.len {
		return
	}
	mut bindings := map[string]types.Type{}
	mut param_c_names := []string{cap: concrete_params.len}
	for i, param_name in generic_param_names {
		concrete_expr := concrete_params[i]
		expr_name := concrete_expr.name()
		if is_generic_placeholder_type_name(expr_name) {
			if parent_type := parent_bindings[expr_name] {
				bindings[param_name] = parent_type
				param_c_names << g.types_type_to_c(parent_type)
				continue
			}
			return
		}
		concrete_c_name := g.expr_type_to_c(concrete_expr)
		param_c_names << concrete_c_name
		if concrete_type := g.lookup_type_by_c_name(concrete_c_name) {
			bindings[param_name] = concrete_type
		}
	}
	if bindings.len != generic_param_names.len {
		return
	}
	params_key := param_c_names.join('_')

	mut instances := g.generic_struct_instances[struct_c_name]
	mut already_exists := false
	for inst in instances {
		if inst.params_key == params_key {
			already_exists = true
			break
		}
	}
	if !already_exists {
		inst_c_name := if instances.len == 0 {
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

// resolve_generic_struct_c_name returns the correct C struct name for a generic
// struct instantiation with specific type parameters. If the instantiation matches
// a non-primary instance, returns the suffixed name.
fn (mut g Gen) resolve_generic_struct_c_name(base_name string, concrete_params []ast.Expr) string {
	instances := g.generic_struct_instances[base_name]
	if instances.len <= 1 {
		return base_name
	}
	mut param_c_names := []string{cap: concrete_params.len}
	for p in concrete_params {
		param_c_names << g.expr_type_to_c(p)
	}
	params_key := param_c_names.join('_')
	for inst in instances {
		if inst.params_key == params_key {
			// If this is a non-primary instance and hasn't been emitted, emit it late
			if inst.c_name != base_name {
				body_key := 'body_${inst.c_name}'
				if body_key !in g.emitted_types {
					g.emit_late_generic_struct(base_name, inst)
				}
			}
			return inst.c_name
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
	def.writeln('${keyword} ${inst.c_name} {')
	for field in struct_node.fields {
		field_type := g.expr_type_to_c(field.typ)
		field_name := if field.name.len > 0 { field.name } else { 'value' }
		def.writeln('\t${field_type} ${field_name};')
		g.struct_field_types['${inst.c_name}.${field_name}'] = field_type
	}
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

// get_comptime_selector_type returns the C type for a comptime field selector expression.
fn (mut g Gen) get_comptime_selector_type(node ast.SelectorExpr) string {
	rhs_name := node.rhs.name
	// val.$(field.name) → type of that field
	if rhs_name == '__comptime_selector__' || rhs_name == 'TODO: comptime selector' {
		return g.comptime_field_type
	}
	// field.name → string
	if node.lhs is ast.Ident && node.lhs.name == g.comptime_field_var {
		match rhs_name {
			'name' { return 'string' }
			'typ' { return 'string' }
			'attrs' { return 'Array_string' }
			'is_mut', 'is_shared', 'is_array', 'is_map', 'is_option' { return 'bool' }
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
				if obj := scope.objects[type_name] {
					return obj.typ()
				}
			}
		}
	}
	// Try current module scope
	mod_name := if g.cur_module != '' { g.cur_module } else { 'main' }
	if scope := g.env_scope(mod_name) {
		short_name := if c_name.contains('__') { c_name.all_after_last('__') } else { c_name }
		if obj := scope.objects[short_name] {
			return obj.typ()
		}
	}
	return none
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
			if resolved := g.resolve_c_type_to_raw(local_type) {
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
	for file in g.files {
		if file.mod !in modules_to_try {
			modules_to_try << file.mod
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

fn (mut g Gen) lookup_struct_field_type_by_name(struct_name string, field_name string) ?string {
	if struct_name == '' || field_name == '' {
		return none
	}
	mut candidates := []string{}
	candidates << struct_name
	base_name := strip_pointer_type_name(struct_name)
	if base_name != '' && base_name !in candidates {
		candidates << base_name
	}
	for candidate in candidates {
		full_key := '${candidate}.${field_name}'
		if field_type := g.struct_field_types[full_key] {
			if field_type != '' {
				return field_type
			}
		}
		short_name := short_type_name(candidate)
		if short_name != '' && short_name != candidate {
			short_key := '${short_name}.${field_name}'
			if field_type := g.struct_field_types[short_key] {
				if field_type != '' {
					return field_type
				}
			}
		}
		if info := g.lookup_embedded_field_info(candidate, field_name) {
			if info.field_type != '' {
				return info.field_type
			}
		}
	}
	return none
}

fn (mut g Gen) selector_field_type(sel ast.SelectorExpr) string {
	// Fast path: use the type checker's env pos.id lookup (O(1) array access).
	// Uses get_env_c_type (alias-preserving) since alias types like
	// strings__Builder, ssa__TypeID are correct for struct field types.
	// Skip _data and _result_/_option_ internal fields (.data, .err, .is_error, .state):
	// the checker stores incorrect types for these on wrapper structs.
	rhs := sel.rhs.name
	mut env_type := ''
	if !rhs.starts_with('_') && rhs !in ['data', 'err', 'is_error', 'state'] {
		if t := g.get_env_c_type(sel) {
			// Nested selector chains can be reduced to `int` in env metadata.
			// Keep resolving through raw/field information in that case.
			if t != 'int' && !t.starts_with('_result_') && !t.starts_with('_option_') {
				// In specialized generic functions, the env may return the substituted
				// generic type param (e.g. Slack for T) instead of the actual struct
				// field type (e.g. ValueInfo). Cross-check against struct_field_types.
				if g.active_generic_types.len > 0 {
					lhs_struct := g.resolve_generic_struct_field_name(sel.lhs)
					if lhs_struct != '' {
						if field_t := g.lookup_struct_field_type_by_name(lhs_struct, rhs) {
							if field_t != t {
								return field_t
							}
						}
					}
				}
				return t
			}
			env_type = t
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
		if resolved != ''
			&& ((!resolved.starts_with('_result_') && !resolved.starts_with('_option_'))
			|| rhs in ['data', 'err', 'is_error', 'state']) {
			return resolved
		}
	}
	lhs_struct_name := g.selector_struct_name(sel.lhs)
	if lhs_struct_name != '' {
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
	lane_type := g.selector_vector_lane_type(sel)
	if lane_type != '' {
		return lane_type
	}
	if env_type.starts_with('_result_') || env_type.starts_with('_option_') {
		return ''
	}
	return env_type
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
			if env_struct.generic_params.len > 0 {
				mut param_c_names := []string{cap: env_struct.generic_params.len}
				for param_name in env_struct.generic_params {
					if concrete := g.active_generic_types[param_name] {
						param_c_names << g.types_type_to_c(concrete)
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
