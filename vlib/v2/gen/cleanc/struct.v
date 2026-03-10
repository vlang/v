// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

fn (mut g Gen) struct_is_leaf(node ast.StructDecl) bool {
	if node.embedded.len > 0 {
		return false
	}
	env_struct := g.lookup_struct_type(node.name)
	if env_struct.fields.len == node.fields.len {
		for field in env_struct.fields {
			if g.struct_leaf_field_type(field.typ) {
				continue
			}
			return false
		}
		return true
	}
	for field in node.fields {
		if g.is_pointer_type(field.typ) {
			continue
		}
		match field.typ {
			ast.Ident {
				if field.typ.name in primitive_types {
					continue
				}
				return false
			}
			ast.Type {
				if field.typ is ast.ArrayFixedType {
					if field.typ.elem_type is ast.Ident
						&& (field.typ.elem_type as ast.Ident).name in primitive_types {
						continue
					}
				}
				return false
			}
			else {
				return false
			}
		}
	}
	return true
}

fn (mut g Gen) struct_leaf_field_type(t types.Type) bool {
	if !type_has_valid_data(t) {
		return false
	}
	if g.struct_leaf_pointer_type(t) {
		return true
	}
	match t {
		types.Primitive, types.Char, types.Rune, types.ISize, types.USize, types.Enum {
			return true
		}
		types.ArrayFixed {
			return g.struct_leaf_field_type(t.elem_type)
		}
		types.Alias {
			type_name := g.types_type_to_c(t)
			if type_name in primitive_types || type_name == 'bool' {
				return true
			}
			return g.struct_leaf_field_type(t.base_type)
		}
		else {
			return false
		}
	}
}

fn (g &Gen) struct_leaf_pointer_type(t types.Type) bool {
	if !type_has_valid_data(t) {
		return false
	}
	match t {
		types.Pointer {
			return true
		}
		types.Alias {
			return g.struct_leaf_pointer_type(t.base_type)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) emit_ready_option_result_structs() bool {
	mut emitted_any := false
	mut option_names := g.option_aliases.keys()
	option_names.sort()
	for name in option_names {
		if name in g.emitted_option_structs {
			continue
		}
		val_type := option_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		g.sb.writeln('struct ${name} { u8 state; IError err; u8 data[sizeof(${val_type}) > 1 ? sizeof(${val_type}) : 1]; };')
		g.emitted_option_structs[name] = true
		emitted_any = true
	}
	mut result_names := g.result_aliases.keys()
	result_names.sort()
	for name in result_names {
		if name in g.emitted_result_structs {
			continue
		}
		val_type := g.result_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		g.sb.writeln('struct ${name} { bool is_error; IError err; u8 data[sizeof(${val_type}) > 1 ? sizeof(${val_type}) : 1]; };')
		g.emitted_result_structs[name] = true
		emitted_any = true
	}
	return emitted_any
}

fn (mut g Gen) emit_option_result_structs() {
	for g.emit_ready_option_result_structs() {}
}

fn (mut g Gen) struct_fields_resolved(node ast.StructDecl) bool {
	// Check embedded types (used by value, not pointer)
	for emb in node.embedded {
		emb_name := g.field_type_name(emb)
		if emb_name != '' && emb_name !in primitive_types {
			emb_body_key := 'body_${emb_name}'
			emb_enum_key := 'enum_${emb_name}'
			emb_alias_key := 'alias_${emb_name}'
			if emb_body_key !in g.emitted_types && emb_enum_key !in g.emitted_types
				&& emb_alias_key !in g.emitted_types {
				return false
			}
		}
	}
	for field in node.fields {
		typ_name := g.field_type_name(field.typ)
		if typ_name == '' {
			continue
		}
		if field.typ is ast.Type {
			if field.typ is ast.OptionType {
				opt_typ := field.typ as ast.OptionType
				base_name := g.field_type_name(opt_typ.base_type)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_option_' + mangle_alias_component(base_name)
					if wrapper_name !in g.emitted_option_structs {
						return false
					}
				}
			} else if field.typ is ast.ResultType {
				res_typ := field.typ as ast.ResultType
				base_name := g.field_type_name(res_typ.base_type)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_result_' + mangle_alias_component(base_name)
					if wrapper_name !in g.emitted_result_structs {
						return false
					}
				}
			}
		}
		if typ_name.starts_with('_option_') {
			if typ_name !in g.emitted_option_structs {
				return false
			}
			continue
		}
		if typ_name.starts_with('_result_') {
			if typ_name !in g.emitted_result_structs {
				return false
			}
			continue
		}
		// Pointer types are fine with forward declarations
		if g.is_pointer_type(field.typ) {
			continue
		}
		// Primitive types are always resolved
		if typ_name in primitive_types {
			continue
		}
		if typ_name == 'string' || typ_name == 'builtin__string' {
			string_body_key := 'body_string'
			builtin_string_body_key := 'body_builtin__string'
			if string_body_key !in g.emitted_types && builtin_string_body_key !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name.starts_with('Array_fixed_') {
			typ_body_key := 'body_${typ_name}'
			typ_alias_key := 'alias_${typ_name}'
			if typ_body_key !in g.emitted_types && typ_alias_key !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name == 'array' || typ_name.starts_with('Array_') || typ_name in g.array_aliases {
			if 'body_array' !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name == 'map' || typ_name.starts_with('Map_') || typ_name in g.map_aliases {
			if 'body_map' !in g.emitted_types {
				return false
			}
			continue
		}
		// Check if this type's body has been emitted
		typ_body_key := 'body_${typ_name}'
		typ_enum_key := 'enum_${typ_name}'
		typ_alias_key := 'alias_${typ_name}'
		if typ_body_key !in g.emitted_types && typ_enum_key !in g.emitted_types
			&& typ_alias_key !in g.emitted_types {
			return false
		}
	}
	return true
}

fn (mut g Gen) get_struct_name(node ast.StructDecl) string {
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (mut g Gen) gen_struct_decl(node ast.StructDecl) {
	// Skip C extern struct declarations
	if node.language == .c {
		return
	}
	// Skip generic struct declarations (unresolved type parameters)
	if node.generic_params.len > 0 {
		return
	}

	name := g.get_struct_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true
	keyword := if node.is_union { 'union' } else { 'struct' }
	// Try to get the resolved struct type from the Environment
	env_struct := g.lookup_struct_type(node.name)

	// Use named struct to match the forward declaration: typedef struct name name;
	g.sb.writeln('${keyword} ${name} {')
	// Embedded structs as fields
	for i, emb in node.embedded {
		emb_type := g.expr_type_to_c(emb)
		g.sb.writeln('\t${emb_type} ${emb_type};')
		if i < env_struct.embedded.len {
			embedded := env_struct.embedded[i]
			for ef in embedded.fields {
				key := name + '.' + ef.name
				g.embedded_field_owner[key] = emb_type
				embedded_field_type := g.types_type_to_c(ef.typ)
				g.struct_field_types[key] = embedded_field_type
				if name.contains('__') {
					short_key := name.all_after_last('__') + '.' + ef.name
					g.embedded_field_owner[short_key] = emb_type
					g.struct_field_types[short_key] = embedded_field_type
				}
			}
		}
	}
	// Regular fields
	mut has_shared_fields := false
	for field in node.fields {
		field_name := escape_c_keyword(field.name)
		field_lookup_type := g.expr_type_to_c(field.typ)
		field_key := '${name}.${field.name}'
		g.struct_field_types[field_key] = field_lookup_type
		if name.contains('__') {
			short_field_key := '${name.all_after_last('__')}.${field.name}'
			g.struct_field_types[short_field_key] = field_lookup_type
		}
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			// Use the resolved array size from the Environment if available
			mut resolved_len := -1
			for ef in env_struct.fields {
				if ef.name == field.name {
					if ef.typ is types.ArrayFixed {
						resolved_len = ef.typ.len
					}
					break
				}
			}
			g.sb.write_string('\t${elem_type} ${field_name}[')
			if resolved_len > 0 {
				g.sb.write_string('${resolved_len}')
			} else {
				g.expr(fixed_typ.len)
			}
			g.sb.writeln('];')
			continue
		}
		// Check for shared modifier
		if field.typ is ast.ModifierExpr && field.typ.kind == .key_shared {
			has_shared_fields = true
		}
		field_type := field_lookup_type
		g.sb.writeln('\t${field_type} ${field_name};')
	}
	// Add mutex field for shared fields
	if has_shared_fields {
		g.sb.writeln('\tsync__RwMutex mtx;')
	}
	if node.embedded.len == 0 && node.fields.len == 0 {
		g.sb.writeln('\tu8 _dummy;')
	}
	g.sb.writeln('};')
	// Emit fallback str macros for structs without explicit str() methods
	struct_str_fn := '${name}__str'
	if struct_str_fn !in g.fn_return_types {
		label := '${name}{}'
		g.sb.writeln('#define ${name}__str(v) ((string){.str = "${label}", .len = ${label.len}, .is_lit = 1})')
	}
	struct_short_str_fn := '${name}_str'
	if struct_short_str_fn !in g.fn_return_types {
		g.sb.writeln('#define ${name}_str(v) ${name}__str(v)')
	}
	g.sb.writeln('')
}

fn (mut g Gen) gen_sum_type_decl(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

	// Track variant names for sum type cast generation
	mut variant_names := []string{}
	for i, variant in node.variants {
		vname := g.get_variant_field_name(variant, i)
		// Strip leading underscore from field name to get the type name
		variant_names << if vname.len > 1 && vname[0] == `_` { vname[1..] } else { vname }
	}
	g.sum_type_variants[name] = variant_names

	g.sb.writeln('struct ${name} {')
	g.sb.writeln('\tint _tag;')
	g.sb.writeln('\tunion {')
	for i, variant in node.variants {
		variant_name := g.get_variant_field_name(variant, i)
		g.sb.writeln('\t\tvoid* ${variant_name};')
	}
	g.sb.writeln('\t} _data;')
	g.sb.writeln('};')
	g.sb.writeln('')
}

fn (mut g Gen) get_variant_field_name(variant ast.Expr, idx int) string {
	if variant is ast.Ident {
		return '_${variant.name}'
	} else if variant is ast.SelectorExpr {
		if variant.lhs is ast.Ident {
			return '_${variant.lhs.name}__${variant.rhs.name}'
		}
		return '_${variant.rhs.name}'
	} else if variant is ast.Type {
		if variant is ast.ArrayType {
			elem := mangle_alias_component(g.field_type_name(variant.elem_type))
			return '_Array_${elem}'
		}
		if variant is ast.MapType {
			key := mangle_alias_component(g.field_type_name(variant.key_type))
			val := mangle_alias_component(g.field_type_name(variant.value_type))
			return '_Map_${key}_${val}'
		}
	}
	return '_v${idx}'
}

fn (mut g Gen) infer_sum_variant_from_expr(type_name string, variants []string, expr ast.Expr) SumVariantMatch {
	// Handle module constants used as sum variants in selfhosted checker code
	// (e.g. char_, string_, void_, nil_, none_).
	if expr is ast.Ident {
		variant_hint := match expr.name {
			'char_' {
				'Char'
			}
			'string_' {
				'String'
			}
			'void_' {
				'Void'
			}
			'nil_' {
				'Nil'
			}
			'none_' {
				'None'
			}
			'rune_' {
				'Rune'
			}
			'usize_' {
				'USize'
			}
			'isize_' {
				'ISize'
			}
			'thread_' {
				'Thread'
			}
			'chan_' {
				'Channel'
			}
			'bool_', 'i8_', 'i16_', 'i32_', 'int_', 'i64_', 'u8_', 'u16_', 'u32_', 'u64_', 'f32_',
			'f64_', 'int_literal_', 'float_literal_' {
				'Primitive'
			}
			else {
				''
			}
		}
		if variant_hint != '' {
			for i, v in variants {
				v_short := if v.contains('__') { v.all_after_last('__') } else { v }
				if v_short == variant_hint || v == variant_hint || v.ends_with('__${variant_hint}') {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: variant_hint == 'Primitive'
						inner_type:   ''
					}
				}
			}
		}
	}

	// For InitExpr, infer from the struct type name
	if expr is ast.InitExpr {
		init_type := g.expr_type_to_c(expr.typ)
		mut resolved_init_type := init_type
		if (resolved_init_type == '' || resolved_init_type == 'int') && expr.typ is ast.Ident {
			resolved_init_type = expr.typ.name.replace('.', '__')
		}
		if resolved_init_type != '' {
			init_short := if resolved_init_type.contains('__') {
				resolved_init_type.all_after_last('__')
			} else {
				resolved_init_type
			}
			for i, v in variants {
				if v == init_short || v == resolved_init_type
					|| resolved_init_type.ends_with('__${v}')
					|| v.ends_with('__${resolved_init_type}') {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: false
						inner_type:   resolved_init_type
					}
				}
			}
		}
	}
	// Generic fallback: if the expression is already a concrete AST sum variant
	// (e.g. ast.BasicLiteral being cast to ast.Expr), map it by runtime variant name.
	expr_variant := expr.type_name()
	if expr_variant != '' {
		expr_variant_c := expr_variant.replace('.', '__')
		expr_variant_short := if expr_variant.contains('.') {
			expr_variant.all_after_last('.')
		} else if expr_variant.contains('__') {
			expr_variant.all_after_last('__')
		} else {
			expr_variant
		}
		for i, v in variants {
			v_short := if v.contains('__') { v.all_after_last('__') } else { v }
			if v_short == expr_variant_short || v == expr_variant || v == expr_variant_c {
				inner_type := if g.is_scalar_sum_payload_type(v)
					|| v in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
					v
				} else if type_name.contains('__') {
					'${type_name.all_before_last('__')}__${v}'
				} else {
					v
				}
				return SumVariantMatch{
					tag:          i
					field_name:   v
					is_primitive: g.is_scalar_sum_payload_type(v)
					inner_type:   inner_type
				}
			}
		}
	}
	return SumVariantMatch{
		tag: -1
	}
}

fn (g &Gen) is_scalar_sum_payload_type(type_name string) bool {
	return type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'bool', 'rune', 'byte', 'usize', 'isize']
}

fn (mut g Gen) gen_sum_type_wrap(type_name string, field_name string, tag int, is_primitive bool, expr ast.Expr, inner_type string) {
	_ = is_primitive
	g.sb.write_string('((${type_name}){._tag = ${tag}, ._data._${field_name} = ')
	mut resolved_type := inner_type
	if resolved_type == '' || resolved_type == 'void*' || resolved_type == 'int'
		|| resolved_type == type_name {
		if g.is_scalar_sum_payload_type(field_name)
			|| field_name in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
			resolved_type = field_name
		} else if type_name.contains('__') {
			resolved_type = '${type_name.all_before_last('__')}__${field_name}'
		} else {
			resolved_type = field_name
		}
	}
	if g.is_scalar_sum_payload_type(resolved_type) {
		// Keep scalar payloads encoded in pointer-size space. Smartcast extraction expects this.
		g.sb.write_string('((void*)((intptr_t)(')
		g.expr(expr)
		g.sb.write_string(')))')
	} else if inner_type == 'void*' {
		// `&fn_call()` lowers to a statement-expression pointer to a temporary.
		// Copy by-value from the inner expression to avoid dangling addresses.
		if inner_expr := g.unwrap_addr_of_value_expr(expr) {
			g.tmp_counter++
			tmp_name := '_st${g.tmp_counter}'
			g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
			g.expr(inner_expr)
			g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
		} else {
			g.sb.write_string('((void*)memdup(')
			g.expr(expr)
			g.sb.write_string(', sizeof(${resolved_type})))')
		}
	} else {
		// Must heap-allocate (memdup) non-primitive sum type variants to avoid
		// dangling pointers to local variables that go out of scope.
		g.tmp_counter++
		tmp_name := '_st${g.tmp_counter}'
		g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
		g.expr(expr)
		g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
	}
	g.sb.write_string('})')
}

fn (g &Gen) get_sum_type_variants_for(type_name string) []string {
	sum_type := type_name.trim_right('*')
	if sum_type == '' {
		return []string{}
	}
	if vs := g.sum_type_variants[sum_type] {
		return vs
	}
	if sum_type.contains('__') {
		short_sum := sum_type.all_after_last('__')
		if vs := g.sum_type_variants[short_sum] {
			return vs
		}
	} else {
		qualified_sum := g.get_qualified_name(sum_type)
		if vs := g.sum_type_variants[qualified_sum] {
			return vs
		}
	}
	return []string{}
}

fn (g &Gen) is_sum_payload_expr(node ast.Expr, variant string) bool {
	match node {
		ast.SelectorExpr {
			return node.rhs.name == variant || node.rhs.name == '_${variant}'
		}
		ast.CastExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ParenExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.PrefixExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ModifierExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		else {}
	}
	return false
}

fn (mut g Gen) gen_sum_variant_field_selector(node ast.SelectorExpr) bool {
	if node.rhs.name.starts_with('_') {
		return false
	}
	// If LHS is an as-cast, the cast already handles narrowing - skip double unwrap
	if node.lhs is ast.AsCastExpr {
		return false
	}
	mut lhs_sum_type := ''
	if raw_lhs := g.get_raw_type(node.lhs) {
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
	if lhs_sum_type == '' {
		lhs_sum_type = g.get_expr_type(node.lhs)
	}
	mut variants := []string{}
	if vs := g.sum_type_variants[lhs_sum_type] {
		variants = vs.clone()
	} else if lhs_sum_type.contains('__') {
		short_sum := lhs_sum_type.all_after_last('__')
		if vs := g.sum_type_variants[short_sum] {
			variants = vs.clone()
		}
	} else {
		qualified_sum := g.get_qualified_name(lhs_sum_type)
		if vs := g.sum_type_variants[qualified_sum] {
			variants = vs.clone()
			lhs_sum_type = qualified_sum
		}
	}
	if variants.len == 0 {
		return false
	}
	mut matched_full := ''
	mut matched_short := ''
	for variant in variants {
		variant_short := if variant.contains('__') { variant.all_after_last('__') } else { variant }
		mut variant_full := variant
		if !variant_full.contains('__') && lhs_sum_type.contains('__')
			&& !g.is_scalar_sum_payload_type(variant_short)
			&& variant_short !in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
			prefix := lhs_sum_type.all_before_last('__')
			if prefix != '' {
				variant_full = '${prefix}__${variant_short}'
			}
		}
		key_full := '${variant_full}.${node.rhs.name}'
		key_short := '${variant_short}.${node.rhs.name}'
		if key_full in g.struct_field_types || key_short in g.struct_field_types {
			if matched_full != '' && matched_full != variant_full {
				// Ambiguous field across multiple variants.
				return false
			}
			matched_full = variant_full
			matched_short = variant_short
		}
	}
	if matched_full == '' {
		return false
	}
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(matched_full, node.rhs.name)
	sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
	g.sb.write_string('(((${matched_full}*)(((')
	g.expr(node.lhs)
	g.sb.write_string(')${sep}_data._${matched_short})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

fn (g &Gen) embedded_owner_for(struct_name string, field_name string) string {
	if struct_name == '' {
		return ''
	}
	key := '${struct_name}.${field_name}'
	if owner := g.embedded_field_owner[key] {
		return owner
	}
	if struct_name.contains('__') {
		short_key := '${struct_name.all_after_last('__')}.${field_name}'
		if owner := g.embedded_field_owner[short_key] {
			return owner
		}
	}
	return ''
}

fn (g &Gen) is_fn_pointer_alias_type(type_name string) bool {
	if type_name == '' {
		return false
	}
	if g.env == unsafe { nil } {
		return type_name.ends_with('Fn')
	}
	mut candidates := []string{cap: 3}
	candidates << type_name
	if type_name.contains('__') {
		candidates << type_name.all_after_last('__')
	}
	for cand in candidates {
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(cand, 0) {
				typ := obj.typ()
				if typ is types.Alias {
					if typ.base_type is types.FnType {
						return true
					}
				}
			}
		}
		if mut scope := g.env_scope('builtin') {
			if obj := scope.lookup_parent(cand, 0) {
				typ := obj.typ()
				if typ is types.Alias {
					if typ.base_type is types.FnType {
						return true
					}
				}
			}
		}
		if cand.contains('__') {
			mod_name := cand.all_before('__')
			short_name := cand.all_after('__')
			if mut scope := g.env_scope(mod_name) {
				if obj := scope.lookup_parent(short_name, 0) {
					typ := obj.typ()
					if typ is types.Alias {
						if typ.base_type is types.FnType {
							return true
						}
					}
				}
			}
		}
	}
	return type_name.ends_with('Fn')
}

fn simd_vector_field_order(type_name string) []string {
	if type_name.ends_with('SimdFloat4') || type_name.ends_with('SimdInt4')
		|| type_name.ends_with('SimdU32_4') || type_name.ends_with('Vec4') {
		return ['x', 'y', 'z', 'w']
	}
	if type_name.ends_with('SimdFloat2') || type_name.ends_with('SimdUint2')
		|| type_name.ends_with('SimdI32_2') || type_name.ends_with('Vec2') {
		return ['x', 'y']
	}
	return []string{}
}

fn (mut g Gen) gen_simd_vector_init_expr(type_name string, fields []ast.FieldInit) bool {
	order := simd_vector_field_order(type_name)
	if order.len == 0 {
		return false
	}
	mut values := map[string]ast.Expr{}
	for field in fields {
		if field.name == '' {
			return false
		}
		values[field.name] = field.value
	}
	g.sb.write_string('((${type_name}){')
	for i, field_name in order {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if value := values[field_name] {
			g.expr(value)
		} else {
			g.sb.write_string('0')
		}
	}
	g.sb.write_string('})')
	return true
}

fn unwrap_alias_type(typ types.Type) types.Type {
	if !type_has_valid_data(typ) {
		return typ
	}
	mut cur := typ
	for {
		if !type_has_valid_data(cur) {
			break
		}
		match cur {
			types.Alias {
				alias_type := cur as types.Alias
				cur = alias_type.base_type
			}
			else {
				break
			}
		}
	}
	return cur
}

fn struct_field_needs_explicit_default(field types.Field) bool {
	field_type := unwrap_alias_type(field.typ)
	return field_type is types.Array || field_type is types.String
}

fn (mut g Gen) write_struct_field_default_value(field types.Field) bool {
	field_type := unwrap_alias_type(field.typ)
	if field_type is types.Array {
		array_type := field_type as types.Array
		elem_type := g.types_type_to_c(array_type.elem_type)
		g.sb.write_string('__new_array_with_default_noscan(0, 0, sizeof(${elem_type}), NULL)')
		return true
	}
	if field_type is types.String {
		g.sb.write_string(c_empty_v_string_expr())
		return true
	}
	return false
}

fn (mut g Gen) gen_none_literal_for_type(type_name string) bool {
	trimmed := type_name.trim_space()
	if trimmed == '' {
		return false
	}
	if trimmed.starts_with('_option_') {
		g.sb.write_string('(${trimmed}){ .state = 2 }')
		return true
	}
	if trimmed in ['IError', 'builtin__IError'] {
		g.sb.write_string('none__')
		return true
	}
	if is_type_name_pointer_like(trimmed) || trimmed in ['void*', 'voidptr', 'byteptr', 'charptr'] {
		g.sb.write_string('NULL')
		return true
	}
	return false
}

fn (mut g Gen) init_field_expected_type(type_name string, env_struct types.Struct, field_name string) string {
	expected_key := '${type_name}.${field_name}'
	mut expected_field_type := g.struct_field_types[expected_key] or { '' }
	if expected_field_type == '' && type_name.contains('__') {
		short_type := type_name.all_after_last('__')
		short_expected_key := '${short_type}.${field_name}'
		expected_field_type = g.struct_field_types[short_expected_key] or { '' }
	}
	if expected_field_type != '' {
		return expected_field_type
	}
	for field in env_struct.fields {
		if field.name == field_name {
			return g.types_type_to_c(field.typ)
		}
	}
	return ''
}

fn (mut g Gen) gen_init_expr(node ast.InitExpr) {
	type_name := g.expr_type_to_c(node.typ)
	mut env_struct := types.Struct{}
	mut has_struct_defaults := false
	if raw_type := g.get_raw_type(node.typ) {
		unwrapped := unwrap_alias_type(raw_type)
		if unwrapped is types.Struct {
			env_struct = unwrapped
			has_struct_defaults = true
		}
	}
	if !has_struct_defaults && !type_name.starts_with('Array_') && !type_name.starts_with('Map_') {
		resolved := g.lookup_struct_type_by_c_name(type_name)
		if resolved.fields.len > 0 {
			env_struct = resolved
			has_struct_defaults = true
		}
	}
	mut only_named_fields := true
	for field in node.fields {
		if field.name == '' {
			only_named_fields = false
			break
		}
	}
	if node.fields.len == 0 {
		if has_struct_defaults && env_struct.fields.len > 0 {
			mut wrote_defaults := 0
			g.sb.write_string('((${type_name}){')
			for field in env_struct.fields {
				if !struct_field_needs_explicit_default(field) {
					continue
				}
				if wrote_defaults > 0 {
					g.sb.write_string(',')
				}
				g.sb.write_string('.${escape_c_keyword(field.name)} = ')
				if !g.write_struct_field_default_value(field) {
					g.sb.write_string('0')
				}
				wrote_defaults++
			}
			if wrote_defaults == 0 {
				g.sb.write_string('0')
			}
			g.sb.write_string('})')
			return
		}
		g.sb.write_string('((${type_name}){0})')
		return
	}
	if g.gen_simd_vector_init_expr(type_name, node.fields) {
		return
	}
	mut initialized_fields := map[string]bool{}
	if only_named_fields {
		for field in node.fields {
			if field.name != '' {
				initialized_fields[field.name] = true
			}
		}
	}
	g.sb.write_string('((${type_name}){')
	mut wrote_fields := 0
	for field in node.fields {
		if wrote_fields > 0 {
			g.sb.write_string(',')
		}
		wrote_fields++
		if field.name == '' {
			g.expr(field.value)
			continue
		}
		if type_name in g.sum_type_variants && field.name.starts_with('_data._') {
			field_name := field.name
			g.sb.write_string('.${field_name} = ')
			variant_name := field_name.all_after('_data._')
			mut inner_type := g.get_expr_type(field.value)
			mut resolved_type := inner_type
			if inner_type == '' || inner_type == 'void*' || inner_type == 'int'
				|| inner_type == type_name {
				if g.is_scalar_sum_payload_type(variant_name)
					|| variant_name in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
					resolved_type = variant_name
				} else if variant_name.contains('__') {
					resolved_type = variant_name
				} else if type_name.contains('__') {
					resolved_type = '${type_name.all_before_last('__')}__${variant_name}'
				} else {
					resolved_type = variant_name
				}
			}
			if g.is_scalar_sum_payload_type(resolved_type) {
				// Keep scalar payloads encoded in pointer-size space.
				g.sb.write_string('((void*)((intptr_t)(')
				g.expr(field.value)
				g.sb.write_string(')))')
			} else if inner_type == 'void*' {
				// `&fn_call()` can point to a temporary; copy by-value from inner expr.
				if inner_expr := g.unwrap_addr_of_value_expr(field.value) {
					g.tmp_counter++
					tmp_name := '_st${g.tmp_counter}'
					g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
					g.expr(inner_expr)
					g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
				} else {
					g.sb.write_string('((void*)memdup(')
					g.expr(field.value)
					g.sb.write_string(', sizeof(${resolved_type})))')
				}
			} else {
				g.tmp_counter++
				tmp_name := '_st${g.tmp_counter}'
				g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
				g.expr(field.value)
				g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
			}
			continue
		}
		owner := g.embedded_owner_for(type_name, field.name)
		field_name := escape_c_keyword(field.name)
		if owner != '' {
			g.sb.write_string('.${escape_c_keyword(owner)}.${field_name} = ')
		} else {
			g.sb.write_string('.${field_name} = ')
		}
		expected_field_type := g.init_field_expected_type(type_name, env_struct, field.name)
		if is_none_like_expr(field.value) && g.gen_none_literal_for_type(expected_field_type) {
			continue
		}
		if g.is_fn_pointer_alias_type(expected_field_type) {
			if field.value is ast.SelectorExpr {
				sel := field.value as ast.SelectorExpr
				if g.gen_bound_method_value_expr(sel, expected_field_type) {
					continue
				}
				if method_value_name := g.selector_method_value_name(sel) {
					g.sb.write_string('((${expected_field_type})${method_value_name})')
					continue
				}
			}
			if field.value is ast.Ident {
				g.sb.write_string('((${expected_field_type})')
				g.expr(field.value)
				g.sb.write_string(')')
				continue
			}
		}
		// Disambiguate shorthand enum values in struct field initializers
		// using the field's declared enum type.
		if field.value is ast.SelectorExpr {
			sel := field.value as ast.SelectorExpr
			if sel.lhs is ast.EmptyExpr {
				expected_enum := expected_field_type
				if expected_enum != '' && g.is_enum_type(expected_enum) {
					g.sb.write_string('${g.normalize_enum_name(expected_enum)}__${sel.rhs.name}')
					continue
				}
			}
		}
		if g.should_deref_init_field_value(type_name, field.name, field.value) {
			g.sb.write_string('(*(')
			g.expr(field.value)
			g.sb.write_string('))')
			continue
		}
		g.expr(field.value)
	}
	if has_struct_defaults && only_named_fields && env_struct.fields.len > 0 {
		for field in env_struct.fields {
			if initialized_fields[field.name] {
				continue
			}
			if !struct_field_needs_explicit_default(field) {
				continue
			}
			if wrote_fields > 0 {
				g.sb.write_string(',')
			}
			g.sb.write_string('.${escape_c_keyword(field.name)} = ')
			if !g.write_struct_field_default_value(field) {
				g.sb.write_string('0')
			}
			wrote_fields++
		}
	}
	g.sb.write_string('})')
}

fn (mut g Gen) should_deref_init_field_value(struct_type string, field_name string, value ast.Expr) bool {
	expected_key := '${struct_type}.${field_name}'
	mut expected := g.struct_field_types[expected_key] or { '' }
	if expected == '' && struct_type.contains('__') {
		short_struct := struct_type.all_after_last('__')
		short_expected_key := '${short_struct}.${field_name}'
		expected = g.struct_field_types[short_expected_key] or { '' }
	}
	if expected == '' || is_type_name_pointer_like(expected) {
		return false
	}
	mut value_type := g.get_expr_type(value)
	if !is_type_name_pointer_like(value_type) {
		call_ret_type := g.expr_pointer_return_type(value)
		if call_ret_type != '' {
			value_type = call_ret_type
		}
	}
	if !is_type_name_pointer_like(value_type) {
		return false
	}
	expected_base := strip_pointer_type_name(expected)
	value_base := strip_pointer_type_name(value_type)
	if expected_base == value_base {
		return true
	}
	return short_type_name(expected_base) == short_type_name(value_base)
}
