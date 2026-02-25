// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types

fn array_alias_elem_type(arr_type string) string {
	base := arr_type.trim_right('*')
	if base.starts_with('Array_') {
		return unmangle_c_ptr_type(base['Array_'.len..])
	}
	if base in ['strings__Builder', 'Builder'] {
		return 'u8'
	}
	return ''
}

fn is_builtin_array_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/array.v')
}

fn should_keep_builtin_array_decl(decl ast.FnDecl) bool {
	return decl.name in ['sort', 'grow_len', 'delete', 'clear', 'prepend', 'insert']
}

fn (mut g Gen) emit_missing_array_contains_fallbacks() {
	mut fn_names := g.fn_param_types.keys()
	fn_names.sort()
	mut emitted_any := false
	for fn_name in fn_names {
		if !fn_name.starts_with('Array_') || !fn_name.ends_with('_contains') {
			continue
		}
		if fn_name !in g.called_fn_names {
			continue
		}
		fn_key := 'fn_${fn_name}'
		if fn_key in g.emitted_types {
			continue
		}
		if g.fn_return_types[fn_name] or { '' } != 'bool' {
			continue
		}
		param_types := g.fn_param_types[fn_name] or { []string{} }
		if param_types.len != 2 {
			continue
		}
		arr_type := param_types[0]
		elem_type := param_types[1]
		if arr_type.len == 0 || elem_type.len == 0 {
			continue
		}
		g.emitted_types[fn_key] = true
		g.sb.writeln('')
		g.sb.writeln('__attribute__((weak)) bool ${fn_name}(${arr_type} a, ${elem_type} v) {')
		g.sb.writeln('\tfor (int i = 0; (i < a.len); i += 1) {')
		if elem_type == 'string' {
			g.sb.writeln('\t\tif (string__eq(*((string*)(array__get(a, i))), v)) {')
		} else {
			left_cmp := '_cmp_l_${g.tmp_counter}'
			right_cmp := '_cmp_r_${g.tmp_counter + 1}'
			g.tmp_counter += 2
			g.sb.writeln('\t\tif (({ ${elem_type} ${left_cmp} = *((${elem_type}*)(array__get(a, i))); ${elem_type} ${right_cmp} = v; memcmp(&${left_cmp}, &${right_cmp}, sizeof(${elem_type})) == 0; })) {')
		}
		g.sb.writeln('\t\t\treturn true;')
		g.sb.writeln('\t\t}')
		g.sb.writeln('\t}')
		g.sb.writeln('\treturn false;')
		g.sb.writeln('}')
		emitted_any = true
	}
	if emitted_any {
		g.sb.writeln('')
	}
}

fn (mut g Gen) emit_deferred_fixed_array_aliases() {
	mut names := g.array_aliases.keys()
	// Sort by name length then alphabetically so leaf types (shorter names)
	// are emitted before compound types that depend on them.
	// E.g. Array_fixed_string_2 before Array_fixed_Array_fixed_string_2_2.
	for i := 0; i < names.len; i++ {
		for j := i + 1; j < names.len; j++ {
			if names[i].len > names[j].len || (names[i].len == names[j].len && names[i] > names[j]) {
				names[i], names[j] = names[j], names[i]
			}
		}
	}
	for name in names {
		if !name.starts_with('Array_fixed_') {
			continue
		}
		if info := g.collected_fixed_array_types[name] {
			if info.elem_type in primitive_types
				|| info.elem_type in ['char', 'voidptr', 'charptr', 'byteptr', 'void*', 'char*'] {
				continue // already emitted in emit_runtime_aliases
			}
			g.sb.writeln('typedef ${info.elem_type} ${name} [${info.size}];')
			alias_key := 'alias_${name}'
			body_key := 'body_${name}'
			g.emitted_types[alias_key] = true
			g.emitted_types[body_key] = true
			// Emit fallback str macros for fixed array types
			g.sb.writeln('#define ${name}_str(a) ((string){.str = "${name}", .len = ${name.len}, .is_lit = 1})')
			g.sb.writeln('#define ${name}__str(a) ${name}_str(a)')
		}
	}
}

fn (mut g Gen) array_append_elem_type(lhs ast.Expr, rhs ast.Expr) (bool, string) {
	mut lhs_type := g.get_expr_type(lhs)
	mut elem_type := ''
	mut is_array_append := lhs_type == 'array' || lhs_type.starts_with('Array_')
	if lhs_type.starts_with('Array_') {
		elem_type = lhs_type['Array_'.len..].trim_right('*')
	}
	if raw_type := g.get_raw_type(lhs) {
		match raw_type {
			types.Array {
				is_array_append = true
				if elem_type == '' {
					elem_type = g.types_type_to_c(raw_type.elem_type)
				}
			}
			types.Alias {
				if raw_type.base_type is types.Array {
					is_array_append = true
					if elem_type == '' {
						elem_type = g.types_type_to_c(raw_type.base_type.elem_type)
					}
				}
			}
			types.Pointer {
				match raw_type.base_type {
					types.Array {
						is_array_append = true
						if elem_type == '' {
							elem_type = g.types_type_to_c(raw_type.base_type.elem_type)
						}
					}
					types.Alias {
						if raw_type.base_type.base_type is types.Array {
							is_array_append = true
							if elem_type == '' {
								elem_type = g.types_type_to_c(raw_type.base_type.base_type.elem_type)
							}
						}
					}
					else {}
				}
			}
			else {}
		}
	}
	if !is_array_append {
		return false, ''
	}
	if elem_type == '' || elem_type == 'int' {
		rhs_type := g.get_expr_type(rhs)
		if rhs_type != '' && rhs_type !in ['int_literal', 'float_literal'] {
			elem_type = rhs_type.trim_right('*')
		}
	}
	// When raw_type gives a module-qualified name (e.g. term__Coord) but the rhs
	// is a local struct (e.g. Coord), prefer the rhs type to match the actual typedef.
	if elem_type.contains('__') {
		rhs_type := g.get_expr_type(rhs).trim_right('*')
		if rhs_type != '' && !rhs_type.contains('__')
			&& rhs_type !in ['int', 'int_literal', 'float_literal', 'void', 'void*', 'voidptr']
			&& elem_type.ends_with('__${rhs_type}') {
			elem_type = rhs_type
		}
	}
	if elem_type == '' {
		elem_type = 'int'
	}
	return true, unmangle_c_ptr_type(elem_type)
}

fn (mut g Gen) expr_is_array_value(expr ast.Expr) bool {
	if expr is ast.ParenExpr {
		return g.expr_is_array_value(expr.expr)
	}
	if expr is ast.PrefixExpr && expr.op == .mul {
		// `*ptr_to_array` is an array value.
		return g.expr_is_array_value(expr.expr)
	}
	expr_type := g.get_expr_type(expr)
	if expr_type == 'array' || expr_type.starts_with('Array_') {
		return true
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.Array {
				return true
			}
			types.Alias {
				return raw_type.base_type is types.Array
			}
			types.Pointer {
				match raw_type.base_type {
					types.Array {
						return true
					}
					types.Alias {
						return raw_type.base_type.base_type is types.Array
					}
					else {
						return false
					}
				}
			}
			else {
				return false
			}
		}
	}
	return false
}

fn (mut g Gen) expr_array_runtime_type(expr ast.Expr) string {
	if expr is ast.ParenExpr {
		return g.expr_array_runtime_type(expr.expr)
	}
	if expr is ast.PrefixExpr && expr.op == .mul {
		// Try direct dereferenced type first.
		deref_type := g.get_expr_type(expr)
		if deref_type != '' && deref_type != 'int_literal' && deref_type != 'float_literal' {
			return deref_type
		}
		inner_type := g.expr_array_runtime_type(expr.expr)
		if inner_type.ends_with('*') {
			return inner_type[..inner_type.len - 1]
		}
	}
	mut typ := g.get_expr_type(expr)
	if typ != '' && typ != 'int_literal' && typ != 'float_literal' {
		return typ
	}
	if raw_type := g.get_raw_type(expr) {
		typ = g.types_type_to_c(raw_type)
	}
	if typ == '' {
		return 'array'
	}
	return typ
}

// array_elem_type_from_expr resolves the element type of an array expression
// using the types.Environment.
fn (mut g Gen) infer_array_elem_type_from_expr(arr_expr ast.Expr) string {
	// For slice calls, the return type is generic `array`; resolve from the source array.
	if arr_expr is ast.CallExpr {
		if arr_expr.lhs is ast.Ident {
			fn_name := sanitize_fn_ident(arr_expr.lhs.name)
			if fn_name in ['array__slice', 'array__slice_ni']
				|| (fn_name.starts_with('Array_') && (fn_name.ends_with('__slice')
				|| fn_name.ends_with('__slice_ni'))) {
				if arr_expr.args.len > 0 {
					return g.infer_array_elem_type_from_expr(arr_expr.args[0])
				}
			}
		}
	}
	if raw_type := g.get_raw_type(arr_expr) {
		if elem := array_elem_type_from_raw(raw_type) {
			return g.types_type_to_c(elem)
		}
	}
	// Secondary path: when get_raw_type fails (e.g. SelectorExpr on sum type cast pointers
	// with invalid pos.ids), resolve the C type name back to a raw type.
	arr_type := g.get_expr_type(arr_expr)
	if arr_type != '' && arr_type != 'int' && arr_type != 'array' {
		if raw := g.resolve_c_type_to_raw(arr_type) {
			if elem := array_elem_type_from_raw(raw) {
				return g.types_type_to_c(elem)
			}
		}
	}
	// Last resort: string-based extraction (e.g. Array_int → int).
	return array_alias_elem_type(arr_type)
}

// array_elem_type_from_raw extracts the element type from a raw types.Type
// that represents an array, unwrapping Pointer and Alias layers as needed.
fn array_elem_type_from_raw(t types.Type) ?types.Type {
	match t {
		types.Array {
			return t.elem_type
		}
		types.Pointer {
			return array_elem_type_from_raw(t.base_type)
		}
		types.Alias {
			return array_elem_type_from_raw(t.base_type)
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) infer_array_method_elem_type(expr ast.Expr) string {
	match expr {
		ast.CallExpr {
			if expr.lhs is ast.Ident
				&& expr.lhs.name in ['array__pop', 'array__pop_left', 'array__first', 'array__last'] {
				if expr.args.len > 0 {
					return g.infer_array_elem_type_from_expr(expr.args[0])
				}
			}
			if expr.lhs is ast.SelectorExpr
				&& expr.lhs.rhs.name in ['pop', 'pop_left', 'first', 'last'] {
				arr_expr := if expr.args.len > 0 { expr.args[0] } else { expr.lhs.lhs }
				return g.infer_array_elem_type_from_expr(arr_expr)
			}
		}
		else {}
	}
	return ''
}

fn (mut g Gen) infer_array_contains_elem_type(name string, call_args []ast.Expr) string {
	arr_type := if call_args.len > 0 { g.get_expr_type(call_args[0]) } else { '' }
	if arr_type.starts_with('Array_fixed_') {
		if finfo := g.collected_fixed_array_types[arr_type] {
			return finfo.elem_type
		}
	} else if arr_type.starts_with('Array_') {
		return arr_type['Array_'.len..]
	}
	mut suffix := ''
	if name.starts_with('array__contains_') {
		suffix = name['array__contains_'.len..]
	}
	if suffix != '' && suffix != 'void' {
		return suffix
	}
	if call_args.len > 1 {
		rhs_type := g.get_expr_type(call_args[1])
		if rhs_type != '' && rhs_type != 'int_literal' && rhs_type != 'float_literal'
			&& rhs_type != 'void' {
			return rhs_type.trim_right('*')
		}
	}
	return 'int'
}

fn (mut g Gen) fixed_array_selector_elem_type(sel ast.SelectorExpr) string {
	mut struct_name := ''
	if raw_type := g.get_raw_type(sel.lhs) {
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					struct_name = raw_type.base_type.name
				} else if raw_type.base_type is types.Alias {
					struct_name = raw_type.base_type.name
				}
			}
			types.Struct {
				struct_name = raw_type.name
			}
			types.Alias {
				struct_name = raw_type.name
			}
			else {}
		}
	}
	if struct_name == '' {
		struct_name = g.get_expr_type(sel.lhs).trim_right('*')
	}
	if struct_name != '' {
		field_key := '${struct_name}.${sel.rhs.name}'
		if elem := g.fixed_array_field_elem[field_key] {
			return elem
		}
	}
	if struct_name.contains('__') {
		short_name := struct_name.all_after_last('__')
		short_field_key := '${short_name}.${sel.rhs.name}'
		if elem := g.fixed_array_field_elem[short_field_key] {
			return elem
		}
	}
	return ''
}

fn (mut g Gen) is_fixed_array_selector(sel ast.SelectorExpr) bool {
	if g.fixed_array_selector_elem_type(sel) != '' {
		return true
	}
	return false
}

fn (mut g Gen) gen_array_init_expr(node ast.ArrayInitExpr) {
	raw_elem := g.extract_array_elem_type(node.typ)
	// Convert C pointer syntax to mangled name for composite types:
	// Array_int* -> Array_intptr (typedef'd alias)
	elem_type := if raw_elem.ends_with('*')
		&& (raw_elem.starts_with('Array_') || raw_elem.starts_with('Map_')) {
		mangle_alias_component(raw_elem)
	} else {
		unmangle_c_ptr_type(raw_elem)
	}
	is_dyn := g.is_dynamic_array_type(node.typ)
	// Fallback: if dynamic array but elem type couldn't be extracted from type annotation,
	// infer from the first expression (e.g., for [][2]int where inner [2]int has type info)
	mut final_elem := elem_type
	if final_elem == '' && is_dyn && node.exprs.len > 0 {
		inferred := g.get_expr_type(node.exprs[0])
		if inferred != '' && inferred != 'array' && inferred != 'int' {
			final_elem = inferred
		}
	}
	if node.exprs.len > 0 {
		// Has elements
		if final_elem != '' && is_dyn {
			// Dynamic array compound literal: (elem_type[N]){e1, e2, ...}
			g.sb.write_string('(${final_elem}[${node.exprs.len}]){')
			for i, e in node.exprs {
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.expr(e)
			}
			g.sb.write_string('}')
			return
		}
		// Fixed-size array or untyped: {e1, e2, ...}
		g.sb.write_string('{')
		for i, e in node.exprs {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.expr(e)
		}
		g.sb.write_string('}')
		return
	}
	// Empty array: should have been lowered by transformer to __new_array_with_default_noscan()
	// Fallback: zero-init
	g.sb.write_string('(array){0}')
}

// extract_array_elem_expr extracts the element type expression from an array type

fn (g &Gen) extract_array_elem_expr(e ast.Expr) ast.Expr {
	match e {
		ast.Type {
			if e is ast.ArrayType {
				return e.elem_type
			}
			if e is ast.ArrayFixedType {
				return e.elem_type
			}
		}
		else {}
	}
	return ast.empty_expr
}

// extract_array_elem_type extracts the element C type from an array type expression

fn (mut g Gen) extract_array_elem_type(e ast.Expr) string {
	elem_expr := g.extract_array_elem_expr(e)
	if elem_expr != ast.empty_expr {
		return g.expr_type_to_c(elem_expr)
	}
	return ''
}

// is_dynamic_array_type checks if the type expression is a dynamic array (ArrayType, not ArrayFixedType)

fn (g &Gen) is_dynamic_array_type(e ast.Expr) bool {
	match e {
		ast.Type {
			if e is ast.ArrayType {
				return true
			}
		}
		else {}
	}
	return false
}

fn extract_array_init_arg(expr ast.Expr) ?ast.ArrayInitExpr {
	unwrapped := strip_expr_wrappers(expr)
	match unwrapped {
		ast.ArrayInitExpr {
			return unwrapped
		}
		ast.PrefixExpr {
			if unwrapped.op == .amp {
				return extract_array_init_arg(unwrapped.expr)
			}
		}
		else {}
	}
	return none
}

fn (mut g Gen) try_emit_const_dynamic_array_call(name string, value ast.Expr) bool {
	base_value := strip_expr_wrappers(value)
	if base_value !is ast.CallExpr {
		return false
	}
	call := base_value as ast.CallExpr
	if call.lhs !is ast.Ident {
		return false
	}
	fn_name := (call.lhs as ast.Ident).name
	if fn_name !in ['builtin__new_array_from_c_array_noscan', 'builtin__new_array_from_c_array',
		'new_array_from_c_array'] {
		return false
	}
	if call.args.len < 4 {
		return false
	}
	array_data := extract_array_init_arg(call.args[3]) or { return false }
	mut elem_type := g.extract_array_elem_type(array_data.typ)
	if elem_type == '' && array_data.exprs.len > 0 {
		elem_type = g.get_expr_type(array_data.exprs[0])
	}
	if elem_type == '' || elem_type == 'array' {
		return false
	}
	// Check that no element contains a function call (not valid in C static initializers)
	for elem in array_data.exprs {
		if g.contains_call_expr(elem) {
			return false
		}
	}
	mut len_expr := expr_to_int_str(call.args[0])
	mut cap_expr := expr_to_int_str(call.args[1])
	if len_expr == '0' && array_data.exprs.len > 0 {
		len_expr = '${array_data.exprs.len}'
	}
	if cap_expr == '0' {
		cap_expr = len_expr
	}
	data_name := '__const_array_data_${name}'
	if array_data.exprs.len > 0 {
		g.sb.write_string('static ${elem_type} ${data_name}[${array_data.exprs.len}] = {')
		for i, e in array_data.exprs {
			if i > 0 {
				g.sb.write_string(', ')
			}
			g.expr(e)
		}
		g.sb.writeln('};')
		g.sb.writeln('array ${name} = ((array){ .data = ${data_name}, .offset = 0, .len = ${len_expr}, .cap = ${cap_expr}, .flags = 0, .element_size = sizeof(${elem_type}) });')
	} else {
		g.sb.writeln('array ${name} = ((array){ .data = NULL, .offset = 0, .len = ${len_expr}, .cap = ${cap_expr}, .flags = 0, .element_size = sizeof(${elem_type}) });')
	}
	return true
}

fn (mut g Gen) gen_fixed_array_cmp_operand(expr ast.Expr, fixed_type string) {
	// For ArrayInitExpr (fixed array literals), wrap in compound literal
	if expr is ast.ArrayInitExpr {
		g.sb.write_string('(${fixed_type})')
		g.expr(expr)
		return
	}
	// Unwrap parens to find ArrayInitExpr inside
	if expr is ast.ParenExpr {
		inner := g.unwrap_parens(expr.expr)
		if inner is ast.ArrayInitExpr {
			g.sb.write_string('(${fixed_type})')
			g.expr(inner)
			return
		}
	}
	g.expr(expr)
}
