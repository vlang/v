// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import v2.ast
import v2.types
import strings
import os

fn is_empty_expr(e ast.Expr) bool {
	return e is ast.EmptyExpr
}

fn is_none_expr(expr ast.Expr) bool {
	match expr {
		ast.Keyword {
			return expr.tok == .key_none
		}
		ast.Ident {
			return expr.name == 'none'
		}
		else {
			return false
		}
	}
}

fn is_none_like_expr(expr ast.Expr) bool {
	if is_none_expr(expr) {
		return true
	}
	if expr is ast.Type && expr is ast.NoneType {
		return true
	}
	return false
}

fn is_numeric_literal(expr ast.Expr) bool {
	if expr is ast.BasicLiteral {
		return expr.kind == .number
	}
	if expr is ast.CastExpr {
		return is_numeric_literal(expr.expr)
	}
	if expr is ast.CallOrCastExpr {
		return is_numeric_literal(expr.expr)
	}
	if expr is ast.ParenExpr {
		return is_numeric_literal(expr.expr)
	}
	return false
}

fn is_nil_like_expr(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name == 'nil'
		}
		ast.BasicLiteral {
			return expr.kind == .number && expr.value == '0'
		}
		ast.ParenExpr {
			return is_nil_like_expr(expr.expr)
		}
		ast.ModifierExpr {
			return is_nil_like_expr(expr.expr)
		}
		ast.CastExpr {
			return is_nil_like_expr(expr.expr)
		}
		ast.CallOrCastExpr {
			// CallOrCastExpr can be a type cast wrapping 0
			return is_nil_like_expr(expr.expr)
		}
		ast.UnsafeExpr {
			if expr.stmts.len == 0 {
				return false
			}
			last := expr.stmts[expr.stmts.len - 1]
			if last is ast.ExprStmt {
				return is_nil_like_expr(last.expr)
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (g &Gen) interface_method_by_name(iface_name string, method_name string) ?InterfaceMethodInfo {
	mut candidates := []string{}
	if iface_name != '' {
		candidates << iface_name
		short_name := iface_name.all_after_last('__')
		if short_name != iface_name {
			candidates << short_name
		}
		if g.cur_module != '' && g.cur_module != 'main' {
			candidates << '${g.cur_module}__${short_name}'
		}
		candidates << 'builtin__${short_name}'
	}
	for candidate in candidates {
		if methods := g.interface_methods[candidate] {
			for method in methods {
				if method.name == method_name {
					return method
				}
			}
		}
	}
	return none
}

// find_method_on_any_interface searches all registered interfaces for a method by name.
// Used for interface-to-interface narrowing where the method is on the target interface.
fn (g &Gen) find_method_on_any_interface(method_name string) ?InterfaceMethodInfo {
	for _, methods in g.interface_methods {
		for method in methods {
			if method.name == method_name {
				return method
			}
		}
	}
	return none
}

fn (g &Gen) is_interface_type(type_name string) bool {
	// Array/Map/option/result types are never interfaces, even if their element type is.
	if type_name.starts_with('Array_') || type_name.starts_with('Map_')
		|| type_name.starts_with('_option_') || type_name.starts_with('_result_') {
		return false
	}
	mut interface_candidates := []string{}
	if type_name != '' {
		interface_candidates << type_name
		short_name := type_name.all_after_last('__')
		if short_name != type_name {
			interface_candidates << short_name
		}
		if g.cur_module != '' && g.cur_module != 'main' {
			interface_candidates << '${g.cur_module}__${short_name}'
		}
		interface_candidates << 'builtin__${short_name}'
	}
	if g.env == unsafe { nil } {
		for candidate in interface_candidates {
			if candidate in g.interface_methods {
				return true
			}
		}
		return false
	}
	// Try the type name as-is in the current module scope
	if mut scope := g.env_scope(g.cur_module) {
		if obj := scope.lookup_parent(type_name, 0) {
			if obj is types.Type && obj is types.Interface {
				return true
			}
		}
		// Also try stripping module prefix: rand__PRNG -> PRNG
		if type_name.contains('__') {
			short_name := type_name.all_after_last('__')
			if obj := scope.lookup_parent(short_name, 0) {
				if obj is types.Type && obj is types.Interface {
					return true
				}
			}
		}
	}
	// Also check interface_methods registry.
	for candidate in interface_candidates {
		if candidate in g.interface_methods {
			return true
		}
	}
	return false
}

// is_interface_vtable_method checks if a method is a vtable (abstract) method
// of an interface, as opposed to a concrete method defined on the interface type.
fn (g &Gen) is_interface_vtable_method(iface_name string, method_name string) bool {
	// Every interface value carries a synthetic type_name(void*) callback in its
	// runtime header, even when not declared in the interface method set.
	if method_name == 'type_name' {
		return true
	}
	return g.interface_method_by_name(iface_name, method_name) != none
}

fn (mut g Gen) selector_method_value_name(node ast.SelectorExpr) ?string {
	if raw := g.get_raw_type(ast.Expr(node)) {
		mut is_fn_value := false
		match raw {
			types.FnType {
				is_fn_value = true
			}
			types.Alias {
				alias_raw := raw
				if alias_raw.base_type is types.FnType {
					is_fn_value = true
				}
			}
			else {}
		}

		if !is_fn_value {
			return none
		}
	}
	recv_type := g.get_expr_type(node.lhs)
	if recv_type == '' {
		return none
	}
	mut base := recv_type
	if base.ends_with('*') {
		base = base[..base.len - 1]
	}
	mut candidates := []string{}
	candidates << '${base}__${node.rhs.name}'
	if base.contains('__') {
		candidates << '${base.all_after_last('__')}__${node.rhs.name}'
	}
	for candidate in candidates {
		if candidate in g.fn_return_types || candidate in g.fn_param_is_ptr {
			return candidate
		}
	}
	return none
}

fn (mut g Gen) gen_bound_method_value_expr(node ast.SelectorExpr, expected_c_type string) bool {
	method_name := g.selector_method_value_name(node) or { return false }
	method_params := g.fn_param_types[method_name] or { return false }
	if method_params.len == 0 {
		return false
	}
	receiver_type := method_params[0]
	if receiver_type == '' {
		return false
	}
	lhs_type := g.get_expr_type(node.lhs)
	if lhs_type == '' {
		return false
	}
	lhs_base := lhs_type.trim_right('*')
	recv_base := receiver_type.trim_right('*')
	// Only bind true method values where LHS matches receiver.
	if lhs_base != recv_base && short_type_name(lhs_base) != short_type_name(recv_base) {
		return false
	}
	callback_param_types := method_params[1..]
	mut ret_type := g.fn_return_types[method_name] or { 'void' }
	if ret_type == '' {
		ret_type = 'void'
	}
	bind_id := g.tmp_counter
	g.tmp_counter++
	recv_store := '_bound_recv_${bind_id}'
	wrapper_name := '_bound_method_${bind_id}'
	mut def := strings.new_builder(256)
	def.writeln('static ${receiver_type} ${recv_store};')
	def.write_string('static ${ret_type} ${wrapper_name}(')
	if callback_param_types.len == 0 {
		def.write_string('void')
	} else {
		for i, ptyp in callback_param_types {
			if i > 0 {
				def.write_string(', ')
			}
			def.write_string('${ptyp} _arg${i}')
		}
	}
	def.writeln(') {')
	def.write_string('\t')
	if ret_type != 'void' {
		def.write_string('return ')
	}
	def.write_string('${method_name}(${recv_store}')
	for i in 0 .. callback_param_types.len {
		def.write_string(', _arg${i}')
	}
	def.writeln(');')
	def.writeln('}')
	def.writeln('')
	g.anon_fn_defs << def.str()
	g.sb.write_string('(({ ${recv_store} = ')
	if receiver_type.ends_with('*') {
		g.expr(node.lhs)
	} else {
		if lhs_type.ends_with('*') {
			g.sb.write_string('(*')
			g.expr(node.lhs)
			g.sb.write_string(')')
		} else {
			g.expr(node.lhs)
		}
	}
	g.sb.write_string('; ((${expected_c_type})${wrapper_name}); }))')
	return true
}

fn (mut g Gen) gen_sum_variant_tag_path_check(expr ast.Expr, path []SumVariantTagStep, positive bool) {
	if path.len == 0 {
		g.sb.write_string(if positive { 'false' } else { 'true' })
		return
	}
	if !positive {
		g.sb.write_string('!')
	}
	g.sb.write_string('(')
	mut access := g.expr_to_string(expr)
	mut sep := if g.expr_is_pointer(expr) { '->' } else { '.' }
	for i, step in path {
		if i > 0 {
			g.sb.write_string(' && ')
		}
		g.sb.write_string('(${access}${sep}_tag == ${step.tag})')
		access = '(((${step.payload_type}*)(${access}${sep}_data._${step.variant_field})))'
		sep = '->'
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_nested_sum_as_cast(inner_str string, expr ast.Expr, target_type string, path []SumVariantTagStep) bool {
	if path.len <= 1 {
		return false
	}
	mut access := '(${inner_str})'
	mut sep := if g.expr_is_pointer(expr) { '->' } else { '.' }
	for i, step in path {
		payload_ptr := '(${access}${sep}_data._${step.variant_field})'
		if i == path.len - 1 {
			if !g.is_scalar_sum_payload_type(target_type) && target_type != 'string' {
				g.sb.write_string('(${payload_ptr} ? (*((${target_type}*)${payload_ptr})) : (${target_type}){0})')
			} else {
				g.sb.write_string('(*((${target_type}*)${payload_ptr}))')
			}
			return true
		}
		access = '(*(${step.payload_type}*)${payload_ptr})'
		sep = '.'
	}
	return false
}

fn (mut g Gen) gen_nested_sum_as_cast_for_selector_source(expr ast.Expr, target_type_name string) bool {
	unwrapped := g.unwrap_parens(expr)
	if unwrapped !is ast.SelectorExpr {
		return false
	}
	sel := unwrapped as ast.SelectorExpr
	mut source_types := []string{}
	for candidate in [
		g.selector_storage_field_type(sel).trim_right('*'),
		g.selector_declared_field_type(sel).trim_right('*'),
		g.selector_field_type(sel).trim_right('*'),
		g.get_expr_type(sel).trim_right('*'),
	] {
		if candidate != '' && candidate !in source_types
			&& g.get_sum_type_variants_for(candidate).len > 0 {
			source_types << candidate
		}
	}
	for source_type in source_types {
		if g.sum_type_has_direct_variant(source_type, target_type_name) {
			return false
		}
		path := g.sum_variant_tag_path(source_type, target_type_name, []string{}) or { continue }
		if path.len <= 1 {
			continue
		}
		nested_target_type := path[path.len - 1].payload_type
		if g.gen_nested_sum_as_cast(g.raw_selector_expr_code(sel), sel, nested_target_type, path) {
			return true
		}
	}
	return false
}

fn (mut g Gen) nested_sum_path_from_narrowed_source(source_sum_type string, target_type string) ?[]SumVariantTagStep {
	if source_sum_type == '' || target_type == '' {
		return none
	}
	for sum_name, _ in g.sum_type_variants {
		path := g.sum_variant_tag_path(sum_name, target_type, []string{}) or { continue }
		if path.len <= 1 {
			continue
		}
		first := path[0]
		if first.payload_type == source_sum_type || first.variant_field == source_sum_type
			|| first.payload_type.all_after_last('__') == source_sum_type.all_after_last('__') {
			return path
		}
	}
	return none
}

fn vector_field_index(field string) int {
	return match field {
		'x' { 0 }
		'y' { 1 }
		'z' { 2 }
		'w' { 3 }
		else { -1 }
	}
}

fn vector_elem_type_for_name(type_name string) string {
	base := type_name.trim_right('*')
	if base.ends_with('SimdFloat4') || base.ends_with('SimdFloat2') {
		return 'f32'
	}
	if base.ends_with('SimdInt4') || base.ends_with('SimdI32_2') {
		return 'i32'
	}
	if base.ends_with('SimdU32_4') || base.ends_with('SimdUint2') {
		return 'u32'
	}
	return ''
}

fn (mut g Gen) raw_concrete_type_for_interface_value(type_name string, value_expr ast.Expr) string {
	if value_expr is ast.CastExpr
		&& g.expr_type_to_c(value_expr.typ) in [type_name, 'builtin__${type_name}'] {
		return g.raw_concrete_type_for_interface_value(type_name, value_expr.expr)
	}
	if value_expr is ast.ParenExpr {
		return g.raw_concrete_type_for_interface_value(type_name, value_expr.expr)
	}
	mut concrete_type := g.get_expr_type(value_expr)
	if value_expr is ast.PrefixExpr && value_expr.op == .amp && value_expr.expr is ast.Ident {
		inner := value_expr.expr as ast.Ident
		if local_type := g.get_local_var_c_type(inner.name) {
			local_base := local_type.trim_right('*')
			if local_type != '' && local_type != 'int' && !g.is_interface_type(local_base) {
				concrete_type = '${local_type}*'
			}
		}
	}
	if value_expr is ast.Ident {
		if local_type := g.get_local_var_c_type(value_expr.name) {
			local_base := local_type.trim_right('*')
			current_base := concrete_type.trim_right('*')
			if local_type != '' && local_type != 'int' && !g.is_interface_type(local_base)
				&& (concrete_type == '' || concrete_type == 'int'
				|| current_base == type_name || local_type.contains('_T_')) {
				concrete_type = local_type
			}
		}
	}
	mut raw_concrete := ''
	if raw := g.get_raw_type(value_expr) {
		raw_concrete = g.types_type_to_c(raw)
	}
	raw_base := raw_concrete.trim_right('*')
	current_base := concrete_type.trim_right('*')
	if raw_base != '' && raw_base != 'int'
		&& (current_base == '' || current_base == 'int' || current_base == type_name) {
		concrete_type = raw_concrete
	}
	// For dereference expressions (*ptr), strip one pointer level from the type.
	// E.g., `*sw` where sw is `SubWindow*` → concrete type is `SubWindow`, not `SubWindow*`.
	if value_expr is ast.PrefixExpr && value_expr.op == .mul && concrete_type.ends_with('*') {
		concrete_type = concrete_type[..concrete_type.len - 1]
	}
	return concrete_type
}

fn (mut g Gen) ierror_concrete_base_for_expr(value_expr ast.Expr) string {
	return g.qualify_ierror_concrete_base(g.raw_concrete_type_for_interface_value('IError',
		value_expr).trim_right('*'))
}

fn (g &Gen) qualify_ierror_concrete_base(base string) string {
	normalized_base := g.normalize_builtin_qualified_c_type(base)
	if normalized_base != base {
		return normalized_base
	}
	if base == '' || base == 'int' || base.contains('__') {
		return base
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		qualified := '${g.cur_module}__${base}'
		if '${qualified}__msg' in g.fn_return_types || 'body_${qualified}' in g.emitted_types {
			return qualified
		}
	}
	suffix := '__${base}__msg'
	mut fn_names := g.fn_return_types.keys()
	fn_names.sort()
	for fn_name in fn_names {
		if fn_name.ends_with(suffix) {
			return fn_name[..fn_name.len - '__msg'.len]
		}
	}
	body_suffix := '__${base}'
	mut body_keys := g.emitted_types.keys()
	body_keys.sort()
	for body_key in body_keys {
		if body_key.starts_with('body_') && body_key.ends_with(body_suffix) {
			return body_key['body_'.len..]
		}
	}
	mut pending_body_keys := g.pending_late_body_keys.keys()
	pending_body_keys.sort()
	for body_key in pending_body_keys {
		if body_key.starts_with('body_') && body_key.ends_with(body_suffix) {
			return body_key['body_'.len..]
		}
	}
	return base
}

fn (mut g Gen) concrete_type_for_interface_value(type_name string, value_expr ast.Expr) string {
	mut concrete_type := g.raw_concrete_type_for_interface_value(type_name, value_expr)
	if type_name in ['IError', 'builtin__IError'] {
		base := g.qualify_ierror_concrete_base(concrete_type.trim_right('*'))
		if base != '' && base != 'int' && base != concrete_type.trim_right('*') {
			if concrete_type.ends_with('*') {
				concrete_type = '${base}*'
			} else {
				concrete_type = base
			}
		}
	}
	return concrete_type
}

// gen_heap_interface_cast generates a heap-allocated interface struct for &InterfaceType(value) patterns.
// Returns true if the type is an interface and the cast was generated.
fn (mut g Gen) gen_heap_interface_cast(type_name string, value_expr ast.Expr) bool {
	if !g.is_interface_type(type_name) {
		return false
	}
	if is_nil_like_expr(value_expr) || is_none_like_expr(value_expr) {
		g.sb.write_string('({ ${type_name}* _iface_t = (${type_name}*)malloc(sizeof(${type_name})); *_iface_t = ((${type_name}){0}); _iface_t; })')
		return true
	}
	mut concrete_type := g.concrete_type_for_interface_value(type_name, value_expr)
	if concrete_type == '' || concrete_type == 'int' {
		return false
	}
	base_concrete := if concrete_type.ends_with('*') {
		concrete_type[..concrete_type.len - 1]
	} else {
		concrete_type
	}
	if type_name in ['IError', 'builtin__IError'] {
		g.ierror_wrapper_bases[base_concrete] = true
		g.needed_ierror_wrapper_bases[base_concrete] = true
	}
	if type_name in ['IError', 'builtin__IError'] && base_concrete != type_name
		&& g.concrete_ierror_base_for_c_type(concrete_type) != '' {
		g.sb.write_string('({ ${type_name}* _iface_t = (${type_name}*)malloc(sizeof(${type_name})); *_iface_t = ')
		g.gen_ierror_from_concrete_expr(value_expr, concrete_type)
		g.sb.write_string('; _iface_t; })')
		return true
	}
	// IError constants like `none__ = IError(&None__{})` may be emitted before
	// embedded Error metadata is available. Still use the IError wrapper shape so
	// the generated C references wrapper functions, not missing direct methods.
	if type_name in ['IError', 'builtin__IError'] && base_concrete != type_name {
		base := g.qualify_ierror_concrete_base(base_concrete)
		if base != '' && base != 'int' {
			g.sb.write_string('({ ${type_name}* _iface_t = (${type_name}*)malloc(sizeof(${type_name})); *_iface_t = ')
			g.gen_ierror_from_base_expr(base, value_expr, concrete_type)
			g.sb.write_string('; _iface_t; })')
			return true
		}
	}
	// Generate: ({ InterfaceType* _iface = malloc(sizeof(InterfaceType));
	//             *_iface = (InterfaceType){._object = (void*)value, ...}; _iface; })
	g.sb.write_string('({ ${type_name}* _iface_t = (${type_name}*)malloc(sizeof(${type_name})); *_iface_t = ((${type_name}){._object = ')
	if concrete_type.ends_with('*') {
		g.sb.write_string('(void*)(')
		g.expr(value_expr)
		g.sb.write_string(')')
	} else {
		g.sb.write_string('(void*)&(')
		g.expr(value_expr)
		g.sb.write_string(')')
	}
	type_short := if base_concrete.contains('__') {
		base_concrete.all_after_last('__')
	} else {
		base_concrete
	}
	type_id := interface_type_id_for_name(type_short)
	g.sb.write_string(', ._type_id = ${type_id}')
	if methods := g.interface_methods[type_name] {
		for method in methods {
			mut fn_name := '${base_concrete}__${method.name}'
			mut uses_embedded_method := false
			if fn_name !in g.fn_param_is_ptr && fn_name !in g.fn_return_types {
				resolved := g.resolve_embedded_method(base_concrete, method.name)
				if resolved != '' {
					fn_name = resolved
					uses_embedded_method = true
				}
			}
			mut target_name := fn_name
			if ptr_params := g.fn_param_is_ptr[fn_name] {
				if uses_embedded_method || (ptr_params.len > 0 && !ptr_params[0]) {
					target_name = interface_wrapper_name(type_name, base_concrete, method.name)
					g.needed_interface_wrappers[target_name] = true
					if target_name !in g.interface_wrapper_specs {
						g.interface_wrapper_specs[target_name] = InterfaceWrapperSpec{
							fn_name:       fn_name
							concrete_type: base_concrete
							method:        method
						}
					}
				}
			}
			g.sb.write_string(', .${method.name} = (${method.cast_signature})${target_name}')
		}
	}
	g.sb.write_string('}); _iface_t; })')
	return true
}

fn (mut g Gen) gen_interface_cast(type_name string, value_expr ast.Expr) bool {
	if !g.is_interface_type(type_name) {
		return false
	}
	if is_nil_like_expr(value_expr) || is_none_like_expr(value_expr) {
		g.sb.write_string('((${type_name}){0})')
		return true
	}
	// Get the concrete type name
	mut concrete_type := g.concrete_type_for_interface_value(type_name, value_expr)
	if concrete_type == '' || concrete_type == 'int' {
		return false
	}
	// Strip pointer suffix for method name construction
	base_concrete := if concrete_type.ends_with('*') {
		concrete_type[..concrete_type.len - 1]
	} else {
		concrete_type
	}
	if type_name in ['IError', 'builtin__IError'] {
		g.ierror_wrapper_bases[base_concrete] = true
		g.needed_ierror_wrapper_bases[base_concrete] = true
	}
	if base_concrete == type_name {
		g.expr(value_expr)
		return true
	}
	// Interface-to-interface casting (e.g., Layout -> ScrollableWidget)
	// requires runtime dispatch: check _type_id to find the concrete type,
	// then construct the target interface from it.
	if g.is_interface_type(base_concrete) {
		return g.gen_iface_to_iface_cast(base_concrete, type_name, value_expr)
	}
	// Generate: (InterfaceType){._object = (void*)&expr, .method = ConcreteType__method, ...}
	// For rvalue expressions (function calls, struct init, etc.), store in a temp
	// to allow taking the address, and reuse the temp for data field pointers.
	// When concrete_type is a pointer and the value_expr is a deref (*ptr),
	// unwrap the deref: for _object we want the pointer, for data fields we use ->.
	mut effective_value := value_expr
	if concrete_type.ends_with('*') && value_expr is ast.PrefixExpr && value_expr.op == .mul {
		effective_value = value_expr.expr
	}
	if type_name in ['IError', 'builtin__IError'] {
		if g.gen_ierror_from_concrete_expr(effective_value, concrete_type) {
			return true
		}
		// See the heap IError cast path above: fallback to wrapper-based IError
		// construction even when embedded method metadata could not prove it.
		base := g.qualify_ierror_concrete_base(base_concrete)
		if base != '' && base != 'int' {
			if g.gen_ierror_from_base_expr(base, effective_value, concrete_type) {
				return true
			}
		}
	}
	is_rvalue := !concrete_type.ends_with('*') && !g.can_take_address(effective_value)
	// Use a temp variable when there are data fields and the expression is complex
	// (e.g., a function call returning a pointer). This avoids re-evaluating the
	// expression for each data field reference, preventing exponential code blowup.
	data_fields := g.interface_data_fields[type_name]
	needs_ptr_tmp := concrete_type.ends_with('*') && data_fields.len > 0
		&& !g.is_simple_addressable(effective_value)
	mut rvalue_tmp := ''
	if is_rvalue {
		rvalue_tmp = '_iface_obj${g.tmp_counter}'
		g.tmp_counter++
		g.sb.write_string('({ ${base_concrete} ${rvalue_tmp} = ')
		g.expr(effective_value)
		g.sb.write_string('; (${type_name}){._object = (void*)&${rvalue_tmp}')
	} else if needs_ptr_tmp {
		rvalue_tmp = '_iface_ptr${g.tmp_counter}'
		g.tmp_counter++
		g.sb.write_string('({ ${base_concrete}* ${rvalue_tmp} = ')
		g.expr(effective_value)
		g.sb.write_string('; (${type_name}){._object = (void*)(${rvalue_tmp})')
	} else {
		g.sb.write_string('((${type_name}){._object = ')
		if concrete_type.ends_with('*') {
			g.sb.write_string('(void*)(')
			g.expr(effective_value)
			g.sb.write_string(')')
		} else {
			g.sb.write_string('(void*)&(')
			g.expr(effective_value)
			g.sb.write_string(')')
		}
	}
	type_short := if base_concrete.contains('__') {
		base_concrete.all_after_last('__')
	} else {
		base_concrete
	}
	type_id := interface_type_id_for_name(type_short)
	g.sb.write_string(', ._type_id = ${type_id}')
	// Generate method function pointers from stored interface info
	if methods := g.interface_methods[type_name] {
		for method in methods {
			mut fn_name := '${base_concrete}__${method.name}'
			mut uses_embedded_method := false
			// If the method doesn't exist on the concrete type directly,
			// try resolving through embedded structs (e.g. ssl__SSLConn embeds
			// openssl__SSLConn, so ssl__SSLConn__addr → openssl__SSLConn__addr).
			if fn_name !in g.fn_param_is_ptr && fn_name !in g.fn_return_types {
				resolved := g.resolve_embedded_method(base_concrete, method.name)
				if resolved != '' {
					fn_name = resolved
					uses_embedded_method = true
				}
			}
			mut target_name := fn_name
			if ptr_params := g.fn_param_is_ptr[fn_name] {
				if uses_embedded_method || (ptr_params.len > 0 && !ptr_params[0]) {
					target_name = interface_wrapper_name(type_name, base_concrete, method.name)
					g.needed_interface_wrappers[target_name] = true
					if target_name !in g.interface_wrapper_specs {
						g.interface_wrapper_specs[target_name] = InterfaceWrapperSpec{
							fn_name:       fn_name
							concrete_type: base_concrete
							method:        method
						}
					}
				}
			}
			g.sb.write_string(', .${method.name} = (${method.cast_signature})${target_name}')
		}
	}
	// Generate data field pointers: .field = &(concrete->field)
	if data_fields.len > 0 {
		sep := if concrete_type.ends_with('*') { '->' } else { '.' }
		for df in data_fields {
			embedded_owner := g.embedded_owner_for(base_concrete, df.name)
			g.sb.write_string(', .${df.name} = &(')
			if rvalue_tmp != '' {
				// Use the temp variable instead of re-evaluating the expression
				g.sb.write_string(rvalue_tmp)
			} else {
				g.expr(effective_value)
			}
			if embedded_owner != '' {
				g.sb.write_string('${sep}${embedded_owner}.${df.name})')
			} else {
				g.sb.write_string('${sep}${df.name})')
			}
		}
	}
	if rvalue_tmp != '' {
		g.sb.write_string('}; })')
	} else {
		g.sb.write_string('})')
	}
	return true
}

fn c_static_v_string_expr(raw string) string {
	return c_static_v_string_expr_from_c_literal(c_string_literal_content_to_c(raw))
}

fn c_empty_v_string_expr() string {
	return c_v_string_expr_from_ptr_len('""', '0', true)
}

fn expr_to_int_str(e ast.Expr) string {
	if e is ast.BasicLiteral {
		return e.value
	}
	return '0'
}

fn (mut g Gen) known_module_runtime_symbol(module_name string, symbol_name string) ?string {
	if module_name == '' || symbol_name == '' {
		return none
	}
	if scope := g.env_scope(module_name) {
		if obj := scope.lookup(symbol_name) {
			if obj is types.Const || obj is types.Global || obj is types.Fn {
				return module_name
			}
		}
	}
	return none
}

fn (mut g Gen) module_selector_storage_c_name(module_name string, symbol_name string) string {
	c_name := if symbol_name.starts_with('${module_name}__') {
		symbol_name
	} else {
		'${module_name}__${symbol_name}'
	}
	if raw_c_name := g.c_extern_module_storage[c_name] {
		return raw_c_name
	}
	return c_name
}

// expr_to_int_str_with_env resolves fixed-array size expressions, handling
// both literal numbers and named constants (looked up via type environment).
fn (g &Gen) expr_to_int_str_with_env(e ast.Expr) string {
	if e is ast.BasicLiteral {
		return e.value
	}
	if e is ast.Ident {
		// Try to resolve the constant from the module scope.
		if g.env != unsafe { nil } {
			if scope := g.env_scope(g.cur_module) {
				if obj := scope.lookup_parent(e.name, 0) {
					if obj is types.Const {
						if obj.int_val != 0 {
							return obj.int_val.str()
						}
					}
				}
			}
		}
	}
	return '0'
}

fn (mut g Gen) local_var_c_type_for_expr(expr ast.Expr) ?string {
	unwrapped := strip_expr_wrappers(expr)
	if unwrapped !is ast.Ident {
		return none
	}
	name := unwrapped.name()
	if name == '' {
		return none
	}
	return g.get_local_var_c_type(name)
}

fn (mut g Gen) fn_type_alias_name_for_base_expr(base ast.Expr) ?string {
	mut candidates := []string{}
	base_c := g.expr_type_to_c(base).trim_space()
	if base_c != '' {
		candidates << base_c
	}
	base_name := base.name()
	if base_name != '' {
		candidates << base_name
		if base_name.contains('.') {
			candidates << base_name.replace('.', '__')
		} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
			&& !base_name.contains('__') {
			candidates << '${g.cur_module}__${base_name}'
		}
	}
	for candidate in candidates {
		if candidate in g.fn_type_aliases {
			return candidate
		}
	}
	for candidate in candidates {
		short_name := if candidate.contains('__') {
			candidate.all_after_last('__')
		} else {
			candidate
		}
		if short_name == '' {
			continue
		}
		for alias_name, _ in g.fn_type_aliases {
			if alias_name == short_name || alias_name.ends_with('__${short_name}') {
				return alias_name
			}
		}
	}
	return none
}

fn (mut g Gen) fn_type_alias_name_from_generic_name(name string) ?string {
	mut base_name := name
	if name.ends_with('_T') {
		base_name = name[..name.len - 2]
	} else if name.contains('_T_') {
		base_name = name.all_before('_T_')
	} else {
		return none
	}
	return g.fn_type_alias_name_for_base_expr(ast.Ident{
		name: base_name
	})
}

fn (mut g Gen) fn_type_alias_cast_type(lhs ast.Expr) ?string {
	match lhs {
		ast.Ident {
			return g.fn_type_alias_name_from_generic_name(lhs.name)
		}
		ast.SelectorExpr {
			if alias_name := g.fn_type_alias_name_for_base_expr(lhs) {
				return alias_name
			}
			return g.fn_type_alias_name_from_generic_name(lhs.rhs.name)
		}
		ast.GenericArgOrIndexExpr {
			return g.fn_type_alias_name_for_base_expr(lhs.lhs)
		}
		ast.GenericArgs {
			return g.fn_type_alias_name_for_base_expr(lhs.lhs)
		}
		else {
			return none
		}
	}
}

fn (mut g Gen) call_or_cast_lhs_is_type(lhs ast.Expr) bool {
	match lhs {
		ast.Type {
			return true
		}
		ast.Ident {
			if lhs.name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'bool', 'byte', 'char', 'rune', 'usize', 'isize', 'string', 'byteptr',
				'charptr', 'voidptr'] {
				return true
			}
			if lhs.name in g.fn_param_is_ptr || lhs.name in g.fn_return_types {
				return false
			}
			if g.is_type_name(lhs.name) || g.is_c_type_name(lhs.name) {
				return true
			}
			for mod in [g.cur_module, 'builtin'] {
				if mod == '' {
					continue
				}
				if mut scope := g.env_scope(mod) {
					if obj := scope.lookup_parent(lhs.name, 0) {
						if obj is types.Fn {
							return false
						}
						if obj is types.Type {
							return true
						}
					}
				}
			}
			return false
		}
		ast.SelectorExpr {
			if lhs.lhs is ast.Ident {
				mod_name := lhs.lhs.name
				if mod_name == 'C' {
					if g.is_c_type_name(lhs.rhs.name) {
						return true
					}
					// Check if a C.Type is known as a struct/type in the environment
					if g.is_type_name(lhs.rhs.name) || g.is_type_name('C__${lhs.rhs.name}') {
						return true
					}
					// C types starting with uppercase that aren't known functions
					// are likely type casts (e.g. C.FONScontext(ptr))
					c_name := lhs.rhs.name
					if c_name.len > 0 && c_name[0] >= `A` && c_name[0] <= `Z`
						&& c_name !in g.fn_return_types && c_name !in g.fn_param_is_ptr {
						return true
					}
					if c_name.contains('__') && c_name !in g.fn_return_types
						&& c_name !in g.fn_param_is_ptr {
						return true
					}
					return false
				}
				qualified := '${mod_name}__${lhs.rhs.name}'
				if qualified in g.fn_param_is_ptr || qualified in g.fn_return_types {
					return false
				}
				if g.is_type_name(qualified) || g.is_type_name(lhs.rhs.name) {
					return true
				}
				if mut scope := g.env_scope(mod_name) {
					if obj := scope.lookup_parent(lhs.rhs.name, 0) {
						return obj is types.Type
					}
				}
			}
			return false
		}
		ast.ParenExpr {
			return g.call_or_cast_lhs_is_type(lhs.expr)
		}
		ast.ModifierExpr {
			return g.call_or_cast_lhs_is_type(lhs.expr)
		}
		ast.PrefixExpr {
			return g.call_or_cast_lhs_is_type(lhs.expr)
		}
		ast.GenericArgOrIndexExpr {
			if _ := g.fn_type_alias_cast_type(lhs) {
				return true
			}
			return g.call_or_cast_lhs_is_type(lhs.lhs)
		}
		ast.GenericArgs {
			if _ := g.fn_type_alias_cast_type(lhs) {
				return true
			}
			return g.call_or_cast_lhs_is_type(lhs.lhs)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_call_or_cast_expr(node ast.CallOrCastExpr) {
	if node.expr is ast.EmptyExpr {
		g.call_expr(node.lhs, []ast.Expr{})
		return
	}
	if g.call_or_cast_lhs_is_type(node.lhs) {
		g.gen_type_cast_expr(g.expr_type_to_c(node.lhs), node.expr)
		return
	}
	g.call_expr(node.lhs, [node.expr])
}

fn (mut g Gen) gen_c_pointer_cast_selector_field_access(sel ast.SelectorExpr) bool {
	mut target_type := ''
	if sel.lhs is ast.CallOrCastExpr {
		cast_expr := sel.lhs as ast.CallOrCastExpr
		if !g.call_or_cast_lhs_is_type(cast_expr.lhs) {
			return false
		}
		target_type = g.expr_type_to_c(cast_expr.lhs)
		if target_type == '' || target_type == 'int' {
			return false
		}
		target_ptr_type := if target_type.ends_with('*') { target_type } else { '${target_type}*' }
		g.sb.write_string('((${target_ptr_type})(')
		g.expr(cast_expr.expr)
		g.sb.write_string('))->${escape_c_keyword(sel.rhs.name)}')
		return true
	} else if sel.lhs is ast.CallExpr {
		call_expr := sel.lhs as ast.CallExpr
		if call_expr.args.len != 1 || !g.call_or_cast_lhs_is_type(call_expr.lhs) {
			return false
		}
		target_type = g.expr_type_to_c(call_expr.lhs)
		if target_type == '' || target_type == 'int' {
			return false
		}
		target_ptr_type := if target_type.ends_with('*') { target_type } else { '${target_type}*' }
		g.sb.write_string('((${target_ptr_type})(')
		g.expr(call_expr.args[0])
		g.sb.write_string('))->${escape_c_keyword(sel.rhs.name)}')
		return true
	} else if sel.lhs is ast.CastExpr {
		cast_expr := sel.lhs as ast.CastExpr
		target_type = g.expr_type_to_c(cast_expr.typ)
		if target_type == '' || target_type == 'int' {
			return false
		}
		target_ptr_type := if target_type.ends_with('*') { target_type } else { '${target_type}*' }
		g.sb.write_string('((${target_ptr_type})(')
		g.expr(cast_expr.expr)
		g.sb.write_string('))->${escape_c_keyword(sel.rhs.name)}')
		return true
	} else {
		return false
	}
}

fn (mut g Gen) selector_use_ptr(lhs_expr ast.Expr) bool {
	if g.expr_is_pointer(lhs_expr) {
		return true
	}
	unwrapped := g.unwrap_parens(lhs_expr)
	if unwrapped is ast.Ident {
		if unwrapped.name in g.cur_fn_mut_params {
			return true
		}
		if local_type := g.local_var_c_type_for_expr(unwrapped) {
			return local_type.ends_with('*')
		}
	}
	lhs_type := g.get_expr_type(lhs_expr)
	if lhs_type == 'chan' {
		return true
	}
	return false
}

fn (mut g Gen) gen_channel_receive_expr(node ast.PrefixExpr) bool {
	if node.op != .arrow {
		return false
	}
	mut expr_type := g.get_expr_type(ast.Expr(node))
	mut elem_type := if expr_type.starts_with('_option_') {
		option_value_type(expr_type)
	} else if expr_type.starts_with('_result_') {
		g.result_value_type(expr_type)
	} else {
		expr_type
	}
	if elem_type == '' || elem_type == 'int' || elem_type.starts_with('_option_')
		|| elem_type.starts_with('_result_') {
		elem_type = g.channel_elem_type_from_expr(node.expr) or { '' }
	}
	if elem_type == '' || elem_type == 'int' {
		elem_type = 'void*'
	}
	g.force_emit_fn_names['sync__Channel__try_pop_priv'] = true
	g.called_fn_names['sync__Channel__try_pop_priv'] = true
	g.tmp_counter++
	if expr_type.starts_with('_option_') {
		g.force_emit_fn_names['error'] = true
		g.called_fn_names['error'] = true
		opt_tmp := '_chopt_${g.tmp_counter}'
		val_tmp := '_chval_${g.tmp_counter}'
		g.sb.write_string('({ ${expr_type} ${opt_tmp} = (${expr_type}){ .state = 2, .err = error((string){.str = "channel closed", .len = sizeof("channel closed") - 1, .is_lit = 1}) }; ${elem_type} ${val_tmp} = ${zero_value_for_type(elem_type)}; if (sync__Channel__try_pop_priv((sync__Channel*)')
		g.expr(node.expr)
		g.sb.write_string(', &${val_tmp}, false) == ChanState__success) { _option_ok(&${val_tmp}, (_option*)&${opt_tmp}, sizeof(${val_tmp})); } ${opt_tmp}; })')
		return true
	}
	val_tmp := '_chval_${g.tmp_counter}'
	g.sb.write_string('({ ${elem_type} ${val_tmp} = ${zero_value_for_type(elem_type)}; sync__Channel__try_pop_priv((sync__Channel*)')
	g.expr(node.expr)
	g.sb.write_string(', &${val_tmp}, false); ${val_tmp}; })')
	return true
}

fn (mut g Gen) gen_unwrapped_value_expr(expr ast.Expr) bool {
	expr_type := g.get_expr_type(expr)
	if expr_type.starts_with('_result_') {
		base := g.result_value_type(expr_type)
		if g.gen_unwrapped_payload_expr(expr, expr_type, base) {
			return true
		}
	}
	if expr_type.starts_with('_option_') {
		base := option_value_type(expr_type)
		if g.gen_unwrapped_payload_expr(expr, expr_type, base) {
			return true
		}
	}
	return false
}

fn (mut g Gen) gen_unwrapped_payload_expr(expr ast.Expr, wrapper_type string, payload_type string) bool {
	if payload_type == '' || payload_type == 'void' {
		return false
	}
	is_addressable := match expr {
		ast.Ident, ast.SelectorExpr, ast.IndexExpr {
			true
		}
		else {
			false
		}
	}

	if is_addressable {
		g.sb.write_string('(*(${payload_type}*)(((u8*)(&')
		g.expr(expr)
		g.sb.write_string('.err)) + sizeof(IError)))')
	} else {
		g.sb.write_string('({ ${wrapper_type} _tmp = ')
		g.expr(expr)
		g.sb.write_string('; (*(${payload_type}*)(((u8*)(&_tmp.err)) + sizeof(IError))); })')
	}
	return true
}

fn (g &Gen) should_use_known_enum_field(enum_name string, field_name string) bool {
	return enum_name != '' && !is_generic_placeholder_c_type_name(enum_name)
		&& g.enum_has_field(enum_name, field_name)
}

fn (mut g Gen) enum_type_from_expr(expr ast.Expr, fallback string) string {
	mut enum_type := fallback.trim_space().trim_right('*')
	if enum_type != '' && enum_type != 'int' && !is_generic_placeholder_c_type_name(enum_type)
		&& g.is_enum_type(enum_type) {
		return enum_type
	}
	if raw_type := g.get_raw_type(expr) {
		enum_type = g.types_type_to_c(raw_type).trim_space().trim_right('*')
		if enum_type != '' && enum_type != 'int' && !is_generic_placeholder_c_type_name(enum_type)
			&& g.is_enum_type(enum_type) {
			return enum_type
		}
	}
	if env_type := g.get_expr_type_from_env(expr) {
		enum_type = env_type.trim_space().trim_right('*')
		if enum_type != '' && enum_type != 'int' && !is_generic_placeholder_c_type_name(enum_type)
			&& g.is_enum_type(enum_type) {
			return enum_type
		}
	}
	cast_type := extract_compare_cast_type(expr).trim_space().trim_right('*')
	if cast_type != '' && cast_type != 'int' && !is_generic_placeholder_c_type_name(cast_type)
		&& g.is_enum_type(cast_type) {
		return cast_type
	}
	return ''
}

fn (mut g Gen) gen_enum_shorthand_compare(node &ast.InfixExpr, lhs_type string, rhs_type string) bool {
	if node.op !in [.eq, .ne] {
		return false
	}
	if node.rhs is ast.SelectorExpr {
		rhs_sel := node.rhs as ast.SelectorExpr
		if rhs_sel.lhs is ast.EmptyExpr {
			enum_type := g.enum_type_from_expr(node.lhs, lhs_type)
			if enum_type != '' && g.enum_has_field(enum_type, rhs_sel.rhs.name) {
				g.sb.write_string('(')
				g.expr(node.lhs)
				g.sb.write_string(if node.op == .eq { ' == ' } else { ' != ' })
				g.sb.write_string(g.enum_member_c_name(enum_type, rhs_sel.rhs.name))
				g.sb.write_string(')')
				return true
			}
		}
	}
	if node.lhs is ast.SelectorExpr {
		lhs_sel := node.lhs as ast.SelectorExpr
		if lhs_sel.lhs is ast.EmptyExpr {
			enum_type := g.enum_type_from_expr(node.rhs, rhs_type)
			if enum_type != '' && g.enum_has_field(enum_type, lhs_sel.rhs.name) {
				g.sb.write_string('(')
				g.sb.write_string(g.enum_member_c_name(enum_type, lhs_sel.rhs.name))
				g.sb.write_string(if node.op == .eq { ' == ' } else { ' != ' })
				g.expr(node.rhs)
				g.sb.write_string(')')
				return true
			}
		}
	}
	return false
}

fn (mut g Gen) gen_infix_expr(node &ast.InfixExpr) {
	lhs_type := g.get_expr_type(node.lhs)
	rhs_type := g.get_expr_type(node.rhs)
	// Channel push: ch <- value → sync__Channel__try_push_priv(ch, &(elem_type){value}, false)
	if node.op == .arrow {
		mut elem_type := g.channel_elem_type_from_expr(node.lhs) or { 'bool' }
		if elem_type == '' || elem_type == 'int' {
			elem_type = 'bool'
		}
		g.force_emit_fn_names['sync__Channel__try_push_priv'] = true
		g.called_fn_names['sync__Channel__try_push_priv'] = true
		g.sb.write_string('sync__Channel__try_push_priv((sync__Channel*)')
		g.expr(node.lhs)
		g.sb.write_string(', &(${elem_type}[]){')
		g.expr(node.rhs)
		g.sb.write_string('}, false)')
		return
	}
	if g.gen_enum_shorthand_compare(node, lhs_type, rhs_type) {
		return
	}
	if node.op in [.logical_or, .and] {
		g.sb.write_string('(')
		if !g.gen_unwrapped_value_expr(node.lhs) {
			g.expr(node.lhs)
		}
		g.sb.write_string(if node.op == .logical_or { ' || ' } else { ' && ' })
		if !g.gen_unwrapped_value_expr(node.rhs) {
			g.expr(node.rhs)
		}
		g.sb.write_string(')')
		return
	}
	if node.op in [.amp, .pipe, .xor] && node.lhs is ast.InfixExpr {
		left := node.lhs as ast.InfixExpr
		if left.op == .left_shift {
			is_array_append, elem_type := g.array_append_elem_type(left.lhs, left.rhs)
			if is_array_append {
				combined_rhs := ast.InfixExpr{
					lhs: left.rhs
					op:  node.op
					rhs: node.rhs
					pos: node.pos
				}
				g.sb.write_string('array__push((array*)')
				if g.expr_is_pointer(left.lhs) {
					g.expr(left.lhs)
				} else {
					g.sb.write_string('&')
					g.expr(left.lhs)
				}
				g.sb.write_string(', ')
				g.gen_addr_of_expr(ast.Expr(combined_rhs), elem_type)
				g.sb.write_string(')')
				return
			}
		}
	}
	// Option none comparison: `opt == none` / `opt != none`.
	if node.op in [.eq, .ne] {
		if lhs_type.starts_with('_option_') && is_none_like_expr(node.rhs) {
			sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
			cmp := if node.op == .eq { '!=' } else { '==' }
			g.sb.write_string('(')
			g.expr(node.lhs)
			g.sb.write_string('${sep}state ${cmp} 0)')
			return
		}
		if rhs_type.starts_with('_option_') && is_none_like_expr(node.lhs) {
			sep := if g.expr_is_pointer(node.rhs) { '->' } else { '.' }
			cmp := if node.op == .eq { '!=' } else { '==' }
			g.sb.write_string('(')
			g.expr(node.rhs)
			g.sb.write_string('${sep}state ${cmp} 0)')
			return
		}
	}
	if node.op in [.eq, .ne, .key_is, .not_is] && ((node.rhs is ast.Ident && node.rhs.name == 'Eof')
		|| (node.rhs is ast.SelectorExpr && node.rhs.rhs.name == 'Eof')) {
		if node.op in [.ne, .not_is] {
			g.sb.write_string('!')
		}
		g.sb.write_string('string__eq(err.type_name(err._object), ${c_static_v_string_expr('Eof')})')
		return
	}
	if node.op in [.eq, .ne, .key_is, .not_is] && node.lhs is ast.Ident && node.lhs.name == 'err'
		&& node.rhs is ast.Ident {
		rhs_ident := node.rhs as ast.Ident
		if rhs_ident.name.len > 0 && rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z` {
			if node.op in [.ne, .not_is] {
				g.sb.write_string('!')
			}
			type_name := rhs_ident.name
			g.sb.write_string('string__eq(err.type_name(err._object), ${c_static_v_string_expr(type_name)})')
			return
		}
	}
	if node.op in [.eq, .ne, .key_is, .not_is] && lhs_type == 'IError' && node.rhs is ast.Ident {
		rhs_ident := node.rhs as ast.Ident
		if g.is_type_name(rhs_ident.name)
			|| (rhs_ident.name.len > 0 && rhs_ident.name[0] >= `A` && rhs_ident.name[0] <= `Z`) {
			if node.op in [.ne, .not_is] {
				g.sb.write_string('!')
			}
			type_name := rhs_ident.name
			g.sb.write_string('string__eq(')
			g.expr(node.lhs)
			g.sb.write_string('.type_name(')
			g.expr(node.lhs)
			g.sb.write_string('._object), ${c_static_v_string_expr(type_name)})')
			return
		}
	}
	if node.op in [.key_is, .not_is, .eq, .ne] {
		if raw_type := g.get_raw_type(node.lhs) {
			is_iface := raw_type is types.Interface
				|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
			if is_iface {
				mut rhs_name := ''
				if node.rhs is ast.Ident {
					rhs_name = node.rhs.name
				} else if node.rhs is ast.SelectorExpr {
					rhs_name = node.rhs.rhs.name
				}
				type_id := interface_type_id_for_name(rhs_name)
				if type_id <= 0 {
					g.sb.write_string(if node.op in [.key_is, .eq] {
						'false'
					} else {
						'true'
					})
					return
				}
				sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
				g.sb.write_string('(')
				g.expr(node.lhs)
				op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
				g.sb.write_string('${sep}_type_id ${op} ${type_id})')
				return
			}
		}
	}
	// Sum type checks: expr is Type and lowered expr ==/!= Type.
	rhs_can_match_sum := node.rhs is ast.Ident
		|| (node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident)
		|| node.rhs is ast.Type
	if node.op in [.key_is, .not_is] || (node.op in [.eq, .ne] && rhs_can_match_sum) {
		sum_lhs := strip_expr_wrappers(node.lhs)
		mut rhs_name := ''
		if node.rhs is ast.Ident {
			rhs_name = node.rhs.name
		} else if node.rhs is ast.SelectorExpr && node.rhs.lhs is ast.Ident {
			rhs_name = '${(node.rhs.lhs as ast.Ident).name}__${node.rhs.rhs.name}'
		} else if node.rhs is ast.Type {
			rhs_name = g.expr_type_to_c(node.rhs)
		}
		if rhs_name != '' {
			mut lhs_sum_type := g.get_expr_type(sum_lhs)
			if raw_lhs := g.get_raw_type(sum_lhs) {
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
				if lhs_env_type := g.get_expr_type_from_env(sum_lhs) {
					lhs_sum_type = lhs_env_type
				}
			}
			if lhs_sum_type == '' && sum_lhs is ast.SelectorExpr {
				lhs_sum_type = g.selector_field_type(sum_lhs)
			}
			lhs_sum_type = lhs_sum_type.trim_space().trim_right('*')
			if sum_lhs is ast.SelectorExpr {
				storage_sum_type := g.selector_storage_field_type(sum_lhs).trim_right('*')
				if storage_sum_type != '' && storage_sum_type != lhs_sum_type
					&& g.get_sum_type_variants_for(storage_sum_type).len > 0 {
					if path := g.sum_variant_tag_path(storage_sum_type, rhs_name, []string{}) {
						g.gen_sum_variant_tag_path_check(sum_lhs, path, node.op in [
							.key_is,
							.eq,
						])
						return
					}
				}
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
				}
			}
			if variants.len == 0 && sum_lhs is ast.SelectorExpr {
				lhs_sum_type = g.selector_field_type(sum_lhs).trim_space().trim_right('*')
				if lhs_sum_type != '' {
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
						}
					}
				}
			}
			if variants.len > 0 {
				mut tag := -1
				for i, v in variants {
					if sum_type_variant_matches(v, rhs_name) {
						tag = i
						break
					}
				}
				if tag >= 0 {
					sep := if g.expr_is_pointer(sum_lhs) { '->' } else { '.' }
					op := if node.op in [.key_is, .eq] { '==' } else { '!=' }
					g.sb.write_string('(')
					g.expr(sum_lhs)
					g.sb.write_string('${sep}_tag ${op} ${tag})')
					return
				}
				if path := g.sum_variant_tag_path(lhs_sum_type, rhs_name, []string{}) {
					g.gen_sum_variant_tag_path_check(sum_lhs, path, node.op in [
						.key_is,
						.eq,
					])
					return
				}
			}
			// Fallback: interface is-check when get_raw_type didn't resolve
			// (e.g. through smartcast chains like child.layout is Widget)
			if node.op in [.key_is, .not_is] && variants.len == 0 {
				mut field_type := lhs_sum_type
				if field_type == '' && sum_lhs is ast.SelectorExpr {
					field_type = g.selector_field_type(sum_lhs)
				}
				if field_type != '' {
					base_ft := field_type.trim_right('*')
					if g.is_interface_type(base_ft) {
						type_id := interface_type_id_for_name(rhs_name)
						if type_id > 0 {
							sep := if g.expr_is_pointer(sum_lhs) { '->' } else { '.' }
							op := if node.op == .key_is { '==' } else { '!=' }
							g.sb.write_string('(')
							g.expr(sum_lhs)
							g.sb.write_string('${sep}_type_id ${op} ${type_id})')
							return
						}
					}
				}
			}
		}
	}
	if node.op == .left_shift {
		is_array_append, elem_type := g.array_append_elem_type(node.lhs, node.rhs)
		if is_array_append {
			if node.lhs is ast.IndexExpr
				&& g.gen_map_index_array_append(node.lhs, node.rhs, elem_type) {
				return
			}
			if g.expr_is_array_value(node.rhs) {
				rhs_tmp := '_arr_append_tmp_${g.tmp_counter}'
				g.tmp_counter++
				arr_rhs_type := g.expr_array_runtime_type(node.rhs)
				g.sb.write_string('({ ${arr_rhs_type} ${rhs_tmp} = ')
				g.expr(node.rhs)
				g.sb.write_string('; array__push_many((array*)')
				g.gen_array_append_target(node.lhs)
				g.sb.write_string(', ${rhs_tmp}.data, ${rhs_tmp}.len); })')
				return
			}
			g.sb.write_string('array__push((array*)')
			g.gen_array_append_target(node.lhs)
			g.sb.write_string(', ')
			if elem_type == 'string' {
				g.sb.write_string('&(string[1]){ string__clone(')
				g.expr(node.rhs)
				g.sb.write_string(') }')
			} else {
				g.gen_array_push_elem_arg(node.rhs, elem_type)
			}
			g.sb.write_string(')')
			return
		}
	}
	if node.op == .plus && lhs_type == 'string' && rhs_type == 'string' {
		g.sb.write_string('string__plus(')
		g.expr(node.lhs)
		g.sb.write_string(', ')
		g.expr(node.rhs)
		g.sb.write_string(')')
		return
	}
	if node.op in [.key_in, .not_in] {
		if node.rhs is ast.ArrayInitExpr {
			join_op := if node.op == .key_in { ' || ' } else { ' && ' }
			g.sb.write_string('(')
			if node.rhs.exprs.len == 0 {
				g.sb.write_string(if node.op == .key_in { 'false' } else { 'true' })
			} else {
				for i in 0 .. node.rhs.exprs.len {
					elem := node.rhs.exprs[i]
					if i > 0 {
						g.sb.write_string(join_op)
					}
					if lhs_type == 'string' {
						if node.op == .not_in {
							g.sb.write_string('!')
						}
						g.sb.write_string('string__eq(')
						g.expr(node.lhs)
						g.sb.write_string(', ')
						g.expr(elem)
						g.sb.write_string(')')
					} else {
						cmp_op := if node.op == .key_in { '==' } else { '!=' }
						g.sb.write_string('(')
						g.expr(node.lhs)
						g.sb.write_string(' ${cmp_op} ')
						g.expr(elem)
						g.sb.write_string(')')
					}
				}
			}
			g.sb.write_string(')')
			return
		}
		if rhs_type == 'map' || rhs_type.starts_with('Map_') {
			mut key_type := if lhs_type == '' { 'int' } else { lhs_type }
			if raw_map_type := g.get_raw_type(node.rhs) {
				if raw_map_type is types.Map {
					kt := g.types_type_to_c(raw_map_type.key_type)
					if kt != '' {
						key_type = kt
					}
				}
			} else if rhs_type.starts_with('Map_') {
				kv := rhs_type.all_after('Map_')
				kt, _ := g.parse_map_kv_types(kv)
				if kt != '' {
					key_type = kt
				}
			}
			if key_type == 'bool' && node.lhs is ast.BasicLiteral {
				key_type = 'int'
			}
			if node.op == .not_in {
				g.sb.write_string('!')
			}
			if node.rhs is ast.Ident || node.rhs is ast.SelectorExpr {
				g.sb.write_string('map__exists(&')
				g.expr(node.rhs)
				g.sb.write_string(', ')
				g.gen_addr_of_expr(node.lhs, key_type)
				g.sb.write_string(')')
			} else {
				tmp_name := '_map_in_tmp_${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ map ${tmp_name} = ')
				g.expr(node.rhs)
				g.sb.write_string('; map__exists(&${tmp_name}, ')
				g.gen_addr_of_expr(node.lhs, key_type)
				g.sb.write_string('); })')
			}
			return
		}
		if rhs_type.starts_with('Array_fixed_') {
			// Fixed-size array: use a linear scan via statement expression.
			tmp := '_fixed_in_${g.tmp_counter}'
			g.tmp_counter++
			g.sb.write_string('({ bool ${tmp} = false; for (int _i = 0; _i < (int)(sizeof(')
			g.expr(node.rhs)
			g.sb.write_string(')/sizeof(')
			g.expr(node.rhs)
			g.sb.write_string('[0])); _i++) { if (')
			g.expr(node.rhs)
			g.sb.write_string('[_i] == ')
			g.expr(node.lhs)
			g.sb.write_string(') { ${tmp} = true; break; } } ')
			if node.op == .not_in {
				g.sb.write_string('!${tmp}')
			} else {
				g.sb.write_string(tmp)
			}
			g.sb.write_string('; })')
			return
		}
		if rhs_type == 'array' || rhs_type.starts_with('Array_') {
			key_type := if lhs_type == '' { 'int' } else { lhs_type }
			if node.op == .not_in {
				g.sb.write_string('!')
			}
			g.sb.write_string('array__contains(')
			g.expr(node.rhs)
			g.sb.write_string(', ')
			g.gen_addr_of_expr(node.lhs, key_type)
			g.sb.write_string(')')
			return
		}
		cmp_op := if node.op == .key_in { '==' } else { '!=' }
		g.sb.write_string('(')
		g.expr(node.lhs)
		g.sb.write_string(' ${cmp_op} ')
		g.expr(node.rhs)
		g.sb.write_string(')')
		return
	}
	is_lhs_fixed := lhs_type.starts_with('Array_fixed_')
	is_rhs_fixed := rhs_type.starts_with('Array_fixed_')
	if node.op in [.eq, .ne] && (is_lhs_fixed || is_rhs_fixed) {
		fixed_type := if is_lhs_fixed { lhs_type } else { rhs_type }
		cmp_op := if node.op == .eq { '==' } else { '!=' }
		g.sb.write_string('(memcmp(')
		g.gen_fixed_array_cmp_operand(node.lhs, fixed_type)
		g.sb.write_string(', ')
		g.gen_fixed_array_cmp_operand(node.rhs, fixed_type)
		g.sb.write_string(', sizeof(${fixed_type})) ${cmp_op} 0)')
		return
	}
	is_lhs_array := lhs_type == 'array'
		|| (lhs_type.starts_with('Array_') && !lhs_type.starts_with('Array_fixed_'))
		|| lhs_type in g.array_aliases
	is_rhs_array := rhs_type == 'array'
		|| (rhs_type.starts_with('Array_') && !rhs_type.starts_with('Array_fixed_'))
		|| rhs_type in g.array_aliases
	if node.op in [.eq, .ne] && is_lhs_array && is_rhs_array {
		if node.op == .ne {
			g.sb.write_string('!')
		}
		g.sb.write_string('__v2_array_eq(')
		if g.expr_is_pointer(node.lhs) {
			g.sb.write_string('*')
		}
		g.expr(node.lhs)
		g.sb.write_string(', ')
		if g.expr_is_pointer(node.rhs) {
			g.sb.write_string('*')
		}
		g.expr(node.rhs)
		g.sb.write_string(')')
		return
	}
	mut lhs_is_string_ptr := false
	lhs_local_type := g.local_var_c_type_for_expr(node.lhs) or { '' }
	if raw_type := g.get_raw_type(node.lhs) {
		if raw_type is types.Pointer && raw_type.base_type is types.String {
			lhs_is_string_ptr = true
		}
	}
	if lhs_type in ['string*', 'stringptr'] || lhs_local_type in ['string*', 'stringptr'] {
		lhs_is_string_ptr = true
	}
	mut rhs_is_string_ptr := false
	rhs_local_type := g.local_var_c_type_for_expr(node.rhs) or { '' }
	if raw_type := g.get_raw_type(node.rhs) {
		if raw_type is types.Pointer && raw_type.base_type is types.String {
			rhs_is_string_ptr = true
		}
	}
	if rhs_type in ['string*', 'stringptr'] || rhs_local_type in ['string*', 'stringptr'] {
		rhs_is_string_ptr = true
	}
	// When either side is nil/0, this is a pointer comparison, not a string comparison
	lhs_is_nil := is_nil_like_expr(node.lhs)
	rhs_is_nil := is_nil_like_expr(node.rhs)
	// Also treat integer-typed operands as nil when compared with string pointers
	// (e.g., `&string == 0` where checker annotates 0 as int)
	lhs_is_nil2 := lhs_is_nil
		|| (rhs_is_string_ptr && lhs_type in ['int', 'int_literal', 'i64', 'u64', 'voidptr'])
	rhs_is_nil2 := rhs_is_nil
		|| (lhs_is_string_ptr && rhs_type in ['int', 'int_literal', 'i64', 'u64', 'voidptr'])
	is_string_cmp := if lhs_is_nil2 || rhs_is_nil2 {
		false
	} else if node.lhs is ast.StringLiteral || node.rhs is ast.StringLiteral {
		true
	} else {
		(lhs_type == 'string' || lhs_is_string_ptr) && (rhs_type == 'string' || rhs_is_string_ptr)
			&& !g.is_enum_type(lhs_type) && !g.is_enum_type(rhs_type)
	}
	// Override: string pointer compared with a numeric literal (null check)
	// The checker may annotate `0` as `string` in `&string == 0` context,
	// but this is a pointer comparison, not a string comparison.
	is_string_cmp2 := if is_string_cmp && ((lhs_is_string_ptr && is_numeric_literal(node.rhs))
		|| (rhs_is_string_ptr && is_numeric_literal(node.lhs))) {
		false
	} else {
		is_string_cmp
	}
	if node.op in [.eq, .ne] && is_string_cmp2 {
		if node.op == .ne {
			g.sb.write_string('!')
		}
		g.sb.write_string('string__eq(')
		g.gen_string_cmp_operand(node.lhs, lhs_is_string_ptr)
		g.sb.write_string(', ')
		g.gen_string_cmp_operand(node.rhs, rhs_is_string_ptr)
		g.sb.write_string(')')
		return
	}
	if node.op in [.lt, .le, .gt, .ge] && is_string_cmp2 {
		op := match node.op {
			.gt { '>' }
			.lt { '<' }
			.ge { '>=' }
			.le { '<=' }
			else { '==' }
		}

		g.sb.write_string('(string__compare(')
		g.gen_string_cmp_operand(node.lhs, lhs_is_string_ptr)
		g.sb.write_string(', ')
		g.gen_string_cmp_operand(node.rhs, rhs_is_string_ptr)
		g.sb.write_string(') ${op} 0)')
		return
	}
	if node.op in [.eq, .ne] && (lhs_type == 'map' || lhs_type.starts_with('Map_'))
		&& (rhs_type == 'map' || rhs_type.starts_with('Map_')) {
		if node.op == .ne {
			g.sb.write_string('!')
		}
		map_eq_fn := if lhs_type.starts_with('Map_') && lhs_type == rhs_type {
			'${lhs_type}_map_eq'
		} else {
			'map_map_eq'
		}
		g.sb.write_string('${map_eq_fn}(')
		g.expr(node.lhs)
		g.sb.write_string(', ')
		g.expr(node.rhs)
		g.sb.write_string(')')
		return
	}
	mut cmp_type := ''
	if g.should_use_memcmp_eq(lhs_type, rhs_type) {
		// Use whichever type is non-empty; prefer lhs_type
		cmp_type = if lhs_type != '' { lhs_type } else { rhs_type }
	} else if node.op in [.eq, .ne] && g.should_use_memcmp_eq(lhs_type, lhs_type)
		&& rhs_type == 'int' && node.rhs is ast.Ident && g.is_known_struct_type(lhs_type) {
		// RHS Ident defaulted to 'int' (unresolved constant) but LHS is a known struct
		cmp_type = lhs_type
	} else if node.op in [.eq, .ne] && g.should_use_memcmp_eq(rhs_type, rhs_type)
		&& lhs_type == 'int' && node.lhs is ast.Ident && g.is_known_struct_type(rhs_type) {
		// LHS Ident defaulted to 'int' (unresolved constant) but RHS is a known struct
		cmp_type = rhs_type
	} else if node.op in [.eq, .ne] {
		lhs_cast_type := extract_compare_cast_type(node.lhs)
		rhs_cast_type := extract_compare_cast_type(node.rhs)
		if lhs_cast_type != '' && rhs_cast_type == '' {
			cmp_type = lhs_cast_type
		} else if rhs_cast_type != '' && lhs_cast_type == '' {
			cmp_type = rhs_cast_type
		} else if lhs_cast_type != '' && lhs_cast_type == rhs_cast_type {
			cmp_type = lhs_cast_type
		}
		if cmp_type in primitive_types || cmp_type == 'string' || cmp_type.ends_with('*')
			|| cmp_type.ends_with('ptr') {
			cmp_type = ''
		}
	}
	if node.op in [.eq, .ne] && cmp_type != '' {
		ltmp := '_cmp_l_${g.tmp_counter}'
		g.tmp_counter++
		rtmp := '_cmp_r_${g.tmp_counter}'
		g.tmp_counter++
		g.sb.write_string('({ ${cmp_type} ${ltmp} = ')
		if !g.gen_unwrapped_value_expr(node.lhs) {
			g.expr(node.lhs)
		}
		g.sb.write_string('; ${cmp_type} ${rtmp} = ')
		if !g.gen_unwrapped_value_expr(node.rhs) {
			g.expr(node.rhs)
		}
		struct_type := g.lookup_struct_type_by_c_name(cmp_type)
		if struct_type.fields.len > 0 && g.struct_has_ref_fields(struct_type) {
			eq_expr := g.gen_struct_field_eq_expr(struct_type, ltmp, rtmp)
			if node.op == .eq {
				g.sb.write_string('; ${eq_expr}; })')
			} else {
				g.sb.write_string('; !(${eq_expr}); })')
			}
		} else {
			cmp_op := if node.op == .eq { '== 0' } else { '!= 0' }
			g.sb.write_string('; memcmp(&${ltmp}, &${rtmp}, sizeof(${cmp_type})) ${cmp_op}; })')
		}
		return
	}
	method_fn, operand_type := g.overloaded_arithmetic_method_for_infix(*node, lhs_type, rhs_type)
	if method_fn != '' {
		g.sb.write_string('${method_fn}(')
		g.gen_overloaded_arithmetic_arg(node.lhs, lhs_type, operand_type)
		g.sb.write_string(', ')
		g.gen_overloaded_arithmetic_arg(node.rhs, rhs_type, operand_type)
		g.sb.write_string(')')
		return
	}
	// Comparison operators (<, <=, >, >=) on struct types with overloaded `<` operator.
	// V derives: a < b → Type__lt(a, b), a > b → Type__lt(b, a),
	//            a <= b → !Type__lt(b, a), a >= b → !Type__lt(a, b)
	if node.op in [.lt, .le, .gt, .ge] && lhs_type != '' && rhs_type != '' && lhs_type == rhs_type
		&& lhs_type !in primitive_types && lhs_type != 'string' && !lhs_type.ends_with('*')
		&& !lhs_type.ends_with('ptr') {
		lt_fn := '${lhs_type}__lt'
		if lt_fn in g.fn_return_types || lt_fn in g.fn_param_is_ptr {
			match node.op {
				.lt {
					g.sb.write_string('${lt_fn}(')
					g.expr(node.lhs)
					g.sb.write_string(', ')
					g.expr(node.rhs)
					g.sb.write_string(')')
				}
				.gt {
					g.sb.write_string('${lt_fn}(')
					g.expr(node.rhs)
					g.sb.write_string(', ')
					g.expr(node.lhs)
					g.sb.write_string(')')
				}
				.le {
					g.sb.write_string('!${lt_fn}(')
					g.expr(node.rhs)
					g.sb.write_string(', ')
					g.expr(node.lhs)
					g.sb.write_string(')')
				}
				.ge {
					g.sb.write_string('!${lt_fn}(')
					g.expr(node.lhs)
					g.sb.write_string(', ')
					g.expr(node.rhs)
					g.sb.write_string(')')
				}
				else {}
			}

			return
		}
	}
	is_bitwise_op := node.op in [.amp, .pipe, .xor, .left_shift, .right_shift]
	lhs_is_float := lhs_type.starts_with('f') || lhs_type == 'float_literal'
	rhs_is_float := rhs_type.starts_with('f') || rhs_type == 'float_literal'
	if node.op == .mod && (lhs_is_float || rhs_is_float) {
		g.sb.write_string(if lhs_type == 'f32' || rhs_type == 'f32' { 'fmodf(' } else { 'fmod(' })
		if !g.gen_unwrapped_value_expr(node.lhs) {
			g.expr(node.lhs)
		}
		g.sb.write_string(', ')
		if !g.gen_unwrapped_value_expr(node.rhs) {
			g.expr(node.rhs)
		}
		g.sb.write_string(')')
		return
	}
	g.sb.write_string('(')
	if is_bitwise_op && lhs_is_float {
		g.sb.write_string('((u64)(')
		if !g.gen_unwrapped_value_expr(node.lhs) {
			g.expr(node.lhs)
		}
		g.sb.write_string('))')
	} else {
		if !g.gen_unwrapped_value_expr(node.lhs) {
			g.expr(node.lhs)
		}
	}
	op := match node.op {
		.plus { '+' }
		.minus { '-' }
		.mul { '*' }
		.div { '/' }
		.mod { '%' }
		.gt { '>' }
		.lt { '<' }
		.eq { '==' }
		.ne { '!=' }
		.ge { '>=' }
		.le { '<=' }
		.and { '&&' }
		.logical_or { '||' }
		.amp { '&' }
		.pipe { '|' }
		.xor { '^' }
		.left_shift { '<<' }
		.right_shift { '>>' }
		.key_is { '==' }
		.not_is { '!=' }
		.question { '==' } // match arm lowering uses ? as equality test
		else { '==' }
	}

	g.sb.write_string(' ${op} ')
	if is_bitwise_op && rhs_is_float {
		cast_type := if node.op in [.left_shift, .right_shift] {
			'int'
		} else {
			'u64'
		}
		g.sb.write_string('((${cast_type})(')
		if !g.gen_unwrapped_value_expr(node.rhs) {
			g.expr(node.rhs)
		}
		g.sb.write_string('))')
	} else {
		if !g.gen_unwrapped_value_expr(node.rhs) {
			g.expr(node.rhs)
		}
	}
	g.sb.write_string(')')
}

fn (mut g Gen) gen_string_cmp_operand(expr ast.Expr, is_string_ptr bool) {
	if is_string_ptr {
		g.sb.write_string('(*')
		g.expr(expr)
		g.sb.write_string(')')
		return
	}
	g.expr(expr)
}

// Helper to extract FnType from an Expr (handles ast.Type wrapping)
fn (mut g Gen) expr(node ast.Expr) {
	if !expr_has_valid_data(node) {
		g.sb.write_string('0 /* corrupt expr */')
		return
	}
	if node is ast.CallExpr && g.try_gen_orm_create_call_expr(node) {
		return
	}
	_ = g.get_expr_type(node)
	match node {
		ast.BasicLiteral {
			if node.kind == .key_true {
				g.sb.write_string('true')
			} else if node.kind == .key_false {
				g.sb.write_string('false')
			} else if node.kind == .char {
				mut raw_value := ''
				raw_value = strip_literal_quotes(node.value)
				if raw_value.len > 1 && raw_value[0] != `\\` {
					// Multi-byte UTF-8 character: emit as numeric codepoint
					runes := raw_value.runes()
					if runes.len > 0 {
						g.sb.write_string(int(runes[0]).str())
					} else {
						g.sb.write_string("'${raw_value}'")
					}
				} else {
					escaped := escape_char_literal_content(raw_value)
					g.sb.write_u8(`'`)
					g.sb.write_string(escaped)
					g.sb.write_u8(`'`)
				}
			} else {
				g.sb.write_string(sanitize_c_number_literal(node.value))
			}
		}
		ast.StringLiteral {
			mut val := strip_literal_quotes(node.value)
			if node.kind == .raw {
				// Raw strings: backslash is literal, escape it for C
				val = val.replace('\\', '\\\\')
			} else {
				// Process V line continuations: `\` + newline + whitespace → strip
				val = process_line_continuations(val)
			}
			c_lit := c_string_literal_content_to_c(val)
			if node.kind == .c {
				// C string literal: emit raw C string
				g.sb.write_string(c_lit)
			} else {
				// Use sizeof on the emitted C literal so escape sequences
				// (e.g. `\t`) get the correct runtime byte length.
				g.sb.write_string(c_static_v_string_expr_from_c_literal(c_lit))
			}
		}
		ast.LifetimeExpr {
			g.sb.write_string('lt__')
			g.sb.write_string(node.name)
		}
		ast.Ident {
			g.mark_needed_ierror_wrapper_from_ident(node.name)
			if node.name == 'nil' {
				g.sb.write_string('NULL')
			} else if node.name == '@FN' || node.name == '@METHOD' || node.name == '@FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr(fn_name))
			} else if node.name == '@LOCATION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr('${g.cur_module}.${fn_name}'))
			} else if node.name == '@MOD' {
				mod_name := g.cur_module
				g.sb.write_string(c_static_v_string_expr(mod_name))
			} else if node.name == '@FILE' {
				g.sb.write_string(c_v_string_expr_from_ptr_len('__FILE__', 'sizeof(__FILE__)-1',
					true))
			} else if node.name == '@LINE' {
				g.sb.write_string('__LINE__')
			} else if node.name == '@VCURRENTHASH' || node.name == '@VHASH' {
				g.sb.write_string(c_static_v_string_expr(g.get_v_hash()))
			} else if node.name == '@VEXE' {
				g.sb.write_string(c_empty_v_string_expr())
			} else if node.name == '@VEXEROOT' || node.name == '@VROOT' {
				vroot := if g.pref != unsafe { nil } { g.pref.vroot } else { '' }
				g.sb.write_string(c_static_v_string_expr(vroot))
			} else if node.name.starts_with('__type_id_') {
				type_name := node.name['__type_id_'.len..]
				type_id := interface_type_id_for_name(type_name)
				g.sb.write_string('${type_id}')
			} else {
				is_local_var := g.get_local_var_c_type(node.name) != none
				mut handled_ident := false
				if !is_local_var {
					if c_name := g.renamed_const_name_for_ident(node.name) {
						g.sb.write_string(c_name)
						handled_ident = true
					}
				}
				if handled_ident {
					return
				}
				const_key := 'const_${g.cur_module}__${node.name}'
				global_key := 'global_${g.cur_module}__${node.name}'
				module_storage_name := module_storage_c_name(g.cur_module, node.name)
				if !is_local_var && node.name in g.global_var_types
					&& module_storage_name !in g.global_var_types {
					g.sb.write_string(node.name)
				} else if !is_local_var && module_storage_name in g.c_extern_module_storage {
					g.sb.write_string(g.c_extern_module_storage[module_storage_name])
				} else if !is_local_var && module_storage_name in g.module_storage_vars {
					g.sb.write_string(module_storage_name)
				} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
					&& !node.name.contains('__') && !is_local_var
					&& ((const_key in g.emitted_types || global_key in g.emitted_types)
					|| g.is_module_local_const_or_global(node.name)) {
					g.sb.write_string('${g.cur_module}__${node.name}')
				} else if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
					&& !node.name.contains('__') && !is_local_var && !g.is_module_ident(node.name)
					&& g.is_module_local_fn(node.name) && !g.is_type_name(node.name) {
					g.sb.write_string('${g.cur_module}__${sanitize_fn_ident(node.name)}')
				} else {
					mut ident_name := node.name
					if g.cur_module != '' {
						double_prefix := '${g.cur_module}__${g.cur_module}__'
						if ident_name.starts_with(double_prefix) {
							ident_name = ident_name[g.cur_module.len + 2..]
						}
					}
					g.sb.write_string(c_local_name(ident_name))
				}
			}
		}
		ast.ParenExpr {
			g.sb.write_string('(')
			g.expr(node.expr)
			g.sb.write_string(')')
		}
		ast.InfixExpr {
			g.gen_infix_expr(&node)
		}
		ast.PrefixExpr {
			if node.op == .arrow && g.gen_channel_receive_expr(node) {
				return
			}
			// &T(x) in unsafe contexts is used as a pointer cast in V stdlib code.
			// Emit it as (T*)(x) so `*unsafe { &T(p) }` becomes `*((T*)p)`.
			if node.op == .amp {
				if node.expr is ast.CastExpr {
					cast_expr := node.expr as ast.CastExpr
					expr_type := g.get_expr_type(cast_expr.expr)
					if (expr_type.starts_with('_option_') && option_value_type(expr_type) != '')
						|| (expr_type.starts_with('_result_')
						&& g.result_value_type(expr_type) != '') {
						if expr_type.starts_with('_option_')
							&& g.cur_fn_ret_type.starts_with('_option_') {
							g.sb.write_string('({ if (')
							g.expr(cast_expr.expr)
							g.sb.write_string('.state != 0) { return (${g.cur_fn_ret_type}){ .state = 2 }; } &')
							g.gen_unwrapped_value_expr(cast_expr.expr)
							g.sb.write_string('; })')
							return
						}
						g.sb.write_string('&')
						g.gen_unwrapped_value_expr(cast_expr.expr)
						return
					}
				}
				// &&T(x) is a pointer-to-pointer cast pattern used in builtin code.
				// Lower it directly to (T**)(x) instead of taking address of a cast rvalue.
				if node.expr is ast.PrefixExpr && node.expr.op == .amp {
					inner := node.expr as ast.PrefixExpr
					if inner.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(inner.expr.typ)
						g.sb.write_string('((${target_type}**)(')
						g.expr(inner.expr.expr)
						g.sb.write_string('))')
						return
					}
					if inner.expr is ast.CallOrCastExpr
						&& g.call_or_cast_lhs_is_type(inner.expr.lhs) {
						mut target_type := g.expr_type_to_c(inner.expr.lhs)
						if !target_type.ends_with('*') {
							target_type += '*'
						}
						g.sb.write_string('((${target_type}*)(')
						g.expr(inner.expr.expr)
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.IndexExpr {
					idx := node.expr as ast.IndexExpr
					if idx.lhs is ast.Ident {
						if idx.lhs.name in g.fixed_array_globals || idx.lhs.name == 'rune_maps' {
							g.sb.write_string('&')
							g.expr(idx.lhs)
							g.sb.write_string('[')
							g.expr(idx.expr)
							g.sb.write_string(']')
							return
						}
						if raw_type := g.get_raw_type(idx.lhs) {
							if raw_type is types.ArrayFixed {
								// Fixed arrays: &arr[i]
								g.sb.write_string('&')
								g.expr(idx.lhs)
								g.sb.write_string('[')
								g.expr(idx.expr)
								g.sb.write_string(']')
								return
							}
						}
						lhs_type := g.get_expr_type(idx.lhs)
						// Fixed arrays: direct C array indexing (no .data field)
						if lhs_type.starts_with('Array_fixed_') {
							g.sb.write_string('&')
							g.expr(idx.lhs)
							g.sb.write_string('[')
							g.expr(idx.expr)
							g.sb.write_string(']')
							return
						}
						if lhs_type == 'array' || lhs_type.starts_with('Array_') {
							mut elem_type := g.get_expr_type(idx)
							if elem_type == '' || elem_type == 'int' {
								if lhs_type.starts_with('Array_') {
									elem_type = lhs_type['Array_'.len..].trim_right('*')
								}
							}
							if elem_type == '' {
								elem_type = 'u8'
							}
							g.sb.write_string('&((')
							g.sb.write_string(elem_type)
							g.sb.write_string('*)')
							g.expr(idx.lhs)
							if lhs_type.ends_with('*') {
								g.sb.write_string('->data)[')
							} else {
								g.sb.write_string('.data)[')
							}
							g.expr(idx.expr)
							g.sb.write_string(']')
							return
						}
					}
				}
				addr_inner := unwrap_sum_common_field_address_base(node.expr)
				if addr_inner is ast.SelectorExpr {
					sel := addr_inner as ast.SelectorExpr
					if g.gen_sum_common_field_selector_addr(sel) {
						return
					}
				}
				if node.expr is ast.SelectorExpr {
					sel := node.expr as ast.SelectorExpr
					saved_sb := g.sb
					g.sb = strings.new_builder(128)
					has_cast_field_access := g.gen_c_pointer_cast_selector_field_access(sel)
					cast_field_access := g.sb.str()
					g.sb = saved_sb
					if has_cast_field_access {
						g.sb.write_string('&')
						g.sb.write_string(cast_field_access)
						return
					}
					c_typedef := g.c_typedef_for_interface_object_access(sel)
					if c_typedef != '' {
						g.sb.write_string('&(((${c_typedef}*)(')
						g.expr(sel.lhs)
						g.sb.write_string('))->${escape_c_keyword(sel.rhs.name)})')
						return
					}
					field_idx := vector_field_index(sel.rhs.name)
					if field_idx >= 0 {
						mut lhs_type := g.get_expr_type(sel.lhs)
						if lhs_type in ['', 'int'] {
							if raw := g.get_raw_type(sel.lhs) {
								lhs_type = g.types_type_to_c(raw)
							}
						}
						mut elem_type := vector_elem_type_for_name(lhs_type)
						if elem_type == '' && sel.lhs is ast.IndexExpr {
							base_type := g.get_expr_type(sel.lhs.lhs)
							if base_type.contains('Simd') {
								elem_type = if base_type.contains('Float') {
									'f32'
								} else if base_type.contains('Uint') || base_type.contains('U32') {
									'u32'
								} else {
									'i32'
								}
							}
						}
						if elem_type != '' {
							g.sb.write_string('(&(((')
							g.sb.write_string(elem_type)
							g.sb.write_string('*)(&(')
							g.expr(sel.lhs)
							g.sb.write_string(')))[')
							g.sb.write_string(field_idx.str())
							g.sb.write_string(']))')
							return
						}
					}
				}
				if node.expr is ast.CallExpr {
					if node.expr.args.len == 1 && node.expr.lhs is ast.Ident
						&& (g.is_type_name(node.expr.lhs.name)
						|| g.is_c_type_name(node.expr.lhs.name)
						|| node.expr.lhs.name.starts_with('cgltf_')) {
						mut target_type := g.expr_type_to_c(node.expr.lhs)
						if target_type == '' || target_type == 'int' {
							target_type = node.expr.lhs.name
						}
						// `&T(value)` where value is a struct rvalue (e.g. string) cannot
						// be lowered to `(T*)(value)` — that casts an rvalue to a pointer.
						// Emit a temporary copy so the address is valid. Only applies when
						// arg is a value (not pointer) and target_type is a non-pointer
						// struct alias compatible with the arg type.
						arg_type := g.get_expr_type(node.expr.args[0])
						if !target_type.ends_with('*') && !arg_type.ends_with('*')
							&& arg_type == 'string' && g.alias_resolves_to_string(target_type) {
							g.tmp_counter++
							tmp := '_aof${g.tmp_counter}'
							g.sb.write_string('({ ${target_type} ${tmp} = ')
							g.expr(node.expr.args[0])
							g.sb.write_string('; &${tmp}; })')
							return
						}
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.args[0])
						g.sb.write_string('))')
						return
					}
					if node.expr.args.len == 1 && node.expr.lhs is ast.SelectorExpr {
						sel := node.expr.lhs as ast.SelectorExpr
						arg := node.expr.args[0]
						if sel.lhs is ast.Ident {
							lhs_ident := sel.lhs as ast.Ident
							if lhs_ident.name == 'C' && (g.is_c_type_name(sel.rhs.name)
								|| sel.rhs.name.starts_with('cgltf_')
								|| is_none_like_expr(arg)
								|| (arg is ast.BasicLiteral && arg.value == '0')) {
								mut target_type := g.expr_type_to_c(node.expr.lhs)
								if target_type == '' || target_type == 'int' {
									target_type = sel.rhs.name
								}
								g.sb.write_string('((${target_type}*)(')
								g.expr(arg)
								g.sb.write_string('))')
								return
							}
						}
					}
				}
				if node.expr is ast.CastExpr {
					target_type := g.expr_type_to_c(node.expr.typ)
					// For interface types, generate vtable construction on the heap
					if g.gen_heap_interface_cast(target_type, node.expr.expr) {
						return
					}
					if g.gen_heap_address_of_cast_expr(node.expr, target_type) {
						return
					}
					// `&T(value)` where T aliases the value's type (e.g. NamedType=string)
					// cannot be lowered to `(T*)(value)` — that casts an rvalue to a pointer.
					// Emit a temporary copy instead. Only applies for compatible alias casts;
					// reinterpret casts of pointer args use the original `(T*)(v)` form.
					arg_type := g.get_expr_type(node.expr.expr)
					if !target_type.ends_with('*') && !arg_type.ends_with('*')
						&& arg_type == 'string' && g.alias_resolves_to_string(target_type) {
						g.tmp_counter++
						tmp := '_aof${g.tmp_counter}'
						g.sb.write_string('({ ${target_type} ${tmp} = ')
						g.expr(node.expr.expr)
						g.sb.write_string('; &${tmp}; })')
						return
					}
					g.sb.write_string('((${target_type}*)(')
					g.expr(node.expr.expr)
					g.sb.write_string('))')
					return
				}
				if node.expr is ast.CallOrCastExpr && g.call_or_cast_lhs_is_type(node.expr.lhs) {
					base_target_type := g.expr_type_to_c(node.expr.lhs)
					// `&T(value)` where T aliases the value's type (e.g. NamedType=string)
					// cannot be lowered to `(T*)(value)` — that casts an rvalue to a pointer.
					// Emit a temporary copy instead. Only applies for compatible alias casts.
					arg_type := g.get_expr_type(node.expr.expr)
					if !base_target_type.ends_with('*') && !arg_type.ends_with('*')
						&& arg_type == 'string' && g.alias_resolves_to_string(base_target_type) {
						g.tmp_counter++
						tmp := '_aof${g.tmp_counter}'
						g.sb.write_string('({ ${base_target_type} ${tmp} = ')
						g.expr(node.expr.expr)
						g.sb.write_string('; &${tmp}; })')
						return
					}
					mut target_type := base_target_type
					if !target_type.ends_with('*') {
						target_type += '*'
					}
					g.sb.write_string('((${target_type})(')
					g.expr(node.expr.expr)
					g.sb.write_string('))')
					return
				}
				if node.expr is ast.ModifierExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
				}
				if node.expr is ast.ParenExpr {
					if node.expr.expr is ast.CastExpr {
						target_type := g.expr_type_to_c(node.expr.expr.typ)
						g.sb.write_string('((${target_type}*)(')
						g.expr(node.expr.expr.expr)
						g.sb.write_string('))')
						return
					}
					if node.expr.expr is ast.CallExpr {
						if node.expr.expr.args.len == 1 {
							target_type := g.expr_type_to_c(node.expr.expr.lhs)
							g.sb.write_string('((${target_type}*)(')
							g.expr(node.expr.expr.args[0])
							g.sb.write_string('))')
							return
						}
					}
				}
			}
			// Handle &fn_call() where fn_call returns a struct (rvalue)
			// Can't take address of rvalue, use compound statement expression
			if node.op == .amp && node.expr is ast.CallExpr {
				if !(node.expr.args.len == 1 && node.expr.lhs is ast.Ident
					&& g.is_type_name(node.expr.lhs.name)) {
					// This is a function call, not a type cast
					ret_type := g.get_expr_type(node.expr)
					if ret_type != '' && ret_type != 'void' && ret_type != 'int' {
						tmp_name := '_sumtmp${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ ${ret_type} ${tmp_name} = ')
						g.expr(node.expr)
						g.sb.write_string('; &${tmp_name}; })')
						return
					}
				}
			}
			// Fixed array literal: &[1.1, 2.2]! needs type prefix for compound literal
			if node.op == .amp && node.expr is ast.ArrayInitExpr {
				if !g.is_dynamic_array_type(node.expr.typ) && node.expr.exprs.len > 0 {
					elem_type := g.extract_array_elem_type(node.expr.typ)
					if elem_type != '' {
						g.sb.write_string('&(${elem_type}[${node.expr.exprs.len}]){')
						is_iface_elem := g.is_interface_type(elem_type)
						for i in 0 .. node.expr.exprs.len {
							e := node.expr.exprs[i]
							if i > 0 {
								g.sb.write_string(', ')
							}
							if is_iface_elem && g.gen_interface_cast(elem_type, e) {
								// interface wrapping handled
							} else {
								g.expr(e)
							}
						}
						g.sb.write_string('}')
						return
					}
				}
				g.sb.write_string('&')
				g.expr(node.expr)
				return
			}
			// V `&Type{...}` must allocate on the heap.
			// Taking the address of a C compound literal here would create a dangling pointer.
			if node.op == .amp && node.expr is ast.InitExpr {
				type_name := g.expr_type_to_c(node.expr.typ)
				tmp_name := '_heap_t${g.tmp_counter}'
				g.tmp_counter++
				g.sb.write_string('({ ${type_name}* ${tmp_name} = (${type_name}*)malloc(sizeof(${type_name})); *${tmp_name} = ')
				g.expr(node.expr)
				g.sb.write_string('; ${tmp_name}; })')
				return
			}
			if node.op == .mul {
				if raw_type := g.get_raw_type(node.expr) {
					// Pointer to interface: *ptr just dereferences to get the interface struct.
					// Do NOT extract ._object — that's for smartcasts, not plain deref.
					if raw_type is types.Pointer && raw_type.base_type is types.Interface {
						g.sb.write_string('(*(')
						g.expr(node.expr)
						g.sb.write_string('))')
						return
					}
					is_iface := raw_type is types.Interface
					if is_iface {
						// If the expression is actually a pointer (e.g., mut for-in loop var
						// registered as Interface but declared as Interface*), just deref normally.
						if g.expr_is_pointer(node.expr) {
							g.sb.write_string('(*(')
							g.expr(node.expr)
							g.sb.write_string('))')
							return
						}
						target_type := g.get_expr_type(node)
						if target_type != '' && target_type != 'int' {
							g.sb.write_string('(*((')
							g.sb.write_string(target_type)
							g.sb.write_string('*)(')
							g.expr(node.expr)
							g.sb.write_string('._object)))')
							return
						}
					}
				}
			}
			if node.op == .amp {
				// &*(ptr) cancellation: when taking address of a deref of a pointer,
				// the two operations cancel out. This avoids &(rvalue) errors when
				// the deref gets null-guard expansion for sum type data pointers.
				inner := g.unwrap_parens(node.expr)
				if inner is ast.PrefixExpr && inner.op == .mul {
					if g.expr_is_pointer(inner.expr) || g.expr_produces_pointer(inner.expr) {
						g.expr(inner.expr)
						return
					}
				}
				// Generate inner expression to check if it produces a GCC statement
				// expression `({...})` which is an rvalue — can't take address of rvalue.
				saved_sb := g.sb
				g.sb = strings.new_builder(256)
				g.expr(node.expr)
				inner_code := g.sb.str()
				g.sb = saved_sb
				// Detect rvalue expressions: GCC statement expressions ({...}),
				// or null-guarded ternaries (ptr ? *ptr : (T){0}) from smartcast.
				is_rvalue := inner_code.starts_with('({') || inner_code.starts_with('(({')
					|| inner_code.contains('){0})')
				if is_rvalue {
					inner_type := g.get_expr_type(node.expr)
					if inner_type != '' && inner_type != 'int' && inner_type != 'void' {
						tmp_name := '_addr_t${g.tmp_counter}'
						g.tmp_counter++
						g.sb.write_string('({ ${inner_type} ${tmp_name} = ${inner_code}; &${tmp_name}; })')
					} else {
						g.sb.write_string('&${inner_code}')
					}
				} else {
					g.sb.write_string('&${inner_code}')
				}
			} else {
				// For dereference of a sum type data pointer (from smartcast),
				// add null guard to avoid crash on zero-initialized sum types.
				if node.op == .mul && g.is_sum_data_ptr_deref(node.expr) {
					saved_sb := g.sb
					g.sb = strings.new_builder(256)
					g.expr(node.expr)
					ptr_code := g.sb.str()
					g.sb = saved_sb
					cast_type := g.get_cast_expr_type_name(node.expr)
					if cast_type != '' {
						g.sb.write_string('(${ptr_code} ? (*(${ptr_code})) : (${cast_type}){0})')
					} else {
						g.sb.write_string('*(')
						g.sb.write_string(ptr_code)
						g.sb.write_string(')')
					}
				} else {
					op := match node.op {
						.minus { '-' }
						.not { '!' }
						.mul { '*' }
						.bit_not { '~' }
						else { '' }
					}

					g.sb.write_string(op)
					g.expr(node.expr)
				}
			}
		}
		ast.CallExpr {
			g.call_expr(node.lhs, node.args)
		}
		ast.CallOrCastExpr {
			g.gen_call_or_cast_expr(node)
		}
		ast.SelectorExpr {
			sel := node as ast.SelectorExpr
			sel_expr := ast.Expr(sel)
			lhs_expr := sel.lhs
			rhs_name := sel.rhs.name
			lhs_name := if lhs_expr is ast.Ident { sel.lhs.name() } else { '' }
			// Comptime field access interception: field.name, field.typ, val.$(field.name), etc.
			if g.comptime_field_var != '' {
				if g.gen_comptime_field_selector(sel) {
					return
				}
			}
			// Comptime method access interception: method.name, method.attrs, method.args, etc.
			if g.comptime_method_var != '' {
				if g.gen_comptime_method_selector(sel) {
					return
				}
			}
			// C.<ident> references C macros/constants directly (e.g. C.EOF -> EOF).
			if lhs_expr is ast.Ident {
				if lhs_name == 'C' {
					g.sb.write_string(rhs_name)
					return
				}
			}
			if rhs_name == 'name' {
				if lhs_expr is ast.Ident {
					// Generic type parameter access: T.name -> string literal with type name.
					if concrete := g.active_generic_types[lhs_name] {
						g.sb.write_string(c_static_v_string_expr(concrete.name()))
						return
					}
					if is_generic_placeholder_type_name(lhs_name) {
						g.sb.write_string(c_static_v_string_expr(lhs_name))
						return
					}
				}
				if type_name := g.typeof_expr_type_name(lhs_expr) {
					g.sb.write_string(c_static_v_string_expr(type_name))
					return
				}
				if raw_lhs_type := g.get_raw_type(lhs_expr) {
					if raw_lhs_type is types.Enum {
						enum_name := g.types_type_to_c(raw_lhs_type)
						if enum_name != '' && !is_generic_placeholder_c_type_name(enum_name) {
							g.sb.write_string('${enum_name}__str(')
							g.expr(lhs_expr)
							g.sb.write_string(')')
							return
						}
					}
				}
				lhs_type_for_name := g.get_expr_type(lhs_expr).trim_right('*')
				if lhs_type_for_name != '' && lhs_type_for_name != 'int'
					&& g.is_enum_type(lhs_type_for_name) {
					g.sb.write_string('${lhs_type_for_name}__str(')
					g.expr(lhs_expr)
					g.sb.write_string(')')
					return
				}
			}
			if lhs_expr is ast.Ident {
				if lhs_name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
					'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
					if enum_name := g.enum_value_to_enum[rhs_name] {
						if g.should_use_known_enum_field(enum_name, rhs_name) {
							g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
							return
						}
					}
				}
				is_known_var := g.local_var_c_type_for_expr(lhs_expr) != none
				if !is_known_var && !g.is_module_ident(lhs_name) {
					if enum_name := g.get_expr_type_from_env(sel_expr) {
						if enum_name != '' && g.is_enum_type(enum_name) {
							g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
							return
						}
					}
					if lhs_name in ['bool', 'string', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16',
						'u32', 'u64', 'f32', 'f64', 'byte', 'rune'] {
						if enum_name := g.enum_value_to_enum[rhs_name] {
							if g.should_use_known_enum_field(enum_name, rhs_name) {
								g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
								return
							}
						}
					}
				}
			}
			// If checker already resolved this selector as an enum value, use Enum__field.
			if raw_type := g.get_raw_type(sel_expr) {
				if raw_type is types.Enum {
					// Verify the field actually belongs to this enum.
					// The checker may annotate branch values with the match expression's
					// enum type instead of the return type's enum.
					mut field_valid := false
					for f in raw_type.fields {
						if f.name == rhs_name {
							field_valid = true
							break
						}
					}
					if field_valid {
						mut emit_enum_value := false
						if lhs_expr is ast.EmptyExpr {
							emit_enum_value = true
						} else if lhs_expr is ast.Ident {
							if g.is_enum_type(lhs_name) {
								emit_enum_value = true
							} else if !g.is_module_ident(lhs_name) {
								emit_enum_value = g.local_var_c_type_for_expr(lhs_expr) == none
							}
						}
						if emit_enum_value {
							enum_name := g.types_type_to_c(raw_type)
							if !is_generic_placeholder_c_type_name(enum_name) {
								g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
								return
							}
						}
					}
				}
			}
			if g.gen_sum_narrowed_selector(sel) {
				return
			}
			if g.gen_sum_narrowed_selector_lhs_field(sel) {
				return
			}
			if g.gen_sum_common_field_selector(sel) {
				return
			}
			if g.gen_sum_variant_field_selector(sel) {
				return
			}
			if lhs_expr is ast.SelectorExpr {
				lhs_sel := lhs_expr as ast.SelectorExpr
				if lhs_sel.lhs is ast.Ident {
					lhs_mod := lhs_sel.lhs as ast.Ident
					mut module_names := [lhs_mod.name]
					resolved_mod_name := g.resolve_module_name(lhs_mod.name)
					if resolved_mod_name !in module_names {
						module_names << resolved_mod_name
					}
					for mod_name in module_names {
						enum_name := '${mod_name}__${lhs_sel.rhs.name}'
						if g.enum_has_field(enum_name, rhs_name) {
							g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
							return
						}
					}
				}
			}
			if rhs_name == 'values' {
				if raw_type := g.get_raw_type(sel_expr) {
					if raw_type is types.Array {
						mut enum_name := ''
						if lhs_expr is ast.Ident {
							lhs_ident := lhs_expr as ast.Ident
							if g.is_enum_type(lhs_ident.name) {
								enum_name = lhs_ident.name
							} else if g.cur_module != '' && g.cur_module != 'main'
								&& g.cur_module != 'builtin' {
								qualified := '${g.cur_module}__${lhs_ident.name}'
								if g.is_enum_type(qualified) {
									enum_name = qualified
								}
							}
						} else if lhs_expr is ast.SelectorExpr {
							lhs_sel := lhs_expr as ast.SelectorExpr
							if lhs_sel.lhs is ast.Ident {
								lhs_mod := lhs_sel.lhs as ast.Ident
								mod_name := g.resolve_module_name(lhs_mod.name)
								qualified := '${mod_name}__${lhs_sel.rhs.name}'
								if g.is_enum_type(qualified) {
									enum_name = qualified
								}
							}
						}
						if enum_name != '' {
							g.sb.write_string('${enum_name}__values_array')
							return
						}
					}
				}
			}
			lhs_type := g.get_expr_type(lhs_expr)
			if rhs_name in ['x', 'y', 'z', 'w'] {
				base_lhs_type := lhs_type.trim_right('*')
				if base_lhs_type in primitive_types {
					if lhs_type.ends_with('*') {
						g.sb.write_string('(*')
						g.expr(lhs_expr)
						g.sb.write_string(')')
					} else {
						g.expr(lhs_expr)
					}
					return
				}
			}
			if rhs_name == 'data' {
				if lhs_type.starts_with('_result_') && g.result_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(lhs_expr)
					return
				}
				if lhs_type.starts_with('_option_') && option_value_type(lhs_type) != '' {
					g.gen_unwrapped_value_expr(lhs_expr)
					return
				}
				if lhs_expr is ast.Ident && lhs_expr.name.starts_with('_or_t') {
					payload_type := g.get_expr_type(sel_expr)
					if payload_type != ''
						&& payload_type !in ['int_literal', 'float_literal', 'void'] {
						if g.gen_unwrapped_payload_expr(lhs_expr, '', payload_type) {
							return
						}
					}
				}
			}
			// Transformer-generated or-block temps (`_or_t*`) can end up with a
			// field-name mismatch when the checker resolved the call to the wrong
			// overload (e.g. `header.get` typed as `http.get`). Trust the temp's
			// actual C type and rewrite the field name accordingly.
			if lhs_expr is ast.Ident && lhs_expr.name.starts_with('_or_t') {
				if rhs_name == 'is_error' && lhs_type.starts_with('_option_') {
					g.sb.write_string(lhs_expr.name)
					g.sb.write_string('.state')
					return
				}
				if rhs_name == 'state' && lhs_type.starts_with('_result_') {
					g.sb.write_string(lhs_expr.name)
					g.sb.write_string('.is_error')
					return
				}
			}
			if variant_field := g.sum_data_variant_selector_field(sel) {
				g.expr(lhs_expr)
				g.sb.write_string('.${variant_field}')
				return
			}
			if lhs_type.trim_right('*') == 'chan' && rhs_name in ['len', 'closed'] {
				if rhs_name == 'len' {
					g.sb.write_string('((int)atomic_load_u32(&((sync__Channel*)(')
					g.expr(lhs_expr)
					g.sb.write_string('))->read_avail))')
				} else {
					g.sb.write_string('(atomic_load_u16(&((sync__Channel*)(')
					g.expr(lhs_expr)
					g.sb.write_string('))->closed) != 0)')
				}
				return
			}
			lhs_string_type := g.builtin_string_field_lhs_type(lhs_expr, rhs_name)
			if lhs_string_type != '' {
				use_ptr := lhs_string_type.ends_with('*')
				selector := if use_ptr { '->' } else { '.' }
				g.expr(lhs_expr)
				g.sb.write_string('${selector}${escape_c_keyword(rhs_name)}')
				return
			}
			// Fixed-size array `.len` becomes compile-time length.
			if rhs_name == 'len' {
				if lhs_expr is ast.Ident {
					lhs_ident := lhs_expr as ast.Ident
					mut fixed_name := lhs_ident.name
					module_const_key := 'const_${g.cur_module}__${lhs_ident.name}'
					if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
						&& module_const_key in g.emitted_types {
						fixed_name = '${g.cur_module}__${lhs_ident.name}'
					}
					if fixed_name in g.fixed_array_globals {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
					if fixed_name in ['strconv__pow5_inv_split_32', 'strconv__pow5_split_32',
						'strconv__pow5_inv_split_64_x', 'strconv__pow5_split_64_x'] {
						g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
						return
					}
				}
				if lhs_expr is ast.SelectorExpr {
					lhs_sel := lhs_expr as ast.SelectorExpr
					if lhs_sel.lhs is ast.Ident {
						lhs_mod := lhs_sel.lhs as ast.Ident
						if g.is_module_ident(lhs_mod.name) {
							mod_name := g.resolve_module_name(lhs_mod.name)
							fixed_name := '${mod_name}__${lhs_sel.rhs.name}'
							if fixed_name in g.fixed_array_globals {
								g.sb.write_string('((int)(sizeof(${fixed_name}) / sizeof(${fixed_name}[0])))')
								return
							}
						}
					}
					if g.is_fixed_array_selector(lhs_sel) {
						g.sb.write_string('((int)(sizeof(')
						g.expr(lhs_expr)
						g.sb.write_string(') / sizeof((')
						g.expr(lhs_expr)
						g.sb.write_string(')[0])))')
						return
					}
				}
				if raw_type := g.get_raw_type(lhs_expr) {
					if raw_type is types.ArrayFixed {
						g.sb.write_string('((int)(sizeof(')
						g.expr(lhs_expr)
						g.sb.write_string(') / sizeof((')
						g.expr(lhs_expr)
						g.sb.write_string(')[0])))')
						return
					}
				}
			}
			// Enum shorthand: `.field` -> `EnumName__field` (using type checker info).
			if lhs_expr is ast.EmptyExpr {
				// The enum_value_to_enum map definitively tells us which enum owns a field.
				// Use it to validate/override type checker results that may be wrong
				// (e.g., match branch values annotated with the match expression's enum type
				// instead of the return type's enum).
				known_enum := g.enum_value_to_enum[rhs_name] or { '' }
				if raw_type := g.get_raw_type(sel_expr) {
					if raw_type is types.Enum {
						enum_name := g.types_type_to_c(raw_type)
						if !is_generic_placeholder_c_type_name(enum_name)
							&& g.enum_has_field(enum_name, rhs_name) {
							g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
							return
						}
					}
				}
				if enum_name := g.get_expr_type_from_env(sel_expr) {
					if enum_name != '' && enum_name != 'int'
						&& !is_generic_placeholder_c_type_name(enum_name)
						&& g.is_enum_type(enum_name) {
						if g.enum_has_field(enum_name, rhs_name) {
							g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
							return
						}
					}
				}
				// Use the definitive enum field mapping
				if g.should_use_known_enum_field(known_enum, rhs_name) {
					g.sb.write_string(g.enum_member_c_name(known_enum, rhs_name))
					return
				}
				// Last resort: use function return type as context
				if g.cur_fn_ret_type != '' && g.is_enum_type(g.cur_fn_ret_type) {
					g.sb.write_string(g.enum_member_c_name(g.cur_fn_ret_type, rhs_name))
					return
				}
			}
			// module.const / module.var => module__const / module__var
			if lhs_expr is ast.Ident {
				lhs_ident := lhs_expr as ast.Ident
				is_local := g.local_var_c_type_for_expr(lhs_expr) != none
				if g.is_module_ident(lhs_ident.name) && !is_local {
					mod_name := g.resolve_module_name(lhs_ident.name)
					g.sb.write_string(g.module_selector_storage_c_name(mod_name, rhs_name))
					return
				}
				if !is_local {
					if mod_name := g.known_module_runtime_symbol(lhs_ident.name, rhs_name) {
						g.sb.write_string(g.module_selector_storage_c_name(mod_name, rhs_name))
						return
					}
				}
			}
			// Method value in expression position: `obj.method` -> `Type__method`.
			// Prefer regular field access when a concrete field exists (e.g. `string.str`).
			if g.selector_field_type(sel) == '' {
				if method_value_name := g.selector_method_value_name(sel) {
					mut target_type := g.get_expr_type(sel_expr)
					if (target_type == '' || target_type == 'int') && g.env != unsafe { nil } {
						if raw := g.get_raw_type(sel_expr) {
							if raw is types.Alias {
								alias_raw := raw
								if alias_raw.base_type is types.FnType {
									target_type = alias_raw.name
								}
							}
						}
					}
					if target_type != '' && target_type !in ['int', 'void*', 'voidptr'] {
						if g.gen_bound_method_value_expr(sel, target_type) {
							return
						}
						g.sb.write_string('((${target_type})${method_value_name})')
					} else {
						g.sb.write_string(method_value_name)
					}
					return
				}
			}
			if _ := g.selector_interface_data_field(sel) {
				mut use_ptr := g.selector_use_ptr(lhs_expr)
				if lhs_name in g.cur_fn_mut_params {
					use_ptr = true
				} else if local_type := g.local_var_c_type_for_expr(lhs_expr) {
					use_ptr = local_type.ends_with('*') || local_type == 'chan'
				}
				lhs_struct := g.selector_struct_name(lhs_expr)
				owner := g.embedded_owner_for(lhs_struct, rhs_name)
				field_name := escape_c_keyword(rhs_name)
				selector := if use_ptr { '->' } else { '.' }
				g.sb.write_string('(*(')
				g.expr(lhs_expr)
				if owner != '' {
					g.sb.write_string('${selector}${escape_c_keyword(owner)}.${field_name}')
				} else {
					g.sb.write_string('${selector}${field_name}')
				}
				g.sb.write_string('))')
				return
			}
			// Check if LHS is an enum type name -> emit EnumName__field
			if lhs_expr is ast.Ident {
				is_local_var := g.local_var_c_type_for_expr(lhs_expr) != none
					|| lhs_name in g.cur_fn_mut_params
				if !is_local_var && g.is_enum_type(lhs_name) {
					enum_name := g.get_qualified_name(lhs_name)
					g.sb.write_string(g.enum_member_c_name(enum_name, rhs_name))
				} else {
					mut use_ptr := g.selector_use_ptr(lhs_expr)
					if lhs_name in g.cur_fn_mut_params {
						use_ptr = true
					} else if local_type := g.local_var_c_type_for_expr(lhs_expr) {
						// Local declaration type is authoritative for value vs pointer access.
						use_ptr = local_type.ends_with('*') || local_type == 'chan'
					}
					lhs_struct := g.selector_struct_name(lhs_expr)
					owner := g.embedded_owner_for(lhs_struct, rhs_name)
					field_name := escape_c_keyword(rhs_name)
					selector := if use_ptr { '->' } else { '.' }
					g.expr(lhs_expr)
					if owner != '' {
						g.sb.write_string('${selector}${escape_c_keyword(owner)}.${field_name}')
					} else {
						g.sb.write_string('${selector}${field_name}')
					}
				}
			} else {
				use_ptr := g.selector_use_ptr(lhs_expr)
				lhs_struct := g.selector_struct_name(lhs_expr)
				owner := g.embedded_owner_for(lhs_struct, rhs_name)
				field_name := escape_c_keyword(rhs_name)
				selector := if use_ptr { '->' } else { '.' }
				g.expr(lhs_expr)
				if owner != '' {
					g.sb.write_string('${selector}${escape_c_keyword(owner)}.${field_name}')
				} else {
					g.sb.write_string('${selector}${field_name}')
				}
			}
		}
		ast.IfExpr {
			g.gen_if_expr_value(&node)
		}
		ast.PostfixExpr {
			op := match node.op {
				.inc { '++' }
				.dec { '--' }
				else { '' }
			}

			if node.expr is ast.Ident && node.expr.name in g.cur_fn_mut_params {
				local_type := (g.get_local_var_c_type(node.expr.name) or { '' }).trim_space()
				if local_type.ends_with('*') {
					g.sb.write_string('(*')
					g.expr(node.expr)
					g.sb.write_string(')')
					g.sb.write_string(op)
					return
				}
			}
			g.expr(node.expr)
			g.sb.write_string(op)
		}
		ast.ModifierExpr {
			g.expr(node.expr)
		}
		ast.CastExpr {
			g.gen_cast_expr(node)
		}
		ast.IndexExpr {
			g.gen_index_expr(node)
		}
		ast.ArrayInitExpr {
			g.gen_array_init_expr(node)
		}
		ast.InitExpr {
			g.gen_init_expr(node)
		}
		ast.MapInitExpr {
			panic('bug in v2 compiler: MapInitExpr should have been lowered in v2.transformer')
		}
		ast.MatchExpr {
			panic('bug in v2 compiler: MatchExpr should have been lowered in v2.transformer')
		}
		ast.UnsafeExpr {
			g.gen_unsafe_expr(node)
		}
		ast.OrExpr {
			panic('bug in v2 compiler: OrExpr should have been expanded in v2.transformer (${g.cur_file_name}:${g.cur_fn_name} pos=${node.pos} expr=${node.expr.name()})')
		}
		ast.AsCastExpr {
			g.gen_as_cast_expr(node)
		}
		ast.StringInterLiteral {
			g.gen_string_inter_literal(node)
		}
		ast.FnLiteral {
			g.gen_fn_literal(node)
		}
		ast.LambdaExpr {
			g.sb.write_string('/* [TODO] LambdaExpr */ NULL')
		}
		ast.ComptimeExpr {
			if node.expr is ast.IfExpr {
				g.gen_comptime_if_expr(node.expr)
				return
			}
			g.gen_comptime_expr(node)
		}
		ast.Keyword {
			g.gen_keyword(node)
		}
		ast.KeywordOperator {
			g.gen_keyword_operator(node)
		}
		ast.RangeExpr {
			panic('bug in v2 compiler: RangeExpr should have been lowered in v2.transformer (${g.cur_file_name}:${g.cur_fn_name} pos=${node.pos})')
		}
		ast.SelectExpr {
			g.sb.write_string('/* [TODO] SelectExpr */ 0')
		}
		ast.LockExpr {
			panic('bug in v2 compiler: LockExpr should have been lowered in v2.transformer')
		}
		ast.Type {
			g.sb.write_string('/* [TODO] Type */ 0')
		}
		ast.AssocExpr {
			panic('bug in v2 compiler: AssocExpr should have been lowered in v2.transformer')
		}
		ast.Tuple {
			tuple_type := g.get_expr_type(node)
			g.sb.write_string('((${tuple_type}){')
			for i in 0 .. node.exprs.len {
				expr := node.exprs[i]
				if i > 0 {
					g.sb.write_string(', ')
				}
				g.sb.write_string('.arg${i} = ')
				g.expr(expr)
			}
			g.sb.write_string('})')
		}
		ast.FieldInit {
			panic('bug in v2 compiler: FieldInit in expression position should have been lowered in v2.transformer (${g.cur_file_name}:${g.cur_fn_name} field=${node.name})')
		}
		ast.IfGuardExpr {
			panic('bug in v2 compiler: IfGuardExpr should have been expanded in v2.transformer')
		}
		ast.GenericArgs {
			if g.generic_args_expr_is_index(node) {
				g.gen_index_expr(ast.IndexExpr{
					lhs:  node.lhs
					expr: node.args[0]
					pos:  node.pos
				})
				return
			}
			if g.try_emit_generic_fn_value(node) {
				return
			}
			g.expr(node.lhs)
		}
		ast.GenericArgOrIndexExpr {
			is_index_expr := g.generic_arg_or_index_expr_is_index(node)
			if is_index_expr {
				g.gen_index_expr(ast.IndexExpr{
					lhs:  node.lhs
					expr: node.expr
					pos:  node.pos
				})
				return
			}
			if g.try_emit_generic_fn_value(node) {
				return
			}
			if !is_index_expr && g.try_emit_generic_fn_value(node.lhs) {
				return
			}
			if raw_type := g.get_raw_type(node.lhs) {
				match raw_type {
					types.FnType {
						g.expr(node.lhs)
						return
					}
					types.Struct {
						if raw_type.generic_params.len > 0 {
							g.expr(node.lhs)
							return
						}
					}
					types.Alias {
						if raw_type.base_type is types.FnType {
							g.expr(node.lhs)
							return
						}
						if raw_type.base_type is types.Struct
							&& raw_type.base_type.generic_params.len > 0 {
							g.expr(node.lhs)
							return
						}
					}
					else {}
				}
			}
			g.gen_index_expr(ast.IndexExpr{
				lhs:  node.lhs
				expr: node.expr
				pos:  node.pos
			})
		}
		ast.SqlExpr {
			g.gen_sql_expr_placeholder(node)
		}
		ast.EmptyExpr {}
	}
}

fn generic_fn_value_base_expr(expr ast.Expr) ast.Expr {
	return match expr {
		ast.GenericArgs {
			expr.lhs
		}
		ast.GenericArgOrIndexExpr {
			expr.lhs
		}
		else {
			expr
		}
	}
}

fn raw_type_is_indexable_for_generic_disambiguation(raw_type types.Type) bool {
	mut typ := raw_type
	for {
		if typ is types.Alias {
			typ = typ.base_type
			continue
		}
		break
	}
	return typ is types.Array || typ is types.ArrayFixed || typ is types.Map || typ is types.String
		|| typ is types.Pointer
}

fn c_type_is_indexable_for_generic_disambiguation(c_type string) bool {
	typ := c_type.trim_space().trim_right('*')
	return typ == 'string' || typ.starts_with('Array_') || typ.starts_with('Map_')
}

fn (mut g Gen) expr_is_indexable_for_generic_disambiguation(expr ast.Expr) bool {
	if raw_type := g.get_raw_type(expr) {
		return raw_type_is_indexable_for_generic_disambiguation(raw_type)
	}
	if expr is ast.SelectorExpr {
		field_type := g.selector_field_type(expr)
		if c_type_is_indexable_for_generic_disambiguation(field_type) {
			return true
		}
	}
	return c_type_is_indexable_for_generic_disambiguation(g.get_expr_type(expr))
}

fn (mut g Gen) generic_args_expr_is_index(expr ast.GenericArgs) bool {
	return expr.args.len == 1 && g.expr_is_indexable_for_generic_disambiguation(expr.lhs)
}

fn (mut g Gen) generic_arg_or_index_expr_is_index(expr ast.GenericArgOrIndexExpr) bool {
	return g.expr_is_indexable_for_generic_disambiguation(expr.lhs)
}

fn (mut g Gen) generic_fn_value_specialized_name(expr ast.Expr) ?string {
	decl := g.generic_call_decl_from_lhs(expr) or { return none }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	type_args := generic_call_type_args(expr)
	mut has_unresolved_explicit_type_arg := false
	for i, param_name in generic_params {
		if i >= type_args.len {
			break
		}
		concrete := g.generic_type_arg_concrete_type(type_args[i]) or {
			has_unresolved_explicit_type_arg = true
			continue
		}
		bindings[param_name] = concrete
	}
	if has_unresolved_explicit_type_arg {
		return none
	}
	// Function values like request_handler[A, X] are emitted without call
	// arguments, so generic placeholders must be resolved from the active
	// enclosing specialization before choosing the C function pointer symbol.
	for param_name in generic_params {
		if param_name in bindings {
			continue
		}
		if concrete := g.active_generic_types[param_name] {
			bindings[param_name] = concrete
		}
	}
	if type_args.len == 0 && bindings.len != generic_params.len
		&& !g.bind_embedded_generic_type_args(expr, generic_params, mut bindings) {
		return none
	}
	if bindings.len != generic_params.len {
		return none
	}
	mut suffixes := []string{cap: generic_params.len}
	for param_name in generic_params {
		concrete := bindings[param_name] or { return none }
		if !generic_concrete_type_is_runtime_specializable(concrete) {
			return none
		}
		suffixes << g.generic_specialization_token_from_type(concrete)
	}
	base_lhs := generic_fn_value_base_expr(expr)
	base_name := g.resolve_call_name(base_lhs, 0)
	if base_name == '' || suffixes.len == 0 {
		return none
	}
	candidate := '${base_name}_T_${suffixes.join('_')}'
	short_name := generic_call_short_name(expr)
	if short_name != '' {
		g.record_late_generic_call_spec(short_name, bindings)
	}
	g.ensure_specialized_call_signature(candidate)
	return candidate
}

fn (mut g Gen) write_specialized_fn_value(name string) {
	ret_type := g.fn_return_types[name] or {
		g.sb.write_string(name)
		return
	}
	param_types := g.fn_param_types[name] or {
		g.sb.write_string(name)
		return
	}
	// Generic function values can be referenced before the prototype pass has
	// seen their late specialization. A block-scope prototype keeps the function
	// pointer expression valid without depending on file-level declaration order.
	g.sb.write_string('({ ${ret_type} ${name}(')
	for i, param_type in param_types {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.sb.write_string(param_type)
	}
	g.sb.write_string('); ${name}; })')
}

fn (mut g Gen) try_emit_generic_fn_value(expr ast.Expr) bool {
	match expr {
		ast.GenericArgs {
			if g.generic_args_expr_is_index(expr) {
				return false
			}
		}
		ast.GenericArgOrIndexExpr {
			if g.generic_arg_or_index_expr_is_index(expr) {
				return false
			}
		}
		else {}
	}

	lhs := generic_fn_value_base_expr(expr)
	if lhs is ast.Ident {
		if _ := g.get_local_var_c_type(lhs.name) {
			return false
		}
	}
	if specialized_name := g.generic_fn_value_specialized_name(expr) {
		g.mark_called_fn_name(specialized_name)
		g.write_specialized_fn_value(specialized_name)
		return true
	}
	name := g.resolve_call_name(lhs, 0)
	if name == '' {
		return false
	}
	if name in g.fn_param_is_ptr || name in g.fn_return_types {
		g.mark_called_fn_name(name)
		g.sb.write_string(name)
		return true
	}
	if _ := g.find_generic_fn_decl_by_base_name(name) {
		g.sb.write_string(name)
		return true
	}
	return false
}

fn (mut g Gen) gen_sql_expr_placeholder(node ast.SqlExpr) {
	mut c_type := g.get_expr_type(ast.Expr(node)).trim_space()
	if c_type == '' || c_type == 'int' {
		if node.is_count {
			c_type = 'int'
		} else if node.table_name == '' {
			c_type = '_result_void'
		}
	}
	g.sb.write_string(zero_value_for_type(c_type))
}

fn extract_compare_cast_type(expr ast.Expr) string {
	if expr is ast.PrefixExpr && expr.op == .mul {
		if expr.expr is ast.CastExpr {
			if expr.expr.typ is ast.PrefixExpr && expr.expr.typ.op == .amp
				&& expr.expr.typ.expr is ast.Ident {
				return expr.expr.typ.expr.name
			}
		}
	}
	return ''
}

fn (mut g Gen) gen_index_expr_value(expr ast.Expr) {
	g.sb.write_string('((int)(')
	g.expr(expr)
	g.sb.write_string('))')
}

fn (mut g Gen) gen_stmts_from_expr(e ast.Expr) {
	match e {
		ast.IfExpr {
			g.gen_if_expr_stmt(&e)
		}
		ast.UnsafeExpr {
			for stmt in e.stmts {
				g.gen_stmt(stmt)
			}
		}
		ast.EmptyExpr {}
		else {
			g.write_indent()
			g.expr(e)
			g.sb.writeln(';')
		}
	}
}

fn (mut g Gen) expr_is_explicit_value_of_type(expr ast.Expr, type_name string) bool {
	match expr {
		ast.InitExpr {
			return g.expr_type_to_c(expr.typ) == type_name
		}
		ast.CastExpr {
			return g.expr_type_to_c(expr.typ) == type_name
		}
		ast.CallOrCastExpr {
			return g.call_or_cast_lhs_is_type(expr.lhs) && g.expr_type_to_c(expr.lhs) == type_name
		}
		ast.CallExpr {
			return expr.args.len == 1 && g.call_or_cast_lhs_is_type(expr.lhs)
				&& g.expr_type_to_c(expr.lhs) == type_name
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) gen_type_cast_expr(type_name string, expr ast.Expr) {
	expr_type := g.get_expr_type(expr)
	if type_name.starts_with('_option_') && is_none_like_expr(expr) {
		g.sb.write_string('(${type_name}){ .state = 2 }')
		return
	}
	if expr is ast.BasicLiteral && expr.kind == .number && expr.value == '0' {
		if type_name !in primitive_types && !type_name.ends_with('*') && !g.is_enum_type(type_name)
			&& type_name !in ['void*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
			g.sb.write_string(zero_value_for_type(type_name))
			return
		}
	}
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(expr)
		g.sb.write_string('))')
		return
	}
	if g.expr_is_sum_variant_extract(expr, type_name) {
		g.gen_as_cast_expr(ast.AsCastExpr{
			expr: expr
			typ:  ast.Expr(ast.Ident{
				name: type_name
			})
		})
		return
	}
	if variants := g.sum_type_variants[type_name] {
		if g.expr_is_explicit_value_of_type(expr, type_name) {
			g.expr(expr)
			return
		}
		mut inner_type := expr_type
		if local_type := g.local_var_c_type_for_expr(expr) {
			if local_type != '' && local_type != 'int' {
				inner_type = local_type
			}
		}
		if expr is ast.SelectorExpr {
			declared_field_type := g.selector_declared_field_type(expr)
			if declared_field_type == type_name || (declared_field_type != ''
				&& short_type_name(declared_field_type) == short_type_name(type_name)) {
				g.expr(expr)
				return
			}
		}
		if expr is ast.AsCastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type != '' && cast_type != 'int' {
				inner_type = cast_type
			}
		}
		mut wrap_expr := expr
		base_inner_type := sumtype_variant_pointer_base(inner_type, variants)
		if base_inner_type != '' {
			inner_type = base_inner_type
			wrap_expr = ast.PrefixExpr{
				op:   .mul
				expr: expr
			}
		}
		if inner_type == '' || inner_type == 'int' {
			match expr {
				ast.CallExpr {
					if ret := g.get_call_return_type(expr.lhs, expr.args) {
						if ret != '' {
							inner_type = ret
						}
					}
				}
				ast.InitExpr {
					inner_type = g.expr_type_to_c(expr.typ)
				}
				ast.SelectorExpr {
					field_type := g.selector_field_type(expr)
					if field_type != '' {
						inner_type = field_type
					}
				}
				else {
					if local_type := g.local_var_c_type_for_expr(expr) {
						if local_type != '' && local_type != 'int' {
							inner_type = local_type
						}
					}
				}
			}
		}
		// Identity cast: inner type is already the target sum type, no wrapping needed.
		// But verify with local var type — env can return the sum type for a variant value.
		if inner_type == type_name {
			if expr is ast.Ident {
				local_t := g.get_local_var_c_type(expr.name) or { '' }
				if local_t != '' && local_t != type_name && local_t != 'int' {
					inner_type = local_t
				}
			}
			if inner_type == type_name {
				g.expr(expr)
				return
			}
		}
		if inner_type != '' {
			mut tag := -1
			mut field_name := ''
			for i, v in variants {
				if v == inner_type || inner_type.ends_with('__${v}')
					|| v.ends_with('__${inner_type}') {
					tag = i
					field_name = v
					break
				}
			}
			// If direct matching failed, try qualifying inner_type with the sum type's module prefix
			// (e.g. 'Array_Attribute' → 'Array_ast__Attribute' when sum type is 'ast__Stmt')
			if tag < 0 && type_name.contains('__') {
				mod_prefix := type_name.all_before_last('__') + '__'
				// Qualify the type: Array_X → Array_mod__X, or just X → mod__X
				qualified := if inner_type.starts_with('Array_') && !inner_type[6..].contains('__') {
					'Array_${mod_prefix}${inner_type[6..]}'
				} else if inner_type.starts_with('Map_') && !inner_type[4..].contains('__') {
					'Map_${mod_prefix}${inner_type[4..]}'
				} else if !inner_type.contains('__') {
					'${mod_prefix}${inner_type}'
				} else {
					''
				}
				if qualified != '' {
					for i, v in variants {
						if v == qualified {
							tag = i
							field_name = v
							break
						}
					}
				}
			}
			// If direct matching failed, check if inner_type is a known sum type
			// that appears as a variant of the target sum type (e.g. ast__Type -> ast__Expr._Type)
			if tag < 0 && inner_type in g.sum_type_variants {
				inner_short := if inner_type.contains('__') {
					inner_type.all_after_last('__')
				} else {
					inner_type
				}
				for i, v in variants {
					if v == inner_short {
						tag = i
						field_name = v
						break
					}
				}
			}
			if tag >= 0 {
				is_primitive :=
					inner_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
					|| inner_type in g.primitive_type_aliases
				g.gen_sum_type_wrap(type_name, field_name, tag, is_primitive, wrap_expr, inner_type)
				return
			}
		}
		// Fallback: try to infer variant from expression structure
		inferred := g.infer_sum_variant_from_expr(type_name, variants, expr)
		if inferred.tag >= 0 {
			g.gen_sum_type_wrap(type_name, inferred.field_name, inferred.tag,
				inferred.is_primitive, expr, inferred.inner_type)
			return
		}
	}
	if g.gen_interface_cast(type_name, expr) {
		return
	}
	// For non-sum-types, use C cast
	g.sb.write_string('((${type_name})(')
	g.expr(expr)
	g.sb.write_string('))')
}

fn (mut g Gen) typeof_expr_type_name(expr ast.Expr) ?string {
	if expr is ast.KeywordOperator && expr.op == .key_typeof && expr.exprs.len > 0 {
		if expr.exprs[0] is ast.Ident && expr.exprs[0].name == g.comptime_field_var {
			return c_name_to_v_name(g.comptime_field_type)
		}
		if raw_type := g.get_raw_type(expr.exprs[0]) {
			return g.types_type_to_v(raw_type)
		}
		type_name := g.expr_type_to_c(expr.exprs[0])
		if type_name != '' {
			return c_name_to_v_name(type_name)
		}
	}
	expr_name := expr.name()
	if expr_name.starts_with('typeof[') {
		inside := expr_name.all_after('typeof[').all_before(']')
		if concrete := g.active_generic_types[inside] {
			return concrete.name()
		}
		if inside != '' {
			return inside
		}
	}
	return none
}

struct SumVariantMatch {
	tag          int
	field_name   string
	is_primitive bool
	inner_type   string
}

fn pointer_c_type_base_name(type_name string) string {
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

fn sumtype_variant_pointer_base(type_name string, variants []string) string {
	base_name := pointer_c_type_base_name(type_name)
	if base_name == '' {
		return ''
	}
	base_short := if base_name.contains('__') { base_name.all_after_last('__') } else { base_name }
	for variant in variants {
		variant_short := if variant.contains('__') {
			variant.all_after_last('__')
		} else {
			variant
		}
		if base_name == variant || base_short == variant_short
			|| base_name.ends_with('__${variant_short}') || variant.ends_with('__${base_short}') {
			return base_name
		}
	}
	return ''
}

fn (mut g Gen) unwrap_addr_of_value_expr(expr ast.Expr) ?ast.Expr {
	match expr {
		ast.PrefixExpr {
			if expr.op == .amp {
				return expr.expr
			}
		}
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type in ['void*', 'voidptr'] {
				if expr.expr is ast.PrefixExpr && expr.expr.op == .amp {
					return expr.expr.expr
				}
			}
		}
		ast.ParenExpr {
			return g.unwrap_addr_of_value_expr(expr.expr)
		}
		else {}
	}

	return none
}

// expr_is_voidptr_cast detects expressions whose top-level form is a cast to
// void*/voidptr (possibly through a chain of ParenExprs). The transformer uses
// this exact shape to pre-encode sum type variant payloads, so the cleanc
// backend can emit them directly without re-wrapping.
fn (mut g Gen) expr_is_voidptr_cast(expr ast.Expr) bool {
	match expr {
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			return cast_type in ['void*', 'voidptr']
		}
		ast.ParenExpr {
			return g.expr_is_voidptr_cast(expr.expr)
		}
		else {
			return false
		}
	}
}

// expr_yields_struct_value detects expressions that evaluate to a struct VALUE
// (not a pointer), e.g. struct literals or already-emitted sum type wraps.
// Used to decide if `memdup(expr, sizeof)` is unsafe (memdup expects a pointer).
fn (mut g Gen) expr_yields_struct_value(expr ast.Expr) bool {
	match expr {
		ast.InitExpr {
			return true
		}
		ast.ParenExpr {
			return g.expr_yields_struct_value(expr.expr)
		}
		ast.ModifierExpr {
			return g.expr_yields_struct_value(expr.expr)
		}
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type in ['void*', 'voidptr'] {
				return false
			}
			if cast_type.ends_with('*') {
				return false
			}
			return g.expr_yields_struct_value(expr.expr)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) is_type_reference_expr(node ast.Expr) bool {
	match node {
		ast.Ident {
			if g.get_local_var_c_type(node.name) != none {
				return false
			}
			return g.is_type_name(node.name)
		}
		ast.SelectorExpr {
			if node.lhs is ast.Ident {
				if g.is_module_ident(node.lhs.name) {
					return true
				}
				return g.is_type_name(node.lhs.name)
			}
		}
		else {}
	}

	return false
}

fn (g &Gen) contains_as_cast_expr(node ast.Expr) bool {
	match node {
		ast.AsCastExpr {
			return true
		}
		ast.CastExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ParenExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.ModifierExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		ast.PrefixExpr {
			return g.contains_as_cast_expr(node.expr)
		}
		else {}
	}

	return false
}

fn (mut g Gen) expr_cast_target_type(node ast.Expr) string {
	match node {
		ast.CastExpr {
			return g.expr_type_to_c(node.typ)
		}
		ast.ParenExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.ModifierExpr {
			return g.expr_cast_target_type(node.expr)
		}
		ast.PrefixExpr {
			return g.expr_cast_target_type(node.expr)
		}
		else {}
	}

	return ''
}

fn (mut g Gen) gen_sum_narrowed_selector_lhs_field(node ast.SelectorExpr) bool {
	lhs_expr := strip_expr_wrappers(node.lhs)
	if node.rhs.name.starts_with('_') || lhs_expr !is ast.SelectorExpr {
		return false
	}
	lhs_sel := lhs_expr as ast.SelectorExpr
	decl_type := g.selector_storage_field_type(lhs_sel).trim_right('*')
	if decl_type == '' || g.get_sum_type_variants_for(decl_type).len == 0 {
		return false
	}
	narrowed := (g.get_expr_type_from_env(lhs_expr) or { return false }).trim_right('*')
	if narrowed == '' || narrowed == decl_type {
		return false
	}
	variants := g.get_sum_type_variants_for(decl_type)
	mut variant_field := g.sum_type_variant_field_name(decl_type, narrowed)
	if variant_field !in variants {
		variant_field = ''
	}
	for v in variants {
		if sum_type_variant_matches(v, narrowed) {
			variant_field = v
			break
		}
	}
	if variant_field == '' {
		return false
	}
	payload_type := g.sum_type_variant_payload_type(decl_type, narrowed, variant_field)
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(payload_type, node.rhs.name)
	g.sb.write_string('(((${payload_type}*)(((')
	g.expr(lhs_expr)
	g.sb.write_string(')._data._${variant_field})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

fn (mut g Gen) gen_sum_narrowed_selector(node ast.SelectorExpr) bool {
	// Internal sum fields (`_tag`, `_data`, `_Variant`) must never go through
	// smartcast field lowering; they are already the raw representation.
	if node.rhs.name.starts_with('_') {
		return false
	}
	if node.lhs !is ast.Ident {
		return false
	}
	decl_type := g.local_var_c_type_for_expr(node.lhs) or { return false }
	narrowed := g.get_expr_type_from_env(node.lhs) or { return false }
	if narrowed == '' || narrowed == decl_type {
		return false
	}
	// Pointer-only difference (e.g. ui__Widget vs ui__Widget*) is not a real
	// narrowing — let the normal selector path handle it.
	if narrowed == decl_type + '*' || decl_type == narrowed + '*'
		|| narrowed.trim_right('*') == decl_type.trim_right('*') {
		return false
	}
	// Check if the declared type is an interface — use _object access pattern
	base_decl_type := decl_type.trim_right('*')
	if g.is_interface_type(base_decl_type) {
		field_name := escape_c_keyword(node.rhs.name)
		is_ptr := g.expr_is_pointer(node.lhs)
		sep := if is_ptr { '->' } else { '.' }
		// Check if narrowed type is also an interface
		base_narrowed := narrowed.trim_right('*')
		if g.is_interface_type(base_narrowed) {
			// Interface-to-interface narrowing: field is on the narrowed interface,
			// not the source. Cast the source to the narrowed interface type.
			// Note: is-checks against interface types generate unreachable code
			// (type_id hashes never match interface hashes), so this cast is safe.
			if is_ptr {
				g.sb.write_string('(*(((${narrowed}*)(')
				g.expr(node.lhs)
				g.sb.write_string('))->${field_name}))')
			} else {
				g.sb.write_string('(*(((${narrowed}*)(&(')
				g.expr(node.lhs)
				g.sb.write_string(')))->${field_name}))')
			}
			return true
		}
		// Interface-to-concrete narrowing: cast _object to concrete type
		g.sb.write_string('(((${narrowed}*)((')
		g.expr(node.lhs)
		g.sb.write_string(')${sep}_object))')
		owner := g.embedded_owner_for(narrowed, node.rhs.name)
		if owner != '' {
			g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
		} else {
			g.sb.write_string('->${field_name}')
		}
		g.sb.write_string(')')
		return true
	}
	// Sum type narrowing path
	variants := g.sum_type_variants[decl_type] or { return false }
	mut narrowed_payload := strip_pointer_type_name(narrowed)
	mut variant_field := g.sum_type_variant_field_name(decl_type, narrowed_payload)
	if variant_field !in variants {
		variant_field = ''
	}
	for v in variants {
		if sum_type_variant_matches(v, narrowed_payload) {
			variant_field = v
			break
		}
	}
	if variant_field == '' {
		return false
	}
	narrowed_payload = g.sum_type_variant_payload_type(decl_type, narrowed_payload, variant_field)
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(narrowed_payload, node.rhs.name)
	// Inside a smartcast block, the is-check already confirmed the tag, so the
	// variant pointer is guaranteed non-NULL. Dereference directly without a
	// null guard (the previous ternary `ptr ? *ptr->field : 0` produced C type
	// mismatches when the field type was a struct/array).
	g.sb.write_string('(((${narrowed_payload}*)(((')
	g.expr(node.lhs)
	g.sb.write_string(')._data._${variant_field})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

// gen_sum_narrowed_ident handles bare Ident usage where the variable is a sum type
// that has been narrowed via an `is` check. When a sum type variable is used directly
// (e.g., passed as a function argument expecting the narrowed type), we need to emit
// the variant extraction: (*(NarrowedType*)(var._data._Variant))
fn (mut g Gen) gen_sum_narrowed_ident(node ast.Ident) bool {
	decl_type := g.get_local_var_c_type(node.name) or { return false }
	narrowed := g.get_expr_type_from_env(ast.Expr(node)) or { return false }
	if narrowed == '' || narrowed == decl_type {
		return false
	}
	// Pointer-only difference is not a real narrowing
	if narrowed == decl_type + '*' || decl_type == narrowed + '*'
		|| narrowed.trim_right('*') == decl_type.trim_right('*') {
		return false
	}
	// Check if the declared type is a sum type
	variants := g.sum_type_variants[decl_type] or { return false }
	mut narrowed_payload := strip_pointer_type_name(narrowed)
	mut variant_field := g.sum_type_variant_field_name(decl_type, narrowed_payload)
	if variant_field !in variants {
		variant_field = ''
	}
	for v in variants {
		if sum_type_variant_matches(v, narrowed_payload) {
			variant_field = v
			break
		}
	}
	if variant_field == '' {
		return false
	}
	narrowed_payload = g.sum_type_variant_payload_type(decl_type, narrowed_payload, variant_field)
	// Generate the extraction expression
	if g.is_scalar_sum_payload_type(narrowed_payload) {
		// Scalar types: ((type)(intptr_t)(var._data._variant))
		g.sb.write_string('((${narrowed_payload})(intptr_t)(${node.name}._data._${variant_field}))')
	} else if narrowed_payload == 'string' {
		// String: same as struct dereference
		g.sb.write_string('(*((${narrowed_payload}*)(${node.name}._data._${variant_field})))')
	} else {
		// Struct types: (*(NarrowedType*)(var._data._Variant))
		g.sb.write_string('(*((${narrowed_payload}*)(${node.name}._data._${variant_field})))')
	}
	return true
}

fn (mut g Gen) gen_sum_ident_as_payload_type(node ast.Ident, expected_type string) bool {
	if expected_type == '' || expected_type == 'int' {
		return false
	}
	decl_type := g.get_local_var_c_type(node.name) or { return false }
	if decl_type == expected_type || decl_type == expected_type + '*' {
		return false
	}
	variants := g.get_sum_type_variants_for(decl_type)
	if variants.len == 0 {
		return false
	}
	mut variant_field := g.sum_type_variant_field_name(decl_type, expected_type)
	if variant_field !in variants {
		for v in variants {
			if sum_type_variant_matches(v, expected_type) {
				variant_field = v
				break
			}
		}
	}
	if variant_field !in variants {
		return false
	}
	payload_type := g.sum_type_variant_payload_type(decl_type, expected_type, variant_field)
	if g.is_scalar_sum_payload_type(payload_type) {
		sep := if decl_type.ends_with('*') { '->' } else { '.' }
		g.sb.write_string('((${payload_type})(intptr_t)(${c_local_name(node.name)}${sep}_data._${variant_field}))')
		return true
	}
	sep := if decl_type.ends_with('*') { '->' } else { '.' }
	g.sb.write_string('((((${payload_type}*)(${c_local_name(node.name)}${sep}_data._${variant_field})) ? (*(((${payload_type}*)(${c_local_name(node.name)}${sep}_data._${variant_field})))) : ${zero_value_for_type(payload_type)}))')
	return true
}

fn (mut g Gen) gen_sum_ident_payload_pointer_arg(expr ast.Expr, expected_type string) bool {
	if expected_type == '' || expected_type == 'int' {
		return false
	}
	ident := sum_payload_ident_from_expr(expr) or { return false }
	decl_type := g.get_local_var_c_type(ident.name) or { return false }
	if decl_type == '' || decl_type == expected_type || decl_type == expected_type + '*' {
		return false
	}
	variants := g.get_sum_type_variants_for(decl_type)
	if variants.len == 0 {
		return false
	}
	mut variant_field := g.sum_type_variant_field_name(decl_type, expected_type)
	if variant_field !in variants {
		for v in variants {
			if sum_type_variant_matches(v, expected_type) {
				variant_field = v
				break
			}
		}
	}
	if variant_field !in variants {
		return false
	}
	payload_type := g.sum_type_variant_payload_type(decl_type, expected_type, variant_field)
	if payload_type == '' || payload_type != expected_type {
		return false
	}
	sep := if decl_type.ends_with('*') { '->' } else { '.' }
	g.sb.write_string('(((${payload_type}*)(${c_local_name(ident.name)}${sep}_data._${variant_field})))')
	return true
}

fn sum_payload_ident_from_expr(expr ast.Expr) ?ast.Ident {
	match expr {
		ast.Ident {
			return expr
		}
		ast.AsCastExpr {
			return sum_payload_ident_from_expr(expr.expr)
		}
		ast.CastExpr {
			return sum_payload_ident_from_expr(expr.expr)
		}
		ast.ParenExpr {
			return sum_payload_ident_from_expr(expr.expr)
		}
		ast.PrefixExpr {
			if expr.op == .mul {
				return sum_payload_ident_from_expr(expr.expr)
			}
		}
		ast.SelectorExpr {
			if expr.lhs is ast.SelectorExpr && expr.lhs.rhs.name == '_data' {
				return sum_payload_ident_from_expr(expr.lhs.lhs)
			}
		}
		ast.ModifierExpr {
			return sum_payload_ident_from_expr(expr.expr)
		}
		else {}
	}

	return none
}

// resolve_narrowed_selector_field_type resolves the field type of a SelectorExpr
// when the LHS is a sum type variable narrowed via an `is` check.
// Returns the raw types.Type of the field on the narrowed variant struct.
fn (mut g Gen) resolve_narrowed_selector_field_type(sel ast.SelectorExpr) ?types.Type {
	if sel.lhs !is ast.Ident {
		return none
	}
	ident := sel.lhs as ast.Ident
	// Get the narrowed C type from the checker environment
	narrowed_c := g.get_expr_type_from_env(sel.lhs) or { return none }
	decl_type := g.get_local_var_c_type(ident.name) or { return none }
	if narrowed_c == '' || narrowed_c == decl_type {
		return none
	}
	// Resolve the narrowed C type name to a raw types.Type
	narrowed_type := g.resolve_c_type_to_raw(narrowed_c) or { return none }
	// Look up the field on the narrowed struct
	if field_type := selector_struct_field_type_from_type(narrowed_type, sel.rhs.name) {
		return field_type
	}
	return none
}

fn (mut g Gen) gen_unsafe_expr(node ast.UnsafeExpr) {
	if node.stmts.len == 0 {
		g.sb.write_string('0')
		return
	}
	if node.stmts.len == 1 {
		stmt := node.stmts[0]
		if stmt is ast.ExprStmt {
			g.expr(stmt.expr)
		} else if stmt is ast.AssignStmt && stmt.lhs.len == 1 && stmt.rhs.len == 1 {
			g.expr(stmt.rhs[0])
		} else {
			// Single non-expression statement (e.g., return) - emit directly
			g.gen_stmt(stmt)
		}
		return
	}
	// Detect the addr-of-temp pattern: { tmp := expr; &tmp }
	// Generated by transformer's addr_of_expr_with_temp().
	// A GCC statement expression ({ type tmp = expr; &tmp; }) is wrong here because
	// tmp's lifetime ends at }), producing a dangling pointer.
	// Instead, use a compound literal &((type[1]){expr}[0]) whose storage lives in
	// the enclosing block scope.
	if node.stmts.len == 2 {
		first := node.stmts[0]
		last_s := node.stmts[1]
		if first is ast.AssignStmt && first.op == .decl_assign && first.lhs.len == 1
			&& first.rhs.len == 1 && last_s is ast.ExprStmt {
			if last_s.expr is ast.PrefixExpr && last_s.expr.op == .amp
				&& last_s.expr.expr is ast.Ident {
				lhs_ident := first.lhs[0]
				addr_ident := last_s.expr.expr as ast.Ident
				if lhs_ident is ast.Ident && lhs_ident.name == addr_ident.name {
					mut c_type := g.get_expr_type(first.rhs[0])
					if c_type == ''
						|| c_type in ['int', 'int_literal', 'float_literal', 'void', 'void*', 'voidptr'] {
						c_type = g.local_var_c_type_for_expr(lhs_ident) or { '' }
					}
					if c_type != '' {
						g.sb.write_string('&((${c_type}[1]){')
						g.expr(first.rhs[0])
						g.sb.write_string('}[0])')
						return
					}
				}
			}
		}
	}
	// Multi-statement: use GCC compound expression ({ ... })
	g.sb.write_string('({ ')
	for i, stmt in node.stmts {
		if i < node.stmts.len - 1 {
			g.gen_stmt(stmt)
		}
	}
	// Last statement - if it's an ExprStmt, its value is the block's value
	last := node.stmts[node.stmts.len - 1]
	if last is ast.ExprStmt {
		if is_empty_expr(last.expr) {
			if payload_expr := g.unsafe_expr_or_payload_value(node.stmts) {
				g.expr(payload_expr)
			}
		} else {
			g.expr(last.expr)
		}
		g.sb.write_string('; ')
	} else {
		g.gen_stmt(last)
		g.sb.write_string('0; ')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) unsafe_expr_or_payload_value(stmts []ast.Stmt) ?ast.Expr {
	for stmt in stmts {
		if stmt is ast.AssignStmt && stmt.op == .decl_assign && stmt.lhs.len == 1
			&& stmt.rhs.len == 1 {
			lhs := stmt.lhs[0]
			if lhs !is ast.Ident {
				continue
			}
			lhs_ident := lhs as ast.Ident
			name := lhs_ident.name
			if !name.starts_with('_or_t') {
				continue
			}
			rhs := stmt.rhs[0]
			mut wrapper_type := g.get_expr_type(rhs)
			if (wrapper_type == '' || wrapper_type == 'int') && rhs is ast.CallExpr {
				if ret_type := g.get_call_return_type(rhs.lhs, rhs.args) {
					wrapper_type = ret_type
				}
			}
			if wrapper_type.starts_with('_result_') {
				if g.result_value_type(wrapper_type) == 'void' {
					continue
				}
			} else if wrapper_type.starts_with('_option_') {
				if option_value_type(wrapper_type) == 'void' {
					continue
				}
			} else {
				continue
			}
			return ast.Expr(ast.SelectorExpr{
				lhs: ast.Ident{
					name: name
				}
				rhs: ast.Ident{
					name: 'data'
				}
			})
		}
	}
	return none
}

fn (mut g Gen) gen_index_expr(node ast.IndexExpr) {
	// Slice syntax: arr[a..b], arr[..b], arr[a..], s[a..b]
	if node.expr is ast.RangeExpr {
		g.gen_slice_index_expr(node, node.expr as ast.RangeExpr)
		return
	}
	if node.lhs is ast.SelectorExpr {
		sel := node.lhs as ast.SelectorExpr
		if sel.rhs.name == 'str' {
			lhs_string_type := g.builtin_string_field_lhs_type(sel.lhs, sel.rhs.name)
			if lhs_string_type != '' {
				selector := if lhs_string_type.ends_with('*') { '->' } else { '.' }
				g.expr(sel.lhs)
				g.sb.write_string('${selector}${escape_c_keyword(sel.rhs.name)}[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	if !g.expr_is_indexable_for_generic_disambiguation(node.lhs)
		&& g.try_emit_generic_fn_value(node.lhs) {
		return
	}
	if node.lhs is ast.Ident {
		if node.lhs.name in g.fixed_array_globals || node.lhs.name == 'rune_maps' {
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	unwrapped_lhs := strip_expr_wrappers(node.lhs)
	if unwrapped_lhs is ast.CallExpr {
		call_name := g.resolve_call_name(unwrapped_lhs.lhs, unwrapped_lhs.args.len)
		if call_name in ['new_array_from_c_array', 'builtin__new_array_from_c_array',
			'builtin__new_array_from_c_array_noscan'] {
			elem_type := g.infer_array_elem_type_from_expr(unwrapped_lhs)
			if elem_type != '' && elem_type != 'array' && elem_type != 'int' && elem_type != 'void' {
				g.sb.write_string('((${elem_type}*)')
				g.expr(node.lhs)
				g.sb.write_string('.data)[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	// Fixed-size array struct fields are emitted as plain C arrays.
	if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
		g.expr(node.lhs)
		g.sb.write_string('[')
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	// Check LHS type from environment to determine indexing strategy
	if raw_type := g.get_raw_type(node.lhs) {
		if raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays - direct indexing
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Array {
			// Dynamic arrays: ((elem_type*)arr.data)[idx]
			mut elem_type := g.types_type_to_c(raw_type.elem_type)
			if node.lhs is ast.ArrayInitExpr {
				value_elem := g.infer_array_init_value_elem_type(node.lhs)
				specialized := specialized_generic_elem_type_from_value(elem_type, value_elem)
				if specialized != '' {
					elem_type = specialized
				}
			}
			g.sb.write_string('((${elem_type}*)')
			g.expr(node.lhs)
			g.sb.write_string('.data)[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Alias {
			match raw_type.base_type {
				types.Array {
					mut elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
					if node.lhs is ast.ArrayInitExpr {
						value_elem := g.infer_array_init_value_elem_type(node.lhs)
						specialized := specialized_generic_elem_type_from_value(elem_type,
							value_elem)
						if specialized != '' {
							elem_type = specialized
						}
					}
					g.sb.write_string('((${elem_type}*)')
					g.expr(node.lhs)
					g.sb.write_string('.data)[')
					g.gen_index_expr_value(node.expr)
					g.sb.write_string(']')
					return
				}
				types.ArrayFixed {
					g.expr(node.lhs)
					g.sb.write_string('[')
					g.gen_index_expr_value(node.expr)
					g.sb.write_string(']')
					return
				}
				types.String {
					g.expr(node.lhs)
					g.sb.write_string('.str[')
					g.gen_index_expr_value(node.expr)
					g.sb.write_string(']')
					return
				}
				else {}
			}
		}
		if raw_type is types.Map {
			g.gen_map_index_expr(node, raw_type, false)
			return
		}
		if raw_type is types.String {
			if node.lhs is ast.SelectorExpr && g.is_fixed_array_selector(node.lhs) {
				g.expr(node.lhs)
				g.sb.write_string('[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
			// Distinguish true string indexing (u8 result) from array-like indexing.
			if out_type := g.get_raw_type(node) {
				out_name := g.types_type_to_c(out_type)
				if out_name !in ['u8', 'byte', 'char'] {
					g.expr(node.lhs)
					g.sb.write_string('[')
					g.gen_index_expr_value(node.expr)
					g.sb.write_string(']')
					return
				}
			}
			g.expr(node.lhs)
			g.sb.write_string('.str[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
			return
		}
		if raw_type is types.Pointer {
			// Pointer to array: use -> accessor
			if raw_type.base_type is types.Array {
				elem_type := g.types_type_to_c(raw_type.base_type.elem_type)
				g.sb.write_string('((${elem_type}*)')
				g.expr(node.lhs)
				g.sb.write_string('->data)[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			} else if raw_type.base_type is types.ArrayFixed {
				// Pointer to fixed array: dereference then index
				g.sb.write_string('(*')
				g.expr(node.lhs)
				g.sb.write_string(')[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			} else if raw_type.base_type is types.Map {
				g.gen_map_index_expr(node, raw_type.base_type as types.Map, true)
				return
			} else if raw_type.base_type is types.Pointer || raw_type.base_type is types.String {
				// Pointer to pointer (e.g. &&char) or pointer to string (e.g. &string used as array):
				// plain C pointer arithmetic
				g.expr(node.lhs)
				g.sb.write_string('[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	if lhs_raw_type := g.get_raw_type(node.lhs) {
		if lhs_raw_type is types.ArrayFixed {
			// Fixed arrays are C arrays: direct indexing
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
			return
		}
	}
	// Sum type narrowing: when indexing a field of a smartcasted sum type variable
	// (e.g. stmt.lhs[0] where stmt is narrowed from ast.Stmt to ast.AssignStmt),
	// get_raw_type fails because the declared type (SumType) has no struct fields.
	// Resolve the field type through the narrowed variant type instead.
	if node.lhs is ast.SelectorExpr && node.lhs.lhs is ast.Ident {
		if narrowed_field_type := g.resolve_narrowed_selector_field_type(node.lhs) {
			if narrowed_field_type is types.Array {
				elem_type := g.types_type_to_c(narrowed_field_type.elem_type)
				g.sb.write_string('((${elem_type}*)')
				g.expr(node.lhs)
				g.sb.write_string('.data)[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
			if narrowed_field_type is types.ArrayFixed {
				g.expr(node.lhs)
				g.sb.write_string('[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	// Fallback: check string-based local type for pointer-to-fixed-array params
	// (e.g., `mut state &[8]u32` → type string `Array_fixed_u32_8*`)
	// get_raw_type may miss these when the env doesn't record the parameter type.
	if node.lhs is ast.Ident {
		if local_type := g.get_local_var_c_type(node.lhs.name) {
			if local_type.starts_with('Array_fixed_') && local_type.ends_with('*') {
				g.sb.write_string('(*')
				g.expr(node.lhs)
				g.sb.write_string(')[')
				g.gen_index_expr_value(node.expr)
				g.sb.write_string(']')
				return
			}
		}
	}
	lhs_type := g.get_expr_type(node.lhs)
	if lhs_type == 'map' || lhs_type.starts_with('Map_') {
		// Try to resolve the full Map type for fallback code generation.
		if map_raw := g.get_raw_type(node.lhs) {
			if map_raw is types.Map {
				g.gen_map_index_expr(node, map_raw, lhs_type.ends_with('*'))
				return
			}
		}
		map_c_type := lhs_type.trim_right('*')
		if info := g.ensure_map_type_info(map_c_type) {
			g.gen_map_index_expr_from_c_types(node, info.key_c_type, info.value_c_type,
				lhs_type.ends_with('*'))
			return
		}
		// Cannot resolve map key/value types — emit a C-level error
		// instead of silently generating incorrect casts.
		g.sb.write_string('/* [TODO] cannot resolve map type for index expr */ 0')
		return
	}
	if lhs_type == 'string' {
		g.expr(node.lhs)
		g.sb.write_string('.str[')
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'string*' {
		elem_type := g.get_expr_type(node)
		if elem_type in ['u8', 'byte', 'char'] {
			g.expr(node.lhs)
			g.sb.write_string('->str[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
		} else {
			g.expr(node.lhs)
			g.sb.write_string('[')
			g.gen_index_expr_value(node.expr)
			g.sb.write_string(']')
		}
		return
	}
	if lhs_type.trim_right('*') in ['strings__Builder', 'Builder'] {
		g.sb.write_string('((u8*)')
		g.expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	// Fixed arrays (including pointer-to-fixed for mut params): direct C array indexing
	if lhs_type.trim_right('*').starts_with('Array_fixed_') {
		if lhs_type.ends_with('*') {
			g.sb.write_string('(*')
			g.expr(node.lhs)
			g.sb.write_string(')[')
		} else {
			g.expr(node.lhs)
			g.sb.write_string('[')
		}
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	// CastExpr to Array_* pointer: transformer-generated cast already points
	// to the correct element type (e.g., for 2D array init), just index directly
	if node.lhs is ast.CastExpr && lhs_type.starts_with('Array_') && lhs_type.ends_with('*') {
		g.expr(node.lhs)
		g.sb.write_string('[')
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	if lhs_type == 'array' || lhs_type.starts_with('Array_') {
		mut elem_type := g.get_expr_type(node)
		if elem_type == '' || elem_type == 'int' {
			if lhs_type.starts_with('Array_fixed_') {
				if finfo := g.collected_fixed_array_types[lhs_type] {
					elem_type = finfo.elem_type
				}
			} else if lhs_type.starts_with('Array_') {
				elem_type = lhs_type['Array_'.len..].trim_right('*')
			} else if lhs_type == 'array' {
				// Unparameterized array - try to infer element type from the call
				if node.lhs is ast.CallExpr {
					call_name := g.resolve_call_name(node.lhs.lhs, node.lhs.args.len)
					if call_name.contains('array_from_c_array') && node.lhs.args.len >= 3 {
						// Extract elem type from sizeof(T) in 3rd arg
						sizeof_arg := node.lhs.args[2]
						if sizeof_arg is ast.KeywordOperator && sizeof_arg.op == .key_sizeof {
							if sizeof_arg.exprs.len > 0 {
								elem_type = g.expr_type_to_c(sizeof_arg.exprs[0])
							}
						}
					} else if node.lhs.args.len > 0 {
						src_type := g.get_expr_type(node.lhs.args[0])
						if src_type.starts_with('Array_') {
							elem_type = src_type['Array_'.len..].trim_right('*')
						}
					}
				}
			}
		}
		if elem_type == '' {
			elem_type = 'u8'
		}
		g.sb.write_string('((${elem_type}*)')
		g.expr(node.lhs)
		if lhs_type.ends_with('*') {
			g.sb.write_string('->data)[')
		} else {
			g.sb.write_string('.data)[')
		}
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	// void*/voidptr/byteptr: cast to u8* for indexing (V treats malloc result as &u8)
	if lhs_type in ['void*', 'voidptr', 'byteptr'] {
		g.sb.write_string('((u8*)')
		g.expr(node.lhs)
		g.sb.write_string(')[')
		g.gen_index_expr_value(node.expr)
		g.sb.write_string(']')
		return
	}
	// When lhs_type is unresolved ('int') and the LHS is a SelectorExpr,
	// try to resolve the actual field type via struct_field_types map.
	// This handles cases like stmt.lhs[0] where stmt is smartcasted from
	// a sum type — get_expr_type/get_raw_type fail because they see the
	// declared sum type, not the narrowed variant.
	if (lhs_type == '' || lhs_type == 'int') && node.lhs is ast.SelectorExpr {
		sel := node.lhs as ast.SelectorExpr
		// Get the struct C type for the selector's LHS. When the LHS ident
		// is narrowed via an is-check, get_expr_type may return the narrowed type.
		mut struct_name := g.get_expr_type(sel.lhs)
		if struct_name == '' || struct_name == 'int' {
			// Also try the env type
			if env_t := g.get_expr_type_from_env(sel.lhs) {
				struct_name = env_t
			}
		}
		if struct_name != '' && struct_name != 'int' {
			if field_type := g.lookup_struct_field_type_by_name(struct_name, sel.rhs.name) {
				if field_type.starts_with('Array_') && !field_type.starts_with('Array_fixed_') {
					elem_type := field_type['Array_'.len..].trim_right('*')
					if elem_type != '' {
						g.sb.write_string('((${elem_type}*)')
						g.expr(node.lhs)
						g.sb.write_string('.data)[')
						g.gen_index_expr_value(node.expr)
						g.sb.write_string(']')
						return
					}
				}
			}
		}
	}
	// Fallback: direct C array indexing
	if lhs_type == '' || lhs_type == 'int' {
		// Generate LHS to temp buffer to analyze the C code and determine
		// if it's a dynamic array that needs .data accessor.
		saved := g.sb
		g.sb = strings.new_builder(256)
		g.expr(node.lhs)
		tmp := g.sb.str()
		g.sb = saved
		// Detect sum type narrowed field access: )->field_name)
		// Extract the struct name from the cast pattern to look up field type.
		if tmp.contains(')->') {
			// Extract field name from the end: ...)->field_name)
			field_part := tmp.all_after_last(')->')
			field_name := field_part.trim_right(')')
			// Extract struct name from the cast: ((struct_name*)(
			struct_part := tmp.all_after('((').all_before('*)(')
			if struct_part != '' && field_name != '' {
				if field_type := g.lookup_struct_field_type_by_name(struct_part, field_name) {
					if field_type.starts_with('Array_') && !field_type.starts_with('Array_fixed_') {
						elem_type := field_type['Array_'.len..].trim_right('*')
						if elem_type != '' {
							g.sb.write_string('((${elem_type}*)')
							g.sb.write_string(tmp)
							g.sb.write_string('.data)[')
							g.gen_index_expr_value(node.expr)
							g.sb.write_string(']')
							return
						}
					}
				}
			}
		}
		g.sb.write_string(tmp)
		g.sb.write_string('[')
		g.expr(node.expr)
		g.sb.write_string(']')
		return
	}
	g.expr(node.lhs)
	g.sb.write_string('[')
	g.expr(node.expr)
	g.sb.write_string(']')
}

fn (mut g Gen) gen_slice_index_expr(node ast.IndexExpr, range ast.RangeExpr) {
	// The transformer normally lowers slices, but unresolved method receivers can
	// still carry raw slice syntax into cgen (for example `buf[..n].bytestr()`).
	lhs_type := g.get_expr_type(node.lhs)
	lhs_base_type := lhs_type.trim_right('*')
	is_string := g.expr_resolves_to_string(node.lhs, lhs_base_type)
	if is_string {
		g.sb.write_string('string__substr(')
	} else if range.end is ast.EmptyExpr {
		g.sb.write_string('array__slice_ni(')
	} else {
		g.sb.write_string('array__slice(')
	}
	g.expr(node.lhs)
	g.sb.write_string(', ')
	if range.start is ast.EmptyExpr {
		g.sb.write_string('0')
	} else {
		g.expr(range.start)
	}
	g.sb.write_string(', ')
	if range.end is ast.EmptyExpr {
		if is_string {
			g.sb.write_string('2147483647')
		} else {
			g.expr(node.lhs)
			if lhs_type.ends_with('*') {
				g.sb.write_string('->len')
			} else {
				g.sb.write_string('.len')
			}
		}
	} else if range.op == .ellipsis {
		g.sb.write_string('(')
		g.expr(range.end)
		g.sb.write_string(' + 1)')
	} else {
		g.expr(range.end)
	}
	g.sb.write_string(')')
}

fn (mut g Gen) expr_resolves_to_string(expr ast.Expr, lhs_base_type string) bool {
	if lhs_base_type == 'string' {
		return true
	}
	if expr is ast.SelectorExpr {
		if g.selector_declared_field_type(expr).trim_right('*') == 'string' {
			return true
		}
		if g.selector_field_type(expr).trim_right('*') == 'string' {
			return true
		}
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.String {
				return true
			}
			types.Pointer {
				return raw_type.base_type is types.String
			}
			types.Alias {
				return raw_type.base_type is types.String
			}
			else {}
		}
	}
	return false
}

fn (mut g Gen) gen_map_index_expr(node ast.IndexExpr, map_type types.Map, lhs_is_ptr bool) {
	// The transformer normally lowers map reads to map__get. Generic functions
	// kept for late function-value specialization can still reach cgen with raw
	// `m[key]`, so emit the same expression-shaped map__get fallback here.
	key_type := g.types_type_to_c(map_type.key_type)
	value_type := g.types_type_to_c(map_type.value_type)
	g.gen_map_index_expr_from_c_types(node, key_type, value_type, lhs_is_ptr)
}

fn (mut g Gen) gen_map_index_expr_from_c_types(node ast.IndexExpr, key_type string, value_type string, lhs_is_ptr bool) {
	actual_lhs_is_ptr := lhs_is_ptr || g.expr_is_pointer(node.lhs)
		|| g.get_expr_type(node.lhs).ends_with('*')
	key_tmp := '_map_key${g.tmp_counter}'
	g.tmp_counter++
	zero_tmp := '_map_zero${g.tmp_counter}'
	g.tmp_counter++
	g.sb.write_string('({ ${key_type} ${key_tmp} = ')
	g.expr(node.expr)
	if value_type.ends_with('*') {
		g.sb.write_string('; ${value_type} ${zero_tmp} = 0; (*(${value_type}*)map__get(')
	} else {
		g.sb.write_string('; ${value_type} ${zero_tmp} = (${value_type}){0}; (*(${value_type}*)map__get(')
	}
	if actual_lhs_is_ptr {
		g.expr(node.lhs)
	} else {
		g.sb.write_string('&(')
		g.expr(node.lhs)
		g.sb.write_string(')')
	}
	g.sb.write_string(', (void*)&${key_tmp}, (void*)&${zero_tmp})); })')
}

fn (g &Gen) eval_comptime_flag(name string) bool {
	match name {
		'macos', 'darwin' {
			return os.user_os() == 'macos'
		}
		'linux' {
			return os.user_os() == 'linux'
		}
		'windows' {
			return os.user_os() == 'windows'
		}
		'bsd' {
			return os.user_os() in ['macos', 'freebsd', 'openbsd', 'netbsd', 'dragonfly']
		}
		'freebsd' {
			return os.user_os() == 'freebsd'
		}
		'openbsd' {
			return os.user_os() == 'openbsd'
		}
		'netbsd' {
			return os.user_os() == 'netbsd'
		}
		'dragonfly' {
			return os.user_os() == 'dragonfly'
		}
		'x64', 'amd64' {
			return g.pref != unsafe { nil } && g.pref.arch == .x64
		}
		'arm64', 'aarch64' {
			return g.pref != unsafe { nil } && g.pref.arch == .arm64
		}
		'debug' {
			return g.pref != unsafe { nil } && g.pref.debug
		}
		'native' {
			return g.pref != unsafe { nil } && (g.pref.backend == .arm64 || g.pref.backend == .x64)
		}
		'builtin_write_buf_to_fd_should_use_c_write' {
			return g.pref != unsafe { nil } && (g.pref.backend == .arm64 || g.pref.backend == .x64)
		}
		'tinyc' {
			return g.pref != unsafe { nil } && (g.pref.backend == .arm64 || g.pref.backend == .x64)
		}
		'prealloc' {
			return g.pref != unsafe { nil } && g.pref.prealloc
		}
		'new_int', 'gcboehm', 'autofree', 'ppc64' {
			return false
		}
		else {
			return g.pref != unsafe { nil } && name in g.pref.user_defines
		}
	}
}

fn (g &Gen) eval_comptime_cond(cond ast.Expr) bool {
	match cond {
		ast.Ident {
			return g.eval_comptime_flag(cond.name)
		}
		ast.PrefixExpr {
			if cond.op == .not {
				return !g.eval_comptime_cond(cond.expr)
			}
		}
		ast.InfixExpr {
			if cond.op == .and {
				return g.eval_comptime_cond(cond.lhs) && g.eval_comptime_cond(cond.rhs)
			}
			if cond.op == .logical_or {
				return g.eval_comptime_cond(cond.lhs) || g.eval_comptime_cond(cond.rhs)
			}
			// Handle `$if T is string`, `$if T.unaliased_typ is $struct`, `$if field.typ is string`, etc.
			if cond.op == .key_is || cond.op == .not_is {
				// Special case: `method.return_type is X` — compare AST exprs by name
				if cond.lhs is ast.SelectorExpr {
					lhs_sel := cond.lhs as ast.SelectorExpr
					if lhs_sel.lhs is ast.Ident && lhs_sel.lhs.name == g.comptime_method_var
						&& lhs_sel.rhs.name == 'return_type' {
						result := g.ast_expr_type_matches(g.comptime_method_return_type, cond.rhs)
						return if cond.op == .key_is { result } else { !result }
					}
				}
				resolved := g.resolve_comptime_is_lhs(cond.lhs)
				if resolved.matched {
					result := g.comptime_type_matches(resolved.typ, cond.rhs)
					return if cond.op == .key_is { result } else { !result }
				}
			}
			// Handle `$if field.is_array` or `$if !field.is_array` style
			if cond.op == .eq || cond.op == .ne {
				// field.name == "x" — string comparison
				if cond.lhs is ast.SelectorExpr {
					lhs_sel := cond.lhs as ast.SelectorExpr
					if lhs_sel.lhs is ast.Ident && lhs_sel.lhs.name == g.comptime_field_var {
						if lhs_sel.rhs.name == 'name' {
							if cond.rhs is ast.BasicLiteral {
								rhs_lit := cond.rhs as ast.BasicLiteral
								rhs_name := strip_literal_quotes(rhs_lit.value)
								matched := g.comptime_field_name == rhs_name
								return if cond.op == .eq { matched } else { !matched }
							}
						}
					}
					// method.name == "x" — string comparison
					if lhs_sel.lhs is ast.Ident && lhs_sel.lhs.name == g.comptime_method_var {
						if lhs_sel.rhs.name == 'name' {
							if cond.rhs is ast.BasicLiteral {
								rhs_lit := cond.rhs as ast.BasicLiteral
								rhs_name := strip_literal_quotes(rhs_lit.value)
								matched := g.comptime_method_name == rhs_name
								return if cond.op == .eq { matched } else { !matched }
							}
						}
					}
				}
			}
		}
		ast.PostfixExpr {
			if cond.op == .question && cond.expr is ast.Ident {
				return g.eval_comptime_flag(cond.expr.name)
			}
		}
		ast.ParenExpr {
			return g.eval_comptime_cond(cond.expr)
		}
		else {}
	}

	return false
}

struct ComptimeResolvedType {
	matched bool
	typ     types.Type = types.Struct{}
}

// resolve_comptime_is_lhs resolves the LHS of a comptime `is` check to a concrete type.
// Handles: T, T.unaliased_typ, field.typ, field.unaliased_typ
fn (g &Gen) resolve_comptime_is_lhs(lhs ast.Expr) ComptimeResolvedType {
	if lhs is ast.Ident {
		// Simple `T is ...` — look up generic type
		if concrete := g.active_generic_types[lhs.name] {
			return ComptimeResolvedType{
				matched: true
				typ:     concrete
			}
		}
	}
	if lhs is ast.SelectorExpr {
		sel := lhs as ast.SelectorExpr
		if sel.lhs is ast.Ident {
			lhs_name := sel.lhs.name
			rhs_name := sel.rhs.name
			// T.unaliased_typ — resolve generic T then strip aliases
			if concrete := g.active_generic_types[lhs_name] {
				if rhs_name == 'unaliased_typ' {
					return ComptimeResolvedType{
						matched: true
						typ:     comptime_unalias_type(concrete)
					}
				}
				return ComptimeResolvedType{
					matched: true
					typ:     concrete
				}
			}
			// field.typ or field.unaliased_typ — use comptime field state
			if lhs_name == g.comptime_field_var {
				if rhs_name == 'typ' || rhs_name == 'unaliased_typ' {
					field_type := g.comptime_field_raw_type
					typ := if rhs_name == 'unaliased_typ' {
						comptime_unalias_type(field_type)
					} else {
						field_type
					}
					return ComptimeResolvedType{
						matched: true
						typ:     typ
					}
				}
			}
		}
	}
	return ComptimeResolvedType{}
}

// ast_expr_type_matches compares two AST type expressions by their normalized name.
// Used for `$if method.return_type is X` where both sides are ast.Exprs.
fn (g &Gen) ast_expr_type_matches(lhs ast.Expr, rhs ast.Expr) bool {
	lhs_name := ast_type_expr_name(lhs)
	rhs_name := ast_type_expr_name(rhs)
	if lhs_name == '' || rhs_name == '' {
		return false
	}
	if lhs_name == rhs_name {
		return true
	}
	// Normalize: bare name on one side matches `mod.Name` on the other if the bare
	// name is the same. e.g., `Result` matches `veb.Result`.
	if lhs_name.ends_with('.' + rhs_name) || rhs_name.ends_with('.' + lhs_name) {
		return true
	}
	// Common aliases
	if (lhs_name == 'byte' && rhs_name == 'u8') || (lhs_name == 'u8' && rhs_name == 'byte') {
		return true
	}
	if (lhs_name == 'int' && rhs_name == 'i32') || (lhs_name == 'i32' && rhs_name == 'int') {
		return true
	}
	return false
}

// ast_type_expr_name extracts a normalized name from an AST type expression.
fn ast_type_expr_name(e ast.Expr) string {
	if e is ast.Ident {
		return e.name
	}
	if e is ast.SelectorExpr {
		lhs_name := ast_type_expr_name(e.lhs)
		if lhs_name == '' {
			return e.rhs.name
		}
		return lhs_name + '.' + e.rhs.name
	}
	if e is ast.EmptyExpr {
		return 'void'
	}
	return ''
}

// comptime_unalias_type strips Alias layers from a type.
fn comptime_unalias_type(t types.Type) types.Type {
	mut cur := t
	for {
		if cur is types.Alias {
			a := cur as types.Alias
			cur = a.base_type
			continue
		}
		break
	}
	return cur
}

// comptime_type_matches checks if a resolved type matches the RHS of a comptime `is` check.
// Handles: Ident (concrete type name), ComptimeExpr ($struct, $enum, etc.), PrefixExpr (&type)
fn (g &Gen) comptime_type_matches(typ types.Type, rhs ast.Expr) bool {
	if rhs is ast.Ident {
		return g.comptime_matches_type_name(typ, rhs.name)
	}
	if rhs is ast.SelectorExpr {
		return g.comptime_matches_selector_type_name(typ, rhs)
	}
	if rhs is ast.ComptimeExpr {
		// $struct, $enum, $int, $float, $array, $map, $option, etc.
		if rhs.expr is ast.Ident {
			return g.comptime_matches_keyword(typ, rhs.expr.name)
		}
		return false
	}
	if rhs is ast.PrefixExpr {
		// e.g., &int — pointer to type
		return false
	}
	return false
}

fn (g &Gen) comptime_matches_selector_type_name(typ types.Type, selector ast.SelectorExpr) bool {
	if selector.lhs !is ast.Ident {
		return false
	}
	lhs_ident := selector.lhs as ast.Ident
	mod_name := lhs_ident.name
	type_name := selector.rhs.name
	dot_name := '${mod_name}.${type_name}'
	c_name := '${mod_name}__${type_name}'
	resolved_name := typ.name()
	if resolved_name == dot_name || resolved_name == c_name {
		return true
	}
	resolved_c_name := g.types_type_to_c(typ)
	if mod_name == g.cur_module && (resolved_name == type_name || resolved_c_name == type_name) {
		return true
	}
	return resolved_c_name == c_name
}

// comptime_matches_type_name checks if a type matches a concrete type name.
fn (g &Gen) comptime_matches_type_name(typ types.Type, name string) bool {
	type_name := typ.name()
	if type_name == name {
		return true
	}
	// Normalize aliases
	if (name == 'byte' && type_name == 'u8') || (name == 'u8' && type_name == 'byte') {
		return true
	}
	if (name == 'int' && type_name == 'i32') || (name == 'i32' && type_name == 'int') {
		return true
	}
	// Check C type name
	c_name := g.types_type_to_c(typ)
	if c_name == name {
		return true
	}
	if g.comptime_type_implements_interface(typ, name) {
		return true
	}
	return false
}

fn comptime_type_name_short(name string) string {
	dot_short := name.all_after_last('.')
	return dot_short.all_after_last('__')
}

fn comptime_type_name_matches(candidate string, target string) bool {
	if candidate == target {
		return true
	}
	if candidate.replace('.', '__') == target.replace('.', '__') {
		return true
	}
	return comptime_type_name_short(candidate) == comptime_type_name_short(target)
}

fn (g &Gen) comptime_struct_for_type(typ types.Type) ?types.Struct {
	if typ is types.Struct {
		if typ.implements.len > 0 || typ.fields.len > 0 {
			return typ
		}
	}
	type_name := typ.name()
	mut candidates := []string{}
	if type_name != '' {
		candidates << type_name
		candidates << type_name.replace('__', '.')
		candidates << type_name.all_after_last('__')
		candidates << type_name.all_after_last('.')
	}
	c_name := g.types_type_to_c(typ).trim_space().trim_right('*')
	if c_name != '' {
		candidates << c_name
		candidates << c_name.replace('__', '.')
		candidates << c_name.all_after_last('__')
	}
	for candidate in candidates {
		mut mod_name := ''
		mut short_name := candidate
		if candidate.contains('.') {
			mod_name = candidate.all_before_last('.')
			short_name = candidate.all_after_last('.')
		} else if candidate.contains('__') {
			mod_name = candidate.all_before_last('__').replace('__', '.')
			short_name = candidate.all_after_last('__')
		}
		if mod_name != '' {
			if scope := g.env_scope(mod_name) {
				if resolved := scope.lookup_type_parent(short_name, 0) {
					if resolved is types.Struct {
						return resolved
					}
				}
			}
		}
		if scope := g.env_scope(g.cur_module) {
			if resolved := scope.lookup_type_parent(short_name, 0) {
				if resolved is types.Struct {
					return resolved
				}
			}
		}
	}
	return none
}

fn (g &Gen) comptime_type_implements_interface(typ types.Type, interface_name string) bool {
	st := g.comptime_struct_for_type(typ) or { return false }
	for impl_name in st.implements {
		if comptime_type_name_matches(impl_name, interface_name) {
			return true
		}
		if interface_name.contains('__') || interface_name.contains('.') {
			continue
		}
		if g.cur_module != '' && g.cur_module != 'main'
			&& comptime_type_name_matches(impl_name, '${g.cur_module}.${interface_name}') {
			return true
		}
	}
	return false
}

// comptime_matches_keyword checks if a type matches a comptime keyword like $struct, $enum, etc.
fn (g &Gen) comptime_matches_keyword(typ types.Type, keyword string) bool {
	match keyword {
		'struct' {
			if typ is types.Struct {
				return true
			}
			c_name := g.types_type_to_c(typ).trim_space().trim_right('*')
			if c_name != '' {
				return g.has_struct_type_by_c_name(c_name)
			}
			return false
		}
		'enum' {
			return typ is types.Enum
		}
		'int' {
			name := typ.name()
			return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'byte',
				'isize', 'usize']
		}
		'float' {
			name := typ.name()
			return name in ['f32', 'f64']
		}
		'array' {
			return typ is types.Array
		}
		'map' {
			return typ is types.Map
		}
		'option' {
			return typ is types.OptionType
		}
		'string' {
			return typ is types.String || typ.name() == 'string'
		}
		else {
			return false
		}
	}
}

fn (g &Gen) has_struct_type_by_c_name(c_name string) bool {
	if g.env == unsafe { nil } || c_name == '' {
		return false
	}
	mut mod_name := ''
	mut struct_name := c_name
	if idx := c_name.index('__') {
		mod_name = c_name[..idx]
		struct_name = c_name[idx + 2..]
	}
	mut tried := map[string]bool{}
	if mod_name != '' {
		tried[mod_name] = true
		if scope := g.env_scope(mod_name) {
			if obj := scope.lookup_parent(struct_name, 0) {
				return obj.typ() is types.Struct
			}
		}
	}
	cur_mod := if g.cur_module != '' { g.cur_module } else { 'main' }
	for try_mod in [cur_mod, 'main', 'builtin'] {
		if tried[try_mod] {
			continue
		}
		tried[try_mod] = true
		if scope := g.env_scope(try_mod) {
			if obj := scope.lookup_parent(struct_name, 0) {
				return obj.typ() is types.Struct
			}
		}
	}
	for file in g.files {
		file_mod := file.mod
		if file_mod == '' || tried[file_mod] {
			continue
		}
		tried[file_mod] = true
		if scope := g.env_scope(file_mod) {
			if obj := scope.lookup_parent(struct_name, 0) {
				return obj.typ() is types.Struct
			}
		}
	}
	return false
}

// gen_comptime_field_selector handles comptime field access patterns:
// - field.name → V string literal with field name
// - field.name.str → C string literal
// - field.name.len → integer literal
// - field.attrs → array literal
// - val.$(field.name) → val->field_name or val.field_name
// Returns true if the selector was handled.
fn (mut g Gen) gen_comptime_field_selector(sel ast.SelectorExpr) bool {
	rhs_name := sel.rhs.name
	// Handle val.$(field.name) — comptime selector placeholder
	if is_comptime_selector_rhs_name(rhs_name) {
		lhs_expr := sel.lhs
		mut use_ptr := g.selector_use_ptr(lhs_expr)
		if lhs_expr is ast.Ident {
			if lhs_expr.name in g.cur_fn_mut_params {
				use_ptr = true
			} else if local_type := g.local_var_c_type_for_expr(lhs_expr) {
				use_ptr = local_type.ends_with('*') || local_type == 'chan'
			}
		}
		g.expr(lhs_expr)
		selector := if use_ptr { '->' } else { '.' }
		g.sb.write_string('${selector}${escape_c_keyword(g.comptime_field_name)}')
		return true
	}
	// Handle field.name, field.typ, field.is_mut, etc.
	if sel.lhs is ast.Ident {
		if sel.lhs.name == g.comptime_field_var {
			match rhs_name {
				'name' {
					g.sb.write_string(c_static_v_string_expr(g.comptime_field_name))
					return true
				}
				'typ' {
					g.sb.write_string(comptime_type_idx_literal(g.comptime_field_type))
					return true
				}
				'unaliased_typ' {
					unaliased := comptime_unalias_type(g.comptime_field_raw_type)
					g.sb.write_string(comptime_type_idx_literal(g.types_type_to_c(unaliased)))
					return true
				}
				'is_mut' {
					g.sb.write_string('true')
					return true
				}
				'is_pub' {
					g.sb.write_string('true')
					return true
				}
				'is_embed' {
					g.sb.write_string(if g.comptime_field_is_embed { 'true' } else { 'false' })
					return true
				}
				'is_shared' {
					g.sb.write_string('false')
					return true
				}
				'is_atomic' {
					g.sb.write_string('false')
					return true
				}
				'is_array' {
					is_arr := g.comptime_field_raw_type is types.Array
					g.sb.write_string(if is_arr { 'true' } else { 'false' })
					return true
				}
				'is_map' {
					is_map := g.comptime_field_raw_type is types.Map
					g.sb.write_string(if is_map { 'true' } else { 'false' })
					return true
				}
				'is_option' {
					is_opt := g.comptime_field_raw_type is types.OptionType
					g.sb.write_string(if is_opt { 'true' } else { 'false' })
					return true
				}
				'is_chan' {
					is_chan := g.comptime_field_raw_type is types.Channel
					g.sb.write_string(if is_chan { 'true' } else { 'false' })
					return true
				}
				'is_enum' {
					field_type := comptime_unalias_type(g.comptime_field_raw_type)
					g.sb.write_string(if field_type is types.Enum { 'true' } else { 'false' })
					return true
				}
				'is_struct' {
					field_type := comptime_unalias_type(g.comptime_field_raw_type)
					g.sb.write_string(if field_type is types.Struct { 'true' } else { 'false' })
					return true
				}
				'is_alias' {
					g.sb.write_string(if g.comptime_field_raw_type is types.Alias {
						'true'
					} else {
						'false'
					})
					return true
				}
				'attrs' {
					if g.comptime_field_attrs.len == 0 {
						g.sb.write_string('((Array_string){0})')
					} else {
						g.sb.write_string('new_array_from_c_array(${g.comptime_field_attrs.len}, ${g.comptime_field_attrs.len}, sizeof(string), (string[${g.comptime_field_attrs.len}]){')
						for i, attr in g.comptime_field_attrs {
							if i > 0 {
								g.sb.write_string(', ')
							}
							g.sb.write_string(c_static_v_string_expr(attr))
						}
						g.sb.write_string('})')
					}
					return true
				}
				else {}
			}
		}
	}
	// Handle field.name.str, field.name.len
	if sel.lhs is ast.SelectorExpr {
		inner := sel.lhs as ast.SelectorExpr
		if inner.lhs is ast.Ident && inner.lhs.name == g.comptime_field_var {
			if inner.rhs.name == 'name' {
				match rhs_name {
					'str' {
						g.sb.write_string('"${g.comptime_field_name}"')
						return true
					}
					'len' {
						g.sb.write_string('${g.comptime_field_name.len}')
						return true
					}
					else {}
				}
			}
		}
	}
	return false
}

fn comptime_type_idx_literal(c_type string) string {
	mut type_name := c_type.trim_space().trim_right('*')
	if type_name.starts_with('_option_') {
		type_name = type_name['_option_'.len..]
	}
	if type_name.starts_with('_result_') {
		type_name = type_name['_result_'.len..]
	}
	v_name := c_name_to_v_name(type_name)
	idx := match v_name {
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

	return idx.str()
}

// gen_comptime_method_selector handles comptime method access patterns inside
// `$for method in T.methods` loop bodies:
// - method.name → V string literal
// - method.location → V string literal (empty for now)
// - method.attrs → []string array
// - method.return_type → integer literal (placeholder type id 0)
// - method.typ → integer literal (placeholder)
// - method.args → []FunctionParam array literal
// - method.args.len → integer literal
// - method.name.str/.len → C string / int literal
// Returns true if the selector was handled.
fn (mut g Gen) gen_comptime_method_selector(sel ast.SelectorExpr) bool {
	rhs_name := sel.rhs.name
	if sel.lhs is ast.Ident {
		if sel.lhs.name == g.comptime_method_var {
			match rhs_name {
				'name' {
					g.sb.write_string(c_static_v_string_expr(g.comptime_method_name))
					return true
				}
				'location' {
					g.sb.write_string(c_static_v_string_expr(''))
					return true
				}
				'return_type' {
					g.sb.write_string('0 /* method.return_type */')
					return true
				}
				'typ' {
					g.sb.write_string('0 /* method.typ */')
					return true
				}
				'attrs' {
					g.write_string_array_literal(g.comptime_method_attrs)
					return true
				}
				'args' {
					g.write_function_param_array_literal(g.comptime_method_args, 0)
					return true
				}
				else {}
			}
		}
	}
	// Handle method.name.str / method.name.len / method.args.len
	if sel.lhs is ast.SelectorExpr {
		inner := sel.lhs as ast.SelectorExpr
		if inner.lhs is ast.Ident && inner.lhs.name == g.comptime_method_var {
			inner_rhs := inner.rhs.name
			if inner_rhs == 'name' {
				match rhs_name {
					'str' {
						g.sb.write_string('"${g.comptime_method_name}"')
						return true
					}
					'len' {
						g.sb.write_string('${g.comptime_method_name.len}')
						return true
					}
					else {}
				}
			}
			if inner_rhs == 'args' && rhs_name == 'len' {
				g.sb.write_string('${g.comptime_method_args.len}')
				return true
			}
			if inner_rhs == 'attrs' && rhs_name == 'len' {
				g.sb.write_string('${g.comptime_method_attrs.len}')
				return true
			}
		}
	}
	return false
}

// write_string_array_literal emits a V []string literal as a C
// new_array_from_c_array(...) call.
fn (mut g Gen) write_string_array_literal(items []string) {
	if items.len == 0 {
		g.sb.write_string('((Array_string){0})')
		return
	}
	g.sb.write_string('new_array_from_c_array(${items.len}, ${items.len}, sizeof(string), (string[${items.len}]){')
	for i, item in items {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.sb.write_string(c_static_v_string_expr(item))
	}
	g.sb.write_string('})')
}

// write_function_param_array_literal emits a V []FunctionParam literal.
// `skip` is the number of leading params to drop (e.g. 1 to skip receiver — but
// `method.args` historically did NOT include the receiver, so default is 0).
fn (mut g Gen) write_function_param_array_literal(params []ast.Parameter, skip int) {
	effective := if skip < params.len { params[skip..] } else { []ast.Parameter{} }
	if effective.len == 0 {
		g.sb.write_string('((Array_FunctionParam){0})')
		return
	}
	g.sb.write_string('new_array_from_c_array(${effective.len}, ${effective.len}, sizeof(FunctionParam), (FunctionParam[${effective.len}]){')
	for i, p in effective {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.sb.write_string('{.typ = 0, .name = ')
		g.sb.write_string(c_static_v_string_expr(p.name))
		g.sb.write_string('}')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_comptime_if_branch(stmts []ast.Stmt) {
	if stmts.len == 0 {
		g.sb.write_string('0')
		return
	}
	if stmts.len == 1 && stmts[0] is ast.ExprStmt {
		stmt := stmts[0] as ast.ExprStmt
		g.expr(stmt.expr)
		return
	}
	g.sb.write_string('({ ')
	for i, stmt in stmts {
		if i < stmts.len - 1 {
			g.gen_stmt(stmt)
		}
	}
	last := stmts[stmts.len - 1]
	if last is ast.ExprStmt {
		g.expr(last.expr)
		g.sb.write_string('; ')
	} else {
		g.gen_stmt(last)
		g.sb.write_string('0; ')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_comptime_if_expr(node ast.IfExpr) {
	if g.eval_comptime_cond(node.cond) {
		g.gen_comptime_if_branch(node.stmts)
		return
	}
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.gen_comptime_if_branch(else_if.stmts)
		} else {
			g.gen_comptime_if_expr(else_if)
		}
		return
	}
	if node.else_expr !is ast.EmptyExpr {
		g.expr(node.else_expr)
		return
	}
	g.sb.write_string('0')
}

fn (mut g Gen) gen_comptime_expr(node ast.ComptimeExpr) {
	if is_veb_html_comptime_call(node.expr) {
		g.sb.write_string('(veb__Result){0}')
		return
	}
	if node.expr is ast.Ident {
		name := node.expr.name
		match name {
			'FN', 'METHOD', 'FUNCTION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr(fn_name))
			}
			'LOCATION' {
				fn_name := g.cur_fn_name
				g.sb.write_string(c_static_v_string_expr('${g.cur_module}.${fn_name}'))
			}
			'MOD' {
				mod_name := g.cur_module
				g.sb.write_string(c_static_v_string_expr(mod_name))
			}
			'FILE' {
				g.sb.write_string(c_v_string_expr_from_ptr_len('__FILE__', 'sizeof(__FILE__)-1',
					true))
			}
			'LINE' {
				g.sb.write_string('__LINE__')
			}
			'VCURRENTHASH' {
				g.sb.write_string(c_static_v_string_expr(g.get_v_hash()))
			}
			'VEXE' {
				g.sb.write_string('__vexe_path()')
			}
			else {
				g.sb.write_string('${c_empty_v_string_expr()} /* unknown comptime: ${name} */')
			}
		}

		return
	}
	if node.expr is ast.CallExpr {
		if node.expr.lhs is ast.Ident {
			match node.expr.lhs.name {
				'res' {
					g.sb.write_string('false')
					return
				}
				'env' {
					if node.expr.args.len > 0 {
						env_key := comptime_string_arg(node.expr.args[0])
						g.sb.write_string(c_static_v_string_expr(os.getenv(env_key)))
					} else {
						g.sb.write_string(c_empty_v_string_expr())
					}
					return
				}
				'compile_error' {
					msg := if node.expr.args.len > 0 {
						comptime_string_arg(node.expr.args[0])
					} else {
						'compile error'
					}
					panic('comptime compile_error: ${msg}')
				}
				'compile_warn' {
					// Keep compilation moving; warnings are not surfaced by cleanc yet.
					g.sb.write_string(c_empty_v_string_expr())
					return
				}
				else {}
			}
		}
	}
	if node.expr is ast.CallOrCastExpr {
		if node.expr.lhs is ast.Ident {
			match node.expr.lhs.name {
				'res' {
					g.sb.write_string('false')
					return
				}
				'env' {
					env_key := comptime_string_arg(node.expr.expr)
					g.sb.write_string(c_static_v_string_expr(os.getenv(env_key)))
					return
				}
				'compile_error' {
					msg := comptime_string_arg(node.expr.expr)
					panic('comptime compile_error: ${msg}')
				}
				'compile_warn' {
					g.sb.write_string(c_empty_v_string_expr())
					return
				}
				else {}
			}
		}
	}
	// Fallback: emit the inner expression
	g.expr(node.expr)
}

fn is_veb_html_comptime_call(expr ast.Expr) bool {
	match expr {
		ast.CallExpr {
			return is_veb_html_comptime_lhs(expr.lhs)
		}
		ast.CallOrCastExpr {
			return is_veb_html_comptime_lhs(expr.lhs)
		}
		else {
			return false
		}
	}
}

fn is_veb_html_comptime_lhs(lhs ast.Expr) bool {
	if lhs is ast.SelectorExpr {
		if lhs.lhs is ast.Ident {
			return lhs.lhs.name == 'veb' && lhs.rhs.name == 'html'
		}
	}
	return false
}

fn comptime_string_arg(expr ast.Expr) string {
	match expr {
		ast.StringLiteral {
			return expr.value
		}
		ast.BasicLiteral {
			if expr.kind == .string {
				return expr.value
			}
			return expr.value
		}
		else {
			return expr.name()
		}
	}
}

fn (mut g Gen) expr_to_string(expr ast.Expr) string {
	saved_sb := g.sb
	mut tmp_sb := strings.new_builder(64)
	g.sb = tmp_sb
	g.expr(expr)
	value := g.sb.str()
	g.sb = saved_sb
	return value
}

fn is_header_type_only_const_expr(expr ast.Expr) bool {
	return match expr {
		ast.Type {
			true
		}
		ast.Ident {
			name := expr.name
				name in ['bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'rune', 'string', 'u8', 'u16', 'u32', 'u64', 'usize', 'void', 'voidptr', 'byteptr', 'charptr']
				|| name.starts_with('&') || name.starts_with('[]') || name.starts_with('?')
				|| name.starts_with('!') || name.contains('[') || name.contains('__')
		}
		else {
			false
		}
	}
}

fn (mut g Gen) gen_heap_address_of_cast_expr(node ast.CastExpr, target_type string) bool {
	if target_type == '' || target_type == 'int' || target_type.ends_with('*') {
		return false
	}
	if node.expr is ast.PrefixExpr && node.expr.op == .amp {
		return false
	}
	if target_type !in g.sum_type_variants && node.expr !is ast.AsCastExpr {
		return false
	}
	tmp_name := '_heap_t${g.tmp_counter}'
	g.tmp_counter++
	g.sb.write_string('({ ${target_type}* ${tmp_name} = (${target_type}*)malloc(sizeof(${target_type})); *${tmp_name} = ')
	g.expr(ast.Expr(node))
	g.sb.write_string('; ${tmp_name}; })')
	return true
}

fn (g &Gen) cast_target_is_aggregate_value(type_name string) bool {
	name := type_name.trim_space()
	if name == '' || is_type_name_pointer_like(name) || name in primitive_types
		|| g.is_enum_type(name) {
		return false
	}
	return name !in ['void', 'void*', 'voidptr', 'char*', 'charptr', 'byteptr']
}

fn same_aggregate_cast_type(expr_type string, type_name string) bool {
	expr_name := expr_type.trim_space()
	target_name := type_name.trim_space()
	if expr_name == '' || target_name == '' || is_type_name_pointer_like(expr_name) {
		return false
	}
	if expr_name == target_name {
		return true
	}
	return (!expr_name.contains('__') || !target_name.contains('__'))
		&& short_type_name(expr_name) == short_type_name(target_name)
}

fn (mut g Gen) gen_cast_expr(node ast.CastExpr) {
	mut type_name := g.expr_type_to_c(node.typ)
	if resolved_type := g.resolved_sum_data_cast_type(node) {
		type_name = resolved_type
	}
	if type_name.starts_with('_option_') && is_none_like_expr(node.expr) {
		g.sb.write_string('(${type_name}){ .state = 2 }')
		return
	}
	if node.expr is ast.BasicLiteral && node.expr.kind == .number && node.expr.value == '0' {
		if type_name !in primitive_types && !type_name.ends_with('*') && !g.is_enum_type(type_name)
			&& type_name !in ['void*', 'char*', 'byteptr', 'charptr', 'voidptr'] {
			g.sb.write_string(zero_value_for_type(type_name))
			return
		}
	}
	if type_name.starts_with('_option_') || type_name.starts_with('_result_') {
		expr_type := g.get_expr_type(node.expr)
		if expr_type == type_name {
			g.expr(node.expr)
			return
		}
		if node.expr is ast.CastExpr && g.expr_type_to_c(node.expr.typ) == type_name {
			g.expr(node.expr)
			return
		}
	}
	if type_name.starts_with('_option_') {
		value_type := option_value_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _opt = (${type_name}){ .state = 2 }; ${value_type} _val = ')
			// For interface value types, wrap the expression in an interface cast
			if g.is_interface_type(value_type) && g.gen_interface_cast(value_type, node.expr) {
				// interface wrapping handled
			} else if g.gen_auto_deref_value_param_arg(value_type, node.expr) {
			} else {
				g.expr(node.expr)
			}
			g.sb.write_string('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; })')
			return
		}
	}
	if type_name.starts_with('_result_') {
		value_type := g.result_value_c_type(type_name)
		if value_type != '' && value_type != 'void' {
			g.sb.write_string('({ ${type_name} _res = (${type_name}){0}; ${value_type} _val = ')
			if !g.gen_auto_deref_value_param_arg(value_type, node.expr) {
				g.expr(node.expr)
			}
			g.sb.write_string('; _result_ok(&_val, (_result*)&_res, sizeof(_val)); _res; })')
			return
		}
	}
	if type_name == 'void' {
		// Preserve side effects when discarding expression results.
		// e.g. `(void)(f())` must still call `f()`.
		mut inner := strings.new_builder(64)
		saved := g.sb
		g.sb = inner
		g.expr(node.expr)
		inner_str := g.sb.str()
		g.sb = saved
		if inner_str == '' {
			g.sb.write_string('((void)0)')
		} else {
			g.sb.write_string('((void)(${inner_str}))')
		}
		return
	}
	if type_name !in g.sum_type_variants && g.cast_target_is_aggregate_value(type_name)
		&& type_name.contains('__') && g.expr_is_pointer(node.expr) {
		g.sb.write_string('((${type_name}*)(')
		g.expr(node.expr)
		g.sb.write_string('))')
		return
	}
	expr_type := g.get_expr_type(node.expr)
	if expr_type.starts_with('_result_') && g.result_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	if expr_type.starts_with('_option_') && option_value_type(expr_type) != '' {
		g.sb.write_string('((${type_name})(')
		g.gen_unwrapped_value_expr(node.expr)
		g.sb.write_string('))')
		return
	}
	if g.expr_is_sum_variant_extract(node.expr, type_name) {
		g.gen_as_cast_expr(ast.AsCastExpr{
			expr: node.expr
			typ:  node.typ
			pos:  node.pos
		})
		return
	}
	// Handle sum type and interface casts
	if type_name in g.sum_type_variants {
		g.gen_type_cast_expr(type_name, node.expr)
		return
	}
	if g.cast_target_is_aggregate_value(type_name) && same_aggregate_cast_type(expr_type, type_name) {
		g.expr(node.expr)
		return
	}
	if g.gen_interface_cast(type_name, node.expr) {
		return
	}
	// Capture inner expression to check if it's empty
	mut inner := strings.new_builder(64)
	saved := g.sb
	g.sb = inner
	g.expr(node.expr)
	inner_str := g.sb.str()
	g.sb = saved
	if inner_str == '' {
		// Empty inner expression (e.g. unresolved C function call) - emit no-op
		g.sb.write_string('((void)0)')
	} else {
		// When casting a pointer field (.data, .metas, const_s, pvalue) to a
		// scalar type like u8, the intended C semantics is a pointer cast
		// (u8*), not a value cast (u8).  Detect this and add the pointer star.
		mut cast_name := type_name
		if !type_name.ends_with('*') && type_name in ['u8', 'i8', 'char'] {
			if inner_str.ends_with('.data)') || inner_str.ends_with('.data)')
				|| inner_str.ends_with('->data)') || inner_str.ends_with('.metas)')
				|| inner_str.ends_with('->metas)')
				|| (node.expr is ast.Ident && node.expr.name in ['const_s', 'pvalue']) {
				cast_name = '${type_name}*'
			}
		}
		if heap_payload_type := g.heap_sum_payload_cast_type(node, type_name) {
			g.sb.write_string('((${cast_name})(*((${heap_payload_type}*)(${inner_str}))))')
			return
		}
		g.sb.write_string('((${cast_name})(${inner_str}))')
	}
}

fn (mut g Gen) heap_sum_payload_cast_type(node ast.CastExpr, type_name string) ?string {
	if type_name.ends_with('*') || g.is_scalar_sum_payload_type(type_name) {
		return none
	}
	if !g.is_sum_data_payload_selector(node.expr) {
		return none
	}
	return type_name
}

fn (mut g Gen) cast_expr_is_sum_variant_extract(node ast.CastExpr, target_type string) bool {
	return g.expr_is_sum_variant_extract(node.expr, target_type)
}

fn (mut g Gen) expr_is_sum_variant_extract(expr ast.Expr, target_type string) bool {
	if target_type == '' || target_type == 'int' || target_type in g.sum_type_variants {
		return false
	}
	source_sum_type := g.cast_source_sum_type(expr)
	if source_sum_type == '' {
		return false
	}
	variants := g.get_sum_type_variants_for(source_sum_type)
	if variants.len == 0 {
		return false
	}
	for variant in variants {
		if sum_type_variant_matches(variant, target_type) {
			return true
		}
	}
	return false
}

fn (mut g Gen) cast_source_sum_type(expr ast.Expr) string {
	match expr {
		ast.ParenExpr, ast.ModifierExpr {
			return g.cast_source_sum_type(expr.expr)
		}
		ast.SelectorExpr {
			for candidate in [
				g.selector_declared_field_type(expr).trim_space().trim_right('*'),
				g.selector_storage_field_type(expr).trim_space().trim_right('*'),
			] {
				if candidate != '' && g.get_sum_type_variants_for(candidate).len > 0 {
					return candidate
				}
			}
		}
		ast.CallExpr {
			elem_type := g.infer_array_method_elem_type(expr).trim_space().trim_right('*')
			if elem_type != '' && g.get_sum_type_variants_for(elem_type).len > 0 {
				return elem_type
			}
		}
		else {}
	}

	mut source_sum_type := g.get_expr_type(expr).trim_space().trim_right('*')
	if expr is ast.PrefixExpr && expr.op == .mul {
		inner := g.unwrap_parens(expr.expr)
		if inner is ast.CastExpr {
			cast_source_type := g.expr_type_to_c(inner.typ).trim_space().trim_right('*')
			if g.get_sum_type_variants_for(cast_source_type).len > 0 {
				source_sum_type = cast_source_type
			}
		}
	}
	if raw_type := g.get_raw_type(expr) {
		match raw_type {
			types.SumType {
				source_sum_type = g.types_type_to_c(raw_type)
			}
			types.Pointer {
				if raw_type.base_type is types.SumType {
					source_sum_type = g.types_type_to_c(raw_type.base_type)
				}
			}
			else {}
		}
	}
	if g.get_sum_type_variants_for(source_sum_type).len > 0 {
		return source_sum_type
	}
	return ''
}

fn (mut g Gen) gen_as_cast_expr(node ast.AsCastExpr) {
	target_type_name := g.expr_type_to_c(node.typ)
	if g.gen_nested_sum_as_cast_for_selector_source(node.expr, target_type_name) {
		return
	}
	mut type_name := target_type_name
	mut source_sum_type := g.cast_source_sum_type(node.expr)
	if source_sum_type == '' {
		source_sum_type = g.get_expr_type(node.expr).trim_space().trim_right('*')
	}
	mut raw_source_expr := ''
	if node.expr is ast.SelectorExpr {
		declared_field_type := g.selector_declared_field_type(node.expr).trim_right('*')
		if g.get_sum_type_variants_for(declared_field_type).len > 0 {
			source_sum_type = declared_field_type
			raw_source_expr = g.raw_selector_expr_code(node.expr)
		}
	}
	if raw_source_expr == '' {
		if raw_type := g.get_raw_type(node.expr) {
			match raw_type {
				types.SumType {
					source_sum_type = g.types_type_to_c(raw_type)
				}
				types.Pointer {
					if raw_type.base_type is types.SumType {
						source_sum_type = g.types_type_to_c(raw_type.base_type)
					}
				}
				else {}
			}
		}
	}
	mut variant_field := g.sum_type_variant_field_name(source_sum_type, target_type_name)
	type_name = g.sum_type_variant_payload_type(source_sum_type, target_type_name, variant_field)
	short_name := if type_name.contains('__') {
		type_name.all_after_last('__')
	} else {
		type_name
	}
	// Interface cast: `iface as T` => `*((T*)iface._object)`
	mut source_is_iface := false
	if raw_type := g.get_raw_type(node.expr) {
		source_is_iface = raw_type is types.Interface
			|| (raw_type is types.Pointer && raw_type.base_type is types.Interface)
	}
	// Fallback: check C type name if raw_type didn't resolve to interface
	if !source_is_iface {
		source_c_type := g.get_expr_type(node.expr)
		if source_c_type != '' {
			source_is_iface = g.is_interface_type(source_c_type.trim_right('*'))
		}
	}
	if source_is_iface {
		sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
		g.sb.write_string('(*((${type_name}*)(')
		g.expr(node.expr)
		g.sb.write_string('${sep}_object)))')
		return
	}
	mut inner := strings.new_builder(64)
	saved := g.sb
	mut inner_str := raw_source_expr
	if inner_str == '' {
		g.sb = inner
		g.expr(node.expr)
		inner_str = g.sb.str()
	}
	g.sb = saved
	if g.contains_as_cast_expr(node.expr)
		&& !g.as_cast_expr_extracts_direct_sum_variant(node.expr, source_sum_type, target_type_name) {
		g.sb.write_string('((${type_name})(${inner_str}))')
		return
	}
	if node.expr is ast.SelectorExpr {
		storage_sum_type := g.selector_storage_field_type(node.expr).trim_right('*')
		if storage_sum_type != '' && storage_sum_type != source_sum_type
			&& g.get_sum_type_variants_for(storage_sum_type).len > 0
			&& !g.sum_type_has_direct_variant(source_sum_type, target_type_name) {
			if path := g.sum_variant_tag_path(storage_sum_type, target_type_name, []string{}) {
				if path.len > 1 {
					nested_target_type := path[path.len - 1].payload_type
					if g.gen_nested_sum_as_cast(g.raw_selector_expr_code(node.expr), node.expr,
						nested_target_type, path)
					{
						return
					}
				}
			}
		}
	}
	mut emitted_nested_sum_cast := false
	if !g.sum_type_has_direct_variant(source_sum_type, target_type_name) {
		if path := g.sum_variant_tag_path(source_sum_type, target_type_name, []string{}) {
			if path.len > 1 {
				nested_target_type := path[path.len - 1].payload_type
				if g.gen_nested_sum_as_cast(inner_str, node.expr, nested_target_type, path) {
					emitted_nested_sum_cast = true
					return
				}
			}
		}
	}
	if !emitted_nested_sum_cast && node.expr is ast.SelectorExpr
		&& !g.sum_type_has_direct_variant(source_sum_type, target_type_name) {
		if path := g.nested_sum_path_from_narrowed_source(source_sum_type, target_type_name) {
			nested_target_type := path[path.len - 1].payload_type
			if g.gen_nested_sum_as_cast(inner_str, node.expr, nested_target_type, path) {
				return
			}
		}
	}
	marker := ')->_data._${variant_field}'
	if idx := inner_str.index(marker) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	marker2 := ')->_data)._${variant_field}'
	if idx := inner_str.index(marker2) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	short_marker := ')->_data._${short_name}'
	if idx := inner_str.index(short_marker) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	short_marker2 := ')->_data)._${short_name}'
	if idx := inner_str.index(short_marker2) {
		simplified := inner_str[..idx + 1]
		g.sb.write_string('(*(${simplified}))')
		return
	}
	expr_is_declared_sum_value := g.expr_is_declared_sum_value(node.expr)
	if (inner_str.starts_with('((${type_name}*)') || inner_str.starts_with('(${type_name}*)'))
		&& !expr_is_declared_sum_value {
		g.sb.write_string('(*(${inner_str}))')
		return
	}
	if inner_str.starts_with('(*(') || inner_str.starts_with('*(') {
		is_sum_value_deref := source_sum_type != ''
			&& g.get_sum_type_variants_for(source_sum_type).len > 0
			&& inner_str.contains('${source_sum_type}*')
		if !is_sum_value_deref && !expr_is_declared_sum_value {
			g.sb.write_string('((${type_name})(${inner_str}))')
			return
		}
	}
	if g.is_sum_payload_expr(node.expr, variant_field)
		|| g.is_sum_payload_expr(node.expr, short_name) {
		g.sb.write_string('(*((${type_name}*)(${inner_str})))')
		return
	}
	// Sum type cast: a as Cat => (*((main__Cat*)(((a))._data._Cat)))
	mut sum_source_expr := inner_str
	mut sep := if g.expr_is_pointer(node.expr) { '->' } else { '.' }
	unwrapped_source_expr := strip_expr_wrappers(node.expr)
	if unwrapped_source_expr is ast.PrefixExpr && unwrapped_source_expr.op == .mul {
		if !sum_source_expr.starts_with('(') {
			sum_source_expr = '(${sum_source_expr})'
		}
		sep = '.'
	}
	// Non-scalar sum type variants are stored as heap pointers that can be NULL
	// when the sum type is zero-initialized. Add null guard with zero fallback.
	if !g.is_scalar_sum_payload_type(type_name) && type_name != 'string' {
		g.sb.write_string('(((${sum_source_expr})${sep}_data._${variant_field}) ? (*((${type_name}*)(((${sum_source_expr})${sep}_data._${variant_field})))) : (${type_name}){0})')
	} else {
		g.sb.write_string('(*((${type_name}*)(((${sum_source_expr})${sep}_data._${variant_field}))))')
	}
}

fn (mut g Gen) as_cast_expr_extracts_direct_sum_variant(expr ast.Expr, source_sum_type string, target_type_name string) bool {
	if source_sum_type == '' || target_type_name == '' {
		return false
	}
	if !g.sum_type_has_direct_variant(source_sum_type, target_type_name) {
		return false
	}
	unwrapped := strip_expr_wrappers(expr)
	return unwrapped is ast.AsCastExpr
}

fn (mut g Gen) expr_is_declared_sum_value(expr ast.Expr) bool {
	match expr {
		ast.SelectorExpr {
			declared := g.selector_declared_field_type(expr).trim_space().trim_right('*')
			return declared != '' && g.get_sum_type_variants_for(declared).len > 0
		}
		ast.ParenExpr {
			return g.expr_is_declared_sum_value(expr.expr)
		}
		ast.ModifierExpr {
			return g.expr_is_declared_sum_value(expr.expr)
		}
		else {}
	}

	return false
}

fn (mut g Gen) raw_selector_expr_code(sel ast.SelectorExpr) string {
	saved := g.sb
	g.sb = strings.new_builder(128)
	g.expr(sel.lhs)
	lhs_code := g.sb.str()
	g.sb = saved
	mut use_ptr := g.selector_use_ptr(sel.lhs)
	if sel.lhs is ast.Ident {
		lhs_name := sel.lhs.name
		if lhs_name in g.cur_fn_mut_params {
			use_ptr = true
		} else if local_type := g.local_var_c_type_for_expr(sel.lhs) {
			use_ptr = local_type.ends_with('*') || local_type == 'chan'
		}
	}
	selector := if use_ptr { '->' } else { '.' }
	return '${lhs_code}${selector}${escape_c_keyword(sel.rhs.name)}'
}

fn (g &Gen) unwrap_parens(expr ast.Expr) ast.Expr {
	if expr is ast.ParenExpr {
		return g.unwrap_parens(expr.expr)
	}
	return expr
}

// is_sum_data_ptr_deref detects CastExpr(Type*, SelectorExpr{..._data._Variant})
// patterns generated by smartcast lowering. These pointers can be NULL when a
// sum type is zero-initialized.
fn (mut g Gen) is_sum_data_ptr_deref(expr ast.Expr) bool {
	inner := g.unwrap_parens(expr)
	if inner is ast.CastExpr {
		type_name := g.resolved_sum_data_cast_type(inner) or { g.expr_type_to_c(inner.typ) }
		if type_name.ends_with('*') && !g.is_scalar_sum_payload_type(type_name.trim_right('*')) {
			cast_inner := g.unwrap_parens(inner.expr)
			if cast_inner is ast.SelectorExpr {
				if cast_inner.rhs.name.starts_with('_') {
					// Check if LHS is also a _data selector
					if cast_inner.lhs is ast.SelectorExpr {
						lhs_sel := cast_inner.lhs as ast.SelectorExpr
						if lhs_sel.rhs.name == '_data' {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

fn (mut g Gen) is_sum_data_payload_selector(expr ast.Expr) bool {
	inner := g.unwrap_parens(expr)
	sel := match inner {
		ast.SelectorExpr {
			inner
		}
		else {
			return false
		}
	}

	if !sel.rhs.name.starts_with('_') {
		return false
	}
	lhs_sel := match sel.lhs {
		ast.SelectorExpr {
			sel.lhs
		}
		else {
			return false
		}
	}

	return lhs_sel.rhs.name == '_data'
}

fn (mut g Gen) resolved_sum_data_cast_type(node ast.CastExpr) ?string {
	mut type_name := g.expr_type_to_c(node.typ)
	mut ptr_suffix := ''
	for type_name.ends_with('*') {
		ptr_suffix += '*'
		type_name = type_name[..type_name.len - 1]
	}
	cast_inner := g.unwrap_parens(node.expr)
	sel := match cast_inner {
		ast.SelectorExpr {
			cast_inner
		}
		else {
			return none
		}
	}

	if !sel.rhs.name.starts_with('_') {
		return none
	}
	lhs_sel := match sel.lhs {
		ast.SelectorExpr {
			sel.lhs
		}
		else {
			return none
		}
	}

	if lhs_sel.rhs.name != '_data' {
		return none
	}
	raw_variant := sel.rhs.name[1..]
	mut sum_type := ''
	if raw_type := g.get_raw_type(lhs_sel.lhs) {
		match raw_type {
			types.SumType {
				sum_type = g.types_type_to_c(raw_type)
			}
			types.Pointer {
				if raw_type.base_type is types.SumType {
					sum_type = g.types_type_to_c(raw_type.base_type)
				}
			}
			types.Alias {
				if raw_type.base_type is types.SumType {
					sum_type = g.types_type_to_c(raw_type.base_type)
				}
			}
			else {}
		}
	}
	if sum_type == '' {
		sum_type = g.get_expr_type(lhs_sel.lhs).trim_space().trim_right('*')
	}
	variant_field := g.sum_type_variant_field_name(sum_type, raw_variant)
	payload_type := g.sum_type_variant_payload_type(sum_type, type_name, variant_field)
	if payload_type == '' || payload_type == type_name {
		return none
	}
	return payload_type + ptr_suffix
}

// get_cast_expr_type_name extracts the base type name from a CastExpr's target type.
fn (mut g Gen) get_cast_expr_type_name(expr ast.Expr) string {
	inner := g.unwrap_parens(expr)
	if inner is ast.CastExpr {
		type_name := g.resolved_sum_data_cast_type(inner) or { g.expr_type_to_c(inner.typ) }
		return type_name.trim_right('*')
	}
	return ''
}
