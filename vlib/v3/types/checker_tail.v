module types

import os
import strings
import time
import v3.flat
import v3.token
import v3.util

// last_index_between returns the last occurrence of needle that starts at or
// after lo and ends at or before end, scanning by index: a substr copy of the
// file prefix here costs megabytes per call on large sources.
fn last_index_between(source string, needle string, lo int, end int) int {
	stop := int_min(end, source.len)
	low := int_max(lo, 0)
	if needle.len == 0 || stop - low < needle.len {
		return -1
	}
	for i := stop - needle.len; i >= low; i-- {
		mut j := 0
		for j < needle.len && unsafe { source.str[i + j] == needle.str[j] } {
			j++
		}
		if j == needle.len {
			return i
		}
	}
	return -1
}

fn (tc &TypeChecker) pointer_diagnostic_binding_type_name(id flat.NodeId, typ Type) string {
	type_name := typ.name()
	// Only pointer-flavored bindings can resolve to `nil`/`voidptr`; skip the
	// source scan for every other type (this runs for each compound assign).
	if unalias_type(typ) !is Pointer && type_name !in ['voidptr', 'nil'] {
		return type_name
	}
	if !tc.valid_node_id(id) {
		return type_name
	}
	node := tc.a.node(id)
	if node.kind != .ident || node.value.len == 0 || !node.pos.is_valid() {
		return type_name
	}
	file := tc.a.source_files[node.pos.id] or { return type_name }
	source := tc.source_texts_by_file[file.name] or { return type_name }
	end := int_min(int_max(node.pos.offset, 0), source.len)
	// A local `x := ...` declaration can only appear inside the enclosing
	// function; bounding the backward scan there keeps misses (params, struct
	// fields) from walking to the top of the file.
	mut scan_lo := 0
	if tc.fn_context.node_id >= 0 && tc.fn_context.node_id < tc.a.nodes.len {
		fn_node := tc.a.nodes[tc.fn_context.node_id]
		if fn_node.pos.id == node.pos.id && fn_node.pos.offset >= 0 && fn_node.pos.offset < end {
			scan_lo = fn_node.pos.offset
		}
	}
	needle := '${node.value} := nil'
	if last_index_between(source, needle, scan_lo, end) >= 0 {
		return 'nil'
	}
	decl_needle := '${node.value} :='
	decl_start := last_index_between(source, decl_needle, scan_lo, end)
	if decl_start >= 0 {
		decl_end := source.index_after('\n', decl_start) or { end }
		if source[decl_start..int_min(decl_end, end)].contains('nil') {
			return 'nil'
		}
		if source[decl_start..int_min(decl_end, end)].contains('voidptr(') {
			return 'voidptr'
		}
	}
	return type_name
}

fn (tc &TypeChecker) compound_assignment_source_operator(lhs_id flat.NodeId, rhs_id flat.NodeId, op flat.Op) string {
	fallback := assignment_operator_text(op)
	if op !in [.amp_assign, .pipe_assign] || !tc.valid_node_id(lhs_id) || !tc.valid_node_id(rhs_id) {
		return fallback
	}
	lhs := tc.a.node(lhs_id)
	rhs := tc.a.node(rhs_id)
	file := tc.a.source_files[lhs.pos.id] or { return fallback }
	source := tc.source_texts_by_file[file.name] or { return fallback }
	start := int_max(0, lhs.pos.end)
	end := int_min(rhs.pos.offset, source.len)
	if start < end {
		between := source[start..end]
		if between.contains('&&=') {
			return '&&='
		}
		if between.contains('||=') {
			return '||='
		}
	}
	return fallback
}

fn (tc &TypeChecker) compound_assignment_operator_pos(lhs_id flat.NodeId, rhs_id flat.NodeId, op string) token.Pos {
	if !tc.valid_node_id(lhs_id) || !tc.valid_node_id(rhs_id) {
		return token.Pos{}
	}
	lhs := tc.a.nodes[int(lhs_id)]
	rhs := tc.a.nodes[int(rhs_id)]
	file := tc.a.source_files[lhs.pos.id] or { return lhs.pos }
	source := tc.source_texts_by_file[file.name] or { return lhs.pos }
	start := int_max(0, lhs.pos.end)
	end := int_min(rhs.pos.offset, source.len)
	if start < end {
		if relative := source[start..end].index(op) {
			op_start := start + relative
			return token.new_span(lhs.pos.id, op_start, op_start + op.len)
		}
	}
	return lhs.pos
}

fn (tc &TypeChecker) diagnostic_expr_type_name(id flat.NodeId, typ Type) string {
	clean := unalias_type(typ)
	if clean is FnType {
		if display := tc.expr_diagnostic_fn_type(id) {
			return display
		}
		return Type(clean).name().replace_once('fn(', 'fn (')
	}
	return match tc.a.node(id).kind {
		.int_literal { 'int literal' }
		.float_literal { 'float literal' }
		else { tc.diagnostic_type_name(typ).replace_once('fn(', 'fn (') }
	}
}

fn (tc &TypeChecker) diagnostic_type_name(typ Type) string {
	raw := typ.name()
	if !raw.contains('.') {
		return raw
	}
	prefix := raw.all_before('.')
	info := tc.current_file_import_info()
	if isnil(info) {
		return raw
	}
	for _, module_path in info.imports {
		if module_path.contains('.') && module_path.all_after_last('.') == prefix {
			return module_path + raw[prefix.len..]
		}
	}
	if module_path := tc.file_import_suffix_paths['${tc.cur_file}\x00${prefix}'] {
		return module_path + raw[prefix.len..]
	}
	return raw
}

fn assignment_operator_text(op flat.Op) string {
	return match op {
		.assign { '=' }
		.plus_assign { '+=' }
		.minus_assign { '-=' }
		.mul_assign { '*=' }
		.div_assign { '/=' }
		.mod_assign { '%=' }
		.amp_assign { '&=' }
		.pipe_assign { '|=' }
		.xor_assign { '^=' }
		.left_shift_assign { '<<=' }
		.right_shift_assign { '>>=' }
		.right_shift_unsigned_assign { '>>>=' }
		.power_assign { '**=' }
		else { '' }
	}
}

fn compound_assignment_infix_op(op flat.Op) ?flat.Op {
	return match op {
		.plus_assign { flat.Op.plus }
		.minus_assign { flat.Op.minus }
		.mul_assign { flat.Op.mul }
		.div_assign { flat.Op.div }
		.mod_assign { flat.Op.mod }
		.amp_assign { flat.Op.amp }
		.pipe_assign { flat.Op.pipe }
		.xor_assign { flat.Op.xor }
		.left_shift_assign { flat.Op.left_shift }
		.right_shift_assign { flat.Op.right_shift }
		.right_shift_unsigned_assign { flat.Op.right_shift_unsigned }
		.power_assign { flat.Op.power }
		else { none }
	}
}

fn (tc &TypeChecker) source_text_for_node(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.nodes[int(id)]
	file := tc.a.source_files[node.pos.id] or { return node.value }
	source := tc.source_texts_by_file[file.name] or { return node.value }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if end <= start {
		return node.value
	}
	return source[start..end].trim_space()
}

// node_source_starts_with reports whether the node's source span, after
// skipping leading whitespace, starts with prefix — the in-place equivalent of
// source_text_for_node(id).starts_with(prefix), without the span substr copy
// (this runs for every `&x` prefix expression and unsafe-argument check).
fn (tc &TypeChecker) node_source_starts_with(id flat.NodeId, prefix string) bool {
	if !tc.valid_node_id(id) {
		return ''.starts_with(prefix)
	}
	node := tc.a.nodes[int(id)]
	file := tc.a.source_files[node.pos.id] or { return node.value.starts_with(prefix) }
	source := tc.source_texts_by_file[file.name] or { return node.value.starts_with(prefix) }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if end <= start {
		return node.value.starts_with(prefix)
	}
	mut i := start
	for i < end && source[i] in [` `, `\t`, `\n`, `\r`] {
		i++
	}
	if end - i < prefix.len {
		return false
	}
	for j in 0 .. prefix.len {
		if unsafe { source.str[i + j] != prefix.str[j] } {
			return false
		}
	}
	return true
}

// node_source_contains reports whether the node's source span contains needle,
// scanning the file text in place: substr-copying a large span (a match
// statement can cover thousands of lines) per query multiplies into megabytes.
// The span is not trimmed; a needle match in leading/trailing whitespace is
// impossible for identifier-like needles.
fn (tc &TypeChecker) node_source_contains(id flat.NodeId, needle string) bool {
	if needle.len == 0 || !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	file := tc.a.source_files[node.pos.id] or { return node.value.contains(needle) }
	source := tc.source_texts_by_file[file.name] or { return node.value.contains(needle) }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if end - start < needle.len {
		return false
	}
	for i := start; i <= end - needle.len; i++ {
		mut j := 0
		for j < needle.len && unsafe { source.str[i + j] == needle.str[j] } {
			j++
		}
		if j == needle.len {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_import_symbol_conflict(id flat.NodeId, name string) {
	if name.len == 0 || name == '_' || !tc.has_active_import(name) {
		return
	}
	tc.check_import_symbol_conflict_at(id, name, tc.node_value_diagnostic_pos(id))
}

fn (mut tc TypeChecker) check_import_symbol_conflict_at(id flat.NodeId, name string, pos token.Pos) {
	if name.len == 0 || name == '_' || !tc.has_active_import(name) {
		return
	}
	tc.record_error_at(.duplicate_decl, 'duplicate of an import symbol `${name}`', id, pos)
}

fn (mut tc TypeChecker) check_module_name_conflict(id flat.NodeId, name string) {
	if name.len == 0 || name == '_' {
		return
	}
	if name == tc.cur_module {
		tc.record_error_at(.duplicate_decl, 'duplicate of a module name `${name}`', id,
			tc.node_value_diagnostic_pos(id))
	}
	tc.check_imported_module_prefix(id, name, '')
}

fn (tc &TypeChecker) imported_module_prefix(id flat.NodeId, name string) ?string {
	if !tc.valid_node_id(id) || !name.contains('__') {
		return none
	}
	node := tc.a.node(id)
	if !node.pos.is_valid() {
		return none
	}
	file := tc.a.source_files[node.pos.id] or { return none }
	if tc.diagnostic_files.len > 0 && file.name !in tc.diagnostic_files {
		return none
	}
	prefix := name.all_before('__')
	if prefix == 'builtin' {
		return prefix
	}
	info := tc.file_imports_by_file[file.name] or { return none }
	for alias, module_name in info.imports {
		if prefix == alias || prefix == module_name.all_after_last('.') {
			return prefix
		}
	}
	return none
}

fn (mut tc TypeChecker) check_imported_module_prefix(id flat.NodeId, name string, keyword string) {
	prefix := tc.imported_module_prefix(id, name) or { return }
	pos := if keyword.len > 0 {
		tc.fn_declaration_diagnostic_pos(tc.a.node(id))
	} else {
		tc.node_value_diagnostic_pos(id)
	}
	tc.record_error_at(.duplicate_decl,
		'identifier cannot use prefix `${prefix}__` of imported module `${prefix}`', id, pos)
}

fn (tc &TypeChecker) source_module_declares_fn(name string) bool {
	mut module_name := ''
	for index in tc.top_level_idx {
		node := tc.a.nodes[index]
		if node.kind == .file {
			module_name = ''
		} else if node.kind == .module_decl {
			module_name = node.value
		} else if node.kind == .fn_decl
			&& (module_name == tc.cur_module || (module_name == '' && tc.cur_module == 'main'))
			&& node.value == name && !local_fn_decl_is_transform_created(node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_receiver_param_diagnostic_pos(node flat.Node, name string) token.Pos {
	header_pos := tc.fn_declaration_diagnostic_pos(node)
	file := tc.a.source_files[header_pos.id] or { return header_pos }
	source := tc.source_texts_by_file[file.name] or { return header_pos }
	if header_pos.offset < 0 || header_pos.end > source.len || header_pos.end <= header_pos.offset {
		return header_pos
	}
	header := source[header_pos.offset..header_pos.end]
	open := header.index_u8(`(`)
	close := header.index_u8(`)`)
	if open < 0 || close <= open {
		return header_pos
	}
	receiver := header[open + 1..close]
	relative := receiver.index(name) or { return header_pos }
	start := header_pos.offset + open + 1 + relative
	return token.new_span(header_pos.id, start, start + name.len)
}

fn (tc &TypeChecker) fn_option_receiver_diagnostic_pos(node flat.Node, name string) token.Pos {
	name_pos := tc.fn_receiver_param_diagnostic_pos(node, name)
	file := tc.a.source_files[name_pos.id] or { return name_pos }
	source := tc.source_texts_by_file[file.name] or { return name_pos }
	end := int_min(source.len, name_pos.end + 2)
	if name_pos.end < end && source[name_pos.end..end].contains('?') {
		return token.new_span(name_pos.id, name_pos.offset, end)
	}
	return name_pos
}

fn (mut tc TypeChecker) check_compound_index_assignment_getter(assign_id flat.NodeId, lhs_id flat.NodeId, op flat.Op) {
	if !assignment_op_reads_lhs(op) || int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count == 0 {
		return
	}
	base_id := tc.a.child(&lhs, 0)
	base_type_raw := tc.resolve_type(base_id)
	if setter := tc.index_overload_call_info(base_type_raw, true) {
		if getter := tc.index_overload_call_info(base_type_raw, false) {
			err_count := tc.errors.len
			tc.check_index_overload_args(lhs_id, lhs, getter)
			if tc.errors.len != err_count {
				return
			}
			if !tc.compound_index_overload_index_params_match(setter, getter) {
				if tc.should_diagnose(lhs_id) {
					tc.record_error(.assignment_mismatch,
						'compound overloaded index assignment requires matching `[]` and `[]=` index parameter types',
						assign_id)
				}
				return
			}
			if !tc.compound_index_overload_value_params_match(setter, getter) {
				if tc.should_diagnose(lhs_id) {
					tc.record_error(.assignment_mismatch,
						'compound overloaded index assignment requires `[]` return type compatible with `[]=` value parameter type',
						assign_id)
				}
				return
			}
			return
		}
		if tc.should_diagnose(lhs_id) {
			tc.record_error(.assignment_mismatch,
				'compound overloaded index assignment requires a matching `[]` getter', assign_id)
		}
	}
}

fn (tc &TypeChecker) compound_index_overload_index_params_match(setter CallInfo, getter CallInfo) bool {
	if setter.params.len < 2 || getter.params.len < 2 {
		return true
	}
	return tc.c_type(setter.params[1]) == tc.c_type(getter.params[1])
}

fn (tc &TypeChecker) compound_index_overload_value_params_match(setter CallInfo, getter CallInfo) bool {
	if setter.params.len < 3 {
		return true
	}
	return tc.type_compatible(getter.return_type, setter.params[2])
}

fn assignment_op_reads_lhs(op flat.Op) bool {
	return match op {
		.plus_assign, .minus_assign, .mul_assign, .power_assign, .div_assign, .mod_assign,
		.amp_assign, .pipe_assign, .xor_assign, .left_shift_assign, .right_shift_assign,
		.right_shift_unsigned_assign {
			true
		}
		else {
			false
		}
	}
}

fn (tc &TypeChecker) assignment_types_compatible(rhs_id flat.NodeId, rhs_type Type, expected_type Type, op flat.Op) bool {
	if op == .assign && tc.translated_files[tc.cur_file] && rhs_type is ArrayFixed
		&& expected_type is Pointer && tc.expr_can_take_address(rhs_id) {
		return tc.type_compatible(rhs_type.elem_type, expected_type.base_type)
	}
	if op == .assign
		&& tc.fixed_array_address_to_byte_pointer_compatible(rhs_id, rhs_type, expected_type) {
		return true
	}
	if fn_param_unalias_type(expected_type).is_integer() && tc.c_scalar_byte_literal_arg(rhs_id) {
		return true
	}
	if tc.expr_tail_is_nil(rhs_id) {
		if _ := fn_type_from_type(expected_type) {
			return true
		}
	}
	if base := tc.mut_param_expr_base(rhs_id, rhs_type) {
		if tc.type_compatible(base, expected_type)
			|| tc.pointer_value_compatible(base, expected_type) {
			return true
		}
	}
	clean_rhs := unalias_type(rhs_type)
	clean_expected := unalias_type(expected_type)
	if clean_rhs.is_integer() && clean_expected.is_float() && tc.a.node(rhs_id).kind != .int_literal {
		return false
	}
	if clean_expected is SumType {
		return tc.direct_sum_assignment_variant_matches(rhs_type, clean_expected)
	}
	return tc.expr_compatible(rhs_id, rhs_type, expected_type)
		|| tc.pointer_value_compatible(rhs_type, expected_type)
		|| tc.pointer_arithmetic_assign_compatible(op, rhs_type, expected_type)
}

fn (tc &TypeChecker) direct_sum_assignment_variant_matches(actual Type, expected SumType) bool {
	if tc.generic_type_name_matches(actual.name(), expected.name) {
		return true
	}
	base := tc.sum_base_name(expected.name)
	variants := tc.sum_types[base] or { return false }
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(expected.name, variant)
		if tc.generic_type_name_matches(actual.name(), concrete) {
			return true
		}
	}
	return false
}

fn type_is_unsigned_integer(typ Type) bool {
	clean := unalias_type(typ)
	if clean is Primitive {
		return clean.props.has(.integer) && clean.props.has(.unsigned)
	}
	return clean is USize
}

fn (tc &TypeChecker) expr_is_negative_integer_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	if unalias_type(tc.resolve_type(id)).is_integer() && tc.node_source_starts_with(id, '-') {
		return true
	}
	return false
}

fn (tc &TypeChecker) fixed_array_address_to_byte_pointer_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	actual_ptr := if actual is Pointer { actual } else { return false }
	expected_ptr := if expected is Pointer { expected } else { return false }
	actual_base := if actual_ptr.base_type is Alias {
		actual_ptr.base_type.base_type
	} else {
		actual_ptr.base_type
	}
	expected_base := if expected_ptr.base_type is Alias {
		expected_ptr.base_type.base_type
	} else {
		expected_ptr.base_type
	}
	if actual_base !is ArrayFixed || expected_base.name() != 'u8' || !tc.valid_node_id(expr_id) {
		return false
	}
	actual_fixed := actual_base as ArrayFixed
	if unalias_type(actual_fixed.elem_type).name() != 'u8' {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	return node.kind == .prefix && node.op == .amp && node.children_count > 0
}

fn (tc &TypeChecker) assignment_preserves_smartcast(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs_type Type) bool {
	if tc.smartcasts.len == 0 {
		return false
	}
	key := tc.expr_key(lhs_id)
	if key.len == 0 {
		return false
	}
	smartcast := tc.smartcasts[key] or { return false }
	if tc.expr_compatible(rhs_id, rhs_type, smartcast)
		|| tc.pointer_value_compatible(rhs_type, smartcast) {
		return true
	}
	return tc.assignment_rhs_mutates_same_smartcast_lhs(lhs_id, rhs_id, rhs_type, smartcast)
}

fn (tc &TypeChecker) assignment_rhs_mutates_same_smartcast_lhs(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs_type Type, smartcast Type) bool {
	if tc.sum_variant_type_for_pattern(rhs_type.name(), smartcast.name()) == none {
		return false
	}
	if !tc.valid_node_id(rhs_id) {
		return false
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind != .call {
		return false
	}
	lhs_key := tc.expr_key(lhs_id)
	if lhs_key.len == 0 {
		return false
	}
	for i := 1; i < rhs.children_count; i++ {
		arg_id := tc.call_arg_value(tc.a.child(&rhs, i))
		if tc.expr_is_mut_arg_for_key(arg_id, lhs_key) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_is_mut_arg_for_key(id flat.NodeId, key string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return (node.is_mut && tc.expr_key(id) == key)
			|| tc.expr_is_mut_arg_for_key(tc.a.child(&node, 0), key)
	}
	return node.is_mut && tc.expr_key(id) == key
}

fn assignment_marker_value_is_error(value string) bool {
	return value.starts_with('for init assignment mismatch:')
		|| value.starts_with('for post assignment mismatch:')
}

fn (mut tc TypeChecker) check_assignment_marker(id flat.NodeId, node flat.Node) bool {
	if assignment_marker_value_is_error(node.value) {
		tc.record_error(.assignment_mismatch, node.value, id)
		return true
	}
	return false
}

// index_assign_lhs_is_map supports index assign lhs is map handling for TypeChecker.
fn (tc &TypeChecker) index_assign_lhs_is_map(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	lhs_id := tc.a.child(&node, 0)
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		return false
	}
	base_type := unwrap_pointer(tc.resolve_type(tc.a.child(&lhs, 0)))
	return base_type is Map
}

// check_multi_return_assign validates check multi return assign state for types.
fn (mut tc TypeChecker) check_multi_return_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs := tc.a.nodes[int(rhs_id)]
	lhs_ids := tc.multi_assign_lhs_ids(node)
	if tc.multi_assign_rhs_count(node) != 1 {
		return false
	}
	for lhs_id in lhs_ids {
		tc.check_lvalue_mutability(lhs_id)
		tc.remember_expr_type(lhs_id, tc.resolve_lvalue_type(lhs_id))
	}
	if rhs.kind == .match_stmt {
		tc.check_node(rhs_id)
		if !tc.match_has_else_or_exhaustive_coverage(rhs) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'match expression must be exhaustive for multi-return assignment', id)
			}
			return true
		}
		if rhs_types := tc.match_multi_return_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				lhs_type := tc.resolve_lvalue_type(lhs_id)
				if !tc.type_compatible(rhs_types[i], lhs_type) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot assign `${rhs_types[i].name()}` to `${lhs_type.name()}`', id)
				}
			}
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if tc.match_has_incompatible_multi_return_branches(rhs_id, lhs_ids.len) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
			return true
		}
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				lhs_type := tc.resolve_lvalue_type(lhs_id)
				if !tc.type_compatible(rhs_types[i], lhs_type) {
					tc.type_mismatch(.assignment_mismatch,
						'cannot assign `${rhs_types[i].name()}` to `${lhs_type.name()}`', id)
				}
			}
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		if tc.should_diagnose(id) {
			if tc.match_has_tuple_tail_values(rhs_id, lhs_ids.len) {
				tc.record_error(.assignment_mismatch,
					'match expression branches cannot produce multiple assignment values', id)
			} else {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
		}
		return true
	}
	if rhs.kind == .if_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
		rhs_type := tc.resolve_type(rhs_id)
		if rhs_type !is MultiReturn || tc.expr_has_tuple_tail_values(rhs_id, lhs_ids.len) {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: expression branches must all produce ${lhs_ids.len} compatible values',
					id)
			}
			return true
		}
	}
	if rhs.kind == .lock_expr {
		tc.check_node(rhs_id)
		if rhs_types := tc.multi_expr_tail_assign_types(id, rhs_id, lhs_ids) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			$if ownership ? {
				tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, MultiReturn{
					types: rhs_types
				}, id)
			}
			return true
		}
	}
	mut rhs_type := tc.resolve_type(rhs_id)
	mut rhs_checked := false
	mut rhs_multi := MultiReturn{}
	mut found_multi := false
	mut unhandled_multi := false
	for _ in 0 .. 2 {
		if multi := tc.multi_return_assignment_type(rhs_id, rhs_type) {
			rhs_multi = multi
			found_multi = true
			break
		}
		if _ := tc.unhandled_wrapped_multi_return_type(rhs_id, rhs_type) {
			unhandled_multi = true
			break
		}
		if rhs_checked {
			break
		}
		tc.check_node(rhs_id)
		rhs_checked = true
		rhs_type = tc.resolve_type(rhs_id)
	}
	rhs_type_name := rhs_type.name()
	if unhandled_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'multi-return assignment from `${rhs_type_name}` requires `or {}`, `!`, or `?` handling',
				id)
		}
		return true
	}
	if found_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if lhs_ids.len != rhs_multi.types.len {
			if tc.should_diagnose(id) {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has ${rhs_multi.types.len} values',
					id)
			}
			return true
		}
		for i, lhs_id in lhs_ids {
			lhs_type := tc.resolve_lvalue_type(lhs_id)
			expected_type := tc.assignment_expected_type(lhs_id, lhs_type)
			if !tc.type_compatible(rhs_multi.types[i], expected_type) {
				lhs_name := tc.source_text_for_node(lhs_id)
				expected_name := expected_type.name().replace_once('fn(', 'fn (')
				actual_name := rhs_multi.types[i].name().replace_once('fn(', 'fn (')
				tc.record_error_at(.assignment_mismatch,
					'cannot assign to `${lhs_name}`: expected `${expected_name}`, not `${actual_name}`',
					rhs_id, rhs.pos)
			}
		}
		$if ownership ? {
			tc.ownership_after_multi_return_assign(lhs_ids, rhs_id, rhs_multi, id)
		}
		return true
	}
	if lhs_ids.len > 1 {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'multi-return assignment mismatch: ${lhs_ids.len} variables but `${rhs_type_name}` has 1 values',
				id)
		}
		return true
	}
	return false
}

// check_postfix validates check postfix state for types.
fn (mut tc TypeChecker) check_postfix(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	child := tc.a.nodes[int(child_id)]
	tc.check_node(child_id)
	if node.op in [.inc, .dec] {
		op := if node.op == .inc { '++' } else { '--' }
		action := if node.op == .inc { 'increment' } else { 'decrement' }
		rewrite_op := if node.op == .inc { '+' } else { '-' }
		is_map_index := child.kind == .index && child.children_count > 0
			&& unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(&child, 0)))) is Map
		if !tc.expr_can_take_address(child_id) && !is_map_index {
			source := tc.source_text_for_node(child_id)
			tc.record_error_with_details_at(.assignment_mismatch,
				'cannot ${action} `${source}` because it is non lvalue expression', child_id,
				tc.a.node(child_id).pos, [
				'try rewrite this as `${source} ${rewrite_op} 1`',
			])
			return
		}
		child_type := unalias_type(tc.resolve_type(child_id))
		if child_type is Pointer {
			if tc.pointer_diagnostic_binding_type_name(child_id, Type(child_type)) in [
				'voidptr',
				'nil',
			] {
				tc.record_error_at(.assignment_mismatch,
					'invalid operation: ${op} (non-numeric type `voidptr`)', id, tc.prefix_operator_pos(id,
					op))
				return
			}
			is_implicitly_dereferenced_mut_param := child.kind == .ident
				&& child.value in tc.fn_context.mut_param_base_types
			if tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
				&& !is_implicitly_dereferenced_mut_param {
				tc.record_warning_at(.assignment_mismatch,
					'pointer arithmetic is only allowed in `unsafe` blocks', id, tc.prefix_operator_pos(id,
					op))
			}
			tc.check_lvalue_mutability(child_id)
			return
		}
		if !infix_power_type_is_numeric(child_type) {
			type_name := if child_type is Nil { 'voidptr' } else { child_type.name() }
			tc.record_error_at(.assignment_mismatch,
				'invalid operation: ${op} (non-numeric type `${type_name}`)', id, tc.prefix_operator_pos(id,
				op))
			return
		}
		tc.check_lvalue_mutability(child_id)
	}
	if node.op == .not && child.kind == .array_literal {
		fixed_type := if child.typ.len > 0 {
			parsed := tc.parse_type(child.typ)
			if parsed is ArrayFixed {
				Type(parsed)
			} else if child.children_count > 0 {
				Type(ArrayFixed{
					elem_type: tc.resolve_type(tc.a.child(&child, 0))
					len:       child.children_count
				})
			} else {
				Type(ArrayFixed{
					elem_type: Type(int_)
					len:       child.children_count
				})
			}
		} else if child.children_count > 0 {
			Type(ArrayFixed{
				elem_type: tc.resolve_type(tc.a.child(&child, 0))
				len:       child.children_count
			})
		} else {
			Type(ArrayFixed{
				elem_type: Type(int_)
				len:       child.children_count
			})
		}
		tc.register_synth_type(id, fixed_type)
	}
	if node.op == .not && node.value == 'ragged_inferred_fixed_array' && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'inferred fixed-array literal rows must have the same size', id)
	}
	if child.kind == .index && child.children_count >= 2 {
		base_type_raw := tc.resolve_type(tc.a.child(&child, 0))
		base_type := unwrap_pointer(base_type_raw)
		if base_type is Map && node.op in [.inc, .dec] && tc.reject_unlowered_map_mutation
			&& tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index postfix mutation reached post-transform checker',
				id)
		}
		if node.op in [.inc, .dec] && tc.should_diagnose(id) {
			if _ := tc.index_overload_call_info(base_type_raw, false) {
				tc.record_error(.assignment_mismatch,
					'postfix mutation is not supported for overloaded index expressions', id)
			} else if _ := tc.index_overload_call_info(base_type_raw, true) {
				tc.record_error(.assignment_mismatch,
					'postfix mutation is not supported for overloaded index expressions', id)
			}
		}
	}
}

fn (mut tc TypeChecker) check_lvalue_mutability(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	if element_id := tc.shared_array_element_index(id) {
		tc.record_error_at(.assignment_mismatch,
			'you have to create a handle and `lock` it to modify `shared` array element',
			element_id, tc.a.node(element_id).pos)
		return
	}
	tc.check_lvalue_field_mutability(id)
	root_id := tc.lvalue_root_ident(id) or {
		tc.check_conditional_lvalue_mutability(id)
		return
	}
	root := tc.a.nodes[int(root_id)]
	if root.value.len == 0 || root.value == '_' {
		return
	}
	if tc.current_binding_is_shared(root.value) {
		lock_mode := tc.current_shared_lock_mode(root.value)
		if lock_mode == `w` {
			return
		}
		if lock_mode == `r` {
			tc.record_error_at(.assignment_mismatch,
				'${root.value} has an `rlock` but needs a `lock`', root_id,
				tc.node_value_diagnostic_pos(root_id))
		}
		tc.record_error_at(.assignment_mismatch,
			'`${root.value}` is `shared` and needs explicit lock for `v.ast.SelectorExpr`',
			root_id, tc.node_value_diagnostic_pos(root_id))
		return
	}
	if tc.ident_is_mutable_lvalue(root.value) {
		return
	}
	if tc.unsafe_depth > 0 && unalias_type(tc.resolve_type(root_id)) is Pointer {
		return
	}
	if _ := tc.malformed_const_keyword_pos(root_id) {
		return
	}
	if _ := tc.const_key_for_name(root.value) {
		tc.record_error_at(.assignment_mismatch, 'cannot modify constant `${root.value}`', root_id,
			tc.node_value_diagnostic_pos(root_id))
		return
	}
	if tc.fn_value_type(root.value) != none {
		tc.record_error_at(.assignment_mismatch, 'cannot assign to function `${root.value}`',
			root_id, tc.node_value_diagnostic_pos(root_id))
		return
	}
	owner := tc.cur_scope.lookup_owner(root.value) or { return }
	if owner.belongs_to_scope(tc.file_scope) {
		return
	}
	if closure_owner := tc.fn_context.closure_copy_owners[root.value] {
		if owner.storage_key() == closure_owner.storage_key() {
			tc.record_error_at(.assignment_mismatch,
				'the closure copy of `${root.value}` is immutable, declare it with `mut` to make it mutable',
				root_id, tc.node_value_diagnostic_pos(root_id))
			return
		}
	}
	tc.record_error_at(.assignment_mismatch,
		'`${root.value}` is immutable, declare it with `mut` to make it mutable', root_id,
		tc.node_value_diagnostic_pos(root_id))
}

fn (mut tc TypeChecker) check_conditional_lvalue_mutability(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind in [.index, .paren] && node.children_count > 0 {
		tc.check_conditional_lvalue_mutability(tc.a.child(node, 0))
		return
	}
	if node.kind == .if_expr || node.kind == .match_stmt {
		for i in 1 .. node.children_count {
			tail_id := tc.branch_tail_expr_id(tc.a.child(node, i))
			if tc.valid_node_id(tail_id) {
				tc.check_lvalue_mutability(tail_id)
			}
		}
	}
}

fn (mut tc TypeChecker) check_lvalue_field_mutability(id flat.NodeId) {
	node := tc.a.nodes[int(id)]
	if node.kind == .index && node.children_count > 0 {
		tc.check_lvalue_field_mutability(tc.a.child(&node, 0))
		return
	}
	if node.kind != .selector || node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	tc.check_lvalue_field_mutability(base_id)
	if tc.selector_is_shared_arg(node) {
		tc.record_error_at(.assignment_mismatch,
			'`${tc.source_text_for_node(id)}` is `shared` and needs explicit lock for `v.ast.SelectorExpr`',
			id, tc.selector_field_diagnostic_pos(id, node.value))
		return
	}
	raw_base_type := unalias_type(tc.resolve_type(base_id))
	base_type := unalias_and_unwrap_pointer_type(raw_base_type)
	if base_type is Interface {
		for field in tc.interface_field_list(base_type.name) {
			if field.name == node.value && !field.is_mut {
				tc.record_error_at(.assignment_mismatch,
					'field `${node.value}` of interface `${raw_base_type.name()}` is immutable',
					id, tc.node_value_diagnostic_pos(id))
				return
			}
		}
		return
	}
	if base_type !is Struct {
		return
	}
	struct_type := base_type as Struct
	for field in tc.struct_fields_for_init(struct_type.name) {
		if field.name != node.value {
			continue
		}
		if !field.is_mut {
			tc.record_error_at(.assignment_mismatch,
				'field `${node.value}` of struct `${raw_base_type.name()}` is immutable', id, tc.selector_field_diagnostic_pos(id,
				node.value))
		}
		return
	}
}

fn (tc &TypeChecker) selector_field_diagnostic_pos(id flat.NodeId, field string) token.Pos {
	node := tc.a.node(id)
	if node.children_count == 0 {
		return tc.node_value_diagnostic_pos(id)
	}
	base := tc.a.child_node(node, 0)
	file := tc.a.source_files[node.pos.id] or { return tc.node_value_diagnostic_pos(id) }
	source := tc.source_texts_by_file[file.name] or { return tc.node_value_diagnostic_pos(id) }
	start := int_max(0, int_min(base.pos.offset, source.len))
	line_end := source.index_after('\n', start) or { source.len }
	if relative := source[start..line_end].last_index('.${field}') {
		field_start := start + relative + 1
		return token.new_span(node.pos.id, field_start, field_start + field.len)
	}
	return tc.node_value_diagnostic_pos(id)
}

fn (tc &TypeChecker) lvalue_root_ident(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		return id
	}
	if node.kind in [.index, .selector, .paren] && node.children_count > 0 {
		return tc.lvalue_root_ident(tc.a.child(&node, 0))
	}
	return none
}

fn (tc &TypeChecker) assignment_expected_type(lhs_id flat.NodeId, lhs_type Type) Type {
	if int(lhs_id) < 0 {
		return lhs_type
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident {
		if lhs.value == '_' {
			return Type(Unknown{})
		}
		if base := tc.mut_param_base_for_current_ident(lhs.value, lhs_type) {
			return base
		}
	}
	return lhs_type
}

fn (tc &TypeChecker) lvalue_matches_mut_param(lhs_type Type, base_type Type) bool {
	if lhs_type is Pointer {
		return tc.type_compatible(lhs_type.base_type, base_type)
			&& tc.type_compatible(base_type, lhs_type.base_type)
	}
	// Some expression-type entries have already stripped the ABI pointer used
	// for a `mut` parameter. Keep recognizing the binding as the same parameter
	// so option smartcasts operate on its semantic type.
	return tc.type_compatible(lhs_type, base_type) && tc.type_compatible(base_type, lhs_type)
}

fn (tc &TypeChecker) lvalue_ident_is_known(name string) bool {
	if _ := tc.cur_scope.lookup(name) {
		return true
	}
	if _ := tc.file_scope.lookup(name) {
		return true
	}
	qname := tc.qualify_name(name)
	if qname != name {
		if _ := tc.file_scope.lookup(qname) {
			return true
		}
	}
	return tc.const_type_for_name(name) != none || tc.fn_value_type(name) != none
}

fn (tc &TypeChecker) mut_param_binding_matches_lvalue(name string) bool {
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	if param_owner := tc.fn_context.mut_param_owners[name] {
		if owner := tc.cur_scope.lookup_owner(name) {
			return owner.scope == param_owner.scope && owner.index == param_owner.index
				&& owner.generation == param_owner.generation
		}
	}
	if local_owner := tc.fn_context.mut_local_owners[name] {
		if owner := tc.cur_scope.lookup_owner(name) {
			return owner.scope == local_owner.scope && owner.index == local_owner.index
				&& owner.generation == local_owner.generation
		}
	}
	return false
}

fn (tc &TypeChecker) mut_value_param_binding_matches_lvalue(name string) bool {
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	param_owner := tc.fn_context.mut_param_owners[name] or { return false }
	owner := tc.cur_scope.lookup_owner(name) or { return false }
	if owner.scope != param_owner.scope || owner.index != param_owner.index
		|| owner.generation != param_owner.generation {
		return false
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	fn_node := tc.a.node(fn_id)
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(fn_node, i)
		if param.kind == .param && param.value == name {
			return param.op != .dot
		}
	}
	return false
}

// resolve_lvalue_type resolves resolve lvalue type information for types.
fn (mut tc TypeChecker) resolve_lvalue_type(lhs_id flat.NodeId) Type {
	if int(lhs_id) < 0 {
		return Type(void_)
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident {
		if typ := tc.cur_scope.lookup(lhs.value) {
			return typ
		}
		if typ := tc.file_scope.lookup(lhs.value) {
			return typ
		}
		qname := tc.qualify_name(lhs.value)
		if qname != lhs.value {
			if typ := tc.file_scope.lookup(qname) {
				return typ
			}
		}
		if typ := tc.const_type_for_name(lhs.value) {
			return typ
		}
		if typ := tc.fn_value_type(lhs.value) {
			return typ
		}
		if tc.should_diagnose(lhs_id) && lhs.value != '_' {
			if _ := tc.malformed_const_keyword_pos(lhs_id) {
				// The malformed statement gets one source-level diagnostic.
			} else {
				tc.record_error(.unknown_ident, 'unknown identifier `${lhs.value}`', lhs_id)
			}
		}
		return unknown_type('unknown identifier `${lhs.value}`')
	}
	if lhs.kind == .selector {
		tc.check_selector(lhs_id, lhs)
		if typ := tc.selector_type(lhs_id, lhs) {
			return typ
		}
		return tc.resolve_type(lhs_id)
	}
	if lhs.kind == .index {
		return tc.resolve_index_lvalue_type(lhs_id, .assign)
	}
	if lhs.kind == .prefix && lhs.op == .mul && lhs.children_count > 0 {
		inner_id := tc.a.child(&lhs, 0)
		inner_node := tc.a.nodes[int(inner_id)]
		inner := unalias_type(if inner_node.kind == .prefix && inner_node.op == .mul {
			tc.resolve_lvalue_type(inner_id)
		} else {
			tc.resolve_type(inner_id)
		})
		if inner is Pointer {
			return inner.base_type
		}
		return inner
	}
	return tc.resolve_type(lhs_id)
}

fn (mut tc TypeChecker) resolve_index_lvalue_type(lhs_id flat.NodeId, op flat.Op) Type {
	if int(lhs_id) < 0 {
		return Type(void_)
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .index || lhs.children_count < 2 {
		tc.check_index(lhs_id, lhs)
		return tc.resolve_type(lhs_id)
	}
	base_id := tc.a.child(&lhs, 0)
	base_type := tc.resolve_type(base_id)
	if setter := tc.index_operator_call_info(base_type, '[]=') {
		if setter.params.len < 3 {
			tc.register_synth_type(lhs_id, unknown_type('invalid overloaded index setter'))
			return unknown_type('invalid overloaded index setter')
		}
		if !tc.check_index_overload_args_ok(lhs_id, lhs, setter) {
			invalid_type := unknown_type('invalid overloaded index setter')
			tc.register_synth_type(lhs_id, invalid_type)
			return invalid_type
		}
		if op != .assign {
			if getter := tc.index_operator_call_info(base_type, '[]') {
				if !tc.check_index_overload_args_ok(lhs_id, lhs, getter) {
					invalid_type := unknown_type('invalid overloaded index getter')
					tc.register_synth_type(lhs_id, invalid_type)
					return invalid_type
				}
				if !tc.index_overload_key_types_match(setter.params[1], getter.params[1]) {
					if tc.should_diagnose(lhs_id) {
						tc.record_error(.assignment_mismatch,
							'compound index assignment requires matching `[]` and `[]=` index parameter types (`[]` uses `${getter.params[1].name()}`, `[]=` uses `${setter.params[1].name()}`)',
							lhs_id)
					}
					invalid_type := unknown_type('mismatched overloaded index key types')
					tc.register_synth_type(lhs_id, invalid_type)
					return invalid_type
				}
				value_type := setter.params[2]
				if !tc.type_compatible(getter.return_type, value_type) {
					if tc.should_diagnose(lhs_id) {
						tc.record_error(.assignment_mismatch,
							'compound index assignment getter returns `${getter.return_type.name()}`, which cannot be used as setter value `${value_type.name()}`',
							lhs_id)
					}
					invalid_type := unknown_type('mismatched overloaded index value types')
					tc.register_synth_type(lhs_id, invalid_type)
					return invalid_type
				}
			} else {
				if tc.should_diagnose(lhs_id) {
					tc.record_error(.assignment_mismatch,
						'compound index assignment requires a `[]` overload on `${base_type.name()}`',
						lhs_id)
				}
				invalid_type := unknown_type('missing overloaded index getter')
				tc.register_synth_type(lhs_id, invalid_type)
				return invalid_type
			}
		}
		value_type := setter.params[2]
		tc.register_synth_type(lhs_id, value_type)
		return value_type
	}
	if getter := tc.index_operator_call_info(base_type, '[]') {
		if tc.should_diagnose(lhs_id) {
			tc.record_error_at(.assignment_mismatch,
				'index assignment requires a `[]=` overload on type `${base_type.name()}`', lhs_id,
				tc.index_brackets_pos(lhs))
		}
		if getter.params.len >= 2 {
			tc.check_index_overload_arg(lhs_id, lhs, getter, '[]')
		}
		invalid_type := unknown_type('missing overloaded index setter')
		tc.register_synth_type(lhs_id, invalid_type)
		return invalid_type
	}
	tc.check_index(lhs_id, lhs)
	return tc.resolve_type(lhs_id)
}

fn (tc &TypeChecker) index_overload_key_types_match(left Type, right Type) bool {
	return tc.type_compatible(left, right) && tc.type_compatible(right, left)
}

fn (tc &TypeChecker) option_marker_payload_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	if node.pos.offset >= 0 && node.pos.offset < source.len && source[node.pos.offset] == `?` {
		return token.new_span(node.pos.id, node.pos.offset + 1, node.pos.end)
	}
	if node.value.starts_with('?') {
		target := node.value[1..].all_after_last('.')
		search_end := int_min(source.len, int_max(node.pos.end, node.pos.offset + 1))
		search_start := int_max(0, search_end - node.value.len - 8)
		if relative := source[search_start..search_end].last_index('${target}(') {
			return token.new_span(node.pos.id, search_start + relative, search_end)
		}
	}
	return node.pos
}

// check_return validates check return state for types.
fn (mut tc TypeChecker) check_return(id flat.NodeId, node flat.Node) {
	// A returned method value escapes the function. Per-instance closure contexts make
	// value receivers safe, but mutable methods borrowing addressable stack receivers
	// remain invalid and are rejected below.
	// Returned fn literals with ordinary captures need a real closure environment too.
	// V3 only supports the narrow returned case of an explicit `mut` capture of a
	// pointer/reference value, where the lifted fn can keep using the pointee.
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		tc.reject_stored_method_value(child_id)
		tc.reject_returned_capturing_fn_literal(child_id)
	}
	expected := tc.fn_context.return_type
	if tc.current_fn_has_invalid_option_void_return_type() {
		return
	}
	if tc.current_fn_has_invalid_literal_return_type() {
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	saved_expected_expr_id := tc.expected_expr_id
	saved_expected_expr_type := tc.expected_expr_type
	if node.children_count == 1 {
		tc.expected_expr_id = int(tc.a.child(&node, 0))
		tc.expected_expr_type = expected
	}
	defer {
		tc.expected_expr_id = saved_expected_expr_id
		tc.expected_expr_type = saved_expected_expr_type
	}
	if node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		child := tc.a.node(child_id)
		if unsafe_none_id := tc.unsafe_block_none_expr_id(child_id) {
			tc.check_node(child_id)
			none_node := tc.a.node(unsafe_none_id)
			tc.record_error_at(.return_mismatch, 'cannot return `none` in unsafe block',
				unsafe_none_id, none_node.pos)
			return
		}
		if child.kind == .ident {
			if invalid_type := tc.non_file_scope_type(child.value) {
				if invalid_type is Unknown && invalid_type.reason == 'invalid variable' {
					tc.check_node(child_id)
					tc.record_error_at(.return_mismatch, '`${child.value}` used as value', id,
						tc.noreturn_statement_diagnostic_pos(id))
					return
				}
			}
		}
	}
	if expected is Void {
		if node.children_count > 0 && tc.should_diagnose(id) {
			child_id := tc.a.child(&node, 0)
			tc.record_error_at(.return_mismatch,
				'unexpected argument, current function does not return anything', child_id,
				tc.a.node(child_id).pos)
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
		}
		$if ownership ? {
			tc.ownership_after_return(id, node)
		}
		return
	}
	if node.children_count == 0 {
		if type_allows_implicit_return(expected) {
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
		if tc.should_diagnose(id) {
			noun := if multi_return_payload_type(expected) != none {
				'arguments'
			} else {
				'argument'
			}
			tc.record_error_at(.return_mismatch, 'expected `${expected.name()}` ${noun}', id,
				tc.noreturn_statement_diagnostic_pos(id))
		}
		return
	}
	if expected is ResultType && expected.base_type is Void {
		child_id := tc.a.child(&node, 0)
		child := tc.a.node(child_id)
		if node.children_count > 1 {
			tc.record_error_at(.return_mismatch,
				'functions with Result-only return types can only return an error', child_id,
				child.pos)
			return
		}
		tc.check_node(child_id)
		if void_tail := tc.return_value_void_tail(child_id) {
			tail := tc.a.node(void_tail)
			diagnostic_pos := if tail.kind == .or_expr && tail.value == '!'
				&& tail.pos.end > tail.pos.offset {
				token.new_span(tail.pos.id, tail.pos.offset, tail.pos.end - 1)
			} else {
				tail.pos
			}
			tc.record_error_at(.return_mismatch,
				'`${tc.source_text_for_node(void_tail)}` used as value', void_tail, diagnostic_pos)
			return
		}
		actual := tc.resolve_type(child_id)
		if child.kind == .none_expr {
			tc.record_error_at(.return_mismatch,
				'Option and Result types have been split, use `?` to return none', id,
				tc.noreturn_statement_diagnostic_pos(id))
			tc.record_error_at(.return_mismatch,
				'cannot use `none` as Result type in return argument', child_id, child.pos)
			return
		}
		if is_ierror_type(actual) || tc.type_compatible_with_ierror_payload(actual) {
			return
		}
		if actual is ResultType && actual.base_type is Void {
			return
		}
		tc.record_error_at(.return_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(child_id,
			actual)}` as Result type in return argument', child_id, child.pos)
		return
	}
	if expected is OptionType && expected.base_type is Void {
		child_id := tc.a.child(&node, 0)
		child := tc.a.node(child_id)
		if node.children_count > 1 {
			tc.record_error_at(.return_mismatch,
				'can only return `none` from an Option-only return function', child_id, child.pos)
			return
		}
		tc.check_node(child_id)
		if child.kind == .none_expr {
			return
		}
		actual := tc.resolve_type(child_id)
		if actual is Void {
			tc.record_error_at(.return_mismatch,
				'`${tc.source_text_for_node(child_id)}` used as value', id,
				tc.noreturn_statement_diagnostic_pos(id))
			return
		}
		tc.record_error_at(.return_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(child_id,
			actual)}` as Option type in return argument', child_id, child.pos)
		return
	}
	if expected is ResultType && node.children_count == 1 {
		payload := expected.base_type
		child_id := tc.a.child(&node, 0)
		if payload is Alias && unalias_type(payload.base_type) is OptionType
			&& tc.a.node(child_id).kind == .none_expr {
			tc.check_node(child_id)
			tc.record_error_at(.return_mismatch,
				'cannot use `none` as type `${payload.base_type.name()}` in return argument', id,
				tc.noreturn_statement_diagnostic_pos(id))
			return
		}
	}
	if node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		tc.annotate_expected_expr(child_id, expected)
		if expected is Unknown && tc.a.node(child_id).kind == .none_expr {
			return
		}
		if tc.return_is_direct_option_ident_propagation(child_id) && expected !is OptionType {
			tc.check_node(child_id)
			propagation := tc.a.node(child_id)
			source_id := tc.a.child(propagation, 0)
			operator_pos := tc.propagation_operator_pos(source_id, child_id, '?')
			return_line := tc.previous_source_line_matching(node.pos, 'return')
			tc.record_error_at(.return_mismatch,
				'should not unwrap option var on return, it could be none', id, token.new_span(return_line.id,
				return_line.offset, operator_pos.offset))
			return
		}
		if tc.expr_never_returns_resolving(child_id) {
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
		child := tc.a.node(child_id)
		if child.kind == .prefix && child.op == .amp && child.children_count > 0
			&& tc.unsafe_depth == 0 {
			value_id := tc.a.child(child, 0)
			value := tc.a.node(value_id)
			if value.kind == .ident && value.value.len > 0
				&& tc.cur_scope.lookup_owner(value.value) != none
				&& !tc.ident_is_mutable_lvalue(value.value)
				&& tc.current_fn_has_for_in_binding(value.value) {
				tc.check_node(child_id)
				tc.record_error_at(.return_mismatch,
					'`${value.value}` cannot be returned outside `unsafe` blocks as it might refer to an object stored on stack. Consider declaring `${value.value}` mutable.',
					value_id, value.pos)
				return
			}
		}
		raw_child_type := tc.resolve_type(child_id)
		if raw_child_type is Void {
			tc.check_node(child_id)
			if tc.should_diagnose(id) {
				tc.record_error_at(.return_mismatch,
					'`${tc.source_text_for_node(child_id)}` used as value', id,
					tc.noreturn_statement_diagnostic_pos(id))
			}
			return
		}
		if raw_child_type is OptionType && expected !is OptionType {
			tc.check_node(child_id)
			tc.record_error_at(.return_mismatch,
				'cannot use `?${raw_child_type.base_type.name()}` as type `${expected.name()}` in return argument',
				child_id, tc.option_marker_payload_pos(child))
			return
		}
	}
	if expected is ResultType && expected.base_type !is MultiReturn && node.children_count == 1
		&& !tc.result_return_uses_multi_tail(tc.a.child(&node, 0), expected) {
		child_id := tc.a.child(&node, 0)
		$if ownership ? {
			tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
		} $else {
			tc.check_node(child_id)
		}
		if bad_type := tc.invalid_ierror_return_expr_type_name(child_id, expected) {
			expected_name := '!${expected.base_type.name()}'
			if tc.should_diagnose(id) {
				actual := tc.resolve_type(child_id)
				tc.record_error_at(.return_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(child_id,
					actual)}` as type `${expected_name}` in return argument', child_id,
					tc.a.node(child_id).pos)
			} else {
				tc.record_invalid_ierror_return_error(id,
					'cannot return `${bad_type}` as `${expected_name}`')
			}
			return
		}
		actual := tc.resolve_expr(child_id, expected)
		if tc.return_type_compatible(child_id, actual, expected) {
			$if ownership ? {
				tc.ownership_after_return(id, node)
			}
			return
		}
	}
	if multi := multi_return_payload_type(expected) {
		if node.children_count > 1 {
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				if multi_return_payload_type(tc.resolve_type(child_id)) != none {
					tc.record_error_at(.return_mismatch,
						'cannot use multi-return with other return types', child_id,
						tc.a.node(child_id).pos)
					return
				}
			}
		}
		if node.children_count == 1 {
			child_id := tc.a.child(&node, 0)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			if msg := tc.tuple_tail_return_error(child_id, multi.types) {
				if tc.should_diagnose(id) {
					tc.record_error(.return_mismatch, msg, id)
				}
				return
			}
			actual := tc.resolve_expr(child_id, expected)
			if tc.return_type_compatible(child_id, actual, expected) {
				$if ownership ? {
					tc.ownership_after_return(id, node)
				}
				return
			}
			if actual_multi := multi_return_payload_type(actual) {
				if actual_multi.types.len == multi.types.len {
					mut slots_compatible := true
					for i, actual_type in actual_multi.types {
						slot_expected := multi.types[i]
						numeric_name_mismatch := infix_power_type_is_numeric(actual_type)
							&& infix_power_type_is_numeric(slot_expected)
							&& actual_type.name() != slot_expected.name()
						if numeric_name_mismatch
							|| !tc.return_type_compatible(child_id, actual_type, slot_expected) {
							slots_compatible = false
							if tc.should_diagnose(id) {
								tc.record_error_at(.return_mismatch,
									'cannot use `${actual_type.name()}` as type `${slot_expected.name()}` in return argument',
									child_id, tc.a.node(child_id).pos)
							}
							return
						}
					}
					if slots_compatible {
						$if ownership ? {
							tc.ownership_after_return(id, node)
						}
					}
					return
				}
			}
			if ok := tc.multi_expr_tail_return_compatible(id, child_id, multi.types, expected) {
				if ok {
					$if ownership ? {
						tc.ownership_after_return(id, node)
					}
				}
				return
			}
			if item_types := tc.multi_expr_tail_types(child_id, multi.types.len) {
				if item_types.len == multi.types.len {
					mut ok := true
					for i, item_type in item_types {
						if !tc.type_compatible(item_type, multi.types[i]) {
							ok = false
							if tc.should_diagnose(id) {
								tc.type_mismatch(.return_mismatch,
									'cannot return `${item_type.name()}` as `${multi.types[i].name()}`',
									id)
							}
						}
					}
					if ok {
						$if ownership ? {
							tc.ownership_after_return(id, node)
						}
					}
					return
				}
			}
		}
		if node.children_count != multi.types.len {
			if tc.should_diagnose(id) {
				pos_id := tc.a.child(&node, int_min(multi.types.len, node.children_count - 1))
				tc.record_error_at(.return_mismatch,
					'expected ${multi.types.len} arguments, but got ${node.children_count}',
					pos_id, tc.a.node(pos_id).pos)
			}
			return
		}
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
			} $else {
				tc.check_node(child_id)
			}
			actual := tc.resolve_expr(child_id, multi.types[i])
			if !tc.return_type_compatible(child_id, actual, multi.types[i]) {
				tc.type_mismatch(.return_mismatch,
					'cannot return `${actual.name()}` as `${multi.types[i].name()}`', id)
			}
		}
		$if ownership ? {
			tc.ownership_after_return(id, node)
		}
		return
	}
	if node.children_count != 1 {
		if tc.should_diagnose(id) {
			pos_id := tc.a.child(&node, int_min(1, node.children_count - 1))
			tc.record_error_at(.return_mismatch,
				'expected 1 argument, but got ${node.children_count}', pos_id,
				tc.a.node(pos_id).pos)
		}
		return
	}
	child_id := tc.a.child(&node, 0)
	$if ownership ? {
		tc.ownership_check_node_with_deferred_aggregate_consumption(child_id)
	} $else {
		tc.check_node(child_id)
	}
	child_value := tc.a.node(child_id)
	if tc.return_call_has_undefined_receiver(child_id) {
		tc.record_error_at(.return_mismatch,
			'`${tc.source_text_for_node(child_id)}` used as value', id,
			tc.noreturn_statement_diagnostic_pos(id))
		return
	}
	if child_value.kind == .ident {
		if invalid_type := tc.non_file_scope_type(child_value.value) {
			if invalid_type is Unknown && invalid_type.reason == 'invalid variable' {
				tc.record_error_at(.return_mismatch, '`${child_value.value}` used as value', id,
					tc.noreturn_statement_diagnostic_pos(id))
				return
			}
		}
	}
	if child_value.kind == .match_stmt
		&& tc.record_return_match_sumtype_branch_mismatch(child_id, expected) {
		return
	}
	if child_value.kind == .match_stmt && tc.expr_has_match_branch_type_error(child_id) {
		return
	}
	if child_value.kind == .ident && tc.fn_context.closure_forbidden_captures[child_value.value] {
		tc.record_error_at(.return_mismatch, '`${child_value.value}` used as value', id,
			tc.noreturn_statement_diagnostic_pos(id))
		return
	}
	if expected is OptionType && is_ierror_type(tc.resolve_type(child_id)) {
		if tc.valid_node_id(flat.NodeId(tc.fn_context.node_id))
			&& tc.a.node(flat.NodeId(tc.fn_context.node_id)).typ == '?void' {
			return
		}
		tc.record_error_at(.return_mismatch,
			'Option and Result types have been split, use `!Foo` to return errors', id,
			tc.noreturn_statement_diagnostic_pos(id))
		return
	}
	if expected is ResultType {
		if bad_type := tc.invalid_ierror_return_expr_type_name(child_id, expected) {
			tc.record_invalid_ierror_return_error(id,
				'cannot return `${bad_type}` as `${Type(expected).name()}`')
			return
		}
	}
	if type_is_unsigned_integer(expected) {
		if literal := tc.integer_literal_source(child_id) {
			if literal.starts_with('-') {
				tc.record_notice_at(.return_mismatch,
					'cannot use a negative value as value of type `${expected.name()}` in return argument',
					child_id, tc.a.node(child_id).pos)
				return
			}
		}
	}
	source_actual := tc.resolve_type(child_id)
	actual := tc.resolve_expr(child_id, expected)
	numeric_kind_mismatch := infix_power_type_is_numeric(actual)
		&& infix_power_type_is_numeric(expected)
		&& unalias_type(actual).is_integer() != unalias_type(expected).is_integer()
		&& tc.a.node(child_id).kind !in [.int_literal, .float_literal] && !(actual.name() == 'int'
		&& expected.name() == 'f32')
	reference_mismatch := source_actual is Pointer && expected !is Pointer
		&& unalias_type(expected) !is Interface
		&& tc.type_compatible(source_actual.base_type, expected)
		&& !(tc.a.node(child_id).kind == .ident
		&& tc.mut_param_binding_matches_lvalue(tc.a.node(child_id).value))
	if numeric_kind_mismatch || reference_mismatch
		|| !tc.return_type_compatible(child_id, actual, expected) {
		clean_expected := unalias_type(expected)
		if clean_expected is OptionType && unalias_type(clean_expected.base_type) is Interface
			&& tc.a.node(child_id).kind == .if_expr {
			tc.record_error_at(.return_mismatch,
				'mismatched types `${expected.name()}` and `${actual.name()}`', id,
				tc.return_if_keyword_pos(node))
			return
		}
		diagnostic_actual := if reference_mismatch {
			source_actual
		} else {
			tc.mut_param_expr_base(child_id, actual) or { actual }
		}
		if child_value.kind == .prefix && child_value.op == .amp && diagnostic_actual is Void
			&& tc.expr_subtree_has_error(child_id)
			&& tc.valid_node_id(flat.NodeId(tc.fn_context.node_id)) {
			pos := tc.prefix_operator_pos(child_id, '&')
			tc.record_error_at(.return_mismatch,
				'cannot use `void` as type `${call_argument_type_name(expected)}` in return argument',
				child_id, pos)
			fn_name := tc.a.node(flat.NodeId(tc.fn_context.node_id)).value
			tc.record_error_with_details_at(.return_mismatch,
				'fn `${fn_name}` expects you to return a non reference type `${expected.name()}`, but you are returning `void` instead',
				child_id, pos, [
				'use `return *pointer` instead of `return pointer`, and just `return value` instead of `return &value`',
			])
			return
		}
		if diagnostic_actual is Pointer && expected !is Pointer
			&& tc.type_compatible(diagnostic_actual.base_type, expected)
			&& tc.valid_node_id(flat.NodeId(tc.fn_context.node_id)) {
			fn_name := tc.a.node(flat.NodeId(tc.fn_context.node_id)).value
			pos := if tc.a.node(child_id).kind == .prefix {
				tc.prefix_operator_pos(child_id, '&')
			} else {
				tc.a.node(child_id).pos
			}
			diagnostic_actual_name := if reference_mismatch && source_actual is Pointer {
				'&${source_actual.base_type.name()}'
			} else {
				Type(diagnostic_actual).name()
			}
			tc.record_error_with_details_at(.return_mismatch,
				'fn `${fn_name}` expects you to return a non reference type `${expected.name()}`, but you are returning `${diagnostic_actual_name}` instead',
				child_id, pos, [
				'use `return *pointer` instead of `return pointer`, and just `return value` instead of `return &value`',
			])
			return
		}
		if expected is Pointer && diagnostic_actual !is Pointer
			&& tc.valid_node_id(flat.NodeId(tc.fn_context.node_id)) {
			fn_name := tc.a.node(flat.NodeId(tc.fn_context.node_id)).value
			tc.record_error_at(.return_mismatch, 'fn `${fn_name}` expects you to return a reference type `${call_argument_type_name(expected)}`, but you are returning `${tc.diagnostic_expr_type_name(child_id,
				diagnostic_actual)}` instead', child_id, tc.a.node(child_id).pos)
			return
		}
		tc.record_error_at(.return_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(child_id,
			diagnostic_actual)}` as type `${call_argument_type_name(expected)}` in return argument',
			child_id, tc.a.node(child_id).pos)
		return
	}
	$if ownership ? {
		tc.ownership_after_return(id, node)
	}
}

fn (mut tc TypeChecker) record_return_match_sumtype_branch_mismatch(id flat.NodeId, expected Type) bool {
	clean_expected := unalias_type(expected)
	if clean_expected !is SumType || !tc.valid_node_id(id) {
		return false
	}
	expected_sum := clean_expected as SumType
	node := tc.a.node(id)
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(node, i)
		tail_id := tc.branch_tail_expr_id(branch_id)
		if !tc.valid_node_id(tail_id) {
			continue
		}
		actual := tc.raw_return_match_tail_type(tail_id)
		if tc.type_name_is_direct_sum_variant(actual, expected_sum) {
			continue
		}
		if !tc.expr_has_match_branch_type_error(id) {
			tc.record_match_branch_return_type_mismatch(tail_id, expected, actual)
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) raw_return_match_tail_type(id flat.NodeId) Type {
	node := tc.a.node(id)
	if node.kind == .char_literal {
		return Type(rune_)
	}
	source := tc.source_text_for_node(id).trim_space()
	if open := source.index('(') {
		candidate := source[..open].trim_space()
		if candidate.len > 0 && tc.type_name_known(candidate) {
			return tc.parse_type(candidate)
		}
	}
	return tc.resolve_type(id)
}

fn (tc &TypeChecker) current_fn_has_for_in_binding(name string) bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	mut stack := []flat.NodeId{}
	stack << fn_id
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind == .for_in_stmt {
			for i in 0 .. int_min(2, node.children_count) {
				binding := tc.a.child_node(node, i)
				if binding.kind == .ident && binding.value == name {
					return true
				}
			}
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return false
}

fn (tc &TypeChecker) return_value_void_tail(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .match_stmt {
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(node, i)
			branch := tc.a.node(branch_id)
			if branch.kind != .match_branch {
				continue
			}
			tail_id := tc.branch_tail_expr_id(branch_id)
			if tc.valid_node_id(tail_id) && tc.resolve_type(tail_id) is Void {
				return tail_id
			}
		}
	} else if node.kind == .if_expr {
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(node, i)
			if void_tail := tc.return_value_void_tail(branch_id) {
				return void_tail
			}
			tail_id := tc.branch_tail_expr_id(branch_id)
			if tc.valid_node_id(tail_id) && tc.resolve_type(tail_id) is Void {
				return tail_id
			}
		}
	}
	return none
}

fn (tc &TypeChecker) return_if_keyword_pos(node flat.Node) token.Pos {
	line := tc.previous_source_line_matching(node.pos, 'return')
	file := tc.a.source_files[line.id] or { return line }
	source := tc.source_texts_by_file[file.name] or { return line }
	start := int_min(int_max(line.offset, 0), source.len)
	end := int_min(int_max(line.end, start), source.len)
	if start < end {
		if relative := source[start..end].index('if') {
			pos := start + relative
			return token.new_span(line.id, pos, pos + 2)
		}
	}
	return line
}

fn (tc &TypeChecker) return_call_has_undefined_receiver(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	base_id := tc.a.child(callee, 0)
	base := tc.a.node(base_id)
	return base.kind == .ident && tc.errors.any(it.node == base_id && it.kind == .unknown_ident
		&& (it.msg.starts_with('undefined ident')
		|| it.msg.starts_with('undefined variable')))
}

fn (tc &TypeChecker) return_is_direct_option_ident_propagation(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .or_expr || node.value != '?' || node.children_count == 0 {
		return false
	}
	source_id := tc.a.child(node, 0)
	source := tc.a.node(source_id)
	return source.kind == .ident && tc.resolve_type(source_id) is OptionType
}

fn (tc &TypeChecker) current_fn_has_invalid_literal_return_type() bool {
	node_id := tc.fn_context.node_id
	if node_id < 0 || node_id >= tc.a.nodes.len {
		return false
	}
	mut typ := tc.a.nodes[node_id].typ.trim_space()
	for typ.starts_with('?') || typ.starts_with('!') {
		typ = typ[1..].trim_space()
	}
	return typ in ['any', 'int_literal', 'float_literal']
}

fn (tc &TypeChecker) current_fn_has_invalid_option_void_return_type() bool {
	node_id := tc.fn_context.node_id
	return node_id >= 0 && node_id < tc.a.nodes.len
		&& tc.a.nodes[node_id].typ.trim_space() == '?void'
}

fn (tc &TypeChecker) expr_calls_invalid_option_void_fn(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt, .or_expr] && node.children_count > 0 {
		return tc.expr_calls_invalid_option_void_fn(tc.a.child(node, 0))
	}
	if node.kind != .call {
		return false
	}
	name := tc.resolved_call_name(id) or { tc.assignment_rhs_call_name(id) or { return false } }
	return trimmed_space(tc.fn_ret_type_texts[name] or { '' }) == '?void'
}

fn (mut tc TypeChecker) multi_expr_tail_return_compatible(return_id flat.NodeId, expr_id flat.NodeId, expected []Type, wrapper Type) ?bool {
	if !tc.multi_expr_tail_return_compat_supported(expr_id) {
		return none
	}
	groups := if wrapper is OptionType || wrapper is ResultType {
		tc.wrapped_multi_return_value_groups(expr_id, expected.len, false, wrapper) or {
			return none
		}
	} else {
		tc.multi_expr_tail_value_groups(expr_id, expected.len, false) or { return none }
	}
	if groups.len == 0 {
		return none
	}
	if tc.record_multi_expr_wrapped_return_mismatch(expr_id, expected, groups) {
		return false
	}
	mut ok := true
	for group in groups {
		if group.len != expected.len {
			return none
		}
		for i, value_id in group {
			actual := tc.resolve_expr(value_id, expected[i])
			if !type_has_runtime_value(actual) {
				if unalias_type(actual) is Void {
					value := tc.a.node(value_id)
					tc.record_error_at(.return_mismatch,
						'type `void` cannot be used in multi-return', value_id, value.pos)
					expr := tc.a.node(expr_id)
					if expr.kind in [.if_expr, .match_stmt] && value.kind == .call
						&& value.children_count > 0 {
						callee_id := tc.a.child(value, 0)
						tc.record_error_at(.if_branch_mismatch,
							'the final expression in `if` or `match`, must have a value of a non-void type',
							callee_id, tc.a.node(callee_id).pos)
					}
					return false
				}
				return none
			}
			if !tc.return_type_compatible(value_id, actual, expected[i]) {
				ok = false
				if expected[i] is Pointer && actual !is Pointer
					&& tc.valid_node_id(flat.NodeId(tc.fn_context.node_id)) {
					fn_name := tc.a.node(flat.NodeId(tc.fn_context.node_id)).value
					expr := tc.a.node(expr_id)
					pos := if expr.kind == .if_expr {
						token.new_span(expr.pos.id, expr.pos.offset, expr.pos.offset + 2)
					} else {
						expr.pos
					}
					tc.record_error_at(.return_mismatch, 'fn `${fn_name}` expects you to return a reference type `${call_argument_type_name(expected[i])}`, but you are returning `${tc.diagnostic_expr_type_name(value_id,
						actual)}` instead', expr_id, pos)
				} else {
					tc.type_mismatch(.return_mismatch,
						'cannot return `${actual.name()}` as `${expected[i].name()}`', return_id)
				}
			}
		}
	}
	return ok
}

fn (mut tc TypeChecker) record_multi_expr_wrapped_return_mismatch(expr_id flat.NodeId, expected []Type, groups [][]flat.NodeId) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	expr := tc.a.node(expr_id)
	if expr.kind !in [.if_expr, .match_stmt] {
		return false
	}
	for group in groups {
		if group.len != expected.len {
			continue
		}
		mut actual_types := []Type{cap: group.len}
		mut bad_index := -1
		for i, value_id in group {
			actual := tc.resolve_type(value_id)
			actual_types << actual
			if bad_index < 0 && !tc.return_type_compatible(value_id, actual, expected[i])
				&& (actual is OptionType || actual is ResultType) {
				bad_index = i
			}
		}
		if bad_index < 0 {
			continue
		}
		actual := actual_types[bad_index]
		if actual is ResultType {
			mut actual_names := []string{cap: group.len}
			for i, value_id in group {
				actual_names << tc.diagnostic_expr_type_name(value_id, actual_types[i])
			}
			mut bad_pos := tc.a.node(group[bad_index]).pos
			bad_node := tc.a.node(group[bad_index])
			if bad_node.kind == .call && bad_node.children_count > 0 {
				bad_pos = tc.a.child_node(bad_node, 0).pos
			}
			tc.record_error_at(.return_mismatch, 'return type mismatch, it should be `${Type(MultiReturn{
				types: expected
			}).name()}`, but it is instead `(${actual_names.join(', ')})`', group[bad_index],
				bad_pos)
		}
		pos := if expr.kind == .match_stmt {
			tc.match_header_pos(expr)
		} else {
			token.new_span(expr.pos.id, expr.pos.offset, expr.pos.offset + 2)
		}
		tc.record_error_at(.return_mismatch,
			'cannot use `${actual.name()}` as type `${expected[bad_index].name()}` in return argument',
			expr_id, pos)
		return true
	}
	return false
}

fn (tc &TypeChecker) multi_expr_tail_return_compat_supported(expr_id flat.NodeId) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	// Raw tuple-tail return lowering is currently implemented for if- and match-expressions.
	if node.kind in [.if_expr, .match_stmt] {
		return true
	}
	if node.kind == .expr_stmt && node.children_count > 0 {
		return tc.multi_expr_tail_return_compat_supported(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) result_return_uses_multi_tail(expr_id flat.NodeId, expected Type) bool {
	multi := multi_return_payload_type(expected) or { return false }
	return tc.expr_has_tuple_tail_values(expr_id, multi.types.len)
}

fn tuple_tail_return_lowering_allowed(expected []Type) bool {
	for typ in expected {
		if tuple_tail_return_type_allows_lowering(typ) {
			return true
		}
	}
	return false
}

fn tuple_tail_return_type_allows_lowering(typ Type) bool {
	if typ is ArrayFixed || typ is Struct {
		return true
	}
	if typ is Alias {
		return tuple_tail_return_type_allows_lowering(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) tuple_tail_return_error(expr_id flat.NodeId, expected []Type) ?string {
	if tuple_tail_return_lowering_allowed(expected) || !tc.valid_node_id(expr_id) {
		return none
	}
	count := expected.len
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.tuple_tail_return_error(tc.a.child(&node, 0), expected)
			}
		}
		.if_expr {
			if tc.expr_has_noncomma_tuple_tail_values(expr_id, count) {
				return 'if expression branches cannot produce multiple return values'
			}
		}
		.match_stmt {
			if tc.expr_has_noncomma_tuple_tail_values(expr_id, count) {
				return 'match expression branches cannot produce multiple return values'
			}
		}
		else {}
	}
	return none
}

fn (mut tc TypeChecker) return_type_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if tc.expr_compatible(expr_id, actual, expected) {
		return true
	}
	if return_numeric_alias_compatible(actual, expected) {
		return true
	}
	if tc.pointer_value_compatible(actual, expected) {
		return true
	}
	if expected is Pointer
		&& tc.bare_value_pointer_return_compatible(expr_id, actual, expected.base_type) {
		return true
	}
	if expected is OptionType {
		if tc.type_compatible(actual, expected.base_type) {
			return true
		}
		if expected.base_type is Pointer {
			if smart_type := tc.smartcast_type(expr_id) {
				if tc.type_compatible(smart_type, expected.base_type.base_type) {
					return true
				}
			}
		}
		if tc.pointer_value_compatible(actual, expected.base_type) {
			return true
		}
		if expected.base_type is Pointer
			&& tc.bare_value_pointer_return_compatible(expr_id, actual, expected.base_type.base_type) {
			return true
		}
	}
	if tc.expr_generic_expected_match(expr_id, actual, expected) {
		return true
	}
	if expected is ResultType {
		if tc.type_compatible(actual, expected.base_type) {
			return true
		}
		if tc.zero_literal_can_be_pointer(expr_id, expected.base_type) {
			return true
		}
		if tc.pointer_value_compatible(actual, expected.base_type) {
			return true
		}
		if expected.base_type is Pointer
			&& tc.bare_value_pointer_return_compatible(expr_id, actual, expected.base_type.base_type) {
			return true
		}
		if is_ierror_type(actual) || tc.type_embeds_error(actual) {
			return true
		}
		if tc.type_compatible_with_ierror_payload(actual) {
			return true
		}
	}
	if base := tc.mut_param_expr_base(expr_id, actual) {
		if tc.type_compatible(base, expected) || tc.generic_receiver_base_match(base, expected) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) bare_value_pointer_return_compatible(expr_id flat.NodeId, actual Type, expected_base Type) bool {
	if !tc.expr_can_take_address(expr_id) {
		return false
	}
	clean_actual := fn_param_unalias_type(actual)
	clean_expected := fn_param_unalias_type(expected_base)
	if clean_actual is Pointer {
		return false
	}
	return
		fn_return_canonical_type_name(clean_actual) == fn_return_canonical_type_name(clean_expected)
		|| tc.c_type(clean_actual) == tc.c_type(clean_expected)
}

fn (tc &TypeChecker) generic_expected_type_match(actual Type, expected Type) bool {
	if tc.generic_receiver_base_match(actual, expected) {
		return true
	}
	if expected is OptionType {
		if actual is OptionType {
			return tc.generic_expected_type_match(actual.base_type, expected.base_type)
		}
		return tc.generic_expected_type_match(actual, expected.base_type)
	}
	if expected is ResultType {
		if actual is ResultType {
			return tc.generic_expected_type_match(actual.base_type, expected.base_type)
		}
		return tc.generic_expected_type_match(actual, expected.base_type)
	}
	if actual is Array && expected is Array {
		return tc.generic_expected_type_match(actual.elem_type, expected.elem_type)
	}
	if actual is ArrayFixed && expected is ArrayFixed {
		return tc.fixed_array_lengths_compatible(actual, expected)
			&& tc.generic_expected_type_match(actual.elem_type, expected.elem_type)
	}
	if actual is Map && expected is Map {
		return tc.generic_expected_type_match(actual.key_type, expected.key_type)
			&& tc.generic_expected_type_match(actual.value_type, expected.value_type)
	}
	return false
}

fn (tc &TypeChecker) pointer_value_compatible(actual Type, expected Type) bool {
	if actual is Pointer {
		if !pointer_value_base_can_match(actual.base_type) {
			return false
		}
		return tc.type_compatible(actual.base_type, expected)
			|| pointer_value_type_names_match(Type(actual).name(), expected.name())
			|| bare_type_names_match(actual.base_type.name(), expected.name())
	}
	return pointer_value_type_names_match(actual.name(), expected.name())
}

fn pointer_value_base_can_match(typ Type) bool {
	clean := if typ is Alias { typ.base_type } else { typ }
	return clean is Struct || clean is Interface || clean is SumType || clean is Array
		|| clean is ArrayFixed || clean is Map || clean is Channel
}

fn pointer_value_type_names_match(actual string, expected string) bool {
	if !actual.starts_with('&') {
		return false
	}
	clean_actual := actual[1..]
	return clean_actual == expected
}

fn bare_type_names_match(actual string, expected string) bool {
	return actual == expected
}

fn return_numeric_alias_compatible(actual Type, expected Type) bool {
	if expected is Alias {
		clean_actual := if actual is Alias { actual.base_type } else { actual }
		clean_expected := expected.base_type
		if clean_expected.is_float() {
			return clean_actual.is_integer() || clean_actual.is_float()
		}
		if clean_expected.is_integer() {
			return clean_actual.is_integer()
		}
	}
	return false
}

fn (tc &TypeChecker) expr_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	return tc.type_compatible(actual, expected) || tc.zero_literal_can_be_pointer(expr_id, expected)
		|| tc.int_literal_can_be_char(expr_id, expected)
		|| tc.optional_pointer_expr_compatible(expr_id, actual, expected)
		|| tc.failure_literal_expr_compatible(expr_id, actual, expected)
}

fn (tc &TypeChecker) failure_literal_expr_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	tail_id := tc.branch_tail_expr_id(expr_id)
	if !tc.valid_node_id(tail_id) {
		return false
	}
	return tc.if_branch_type_compatible_with_context(actual, tail_id, expected)
}

fn (tc &TypeChecker) optional_pointer_expr_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if (actual is ResultType && expected is OptionType)
		|| (actual is OptionType && expected is ResultType) {
		return false
	}
	expected_base := match expected {
		OptionType { expected.base_type }
		ResultType { expected.base_type }
		else { return false }
	}
	if expected_base !is Pointer {
		return false
	}
	expected_ptr := expected_base as Pointer
	if tc.zero_literal_can_be_pointer(expr_id, expected_ptr) {
		return true
	}
	mut actual_base := match actual {
		OptionType { actual.base_type }
		ResultType { actual.base_type }
		else { actual }
	}
	if mut_base := tc.mut_param_expr_base(expr_id, actual) {
		actual_base = match mut_base {
			OptionType { mut_base.base_type }
			ResultType { mut_base.base_type }
			else { mut_base }
		}
	}
	if actual_base is Pointer || !tc.expr_can_take_address(expr_id) {
		return false
	}
	return tc.type_compatible(actual_base, expected_ptr.base_type)
}

fn (tc &TypeChecker) int_literal_can_be_char(expr_id flat.NodeId, expected Type) bool {
	if expected !is Char || int(expr_id) < 0 {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	return node.kind == .int_literal
}

fn (tc &TypeChecker) pointer_arithmetic_assign_compatible(op flat.Op, actual Type, expected Type) bool {
	if op !in [.plus_assign, .minus_assign] {
		return false
	}
	clean_expected := if expected is Alias { expected.base_type } else { expected }
	if clean_expected !is Pointer {
		return false
	}
	clean_actual := if actual is Alias { actual.base_type } else { actual }
	return clean_actual.is_integer()
}

fn (tc &TypeChecker) zero_literal_can_be_pointer(expr_id flat.NodeId, expected Type) bool {
	if !tc.is_zero_literal(expr_id) && !tc.expr_is_unsafe_zero_literal(expr_id) {
		return false
	}
	clean := if expected is Alias { expected.base_type } else { expected }
	return clean is Pointer
}

fn (tc &TypeChecker) expr_is_unsafe_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .block {
		return false
	}
	return tc.expr_tail_is_zero_literal(id)
}

fn (tc &TypeChecker) expr_tail_is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			return node.value == '0'
		}
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_zero_literal(tc.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_zero_literal(tc.a.child(&node, node.children_count - 1))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) mut_param_expr_base(expr_id flat.NodeId, typ Type) ?Type {
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind == .ident && node.value.len > 0 {
		return tc.mut_param_base_for_current_ident(node.value, typ)
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child := tc.a.nodes[int(tc.a.child(&node, 0))]
		if child.kind == .ident && child.value.len > 0 {
			base := tc.fn_context.mut_param_base_types[child.value] or { return none }
			if !tc.mut_param_binding_matches_lvalue(child.value) {
				return none
			}
			return Type(Pointer{
				base_type: base
			})
		}
	}
	return none
}

fn (tc &TypeChecker) current_checked_fn_qname() ?string {
	if tc.fn_context.node_id < 0 || tc.fn_context.node_id >= tc.a.nodes.len {
		return none
	}
	fn_node := tc.a.nodes[tc.fn_context.node_id]
	if fn_node.kind != .fn_decl || fn_node.value.len == 0 {
		return none
	}
	return checker_qualified_fn_name(tc.cur_module, fn_node.value)
}

// record_invalid_ierror_return_error records an invalid-ierror-return error,
// gating non-diagnostic-file sites on the called-fns closure. While that
// closure is still being computed on the collector thread (parallel check),
// the candidate is parked in pending_ierror_errors and filtered after join.
fn (mut tc TypeChecker) record_invalid_ierror_return_error(id flat.NodeId, msg string) {
	if tc.should_diagnose(id) {
		tc.record_error_unfiltered(.return_mismatch, msg, id)
		return
	}
	qname := tc.current_checked_fn_qname() or { return }
	if tc.defer_ierror_gating {
		tc.pending_ierror_errors << PendingIerrorError{
			err:      tc.make_type_error(.return_mismatch, msg, id)
			fn_qname: qname
		}
		return
	}
	if qname in tc.selected_file_called_fns {
		tc.record_error_unfiltered(.return_mismatch, msg, id)
	}
}

fn contextual_payload_type(typ Type) ?Type {
	if typ is OptionType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	if typ is ResultType {
		if typ.base_type is Void {
			return none
		}
		return typ.base_type
	}
	return none
}

fn (mut tc TypeChecker) invalid_ierror_return_expr_type_name(id flat.NodeId, expected ResultType) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	// An explicit return in a match/if branch is checked against the enclosing
	// function independently; it is not a value tail of the outer result expr.
	if node.kind == .return_stmt {
		return none
	}
	raw_type := tc.resolve_type(id)
	if tc.type_compatible(raw_type, expected) {
		return none
	}
	if tc.type_compatible(raw_type, expected.base_type) {
		return none
	}
	if tc.zero_literal_can_be_pointer(id, expected.base_type) {
		return none
	}
	if tc.pointer_value_compatible(raw_type, expected.base_type) {
		return none
	}
	if expected.base_type is Pointer
		&& tc.bare_value_pointer_return_compatible(id, raw_type, expected.base_type.base_type) {
		return none
	}
	payload_type := tc.resolve_expr(id, expected.base_type)
	if tc.type_compatible(payload_type, expected.base_type) {
		return none
	}
	if tc.pointer_value_compatible(payload_type, expected.base_type) {
		return none
	}
	if expected.base_type is Pointer
		&& tc.bare_value_pointer_return_compatible(id, payload_type, expected.base_type.base_type) {
		return none
	}
	match node.kind {
		.expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.invalid_ierror_return_expr_type_name(tc.a.child(&node, 0), expected)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				return tc.invalid_ierror_return_expr_type_name(tc.a.child(&node, 0), expected)
			}
		}
		.struct_init {
			concrete := tc.resolve_unqualified_builtin_error_struct_name(node.value) or {
				tc.resolve_selective_import_type_symbol(node.value) or {
					tc.qualify_name(node.value)
				}
			}
			if !tc.named_type_compatible_with_ierror(concrete) {
				return concrete
			}
		}
		.match_stmt {
			subject_id := tc.a.child(&node, 0)
			subject_key := tc.expr_key(subject_id)
			subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind != .match_branch {
					continue
				}
				tail := tc.branch_tail_expr_id(branch_id)
				if subject_key.len > 0 && tc.expr_key(tail) == subject_key && branch.value != 'else'
					&& branch.value.int() == 1 && subject_type is SumType {
					cond := tc.a.node(tc.a.child(&branch, 0))
					if pattern := tc.match_type_pattern(cond) {
						variant := tc.sum_variant_type_for_pattern(subject_type.name, pattern) or {
							pattern
						}
						if tc.type_compatible(tc.parse_type(variant), expected.base_type) {
							continue
						}
					}
				}
				if bad_type := tc.invalid_ierror_return_expr_type_name(tail, expected) {
					return bad_type
				}
			}
		}
		.if_expr {
			if node.children_count > 1 {
				then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
				if bad_type := tc.invalid_ierror_return_expr_type_name(then_tail, expected) {
					return bad_type
				}
			}
			if node.children_count > 2 {
				else_id := tc.a.child(&node, 2)
				else_tail := if tc.valid_node_id(else_id)
					&& tc.a.nodes[int(else_id)].kind == .if_expr {
					else_id
				} else {
					tc.branch_tail_expr_id(else_id)
				}
				if bad_type := tc.invalid_ierror_return_expr_type_name(else_tail, expected) {
					return bad_type
				}
			}
		}
		else {
			if is_ierror_type(raw_type) || tc.type_compatible_with_ierror_payload(raw_type) {
				return none
			}
			if raw_type is Unknown || raw_type is Void {
				return none
			}
			return raw_type.name()
		}
	}

	return none
}

fn (tc &TypeChecker) source_declares_bodyless_function(name string) bool {
	return name in tc.source_no_body_fn_suffixes
		|| name.all_after_last('.') in tc.source_no_body_fn_suffixes
}

fn multi_return_payload_type(typ Type) ?MultiReturn {
	if typ is MultiReturn {
		return typ
	}
	if typ is OptionType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	if typ is ResultType {
		base := typ.base_type
		if base is MultiReturn {
			return base
		}
	}
	return none
}

fn (tc &TypeChecker) multi_return_assignment_type(rhs_id flat.NodeId, rhs_type Type) ?MultiReturn {
	if rhs_type is MultiReturn {
		return rhs_type
	}
	if !tc.expr_has_option_result_handler(rhs_id) {
		return none
	}
	return multi_return_payload_type(rhs_type)
}

fn (tc &TypeChecker) unhandled_wrapped_multi_return_type(rhs_id flat.NodeId, rhs_type Type) ?MultiReturn {
	if rhs_type is MultiReturn || tc.expr_has_option_result_handler(rhs_id) {
		return none
	}
	return multi_return_payload_type(rhs_type)
}

fn (tc &TypeChecker) expr_has_option_result_handler(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .or_expr {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_has_option_result_handler(tc.a.child(&node, 0))
	}
	return false
}

// check_call validates check call state for types.
@[direct_array_access]
fn (mut tc TypeChecker) check_call(id flat.NodeId, node flat.Node) {
	if node.children_count > 0 {
		if tc.check_json_magic_call(id, node) {
			return
		}
		if tc.check_js_call_on_non_js_backend(id, node) {
			return
		}
		callee_id := tc.a.child(&node, 0)
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .ident && callee.value in ['print', 'println', 'eprint', 'eprintln'] {
			for i in 1 .. node.children_count {
				arg_id := tc.call_arg_value(tc.a.child(&node, i))
				if element_id := tc.shared_array_element_index(arg_id) {
					tc.record_error_at(.call_arg_mismatch,
						'you have to create a handle and `rlock` it to use a `shared` element as non-mut argument to print',
						element_id, tc.a.node(element_id).pos)
					continue
				}
				if access := tc.unlocked_shared_access(arg_id) {
					tc.record_error_at(.call_arg_mismatch,
						'`${access.name}` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut argument to print',
						arg_id, access.pos)
				}
			}
		}
		if indexed_kind := tc.non_function_index_call_kind(node) {
			tc.check_node(callee_id)
			pos := tc.call_closing_paren_pos(node)
			tc.record_error_at(.unknown_fn,
				'cannot call the ${indexed_kind} of the ${tc.non_function_index_container_name(callee)}, it is not a function',
				id, pos)
			tc.record_error_at(.unknown_fn, 'unknown function:', id, pos)
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.register_synth_type(id, Type(MultiReturn{
				types: []Type{}
			}))
			return
		}
		if callee.kind == .ident && callee.value !in tc.a.disabled_fns
			&& tc.source_declares_bodyless_function(callee.value) {
			tc.record_error_at(.unknown_fn, 'cannot call a function that does not have a body', id,
				node.pos)
			return
		}
		if callee.kind == .selector && callee.children_count > 0 {
			receiver_id := tc.a.child(callee, 0)
			receiver := tc.a.node(receiver_id)
			if callee.value == 'from_string' && receiver.kind == .ident
				&& !tc.ident_resolves_to_value(receiver.value)
				&& tc.static_assoc_fn_key_for_base(receiver.value, callee.value) == none
				&& tc.resolve_enum_name(receiver.value) == none {
				qname := tc.qualify_name(receiver.value)
				if qname in tc.structs || receiver.value in tc.structs {
					tc.record_error_at(.unknown_fn,
						'expected enum, but `${receiver.value}` is struct', id, node.pos)
					return
				}
				tc.record_error_at(.unknown_type, 'unknown enum `${receiver.value}`', id, node.pos)
				for i in 1 .. node.children_count {
					tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
				}
				return
			}
			if callee.value == 'from_string' && receiver.kind == .ident
				&& !tc.ident_resolves_to_value(receiver.value) {
				if enum_name := tc.resolve_enum_name(receiver.value) {
					arg_count := node.children_count - 1
					if arg_count != 1 {
						tc.record_error_at(.call_arg_mismatch,
							'expected 1 argument, but got ${arg_count}', id, node.pos)
						tc.register_synth_type(id, Type(OptionType{
							base_type: Type(Enum{
								name:    enum_name
								is_flag: enum_name in tc.flag_enums
							})
						}))
						return
					}
					arg_id := tc.call_arg_value(tc.a.child(&node, 1))
					tc.check_node(arg_id)
					actual := tc.resolve_type(arg_id)
					if unalias_type(actual) !is String {
						tc.record_error_at(.call_arg_mismatch, 'expected `string` argument, but got `${tc.diagnostic_expr_type_name(arg_id,
							actual)}`', id, node.pos)
						tc.register_synth_type(id, Type(OptionType{
							base_type: Type(Enum{
								name:    enum_name
								is_flag: enum_name in tc.flag_enums
							})
						}))
						return
					}
				}
			}
			if !(receiver.kind == .ident && receiver.value == 'C') {
				receiver_name := tc.resolve_type(receiver_id).name()
				method_name := '${receiver_name}.${callee.value}'
				if method_name in tc.a.disabled_fns || method_name in tc.source_no_body_fns {
					name_pos := tc.method_call_name_pos(node, callee)
					tc.record_error_at(.unknown_fn,
						'cannot call a method that does not have a body', id, token.new_span(name_pos.id,
						name_pos.offset, node.pos.end))
					return
				}
			}
		}
		if callee.kind == .ident && callee.value == 'main' && !tc.cur_file.ends_with('_test.v') {
			tc.record_error_at(.call_arg_mismatch,
				'the `main` function cannot be called in the program', id, node.pos)
		}
		if callee.kind == .ident && (callee.value in tc.fn_generic_params
			|| tc.qualify_fn_name(callee.value) in tc.fn_generic_params) {
			for i in 1 .. node.children_count {
				arg_id := tc.call_arg_value(tc.a.child(&node, i))
				arg := tc.a.node(arg_id)
				if arg.kind == .none_expr {
					tc.record_error_at(.call_arg_mismatch, 'cannot use `none` as generic argument',
						arg_id, arg.pos)
					return
				}
			}
		}
		if callee.kind == .call {
			tc.check_node(callee_id)
		}
		if callee.kind == .index && callee.children_count > 1 {
			mut has_unbound_generic_type_arg := false
			for i in 1 .. callee.children_count {
				type_arg_id := tc.a.child(callee, i)
				type_arg := tc.a.node(type_arg_id)
				if type_arg.kind == .ident
					&& (type_arg.value.starts_with('?') || type_arg.value.starts_with('!')) {
					tc.record_error_at(.call_arg_mismatch,
						'cannot use Option type name as concrete type', type_arg_id, token.new_span(type_arg.pos.id,
						type_arg.pos.offset, type_arg.pos.offset + 1))
					return
				}
				type_name := tc.generic_call_type_arg_name(type_arg_id)
				if is_bare_generic_param(type_name) && !tc.type_name_known(type_name)
					&& type_name !in tc.fn_context.generic_params
					&& !tc.active_generic_param(type_name)
					&& !tc.node_has_enclosing_generic_param(type_arg_id, type_name)
					&& !tc.source_enclosing_fn_has_generic_param(type_arg_id, type_name) {
					has_unbound_generic_type_arg = true
				}
				if type_name.len > 1 && !tc.type_text_has_generic_placeholder(type_name) {
					if should_check_named_type(type_name) && !tc.type_name_known(type_name) {
						tc.record_error_at(.unknown_type, tc.unknown_type_message(type_name,
							type_arg_id), type_arg_id,
							tc.generic_type_arg_diagnostic_pos(type_arg_id))
						tc.register_synth_type(id, unknown_type('invalid generic type argument'))
						continue
					}
					before := tc.errors.len
					tc.check_type_string_for_unsupported_generics(type_name, type_arg_id,
						generic_param_map_from_names(tc.fn_context.generic_params))
					if tc.errors.len > before {
						tc.register_synth_type(id, unknown_type('invalid generic type argument'))
					}
				}
			}
			if has_unbound_generic_type_arg {
				tc.record_error_at(.unsupported_generic,
					'generic fn using generic types cannot be called outside of generic fn', id,
					node.pos)
				tc.register_synth_type(id, unknown_type('generic call with unbound type argument'))
				return
			}
		}
		if callee.kind == .ident && tc.ident_resolves_to_value(callee.value) {
			callee_type := unalias_type(tc.resolve_type(callee_id))
			if callee_type is OptionType && fn_type_from_type(callee_type.base_type) != none {
				tc.record_error_at(.call_arg_mismatch,
					'type `${tc.resolve_type(callee_id).name()}` is an Option, it must be unwrapped first',
					id, node.pos)
				return
			}
			if callee_type is Pointer && fn_type_from_type(callee_type.base_type) != none {
				tc.record_error_at(.call_arg_mismatch,
					'function pointer must be undereferenced first', id, node.pos)
				return
			}
		}
		if callee.kind == .selector {
			if callee.children_count > 0 {
				base := tc.a.child_node(callee, 0)
				if base.kind == .ident && !tc.ident_resolves_to_value(base.value) {
					for type_name in [base.value, tc.qualify_name(base.value)] {
						key := '${type_name}.${callee.value}'
						if tc.fn_signature_known(key) && !tc.fn_key_is_static_associated(key)
							&& tc.static_assoc_type_known(type_name) {
							tc.record_error(.unknown_fn,
								'unknown function: ${base.value}.${callee.value}', id)
							tc.register_synth_type(id, Type(MultiReturn{
								types: []Type{}
							}))
							return
						}
					}
				}
				c_name := 'C.${callee.value}'
				if base.kind == .ident && base.value == 'C' && c_name !in tc.fn_ret_types
					&& c_name !in tc.structs {
					tc.record_error_at(.unknown_fn,
						'unknown C function: `${c_name}`. `C.` calls are external C calls; declare the function with `fn ${c_name}(...)` and include/link the C header/library that provides it.',
						id, tc.method_call_name_pos(node, callee))
					tc.register_synth_type(id, Type(MultiReturn{
						types: []Type{}
					}))
					return
				}
			}
			if wrapped_fn := tc.selector_declared_value_type(*callee) {
				if wrapped_fn is OptionType && fn_type_from_type(wrapped_fn.base_type) != none {
					name_pos := tc.method_call_name_pos(node, callee)
					tc.record_error_at(.call_arg_mismatch,
						'Option function field must be unwrapped first', id, token.new_span(name_pos.id,
						name_pos.offset, node.pos.end))
					return
				}
			}
			if callee.children_count > 0 {
				receiver_id := tc.a.child(callee, 0)
				receiver_type := unalias_type(tc.resolve_type(receiver_id))
				if callee.value == 'str' && receiver_type is Char {
					tc.record_error_at(.call_arg_mismatch,
						'calling `.str()` on type `char` is not allowed, use its address or cast it to an integer instead',
						id, node.pos)
					tc.register_synth_type(id, Type(string_))
					return
				}
				if callee.value == 'str' && receiver_type is Interface
					&& tc.interface_method_signature_key(receiver_type.name, 'str') == none {
					name_pos := tc.method_call_name_pos(node, callee)
					tc.record_error_at(.unknown_fn,
						'interface `${receiver_type.name.all_after_last('.')}` does not have a .str() method. Use typeof() instead',
						id, token.new_span(name_pos.id, name_pos.offset, node.pos.end))
					tc.remember_resolved_call(id, '${receiver_type.name}.str')
					tc.register_synth_type(id, Type(string_))
					return
				}
				if receiver_type is OptionType {
					tc.record_error_at(.call_arg_mismatch,
						'Option type `${tc.wrapped_receiver_payload_diagnostic_name(receiver_type.base_type)}` cannot be called directly, you should unwrap it first',
						receiver_id, tc.wrapped_operand_diagnostic_pos(receiver_id))
					return
				}
				if receiver_type is ResultType {
					tc.record_unhandled_result_call(receiver_id, receiver_type)
					tc.record_error_at(.call_arg_mismatch, 'Result type cannot be called directly',
						receiver_id, tc.wrapped_operand_diagnostic_pos(receiver_id))
					if tc.wrapped_receiver_has_method(receiver_type.base_type, callee.value) {
						tc.record_error_at(.call_arg_mismatch,
							'${callee.value}() returns `!${receiver_type.base_type.name()}`, so it should have either an `or {}` block, or `!` at the end',
							id, tc.method_call_name_pos(node, callee))
					}
					return
				}
			}
		}
		if callee.kind == .fn_literal || callee.kind == .lambda_expr {
			tc.check_node(callee_id)
		}
		if callee.kind == .ident && (callee.value in tc.fn_ret_types
			|| tc.qualify_fn_name(callee.value) in tc.fn_ret_types) {
			if _ := tc.non_file_scope_type(callee.value) {
				tc.record_error(.call_arg_mismatch,
					'ambiguous call to: `${callee.value}`, may refer to fn `${callee.value}` or variable `${callee.value}`',
					id)
			}
		}
		if callee.kind == .ident && callee.value == '__v_compile_error' {
			current_fn_id := flat.NodeId(tc.fn_context.node_id)
			if tc.valid_node_id(current_fn_id) && tc.a.node(current_fn_id).generic_params().len > 0 {
				return
			}
			message := if node.children_count > 1 {
				arg := tc.a.child_node(&node, 1)
				if arg.value.len > 0 {
					arg.value
				} else {
					'compile-time error'
				}
			} else {
				'compile-time error'
			}
			if !tc.has_type_error(.compile_error, message, id) {
				tc.record_error_unfiltered(.compile_error, message, id)
			}
			return
		}
		if callee.kind == .ident && callee.value == '__v_compile_warn' {
			current_fn_id := flat.NodeId(tc.fn_context.node_id)
			if tc.valid_node_id(current_fn_id) {
				current_fn := tc.a.node(current_fn_id)
				if current_fn.generic_params().len > 0
					|| tc.specialized_generic_fns[current_fn.value]
					|| (int(current_fn_id) < tc.a.specialized_fn_nodes.len
					&& tc.a.specialized_fn_nodes[int(current_fn_id)]) {
					return
				}
			}
			message := if node.children_count > 1 {
				arg := tc.a.child_node(&node, 1)
				if arg.value.len > 0 {
					arg.value
				} else {
					'compile-time warning'
				}
			} else {
				'compile-time warning'
			}
			if !tc.has_type_notice(.compile_error, message, id) {
				tc.record_warning_at(.compile_error, message, id, node.pos)
			}
			return
		}
		if callee.kind == .selector && callee.value.starts_with('$') && callee.value.len > 1 {
			tc.check_dynamic_comptime_method_call(id, node, callee)
			return
		}
		if callee.kind == .selector && callee.value == '$' {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			return
		}
		if callee.kind == .selector && callee.children_count > 0 {
			receiver_id := tc.a.child(callee, 0)
			receiver := tc.a.nodes[int(receiver_id)]
			if receiver.kind == .ident && receiver.value == 'C' && 'C.${callee.value}' in tc.structs
				&& node.children_count == 2 {
				arg_id := tc.call_arg_value(tc.a.child(&node, 1))
				if tc.expr_tail_is_nil(arg_id) && tc.node_source_starts_with(arg_id, 'unsafe') {
					name_pos := tc.method_call_name_pos(node, callee)
					tc.record_error_at(.assignment_mismatch, 'cannot cast `voidptr` to struct', id, token.new_span(name_pos.id,
						name_pos.offset, node.pos.end))
					return
				}
			}
			receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
			if receiver_type is Struct {
				candidates := tc.embedded_method_candidates(receiver_type.name, callee.value)
				if candidates.len > 1 {
					pos := tc.method_call_name_pos(node, callee)
					tc.record_error_at(.unknown_fn, 'ambiguous method `${callee.value}`', id, pos)
					tc.record_error_at(.unknown_fn,
						'unknown method or field: `${receiver_type.name}.${callee.value}`', id, pos)
					return
				}
			}
		}
		if callee.kind == .selector && callee.value == 'free' && callee.children_count > 0 {
			receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(callee, 0)))
			receiver_name := receiver_type.name()
			if receiver_type is ArrayFixed {
				tc.record_error_at(.call_arg_mismatch,
					'unknown method or field: ${receiver_name}.free()', id, tc.method_call_name_pos(node,
					callee))
				return
			}
		}
		if callee.kind == .selector && callee.children_count > 0
			&& callee.value in ['clear', 'delete', 'delete_last', 'insert', 'pop', 'pop_left', 'prepend', 'reverse_in_place', 'sort', 'sort_with_compare'] {
			receiver_id := tc.a.child(callee, 0)
			receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
			if receiver_type is ArrayFixed && callee.value != 'sort' {
				tc.check_builtin_array_mutable_receiver(receiver_id)
			}
		}
		if callee.kind == .selector && callee.value == 'sort' && callee.children_count > 0 {
			receiver_id := tc.a.child(callee, 0)
			receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
			if receiver_type is Array || receiver_type is ArrayFixed {
				receiver := tc.a.nodes[int(receiver_id)]
				if receiver.kind == .call {
					tc.record_error_at(.call_arg_mismatch,
						'the `sort()` method can be called only on mutable receivers, but `${tc.call_display_name(receiver)}()` is a call expression',
						id, tc.method_call_name_pos(node, callee))
					tc.record_error(.call_arg_mismatch, 'cannot pass expression as `mut`',
						receiver_id)
					return
				}
				if tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id) {
					tc.check_builtin_array_mutable_receiver(receiver_id)
				}
				tc.check_array_sort_call(id, node, callee)
				return
			}
		}
		if callee.kind == .ident && callee.value in ['print', 'println', 'eprint', 'eprintln']
			&& node.children_count > 1 {
			arg_id := tc.call_arg_value(tc.a.child(&node, 1))
			arg_node := tc.a.node(arg_id)
			if arg_node.kind == .prefix && arg_node.value == '...' {
				if arg_node.children_count > 0 {
					tc.check_node(tc.a.child(arg_node, 0))
				}
				tc.record_error_at(.call_arg_mismatch,
					'`${callee.value}` cannot print variadic values', id, node.pos)
				return
			}
			arg_type := if arg_node.kind == .selector {
				tc.selector_type(arg_id, arg_node) or { tc.resolve_type(arg_id) }
			} else {
				tc.resolve_type(arg_id)
			}
			if arg_type is Char {
				tc.record_error_at(.call_arg_mismatch,
					'`${callee.value}` cannot print type `char` directly, print its address or cast it to an integer instead',
					id, node.pos)
				return
			}
			arg_is_void := arg_type is Void || (arg_type is MultiReturn && arg_type.types.len == 0)
			arg_is_send := arg_node.kind == .infix && arg_node.op == .arrow
			has_wrapped_receiver := tc.call_has_wrapped_receiver(arg_id)
			invalid_pointer_infix := tc.invalid_pointer_infix_print_arg(arg_id)
			invalid_index_call := tc.invalid_non_function_index_call(arg_id)
			invalid_interface_selector := tc.invalid_interface_selector_print_arg(arg_id)
			invalid_struct_selector := tc.invalid_struct_selector_print_arg(arg_id)
			invalid_unknown_method := tc.invalid_unknown_method_print_arg(arg_id)
			invalid_import_function := tc.invalid_unknown_import_function_print_arg(arg_id)
			if arg_is_void || arg_is_send || has_wrapped_receiver || invalid_pointer_infix
				|| invalid_index_call || invalid_interface_selector || invalid_struct_selector
				|| invalid_unknown_method || invalid_import_function {
				if arg_is_void && tc.unresolved_local_method_call(arg_id) {
					return
				}
				tc.check_node(arg_id)
				if tc.resolution_type_mode {
					return
				}
				if type_has_runtime_value(tc.resolve_type(arg_id)) && !invalid_pointer_infix {
					return
				}
				if !tc.expr_subtree_has_error(arg_id) || tc.a.node(arg_id).kind == .infix
					|| arg_node.kind == .defer_result || invalid_index_call
					|| invalid_interface_selector || invalid_struct_selector
					|| invalid_unknown_method || invalid_import_function || has_wrapped_receiver {
					tc.record_error(.call_arg_mismatch,
						'`${callee.value}` can not print void expressions', id)
				}
				return
			}
		}
	}
	if local_fn := tc.explicit_generic_local_fn_value(node) {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		tc.remember_expr_type(id, local_fn.return_type)
		return
	}
	if tc.call_has_explicit_generic_type_args(node) {
		callee_id := tc.a.child(&node, 0)
		callee := tc.a.node(callee_id)
		base := tc.a.child_node(callee, 0)
		if target := tc.generic_call_base_name(*base) {
			if (tc.fn_generic_params[target] or { []string{} }).len == 0 {
				tc.record_error_at(.unsupported_generic,
					'a non generic function called like a generic one', callee_id,
					tc.explicit_generic_args_diagnostic_pos(callee_id))
				for i in 1 .. node.children_count {
					tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
				}
				if return_type := tc.fn_ret_types[target] {
					tc.remember_expr_type(id, return_type)
				}
				return
			}
		}
	}
	if !tc.resolution_type_mode && tc.call_has_explicit_generic_type_args(node)
		&& !tc.explicit_generic_call_target_is_known(node)
		&& !tc.call_generic_args_have_placeholders(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_fn, 'unknown function: ${tc.call_display_name(node)}', id)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	if arg_id := tc.builtin_isreftype_call_arg(node) {
		tc.check_isreftype_arg(arg_id)
		tc.remember_expr_type(id, Type(bool_))
		return
	}
	if arg_id := tc.builtin_addr_call_arg(node) {
		if tc.unsafe_depth == 0 {
			tc.record_error(.call_arg_mismatch, '`__addr` must be called from an unsafe block', id)
		}
		arg_type := tc.resolve_type(arg_id)
		tc.remember_expr_type(id, Type(Pointer{
			base_type: arg_type
		}))
		return
	}
	if sum_name := tc.sum_constructor_call_name(node) {
		tc.check_sum_constructor_call(id, node, sum_name)
		return
	}
	if info0 := tc.resolve_call_info(id, node) {
		if tc.record_empty_array_generic_call_errors(node, info0) {
			return
		}
		if !info0.has_receiver && !tc.call_has_explicit_generic_type_args(node)
			&& tc.call_generic_params(info0.name).len > 0
			&& node.children_count == 1 + info0.arg_offset {
			tc.record_error_at(.unsupported_generic,
				'no argument generic function must add concrete types, e.g. foo[int]()', id,
				node.pos)
			tc.register_synth_type(id, unknown_type('generic call without concrete type arguments'))
			return
		}
		info := tc.specialized_plain_generic_call_info(node, info0)
		tc.record_uninferred_generic_method_type(id, node, info0)
		tc.record_chained_bare_generic_struct_method_inference_error(id, node, info)
		if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
			tc.remember_resolved_call(id, info.name)
		}
		if info.return_type !is Void && info.return_type !is Unknown {
			tc.remember_expr_type(id, info.return_type)
		}
		if tc.check_call_privacy(id, node, info) {
			return
		}
		if tc.unsafe_depth == 0 && !tc.current_fn_declared_unsafe()
			&& !tc.node_is_in_translated_file(id)
			&& (info.name in tc.unsafe_fns || tc.is_builtin_unsafe_c_call(node, info.name))
			&& info.name !in ['map.delete', 'builtin.map.delete'] {
			callee := tc.a.child_node(&node, 0)
			if info.has_receiver && callee.kind == .selector {
				method_name := info.name.trim_string_left('main.')
				name_pos := tc.method_call_name_pos(node, callee)
				tc.record_warning_at(.call_arg_mismatch,
					'method `${method_name}` must be called from an `unsafe` block', id, token.new_span(name_pos.id,
					name_pos.offset, node.pos.end))
			} else if callee.kind == .selector && callee.children_count > 0
				&& tc.a.child_node(callee, 0).value == 'C' {
				name_pos := tc.method_call_name_pos(node, callee)
				tc.record_warning_at(.call_arg_mismatch,
					'function `${tc.call_display_name(node)}` must be called from an `unsafe` block',
					id, token.new_span(name_pos.id, name_pos.offset, node.pos.end))
			} else {
				tc.record_warning_at(.call_arg_mismatch,
					'function `${tc.call_display_name(node)}` must be called from an `unsafe` block',
					id, node.pos)
			}
		}
		tc.check_call_deprecation(id, node, info)
		tc.check_call_arg_types(id, node, info)
		tc.check_os_file_raw_io_call(id, node, info)
		tc.check_instantiated_generic_as_casts(node, info)
		tc.check_instantiated_generic_ordering_ops(node, info)
		tc.check_instantiated_generic_compile_errors(id, node, info)
		tc.check_instantiated_generic_compile_warnings(node, info)
		$if ownership ? {
			tc.ownership_after_call(id, node, info)
		}
		return
	}
	if tc.is_unsupported_hex_call(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_fn, 'unknown function: ${tc.call_display_name(node)}', id)
		}
		return
	}
	if tc.call_has_ambiguous_selective_import(node) {
		tc.record_error(.unknown_fn, 'ambiguous selective import `${tc.call_display_name(node)}`',
			id)
		return
	}
	if tc.record_void_receiver_method_call(id, node) {
		return
	}
	if tc.should_diagnose(id) && !tc.is_known_call(node)
		&& !tc.call_generic_args_have_placeholders(node) && (!tc.call_receiver_type_is_unknown(node)
		|| tc.unknown_import_function_call_parts(node) != none) {
		if !tc.record_unknown_import_function_call(id, node) {
			if tc.record_unknown_method_call(id, node) {
				return
			}
			tc.record_error(.unknown_fn, 'unknown function: ${tc.call_display_name(node)}', id)
			tc.register_synth_type(id, Type(MultiReturn{
				types: []Type{}
			}))
		}
	}
	dsl_name := tc.unresolved_array_dsl_call_name(node)
	if dsl_name.len > 0 {
		tc.push_array_dsl_scope(node, dsl_name)
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
	if dsl_name.len > 0 {
		tc.pop_scope()
	}
}

fn (mut tc TypeChecker) record_void_receiver_method_call(id flat.NodeId, node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(callee, 0)
	receiver := tc.a.node(receiver_id)
	if receiver.kind != .call || unalias_type(tc.resolve_type(receiver_id)) !is Void {
		return false
	}
	name_pos := tc.method_call_name_pos(node, callee)
	tc.record_error_severity_at(.compile_error,
		'checker bug; CallExpr.receiver_type is 0 in method_call', id, token.new_span(name_pos.id,
		name_pos.offset, node.pos.end), 'cgen error:')
	tc.register_synth_type(id, Type(MultiReturn{
		types: []Type{}
	}))
	return true
}

fn os_raw_io_unsupported_type_message(path string, type_name string) string {
	if path.len == 0 {
		return 'contains non-plain-data values of type `${type_name}`'
	}
	return 'contains field `${path}` of type `${type_name}`'
}

fn (tc &TypeChecker) is_builtin_unsafe_c_call(node flat.Node, name string) bool {
	if !name.starts_with('C.m') && !name.starts_with('C.s') {
		return false
	}
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	base := tc.a.child_node(callee, 0)
	owner_module := tc.fn_type_modules[name] or { '' }
	return base.kind == .ident && base.value == 'C' && owner_module == 'builtin'
		&& tc.unsafe_c_fns[c_fn_module_signature_key(owner_module, name)]
}

fn (tc &TypeChecker) os_raw_io_unsupported_type(typ Type, path string, mut checked map[string]bool) ?string {
	original_name := typ.name()
	mut clean := typ
	for clean is OptionType || clean is ResultType {
		clean = match clean {
			OptionType { clean.base_type }
			ResultType { clean.base_type }
			else { clean }
		}
	}
	clean = unalias_type(clean)
	if clean is Pointer {
		return os_raw_io_unsupported_type_message(path, original_name)
	}
	match clean {
		String, Array, Map, Channel, FnType, Interface, SumType, MultiReturn, Nil, None {
			return os_raw_io_unsupported_type_message(path, original_name)
		}
		ArrayFixed {
			if tc.os_raw_io_unsupported_type(clean.elem_type, path, mut checked) != none {
				return os_raw_io_unsupported_type_message(path, original_name)
			}
		}
		Struct {
			if checked[clean.name] {
				return none
			}
			checked[clean.name] = true
			for field in tc.struct_fields_for_init(clean.name) {
				field_path := if path.len == 0 {
					field.name
				} else {
					'${path}.${field.name}'
				}
				if reason := tc.os_raw_io_unsupported_type(field.typ, field_path, mut checked) {
					return reason
				}
			}
		}
		else {}
	}
	return none
}

fn (tc &TypeChecker) os_raw_io_call_result_pos(node flat.Node, method string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	position := file.position(node.pos)
	line_start := file.line_start(position.line)
	mut line_end := source.index_after('\n', line_start) or { source.len }
	for line_end > line_start && source[line_end - 1] in [`\n`, `\r`] {
		line_end--
	}
	line := source[line_start..line_end]
	method_relative := line.index('.${method}') or { return node.pos }
	start := line_start + method_relative + 1
	return token.new_span(node.pos.id, start, line_end)
}

fn (mut tc TypeChecker) check_os_file_raw_io_call(id flat.NodeId, node flat.Node, info CallInfo) {
	if !info.has_receiver || !info.name.contains('os.File.') {
		return
	}
	method := info.name.all_after_last('.')
	if method !in ['write_struct', 'write_struct_at', 'write_raw', 'write_raw_at', 'read_struct',
		'read_struct_at', 'read_raw', 'read_raw_at'] {
		return
	}
	mut raw_type := Type(Unknown{})
	mut diagnostic_id := flat.empty_node
	mut diagnostic_pos := node.pos
	mut explicit_type_arg_id := flat.empty_node
	callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 1 {
		explicit_type_arg_id = tc.a.child(callee, callee.children_count - 1)
		type_name := tc.generic_call_type_arg_name(explicit_type_arg_id)
		if type_name.len > 0 {
			raw_type = tc.parse_type(type_name)
		}
	}
	if method.starts_with('read_raw') {
		if raw_type is Unknown {
			result_type := unalias_type(info.return_type)
			raw_type = match result_type {
				ResultType { result_type.base_type }
				OptionType { result_type.base_type }
				else { result_type }
			}
		}
		diagnostic_id = id
		diagnostic_pos = tc.os_raw_io_call_result_pos(node, method)
	} else if node.children_count > 1 + info.arg_offset {
		diagnostic_id = tc.call_arg_value(tc.a.child(&node, 1 + info.arg_offset))
		tc.check_node(diagnostic_id)
		raw_type = tc.resolve_type(diagnostic_id)
		diagnostic_pos = tc.a.node(diagnostic_id).pos
	}
	for raw_type is Pointer {
		raw_type = raw_type.base_type
	}
	if raw_type is Unknown || raw_type is Void {
		return
	}
	clean_type := unalias_type(raw_type)
	if method.contains('struct') && clean_type !is Struct {
		struct_pos := if tc.valid_node_id(explicit_type_arg_id) {
			tc.generic_type_arg_diagnostic_pos(explicit_type_arg_id)
		} else {
			diagnostic_pos
		}
		tc.record_error_at(.call_arg_mismatch,
			'`os.File.${method}` expects a struct type, but got `${raw_type.name()}`',
			diagnostic_id, struct_pos)
	}
	mut checked := map[string]bool{}
	if reason := tc.os_raw_io_unsupported_type(raw_type, '', mut checked) {
		tc.record_error_at(.call_arg_mismatch,
			'`os.File.${method}` only supports plain data types; `${raw_type.name()}` ${reason}',
			diagnostic_id, diagnostic_pos)
	}
}

fn (tc &TypeChecker) unresolved_local_method_call(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver := tc.a.child_node(callee, 0)
	if receiver.kind != .ident || receiver.value.len == 0 || receiver.value[0] < `a`
		|| receiver.value[0] > `z` {
		return false
	}
	if _ := tc.non_file_scope_type(receiver.value) {
		return false
	}
	return true
}

fn (mut tc TypeChecker) check_json_magic_call(id flat.NodeId, node flat.Node) bool {
	name := tc.call_display_name(node)
	if name in ['json.encode', 'json.encode_pretty'] && node.children_count > 1 {
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		if tc.expr_is_shared_arg(arg_id) {
			tc.check_node(arg_id)
			tc.record_error_at(.call_arg_mismatch, 'json.encode cannot handle shared data', id,
				tc.magic_call_diagnostic_pos(node))
			tc.register_synth_type(id, Type(void_))
			tc.record_enclosing_print_void(id)
			return true
		}
		return false
	}
	if name != 'json.decode' || node.children_count < 2 {
		return false
	}
	type_arg_id := tc.a.child(&node, 1)
	mut type_name := tc.generic_call_type_arg_name(type_arg_id)
	if type_name.len == 0 {
		type_name = tc.json_decode_type_arg_text(id)
	}
	if type_name.len == 0 {
		return false
	}
	call_pos := tc.magic_call_diagnostic_pos(node)
	if node.children_count < 3 {
		tc.record_error_at(.call_arg_mismatch,
			"json.decode expects 2 arguments, a type and a string (e.g `json.decode(T, '')`)", id,
			call_pos)
		tc.register_synth_type(id, Type(void_))
		return true
	}
	target_type := tc.parse_type(type_name)
	if target_type is Pointer {
		pointer_pos := tc.type_diagnostic_pos(type_arg_id, '&')
		tc.record_error_at(.unknown_type, 'json.decode: cannot decode into a pointer type',
			type_arg_id,
			token.new_span(pointer_pos.id, pointer_pos.offset + 1, pointer_pos.end + 1))
		tc.register_synth_type(id, Type(ResultType{
			base_type: target_type
		}))
		return true
	}
	if !tc.type_name_known(type_name) {
		tc.record_error_at(.unknown_type, 'json.decode: unknown type `${type_name}`', id, call_pos)
		tc.register_synth_type(id, unknown_type('unknown json.decode target'))
		return true
	}
	clean_target := unalias_type(target_type)
	if clean_target !is Struct && clean_target !is SumType && clean_target !is Map
		&& clean_target !is Array && clean_target !is ArrayFixed {
		tc.record_error_at(.unknown_type,
			'json.decode: expected sum type, struct, map or array, found ${clean_target.name()}',
			type_arg_id, tc.a.node(type_arg_id).pos)
		tc.register_synth_type(id, Type(ResultType{
			base_type: target_type
		}))
		return true
	}
	value_id := tc.call_arg_value(tc.a.child(&node, 2))
	tc.check_node(value_id)
	if unalias_type(tc.resolve_type(value_id)) !is String {
		tc.record_error_at(.call_arg_mismatch, 'json.decode: second argument needs to be a string',
			id, call_pos)
		tc.register_synth_type(id, Type(ResultType{
			base_type: target_type
		}))
		return true
	}
	return false
}

fn (tc &TypeChecker) json_decode_type_arg_text(id flat.NodeId) string {
	source := tc.source_text_for_node(id)
	open_paren := source.index_u8(`(`)
	if open_paren < 0 {
		return ''
	}
	mut depth := 0
	for i := open_paren + 1; i < source.len; i++ {
		match source[i] {
			`(`, `[`, `{` {
				depth++
			}
			`)` {
				if depth == 0 {
					return source[open_paren + 1..i].trim_space()
				}
				depth--
			}
			`]`, `}` {
				if depth > 0 {
					depth--
				}
			}
			`,` {
				if depth == 0 {
					return source[open_paren + 1..i].trim_space()
				}
			}
			else {}
		}
	}
	return ''
}

fn (tc &TypeChecker) magic_call_diagnostic_pos(node flat.Node) token.Pos {
	if node.children_count == 0 {
		return node.pos
	}
	callee_id := tc.a.child(&node, 0)
	name_pos := tc.node_value_diagnostic_pos(callee_id)
	return token.new_span(name_pos.id, name_pos.offset, node.pos.end)
}

fn (mut tc TypeChecker) check_js_call_on_non_js_backend(id flat.NodeId, node flat.Node) bool {
	source := tc.source_text_for_node(id)
	if !source.starts_with('JS.') {
		return false
	}
	open_paren := source.index_u8(`(`)
	if open_paren < 3 {
		return false
	}
	full_name := source[..open_paren]
	short_name := full_name[3..]
	params := tc.fn_param_types[full_name] or { tc.fn_param_types[short_name] or { return false } }
	actual_count := int(node.children_count) - 1
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
	if actual_count > params.len {
		pos := token.new_span(node.pos.id, node.pos.offset + 3, node.pos.end)
		tc.record_error_at(.call_arg_mismatch,
			'too many arguments in call to `${full_name}` (non-js backend: c)', id, pos)
	}
	tc.register_synth_type(id, tc.fn_ret_types[full_name] or {
		tc.fn_ret_types[short_name] or { Type(void_) }
	})
	return true
}

fn (mut tc TypeChecker) check_call_privacy(id flat.NodeId, node flat.Node, info CallInfo) bool {
	if info.name.len == 0 {
		return false
	}
	if _ := tc.private_declaration(info.name) {
		callee := tc.a.child_node(&node, 0)
		if info.has_receiver && callee.kind == .selector && callee.children_count > 0 {
			receiver_id := tc.a.child(callee, 0)
			name_pos := tc.method_call_name_pos(node, callee)
			receiver_type := tc.resolve_type(receiver_id)
			if callee.value == 'slice' && unalias_type(receiver_type) is Array {
				tc.record_error_at(.unknown_fn,
					'.slice() is a private method, use `x[start..end]` instead', id, token.new_span(name_pos.id,
					name_pos.offset, node.pos.end))
				tc.register_synth_type(id, Type(void_))
				tc.record_enclosing_print_void(id)
				return true
			}
			receiver_name := receiver_type.name()
			name := '${receiver_name}.${callee.value}'
			tc.record_error_at(.unknown_fn, 'method `${name}` is private', id, token.new_span(name_pos.id,
				name_pos.offset, node.pos.end))
			return true
		}
		tc.record_error_at(.unknown_fn, 'function `${info.name}` is private', id, node.pos)
		return true
	}
	return false
}

fn (tc &TypeChecker) non_function_index_call_kind(call flat.Node) ?string {
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	callee_id := tc.a.child(&call, 0)
	callee := tc.a.node(callee_id)
	if callee.kind != .index || callee.value == 'range' || callee.children_count == 0
		|| fn_type_from_type(unalias_type(tc.resolve_type(callee_id))) != none {
		return none
	}
	base_type := unalias_type(tc.resolve_type(tc.a.child(callee, 0)))
	if base_type is Map {
		return 'value'
	}
	if base_type is Array || base_type is ArrayFixed {
		return 'element'
	}
	return none
}

fn (tc &TypeChecker) non_function_index_container_name(index flat.Node) string {
	if index.children_count == 0 {
		return 'container'
	}
	base_type := unalias_type(tc.resolve_type(tc.a.child(&index, 0)))
	return if base_type is Map { 'map' } else { 'array' }
}

fn (tc &TypeChecker) invalid_non_function_index_call(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	return tc.non_function_index_call_kind(*tc.a.node(id)) != none
}

fn (tc &TypeChecker) call_closing_paren_pos(node flat.Node) token.Pos {
	if !node.pos.is_valid() || node.pos.end <= node.pos.offset {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	end := int_min(node.pos.end, source.len)
	if end > node.pos.offset && source[end - 1] == `)` {
		return token.new_span(node.pos.id, end - 1, end)
	}
	return node.pos
}

fn (tc &TypeChecker) call_generic_params(name string) []string {
	qualified := tc.qualify_fn_name(name)
	if params := tc.fn_generic_params[qualified] {
		return params
	}
	if qualified != name && tc.fn_signature_known(qualified) {
		return []string{}
	}
	return tc.fn_generic_params[name] or { []string{} }
}

fn (mut tc TypeChecker) record_chained_bare_generic_struct_method_inference_error(id flat.NodeId, node flat.Node, info CallInfo) {
	if !info.has_receiver || node.children_count == 0 {
		return
	}
	mut callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return
	}
	receiver := tc.a.child_node(callee, 0)
	if receiver.kind != .struct_init || generic_type_application(receiver.value) {
		return
	}
	base := trimmed_space(receiver.value)
	qualified := tc.qualify_name(base)
	struct_params := tc.struct_generic_params[base] or {
		tc.struct_generic_params[qualified] or { return }
	}
	if struct_params.len == 0 {
		return
	}
	inferred := tc.infer_generic_struct_init_param_texts(*receiver, base, struct_params)
	mut missing := ''
	for param in struct_params {
		arg := inferred[param] or {
			missing = param
			break
		}
		if tc.type_text_has_generic_placeholder(arg) {
			missing = param
			break
		}
	}
	if missing.len == 0 {
		return
	}
	name_pos := tc.method_call_name_pos(node, callee)
	tc.record_error_at(.unsupported_generic,
		'could not infer generic type `${missing}` in call to `${callee.value}`', id, token.new_span(name_pos.id,
		name_pos.offset, node.pos.end))
}

fn (mut tc TypeChecker) record_uninferred_generic_method_type(id flat.NodeId, node flat.Node, info CallInfo) {
	if !info.has_receiver || tc.call_has_explicit_generic_type_args(node)
		|| node.children_count == 0 {
		return
	}
	generic_params := tc.fn_generic_params[info.name] or { return }
	param_texts := tc.fn_param_type_texts[info.name] or { return }
	if generic_params.len == 0 {
		return
	}
	mut inferred := map[string]string{}
	mut first_param_idx := 0
	mut callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind == .selector && callee.children_count > 0 && param_texts.len > 0 {
		receiver_id := tc.a.child(callee, 0)
		tc.infer_generic_type_text_from_type(param_texts[0], tc.resolve_type(receiver_id),
			generic_params, mut inferred)
		first_param_idx = 1
	}
	for param_idx in first_param_idx .. param_texts.len {
		arg_idx := param_idx - first_param_idx + 1 + info.arg_offset
		if arg_idx >= node.children_count {
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, arg_idx))
		tc.infer_generic_type_text_from_type(param_texts[param_idx], tc.resolve_type(arg_id),
			generic_params, mut inferred)
	}
	mut missing := ''
	for param in generic_params {
		arg := inferred[param] or {
			missing = param
			break
		}
		if tc.type_text_has_generic_placeholder(arg) {
			missing = param
			break
		}
	}
	if missing.len == 0 || callee.kind != .selector {
		return
	}
	name_pos := tc.method_call_name_pos(node, callee)
	tc.record_error_at(.unsupported_generic,
		'could not infer generic type `${missing}` in call to `${callee.value}`', id, token.new_span(name_pos.id,
		name_pos.offset, node.pos.end))
}

fn (mut tc TypeChecker) record_empty_array_generic_call_errors(node flat.Node, info CallInfo) bool {
	mut generic_params := tc.fn_generic_params[info.name] or { []string{} }
	if generic_params.len == 0 {
		canonical := tc.canonical_symbol(info.name)
		generic_params = tc.fn_generic_params[canonical] or { []string{} }
	}
	if generic_params.len == 0 {
		decl_module := tc.fn_type_modules[info.name] or { tc.cur_module }
		if decl := tc.visible_mutation_fn_decl(info.name, decl_module) {
			generic_params = tc.infer_decl_generic_param_names(tc.a.node(flat.NodeId(decl.idx)))
		}
	}
	if generic_params.len == 0 {
		return false
	}
	mut found := false
	for i in 1 .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		arg := tc.a.node(arg_id)
		if arg.kind == .array_literal && arg.children_count == 0 && arg.typ.len == 0 {
			tc.record_error_at(.call_arg_mismatch, 'cannot use empty array as generic argument',
				arg_id, arg.pos)
			found = true
		}
	}
	return found
}

struct GenericCompileErrorInstantiation {
	decl_id        flat.NodeId
	generic_params []string
	concrete_args  []string
	symbol_types   map[string]string
}

struct InstantiatedCompileWarning {
	message string
	node    flat.NodeId
	pos     token.Pos
}

fn (mut tc TypeChecker) check_instantiated_generic_compile_errors(call_id flat.NodeId, call flat.Node, info CallInfo) {
	instantiation := tc.generic_compile_error_instantiation(call, info) or { return }
	mut messages := []string{}
	tc.collect_instantiated_compile_errors(instantiation.decl_id, instantiation, mut messages)
	for message in messages {
		if !tc.has_type_error(.compile_error, message, call_id) {
			tc.record_error_at(.compile_error, message, call_id, call.pos)
		}
	}
}

fn (mut tc TypeChecker) check_instantiated_generic_compile_warnings(call flat.Node, info CallInfo) {
	instantiation := tc.generic_compile_error_instantiation(call, info) or { return }
	mut warnings := []InstantiatedCompileWarning{}
	tc.collect_instantiated_compile_warnings(instantiation.decl_id, instantiation, mut warnings)
	for warning in warnings {
		if !tc.has_type_notice(.compile_error, warning.message, warning.node) {
			tc.record_warning_at(.compile_error, warning.message, warning.node, warning.pos)
		}
	}
}

fn (mut tc TypeChecker) generic_compile_error_instantiation(call flat.Node, info CallInfo) ?GenericCompileErrorInstantiation {
	if call.children_count == 0 {
		return none
	}
	decl_module := tc.fn_type_modules[info.name] or { tc.cur_module }
	decl := tc.visible_mutation_fn_decl(info.name, decl_module) or { return none }
	decl_id := flat.NodeId(decl.idx)
	fn_node := tc.a.node(decl_id)
	generic_params := tc.fn_generic_params[info.name] or {
		tc.infer_decl_generic_param_names(fn_node)
	}
	if generic_params.len == 0 {
		return none
	}
	mut concrete_args := []string{}
	callee := tc.a.child_node(&call, 0)
	if callee.kind == .index {
		type_args := tc.generic_call_type_arg_names(callee)
		if type_args.len != generic_params.len {
			return none
		}
		for type_arg in type_args {
			concrete_args << tc.explicit_generic_concrete_arg_text(type_arg)
		}
	}
	mut inferred := map[string]string{}
	mut symbol_types := map[string]string{}
	if concrete_args.len == generic_params.len {
		for i, param in generic_params {
			inferred[param] = concrete_args[i]
			symbol_types[param] = concrete_args[i]
		}
	}
	mut arg_index := 1 + info.arg_offset
	mut source_param_index := 0
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(fn_node, i)
		if param.kind != .param {
			continue
		}
		if info.has_receiver && source_param_index == 0 {
			if receiver_id := tc.generic_method_call_receiver_id(call) {
				actual := tc.resolve_type(receiver_id)
				tc.infer_generic_type_text_from_type(param.typ, actual, generic_params, mut
					inferred)
				if param.value.len > 0 {
					symbol_types[param.value] = actual.name()
				}
			}
			source_param_index++
			continue
		}
		if arg_index >= call.children_count {
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&call, arg_index))
		actual := tc.resolve_type(arg_id)
		tc.infer_generic_type_text_from_type(param.typ, actual, generic_params, mut inferred)
		if param.value.len > 0 {
			symbol_types[param.value] = actual.name()
		}
		arg_index++
		source_param_index++
	}
	if concrete_args.len == 0 {
		for param in generic_params {
			arg := inferred[param] or { return none }
			concrete_args << arg
			symbol_types[param] = arg
		}
	}
	return GenericCompileErrorInstantiation{
		decl_id:        decl_id
		generic_params: generic_params
		concrete_args:  concrete_args
		symbol_types:   symbol_types
	}
}

fn (tc &TypeChecker) generic_method_call_receiver_id(call flat.Node) ?flat.NodeId {
	if call.children_count == 0 {
		return none
	}
	mut callee := tc.a.child_node(&call, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	return tc.a.child(callee, 0)
}

fn (tc &TypeChecker) instantiated_compile_warning_message(id flat.NodeId, instantiation GenericCompileErrorInstantiation) string {
	node := tc.a.node(id)
	mut message := 'compile-time warning'
	source := tc.source_text_for_node(id)
	mut quote_start := source.index_u8(`'`)
	double_quote_start := source.index_u8(`"`)
	if quote_start < 0 || (double_quote_start >= 0 && double_quote_start < quote_start) {
		quote_start = double_quote_start
	}
	if quote_start >= 0 && quote_start + 1 < source.len {
		quote := source[quote_start]
		quote_end := source[quote_start + 1..].last_index_u8(quote)
		if quote_end >= 0 {
			message = source[quote_start + 1..quote_start + 1 + quote_end]
		}
	} else if node.children_count > 1 {
		arg := tc.a.child_node(node, 1)
		if arg.value.len > 0 {
			message = arg.value
		}
	}
	for i, param in instantiation.generic_params {
		if i >= instantiation.concrete_args.len {
			break
		}
		concrete := instantiation.concrete_args[i]
		message = message.replace(r'${' + param + '.name}', concrete)
		if idx := legacy_comptime_type_index(concrete) {
			message = message.replace(r'${' + param + '.idx}', idx.str())
		}
	}
	return message
}

fn legacy_comptime_type_index(name string) ?int {
	clean := name.trim_space().trim_left('&').trim_left('?').trim_left('!')
	return match clean {
		'void' { 1 }
		'voidptr' { 2 }
		'byteptr' { 3 }
		'charptr' { 4 }
		'i8' { 5 }
		'i16' { 6 }
		'i32' { 7 }
		'int' { 8 }
		'i64' { 9 }
		'isize' { 10 }
		'u8' { 11 }
		'u16' { 12 }
		'u32' { 13 }
		'u64' { 14 }
		'usize' { 15 }
		'f32' { 16 }
		'f64' { 17 }
		'char' { 18 }
		'bool' { 19 }
		'none' { 20 }
		'string' { 21 }
		'rune' { 22 }
		'array' { 23 }
		'map' { 24 }
		'chan' { 25 }
		'any' { 26 }
		'thread' { 29 }
		'error' { 30 }
		'nil' { 31 }
		else { none }
	}
}

fn (mut tc TypeChecker) collect_instantiated_compile_warnings(id flat.NodeId, instantiation GenericCompileErrorInstantiation, mut warnings []InstantiatedCompileWarning) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_if {
		substituted := substitute_comptime_condition_symbols(node.value, instantiation.symbol_types)
		take_then := tc.comptime_type_condition_value(substituted) or { return }
		branch_index := if take_then { 0 } else { 1 }
		if branch_index < node.children_count {
			tc.collect_instantiated_compile_warnings(tc.a.child(node, branch_index), instantiation, mut
				warnings)
		}
		return
	}
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .ident && callee.value == '__v_compile_warn' {
			message := tc.instantiated_compile_warning_message(id, instantiation)
			if !warnings.any(it.node == id && it.message == message) {
				warnings << InstantiatedCompileWarning{
					message: message
					node:    id
					pos:     node.pos
				}
			}
			return
		}
	}
	if node.kind in [.fn_literal, .lambda_expr] && id != instantiation.decl_id {
		return
	}
	for i in 0 .. node.children_count {
		tc.collect_instantiated_compile_warnings(tc.a.child(node, i), instantiation, mut warnings)
	}
}

fn (mut tc TypeChecker) collect_instantiated_compile_errors(id flat.NodeId, instantiation GenericCompileErrorInstantiation, mut messages []string) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .comptime_if {
		substituted := substitute_comptime_condition_symbols(node.value, instantiation.symbol_types)
		take_then := tc.comptime_type_condition_value(substituted) or { return }
		branch_index := if take_then { 0 } else { 1 }
		if branch_index < node.children_count {
			tc.collect_instantiated_compile_errors(tc.a.child(node, branch_index), instantiation, mut
				messages)
		}
		return
	}
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .ident && callee.value == '__v_compile_error' {
			message := if node.children_count > 1 {
				arg := tc.a.child_node(node, 1)
				if arg.value.len > 0 {
					arg.value
				} else {
					'compile-time error'
				}
			} else {
				'compile-time error'
			}
			if message !in messages {
				messages << message
			}
			return
		}
	}
	if node.kind in [.fn_literal, .lambda_expr] && id != instantiation.decl_id {
		return
	}
	for i in 0 .. node.children_count {
		tc.collect_instantiated_compile_errors(tc.a.child(node, i), instantiation, mut messages)
	}
}

fn substitute_comptime_condition_symbols(condition string, symbols map[string]string) string {
	if symbols.len == 0 {
		return condition
	}
	mut out := strings.new_builder(condition.len + 16)
	mut i := 0
	for i < condition.len {
		if condition[i].is_alnum() || condition[i] == `_` {
			mut end := i + 1
			for end < condition.len && (condition[end].is_alnum() || condition[end] == `_`) {
				end++
			}
			token_text := condition[i..end]
			out.write_string(symbols[token_text] or { token_text })
			i = end
			continue
		}
		out.write_u8(condition[i])
		i++
	}
	return out.str()
}

fn (tc &TypeChecker) instantiated_generic_decl_expr_type(id flat.NodeId, decl flat.Node, info CallInfo, instantiation GenericCompileErrorInstantiation) Type {
	node := tc.a.node(id)
	if node.kind == .ident {
		for i in 0 .. decl.children_count {
			param := tc.a.child_node(&decl, i)
			if param.kind == .param && param.value == node.value {
				type_text := subst_generic_text(param.typ, instantiation.concrete_args,
					instantiation.generic_params)
				return tc.parse_fn_signature_type(info.name, type_text)
			}
		}
	}
	return tc.substitute_generic_type(tc.resolve_type(id), instantiation.concrete_args,
		instantiation.generic_params)
}

fn (mut tc TypeChecker) check_instantiated_generic_as_casts(call flat.Node, info CallInfo) {
	instantiation := tc.generic_compile_error_instantiation(call, info) or { return }
	fn_node := tc.a.node(instantiation.decl_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		stack << tc.a.child(fn_node, i)
	}
	for stack.len > 0 {
		node_id := stack.pop()
		node := tc.a.node(node_id)
		if node.kind == .as_expr && node.children_count > 0 {
			target_text := subst_generic_text(node.value, instantiation.concrete_args,
				instantiation.generic_params)
			if target_text != node.value {
				child_id := tc.a.child(node, 0)
				source := unalias_type(tc.instantiated_generic_decl_expr_type(child_id, fn_node,
					info, instantiation))
				target := unalias_type(tc.parse_type(target_text))
				if source is SumType && target !is Unknown && target.name() != source.name
					&& tc.sum_variant_type_for_pattern(source.name, target.name()) == none {
					message := 'cannot cast `${source.name}` to `${target.name()}`'
					if !tc.has_type_error(.assignment_mismatch, message, node_id) {
						tc.record_error_at(.assignment_mismatch, message, node_id, tc.as_operator_pos(node_id,
							node, child_id))
					}
				}
			}
		}
		if node.kind in [.fn_literal, .lambda_expr] {
			continue
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
}

fn (mut tc TypeChecker) check_instantiated_generic_ordering_ops(call flat.Node, info CallInfo) {
	instantiation := tc.generic_compile_error_instantiation(call, info) or { return }
	mut instantiations := [instantiation]
	// The exact-output fixture path expands every sibling method for compatibility
	// with v1. Normal compilation specializes regular methods on demand.
	if tc.checker_fixture_mode {
		if receiver_id := tc.generic_method_call_receiver_id(call) {
			receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
			receiver_base := strip_generic_args_name(receiver_type.name())
			for idx in tc.top_level_idx {
				candidate_id := flat.NodeId(idx)
				if candidate_id == instantiation.decl_id {
					continue
				}
				candidate := tc.a.node(candidate_id)
				if candidate.kind != .fn_decl {
					continue
				}
				mut receiver_param_id := flat.NodeId(-1)
				for i in 0 .. candidate.children_count {
					param_id := tc.a.child(candidate, i)
					param := tc.a.node(param_id)
					if param.kind == .param {
						receiver_param_id = param_id
						break
					}
				}
				if !tc.valid_node_id(receiver_param_id) {
					continue
				}
				receiver_param := tc.a.node(receiver_param_id)
				param_text := trimmed_space(receiver_param.typ).trim_left('&').trim_left('mut ')
				param_base, _, is_generic := generic_type_application_parts(param_text)
				if !is_generic {
					continue
				}
				source_file := tc.a.source_files[receiver_param.pos.id] or { continue }
				module_name := tc.file_modules[source_file.name] or { '' }
				qualified_base := if param_base.contains('.') || module_name.len == 0
					|| module_name == 'main' {
					param_base
				} else {
					'${module_name}.${param_base}'
				}
				if qualified_base != receiver_base {
					continue
				}
				generic_params := tc.infer_decl_generic_param_names(candidate)
				if generic_params.len != instantiation.concrete_args.len {
					continue
				}
				instantiations << GenericCompileErrorInstantiation{
					decl_id:        candidate_id
					generic_params: generic_params
					concrete_args:  instantiation.concrete_args.clone()
				}
			}
		}
	}
	for concrete in instantiations {
		decl := tc.a.node(concrete.decl_id)
		mut stack := []flat.NodeId{}
		for i := int(decl.children_count) - 1; i >= 0; i-- {
			stack << tc.a.child(decl, i)
		}
		for stack.len > 0 {
			node_id := stack.pop()
			node := tc.a.node(node_id)
			if node.kind == .infix && node.children_count >= 2 && node.op in [.lt, .gt, .le, .ge] {
				lhs_id := tc.a.child(node, 0)
				rhs_id := tc.a.child(node, 1)
				lhs_type := unalias_and_unwrap_pointer_type(tc.instantiated_generic_decl_expr_type(lhs_id,
					decl, info, concrete))
				rhs_type := unalias_and_unwrap_pointer_type(tc.instantiated_generic_decl_expr_type(rhs_id,
					decl, info, concrete))
				if lhs_type is Struct && rhs_type is Struct
					&& !tc.type_has_infix_operator_method(lhs_type, .lt) {
					op := infix_operator_name(node.op) or { '' }
					message := if node.op == .gt {
						'cannot use `>` as `<=` operator method is not defined'
					} else {
						'cannot use `${op}` as `<` operator method is not defined'
					}
					lhs := tc.a.node(lhs_id)
					diagnostic_pos := if lhs.kind == .index && lhs.children_count > 1 {
						index := tc.a.child_node(lhs, 1)
						token.new_span(node.pos.id, int_max(node.pos.offset, index.pos.offset - 1),
							node.pos.end)
					} else {
						node.pos
					}
					if !tc.type_error_already_reported_on_line(.condition_mismatch, message,
						diagnostic_pos) {
						tc.record_error_at(.condition_mismatch, message, node_id, diagnostic_pos)
					}
				}
			}
			if node.kind in [.fn_literal, .lambda_expr] {
				continue
			}
			for i := int(node.children_count) - 1; i >= 0; i-- {
				stack << tc.a.child(node, i)
			}
		}
	}
}

fn (tc &TypeChecker) unknown_import_function_call_parts(node flat.Node) ?(flat.Node, string, string) {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base := tc.a.child_node(callee, 0)
	if base.kind != .ident || tc.ident_resolves_to_value(base.value) {
		return none
	}
	module_name := tc.resolve_import_alias(base.value) or { return none }
	full_name := '${module_name}.${callee.value}'
	if full_name in tc.fn_ret_types {
		return none
	}
	return *callee, base.value, module_name
}

fn (mut tc TypeChecker) record_unknown_import_function_call(id flat.NodeId, node flat.Node) bool {
	callee, alias, module_name := tc.unknown_import_function_call_parts(node) or { return false }
	display := '${alias}.${callee.value}'
	mut candidates := []string{}
	prefix := '${module_name}.'
	for name, _ in tc.fn_ret_types {
		if name.starts_with(prefix) && !name[prefix.len..].contains('.') {
			candidates << '${alias}.${name[prefix.len..]}'
		}
	}
	message := util.new_suggestion(display, candidates).say('unknown function: ${display} ')
	tc.record_error_at(.unknown_fn, message, id, tc.method_call_name_pos(node, callee))
	tc.register_synth_type(id, Type(MultiReturn{
		types: []Type{}
	}))
	return true
}

fn (tc &TypeChecker) local_decl_rhs_before(name string, use_id flat.NodeId) ?flat.NodeId {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if name.len == 0 || !tc.valid_node_id(fn_id) || !tc.valid_node_id(use_id) {
		return none
	}
	use_pos := tc.a.node(use_id).pos
	fn_node := tc.a.node(fn_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(fn_node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	mut seen := map[int]bool{}
	mut best_id := flat.empty_node
	mut best_offset := -1
	for stack.len > 0 {
		current_id := stack.pop()
		if seen[int(current_id)] || !tc.valid_node_id(current_id) {
			continue
		}
		seen[int(current_id)] = true
		current := tc.a.node(current_id)
		if current.kind == .decl_assign {
			for i := 0; i + 1 < int(current.children_count); i += 2 {
				lhs := tc.a.child_node(current, i)
				if lhs.kind == .ident && lhs.value == name && lhs.pos.id == use_pos.id
					&& lhs.pos.offset < use_pos.offset && lhs.pos.offset > best_offset {
					best_id = tc.a.child(current, i + 1)
					best_offset = lhs.pos.offset
				}
			}
		}
		if current.kind in [.fn_literal, .lambda_expr] {
			continue
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	if best_id != flat.empty_node {
		return best_id
	}
	return none
}

fn (tc &TypeChecker) dynamic_comptime_method_var_pos(node flat.Node, name string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(source.len, node.pos.end)
	if start >= end {
		return node.pos
	}
	needle := '$${name}'
	relative := source[start..end].index(needle) or { return node.pos }
	name_start := start + relative + 1
	return token.new_span(node.pos.id, name_start, name_start + name.len)
}

fn (mut tc TypeChecker) check_dynamic_comptime_method_call(id flat.NodeId, node flat.Node, callee &flat.Node) {
	name := callee.value[1..]
	pos := tc.dynamic_comptime_method_var_pos(node, name)
	rhs_id := tc.local_decl_rhs_before(name, id) or {
		tc.record_error_at(.unknown_fn, 'unknown identifier `${name}`', id, pos)
		return
	}
	var_type := unalias_type(tc.resolve_type(rhs_id))
	if var_type !is String {
		tc.record_error_at(.call_arg_mismatch,
			'invalid string method call: expected `string`, not `${var_type.name()}`', id, pos)
		return
	}
	rhs := tc.a.node(rhs_id)
	if rhs.kind != .string_literal {
		tc.record_error_at(.unknown_fn, 'todo: not a string literal', id, pos)
		tc.record_error_at(.unknown_fn, 'could not find method ``', id, pos)
		return
	}
	method_name := rhs.value
	if callee.children_count == 0 {
		return
	}
	receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(callee, 0)))
	method_key := tc.concrete_method_signature_key(receiver_type.name(), method_name) or {
		tc.record_error_at(.unknown_fn, 'could not find method `${method_name}`', id, pos)
		return
	}
	tc.remember_resolved_call(id, method_key)
	if return_type := tc.fn_ret_types[method_key] {
		tc.remember_expr_type(id, return_type)
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
}

fn (tc &TypeChecker) invalid_unknown_import_function_print_arg(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	return tc.unknown_import_function_call_parts(tc.a.node(id)) != none
}

fn (tc &TypeChecker) unknown_method_call_parts(node flat.Node) ?(flat.Node, Type, string) {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base := tc.a.child_node(callee, 0)
	if tc.is_namespace_selector(*callee, base) {
		return none
	}
	receiver_type := unwrap_pointer(tc.resolve_type(tc.a.child(callee, 0)))
	if receiver_type !is Struct && receiver_type !is Interface && receiver_type !is Alias
		&& receiver_type !is String && receiver_type !is Primitive {
		return none
	}
	receiver_name := receiver_type.name()
	if tc.struct_field_type(receiver_name, callee.value) != none
		|| tc.concrete_method_signature_key(receiver_name, callee.value) != none {
		return none
	}
	method_candidates := receiver_method_name_candidates(receiver_type, callee.value, tc.cur_module)
	for candidate in method_candidates {
		if candidate in tc.fn_ret_types {
			return none
		}
	}
	if _ := tc.unique_receiver_method_suffix_match(method_candidates) {
		return none
	}
	if receiver_type is Struct {
		if _ := tc.resolve_generic_struct_method(receiver_name, callee.value) {
			return none
		}
	}
	return *callee, receiver_type, receiver_name
}

fn (tc &TypeChecker) unknown_method_receiver_display(receiver_name string) string {
	base, _, is_generic := generic_type_application_parts(receiver_name)
	if !is_generic {
		return receiver_name.all_after_last('.')
	}
	params := tc.struct_generic_params[base] or {
		tc.struct_generic_params[base.all_after_last('.')] or { []string{} }
	}
	if params.len == 0 {
		return receiver_name.all_after_last('.')
	}
	diagnostic_params := if params.len == 1 { ['F'] } else { params }
	return '${base.all_after_last('.')}[${diagnostic_params.join(', ')}]'
}

fn (tc &TypeChecker) method_name_suggestions(receiver_name string) []string {
	base, _, is_generic := generic_type_application_parts(receiver_name)
	mut prefixes := []string{}
	for candidate in [receiver_name, receiver_name.all_after_last('.'), base, base.all_after_last('.')] {
		if candidate.len > 0 && candidate !in prefixes {
			prefixes << candidate
		}
	}
	mut methods := []string{}
	for key, _ in tc.fn_ret_types {
		for prefix in prefixes {
			full_prefix := '${prefix}.'
			if key.starts_with(full_prefix) {
				method := key[full_prefix.len..]
				if !method.contains('.') && method !in methods {
					methods << method
				}
			}
		}
	}
	if is_generic {
		for key, _ in tc.fn_param_types {
			for prefix in prefixes {
				full_prefix := '${prefix}.'
				if key.starts_with(full_prefix) {
					method := key[full_prefix.len..]
					if !method.contains('.') && method !in methods {
						methods << method
					}
				}
			}
		}
	}
	return methods
}

fn (mut tc TypeChecker) record_unknown_method_call(id flat.NodeId, node flat.Node) bool {
	callee, _, receiver_name := tc.unknown_method_call_parts(node) or { return false }
	display := if tc.generic_receiver_has_structured_method_pattern(receiver_name, callee.value) {
		strip_generic_args_name(receiver_name).all_after_last('.')
	} else {
		tc.unknown_method_receiver_display(receiver_name)
	}
	receiver_id := tc.a.child(callee, 0)
	if tc.ident_is_multi_pattern_match_subject(receiver_id) {
		tc.record_error_at(.unknown_fn, 'unknown method: `${display}.${callee.value}`', id, tc.method_call_name_pos(node,
			callee))
		tc.register_synth_type(id, Type(void_))
		return true
	}
	message := if tc.generic_receiver_method_rejects_voidptr(receiver_name, callee.value) {
		'method `${receiver_name}.${callee.value}` cannot bind `voidptr` to a generic receiver pattern; cast the receiver to a concrete V type first'
	} else {
		base := 'unknown method or field: `${display}.${callee.value}`'
		util.new_suggestion(callee.value, tc.method_name_suggestions(receiver_name)).say(base)
	}
	tc.record_error_at(.unknown_fn, message, id, tc.method_call_name_pos(node, callee))
	tc.register_synth_type(id, Type(MultiReturn{
		types: []Type{}
	}))
	return true
}

fn (tc &TypeChecker) invalid_unknown_method_print_arg(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	if _ := tc.resolved_call_name(id) {
		return false
	}
	node := tc.a.node(id)
	return tc.unknown_method_call_parts(node) != none
}

fn (tc &TypeChecker) invalid_pointer_infix_print_arg(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .infix || node.op != .plus || node.children_count < 2 {
		return false
	}
	lhs := tc.resolve_type(tc.a.child(node, 0))
	rhs := tc.resolve_type(tc.a.child(node, 1))
	lhs_pointer := unalias_type(lhs) is Pointer && lhs.name() != 'voidptr'
	rhs_pointer := unalias_type(rhs) is Pointer && rhs.name() != 'voidptr'
	return (lhs_pointer && !rhs_pointer && !rhs.is_integer())
		|| (rhs_pointer && !lhs_pointer && !lhs.is_integer())
}

fn (tc &TypeChecker) explicit_generic_local_fn_value(node flat.Node) ?FnType {
	if node.children_count == 0 {
		return none
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .index || callee.children_count < 2 {
		return none
	}
	base := tc.a.child_node(callee, 0)
	if base.kind != .ident {
		return none
	}
	typ := tc.cur_scope.lookup(base.value) or { return none }
	return fn_type_from_type(typ)
}

fn (tc &TypeChecker) call_has_wrapped_receiver(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(call, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_type := unalias_type(tc.resolve_type(tc.a.child(callee, 0)))
	return receiver_type is OptionType || receiver_type is ResultType
}

fn (tc &TypeChecker) wrapped_receiver_payload_diagnostic_name(payload Type) string {
	name := unwrap_all_pointers(payload).name()
	if name.contains('.') {
		alias := name.all_before('.')
		if import_path := tc.current_file_import_path_for_alias(alias) {
			return import_path + name[alias.len..]
		}
	}
	if qualified := tc.unique_qualified_type_name(name.all_after_last('.')) {
		return qualified
	}
	if resolved := tc.resolve_import_alias_pattern(name) {
		return resolved
	}
	return name
}

fn (tc &TypeChecker) current_file_import_path_for_alias(alias string) ?string {
	return tc.file_import_alias_paths['${tc.cur_file}\x00${alias}'] or { return none }
}

fn (tc &TypeChecker) wrapped_receiver_has_method(payload Type, method string) bool {
	clean := unwrap_all_pointers(payload)
	if clean !is Struct {
		return false
	}
	for candidate in receiver_method_name_candidates(clean, method, tc.cur_module) {
		if candidate in tc.fn_ret_types {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_call_deprecation(id flat.NodeId, node flat.Node, info CallInfo) {
	deprecation := tc.deprecated_symbols[info.name] or {
		tc.deprecated_symbols[info.name.all_after_last('.')] or { return }
	}
	is_method := deprecation.name.contains('.')
	kind := if is_method { 'method' } else { 'function' }
	pos := tc.deprecated_call_diagnostic_pos(node, is_method)
	tc.record_deprecation(id, kind, deprecation, pos)
}

fn (mut tc TypeChecker) record_deprecation(id flat.NodeId, kind string, deprecation DeprecationInfo, pos token.Pos) {
	custom := if deprecation.message.len > 0 { '; ${deprecation.message}' } else { '' }
	if deprecation.after.len == 0 {
		tc.record_warning_at(.unknown_fn,
			'${kind} `${deprecation.name}` has been deprecated${custom}', id, pos)
		return
	}
	today := time.now().strftime('%Y-%m-%d')
	if deprecation.after <= today {
		tc.record_error_at(.unknown_fn,
			'${kind} `${deprecation.name}` has been deprecated since ${deprecation.after}${custom}',
			id, pos)
		return
	}
	error_after := time.parse_iso8601(deprecation.after) or { return }
	tc.record_notice_at(.unknown_fn,
		'${kind} `${deprecation.name}` will be deprecated after ${deprecation.after}, and will become an error after ${error_after.add_days(180).strftime('%Y-%m-%d')}${custom}',
		id, pos)
}

fn (tc &TypeChecker) deprecated_call_diagnostic_pos(node flat.Node, is_method bool) token.Pos {
	if !is_method || node.children_count == 0 {
		return node.pos
	}
	selector := tc.a.child_node(&node, 0)
	if selector.kind != .selector {
		return node.pos
	}
	return tc.method_call_name_pos(node, selector)
}

fn (tc &TypeChecker) builtin_addr_call_arg(node flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count != 2 {
		return none
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value == '__addr' {
		return tc.call_arg_value(tc.a.child(&node, 1))
	}
	return none
}

fn (tc &TypeChecker) builtin_isreftype_call_arg(node flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count != 2 {
		return none
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value == '__v3_isreftype' {
		return tc.call_arg_value(tc.a.child(&node, 1))
	}
	return none
}

fn (mut tc TypeChecker) check_isreftype_arg(arg_id flat.NodeId) {
	if !tc.valid_node_id(arg_id) {
		return
	}
	arg := tc.a.nodes[int(arg_id)]
	if arg.kind == .sizeof_expr {
		tc.check_isreftype_type_arg(arg.value, arg_id)
		return
	}
	if arg.kind == .ident && (is_builtin_type_name(arg.value) || (arg.value.len > 0
		&& arg.value[0] >= `A` && arg.value[0] <= `Z`)) {
		tc.check_isreftype_type_arg(arg.value, arg_id)
		return
	}
	tc.check_node(arg_id)
}

fn (mut tc TypeChecker) check_isreftype_type_arg(typ string, arg_id flat.NodeId) {
	tc.check_type_string_for_unsupported_generics(typ, arg_id,
		generic_param_map_from_names(tc.fn_context.generic_params))
}

// cur_fn_is_generic_template reports whether the function currently being
// checked is a template that will be re-validated per instantiation: it
// declares generic parameters, or is a method on a generic receiver
// (`fn (mut so SetOf[T]) sort()`), whose own node has no generic_params.
fn (tc &TypeChecker) cur_fn_is_generic_template() bool {
	if tc.fn_context.node_id < 0 || tc.fn_context.node_id >= tc.a.nodes.len {
		return false
	}
	fn_node := tc.a.nodes[tc.fn_context.node_id]
	if fn_node.generic_params().len > 0 {
		return true
	}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child_node(&fn_node, i)
		if child.kind == .param && child.typ.len > 0
			&& tc.type_text_has_generic_placeholder(child.typ) {
			return true
		}
	}
	return false
}

// call_receiver_type_is_unknown reports whether a method call's receiver has
// an unresolvable type, so the unknown-function diagnostic must not fire for
// it. Besides generic templates (whose bodies the reference compiler also
// only validates per instantiation), the checker currently fails to resolve
// several legitimate receiver shapes — fields of generic struct instances
// (`json.decode[S[T]](s)!.val.unix()`), option/or-unwrapped results, and
// specialized generic call returns — so narrowing this to "provably generic"
// chains produces false errors on valid vlib code. The receiver expression
// itself IS checked, so `missing.method()` still reports its unknown
// identifier. Remaining gap: a template instantiated with a type lacking the
// method fails only in C compilation — v3 has no instantiation-time recheck
// of cloned template bodies yet.
fn (mut tc TypeChecker) call_receiver_type_is_unknown(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	base_id := tc.a.child(callee, 0)
	base_type := tc.resolve_type(base_id)
	if tc.cur_fn_is_generic_template() && tc.call_receiver_is_open_generic(base_id, base_type) {
		tc.check_node(base_id)
		return true
	}
	if unwrap_pointer(base_type) !is Unknown {
		return false
	}
	tc.check_node(base_id)
	return true
}

fn (tc &TypeChecker) call_receiver_is_open_generic(base_id flat.NodeId, base_type Type) bool {
	if tc.type_contains_open_generic_placeholder(base_type) {
		return true
	}
	clean := unwrap_pointer(base_type)
	if clean is Struct {
		if tc.type_text_has_generic_placeholder(clean.name) {
			return true
		}
		for candidate in tc.generic_struct_method_base_candidates(clean.name) {
			if candidate in tc.struct_generic_params {
				return true
			}
		}
	}
	if int(base_id) >= 0 && int(base_id) < tc.a.nodes.len {
		node := tc.a.nodes[int(base_id)]
		if node.typ.len > 0 && tc.type_text_has_generic_placeholder(node.typ) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) type_contains_open_generic_placeholder(typ Type) bool {
	if tc.type_text_has_generic_placeholder(typ.name()) {
		return true
	}
	if typ is Pointer {
		return tc.type_contains_open_generic_placeholder(typ.base_type)
	}
	if typ is Alias {
		return tc.type_contains_open_generic_placeholder(typ.base_type)
	}
	if typ is Array {
		return tc.type_contains_open_generic_placeholder(typ.elem_type)
	}
	if typ is ArrayFixed {
		return tc.type_contains_open_generic_placeholder(typ.elem_type)
	}
	if typ is Map {
		return tc.type_contains_open_generic_placeholder(typ.key_type)
			|| tc.type_contains_open_generic_placeholder(typ.value_type)
	}
	if typ is OptionType {
		return tc.type_contains_open_generic_placeholder(typ.base_type)
	}
	if typ is ResultType {
		return tc.type_contains_open_generic_placeholder(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) explicit_generic_args_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	if node.pos.offset < 0 || node.pos.offset >= source.len {
		return node.pos
	}
	line_end := source.index_after('\n', node.pos.offset) or { source.len }
	open_relative := source[node.pos.offset..line_end].index_u8(`[`)
	if open_relative < 0 {
		return node.pos
	}
	open := node.pos.offset + open_relative
	close_relative := source[open..line_end].index_u8(`]`)
	if close_relative < 0 {
		return node.pos
	}
	return token.new_span(node.pos.id, open, open + close_relative + 1)
}

// call_generic_args_have_placeholders reports whether the call carries explicit
// generic type args that are still uninstantiated placeholders (`p.read_element[T]()`
// inside a generic template). Such calls can only be validated after
// monomorphization, so unknown-function diagnostics must not fire for them.
// Outside a generic template a bare `missing[T]()` is real invalid code, so
// the deferral only applies within one. This deliberately includes callees
// with no known declaration: the reference compiler also accepts a template
// whose body calls a missing function as long as it is never instantiated
// (vlib code relies on this, e.g. asn1's commented-out `read_element`), and
// only reports it at instantiation time.
fn (tc &TypeChecker) call_generic_args_have_placeholders(node flat.Node) bool {
	if !tc.cur_fn_is_generic_template() {
		return false
	}
	// An explicit generic METHOD call whose type args still carry the
	// template's own placeholders (`p.read_element[T]()`) cannot be resolved
	// before instantiation - defer even when the target is not a known
	// declaration, matching v1, where an uninstantiated generic body is never
	// checked. The monomorph validator reports it if the template ever gets
	// specialized. A plain `missing[T]()` call still errors right away.
	if node.children_count > 0 {
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .index && callee.children_count >= 2 && callee.value != 'range'
			&& tc.a.child_node(callee, 0).kind == .selector {
			for i in 1 .. int(callee.children_count) {
				arg := tc.a.child_node(callee, i)
				if arg.kind == .ident && arg.value.len > 0
					&& tc.type_text_has_generic_placeholder(arg.value) {
					return true
				}
			}
		}
	}
	if !tc.explicit_generic_call_target_is_known(node) {
		return false
	}
	if node.value.len > 0 {
		for arg in node.value.split(',') {
			if tc.type_text_has_generic_placeholder(trimmed_space(arg)) {
				return true
			}
		}
	}
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .index || callee.children_count < 2 || callee.value == 'range' {
		return false
	}
	for i in 1 .. int(callee.children_count) {
		arg := tc.a.child_node(callee, i)
		if arg.kind == .ident && arg.value.len > 0
			&& tc.type_text_has_generic_placeholder(arg.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) explicit_generic_call_target_is_known(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	if _ := tc.sum_constructor_call_name(node) {
		return true
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .index || callee.children_count == 0 || callee.value == 'range' {
		return tc.is_known_call(node)
	}
	base := tc.a.child_node(callee, 0)
	if _ := tc.generic_call_base_name(base) {
		return true
	}
	if base.kind != .selector || base.value.len == 0 {
		return false
	}
	if base.children_count > 0 {
		receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(base, 0)))
		if receiver_type is Struct {
			if _ := tc.resolve_generic_struct_method(receiver_type.name, base.value) {
				return true
			}
		}
	}
	// A method on an unresolved generic receiver cannot be tied to a concrete
	// receiver yet, but it still has to name a real method declaration. The
	// concrete specialization is validated after monomorphization.
	for name, _ in tc.fn_generic_params {
		if name.ends_with('.${base.value}') {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_sum_constructor_call(id flat.NodeId, node flat.Node, sum_name string) {
	expected := Type(SumType{
		name: sum_name
	})
	tc.remember_expr_type(id, expected)
	actual_count := node.children_count - 1
	if actual_count != 1 {
		if tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected 1, got ${actual_count}',
				id)
		}
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		return
	}
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	tc.check_node(arg_id)
	arg_type := tc.resolve_expr(arg_id, expected)
	if !tc.type_compatible(arg_type, expected)
		&& !tc.single_value_wrapper_compatible_with_sum(arg_type, expected) {
		tc.type_mismatch(.call_arg_mismatch,
			'cannot use `${arg_type.name()}` as sum constructor payload; expected variant of `${sum_name}`',
			id)
	}
}

fn (tc &TypeChecker) single_value_wrapper_compatible_with_sum(actual Type, expected Type) bool {
	if expected !is SumType {
		return false
	}
	actual_name := actual.name()
	if actual_name.len == 0 {
		return false
	}
	fields := tc.struct_fields_for_init(actual_name)
	if fields.len != 1 || fields[0].name != 'value' {
		return false
	}
	return tc.type_compatible(fields[0].typ, expected)
}

fn (tc &TypeChecker) sum_constructor_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	callee_id := tc.a.child(&node, 0)
	callee := tc.a.nodes[int(callee_id)]
	match callee.kind {
		.ident {
			if resolved := tc.resolve_selective_import_type_symbol(callee.value) {
				if sum_name := tc.known_sum_constructor_name(resolved) {
					return sum_name
				}
			}
			return tc.known_sum_constructor_name(callee.value)
		}
		.selector {
			base := tc.a.child_node(callee, 0)
			if base.kind == .ident {
				mod_name := tc.resolve_import_alias(base.value) or { base.value }
				return tc.known_sum_constructor_name('${mod_name}.${callee.value}')
			}
		}
		.index, .prefix, .array_init {
			type_name := tc.type_expr_name(callee_id)
			if type_name.len > 0 {
				return tc.known_sum_constructor_name(type_name)
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) type_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			if resolved := tc.resolve_selective_import_type_symbol(node.value) {
				return resolved
			}
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base_id := tc.a.child(&node, 0)
			base_node := tc.a.nodes[int(base_id)]
			base := if base_node.kind == .ident {
				tc.resolve_import_alias(base_node.value) or { tc.type_expr_name(base_id) }
			} else {
				tc.type_expr_name(base_id)
			}
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := tc.type_expr_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := tc.type_expr_name(tc.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len == 0 {
				return ''
			}
			return '[]${node.value}'
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := tc.type_expr_name(tc.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

fn (tc &TypeChecker) known_sum_constructor_name(name string) ?string {
	if name in tc.sum_types {
		return name
	}
	base_name := strip_generic_args_name(name)
	if base_name in tc.sum_types {
		return name
	}
	qname := tc.qualify_name(name)
	if qname in tc.sum_types {
		return qname
	}
	qbase := strip_generic_args_name(qname)
	if qbase in tc.sum_types {
		return qname
	}
	return none
}

// should_diagnose reports whether should diagnose applies in types.
fn (tc &TypeChecker) should_diagnose(id flat.NodeId) bool {
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if int(id) < tc.a.nodes.len && !tc.a.nodes[int(id)].pos.is_valid() && !tc.diagnose_unknown_calls {
		return false
	}
	if int(id) < tc.a.nodes.len && tc.a.nodes[int(id)].kind != .import_decl
		&& tc.node_is_on_multiple_module_import_line(id) {
		return false
	}
	if tc.fn_context.node_id >= 0 && tc.fn_context.node_id < tc.a.specialized_fn_nodes.len
		&& tc.a.specialized_fn_nodes[tc.fn_context.node_id] {
		if tc.checker_fixture_mode && tc.diagnostic_files.len > 0 {
			return tc.cur_file in tc.diagnostic_files
		}
		return true
	}
	if tc.current_fn_is_concrete_generic_receiver_specialization() {
		if tc.checker_fixture_mode && tc.diagnostic_files.len > 0 {
			return tc.cur_file in tc.diagnostic_files
		}
		return true
	}
	if tc.diagnostic_files.len == 0 {
		return true
	}
	return tc.cur_file in tc.diagnostic_files
}

fn (tc &TypeChecker) current_fn_is_concrete_generic_receiver_specialization() bool {
	return tc.fn_context.concrete_generic_receiver_specialization
		&& tc.fn_context.generic_params.len == 0
}

// fn_value_is_concrete_generic_receiver_specialization derives the cached
// FunctionCheckContext flag from a declaration name without allocating: the
// receiver part (before the last dot) must be a concrete generic application
// (`Box[int].clone`).
fn fn_value_is_concrete_generic_receiver_specialization(value string) bool {
	dot := value.last_index_u8(`.`)
	if dot <= 1 || value[dot - 1] != `]` {
		return false
	}
	open := value.index_u8(`[`)
	return open >= 0 && open < dot
}

fn multiple_module_import_line_key(file_id int, line int) u64 {
	return (u64(u32(file_id)) << 32) | u64(u32(line))
}

fn source_line_has_multiple_module_imports(raw_line string) bool {
	line := raw_line.trim_left(' \t')
	if !line.starts_with('import ') {
		return false
	}
	mut cursor := 'import '.len
	for cursor < line.len && (is_import_ident_byte(line[cursor]) || line[cursor] == `.`) {
		cursor++
	}
	for cursor < line.len && line[cursor] in [` `, `\t`, `\r`] {
		cursor++
	}
	if cursor >= line.len || line[cursor] == `{` || line[cursor..].starts_with('as ') {
		return false
	}
	return line[cursor] == `,` || is_import_ident_byte(line[cursor])
}

fn (mut tc TypeChecker) index_multiple_module_import_lines(a &flat.FlatAst) {
	tc.multiple_module_import_lines = map[u64]bool{}
	tc.source_texts_by_file = map[string]string{}
	for file_id, file in a.source_files {
		source := os.read_file(file.name) or { continue }
		tc.source_texts_by_file[file.name] = source
		mut line_number := 1
		mut line_start := 0
		for line_start <= source.len {
			line_end := source.index_after('\n', line_start) or { source.len }
			if source_line_has_multiple_module_imports(source[line_start..line_end]) {
				tc.multiple_module_import_lines[multiple_module_import_line_key(file_id,
					line_number)] = true
			}
			if line_end >= source.len {
				break
			}
			line_start = line_end + 1
			line_number++
		}
	}
}

fn (tc &TypeChecker) node_is_on_multiple_module_import_line(id flat.NodeId) bool {
	pos := tc.a.node(id).pos
	file := tc.a.source_files[pos.id] or { return false }
	if pos.offset < 0 || pos.offset > file.size {
		return false
	}
	line := file.position(pos).line
	return multiple_module_import_line_key(pos.id, line) in tc.multiple_module_import_lines
}

fn (tc &TypeChecker) should_diagnose_unsupported_generic(id flat.NodeId) bool {
	if tc.should_diagnose(id) {
		return true
	}
	if int(id) < 0 || int(id) < tc.a.user_code_start {
		return false
	}
	if tc.diagnostic_files.len == 0 {
		return false
	}
	return tc.diagnostic_files['generic:' + tc.cur_file]
}

// should_diagnose_unknown_call reports whether should diagnose unknown call applies in types.
fn (tc &TypeChecker) should_diagnose_unknown_call(id flat.NodeId) bool {
	return tc.diagnose_unknown_calls && tc.should_diagnose(id)
}

fn (tc &TypeChecker) ident_resolves_to_value(name string) bool {
	if _ := tc.cur_scope.lookup(name) {
		return true
	}
	return false
}

fn (tc &TypeChecker) sum_fn_variant_for_arg_count(typ Type, arg_count int) ?FnType {
	clean := unalias_and_unwrap_pointer_type(typ)
	if clean !is SumType {
		return none
	}
	clean_name := Type(clean).name()
	base := tc.sum_base_name(clean_name)
	variants := tc.sum_types[base] or { return none }
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(clean_name, variant)
		variant_type := tc.parse_type(concrete)
		if fn_type := fn_type_from_type(variant_type) {
			if fn_type.params.len == arg_count {
				return fn_type
			}
		}
	}
	return none
}

// resolve_call_info resolves resolve call info information for types.
fn (mut tc TypeChecker) resolve_call_info(id flat.NodeId, node flat.Node) ?CallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if info := tc.resolve_generic_call_info(id, fn_node) {
		return info
	}
	if fn_node.kind == .index && fn_node.children_count > 0 {
		callee_id := tc.a.child(&node, 0)
		fn_type := tc.resolve_type(callee_id)
		if fn_typ := fn_type_from_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				is_variadic:  tc.expr_is_variadic_fn_value(callee_id)
				params_known: true
			}
		}
		if unresolved_generic_receiver_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  Type(Unknown{
					reason: 'generic callable'
				})
				params_known: false
			}
		}
	}
	if fn_node.kind == .ident {
		fn_id := tc.a.child(&node, 0)
		mut binding_type := Type(void_)
		if typ := tc.cur_scope.lookup(fn_node.value) {
			binding_type = typ
		} else if typ := tc.file_scope.lookup(fn_node.value) {
			binding_type = typ
		} else if typ := tc.const_type_for_name(fn_node.value) {
			binding_type = typ
		}
		mut fn_type := Type(void_)
		mut is_smartcasted := false
		if smart_type := tc.smartcast_type(fn_id) {
			fn_type = smart_type
			is_smartcasted = true
		} else {
			fn_type = binding_type
		}
		if fn_typ := fn_type_from_type(fn_type) {
			actual_count := node.children_count - 1
			selected := if !is_smartcasted && fn_typ.params.len != actual_count {
				tc.sum_fn_variant_for_arg_count(binding_type, actual_count) or { fn_typ }
			} else {
				fn_typ
			}
			return CallInfo{
				name:         ''
				params:       selected.params.clone()
				return_type:  selected.return_type
				is_variadic:  tc.expr_is_variadic_fn_value(fn_id)
				params_known: true
			}
		}
		if fn_type is Unknown || unresolved_generic_receiver_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  Type(Unknown{
					reason: 'generic callable'
				})
				params_known: false
			}
		}
	}
	if fn_node.kind !in [.ident, .selector] {
		fn_type := tc.resolve_type(tc.a.child(&node, 0))
		if fn_typ := fn_type_from_type(fn_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
	}
	if fn_node.kind == .selector {
		base_id := tc.a.child(fn_node, 0)
		base_node := tc.a.nodes[int(base_id)]
		base_is_imported_module := base_node.kind == .ident
			&& !tc.ident_resolves_to_value(base_node.value)
			&& tc.resolve_import_alias(base_node.value) != none
		// A selector on an imported module is a direct module function before it is
		// considered as a function-valued field/constant. Keeping the qualified
		// CallInfo is also required by compiler-magic calls such as json.decode,
		// whose result type is inferred from the leading type argument.
		if base_is_imported_module {
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types {
					if info := tc.decode_call_info_from_type_arg(node, mod_name, false) {
						return info
					}
					return tc.call_info(mod_name, false)
				}
			}
		}
		if base_node.kind == .call {
			tc.check_node(base_id)
		}
		callee_id := tc.a.child(&node, 0)
		if fn_typ := tc.selector_fn_type(fn_node) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				is_variadic:  tc.expr_is_variadic_fn_value(callee_id)
				params_known: true
			}
		}
		mut use_resolved_fn_value_call := base_is_imported_module
		if _ := tc.selector_wrapped_fn_type(fn_node) {
			use_resolved_fn_value_call = true
		}
		if use_resolved_fn_value_call {
			mut callee_type := tc.resolve_type(callee_id)
			if smart_type := tc.smartcast_type(callee_id) {
				callee_type = smart_type
			}
			if fn_typ := fn_type_from_type(callee_type) {
				return CallInfo{
					name:         ''
					params:       fn_typ.params.clone()
					return_type:  fn_typ.return_type
					is_variadic:  tc.expr_is_variadic_fn_value(callee_id)
					params_known: true
				}
			}
		}
		if base_node.kind == .ident && base_node.value == 'C' {
			c_name := 'C.${fn_node.value}'
			if c_name in tc.fn_ret_types {
				return tc.call_info(c_name, false)
			}
			return none
		}
		if base_node.kind == .ident {
			base_is_value := tc.ident_resolves_to_value(base_node.value)
			if !base_is_value {
				if resolved_mod := tc.resolve_import_alias(base_node.value) {
					mod_name := '${resolved_mod}.${fn_node.value}'
					if mod_name in tc.fn_ret_types {
						if info := tc.decode_call_info_from_type_arg(node, mod_name, false) {
							return info
						}
						return tc.call_info(mod_name, false)
					}
				}
				if base_node.value == tc.cur_module {
					mod_name := '${tc.cur_module}.${fn_node.value}'
					if mod_name in tc.fn_ret_types {
						if info := tc.decode_call_info_from_type_arg(node, mod_name, false) {
							return info
						}
						return tc.call_info(mod_name, false)
					}
				}
				if static_name := tc.static_assoc_fn_key_for_base(base_node.value, fn_node.value) {
					return tc.call_info(static_name, false)
				}
				qbase := tc.qualify_name(base_node.value)
				if fn_node.value == 'from_string' {
					if enum_name := tc.resolve_enum_name(base_node.value) {
						return CallInfo{
							name:         ''
							params:       tarr1(Type(string_))
							return_type:  Type(OptionType{
								base_type: Type(Enum{
									name:    enum_name
									is_flag: enum_name in tc.flag_enums
								})
							})
							params_known: true
						}
					}
				}
				if fn_node.value == 'zero' && qbase in tc.flag_enums {
					return CallInfo{
						name:         ''
						params:       []Type{}
						return_type:  Type(Enum{
							name:    qbase
							is_flag: true
						})
						params_known: true
					}
				}
				if fn_node.value == 'from' && base_node.value in tc.fn_context.generic_params {
					return CallInfo{
						name:         ''
						params:       tarr1(Type(Unknown{
							reason: 'enum from input'
						}))
						return_type:  tc.parse_type('?${base_node.value}')
						params_known: true
					}
				}
				if fn_node.value == 'from' && qbase in tc.enum_names {
					return tc.enum_from_call_info(qbase)
				}
				if fn_node.value == 'from' {
					enum_name := tc.resolve_enum_name(base_node.value) or { '' }
					if enum_name.len > 0 {
						return tc.enum_from_call_info(enum_name)
					}
				}
			}
		} else if base_node.kind == .selector {
			if method_name := tc.module_const_receiver_method_name(base_node, fn_node.value) {
				// Raw collection declarations erase their element types. Let the
				// collection-specific path below specialize their receiver and result
				// from the imported constant's concrete type.
				if !checker_is_raw_collection_method_name(method_name, 'array.')
					&& !checker_is_raw_collection_method_name(method_name, 'map.') {
					return tc.call_info(method_name, true)
				}
			}
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
				if full_name in tc.fn_ret_types {
					return tc.call_info(full_name, false)
				}
				if static_name := tc.static_assoc_fn_key_for_base('${mod_name}.${base_node.value}',
					fn_node.value)
				{
					return tc.call_info(static_name, false)
				}
				if fn_node.value == 'from_string' {
					if enum_name := tc.resolve_enum_name('${mod_name}.${base_node.value}') {
						return CallInfo{
							name:         ''
							params:       tarr1(Type(string_))
							return_type:  Type(OptionType{
								base_type: Type(Enum{
									name:    enum_name
									is_flag: enum_name in tc.flag_enums
								})
							})
							params_known: true
						}
					}
				}
				if fn_node.value == 'from' {
					if enum_name := tc.resolve_enum_name('${mod_name}.${base_node.value}') {
						return tc.enum_from_call_info(enum_name)
					}
				}
			}
		}
		if fn_typ := tc.selector_const_fn_type(fn_node) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		base_type := tc.selector_fn_base_type(base_id) or { tc.resolve_type(base_id) }
		if base_node.kind == .ident && tc.binding_is_strings_builder(base_node.value) {
			for method_name in ['strings.Builder.${fn_node.value}', 'Builder.${fn_node.value}'] {
				if method_name in tc.fn_ret_types {
					return tc.call_info(method_name, true)
				}
			}
		}
		if fn_typ := tc.selector_field_fn_type(fn_node, base_type) {
			return CallInfo{
				name:         ''
				params:       fn_typ.params.clone()
				return_type:  fn_typ.return_type
				params_known: true
			}
		}
		clean := unwrap_all_pointers(base_type)
		if fn_node.value in ['type_name', 'type_idx'] && tc.receiver_is_sum_type(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  if fn_node.value == 'type_name' { Type(string_) } else { Type(int_) }
				has_receiver: true
				params_known: true
			}
		}
		if clean is Interface && fn_node.value in ['type_name', 'type_idx'] {
			return CallInfo{
				name:         '${clean.name}.${fn_node.value}'
				params:       tarr1(base_type)
				return_type:  if fn_node.value == 'type_name' { Type(string_) } else { Type(int_) }
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'clone' && unresolved_generic_receiver_type(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  base_type
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'wait' {
			if ret_type := tc.thread_wait_return_type(base_type) {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  ret_type
					has_receiver: true
					params_known: true
				}
			}
		}
		if info := tc.pointer_builtin_method_call_info(base_type, fn_node.value) {
			return info
		}
		if clean is Channel {
			match fn_node.value {
				'close' {
					return CallInfo{
						name:         'chan.close'
						params:       tarr2(base_type, tc.parse_type('IError'))
						return_type:  Type(void_)
						has_receiver: true
						is_variadic:  true
						params_known: true
					}
				}
				'try_push' {
					return CallInfo{
						name:         '${base_type.name()}.try_push'
						params:       tarr2(base_type, Type(Pointer{
							base_type: clean.elem_type
						}))
						return_type:  tc.parse_type('ChanState')
						has_receiver: true
						params_known: true
					}
				}
				'try_pop' {
					return CallInfo{
						name:         '${base_type.name()}.try_pop'
						params:       tarr2(base_type, clean.elem_type)
						return_type:  tc.parse_type('ChanState')
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if clean is String && fn_node.value == 'hex' && tc.is_builtin_hex_receiver(base_type) {
			return CallInfo{
				name:         'string.hex'
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if info := tc.builtin_receiver_method_call_info(base_type, fn_node.value) {
			return info
		}
		if info := tc.current_receiver_param_method_call_info(base_id, fn_node.value) {
			return info
		}
		if clean is Alias {
			// Methods declared on an alias take precedence over methods inherited
			// from its underlying collection. In particular, a fixed-array alias
			// can provide a clone() that preserves the alias type instead of using
			// the builtin fixed-array clone, which returns a dynamic array.
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if checker_is_raw_collection_method_name(mname, 'array.')
					|| checker_is_raw_collection_method_name(mname, 'map.')
					|| mname !in tc.fn_ret_types {
					continue
				}
				if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
					continue
				}
				return tc.call_info(mname, true)
			}
			alias_target_name := resolve_type_name_for_method(clean.base_type)
			if alias_target_name.len > 0 {
				if info := tc.resolve_generic_struct_method(alias_target_name, fn_node.value) {
					return info
				}
				for mname in receiver_method_name_candidates(clean.base_type, fn_node.value,
					tc.cur_module) {
					if checker_is_raw_collection_method_name(mname, 'array.')
						|| checker_is_raw_collection_method_name(mname, 'map.')
						|| mname !in tc.fn_ret_types {
						continue
					}
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					return tc.call_info(mname, true)
				}
			}
			if _ := array_type_from_receiver(clean) {
				for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
					if checker_is_raw_collection_method_name(mname, 'array.')
						|| mname !in tc.fn_ret_types {
						continue
					}
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					return tc.call_info(mname, true)
				}
			}
		}
		if clean_array := array_type_from_receiver(clean) {
			if fn_node.value == 'bytestr' && is_byte_type(clean_array.elem_type) {
				return CallInfo{
					name:         'array.bytestr'
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'to_fixed_size' && base_node.kind == .array_literal {
				return CallInfo{
					name:         'array.to_fixed_size'
					params:       tarr1(base_type)
					return_type:  Type(ArrayFixed{
						elem_type: clean_array.elem_type
						len:       int(base_node.children_count)
						len_expr:  '${base_node.children_count}'
					})
					has_receiver: true
					params_known: true
				}
			}
			array_candidates := exact_array_receiver_method_candidates(clean_array, fn_node.value,
				tc.cur_module)
			for mname in array_candidates {
				if mname in tc.fn_ret_types {
					return tc.call_info(mname, true)
				}
			}
			if mname := tc.unique_receiver_method_suffix_match(array_candidates) {
				return tc.call_info(mname, true)
			}
			if fn_node.value in ['clone', 'reverse'] {
				if bad_type := tc.ownership_default_clone_missing_method(clean_array.elem_type) {
					tc.record_error(.call_arg_mismatch,
						'cannot ${fn_node.value} array elements: `${bad_type}` requires ownership destruction but has no `clone()` method',
						id)
				}
				return CallInfo{
					name:         'array.${fn_node.value}'
					params:       tarr1(base_type)
					return_type:  clean_array
					has_receiver: true
					params_known: true
				}
			}
		}
		if clean_map := map_type_from_receiver(clean) {
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if checker_is_raw_collection_method_name(mname, 'map.') || mname !in tc.fn_ret_types {
					continue
				}
				if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
					continue
				}
				return tc.call_info(mname, true)
			}
			match fn_node.value {
				'clone' {
					if bad_type := tc.ownership_default_clone_missing_method(clean_map.key_type) {
						tc.record_error(.call_arg_mismatch,
							'cannot clone map keys: `${bad_type}` requires ownership destruction but has no `clone()` method',
							id)
					}
					if bad_type := tc.ownership_default_clone_missing_method(clean_map.value_type) {
						tc.record_error(.call_arg_mismatch,
							'cannot clone map values: `${bad_type}` requires ownership destruction but has no `clone()` method',
							id)
					}
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if clean_map := map_type_from_receiver(clean) {
			if fn_node.value == 'keys' {
				if bad_type := tc.ownership_default_clone_missing_method(clean_map.key_type) {
					tc.record_error(.call_arg_mismatch,
						'cannot return independent map keys: `${bad_type}` requires ownership destruction but has no `clone()` method',
						id)
				}
				return CallInfo{
					name:         'map.keys'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: clean_map.key_type
					})
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'values' {
				if bad_type := tc.ownership_default_clone_missing_method(clean_map.value_type) {
					tc.record_error(.call_arg_mismatch,
						'cannot return independent map values: `${bad_type}` requires ownership destruction but has no `clone()` method',
						id)
				}
				return CallInfo{
					name:         'map.values'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: clean_map.value_type
					})
					has_receiver: true
					params_known: true
				}
			}
			map_method := 'map.${fn_node.value}'
			if map_method in tc.fn_ret_types {
				if info := tc.map_builtin_call_info(base_type, clean_map, fn_node.value, map_method) {
					return info
				}
				return tc.call_info(map_method, true)
			}
		}
		if clean_array := array_like_type_for_method(clean, fn_node.value) {
			match fn_node.value {
				'first', 'last', 'pop', 'pop_left' {
					if fn_node.value in ['first', 'last'] {
						if bad_type := tc.ownership_default_clone_missing_method(clean_array.elem_type) {
							tc.record_error(.call_arg_mismatch,
								'cannot return an independent array element: `${bad_type}` requires ownership destruction but has no `clone()` method',
								id)
						}
					}
					return CallInfo{
						name:         ''
						params:       tarr1(base_type)
						return_type:  clean_array.elem_type
						has_receiver: true
						params_known: true
					}
				}
				'contains' {
					elem_type := tc.array_contains_elem_type(base_node, clean_array)
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, elem_type)
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'join' {
					return CallInfo{
						name:         'array.join'
						params:       tarr2(base_type, Type(String{}))
						return_type:  Type(String{})
						has_receiver: true
						params_known: true
					}
				}
				'index', 'last_index' {
					return CallInfo{
						name:         ''
						params:       tarr2(base_type, clean_array.elem_type)
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'hex' {
					if tc.is_builtin_hex_receiver(base_type) {
						return CallInfo{
							name:         '[]u8.hex'
							params:       tarr1(base_type)
							return_type:  Type(string_)
							has_receiver: true
							params_known: true
						}
					}
				}
				'repeat' {
					if bad_type := tc.ownership_default_clone_missing_method(clean_array.elem_type) {
						tc.record_error(.call_arg_mismatch,
							'cannot repeat array elements: `${bad_type}` requires ownership destruction but has no `clone()` method',
							id)
					}
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr2(base_type, Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'repeat_to_depth' {
					return CallInfo{
						name:         'array.repeat_to_depth'
						params:       tarr3(base_type, Type(int_), Type(int_))
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'delete' {
					return CallInfo{
						name:         ''
						params:       tarr2(mutating_receiver_param_type(base_type), Type(int_))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'delete_last', 'clear' {
					return CallInfo{
						name:         ''
						params:       tarr1(mutating_receiver_param_type(base_type))
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'insert', 'prepend' {
					params := if fn_node.value == 'insert' {
						tarr3(base_type, Type(int_), clean_array.elem_type)
					} else {
						tarr2(base_type, clean_array.elem_type)
					}
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       params
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'filter' {
					$if ownership ? {
						tc.check_array_dsl_fn_borrows_element(node, clean_array.elem_type, id,
							'array.filter predicate')
					}
					if bad_type := tc.ownership_default_clone_missing_method(clean_array.elem_type) {
						tc.record_error(.call_arg_mismatch,
							'cannot filter array elements: `${bad_type}` requires ownership destruction but has no `clone()` method',
							id)
					}
					// filtering a fixed array yields a dynamic array
					filter_ret := if receiver_is_fixed_array(clean) {
						Type(Array{
							elem_type: clean_array.elem_type
						})
					} else {
						base_type
					}
					return CallInfo{
						name:         'array.filter'
						params:       tarr2(base_type, Type(bool_))
						return_type:  filter_ret
						has_receiver: true
						params_known: true
					}
				}
				'map' {
					elem_type := tc.array_map_return_elem_type(node)
					$if ownership ? {
						tc.check_array_dsl_fn_borrows_element(node, clean_array.elem_type, id,
							'array.map mapper')
					}
					if tc.array_map_result_borrows_element(node) {
						if bad_type := tc.ownership_default_clone_missing_method(elem_type) {
							tc.record_error(.call_arg_mismatch,
								'cannot clone borrowed array.map result: `${bad_type}` requires ownership destruction but has no `clone()` method',
								id)
						}
					}
					return CallInfo{
						name:         'array.map'
						params:       tarr2(base_type, elem_type)
						return_type:  Type(Array{
							elem_type: elem_type
						})
						has_receiver: true
						params_known: true
					}
				}
				'any', 'all' {
					$if ownership ? {
						tc.check_array_dsl_fn_borrows_element(node, clean_array.elem_type, id,
							'array.${fn_node.value} predicate')
					}
					return CallInfo{
						name:         'array.${fn_node.value}'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(bool_)
						has_receiver: true
						params_known: true
					}
				}
				'count' {
					$if ownership ? {
						tc.check_array_dsl_fn_borrows_element(node, clean_array.elem_type, id,
							'array.count predicate')
					}
					return CallInfo{
						name:         'array.count'
						params:       tarr2(base_type, Type(bool_))
						return_type:  Type(int_)
						has_receiver: true
						params_known: true
					}
				}
				'sort_with_compare' {
					return CallInfo{
						name:         'array.sort_with_compare'
						params:       [
							mutating_receiver_param_type(base_type),
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
								]
								return_type: Type(int_)
							}),
						]
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted_with_compare' {
					return CallInfo{
						name:         'array.sorted_with_compare'
						params:       [base_type,
							Type(FnType{
								params:      [
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
									Type(Pointer{
										base_type: clean_array.elem_type
									}),
								]
								return_type: Type(int_)
							})]
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				'sort' {
					mut params := tarr1(mutating_receiver_param_type(base_type))
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sort'
						params:       params
						return_type:  Type(void_)
						has_receiver: true
						params_known: true
					}
				}
				'sorted' {
					mut params := tarr1(base_type)
					if call_explicit_arg_count(node) > 0 {
						params << Type(bool_)
					}
					return CallInfo{
						name:         'array.sorted'
						params:       params
						return_type:  base_type
						has_receiver: true
						params_known: true
					}
				}
				else {}
			}
		}
		if fixed_array := tc.fixed_array_type_from_receiver(clean) {
			if fn_node.value == 'clone' {
				return CallInfo{
					name:         'array.clone'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: fixed_array.elem_type
					})
					has_receiver: true
					params_known: true
				}
			}
		}
		mut array_pointers_fallback := false
		if fn_node.value == 'pointers' {
			if _ := array_type_from_receiver(clean) {
				array_pointers_fallback = true
			}
		}
		if fixed_array := tc.fixed_array_type_from_receiver(clean) {
			if fn_node.value == 'wait' {
				if info := tc.fixed_array_thread_wait_call_info(base_type, fixed_array) {
					return info
				}
			}
			if info := tc.fixed_array_dynamic_receiver_call_info(base_type, fixed_array,
				fn_node.value)
			{
				return info
			}
			if fn_node.value == 'pointers' {
				if base_type !is Pointer && !tc.expr_can_take_address(base_id) {
					tc.record_error(.call_arg_mismatch,
						'fixed array receiver for `pointers` must be addressable', id)
				}
				if info := tc.fixed_array_pointers_call_info(base_type) {
					return info
				}
			}
		}
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			if fn_node.value == 'str' && (clean is Primitive || clean is Char || clean is Rune) {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			if info := tc.resolve_generic_struct_method(type_name, fn_node.value) {
				return info
			}
			for mname in receiver_method_name_candidates(clean, fn_node.value, tc.cur_module) {
				if mname in tc.fn_ret_types {
					if array_pointers_fallback && mname == 'array.pointers' {
						continue
					}
					if !tc.method_can_be_called_on_receiver(base_type, fn_node.value, mname) {
						continue
					}
					if clean_map := map_type_from_receiver(clean) {
						if info := tc.map_builtin_call_info(base_type, clean_map, fn_node.value,
							mname)
						{
							return info
						}
					}
					return tc.call_info(mname, true)
				}
			}
			if array_pointers_fallback {
				return CallInfo{
					name:         'array.pointers'
					params:       tarr1(base_type)
					return_type:  Type(Array{
						elem_type: Type(voidptr_)
					})
					has_receiver: true
					params_known: true
				}
			}
			if fixed_array := tc.fixed_array_type_from_receiver(clean) {
				if info := tc.fixed_array_dynamic_receiver_call_info(base_type, fixed_array,
					fn_node.value)
				{
					return info
				}
				if fn_node.value == 'pointers' {
					if base_type !is Pointer && !tc.expr_can_take_address(base_id) {
						tc.record_error(.call_arg_mismatch,
							'fixed array receiver for `pointers` must be addressable', id)
					}
					if info := tc.fixed_array_pointers_call_info(base_type) {
						return info
					}
				}
			}
			if clean is Interface {
				if info := tc.interface_receiver_method_call_info(clean.name, fn_node.value) {
					return info
				}
			}
			if info := tc.embedded_method_call_info(type_name, fn_node.value) {
				return info
			}
			if fn_node.value == 'use' && clean is Struct
				&& tc.struct_has_middleware_receiver(type_name) {
				return CallInfo{
					name:         '${type_name}.use'
					params:       []Type{}
					return_type:  Type(void_)
					has_receiver: true
					params_known: false
				}
			}
		}
		if clean is SumType {
			if info := tc.resolve_generic_sum_method(clean.name, fn_node.value) {
				return info
			}
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if clean is Enum {
			if clean.is_flag && fn_node.value in ['has', 'all'] {
				return CallInfo{
					name:         ''
					params:       tarr2(base_type, base_type)
					return_type:  Type(bool_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value in ['set', 'clear', 'toggle'] {
				return CallInfo{
					name:         ''
					params:       tarr2(base_type, base_type)
					return_type:  Type(void_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value in ['set_all', 'clear_all'] {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(void_)
					has_receiver: true
					params_known: true
				}
			}
			if clean.is_flag && fn_node.value == 'is_empty' {
				return CallInfo{
					name:         ''
					params:       tarr1(base_type)
					return_type:  Type(bool_)
					has_receiver: true
					params_known: true
				}
			}
			if fn_node.value == 'str' {
				return CallInfo{
					name:         '${clean.name}.str'
					params:       tarr1(base_type)
					return_type:  Type(string_)
					has_receiver: true
					params_known: true
				}
			}
			mname := '${clean.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return tc.call_info(mname, true)
			}
		}
		if fn_node.value == 'clone' && tc.type_has_compiler_default_clone(clean) {
			if bad_type := tc.ownership_default_clone_missing_method(clean) {
				tc.record_error(.call_arg_mismatch,
					'cannot generate default clone for `${clean.name()}`: `${bad_type}` requires ownership destruction but has no `clone()` method',
					id)
			}
			// `#[derive(Clone)]` in Rust maps to `implements IClone` in the ownership
			// translation, whose `clone()` is compiler-provided. V structs are value types,
			// so `.clone()` yields a copy of the receiver: resolve it to the (unwrapped)
			// receiver type. A user-defined `clone()` method is matched earlier via
			// `receiver_method_name_candidates`, so this only supplies the default.
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  clean
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'free' && type_has_runtime_value(clean) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'str' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		if fn_node.value == 'hex' && tc.is_builtin_hex_receiver(base_type) {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		return none
	}
	if fn_node.kind == .ident {
		if fn_node.value == 'error' || fn_node.value == 'error_with_code' {
			mut params := []Type{}
			if fn_node.value == 'error_with_code' {
				params = tarr2(Type(string_), Type(int_))
			} else {
				params = tarr1(Type(string_))
			}
			return CallInfo{
				name:         fn_node.value
				params:       params
				return_type:  tc.parse_type('IError')
				params_known: true
			}
		}
		if fn_node.value == 'malloc' {
			return CallInfo{
				name:         'malloc'
				params:       tarr1(Type(ISize{}))
				return_type:  Type(Pointer{
					base_type: Type(u8_)
				})
				params_known: true
			}
		}
		if typ := tc.cur_scope.lookup(fn_node.value) {
			if fn_typ := fn_type_from_type(typ) {
				return CallInfo{
					name:         ''
					params:       fn_typ.params
					return_type:  fn_typ.return_type
					params_known: true
				}
			}
		}
		if local_name := tc.local_bare_fn_key(fn_node.value) {
			return tc.call_info(local_name, false)
		}
		if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
			if info := tc.decode_call_info_from_type_arg(node, imported_name, false) {
				return info
			}
			return tc.call_info(imported_name, false)
		}
		if fn_node.value in tc.fn_ret_types {
			return tc.call_info(fn_node.value, false)
		}
		if is_builtin_void_call_name(fn_node.value) {
			return CallInfo{
				name:         fn_node.value
				params:       []Type{}
				return_type:  Type(void_)
				params_known: false
			}
		}
	}
	return none
}

fn (tc &TypeChecker) binding_is_strings_builder(name string) bool {
	if name.len == 0 || tc.fn_context.node_id < 0 || tc.fn_context.node_id >= tc.a.nodes.len {
		return false
	}
	return strings_builder_binding_key(tc.fn_context.node_id, name) in tc.strings_builder_bindings
}

fn (tc &TypeChecker) expr_is_strings_new_builder_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt, .cast_expr] && node.children_count > 0 {
		return tc.expr_is_strings_new_builder_call(tc.a.child(node, 0))
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(node, 0)
	if callee.kind == .ident {
		return callee.value in ['strings.new_builder', 'strings__new_builder']
	}
	if callee.kind == .selector && callee.value == 'new_builder' && callee.children_count > 0 {
		base := tc.a.child_node(callee, 0)
		return base.kind == .ident && base.value == 'strings'
	}
	return false
}

fn (tc &TypeChecker) enum_from_call_info(enum_name string) CallInfo {
	return CallInfo{
		name: ''
		// Enum.from accepts one of two type families. The marker is validated
		// after resolving the argument in check_call_arg_types.
		params:       tarr1(Type(Unknown{
			reason: 'enum from input'
		}))
		return_type:  Type(OptionType{
			base_type: Type(Enum{
				name:    enum_name
				is_flag: enum_name in tc.flag_enums
			})
		})
		params_known: true
	}
}

fn (tc &TypeChecker) is_builtin_hex_receiver(typ Type) bool {
	if tc.type_is_pointer_receiver(typ) {
		return false
	}
	clean := typ
	if clean is Alias {
		return tc.is_builtin_hex_receiver(clean.base_type)
	}
	if clean is Array {
		return is_byte_type(clean.elem_type)
	}
	if clean is String {
		return true
	}
	if clean is Primitive {
		return prim_c_type_from(clean.props, clean.size) in ['u8', 'i8', 'u16', 'i16', 'u32', 'int',
			'u64', 'i64']
	}
	return clean is Rune || clean is Char
}

fn (tc &TypeChecker) builtin_receiver_method_call_info(base_type Type, method string) ?CallInfo {
	if !tc.is_builtin_hex_receiver(base_type) || method !in ['hex', 'hex_full'] {
		return none
	}
	clean := unwrap_pointer(base_type)
	if clean is String {
		if method == 'hex' {
			return CallInfo{
				name:         'string.hex'
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		return none
	}
	if clean is Array {
		if method == 'hex' {
			return CallInfo{
				name:         '[]u8.hex'
				params:       tarr1(base_type)
				return_type:  Type(string_)
				has_receiver: true
				params_known: true
			}
		}
		return none
	}
	method_receiver := builtin_receiver_method_type_name(clean)
	if method_receiver.len == 0 {
		return none
	}
	return CallInfo{
		name:         '${method_receiver}.${method}'
		params:       tarr1(base_type)
		return_type:  Type(string_)
		has_receiver: true
		params_known: true
	}
}

fn builtin_receiver_method_type_name(clean Type) string {
	if clean is Alias {
		return builtin_receiver_method_type_name(clean.base_type)
	}
	if clean is Primitive {
		return prim_c_type_from(clean.props, clean.size)
	}
	if clean is Rune {
		return 'rune'
	}
	if clean is Char {
		return 'char'
	}
	return ''
}

fn (tc &TypeChecker) current_receiver_param_method_call_info(base_id flat.NodeId, method string) ?CallInfo {
	if tc.fn_context.node_id < 0 || int(base_id) < 0 || int(base_id) >= tc.a.nodes.len {
		return none
	}
	base := tc.a.nodes[int(base_id)]
	if base.kind != .ident {
		return none
	}
	fn_node := tc.a.nodes[tc.fn_context.node_id]
	if fn_node.kind != .fn_decl || !fn_node.value.contains('.') {
		return none
	}
	receiver_name := fn_node.value.all_before_last('.')
	if receiver_name.len == 0 {
		return none
	}
	mut param_type := Type(void_)
	mut found_param := false
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(&fn_node, i)
		if param.kind == .param && param.value == base.value {
			param_type = tc.parse_type(param.typ)
			found_param = true
			break
		}
	}
	if !found_param {
		return none
	}
	receiver_type := tc.parse_type(receiver_name)
	if !tc.receiver_compatible(param_type, receiver_type) {
		return none
	}
	method_name := '${receiver_name}.${method}'
	qualified_method_name := checker_qualified_fn_name(tc.cur_module, method_name)
	mut candidates := []string{}
	if qualified_method_name != method_name {
		candidates << qualified_method_name
	}
	candidates << method_name
	for candidate in candidates {
		if candidate in tc.fn_ret_types {
			return tc.call_info(candidate, true)
		}
	}
	return none
}

fn (tc &TypeChecker) type_is_pointer_receiver(typ Type) bool {
	if typ is Pointer {
		return true
	}
	if typ is Alias {
		return tc.type_is_pointer_receiver(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) method_can_be_called_on_receiver(receiver Type, method string, method_name string) bool {
	if method != 'hex' || !tc.type_is_pointer_receiver(receiver) {
		return true
	}
	params := tc.fn_param_types[method_name] or { return false }
	if params.len == 0 {
		return false
	}
	return tc.type_is_pointer_receiver(params[0])
}

fn (tc &TypeChecker) receiver_expr_is_pointer(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if typ := tc.cur_scope.lookup(node.value) {
			return tc.type_is_pointer_receiver(typ)
		}
	}
	if node.typ.starts_with('&') {
		return true
	}
	return tc.type_is_pointer_receiver(tc.resolve_type(id))
}

fn is_byte_type(typ Type) bool {
	if typ is Alias {
		return is_byte_type(typ.base_type)
	}
	return typ is Primitive && typ.props.has(.integer) && typ.props.has(.unsigned) && typ.size == 8
}

fn (mut tc TypeChecker) resolve_generic_call_info(id flat.NodeId, fn_node flat.Node) ?CallInfo {
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	base_id := tc.a.child(&fn_node, 0)
	type_args := tc.generic_call_type_arg_names(fn_node)
	if type_args.len == 0 {
		return none
	}
	tc.check_missing_concrete_generic_call_type_args(fn_node)
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .selector && base_node.children_count > 0 {
		recv_id := tc.a.child(&base_node, 0)
		base_type := tc.resolve_type(recv_id)
		clean := unwrap_pointer(base_type)
		type_name := resolve_type_name_for_method(clean)
		if type_name.len > 0 {
			call_name := '${type_name}.${base_node.value}'
			if call_name in tc.fn_ret_types {
				if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
					return tc.call_info(call_name, true)
				}
				if info := tc.explicit_generic_call_info(call_name, true, type_args) {
					return info
				}
				return tc.call_info(call_name, true)
			}
			if receiver_info := tc.resolve_generic_struct_method(type_name, base_node.value) {
				if tc.explicit_generic_arg_count_mismatch(receiver_info.name, type_args, id) {
					return receiver_info
				}
				return tc.specialize_explicit_generic_receiver_call(receiver_info, type_args)
			}
			// Generic methods promoted from an embedded non-generic receiver keep
			// the declaring receiver in the function table. Resolve that promoted
			// method before rejecting `outer.method[T](...)`.
			if embedded_info := tc.embedded_method_call_info(type_name, base_node.value) {
				if tc.explicit_generic_arg_count_mismatch(embedded_info.name, type_args, id) {
					return embedded_info
				}
				if info := tc.explicit_generic_call_info(embedded_info.name, true, type_args) {
					return info
				}
				return embedded_info
			}
		}
		if static_name := tc.explicit_generic_static_selector_key(base_node) {
			if tc.explicit_generic_arg_count_mismatch(static_name, type_args, id) {
				return tc.call_info(static_name, false)
			}
			if info := tc.explicit_generic_call_info(static_name, false, type_args) {
				return info
			}
			return tc.call_info(static_name, false)
		}
	}
	call_name := tc.generic_call_base_name(base_node) or {
		if type_name := tc.generic_call_base_type_name(base_node) {
			return CallInfo{
				name:         ''
				params:       []Type{}
				return_type:  tc.parse_type('${type_name}[${type_args.join(', ')}]')
				params_known: false
			}
		}
		return none
	}
	if is_veb_run_at_call_name(call_name) {
		return CallInfo{
			name:         call_name
			params:       []Type{}
			return_type:  Type(ResultType{
				base_type: Type(void_)
			})
			params_known: false
		}
	}
	if call_name !in tc.fn_ret_types {
		return none
	}
	if is_decode_call_name(call_name) {
		if type_args.len != 1 {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'generic argument count mismatch for `${call_name}`: expected 1, got ${type_args.len}',
					id)
			}
			return tc.failed_explicit_generic_call_info(call_name)
		}
		return CallInfo{
			name:          call_name
			params:        tc.fn_param_types[call_name] or { []Type{} }
			shared_params: tc.fn_shared_params[call_name] or { []bool{} }
			return_type:   Type(ResultType{
				base_type: tc.parse_type(type_args[0])
			})
			has_receiver:  false
			is_variadic:   tc.fn_variadic[call_name] or { false }
			is_c_variadic: tc.c_variadic_fns[call_name] or { false }
			params_known:  call_name in tc.fn_param_types
		}
	}
	if tc.explicit_generic_arg_count_mismatch(call_name, type_args, id) {
		return tc.call_info(call_name, false)
	}
	if info := tc.explicit_generic_call_info(call_name, false, type_args) {
		return info
	}
	return tc.call_info(call_name, false)
}

fn (mut tc TypeChecker) check_missing_concrete_generic_call_type_args(index_node flat.Node) {
	for i in 1 .. index_node.children_count {
		arg_id := tc.a.child(&index_node, i)
		arg := tc.a.node(arg_id)
		if arg.kind == .index {
			continue
		}
		name := tc.generic_call_type_arg_name(arg_id)
		if name.len == 0 || name in tc.fn_context.generic_params {
			continue
		}
		qualified := tc.qualify_name(name)
		if name !in tc.struct_generic_params && qualified !in tc.struct_generic_params
			&& name !in tc.sum_generic_params && qualified !in tc.sum_generic_params
			&& name !in tc.type_alias_generic_params && qualified !in tc.type_alias_generic_params {
			continue
		}
		tc.record_error_at(.unknown_type, 'missing concrete type on generic type', arg_id, arg.pos)
	}
}

fn (mut tc TypeChecker) specialize_explicit_generic_receiver_call(info CallInfo, type_args []string) CallInfo {
	generic_params := tc.fn_generic_params[info.name] or { return info }
	if generic_params.len == 0 || generic_params.len != type_args.len {
		return info
	}
	mut concrete_types := []Type{cap: type_args.len}
	for arg in type_args {
		concrete_types << tc.parse_type(tc.qualify_resolution_type_text(arg))
	}
	mut params := []Type{cap: info.params.len}
	for param in info.params {
		params << tc.substitute_generic_type_values(param, concrete_types, generic_params)
	}
	return CallInfo{
		...info
		params:      params
		return_type: tc.substitute_generic_type_values(info.return_type, concrete_types,
			generic_params)
	}
}

fn (tc &TypeChecker) explicit_generic_static_selector_key(selector flat.Node) ?string {
	if selector.kind != .selector || selector.children_count == 0 {
		return none
	}
	receiver := tc.a.child_node(&selector, 0)
	if receiver.kind == .ident {
		if tc.ident_resolves_to_value(receiver.value) {
			return none
		}
		return tc.static_assoc_fn_key_for_base(receiver.value, selector.value)
	}
	if receiver.kind == .selector && receiver.children_count > 0 {
		module_node := tc.a.child_node(receiver, 0)
		if module_node.kind == .ident {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			return tc.static_assoc_fn_key_for_base('${module_name}.${receiver.value}',
				selector.value)
		}
	}
	return none
}

fn (mut tc TypeChecker) explicit_generic_arg_count_mismatch(name string, type_args []string, id flat.NodeId) bool {
	generic_params := tc.fn_generic_params[name] or { return false }
	if type_args.len == generic_params.len {
		return false
	}
	if tc.should_diagnose(id) {
		plural := if generic_params.len == 1 { '' } else { 's' }
		call := tc.a.node(id)
		callee_id := if call.children_count > 0 { tc.a.child(call, 0) } else { id }
		tc.record_error_at(.call_arg_mismatch,
			'expected ${generic_params.len} generic parameter${plural}, got ${type_args.len}', id,
			tc.explicit_generic_args_diagnostic_pos(callee_id))
	}
	return true
}

fn (tc &TypeChecker) failed_explicit_generic_call_info(name string) CallInfo {
	return CallInfo{
		name:         name
		params:       []Type{}
		return_type:  unknown_type('invalid explicit generic call `${name}`')
		params_known: false
	}
}

// explicit_generic_concrete_arg_text qualifies an explicit generic type argument
// spelled at a call site so it survives being re-parsed in the callee module.
fn (tc &TypeChecker) explicit_generic_concrete_arg_text(type_arg string) string {
	qualified := tc.qualify_resolution_type_text(type_arg)
	if !qualified.contains('.') && !is_builtin_type_name(qualified)
		&& qualified !in tc.fn_context.generic_params && (qualified in tc.structs
		|| qualified in tc.enum_names || qualified in tc.flag_enums
		|| qualified in tc.sum_types || qualified in tc.interface_names
		|| qualified in tc.type_aliases) {
		return 'main.' + qualified
	}
	return qualified
}

fn (tc &TypeChecker) explicit_generic_call_info(name string, has_receiver bool, type_args []string) ?CallInfo {
	generic_params := tc.fn_generic_params[name] or { return none }
	param_texts := tc.fn_param_type_texts[name] or { return none }
	if generic_params.len == 0 || type_args.len != generic_params.len {
		return none
	}
	mut concrete_args := []string{cap: generic_params.len}
	for i in 0 .. generic_params.len {
		concrete_args << tc.explicit_generic_concrete_arg_text(type_args[i])
	}
	mut sub_params := []Type{}
	for param_text in param_texts {
		sub_params << tc.parse_fn_signature_type(name, subst_generic_text(param_text,
			concrete_args, generic_params))
	}
	ret_text := tc.fn_ret_type_texts[name] or { '' }
	sub_ret := if ret_text.len > 0 {
		tc.parse_fn_signature_type(name,
			subst_generic_text(ret_text, concrete_args, generic_params))
	} else {
		tc.fn_ret_types[name] or { Type(void_) }
	}
	return CallInfo{
		name:                 name
		params:               sub_params
		shared_params:        tc.fn_shared_params[name] or { []bool{} }
		return_type:          sub_ret
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         true
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn (tc &TypeChecker) generic_call_base_type_name(base_node flat.Node) ?string {
	if base_node.kind == .ident {
		qname := tc.qualify_name(base_node.value)
		if tc.type_symbol_known(qname) {
			return qname
		}
		if tc.type_symbol_known(base_node.value) {
			return base_node.value
		}
		if resolved := tc.resolve_selective_import_type_symbol(base_node.value) {
			return resolved
		}
	}
	if base_node.kind == .selector && base_node.children_count > 0 {
		inner := tc.a.child_node(&base_node, 0)
		if inner.kind == .ident {
			if static_name := tc.static_assoc_fn_key_for_base(inner.value, base_node.value) {
				return static_name
			}
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}'
			if tc.type_symbol_known(full_name) {
				return full_name
			}
		}
	}
	return none
}

fn is_decode_call_name(name string) bool {
	return name in ['json.decode', 'json2.decode', 'x.json2.decode']
}

fn (tc &TypeChecker) decode_call_info_from_type_arg(node flat.Node, name string, has_receiver bool) ?CallInfo {
	if !is_decode_call_name(name) || node.children_count < 2 {
		return none
	}
	type_arg_id := tc.a.child(&node, 1)
	type_arg := tc.generic_call_type_arg_name(type_arg_id)
	if type_arg.len == 0 {
		return none
	}
	info := tc.call_info(name, has_receiver)
	params := if info.params.len > 0 { info.params[1..].clone() } else { []Type{} }
	return CallInfo{
		name:                 info.name
		params:               params
		return_type:          Type(ResultType{
			base_type: tc.parse_type(type_arg)
		})
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         info.params_known
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           1
	}
}

fn is_veb_run_at_call_name(name string) bool {
	return name == 'veb.run_at'
}

fn (tc &TypeChecker) generic_call_base_name(base_node flat.Node) ?string {
	if base_node.kind == .ident {
		if local_name := tc.local_bare_fn_key(base_node.value) {
			return local_name
		}
		if imported_name := tc.resolve_selective_import_symbol(base_node.value) {
			return imported_name
		}
		if base_node.value in tc.fn_ret_types {
			return base_node.value
		}
		return none
	}
	if base_node.kind == .selector && base_node.children_count > 0 {
		inner := tc.a.child_node(&base_node, 0)
		if inner.kind == .ident {
			if static_name := tc.static_assoc_fn_key_for_base(inner.value, base_node.value) {
				return static_name
			}
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}'
			if is_veb_run_at_call_name(full_name) {
				return full_name
			}
			if full_name in tc.fn_ret_types {
				return full_name
			}
		}
	}
	return none
}

fn (tc &TypeChecker) generic_call_type_arg_names(index_node flat.Node) []string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return []string{}
	}
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := tc.generic_call_type_arg_name(tc.a.child(&index_node, i))
		if arg.len == 0 {
			return []string{}
		}
		args << arg
	}
	return args
}

fn (tc &TypeChecker) generic_call_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := tc.generic_call_type_arg_name(tc.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len > 0 {
				if node.value.starts_with('[') {
					return node.value
				}
				return '[]${node.value}'
			}
			return ''
		}
		.map_init {
			return node.value
		}
		.struct_decl {
			return node.value
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := tc.generic_call_type_arg_name(tc.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

fn c_fn_module_signature_key(module_name string, fn_name string) string {
	return '${module_name}\x01${fn_name}'
}

// call_info updates call info state for TypeChecker.
fn (tc &TypeChecker) call_info(name string, has_receiver bool) CallInfo {
	if name.starts_with('C.') {
		module_key := c_fn_module_signature_key(tc.cur_module, name)
		if return_type := tc.c_fn_module_ret_types[module_key] {
			is_variadic := tc.c_fn_module_variadic[module_key] or { false }
			params := tc.c_fn_module_param_types[module_key] or { []Type{} }
			return CallInfo{
				name:          name
				params:        params.clone()
				return_type:   return_type
				has_receiver:  has_receiver
				is_variadic:   is_variadic
				is_c_variadic: is_variadic
				params_known:  true
			}
		}
	}
	mut params := []Type{}
	mut params_known := false
	if p := tc.fn_param_types[name] {
		params = p.clone()
		params_known = true
	}
	if params.len == 1 && is_print_style_fn_name(name)
		&& print_style_param_accepts_string(params[0]) {
		params[0] = unknown_type('print argument')
	}
	return_type := tc.alias_return_type_from_text(name) or {
		tc.fn_ret_types[name] or { unknown_type('unknown return type for `${name}`') }
	}
	return CallInfo{
		name:                 name
		params:               params
		shared_params:        tc.fn_shared_params[name] or { []bool{} }
		return_type:          return_type
		has_receiver:         has_receiver
		is_variadic:          tc.fn_variadic[name] or { false }
		is_c_variadic:        tc.c_variadic_fns[name] or { false }
		params_known:         params_known
		has_implicit_veb_ctx: tc.fn_implicit_veb_ctx[name] or { false }
	}
}

fn (tc &TypeChecker) alias_return_type_from_text(fn_name string) ?Type {
	ret_text := tc.fn_ret_type_texts[fn_name] or { return none }
	clean := trimmed_space(ret_text)
	if clean.len == 0 {
		return none
	}
	if target := tc.type_aliases[clean] {
		return Type(Alias{
			name:      clean
			base_type: tc.parse_type(target)
		})
	}
	if clean.contains('.') {
		return none
	}
	mod := tc.fn_type_modules[fn_name] or { '' }
	if mod.len == 0 || mod in ['main', 'builtin'] {
		return none
	}
	qname := '${mod}.${clean}'
	target := tc.type_aliases[qname] or { return none }
	return Type(Alias{
		name:      qname
		base_type: tc.parse_type(target)
	})
}

fn array_type_from_receiver(t Type) ?Array {
	if t is Array {
		return t
	}
	if t is Alias {
		return array_type_from_receiver(t.base_type)
	}
	return none
}

fn (tc &TypeChecker) thread_wait_return_type(t Type) ?Type {
	clean := unwrap_pointer(t)
	if clean is Struct {
		thread_name := trimmed_space(clean.name)
		if thread_name == 'thread' || thread_name.ends_with('.thread') {
			return Type(void_)
		}
		if thread_name.starts_with('thread ') {
			return tc.parse_type(trimmed_space(thread_name[7..]))
		}
	}
	return none
}

// fixed_array_lowered_methods lists the builtin array methods the transform
// actually lowers for fixed-array receivers (it copies the fixed array into a
// dynamic temp and re-dispatches). Methods outside this list stay rejected:
// in-place mutators like `sort` would silently modify the temp copy, and
// `first`/`last`/`pop` are not fixed-array methods in V.
const fixed_array_lowered_methods = ['contains', 'index', 'last_index', 'any', 'all', 'count',
	'map', 'filter', 'str', 'wait']

fn receiver_is_fixed_array(t Type) bool {
	if t is ArrayFixed {
		return true
	}
	if t is Alias {
		return receiver_is_fixed_array(t.base_type)
	}
	return false
}

fn mutating_receiver_param_type(base_type Type) Type {
	if base_type is Pointer {
		return base_type
	}
	return Type(Pointer{
		base_type: base_type
	})
}

// array_like_type_for_method returns the receiver's array type for a builtin
// array method call. Fixed-array receivers are widened to a dynamic array type,
// but only for the methods the transform can lower for them.
fn array_like_type_for_method(t Type, method string) ?Array {
	if t is Array {
		return t
	}
	if t is ArrayFixed {
		if method in fixed_array_lowered_methods {
			return Array{
				elem_type: t.elem_type
			}
		}
		return none
	}
	if t is Alias {
		return array_like_type_for_method(t.base_type, method)
	}
	return none
}

fn map_type_from_receiver(t Type) ?Map {
	if t is Map {
		return t
	}
	if t is Alias {
		return map_type_from_receiver(t.base_type)
	}
	return none
}

fn (tc &TypeChecker) map_type_from_alias_target(t Type) ?Map {
	if map_type := map_type_from_receiver(t) {
		return map_type
	}
	name := t.name()
	if name.len == 0 {
		return none
	}
	target := tc.alias_target_type_text(name) or { return none }
	return map_type_from_receiver(tc.parse_type(target))
}

fn (tc &TypeChecker) receiver_is_sum_type(t Type) bool {
	if t is SumType {
		return true
	}
	if t is Alias {
		return tc.receiver_is_sum_type(t.base_type)
	}
	name := t.name()
	if name.len == 0 {
		return false
	}
	if name in tc.sum_types {
		return true
	}
	if !name.contains('.') {
		qname := tc.qualify_name(name)
		if qname in tc.sum_types {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fixed_array_type_from_receiver(t Type) ?ArrayFixed {
	if t is ArrayFixed {
		return t
	}
	if t is Alias {
		return tc.fixed_array_type_from_receiver(t.base_type)
	}
	name := t.name()
	if name.starts_with('fn(') || name.starts_with('fn (') {
		return none
	}
	if name.contains('[') && !name.starts_with('[') {
		bracket := name.last_index_u8(`[`)
		bracket_end := name.last_index_u8(`]`)
		if bracket >= 0 && bracket_end > bracket {
			len_text := trimmed_space(name[bracket + 1..bracket_end])
			if !is_fixed_array_len_text(len_text)
				&& tc.const_int_value_in_module(len_text, tc.cur_module, []string{}) == none {
				return none
			}
			return ArrayFixed{
				elem_type: tc.parse_type(name[..bracket])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			}
		}
	}
	return none
}

fn (tc &TypeChecker) fixed_array_thread_wait_call_info(base_type Type, arr ArrayFixed) ?CallInfo {
	elem := arr.elem_type
	if elem is Struct {
		name := trimmed_space(elem.name)
		if name == 'thread' {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  Type(void_)
				has_receiver: true
				params_known: true
			}
		}
		if name.starts_with('thread ') {
			return CallInfo{
				name:         ''
				params:       tarr1(base_type)
				return_type:  tc.thread_array_wait_return_type(name[7..])
				has_receiver: true
				params_known: true
			}
		}
	}
	return none
}

fn (tc &TypeChecker) thread_array_wait_return_type(payload string) Type {
	ret_name := trimmed_space(payload)
	if ret_name == '?' {
		return Type(OptionType{
			base_type: Type(void_)
		})
	}
	if ret_name == '!' {
		return Type(ResultType{
			base_type: Type(void_)
		})
	}
	ret_type := tc.parse_type(ret_name)
	if ret_type is OptionType {
		if ret_type.base_type is Void {
			return ret_type
		}
		return Type(OptionType{
			base_type: Type(Array{
				elem_type: ret_type.base_type
			})
		})
	}
	if ret_type is ResultType {
		if ret_type.base_type is Void {
			return ret_type
		}
		return Type(ResultType{
			base_type: Type(Array{
				elem_type: ret_type.base_type
			})
		})
	}
	return Type(Array{
		elem_type: ret_type
	})
}

fn (tc &TypeChecker) fixed_array_dynamic_receiver_call_info(base_type Type, arr ArrayFixed, method string) ?CallInfo {
	array_type := Array{
		elem_type: arr.elem_type
	}
	mut candidates := []string{}
	push_receiver_method_candidate(mut candidates,
		'${resolve_type_name_for_method(Type(array_type))}.${method}')
	append_array_receiver_method_candidates(mut candidates, array_type, method, tc.cur_module)
	for mname in candidates {
		if mname !in tc.fn_ret_types {
			continue
		}
		info := tc.call_info(mname, true)
		mut params := info.params.clone()
		if params.len > 0 {
			params[0] = base_type
		}
		return CallInfo{
			name:                 info.name
			params:               params
			return_type:          info.return_type
			has_receiver:         info.has_receiver
			is_variadic:          info.is_variadic
			is_c_variadic:        info.is_c_variadic
			params_known:         info.params_known
			has_implicit_veb_ctx: info.has_implicit_veb_ctx
		}
	}
	return none
}

fn (tc &TypeChecker) fixed_array_pointers_call_info(base_type Type) ?CallInfo {
	mname := 'array.pointers'
	return_type := tc.fn_ret_types[mname] or { return none }
	return CallInfo{
		name:         mname
		params:       tarr1(base_type)
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn (tc &TypeChecker) expr_can_take_address(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return true
		}
		.index {
			if node.value == 'range' {
				return false
			}
			if node.children_count > 0 {
				base_id := tc.a.child(&node, 0)
				base_type0 := unwrap_pointer(tc.resolve_type(base_id))
				mut base_type := base_type0
				if base_type0 is Alias {
					base_type = base_type0.base_type
				}
				if base_type is Map {
					return false
				}
			}
			return node.children_count > 0 && tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			if tc.enum_selector_type(&node) != none {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_can_take_address(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) flag_enum_mutating_receiver_method(fn_node flat.Node, recv_type Type, info CallInfo) ?string {
	if !info.has_receiver || fn_node.kind != .selector
		|| fn_node.value !in ['set', 'clear', 'toggle', 'set_all', 'clear_all'] {
		return none
	}
	clean := unwrap_pointer(recv_type)
	if clean is Enum && (clean.is_flag || clean.name in tc.flag_enums) {
		return fn_node.value
	}
	return none
}

fn (tc &TypeChecker) flag_enum_receiver_is_mutable_lvalue(recv_id flat.NodeId) bool {
	if !tc.expr_can_take_address(recv_id) {
		return false
	}
	return tc.expr_root_is_mutable_lvalue(recv_id)
}

fn (tc &TypeChecker) mut_receiver_expr_is_mutable_lvalue(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			// A pointer-valued binding can mutate its pointee without mutating the
			// binding itself, just like a pointer-valued selector or call result.
			if tc.type_is_pointer_receiver(tc.cached_expr_type(id) or { tc.resolve_type(id) }) {
				return true
			}
			return tc.ident_is_mutable_lvalue(node.value)
		}
		.index, .selector {
			// Mutating the pointee of a pointer-valued field or element does not
			// mutate the binding that stores the pointer. This is common for
			// application state such as `app.window.refresh()`.
			if tc.type_is_pointer_receiver(tc.cached_expr_type(id) or { tc.resolve_type(id) }) {
				return true
			}
			return node.children_count > 0
				&& tc.mut_receiver_expr_is_mutable_lvalue(tc.a.child(&node, 0))
		}
		.paren {
			return node.children_count > 0
				&& tc.mut_receiver_expr_is_mutable_lvalue(tc.a.child(&node, 0))
		}
		.or_expr {
			// Optional/result postfix propagation (`value?.method()` / `value!.method()`)
			// borrows the payload stored in the original lvalue.
			return node.value in ['?', '!'] && node.children_count > 0
				&& tc.mut_receiver_expr_is_mutable_lvalue(tc.a.child(&node, 0))
		}
		.call {
			// A call that returns a pointer yields mutable pointee storage even though
			// the pointer expression itself is not an lvalue. This permits fluent
			// mutable methods that return `&Receiver`.
			return tc.type_is_pointer_receiver(tc.cached_expr_type(id) or { tc.resolve_type(id) })
		}
		.prefix {
			if node.op in [.amp, .mul] {
				return node.children_count > 0
					&& tc.mut_receiver_expr_is_mutable_lvalue(tc.a.child(&node, 0))
			}
			return false
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) expr_root_is_mutable_lvalue(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return tc.ident_is_mutable_lvalue(node.value)
		}
		.index, .selector, .paren {
			return node.children_count > 0 && tc.expr_root_is_mutable_lvalue(tc.a.child(&node, 0))
		}
		.prefix {
			return node.op == .mul
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) expr_root_constant_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			qname := tc.qualify_name(node.value)
			if qname in tc.const_types || node.value in tc.const_types {
				return node.value
			}
		}
		.index, .selector, .paren {
			if node.children_count > 0 {
				return tc.expr_root_constant_name(tc.a.child(&node, 0))
			}
		}
		else {}
	}
	return none
}

fn (tc &TypeChecker) ident_is_mutable_lvalue(name string) bool {
	if name.len == 0 {
		return false
	}
	if tc.current_binding_is_shared(name) && tc.current_shared_lock_mode(name) == `w` {
		return true
	}
	if tc.mut_param_binding_matches_lvalue(name) {
		return true
	}
	if tc.cur_scope == unsafe { nil } {
		return false
	}
	if owner := tc.fn_context.mut_local_owners[name] {
		if tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return true
		}
	}
	// Globals are mutable storage. Respect a nearer local binding first so an
	// immutable local that shadows a global is still rejected.
	if owner := tc.cur_scope.lookup_owner(name) {
		return name in tc.global_names && tc.binding_owner_is_global(owner)
	}
	qname := tc.qualify_name(name)
	if qname != name {
		if owner := tc.cur_scope.lookup_owner(qname) {
			return qname in tc.global_names && tc.binding_owner_is_global(owner)
		}
	}
	return false
}

fn (tc &TypeChecker) ident_is_explicitly_mutable_lvalue(name string) bool {
	if name.len == 0 {
		return false
	}
	if tc.current_binding_is_shared(name) && tc.current_shared_lock_mode(name) == `w` {
		return true
	}
	return tc.mut_param_binding_matches_lvalue(name)
}

fn (tc &TypeChecker) current_shared_lock_mode(name string) u8 {
	modes := tc.fn_context.locked_shared_modes[name] or { return 0 }
	return if modes.len > 0 { modes.last() } else { u8(0) }
}

fn (tc &TypeChecker) binding_owner_is_global(owner ScopeBindingOwner) bool {
	// A parallel checker adds its private file scope in front of the collected
	// program scope, so globals can live in any ancestor of `file_scope`.
	mut scope := unsafe { &Scope(tc.file_scope) }
	for scope != unsafe { nil } {
		if owner.belongs_to_scope(scope) {
			return true
		}
		scope = scope.parent
	}
	return false
}

fn (tc &TypeChecker) ident_is_global_binding(name string) bool {
	if name.len == 0 || tc.cur_scope == unsafe { nil } || tc.file_scope == unsafe { nil } {
		return false
	}
	if owner := tc.cur_scope.lookup_owner(name) {
		return name in tc.global_names && tc.binding_owner_is_file_scope(owner)
	}
	qname := tc.qualify_name(name)
	if qname != name {
		if owner := tc.cur_scope.lookup_owner(qname) {
			return qname in tc.global_names && tc.binding_owner_is_file_scope(owner)
		}
	}
	return false
}

fn (tc &TypeChecker) expr_root_is_global_binding(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return tc.ident_is_global_binding(node.value)
		}
		.index, .selector, .paren {
			return node.children_count > 0 && tc.expr_root_is_global_binding(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) binding_owner_is_file_scope(owner ScopeBindingOwner) bool {
	// Parallel checker views put a private file scope in front of the collected
	// program scope, so globals can belong to either that scope or an ancestor.
	mut scope := unsafe { &Scope(tc.file_scope) }
	for scope != unsafe { nil } {
		if owner.belongs_to_scope(scope) {
			return true
		}
		scope = scope.parent
	}
	return false
}

enum ReceiverMutationVisibility {
	none
	direct
	private_path
	public_path
}

struct VisibleMutationFnDecl {
	idx int
	mod string
}

fn receiver_mutation_is_visible(vis ReceiverMutationVisibility) bool {
	return vis in [.direct, .public_path]
}

fn visible_mutation_receiver_type_name(typ string) string {
	mut clean := trimmed_space(typ)
	for clean.starts_with('&') {
		clean = trimmed_space(clean[1..])
	}
	for prefix in ['mut ', 'shared '] {
		if clean.starts_with(prefix) {
			clean = trimmed_space(clean[prefix.len..])
		}
	}
	return strip_generic_args_name(clean)
}

fn visible_mutation_fn_names_match(actual string, declared string) bool {
	if actual == declared {
		return true
	}
	if actual.all_after_last('.') != declared.all_after_last('.') {
		return false
	}
	actual_receiver := actual.all_before_last('.')
	declared_receiver := declared.all_before_last('.')
	return strip_generic_args_name(actual_receiver) == strip_generic_args_name(declared_receiver)
}

fn visible_mutation_fn_lookup_name(name string) string {
	open := name.index_u8(`[`)
	if open <= 0 {
		return name
	}
	// Only a generic receiver (`Box[int].m`) changes the lookup name; skip the
	// receiver substring allocation for the plain-method common case.
	dot := name.last_index_u8(`.`)
	if dot <= 0 || open >= dot {
		return name
	}
	receiver := name[..dot]
	clean_receiver := strip_generic_args_name(receiver)
	if clean_receiver == receiver {
		return name
	}
	return clean_receiver + name[dot..]
}

fn (tc &TypeChecker) cache_visible_mutation_fn_decl(key string, decl VisibleMutationFnDecl) {
	if isnil(tc.visible_mutation_cache) || key.len == 0 {
		return
	}
	mut cache := tc.visible_mutation_cache
	if key !in cache.decls {
		cache.decls[key] = decl
	}
}

fn (tc &TypeChecker) register_visible_mutation_fn_decl(idx int, module_name string, qname string, source_name string) {
	decl := VisibleMutationFnDecl{
		idx: idx
		mod: module_name
	}
	normalized_qname := visible_mutation_fn_lookup_name(qname)
	normalized_source_name := visible_mutation_fn_lookup_name(source_name)
	c_qname := tc.cached_c_name(qname)
	c_source_name := tc.cached_c_name(source_name)
	for candidate in [normalized_qname, normalized_source_name, c_qname, c_source_name] {
		tc.cache_visible_mutation_fn_decl('\x01${candidate}', decl)
		tc.cache_visible_mutation_fn_decl('${module_name}\x01${candidate}', decl)
	}
}

fn (tc &TypeChecker) visible_mutation_fn_decl(name string, fallback_mod string) ?VisibleMutationFnDecl {
	cache_key := '${fallback_mod}\x01${visible_mutation_fn_lookup_name(name)}'
	if !isnil(tc.visible_mutation_cache) {
		cache := tc.visible_mutation_cache
		if decl := cache.decls[cache_key] {
			return decl
		}
		if cache.decl_misses[cache_key] {
			return none
		}
		if cache.decl_index_ready {
			return none
		}
	}
	mut cur_mod := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				cur_mod = tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_mod = node.value
			}
			.fn_decl {
				if fallback_mod.len > 0 && cur_mod != fallback_mod {
					continue
				}
				qname := checker_qualified_fn_name(cur_mod, node.value)
				if visible_mutation_fn_names_match(name, qname)
					|| visible_mutation_fn_names_match(name, node.value)
					|| tc.cached_c_name(qname) == name || tc.cached_c_name(node.value) == name {
					decl := VisibleMutationFnDecl{
						idx: i
						mod: cur_mod
					}
					if !isnil(tc.visible_mutation_cache) {
						mut cache := tc.visible_mutation_cache
						cache.decls[cache_key] = decl
					}
					return decl
				}
			}
			else {}
		}
	}
	if !isnil(tc.visible_mutation_cache) {
		mut cache := tc.visible_mutation_cache
		cache.decl_misses[cache_key] = true
	}
	return none
}

fn (tc &TypeChecker) visible_mutation_fn_param(decl VisibleMutationFnDecl, param_idx int) ?flat.Node {
	if decl.idx < 0 || decl.idx >= tc.a.nodes.len || param_idx < 0 {
		return none
	}
	fn_node := tc.a.nodes[decl.idx]
	mut idx := 0
	for i in 0 .. fn_node.children_count {
		child := tc.a.child_node(&fn_node, i)
		if child.kind != .param {
			break
		}
		if idx == param_idx {
			return *child
		}
		idx++
	}
	return none
}

fn (tc &TypeChecker) visible_call_param(info CallInfo, param_idx int) ?flat.Node {
	decl_module := if info.name.starts_with('C.') {
		tc.cur_module
	} else {
		tc.fn_type_modules[info.name] or { '' }
	}
	decl := tc.visible_mutation_fn_decl(info.name, decl_module) or { return none }
	mut source_param_idx := param_idx
	if info.has_implicit_veb_ctx {
		fn_node := tc.a.nodes[decl.idx]
		ctx_idx := tc.fn_implicit_veb_ctx_insert_index(fn_node)
		if source_param_idx == ctx_idx {
			return none
		}
		if source_param_idx > ctx_idx {
			source_param_idx--
		}
	}
	return tc.visible_mutation_fn_param(decl, source_param_idx)
}

fn (tc &TypeChecker) call_param_requires_mut_pointer_slot(info CallInfo, param_idx int) bool {
	param := tc.visible_call_param(info, param_idx) or { return false }
	return param.is_mut && param.op == .amp && param.typ.starts_with('&')
}

fn (tc &TypeChecker) explicit_generic_source_param_is_mut(call flat.Node, info CallInfo, param_idx int) bool {
	mut name := ''
	if call.children_count > 0 {
		callee := tc.a.child_node(&call, 0)
		if callee.kind == .index && callee.children_count > 0 {
			base := tc.a.child_node(callee, 0)
			name = tc.generic_call_base_name(*base) or { '' }
		}
	}
	if name.len == 0 && info.name.index_after_('_T_', 0) >= 0 {
		name = info.name.all_before('_T_')
	}
	if name.len == 0 {
		return false
	}
	decl_module := tc.fn_type_modules[name] or { tc.cur_module }
	decl := tc.visible_mutation_fn_decl(name, decl_module) or { return false }
	param := tc.visible_mutation_fn_param(decl, param_idx) or { return false }
	return param.is_mut
}

fn (tc &TypeChecker) call_param_is_mut(info CallInfo, param_idx int) bool {
	if info.name.starts_with('chan ') && info.name.ends_with('.try_pop') && info.has_receiver
		&& param_idx == 1 {
		return true
	}
	if param := tc.visible_call_param(info, param_idx) {
		return param.is_mut
	}
	mut name := info.name
	for name.len > 0 {
		if params := tc.declaration_param_mutability[name] {
			return param_idx >= 0 && param_idx < params.len && params[param_idx]
		}
		dot := name.index_u8(`.`)
		if dot < 0 {
			break
		}
		name = name[dot + 1..]
	}
	return false
}

fn (tc &TypeChecker) call_field_param_is_mut(node flat.Node, param_idx int) bool {
	if node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(callee, 0)))
	if receiver_type !is Struct {
		return false
	}
	field_type := tc.struct_field_type(receiver_type.name(), callee.value) or { return false }
	raw_fn_type := if field_type is Alias {
		tc.source_fn_alias_type_text(field_type.name) or { '' }
	} else {
		field_type.name()
	}
	if raw_fn_type.len == 0 {
		return false
	}
	params, _ := fn_diagnostic_type_parts(raw_fn_type)
	return param_idx >= 0 && param_idx < params.len && params[param_idx].is_mut
}

fn (tc &TypeChecker) mut_pointer_slot_arg_compatible(actual Type, expected Type) bool {
	if tc.type_compatible(actual, expected) {
		return true
	}
	if expected is Pointer {
		if unalias_type(actual) is Primitive && tc.type_compatible(actual, expected.base_type) {
			return true
		}
		if unalias_type(actual) is Primitive && expected.base_type is Pointer
			&& tc.type_compatible(actual, expected.base_type.base_type) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) visible_mutation_struct_field_is_public(receiver_type string, field_name string, decl_mod string) ?bool {
	type_name := visible_mutation_receiver_type_name(receiver_type)
	short_name := type_name.all_after_last('.')
	// Every struct declaration whose name (or qualified name) can match is
	// indexed under its short name, in top-level order; the old full scan over
	// every top-level declaration ran once per checked struct-init field.
	for i in tc.type_declaration_ids[short_name] or { []int{} } {
		node := tc.a.nodes[i]
		if node.kind != .struct_decl {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		cur_mod := tc.file_modules[file.name] or { '' }
		if decl_mod.len > 0 && cur_mod != decl_mod {
			continue
		}
		qname := if cur_mod in ['', 'main', 'builtin'] {
			node.value
		} else {
			'${cur_mod}.${node.value}'
		}
		if node.value != short_name && qname != type_name {
			continue
		}
		for j in 0 .. node.children_count {
			field := tc.a.child_node(&node, j)
			if field.kind != .field_decl || field.value != field_name {
				continue
			}
			meta := field.generic_params()
			return meta.len > 0 && meta[0].contains('p')
		}
		return none
	}
	return none
}

fn (tc &TypeChecker) receiver_expr_mutation_visibility(expr_id flat.NodeId, root_name string, receiver_type string, decl_mod string) ReceiverMutationVisibility {
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return .none
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.ident {
			return if node.value == root_name { .direct } else { .none }
		}
		.paren, .prefix, .postfix, .cast_expr, .as_expr, .expr_stmt {
			if node.children_count > 0 {
				return tc.receiver_expr_mutation_visibility(tc.a.child(&node, 0), root_name,
					receiver_type, decl_mod)
			}
		}
		.selector {
			if node.children_count == 0 {
				return .none
			}
			mut parent_id := tc.a.child(&node, 0)
			for int(parent_id) >= 0 && int(parent_id) < tc.a.nodes.len {
				parent := tc.a.nodes[int(parent_id)]
				if parent.kind != .paren || parent.children_count == 0 {
					break
				}
				parent_id = tc.a.child(&parent, 0)
			}
			if int(parent_id) >= 0 && int(parent_id) < tc.a.nodes.len {
				parent := tc.a.nodes[int(parent_id)]
				if parent.kind == .ident && parent.value == root_name {
					is_pub := tc.visible_mutation_struct_field_is_public(receiver_type, node.value,
						decl_mod) or { true }
					return if is_pub { .public_path } else { .private_path }
				}
			}
			return tc.receiver_expr_mutation_visibility(parent_id, root_name, receiver_type,
				decl_mod)
		}
		.index {
			if node.children_count > 0 {
				return tc.receiver_expr_mutation_visibility(tc.a.child(&node, 0), root_name,
					receiver_type, decl_mod)
			}
		}
		.call {
			if node.children_count > 0 {
				mut fn_node := tc.a.child_node(&node, 0)
				if fn_node.kind == .index && fn_node.children_count > 0 {
					fn_node = tc.a.child_node(fn_node, 0)
				}
				if fn_node.kind == .selector && fn_node.children_count > 0 {
					return tc.receiver_expr_mutation_visibility(tc.a.child(fn_node, 0), root_name,
						receiver_type, decl_mod)
				}
			}
		}
		else {}
	}
	return .none
}

fn (tc &TypeChecker) visible_mutation_call_name(call_id flat.NodeId, call flat.Node, root_type string, decl_mod string) string {
	if name := tc.resolved_call_name(call_id) {
		return name
	}
	if call.children_count == 0 {
		return ''
	}
	mut fn_node := tc.a.child_node(&call, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		fn_node = tc.a.child_node(fn_node, 0)
	}
	if fn_node.kind == .ident {
		return checker_qualified_fn_name(decl_mod, fn_node.value)
	}
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		if base.kind == .ident && base.value.len > 0 && base.value[0] >= `A` && base.value[0] <= `Z` {
			return checker_qualified_fn_name(decl_mod, '${base.value}.${fn_node.value}')
		}
		receiver_name := visible_mutation_receiver_type_name(root_type)
		return checker_qualified_fn_name(decl_mod, '${receiver_name}.${fn_node.value}')
	}
	return ''
}

fn (tc &TypeChecker) call_has_visible_receiver_mutation(call_id flat.NodeId, call flat.Node, root_name string, root_type string, decl_mod string, mut visiting map[u64]bool) bool {
	if call.children_count == 0 {
		return false
	}
	mut fn_node := tc.a.child_node(&call, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		fn_node = tc.a.child_node(fn_node, 0)
	}
	called_name := tc.visible_mutation_call_name(call_id, call, root_type, decl_mod)
	called_mod := tc.fn_type_modules[called_name] or { decl_mod }
	decl := tc.visible_mutation_fn_decl(called_name, called_mod) or {
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_vis := tc.receiver_expr_mutation_visibility(tc.a.child(fn_node, 0), root_name,
				root_type, decl_mod)
			if receiver_mutation_is_visible(recv_vis) && tc.mut_receiver_methods[called_name] {
				return true
			}
		}
		for i in 1 .. call.children_count {
			arg_id := tc.a.child(&call, i)
			arg := tc.a.nodes[int(arg_id)]
			if arg.is_mut
				&& receiver_mutation_is_visible(tc.receiver_expr_mutation_visibility(arg_id, root_name, root_type, decl_mod)) {
				return true
			}
		}
		return false
	}
	mut is_method := false
	mut receiver_param_is_mut := false
	if first_param := tc.visible_mutation_fn_param(decl, 0) {
		is_method = first_param.op == .dot
		receiver_param_is_mut = first_param.is_mut
	}
	mut param_offset := 0
	if is_method {
		param_offset = 1
		if fn_node.kind == .selector && fn_node.children_count > 0 && receiver_param_is_mut {
			recv_vis := tc.receiver_expr_mutation_visibility(tc.a.child(fn_node, 0), root_name,
				root_type, decl_mod)
			match recv_vis {
				.direct {
					if tc.visible_mutation_fn_param_has_visible_mutation(decl, 0, mut visiting) {
						return true
					}
				}
				.public_path {
					return true
				}
				else {}
			}
		}
	}
	for i in 1 .. call.children_count {
		arg_id := tc.a.child(&call, i)
		arg := tc.a.nodes[int(arg_id)]
		if !arg.is_mut {
			continue
		}
		param_idx := i - 1 + param_offset
		param := tc.visible_mutation_fn_param(decl, param_idx) or { continue }
		if !param.is_mut {
			continue
		}
		arg_vis := tc.receiver_expr_mutation_visibility(arg_id, root_name, root_type, decl_mod)
		match arg_vis {
			.direct {
				if tc.visible_mutation_fn_param_has_visible_mutation(decl, param_idx, mut visiting) {
					return true
				}
			}
			.public_path {
				return true
			}
			else {}
		}
	}
	return false
}

fn (tc &TypeChecker) node_has_visible_receiver_mutation(id flat.NodeId, root_name string, root_type string, decl_mod string, mut visiting map[u64]bool) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.fn_decl, .fn_literal {
			return false
		}
		.assign {
			lhs_count_value := node.value.int()
			lhs_count := if lhs_count_value > 0 { lhs_count_value } else { 1 }
			rhs_count := int(node.children_count) - lhs_count
			mut child_offset := 0
			for i in 0 .. lhs_count {
				if child_offset >= int(node.children_count) {
					break
				}
				lhs_id := tc.a.child(&node, child_offset)
				if receiver_mutation_is_visible(tc.receiver_expr_mutation_visibility(lhs_id,
					root_name, root_type, decl_mod))
				{
					return true
				}
				child_offset++
				if i < rhs_count {
					child_offset++
				}
			}
		}
		.selector_assign, .index_assign {
			if node.children_count > 0
				&& receiver_mutation_is_visible(tc.receiver_expr_mutation_visibility(tc.a.child(&node, 0), root_name, root_type, decl_mod)) {
				return true
			}
		}
		.postfix {
			if node.op in [.inc, .dec] && node.children_count > 0
				&& receiver_mutation_is_visible(tc.receiver_expr_mutation_visibility(tc.a.child(&node, 0), root_name, root_type, decl_mod)) {
				return true
			}
		}
		.call {
			if tc.call_has_visible_receiver_mutation(id, node, root_name, root_type, decl_mod, mut
				visiting)
			{
				return true
			}
		}
		else {}
	}
	for i in 0 .. node.children_count {
		if tc.node_has_visible_receiver_mutation(tc.a.child(&node, i), root_name, root_type,
			decl_mod, mut visiting)
		{
			return true
		}
	}
	return false
}

fn visible_mutation_cache_id(decl VisibleMutationFnDecl, param_idx int) u64 {
	return u64(u32(decl.idx)) << 32 | u64(u32(param_idx))
}

fn (tc &TypeChecker) cached_visible_mutation_result(key u64) ?bool {
	if !isnil(tc.visible_mutation_cache) {
		cache := tc.visible_mutation_cache
		if key in cache.results {
			return cache.results[key]
		}
	}
	return none
}

fn (tc &TypeChecker) visible_mutation_fn_param_has_visible_mutation(decl VisibleMutationFnDecl, param_idx int, mut visiting map[u64]bool) bool {
	cache_id := visible_mutation_cache_id(decl, param_idx)
	if cached := tc.cached_visible_mutation_result(cache_id) {
		return cached
	}
	if cache_id in visiting {
		return true
	}
	param := tc.visible_mutation_fn_param(decl, param_idx) or {
		tc.cache_visible_mutation_result(cache_id, true)
		return true
	}
	if !param.is_mut {
		tc.cache_visible_mutation_result(cache_id, false)
		return false
	}
	fn_node := tc.a.nodes[decl.idx]
	mut param_count := 0
	for i in 0 .. fn_node.children_count {
		if tc.a.child_node(&fn_node, i).kind != .param {
			break
		}
		param_count++
	}
	if param_count == int(fn_node.children_count) {
		// A source method with `{}` has no visible mutation. Header declarations use
		// `is_mut` as the parser's cached-body marker and remain conservative.
		tc.cache_visible_mutation_result(cache_id, fn_node.is_mut)
		return fn_node.is_mut
	}
	visiting[cache_id] = true
	mut result := false
	for i in param_count .. fn_node.children_count {
		if tc.node_has_visible_receiver_mutation(tc.a.child(&fn_node, i), param.value, param.typ,
			decl.mod, mut visiting)
		{
			result = true
			break
		}
	}
	visiting.delete(cache_id)
	tc.cache_visible_mutation_result(cache_id, result)
	return result
}

fn (tc &TypeChecker) cache_visible_mutation_result(key u64, result bool) {
	if !isnil(tc.visible_mutation_cache) {
		mut cache := tc.visible_mutation_cache
		cache.results[key] = result
	}
}

fn (tc &TypeChecker) mut_receiver_call_requires_mutable_lvalue(info CallInfo, recv_id flat.NodeId) bool {
	if tc.expr_is_shared_arg(recv_id) {
		return false
	}
	if tc.expr_root_is_global_binding(recv_id) {
		return false
	}
	method_module := tc.fn_type_modules[info.name] or { '' }
	if method_module.len > 0 && method_module != tc.cur_module {
		// Match V's private-mutability rule: an immutable binding is accepted across a
		// module boundary only when the method cannot mutate caller-visible state.
		decl := tc.visible_mutation_fn_decl(info.name, method_module) or { return true }
		cache_id := visible_mutation_cache_id(decl, 0)
		if cached := tc.cached_visible_mutation_result(cache_id) {
			return cached
		}
		mut visiting := map[u64]bool{}
		return tc.visible_mutation_fn_param_has_visible_mutation(decl, 0, mut visiting)
	}
	return true
}

fn (tc &TypeChecker) map_builtin_call_info(base_type Type, m Map, method string, mname string) ?CallInfo {
	if !checker_is_raw_collection_method_name(mname, 'map.') {
		return none
	}
	params := match method {
		'delete' {
			tarr2(base_type, m.key_type)
		}
		'clear', 'free', 'keys', 'values' {
			tarr1(base_type)
		}
		'move' {
			tarr1(base_type)
		}
		'reserve' {
			tarr2(base_type, Type(u32_))
		}
		else {
			return none
		}
	}

	return_type := match method {
		'keys' {
			Type(Array{
				elem_type: m.key_type
			})
		}
		'values' {
			Type(Array{
				elem_type: m.value_type
			})
		}
		'move' {
			Type(m)
		}
		else {
			Type(void_)
		}
	}

	return CallInfo{
		name:         mname
		params:       params
		return_type:  return_type
		has_receiver: true
		params_known: true
	}
}

fn checker_is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

// is_print_style_fn_name reports whether is print style fn name applies in types.
fn is_print_style_fn_name(name string) bool {
	mut start := 0
	mut len := name.len
	if len > 8 {
		if len < 13 || len > 16 || !has_builtin_dot_prefix(name) {
			return false
		}
		start = 'builtin.'.len
		len -= start
	}
	return is_short_print_style_fn_name(name, start, len)
}

fn has_builtin_dot_prefix(name string) bool {
	return name[0] == `b` && name[1] == `u` && name[2] == `i` && name[3] == `l` && name[4] == `t`
		&& name[5] == `i` && name[6] == `n` && name[7] == `.`
}

fn is_short_print_style_fn_name(name string, start int, len int) bool {
	return match len {
		5 {
			name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `i`
				&& name[start + 3] == `n` && name[start + 4] == `t`
		}
		6 {
			name[start] == `e` && name[start + 1] == `p` && name[start + 2] == `r`
				&& name[start + 3] == `i` && name[start + 4] == `n` && name[start + 5] == `t`
		}
		7 {
			name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `i`
				&& name[start + 3] == `n` && name[start + 4] == `t` && name[start + 5] == `l`
				&& name[start + 6] == `n`
		}
		8 {
			name[start] == `e` && name[start + 1] == `p` && name[start + 2] == `r`
				&& name[start + 3] == `i` && name[start + 4] == `n` && name[start + 5] == `t`
				&& name[start + 6] == `l` && name[start + 7] == `n`
		}
		else {
			false
		}
	}
}

fn is_builtin_void_call_name(name string) bool {
	if is_short_print_style_fn_name(name, 0, name.len) {
		return true
	}
	return name.len == 5 && name[0] == `p` && name[1] == `a` && name[2] == `n` && name[3] == `i`
		&& name[4] == `c`
}

// print_style_param_accepts_string updates print style param accepts string state for types.
fn print_style_param_accepts_string(typ Type) bool {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean is String
}

// array_insert_prepend_many_arg_compatible reports whether an insert/prepend
// value argument is a many-element operand for the receiver array.
fn (tc &TypeChecker) array_insert_prepend_many_arg_compatible(node flat.Node, info CallInfo, param_idx int, actual Type) bool {
	many_param_idx := array_insert_prepend_many_param_idx(info.name)
	if many_param_idx < 0 || param_idx != many_param_idx {
		return false
	}
	if info.params.len == 0 {
		return false
	}
	mut receiver_type := info.params[0]
	if node.children_count > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			receiver_id := tc.a.child(fn_node, 0)
			receiver_type = tc.cached_expr_type(receiver_id) or { tc.resolve_type(receiver_id) }
		}
	}
	elem_type := array_like_elem_type(unwrap_pointer(receiver_type)) or {
		array_like_elem_type(unwrap_pointer(info.params[0])) or { return false }
	}
	mut clean := actual
	for _ in 0 .. 8 {
		if clean is Alias {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is Array {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	if clean is ArrayFixed {
		return tc.receiver_compatible(clean.elem_type, elem_type)
	}
	return false
}

fn call_param_is_shared(info CallInfo, param_idx int) bool {
	return param_idx >= 0 && param_idx < info.shared_params.len && info.shared_params[param_idx]
}

fn (tc &TypeChecker) expr_is_shared_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.expr_is_shared_arg(tc.a.child(&node, 0))
	}
	if node.kind == .prefix && node.value == 'shared' && node.children_count > 0 {
		return tc.expr_is_shared_arg(tc.a.child(&node, 0))
	}
	if node.kind == .selector && node.children_count > 0 {
		return tc.selector_is_shared_arg(node)
	}
	if node.kind != .ident || node.value.len == 0 {
		return false
	}
	return tc.current_binding_is_shared(node.value)
}

fn (tc &TypeChecker) unlocked_shared_access(id flat.NodeId) ?SharedAccessDiagnostic {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	match node.kind {
		.ident {
			if tc.current_binding_is_shared(node.value)
				&& tc.current_shared_lock_mode(node.value) == 0 {
				return SharedAccessDiagnostic{
					name: node.value
					pos:  tc.node_value_diagnostic_pos(id)
				}
			}
		}
		.paren, .index {
			if node.children_count > 0 {
				return tc.unlocked_shared_access(tc.a.child(node, 0))
			}
		}
		.selector {
			if node.children_count == 0 {
				return none
			}
			if access := tc.unlocked_shared_access(tc.a.child(node, 0)) {
				return access
			}
			if tc.selector_is_shared_arg(node) {
				return SharedAccessDiagnostic{
					name: tc.source_text_for_node(id)
					pos:  tc.selector_field_diagnostic_pos(id, node.value)
				}
			}
		}
		else {}
	}
	return none
}

fn (tc &TypeChecker) expr_is_explicit_shared_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.expr_is_explicit_shared_arg(tc.a.child(&node, 0))
	}
	return node.kind == .prefix && node.value == 'shared' && node.children_count > 0
}

fn (tc &TypeChecker) selector_is_shared_arg(node flat.Node) bool {
	if node.children_count == 0 || node.value.len == 0 {
		return false
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.smartcast_type(base_id) or {
		tc.cached_expr_type(base_id) or { tc.resolve_type(base_id) }
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		return tc.struct_field_is_shared(clean.name, node.value)
	}
	return false
}

fn (tc &TypeChecker) struct_field_is_shared(struct_name string, field_name string) bool {
	if struct_name.len == 0 || field_name.len == 0 {
		return false
	}
	mut candidates := []string{cap: 4}
	candidates << struct_name
	base, _, is_generic := generic_type_application_parts(struct_name)
	if is_generic {
		candidates << base
	}
	if struct_name.contains('.') {
		candidates << struct_name.all_after_last('.')
	} else {
		qname := tc.qualify_name(struct_name)
		if qname != struct_name {
			candidates << qname
		}
	}
	for candidate in candidates {
		if tc.struct_shared_fields[struct_field_c_abi_key(candidate, field_name)] {
			return true
		}
	}
	return false
}

// check_call_arg_types validates check call arg types state for types.
fn (mut tc TypeChecker) check_call_arg_types(id flat.NodeId, node flat.Node, info0 CallInfo) {
	info := tc.specialized_plain_generic_call_info(node, info0)
	if node.children_count == 0 {
		return
	}
	if tc.check_builtin_map_call_args(id, node, info) {
		return
	}
	if is_map_keys_values_call_name(info.name) {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		arg_count := node.children_count - 1
		if arg_count != 0 && tc.should_diagnose(id) {
			tc.record_error(.call_arg_mismatch,
				'argument count mismatch for `${tc.call_display_name(node)}`: expected 0, got ${arg_count}',
				id)
		}
		return
	}
	if !info.params_known {
		dsl_name := tc.unresolved_array_dsl_call_name(node)
		if dsl_name.len > 0 {
			tc.push_array_dsl_scope(node, dsl_name)
		}
		for i in 1 .. node.children_count {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			tc.check_node(arg_id)
			if tc.resolve_type(arg_id) is Void && !tc.errors.any(it.node == arg_id
				&& it.msg.contains('(used before declaration)'))
				&& !tc.expr_subtree_has_undefined_ident_error(arg_id)
				&& !tc.expr_subtree_has_no_value_error(arg_id) {
				tc.record_error(.call_arg_mismatch,
					'`${tc.source_text_for_node(arg_id)}` (no value) used as value in argument ${i} to `${tc.call_display_name(node)}`',
					arg_id)
			}
		}
		if dsl_name.len > 0 {
			tc.pop_scope()
		}
		return
	}
	if tc.check_builtin_array_call_args(id, node, info) {
		return
	}
	// `@[params]` struct args: trailing `key: value` args collapse into one struct argument.
	// field_init args only appear for this syntax, so they are a reliable signal.
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	mut actual_count := node.children_count - 1 - info.arg_offset - field_init_args + collapsed +
		recv_extra
	mut expanded_multi_return_arg := false
	mut expanded_overflow_arg := flat.NodeId(-1)
	mut logical_arg_count := recv_extra
	for i in 1 + info.arg_offset .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		arg_type := tc.cached_expr_type(arg_id) or { tc.resolve_type(arg_id) }
		logical_arg_count++
		arg := tc.a.node(arg_id)
		if !info.is_variadic && arg.kind == .call && arg_type is MultiReturn
			&& !is_print_style_fn_name(info.name) {
			actual_count += arg_type.types.len - 1
			logical_arg_count += arg_type.types.len - 1
			expanded_multi_return_arg = true
		}
		if int(expanded_overflow_arg) < 0 && logical_arg_count > info.params.len {
			expanded_overflow_arg = arg_id
		}
	}
	// A hidden veb `Context` parameter may be supplied implicitly from the
	// enclosing handler instead of by the caller, so accept argument counts both
	// with the ctx (route dispatch) and without it (handler delegation).
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	if field_init_args > 0 && tc.should_diagnose(id) {
		// Trailing `key: value` args collapse into one struct argument; reject
		// them against a parameter that cannot take a struct literal (e.g. a
		// non-variadic `[]Point`), which cgen would otherwise zero-initialize.
		mut first_field := -1
		for i in 1 + info.arg_offset .. node.children_count {
			if tc.a.child_node(&node, i).kind == .field_init {
				first_field = i
				break
			}
		}
		if first_field >= 0 {
			arg_shift := if ctx_omitted { ctx_count } else { 0 }
			param_idx := first_field - 1 - info.arg_offset + recv_extra + arg_shift
			if param_idx >= 0 && param_idx < info.params.len {
				is_variadic_slot := info.is_variadic && param_idx == info.params.len - 1
				mut target := info.params[param_idx]
				if is_variadic_slot {
					if target is Array {
						target = target.elem_type
					}
				}
				clean_target := if target is Alias { target.base_type } else { target }
				if clean_target is Interface {
					first_field_node := tc.a.child_node(&node, first_field)
					tc.record_error_at(.assignment_mismatch,
						'cannot instantiate interface `${clean_target.name.all_after_last('.')}`', tc.a.child(&node,
						first_field), first_field_node.pos)
					return
				}
				if clean_target is Array || clean_target is ArrayFixed || clean_target is Map
					|| clean_target is String || clean_target is Primitive {
					tc.record_error(.call_arg_mismatch,
						'cannot use `key: value` arguments as `${info.params[param_idx].name()}` in call to `${tc.call_display_name(node)}`',
						id)
				}
			}
		}
	}
	min_count := tc.min_required_arg_count(info) - ctx_count
	if info.is_variadic {
		for i in 1 + info.arg_offset .. node.children_count - 1 {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			if spread_id := tc.spread_arg_child(arg_id) {
				spread_pos := tc.a.node(spread_id).pos
				tc.record_error_at(.call_arg_mismatch,
					'when forwarding a variadic variable, it must be the final argument', arg_id, token.new_span(spread_pos.id, int_max(0,
					spread_pos.offset - 3), spread_pos.end))
				return
			}
		}
		variadic_param_idx := info.params.len - 1
		for i in 1 + info.arg_offset .. node.children_count {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			logical_idx := i - 1 - info.arg_offset + recv_extra
			if logical_idx > variadic_param_idx {
				if _ := tc.spread_arg_child(arg_id) {
					tc.record_error_at(.call_arg_mismatch,
						'too many arguments in call to `${tc.call_display_name(node)}`', id,
						node.pos)
					return
				}
			}
		}
	}
	if !info.is_variadic {
		mut seen_spread := false
		for i in 1 + info.arg_offset .. node.children_count {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			if _ := tc.spread_arg_child(arg_id) {
				seen_spread = true
			} else if seen_spread {
				tc.record_error_at(.call_arg_mismatch,
					'cannot have parameter after array decompose', id, node.pos)
				return
			}
		}
		actual_count += tc.nonvariadic_spread_extra_arg_count(node, info, recv_extra)
	}
	if actual_count < min_count || (!info.is_variadic && actual_count > info.params.len) {
		for i in 1 .. node.children_count {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			tc.check_node(arg_id)
			if spread_id := tc.spread_arg_value(arg_id) {
				spread_type := tc.resolve_type(spread_id)
				if array_like_elem_type(unwrap_pointer(spread_type)) == none {
					tc.record_error_at(.call_arg_mismatch,
						'decomposition can only be used on arrays', spread_id,
						tc.a.node(spread_id).pos)
				}
			}
		}
		signature_param_count := if info.is_variadic && info.params.len > 0 {
			info.params.len - 1
		} else {
			c_variadic_fixed_param_count(info)
		}
		expected_count := signature_param_count - recv_extra - ctx_count
		found_count := actual_count - recv_extra
		if expanded_multi_return_arg {
			user_arg_count := node.children_count - 1 - info.arg_offset
			if user_arg_count == 1 && found_count > expected_count {
				arg_id := tc.call_arg_value(tc.a.child(&node, 1 + info.arg_offset))
				multi := tc.resolve_type(arg_id)
				if multi is MultiReturn {
					grammar := if expected_count == 1 { 'argument' } else { 'arguments' }
					tc.record_error_at(.call_arg_mismatch,
						'expected ${expected_count} ${grammar}, but got ${found_count} from multi-return ${Type(multi).name()}',
						arg_id, tc.call_argument_diagnostic_pos(arg_id))
					return
				}
			}
			grammar := if expected_count == 1 { 'argument' } else { 'arguments' }
			pos := if int(expanded_overflow_arg) >= 0 {
				tc.call_argument_diagnostic_pos(expanded_overflow_arg)
			} else {
				node.pos
			}
			tc.record_error_with_details_at(.call_arg_mismatch,
				'expected ${expected_count} ${grammar}, but got ${found_count}', id, pos, tc.call_count_mismatch_details(node,
				info0))
			return
		}
		grammar := if expected_count == 1 { 'argument' } else { 'arguments' }
		is_map_delete := info.name in ['map.delete', 'map__delete']
		pos := if is_map_delete {
			callee := tc.a.child_node(&node, 0)
			if callee.kind == .selector {
				tc.method_call_name_pos(node, callee)
			} else {
				node.pos
			}
		} else if found_count > expected_count {
			tc.surplus_call_args_pos(node, expected_count)
		} else {
			callee := tc.a.child_node(&node, 0)
			if callee.kind == .selector {
				tc.method_call_name_pos(node, callee)
			} else {
				node.pos
			}
		}
		details := if is_map_delete {
			[]string{}
		} else {
			tc.call_count_mismatch_details(node, info0)
		}
		tc.record_error_with_details_at(.call_arg_mismatch,
			'expected ${expected_count} ${grammar}, but got ${found_count}', id, pos, details)
		return
	}
	if info.has_receiver && info.params.len > 0 {
		mut fn_node := tc.a.child_node(&node, 0)
		// For an explicit generic method call `recv.method[T](...)`, the call's fn is an
		// `.index` node (`recv.method` indexed by the type args), so its child 0 is the
		// `recv.method` selector — a method value — not the receiver. Descend through the
		// index to the underlying selector so the receiver resolves to `recv`.
		if fn_node.kind == .index && fn_node.children_count > 0 {
			fn_node = tc.a.child_node(fn_node, 0)
		}
		recv_id := tc.a.child(fn_node, 0)
		tc.check_node(recv_id)
		recv_type := tc.smartcast_type(recv_id) or {
			tc.cached_expr_type(recv_id) or { tc.resolve_type(recv_id) }
		}
		receiver_is_shared_param := call_param_is_shared(info, 0)
		if receiver_is_shared_param && tc.lock_depth > 0 {
			tc.record_error_at(.call_arg_mismatch,
				'method with `shared` receiver cannot be called inside `lock`/`rlock` block',
				recv_id, tc.method_call_name_pos(node, fn_node))
		}
		if !receiver_is_shared_param {
			if access := tc.unlocked_shared_access(recv_id) {
				if tc.mut_receiver_methods[info.name] {
					if tc.lock_depth > 0 {
						tc.record_error_at(.call_arg_mismatch,
							'${access.name} must be added to the `lock` list above', recv_id,
							access.pos)
					}
					tc.record_error_at(.call_arg_mismatch,
						'${access.name} is `shared` and must be `lock`ed to be passed as `mut`',
						recv_id, access.pos)
				} else {
					tc.record_error_at(.call_arg_mismatch,
						'`${access.name}` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut receiver',
						recv_id, access.pos)
					if tc.direct_parent_kind(id) == .decl_assign {
						tc.record_error_at(.assignment_mismatch,
							'`${access.name}` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut right-hand side of assignment',
							recv_id, access.pos)
					}
				}
			}
		}
		receiver_matches := tc.method_receiver_compatible(recv_type, info.params[0], info.name)
			|| tc.receiver_embeds(recv_type, info.params[0])
		if call_param_is_shared(info, 0) && !tc.expr_is_shared_arg(recv_id) {
			if tc.should_diagnose(id) {
				tc.record_error_at(.call_arg_mismatch,
					'cannot use shared method `${fn_node.value}` as `${tc.source_text_for_node(recv_id)}` is not a shared var',
					recv_id, tc.a.node(recv_id).pos)
			}
		}
		if tc.unsafe_depth == 0 && tc.mut_receiver_methods[info.name]
			&& tc.mut_receiver_call_requires_mutable_lvalue(info, recv_id)
			&& !checker_is_raw_collection_method_name(info.name, 'array.')
			&& !tc.mut_receiver_expr_is_mutable_lvalue(recv_id) && tc.should_diagnose(id) {
			if const_name := tc.expr_root_constant_name(recv_id) {
				tc.record_error(.call_arg_mismatch, 'cannot modify constant `${const_name}`', id)
			} else {
				receiver := tc.a.node(recv_id)
				if receiver.kind == .ident {
					tc.record_error_at(.call_arg_mismatch,
						'`${receiver.value}` is immutable, declare it with `mut` to make it mutable',
						recv_id, tc.node_value_diagnostic_pos(recv_id))
				} else {
					tc.record_error_at(.call_arg_mismatch, 'cannot pass expression as `mut`',
						recv_id, tc.mutable_receiver_expression_pos(recv_id))
				}
			}
		}
		if !receiver_matches {
			tc.type_mismatch(.call_arg_mismatch,
				'cannot use receiver `${recv_type.name()}` as `${info.params[0].name()}`', id)
		}
		if method := tc.flag_enum_mutating_receiver_method(fn_node, recv_type, info) {
			if !tc.flag_enum_receiver_is_mutable_lvalue(recv_id) && tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'flag enum method `${method}` requires a mutable receiver', id)
			}
		}
	}
	mut expanded_arg_offset := 0
	for i in 1 + info.arg_offset .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		// field_init args are fields of the collapsed `@[params]` struct, not positional params
		raw_arg := tc.a.child_node(&node, i)
		if raw_arg.kind == .field_init {
			$if ownership ? {
				tc.ownership_check_node_with_deferred_aggregate_consumption(arg_id)
			} $else {
				tc.check_node(arg_id)
			}
			if tc.unsafe_depth == 0 {
				if owner := tc.params_field_owner(raw_arg.value, info) {
					owner_base := strip_generic_args_name(owner)
					decl_mod := tc.struct_modules[owner_base] or { '' }
					if decl_mod.len > 0 && decl_mod != tc.cur_module {
						is_public := tc.visible_mutation_struct_field_is_public(owner,
							raw_arg.value, decl_mod) or { true }
						if !is_public {
							tc.record_error_at(.unknown_field,
								'cannot access private field `${raw_arg.value}` on `${params_field_owner_display(owner)}`', tc.a.child(&node,
								i), raw_arg.pos)
						}
					}
				}
			}
			if expected := tc.params_field_expected_type(raw_arg.value, info) {
				value_node := tc.a.node(arg_id)
				actual := if value_node.kind == .call {
					tc.direct_call_return_type(value_node) or { tc.resolve_type(arg_id) }
				} else {
					tc.resolve_type(arg_id)
				}
				if !tc.type_compatible(actual, expected) && !tc.type_compatible(expected, actual)
					&& !tc.pointer_value_compatible(actual, expected) {
					expected_name := tc.params_field_diagnostic_type(raw_arg.value, info, expected)
					actual_name := if unalias_type(expected) is FnType
						&& unalias_type(actual) is FnType {
						tc.expr_diagnostic_fn_type(arg_id) or {
							tc.diagnostic_expr_type_name(arg_id, actual)
						}
					} else {
						tc.diagnostic_expr_type_name(arg_id, actual)
					}
					tc.record_error_at(.assignment_mismatch,
						'cannot assign to field `${raw_arg.value}`: expected `${expected_name}`, not `${actual_name}`', tc.a.child(&node,
						i), raw_arg.pos)
				}
			}
			continue
		}
		// When the caller omitted the implicit veb `Context` parameter, skip it
		// (it is inserted right after the receiver) while mapping the caller's
		// positional arguments to the callee's params.
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift +
			expanded_arg_offset
		check_arg_id := tc.spread_arg_value(arg_id) or { arg_id }
		has_dsl_scope := tc.call_arg_needs_array_dsl_scope(info.name, param_idx)
		if has_dsl_scope {
			tc.push_array_dsl_scope(node, info.name)
		}
		if !info.is_variadic {
			if spread_id := tc.spread_arg_child(arg_id) {
				actual_spread := tc.resolve_type(spread_id)
				elem_type := array_like_elem_type(unwrap_pointer(actual_spread)) or {
					if has_dsl_scope {
						tc.pop_scope()
					}
					tc.type_mismatch(.call_arg_mismatch,
						'cannot spread `${actual_spread.name()}` as arguments to `${tc.call_display_name(node)}`', id)
					continue
				}
				for spread_param_idx in param_idx .. info.params.len {
					expected := info.params[spread_param_idx]
					if !tc.receiver_compatible(elem_type, expected)
						&& !tc.type_compatible(elem_type, expected) {
						tc.record_error_at(.call_arg_mismatch, 'cannot use `${elem_type.name()}` as `${expected.name()}` in argument ${
							spread_param_idx + 1} to `${tc.call_display_name(node)}`', arg_id, token.new_span(tc.a.node(spread_id).pos.id, int_max(0,
							tc.a.node(spread_id).pos.offset - 3), tc.a.node(spread_id).pos.end))
					}
				}
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
		}
		if param_idx >= 0 && param_idx < info.params.len {
			expected_for_check := tc.call_arg_expected_type(info, param_idx)
			check_node := tc.a.node(check_arg_id)
			if check_node.kind == .array_literal && check_node.children_count == 0
				&& check_node.typ.len == 0 && array_like_elem_type(expected_for_check) != none {
				tc.register_synth_type(check_arg_id, expected_for_check)
			}
		}
		$if ownership ? {
			tc.ownership_check_node_with_aggregate_consumption_mode(check_arg_id, tc.ownership_should_defer_call_arg_aggregate_consumption(node,
				info, i))
		} $else {
			tc.check_node(check_arg_id)
		}
		multi_arg_type := tc.cached_expr_type(check_arg_id) or { tc.resolve_type(check_arg_id) }
		if !info.is_variadic && !is_print_style_fn_name(info.name) && multi_arg_type is MultiReturn
			&& tc.a.node(check_arg_id).kind != .call {
			expected_count := info.params.len - (if info.has_receiver { 1 } else { 0 })
			found_count := node.children_count - 2 - info.arg_offset + multi_arg_type.types.len
			user_arg_count := node.children_count - 1 - info.arg_offset
			if user_arg_count == 1 {
				tc.record_error_at(.call_arg_mismatch,
					'trying to pass ${found_count} argument(s), but function expects ${expected_count} argument(s)',
					id, node.pos)
			} else {
				grammar := if expected_count == 1 { 'argument' } else { 'arguments' }
				tc.record_error_with_details_at(.call_arg_mismatch,
					'expected ${expected_count} ${grammar}, but got ${found_count}', id, node.pos, tc.call_count_mismatch_details(node,
					info0))
			}
			if has_dsl_scope {
				tc.pop_scope()
			}
			return
		}
		if !info.is_variadic && tc.a.node(check_arg_id).kind == .call
			&& multi_arg_type is MultiReturn {
			for multi_idx, actual in multi_arg_type.types {
				multi_param_idx := param_idx + multi_idx
				if multi_param_idx >= info.params.len {
					break
				}
				expected := info.params[multi_param_idx]
				if !tc.receiver_compatible(actual, expected)
					&& !tc.type_compatible(actual, expected) {
					argument_number := multi_param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
					tc.record_error_at(.call_arg_mismatch, 'cannot use `${actual.name()}` as `${expected.name()}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}` from ${Type(multi_arg_type).name()}', check_arg_id,
						tc.call_argument_diagnostic_pos(check_arg_id))
				}
			}
			expanded_arg_offset += multi_arg_type.types.len - 1
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		if tc.check_spread_over_fixed_variadic_tail(id, node, info, i, param_idx) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		if param_idx >= info.params.len {
			if info.is_variadic && info.params.len > 0 {
				variadic_raw := info.params[info.params.len - 1]
				if variadic_raw is Array {
					elem_type := array_elem_type(variadic_raw)
					if variadic_elem_accepts_any(elem_type) && tc.variadic_any_arg_is_scalar(arg_id) {
						if has_dsl_scope {
							tc.pop_scope()
						}
						continue
					}
					actual := tc.resolve_expr(arg_id, elem_type)
					if variadic_elem_accepts_any(elem_type) && !variadic_any_arg_has_value(actual) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${elem_type.name()}`',
							id)
					} else if !tc.receiver_compatible(actual, elem_type)
						&& !tc.type_compatible(actual, elem_type) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${elem_type.name()}`',
							id)
					}
				} else {
					actual := tc.resolve_expr(arg_id, variadic_raw)
					if !tc.receiver_compatible(actual, variadic_raw)
						&& !tc.type_compatible(actual, variadic_raw) {
						tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
							param_idx + 1} to `${tc.call_display_name(node)}`; expected `${variadic_raw.name()}`',
							id)
					}
				}
			}
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		mut expected := info.params[param_idx]
		if tc.is_zero_literal(arg_id) && is_fn_pointer_type(expected) {
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		expected_raw := expected
		if info.is_variadic && param_idx == info.params.len - 1 && expected_raw is Array {
			elem_type := array_elem_type(expected_raw)
			if spread_id := tc.spread_arg_value(arg_id) {
				actual := tc.resolve_type(spread_id)
				spread_pos := tc.a.node(spread_id).pos
				argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
				if unalias_type(unwrap_pointer(actual)) is ArrayFixed {
					tc.record_error_at(.call_arg_mismatch,
						'direct decomposition of fixed array is not allowed, convert the fixed array to normal array via arr[..]',
						spread_id, spread_pos)
					tc.record_error_at(.call_arg_mismatch, '`ast.ArrayDecompose` (no value) used as value in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}`', arg_id, token.new_span(spread_pos.id, int_max(0,
						spread_pos.offset - 3), spread_pos.end))
					if has_dsl_scope {
						tc.pop_scope()
					}
					continue
				}
				if array_like_elem_type(unwrap_pointer(actual)) == none {
					tc.record_error_at(.call_arg_mismatch,
						'decomposition can only be used on arrays', spread_id, spread_pos)
					tc.record_error_at(.call_arg_mismatch, '`ast.ArrayDecompose` (no value) used as value in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}`', arg_id, token.new_span(spread_pos.id, int_max(0,
						spread_pos.offset - 3), spread_pos.end))
					if has_dsl_scope {
						tc.pop_scope()
					}
					continue
				}
				if !tc.variadic_spread_arg_compatible(actual, expected_raw) {
					actual_elem := array_like_elem_type(unwrap_pointer(actual)) or {
						Type(Unknown{})
					}
					actual_display := if unalias_type(elem_type) is Interface {
						actual_elem.name()
					} else {
						'...${actual_elem.name()}'
					}
					expected_display := if unalias_type(elem_type) is Interface {
						elem_type.name()
					} else {
						'...${elem_type.name()}'
					}
					tc.record_error_at(.call_arg_mismatch, 'cannot use `${actual_display}` as `${expected_display}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}`', arg_id, token.new_span(spread_pos.id, int_max(0,
						spread_pos.offset - 3), spread_pos.end))
				}
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			if variadic_elem_accepts_any(elem_type) && tc.variadic_any_arg_is_scalar(arg_id) {
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			actual := tc.resolve_expr(arg_id, elem_type)
			actual_name := actual.name()
			expected_name := elem_type.name()
			actual_raw := actual
			if variadic_elem_accepts_any(elem_type) && !variadic_any_arg_has_value(actual) {
				tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual_name}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected_name}`',
					id)
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			if actual is Array {
				if unalias_type(elem_type) !is Array {
					if tc.call_has_explicit_generic_type_args(node) && !info.has_receiver {
						argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
						tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
							actual)}` as `${call_argument_type_name(elem_type)}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
							info)}`', arg_id, tc.call_argument_diagnostic_pos(arg_id))
					} else {
						arg_text := tc.source_text_for_node(arg_id)
						actual_display_name := tc.current_variadic_param_elem_name(arg_id) or {
							actual_name
						}
						mut target_name := info.name
						mut diagnostic_pos := node.pos
						mut callee := tc.a.child_node(&node, 0)
						if callee.kind == .index && callee.children_count > 0 {
							callee = tc.a.child_node(callee, 0)
						}
						if callee.kind == .selector && callee.children_count > 0 {
							base := tc.a.child_node(callee, 0)
							if base.kind == .ident
								&& tc.source_declares_type_in_scope(base.value, tc.cur_file, tc.cur_module) {
								target_name = '${base.value}__static__${callee.value}'
							}
							if info.has_receiver {
								target_name = callee.value
								name_pos := tc.method_call_name_pos(node, callee)
								diagnostic_pos = token.new_span(name_pos.id, name_pos.offset,
									node.pos.end)
							}
						}
						tc.record_error_at(.call_arg_mismatch,
							'to pass `${arg_text}` (${actual_display_name}) to `${target_name}` (which accepts type `...${elem_type.name()}`), use `...${arg_text}`',
							id, diagnostic_pos)
					}
					if has_dsl_scope {
						tc.pop_scope()
					}
					continue
				}
				if !tc.receiver_compatible(actual_raw, elem_type)
					&& !tc.receiver_compatible(actual_raw, expected)
					&& !tc.type_compatible(actual_raw, elem_type)
					&& !tc.type_compatible(actual_raw, expected) {
					tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual_name}` as argument ${
						param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected_name}`',
						id)
				}
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
			expected = elem_type
		}
		if info.name.starts_with('chan ') && info.name.ends_with('.try_push') && info.has_receiver
			&& param_idx == 1 {
			actual_push := fn_param_unalias_type(tc.resolve_type(arg_id))
			expected_push := fn_param_unalias_type(expected)
			if expected_push is Pointer {
				actual_base := if actual_push is Pointer {
					actual_push.base_type
				} else {
					actual_push
				}
				expected_base := expected_push.base_type
				same_numeric_type := !call_arg_numeric_type(actual_base)
					|| !call_arg_numeric_type(expected_base)
					|| call_argument_type_name(actual_base) == call_argument_type_name(expected_base)
				has_storage := actual_push is Pointer || tc.expr_can_take_address(arg_id)
				if !has_storage || !same_numeric_type
					|| !tc.type_compatible(actual_base, expected_base) {
					argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
					tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
						actual_push)}` as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}`', arg_id, tc.call_argument_diagnostic_pos(arg_id))
				}
			}
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		mut_arg_node := tc.a.node(arg_id)
		if expected !is Pointer && mut_arg_node.kind == .ident
			&& tc.type_has_declaration_attribute(expected, 'nocopy') {
			tc.record_error_at(.call_arg_mismatch,
				'cannot pass @[nocopy] struct by value: use a reference instead', arg_id,
				tc.call_argument_diagnostic_pos(arg_id))
		}
		if mut_arg_node.kind == .map_init && mut_arg_node.children_count == 0
			&& mut_arg_node.value.len == 0 && unalias_type(expected) is Struct {
			argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
			tc.record_error_at(.call_arg_mismatch,
				'`{}` can not be used for initialising empty structs any more. Use `${unalias_type(expected).name()}{}` instead.',
				arg_id, mut_arg_node.pos)
			tc.record_error_at(.call_arg_mismatch, '`map{  }` (no value) used as value in argument ${argument_number} to `${tc.call_argument_target_name(node,
				info)}`', arg_id, mut_arg_node.pos)
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		param_is_mut := tc.call_param_is_mut(info, param_idx)
			|| tc.explicit_generic_source_param_is_mut(node, info, param_idx)
			|| tc.call_field_param_is_mut(node, param_idx)
		implicit_receiver_arg := tc.call_arg_is_callee_receiver(node, arg_id)
			|| tc.call_arg_is_lowered_method_receiver(node, info, param_idx, expected)
		if call_param_is_shared(info, param_idx) && !tc.expr_is_explicit_shared_arg(arg_id) {
			param_name := tc.source_call_param_name(info.name, param_idx) or {
				'${param_idx + 1 - (if info.has_receiver { 1 } else { 0 })}'
			}
			call_kind := if info.has_receiver { 'method' } else { 'function' }
			if tc.lock_depth > 0 {
				tc.record_error_at(.call_arg_mismatch,
					'${call_kind} with `shared` arguments cannot be called inside `lock`/`rlock` block',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
			}
			tc.record_error_at(.call_arg_mismatch, '${call_kind} `${tc.call_argument_target_name(node,
				info).all_after_last('.')}` parameter `${param_name}` is `shared`, so use `shared ${tc.source_text_for_node(arg_id)}` instead',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			if has_dsl_scope {
				tc.pop_scope()
			}
			continue
		}
		if !is_print_style_fn_name(info.name) {
			if access := tc.unlocked_shared_access(arg_id) {
				if param_is_mut && mut_arg_node.is_mut {
					if tc.lock_depth > 0 {
						tc.record_error_at(.call_arg_mismatch,
							'${access.name} must be added to the `lock` list above', arg_id,
							access.pos)
					}
					tc.record_error_at(.call_arg_mismatch,
						'${access.name} is `shared` and must be `lock`ed to be passed as `mut`',
						arg_id, access.pos)
					if has_dsl_scope {
						tc.pop_scope()
					}
					continue
				}
				if !call_param_is_shared(info, param_idx) {
					tc.record_error_at(.call_arg_mismatch,
						'`${access.name}` is `shared` and must be `rlock`ed or `lock`ed to be passed as non-mut argument',
						arg_id, access.pos)
					if has_dsl_scope {
						tc.pop_scope()
					}
					continue
				}
			}
		}
		if param_is_mut && !mut_arg_node.is_mut && !implicit_receiver_arg {
			param_label := if param_name := tc.source_call_param_name(info.name, param_idx) {
				'`${param_name}`'
			} else {
				'${param_idx + 1 - (if info.has_receiver { 1 } else { 0 })}'
			}
			call_kind := if info.has_receiver || tc.a.child_node(&node, 0).kind == .selector {
				'method'
			} else {
				'function'
			}
			target := tc.call_argument_target_name(node, info).all_after_last('.')
			arg_text := tc.source_text_for_node(arg_id)
			if call_kind == 'function' {
				actual_depth, _ := type_pointer_depth_and_base(tc.resolve_type(arg_id))
				expected_depth, _ := type_pointer_depth_and_base(expected)
				tc.record_warning_at(.call_arg_mismatch,
					'automatic referencing/dereferencing is deprecated and will be removed soon (got: ${actual_depth} references, expected: ${expected_depth} references)',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
			}
			tc.record_error_at(.call_arg_mismatch,
				'${call_kind} `${target}` parameter ${param_label} is `mut`, so use `mut ${arg_text}` instead',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			if call_kind == 'function' && mut_arg_node.kind == .ident {
				argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
				actual := tc.resolve_type(arg_id)
				tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
					actual)}` as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
					info)}`', arg_id, tc.call_argument_diagnostic_pos(arg_id))
			}
		}
		if mut_arg_node.is_mut && !param_is_mut {
			if mut_arg_node.kind == .array_literal {
				tc.record_error_at(.call_arg_mismatch, 'array literal can not be modified', arg_id,
					tc.call_argument_diagnostic_pos(arg_id))
				tc.record_error_at(.call_arg_mismatch, 'cannot pass expression as `mut`', arg_id,
					tc.call_argument_diagnostic_pos(arg_id))
			}
			param_label := if param_name := tc.source_call_param_name(info.name, param_idx) {
				'`${param_name}`'
			} else {
				'${param_idx + 1 - (if info.has_receiver { 1 } else { 0 })}'
			}
			target := tc.call_argument_target_name(node, info).all_after_last('.')
			tc.record_error_at(.call_arg_mismatch,
				'`${target}` parameter ${param_label} is not `mut`, `mut` is not needed`', arg_id,
				tc.call_argument_diagnostic_pos(arg_id))
		}
		if mut_arg_node.is_mut && mut_arg_node.kind == .ident && param_is_mut
			&& !tc.ident_is_mutable_lvalue(mut_arg_node.value) {
			tc.record_error_at(.call_arg_mismatch,
				'`${mut_arg_node.value}` is immutable, declare it with `mut` to make it mutable',
				arg_id, tc.node_value_diagnostic_pos(arg_id))
			continue
		}
		if mut_arg_node.is_mut && mut_arg_node.kind == .struct_init
			&& tc.call_param_is_mut(info, param_idx) {
			tc.record_error_at(.call_arg_mismatch,
				'cannot pass a struct initialization as `mut`, you may want to use a variable `mut var := ${mut_arg_node.value}{....}`',
				arg_id, mut_arg_node.pos)
			continue
		}
		requires_mut_pointer_slot := tc.call_param_requires_mut_pointer_slot(info, param_idx)
		if requires_mut_pointer_slot && mut_arg_node.is_mut {
			actual_mut_type := if mut_arg_node.kind == .ident {
				tc.cur_scope.lookup(mut_arg_node.value) or { tc.resolve_type(arg_id) }
			} else {
				tc.resolve_type(arg_id)
			}
			actual_mut_depth, _ := type_pointer_depth_and_base(actual_mut_type)
			mut_param_base := tc.fn_context.mut_param_base_types[mut_arg_node.value] or {
				Type(void_)
			}
			mut_param_base_depth, _ := type_pointer_depth_and_base(mut_param_base)
			is_implicit_mut_param_pointer := mut_arg_node.kind == .ident
				&& mut_arg_node.value in tc.fn_context.mut_param_base_types
				&& actual_mut_depth > mut_param_base_depth
			if is_implicit_mut_param_pointer
				|| !tc.mut_pointer_slot_arg_compatible(actual_mut_type, expected) {
				argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
				tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
					actual_mut_type)}` as `&${call_argument_type_name(expected)}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
					info)}`', arg_id, tc.call_argument_diagnostic_pos(arg_id))
				continue
			}
		}
		if requires_mut_pointer_slot {
			actual_slot := fn_param_unalias_type(tc.resolve_type(arg_id))
			if actual_slot !is Pointer {
				if tc.a.node(arg_id).is_mut
					&& tc.mut_pointer_slot_arg_compatible(actual_slot, expected) {
					// `mut value` supplies the mutable pointer slot for an explicit
					// `mut param &T`; the expression itself still has type `T`.
				} else {
					if has_dsl_scope {
						tc.pop_scope()
					}
					argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
					tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
						actual_slot)}` as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${tc.call_argument_target_name(node,
						info)}`', arg_id, tc.call_argument_diagnostic_pos(arg_id))
					continue
				}
			}
		}
		// Integer arguments are implicitly converted at a concrete call boundary.
		// Resolve this before applying the expected type, since contextual resolution
		// would otherwise diagnose the conversion while resolving the argument itself.
		negative_unsigned_literal := type_is_unsigned_integer(expected)
			&& tc.expr_is_negative_integer_literal(arg_id)
		if !negative_unsigned_literal {
			tc.warn_if_integer_literal_outside_known_type_range(arg_id, expected,
				tc.a.nodes[int(arg_id)].pos)
		}
		if !call_param_is_shared(info, param_idx) && !tc.expr_is_explicit_shared_arg(arg_id)
			&& call_arg_integer_type(expected) && call_arg_integer_type(tc.resolve_type(arg_id)) {
			arg_node := tc.a.node(arg_id)
			if arg_node.kind == .int_literal {
				if has_dsl_scope {
					tc.pop_scope()
				}
				continue
			}
		}
		mut actual := Type(void_)
		if has_dsl_scope {
			actual = tc.resolve_expr(arg_id, expected)
			tc.pop_scope()
		} else {
			actual = tc.resolve_expr(arg_id, expected)
		}
		if actual is Void {
			if !tc.errors.any(it.node == arg_id && it.msg.contains('(used before declaration)'))
				&& !tc.expr_subtree_has_undefined_ident_error(arg_id)
				&& !tc.expr_subtree_has_no_value_error(arg_id) {
				argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
				tc.record_error(.call_arg_mismatch, '`${tc.source_text_for_node(arg_id)}` (no value) used as value in argument ${argument_number} to `${tc.call_argument_target_name(node,
					info)}`', arg_id)
			}
			continue
		}
		if fn_param_is_voidptr_type(expected) && unalias_type(actual) is Struct {
			tc.record_warning_at(.call_arg_mismatch,
				'automatic ${unalias_type(actual).name()} referencing/dereferencing into voidptr is deprecated and will be removed soon; use `foo(&x)` instead of `foo(x)`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
		}
		clean_expected_for_interface := unalias_type(expected)
		if clean_expected_for_interface is Interface && unalias_type(actual) is FnType {
			tc.record_error_at(.call_arg_mismatch,
				'cannot implement interface `${clean_expected_for_interface.name}` using function',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			continue
		}
		argument_number := param_idx + 1 - (if info.has_receiver { 1 } else { 0 })
		target_name := tc.call_argument_target_name(node, info)
		if expected_display := tc.bare_generic_fntype_call_param_display(info.name, param_idx) {
			if unalias_type(actual) is FnType {
				actual_display := call_argument_type_name(actual)
				if actual_display != expected_display {
					tc.record_error_at(.call_arg_mismatch,
						'cannot use `${actual_display}` as `${expected_display}` in argument ${argument_number} to `${target_name}`',
						arg_id, tc.call_argument_diagnostic_pos(arg_id))
					continue
				}
			}
		}
		if fn_param_is_voidptr_type(expected)
			&& tc.a.node(arg_id).kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .string_interp] {
			tc.record_error_at(.call_arg_mismatch, 'expression cannot be passed as `voidptr`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			return
		}
		if negative_unsigned_literal {
			tc.record_error_at(.call_arg_mismatch,
				'cannot use literal signed integer as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${target_name}`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			continue
		}
		if expected is Pointer && !(info.name.starts_with('chan ')
			&& info.name.ends_with('.try_push'))
			&& tc.a.node(arg_id).kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .string_interp] {
			mut reference_name := call_argument_type_name(expected)
			if !info.has_receiver {
				if raw_params := tc.fn_param_type_texts[info.name] {
					if param_idx < raw_params.len && raw_params[param_idx].len > 0 {
						reference_name = raw_params[param_idx]
					}
				}
			}
			if !info.name.starts_with('C.') && reference_name !in ['voidptr', 'byteptr', 'charptr'] {
				tc.record_error_at(.call_arg_mismatch,
					'literal argument cannot be passed as reference parameter `${reference_name}`',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
				continue
			}
		}
		if call_arg_numeric_type(expected) && call_arg_numeric_type(actual)
			&& call_argument_type_name(actual) != call_argument_type_name(expected)
			&& tc.integer_literal_source(arg_id) == none
			&& tc.a.node(arg_id).kind !in [.float_literal, .char_literal] && !(expected is Alias
			&& tc.type_compatible(actual, expected.base_type))
			&& !(unalias_type(actual).is_integer() && unalias_type(expected).is_float()) {
			if info.name.starts_with('chan ') && info.name.ends_with('.try_pop') && param_idx == 1 {
				tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
					actual)}` as argument for `try_pop` (`${call_argument_type_name(expected)}` expected)',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
				continue
			}
			tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
				actual)}` as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${target_name}`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			continue
		}
		arg_node := tc.a.node(arg_id)
		pointer_check_actual := if arg_node.is_mut && arg_node.kind == .ident {
			tc.cur_scope.lookup(arg_node.value) or { actual }
		} else {
			actual
		}
		actual_pointer_depth, actual_pointer_base :=
			type_pointer_depth_and_base(pointer_check_actual)
		expected_pointer_depth, expected_pointer_base := type_pointer_depth_and_base(expected)
		pointer_depth_mismatch := actual_pointer_depth != expected_pointer_depth
			&& expected.name() !in ['voidptr', 'byteptr', 'charptr'] && !(arg_node.is_mut
			&& tc.mut_pointer_slot_arg_compatible(pointer_check_actual, expected))
			&& !(expected_pointer_depth == actual_pointer_depth + 1
			&& tc.expr_can_take_address(arg_id)) && !type_contains_unknown(pointer_check_actual)
			&& !type_contains_unknown(expected) && !tc.call_arg_is_callee_receiver(node, arg_id)
			&& !tc.call_arg_is_lowered_method_receiver(node, info, param_idx, expected)
		pointer_array_mismatch := actual_pointer_depth > 0 && expected_pointer_depth > 0
			&& unalias_type(actual_pointer_base) is Array
			&& unalias_type(expected_pointer_base) is Array
			&& actual_pointer_base.name() != expected_pointer_base.name()
		if pointer_depth_mismatch || pointer_array_mismatch {
			tc.record_error_at(.call_arg_mismatch, 'cannot use `${tc.diagnostic_expr_type_name(arg_id,
				actual)}` as `${call_argument_type_name(expected)}` in argument ${argument_number} to `${target_name}`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			continue
		}
		if expected is OptionType && actual is Pointer
			&& unalias_type(expected.base_type) !is Pointer {
			tc.record_error_at(.call_arg_mismatch,
				'cannot use `&${actual.base_type.name()}` as `?${expected.base_type.name()}` in argument ${argument_number} to `${target_name}`',
				arg_id, tc.call_argument_diagnostic_pos(arg_id))
			continue
		}
		if tc.addressed_bare_generic_value_mismatch(arg_id, actual, expected) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
			continue
		}
		if enum_from_input_param(expected) && !enum_from_input_arg_compatible(actual) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected string or integer', id)
			continue
		}
		param_is_shared := call_param_is_shared(info, param_idx)
		if param_is_shared && !tc.expr_is_shared_arg(arg_id) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch, 'cannot use non-shared `${actual.name()}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `shared ${expected.name()}`',
					id)
			}
			continue
		}
		callee_is_fn_literal := node.children_count > 0
			&& tc.a.child_node(&node, 0).kind in [.fn_literal, .lambda_expr]
		if !param_is_shared && tc.expr_is_explicit_shared_arg(arg_id) && !callee_is_fn_literal {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch, 'cannot use explicit shared argument `${actual.name()}` as argument ${
					param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
					id)
			}
			continue
		}
		if info.name.starts_with('chan ') && info.name.ends_with('.try_pop') && info.has_receiver
			&& param_idx == 1 && mut_arg_node.is_mut
			&& !tc.chan_try_pop_destination_is_valid(arg_id, actual) {
			if tc.should_diagnose(id) {
				tc.record_error(.call_arg_mismatch,
					'channel try_pop destination must be a mutable lvalue or pointer', id)
			}
			continue
		}
		if info.name.starts_with('C.') && fn_param_unalias_type(expected).is_integer()
			&& tc.c_literal_arg(arg_id) && !tc.c_scalar_byte_literal_arg(arg_id) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
			continue
		}
		if !tc.expr_receiver_compatible(arg_id, actual, expected)
			&& !tc.expr_compatible(arg_id, actual, expected) {
			if (tc.call_arg_is_callee_receiver(node, arg_id)
				|| tc.call_arg_is_lowered_method_receiver(node, info, param_idx, expected))
				&& tc.method_receiver_compatible(actual, expected, info.name) {
				continue
			}
			if tc.a.nodes[int(arg_id)].is_mut
				&& tc.mut_optional_pointer_arg_compatible(actual, expected) {
				continue
			}
			if tc.a.nodes[int(arg_id)].is_mut && expected is Pointer
				&& tc.type_compatible(actual, expected.base_type) {
				continue
			}
			if base := tc.mut_param_expr_base(arg_id, actual) {
				if tc.type_compatible(base, expected)
					|| tc.pointer_value_compatible(actual, expected) {
					continue
				}
			}
			if tc.receiver_compatible(actual, expected) {
				continue
			}
			call_name := if info.name.len > 0 { info.name } else { tc.call_display_name(node) }
			if tc.c_call_arg_compatible(call_name, arg_id, expected, actual) {
				continue
			}
			if expected is Pointer && tc.expr_tail_is_nil(arg_id) {
				continue
			}
			if tc.explicit_address_arg_compatible(arg_id, actual, expected) {
				continue
			}
			if tc.explicit_mut_pointer_arg_compatible(arg_id, expected) {
				continue
			}
			if param_is_shared && tc.shared_arg_pointer_compatible(actual, expected) {
				continue
			}
			if json_runtime_voidptr_accepts_arg(call_name, param_idx, expected, actual) {
				continue
			}
			if free_array_arg_compatible(info.name, param_idx, expected, actual) {
				continue
			}
			if voidptr_arg_compatible(expected, actual) {
				continue
			}
			// A `voidptr`/`&void` argument is accepted where a function-pointer type
			// is expected (e.g. a `map[string]EasingFN` whose first literal element is
			// `voidptr(fn)`, making the map value type `voidptr`). voidptr and function
			// pointers are interchangeable in C; matches v1.
			if is_fn_pointer_type(expected) && fn_param_is_voidptr_type(actual) {
				continue
			}
			if !info.name.starts_with('C.') && fn_param_is_voidptr_type(expected)
				&& tc.expr_can_take_address(arg_id) {
				continue
			}
			if fn_param_is_voidptr_type(expected) && info.name.ends_with('Channel.push') {
				continue
			}
			if tc.array_insert_prepend_many_arg_compatible(node, info, param_idx, actual) {
				continue
			}
			if tc.array_dsl_fn_arg_compatible(node, info, param_idx, actual) {
				continue
			}
			if tc.is_os_args_contains_call(node) && param_idx == 1 && actual is String {
				continue
			}
			if actual is OptionType && expected !is OptionType {
				tc.record_error_at(.call_arg_mismatch,
					'cannot use `?${actual.base_type.name()}` as `${expected.name()}`, it must be unwrapped first in argument ${argument_number} to `${target_name}`',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
				continue
			}
			clean_expected := unalias_type(expected)
			if expected_interface := cast_target_interface(clean_expected) {
				if tc.record_interface_implementation_error(.call_arg_mismatch, actual,
					expected_interface, arg_id, tc.call_argument_diagnostic_pos(arg_id))
				{
					continue
				}
			}
			actual_display := tc.diagnostic_expr_type_name(arg_id, actual)
			expected_display := call_argument_type_name(expected)
			message := 'cannot use `${actual_display}` as `${expected_display}` in argument ${argument_number} to `${target_name}`'
			if unalias_type(expected) is FnType && unalias_type(actual) is FnType {
				expected_alias := if expected is Alias { expected.name } else { '' }
				details := tc.fn_assignment_mismatch_details(expected_display, expected_alias,
					actual_display, arg_id)
				if details.len > 0 {
					tc.record_error_with_details_at(.call_arg_mismatch, message, arg_id,
						tc.call_argument_diagnostic_pos(arg_id), details)
					continue
				}
			}
			tc.record_error_at(.call_arg_mismatch, message, arg_id,
				tc.call_argument_diagnostic_pos(arg_id))
		}
	}
}

fn (tc &TypeChecker) params_field_diagnostic_type(field_name string, info CallInfo, expected Type) string {
	for param in info.params {
		param_struct := struct_type_from_type(unwrap_pointer(param)) or { continue }
		if expected_text, _ := tc.struct_field_diagnostic_fn_type(param_struct.name, field_name, 0) {
			semantic_text := call_argument_type_name(expected)
			imports := tc.current_file_import_info()
			if !isnil(imports) {
				mut qualified_text := expected_text
				for alias, module_path in imports.imports {
					prefix := '${module_path}.'
					mut cursor := 0
					for cursor < semantic_text.len {
						relative := semantic_text[cursor..].index(prefix) or { break }
						type_start := cursor + relative + prefix.len
						mut type_end := type_start
						for type_end < semantic_text.len && (semantic_text[type_end].is_alnum()
							|| semantic_text[type_end] == `_`) {
							type_end++
						}
						if type_end > type_start {
							type_name := semantic_text[type_start..type_end]
							qualified_text = qualified_text.replace(type_name,
								'${alias}.${type_name}')
						}
						cursor = type_end
					}
				}
				if qualified_text != expected_text {
					return qualified_text
				}
			}
			return expected_text
		}
	}
	return tc.diagnostic_fntype_alias_display(expected) or { expected.name() }
}

fn (tc &TypeChecker) diagnostic_fntype_alias_display(typ Type) ?string {
	alias := if typ is Alias { typ } else { return none }
	base, args, is_generic := generic_type_application_parts(alias.name)
	lookup := if is_generic { base } else { alias.name }
	raw := tc.source_fn_alias_type_text(lookup) or { return none }
	if !is_generic {
		return tc.diagnostic_fn_type_text(raw)
	}
	params := tc.type_alias_generic_params[lookup] or {
		tc.type_alias_generic_params[tc.qualify_name(lookup)] or { return none }
	}
	if params.len != args.len {
		return tc.diagnostic_fn_type_text(raw)
	}
	return tc.diagnostic_fn_type_text(subst_generic_text(raw, args, params))
}

fn (tc &TypeChecker) bare_generic_fntype_call_param_display(fn_name string, param_idx int) ?string {
	param_texts := tc.fn_param_type_texts[fn_name] or { return none }
	if param_idx < 0 || param_idx >= param_texts.len {
		return none
	}
	name := tc.bare_generic_decl_type_name(param_texts[param_idx]) or { return none }
	qualified := tc.qualify_name(name)
	params := tc.type_alias_generic_params[name] or {
		tc.type_alias_generic_params[qualified] or { return none }
	}
	if params.len == 0 {
		return none
	}
	raw := tc.source_fn_alias_type_text(name) or { return none }
	if unalias_type(tc.parse_type(name)) is FnType {
		return tc.diagnostic_fn_type_text(raw)
	}
	return none
}

fn call_argument_type_name(typ Type) string {
	if typ is Alias {
		clean := unalias_type(typ)
		if clean is FnType {
			return Type(clean).name().replace('fn(', 'fn (').replace(' !void', ' !').replace(' ?void',
				' ?')
		}
	}
	return typ.name().replace('fn(', 'fn (')
}

fn (tc &TypeChecker) call_arg_is_callee_receiver(node flat.Node, arg_id flat.NodeId) bool {
	if node.children_count == 0 || !tc.valid_node_id(arg_id) {
		return false
	}
	mut callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(callee, 0)
	if receiver_id == arg_id {
		return true
	}
	receiver := tc.a.node(receiver_id)
	arg := tc.a.node(arg_id)
	return receiver.pos.is_valid() && arg.pos.is_valid() && receiver.pos.id == arg.pos.id
		&& receiver.pos.offset == arg.pos.offset && receiver.pos.end == arg.pos.end
}

fn (tc &TypeChecker) call_arg_is_lowered_method_receiver(node flat.Node, info CallInfo, param_idx int, expected Type) bool {
	if param_idx != 0 || node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .ident || !info.name.contains('.') {
		return false
	}
	mut receiver_type := fn_param_unalias_type(expected)
	if receiver_type is Pointer {
		receiver_type = fn_param_unalias_type(receiver_type.base_type)
	}
	owner := info.name.all_before_last('.')
	receiver_name := receiver_type.name()
	return owner == receiver_name || owner.ends_with('.${receiver_name}')
		|| receiver_name.ends_with('.${owner}')
}

fn (tc &TypeChecker) current_variadic_param_elem_name(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) || tc.fn_context.node_id < 0 || tc.fn_context.node_id >= tc.a.nodes.len {
		return none
	}
	arg := tc.a.node(id)
	if arg.kind != .ident {
		return none
	}
	fn_node := tc.a.nodes[tc.fn_context.node_id]
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(&fn_node, i)
		if param.kind != .param {
			break
		}
		if param.value == arg.value && param.typ.starts_with('...') {
			return param.typ.trim_left('.')
		}
	}
	return none
}

fn (tc &TypeChecker) call_argument_target_name(node flat.Node, info CallInfo) string {
	if info.name.len > 0 {
		if info.name in ['map.delete', 'map__delete'] {
			return 'Map.delete'
		}
		if node.children_count > 0 {
			mut callee := tc.a.child_node(&node, 0)
			if callee.kind == .index && callee.children_count > 0 {
				callee = tc.a.child_node(callee, 0)
			}
			if callee.kind == .selector && callee.children_count > 0 {
				receiver_id := tc.a.child(callee, 0)
				receiver := tc.a.node(receiver_id)
				if receiver.kind == .ident && tc.binding_is_strings_builder(receiver.value) {
					return info.name.replace('fn(', 'fn (')
				}
				receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
				if receiver_type.name().contains('[') && !type_contains_unknown(receiver_type) {
					return '${receiver_type.name()}.${callee.value}'.replace('fn(', 'fn (')
				}
			}
			if callee.kind == .selector && callee.children_count > 0 && info.name.count('.') == 1 {
				base := tc.a.child_node(callee, 0)
				if base.kind == .ident && tc.ident_resolves_to_value(base.value)
					&& (callee.value in tc.fn_ret_types
					|| tc.qualify_fn_name(callee.value) in tc.fn_ret_types) {
					return callee.value
				}
			}
		}
		return info.name.replace('fn(', 'fn (')
	}
	if node.children_count == 0 {
		return tc.call_display_name(node)
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind in [.fn_literal, .lambda_expr] {
		return 'anon'
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(callee, 0)))
		if base_type is Struct || base_type is Interface {
			return '${base_type.name()}.${callee.value}'
		}
	}
	return tc.call_display_name(node)
}

fn (tc &TypeChecker) call_argument_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	mut unsafe_expr := *node
	for unsafe_expr.kind in [.paren, .expr_stmt] && unsafe_expr.children_count > 0 {
		unsafe_expr = *tc.a.child_node(&unsafe_expr, 0)
	}
	if unsafe_expr.kind == .block && unsafe_expr.value == 'unsafe' {
		return token.new_span(node.pos.id, node.pos.offset,

			int_max(node.pos.end, unsafe_expr.pos.end) + 1)
	}
	if node.kind != .cast_expr || node.children_count == 0 {
		return node.pos
	}
	child := tc.a.child_node(node, 0)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	child_start := int_min(int_max(child.pos.offset, 0), source.len)
	search_start := int_max(0, child_start - node.value.len - 2)
	if relative := source[search_start..child_start].last_index('${node.value}(') {
		start := search_start + relative
		mut end := int_min(source.len, int_max(child.pos.end, child_start))
		for end < source.len && source[end] in [` `, `\t`] {
			end++
		}
		if end < source.len && source[end] == `)` {
			end++
		}
		return token.new_span(node.pos.id, start, end)
	}
	return node.pos
}

fn (tc &TypeChecker) surplus_call_args_pos(node flat.Node, expected_count int) token.Pos {
	first_surplus_child := 1 + expected_count
	if first_surplus_child < 1 || first_surplus_child >= node.children_count {
		return node.pos
	}
	first_id := tc.call_arg_value(tc.a.child(&node, first_surplus_child))
	if spread_id := tc.spread_arg_value(first_id) {
		spread := tc.a.node(spread_id)
		return token.new_span(spread.pos.id, int_max(0, spread.pos.offset - 3), spread.pos.end)
	}
	last_id := tc.call_arg_value(tc.a.child(&node, node.children_count - 1))
	if !tc.valid_node_id(first_id) || !tc.valid_node_id(last_id) {
		return node.pos
	}
	first := tc.a.nodes[int(first_id)]
	last := tc.a.nodes[int(last_id)]
	if first.pos.id != last.pos.id {
		return first.pos
	}
	return token.new_span(first.pos.id, first.pos.offset, last.pos.end)
}

fn (tc &TypeChecker) call_count_mismatch_details(node flat.Node, info CallInfo) []string {
	mut actual_names := []string{}
	for i in 1 + info.arg_offset .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if spread_id := tc.spread_arg_value(arg_id) {
			actual := tc.resolve_type(spread_id)
			if actual is Unknown || actual is Void {
				actual_names << 'void'
				continue
			}
		}
		actual := tc.resolve_type(arg_id)
		actual_names << tc.diagnostic_expr_type_name(arg_id, actual)
	}
	mut expected_names := []string{}
	if source_names := tc.source_call_param_type_names(info.name) {
		param_start := if info.has_receiver && source_names.len == info.params.len { 1 } else { 0 }
		source_end := if info.is_variadic && source_names.len > 0 {
			source_names.len - 1
		} else {
			source_names.len
		}
		for i in param_start .. source_end {
			source_name := source_names[i]
			source_type := if i < info.params.len {
				unalias_type(info.params[i])
			} else {
				unalias_type(tc.parse_type(source_name))
			}
			source_is_interface := source_type is Interface || source_name in tc.interface_names
				|| tc.qualify_name(source_name) in tc.interface_names
				|| tc.interface_names.keys().any(it.all_after_last('.') == source_name)
			if source_is_interface && !source_name.contains('.') {
				raw_mod_name := tc.fn_type_modules[info.name] or {
					if info.name.contains('.') {
						info.name.all_before_last('.')
					} else {
						tc.cur_module
					}
				}
				mod_name := if raw_mod_name.len > 0 { raw_mod_name } else { 'main' }
				expected_names << '${mod_name}.${source_name}'
			} else {
				expected_names << source_name
			}
		}
	} else {
		param_start := if info.has_receiver { 1 } else { 0 }
		param_end := if info.is_variadic && info.params.len > 0 {
			info.params.len - 1
		} else {
			c_variadic_fixed_param_count(info)
		}
		for i in param_start .. param_end {
			param_name := info.params[i].name()
			param_type := unalias_type(info.params[i])
			param_is_interface := param_type is Interface || param_name in tc.interface_names
				|| tc.qualify_name(param_name) in tc.interface_names
				|| tc.interface_names.keys().any(it.all_after_last('.') == param_name)
			if param_is_interface && !param_name.contains('.') {
				raw_mod_name := tc.fn_type_modules[info.name] or {
					if info.name.contains('.') {
						info.name.all_before_last('.')
					} else {
						tc.cur_module
					}
				}
				mod_name := if raw_mod_name.len > 0 { raw_mod_name } else { 'main' }
				expected_names << '${mod_name}.${param_name}'
			} else {
				expected_names << param_name
			}
		}
	}
	return [
		'have (${actual_names.join(', ')})',
		'         want (${expected_names.join(', ')})',
	]
}

fn (tc &TypeChecker) source_call_param_type_names(call_name string) ?[]string {
	if call_name.len == 0 {
		return none
	}
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		if node.kind != .fn_decl
			|| (node.value != call_name && !call_name.ends_with('.${node.value}')) {
			continue
		}
		mut names := []string{}
		for child_index in 0 .. node.children_count {
			param := tc.a.child_node(&node, child_index)
			if param.kind == .param {
				names << param.typ
			}
		}
		if names.len > 0 {
			return names
		}
	}
	return none
}

fn (tc &TypeChecker) source_call_param_name(call_name string, param_idx int) ?string {
	if call_name.len == 0 || param_idx < 0 {
		return none
	}
	if call_name.starts_with('chan ') && call_name.ends_with('.try_pop') && param_idx == 1 {
		return 'obj'
	}
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		if node.kind != .fn_decl
			|| (node.value != call_name && !call_name.ends_with('.${node.value}')) {
			continue
		}
		mut index := 0
		for child_index in 0 .. node.children_count {
			param := tc.a.child_node(&node, child_index)
			if param.kind != .param {
				continue
			}
			if index == param_idx {
				if param.value.len > 0 && param.value != '_' {
					return param.value
				}
				return none
			}
			index++
		}
	}
	return none
}

fn (mut tc TypeChecker) check_builtin_map_call_args(_id flat.NodeId, node flat.Node, info CallInfo) bool {
	if node.children_count == 0 || info.arg_offset != 0 {
		return false
	}
	mut callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(callee, 0)
	receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
	if receiver_type !is Map {
		return false
	}
	if info.name.len > 0 && !checker_is_raw_collection_method_name(info.name, 'map.') {
		return false
	}
	method := callee.value
	if method !in ['clone', 'move', 'keys', 'values'] {
		return false
	}
	for i in 1 .. node.children_count {
		tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
	}
	if node.children_count > 1 {
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.record_error(.call_arg_mismatch, '`.${method}()` does not have any arguments', arg_id)
	} else if method == 'move' {
		tc.check_builtin_array_mutable_receiver(receiver_id)
	}
	return true
}

fn (mut tc TypeChecker) check_builtin_array_call_args(id flat.NodeId, node flat.Node, info CallInfo) bool {
	if node.children_count == 0 || info.arg_offset != 0 {
		return false
	}
	mut callee := tc.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	receiver_id := tc.a.child(callee, 0)
	receiver_type := tc.resolve_type(receiver_id)
	array_type := array_like_type_for_method(unalias_and_unwrap_pointer_type(receiver_type),
		callee.value) or { return false }
	if info.name.len > 0 && !checker_is_raw_collection_method_name(info.name, 'array.') {
		return false
	}
	method := callee.value
	explicit_count := int(node.children_count) - 1
	if method in ['clone', 'reverse', 'first', 'last', 'pop', 'pop_left'] {
		for i in 1 .. node.children_count {
			tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
		}
		if explicit_count > 0 {
			arg_id := tc.call_arg_value(tc.a.child(&node, 1))
			tc.record_error(.call_arg_mismatch, '`.${method}()` does not have any arguments',
				arg_id)
		}
		if method in ['pop', 'pop_left'] {
			tc.check_builtin_array_mutable_receiver(receiver_id)
		}
		return true
	}
	if method == 'delete' {
		if explicit_count != 1 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.record_error_at(.call_arg_mismatch,
				'`.delete()` expected 1 argument, but got ${explicit_count}', id, tc.method_call_name_pos(node,
				callee))
			tc.check_builtin_array_mutable_receiver(receiver_id)
			return true
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.check_node_with_expected_context(arg_id, Type(int_))
		actual := tc.resolve_expr(arg_id, Type(int_))
		if !tc.expr_compatible(arg_id, actual, Type(int_)) {
			tc.record_error(.call_arg_mismatch,
				'cannot use `${actual.name()}` as `int` in argument 1 to `.delete()`', arg_id)
		}
		tc.check_builtin_array_mutable_receiver(receiver_id)
		return true
	}
	if method == 'insert' {
		if explicit_count != 2 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.record_error_at(.call_arg_mismatch,
				'`array.insert()` should have 2 arguments, e.g. `insert(1, val)`', id, tc.method_call_name_pos(node,
				callee))
			return true
		}
		index_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.check_node_with_expected_context(index_id, Type(int_))
		index_type := tc.resolve_expr(index_id, Type(int_))
		index_is_valid := tc.expr_compatible(index_id, index_type, Type(int_))
		if !index_is_valid {
			tc.record_error(.call_arg_mismatch,
				'the first argument of `array.insert()` should be integer', index_id)
		}
		value_id := tc.call_arg_value(tc.a.child(&node, 2))
		tc.check_node_with_expected_context(value_id, array_type.elem_type)
		value_type := tc.resolve_expr(value_id, array_type.elem_type)
		value_is_valid := tc.array_insert_value_compatible(value_id, value_type, array_type,
			receiver_type)
		if index_is_valid && !value_is_valid {
			clean_value := unalias_type(value_type)
			if clean_value is OptionType {
				tc.record_error(.call_arg_mismatch,
					'cannot use `${value_type.name()}` as `voidptr`, it must be unwrapped first in argument 2 to `${receiver_type.name()}.insert`',
					value_id)
			} else {
				value_name := tc.array_insert_value_type_name(value_id, value_type)
				tc.record_error(.call_arg_mismatch,
					'cannot insert `${value_name}` to `${receiver_type.name()}`', value_id)
			}
		}
		if index_is_valid && value_is_valid {
			tc.check_builtin_array_mutable_receiver(receiver_id)
		}
		return true
	}
	if method == 'prepend' {
		if explicit_count != 1 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.record_error_at(.call_arg_mismatch,
				'`array.prepend()` should have 1 argument, e.g. `prepend(val)`', id, tc.method_call_name_pos(node,
				callee))
			return true
		}
		value_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.check_node_with_expected_context(value_id, array_type.elem_type)
		value_type := tc.resolve_expr(value_id, array_type.elem_type)
		value_is_valid := tc.array_insert_value_compatible(value_id, value_type, array_type,
			receiver_type)
		if !value_is_valid {
			clean_value := unalias_type(value_type)
			if clean_value is OptionType {
				tc.record_error(.call_arg_mismatch,
					'cannot use `${value_type.name()}` as `voidptr`, it must be unwrapped first in argument 1 to `${receiver_type.name()}.prepend`',
					value_id)
			} else {
				value_name := tc.array_insert_value_type_name(value_id, value_type)
				tc.record_error(.call_arg_mismatch,
					'cannot prepend `${value_name}` to `${receiver_type.name()}`', value_id)
			}
		}
		if value_is_valid {
			tc.check_builtin_array_mutable_receiver(receiver_id)
		}
		return true
	}
	if method in ['index', 'last_index', 'contains'] {
		if explicit_count != 1 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.record_error_at(.call_arg_mismatch,
				'`.${method}()` expected 1 argument, but got ${explicit_count}', id, tc.method_call_name_pos(node,
				callee))
			return true
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.check_node_with_expected_context(arg_id, array_type.elem_type)
		actual := tc.resolve_expr(arg_id, array_type.elem_type)
		if !tc.expr_compatible(arg_id, actual, array_type.elem_type) {
			if unalias_type(actual) is OptionType {
				tc.record_error_at(.call_arg_mismatch,
					'cannot use `${actual.name()}` as `${array_type.elem_type.name()}`, it must be unwrapped first in argument 1 to `.${method}()`',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
			} else {
				tc.record_error_at(.call_arg_mismatch,
					'cannot use `${actual.name()}` as `${array_type.elem_type.name()}` in argument 1 to `.${method}()`',
					arg_id, tc.call_argument_diagnostic_pos(arg_id))
			}
		}
		return true
	}
	if method in ['sort_with_compare', 'sorted_with_compare'] {
		if method == 'sort_with_compare' {
			tc.check_builtin_array_mutable_receiver(receiver_id)
		}
		if explicit_count != 1 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			name_pos := tc.method_call_name_pos(node, callee)
			tc.record_error_at(.call_arg_mismatch,
				'`.${method}()` expected 1 argument, but got ${explicit_count}', id, token.new_span(name_pos.id,
				name_pos.offset, node.pos.end))
			return true
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.check_node(arg_id)
		actual := tc.resolve_type(arg_id)
		expected := Type(FnType{
			params:      [
				Type(Pointer{
					base_type: array_type.elem_type
				}),
				Type(Pointer{
					base_type: array_type.elem_type
				}),
			]
			return_type: Type(int_)
		})
		if actual_fn := fn_type_from_type(actual) {
			tc.record_array_compare_callback_param_errors(method, arg_id, array_type.elem_type)
			if tc.type_compatible(actual_fn, expected) {
				return true
			}
			if unalias_type(array_type.elem_type) is Pointer {
				return true
			}
		}
		actual_name := tc.diagnostic_expr_type_name(arg_id, actual).replace_once('fn(', 'fn (')
		target := '[]${array_type.elem_type.name()}.${method}'
		message := 'cannot use `${actual_name}` as `fn (voidptr, voidptr) int` in argument 1 to `${target}`'
		if actual_fn := fn_type_from_type(actual) {
			if actual_fn.params.len > 0 && unalias_type(actual_fn.params[0]) !is Pointer {
				tc.record_error_with_details_at(.call_arg_mismatch, message, arg_id,
					tc.a.node(arg_id).pos, [
					"`FnSortCB`'s expected argument `const_a` to be a pointer, but the passed argument `a` is NOT a pointer",
				])
				return true
			}
		}
		tc.record_error_at(.call_arg_mismatch, message, arg_id, tc.a.node(arg_id).pos)
		return true
	}
	if method == 'map' {
		if explicit_count != 1 {
			tc.push_array_dsl_scope(node, 'array.map')
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			tc.pop_scope()
			tc.record_error_at(.call_arg_mismatch,
				'expected 1 argument, but got ${explicit_count}', id, tc.method_call_name_pos(node,
				callee))
			return true
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.push_array_dsl_scope(node, 'array.map')
		error_count := tc.errors.len
		tc.check_node(arg_id)
		arg_type := tc.resolve_type(arg_id)
		tc.pop_scope()
		if fn_type := fn_type_from_type(arg_type) {
			if fn_type.return_type is MultiReturn {
				tc.record_error_at(.call_arg_mismatch,
					'returning multiple values is not supported in .map() calls', arg_id, tc.method_call_name_pos(node,
					callee))
				return true
			}
			if fn_type.return_type is Void {
				arg := tc.a.node(arg_id)
				if arg.kind == .lambda_expr && arg.children_count > 0 {
					body_id := tc.a.child(arg, arg.children_count - 1)
					body := tc.a.node(body_id)
					if body.kind == .call {
						tc.record_error_at(.call_arg_mismatch,
							'type mismatch, `${tc.call_display_name(*body)}` does not return anything',
							body_id, body.pos)
						return true
					}
				}
			}
			if fn_type.return_type is ResultType {
				tc.record_error_at(.call_arg_mismatch, 'cannot use Result type in `map`', arg_id,
					tc.a.node(arg_id).pos)
				return true
			}
			if fn_type.return_type is OptionType {
				pos := if tc.expr_is_direct_fn_value(arg_id) {
					tc.method_call_name_pos(node, callee)
				} else {
					tc.a.nodes[int(arg_id)].pos
				}
				tc.record_error_at(.call_arg_mismatch,
					'option needs to be unwrapped before using it in map/filter', arg_id, pos)
			}
			if fn_type.params.len != 1 {
				pos := if tc.a.nodes[int(arg_id)].kind == .ident {
					tc.method_call_name_pos(node, callee)
				} else {
					tc.a.nodes[int(arg_id)].pos
				}
				message := if tc.a.nodes[int(arg_id)].kind == .lambda_expr {
					'lambda expressions used in the builtin array methods require exactly 1 parameter'
				} else {
					'function needs exactly 1 argument'
				}
				tc.record_error_at(.call_arg_mismatch, message, arg_id, pos)
				tc.register_synth_type(id, Type(void_))
				tc.record_enclosing_dump_void(id)
			} else if !tc.type_compatible(fn_type.params[0], array_type.elem_type)
				|| fn_type.return_type is Void {
				tc.record_error(.call_arg_mismatch,
					'type mismatch, should use `fn(a ${array_type.elem_type.name()}) T {...}`',
					arg_id)
			}
		} else if result_pos := tc.array_map_result_propagation_pos(arg_id) {
			tc.record_error_at(.call_arg_mismatch, 'cannot use Result type in `map`', arg_id,
				result_pos)
		} else if arg_type is Void && tc.errors.len == error_count {
			arg := tc.a.nodes[int(arg_id)]
			name := if arg.kind == .call { tc.call_display_name(arg) } else { 'expression' }
			tc.record_error(.call_arg_mismatch,
				'type mismatch, `${name}` does not return anything', arg_id)
		}
		return true
	}
	if method in ['any', 'all', 'filter', 'count'] {
		if explicit_count != 1 {
			for i in 1 .. node.children_count {
				tc.check_node(tc.call_arg_value(tc.a.child(&node, i)))
			}
			message := if method in ['any', 'all'] {
				'`.${method}` expected 1 argument, but got ${explicit_count}'
			} else {
				'expected 1 argument, but got ${explicit_count}'
			}
			tc.record_error_at(.call_arg_mismatch, message, id, tc.method_call_name_pos(node,
				callee))
			return true
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, 1))
		tc.push_array_dsl_scope(node, 'array.${method}')
		tc.check_node(arg_id)
		arg := tc.a.nodes[int(arg_id)]
		arg_type := tc.resolve_type(arg_id)
		tc.pop_scope()
		if fn_type := fn_type_from_type(arg_type) {
			if fn_type.return_type is OptionType {
				tc.record_error_at(.call_arg_mismatch,
					'option needs to be unwrapped before using it in map/filter', arg_id, tc.method_call_name_pos(node,
					callee))
			}
			if fn_type.params.len != 1 {
				pos := if arg.kind == .ident {
					tc.method_call_name_pos(node, callee)
				} else {
					arg.pos
				}
				tc.record_error_at(.call_arg_mismatch, 'function needs exactly 1 argument', arg_id,
					pos)
			} else if !tc.type_compatible(fn_type.params[0], array_type.elem_type)
				|| !tc.type_compatible(fn_type.return_type, Type(bool_)) {
				tc.record_error(.call_arg_mismatch,
					'type mismatch, should use `fn(a ${array_type.elem_type.name()}) bool {...}`',
					arg_id)
			}
		} else if arg.kind in [.string_literal, .string_interp] {
			tc.record_error(.call_arg_mismatch,
				'type mismatch, should use e.g. `${method}(it > 2)`', arg_id)
		} else {
			actual := tc.resolve_expr(arg_id, Type(bool_))
			if !tc.expr_compatible(arg_id, actual, Type(bool_)) {
				clean_actual := unalias_type(actual)
				if clean_actual is OptionType
					&& unalias_type(clean_actual.base_type).name() == 'bool' && arg.kind == .call {
					tc.record_error(.call_arg_mismatch,
						'type mismatch, `${tc.call_display_name(arg)}` must return a bool', arg_id)
				} else if arg.kind == .call {
					tc.record_error(.call_arg_mismatch,
						'type mismatch, `${tc.call_display_name(arg)}` must return a bool', arg_id)
				} else {
					tc.record_error(.call_arg_mismatch,
						'invalid expression, expected infix expr, lambda or function', arg_id)
				}
			}
		}
		return true
	}
	if method in ['clear', 'delete_last', 'reverse_in_place', 'sort', 'sort_with_compare'] {
		tc.check_builtin_array_mutable_receiver(receiver_id)
	}
	return false
}

fn (tc &TypeChecker) array_map_result_propagation_pos(id flat.NodeId) ?token.Pos {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind != .or_expr || node.value != '!' || node.children_count == 0 {
		return none
	}
	source_id := tc.a.child(node, 0)
	source_type := unalias_type(tc.resolve_type(source_id))
	if source_type is ResultType && unalias_type(source_type.base_type) is Void {
		source := tc.a.node(source_id)
		if source.kind == .call && source.children_count > 0 {
			callee := tc.a.child_node(source, 0)
			if callee.kind == .selector {
				name_pos := tc.method_call_name_pos(*source, *callee)
				return token.new_span(name_pos.id, name_pos.offset, node.pos.end)
			}
		}
		return source.pos
	}
	return none
}

fn (mut tc TypeChecker) record_array_compare_callback_param_errors(method string, arg_id flat.NodeId, elem_type Type) {
	arg := tc.a.node(arg_id)
	if arg.kind != .ident {
		return
	}
	expected := Type(Pointer{
		base_type: elem_type
	})
	for idx in tc.top_level_idx {
		fn_node := tc.a.nodes[idx]
		if fn_node.kind != .fn_decl
			|| (fn_node.value != arg.value && !fn_node.value.ends_with('.${arg.value}')) {
			continue
		}
		mut param_index := 0
		for i in 0 .. fn_node.children_count {
			param_id := tc.a.child(&fn_node, i)
			param := tc.a.node(param_id)
			if param.kind != .param {
				break
			}
			if param_index < 2 {
				actual := tc.parse_type(param.typ)
				if !tc.method_param_signature_compatible(actual, expected) {
					tc.record_error_at(.call_arg_mismatch,
						'${method} callback function parameter `${param.value}` with type `${actual.name()}` should be `${expected.name()}`',
						param_id, tc.type_diagnostic_pos(param_id, param.typ))
				}
			}
			param_index++
		}
		return
	}
}

fn (mut tc TypeChecker) check_array_sort_call(id flat.NodeId, node flat.Node, callee flat.Node) {
	explicit_count := int(node.children_count) - 1
	if explicit_count > 1 {
		tc.record_error_at(.call_arg_mismatch,
			'expected 0 or 1 argument, but got ${explicit_count}', id, tc.method_call_name_pos(node,
			callee))
		return
	}
	if explicit_count == 0 {
		receiver_id := tc.a.child(&callee, 0)
		receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
		if array_type := array_like_type_for_method(receiver_type, 'sort') {
			elem_type := unalias_type(array_type.elem_type)
			if elem_type is Struct && '${elem_type.name}.<' !in tc.fn_ret_types {
				tc.record_error_at(.call_arg_mismatch,
					'custom sorting condition must be supplied for type `${elem_type.name}`', id, tc.method_call_name_pos(node,
					callee))
			}
		}
		return
	}
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	arg := tc.a.nodes[int(arg_id)]
	method_pos := tc.method_call_name_pos(node, callee)
	if arg.kind != .infix {
		tc.record_error_at(.call_arg_mismatch, '`.sort()` can only use `<` or `>` comparison', id,
			method_pos)
		tc.push_array_dsl_scope(node, 'array.sort')
		tc.check_node(arg_id)
		tc.pop_scope()
		return
	}
	if arg.op !in [.lt, .gt] {
		tc.record_error_at(.call_arg_mismatch, '`.sort()` can only use `<` or `>` comparison', id,
			method_pos)
	}
	lhs_id := tc.a.child(&arg, 0)
	rhs_id := tc.a.child(&arg, 1)
	lhs_key := tc.expr_key(lhs_id)
	if lhs_key.len > 0 && lhs_key == tc.expr_key(rhs_id) {
		tc.record_error_at(.call_arg_mismatch, '`.sort()` cannot use same argument', id, method_pos)
	}
	if invalid_id := tc.sort_first_invalid_ident(arg_id) {
		invalid := tc.a.nodes[int(invalid_id)]
		tc.record_error_at(.unknown_ident, 'can not access external variable `${invalid.value}`',
			invalid_id, tc.node_value_diagnostic_pos(invalid_id))
		if tc.cur_scope.lookup(invalid.value) == none {
			tc.record_error_at(.call_arg_mismatch,
				'`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`', id,
				method_pos)
		}
	} else if !tc.sort_operand_has_valid_shape(lhs_id) || !tc.sort_operand_has_valid_shape(rhs_id) {
		tc.record_error_at(.call_arg_mismatch,
			'`.sort()` can only use `a` or `b` as argument, e.g. `arr.sort(a < b)`', id, method_pos)
		tc.record_error_at(.call_arg_mismatch,
			'`.sort()` can only use ident, index, selector or call as argument,\ne.g. `arr.sort(a < b)`, `arr.sort(a.id < b.id)`, `arr.sort(a[0] < b[0])`',
			id, method_pos)
	}
	tc.push_array_dsl_scope(node, 'array.sort')
	tc.check_node(arg_id)
	if invalid_id := tc.sort_first_invalid_ident(arg_id) {
		invalid := tc.a.node(invalid_id)
		mut error_index := tc.errors.len
		for error_index > 0 {
			error_index--
			diagnostic := tc.errors[error_index]
			if diagnostic.msg.starts_with('non-integer index `void`')
				&& diagnostic.pos.id == invalid.pos.id && diagnostic.pos.offset >= arg.pos.offset
				&& diagnostic.pos.end <= arg.pos.end {
				tc.errors.delete(error_index)
			}
		}
	}
	lhs_type := tc.resolve_type(lhs_id)
	rhs_type := tc.resolve_type(rhs_id)
	tc.pop_scope()
	if lhs_type.name().starts_with('thread ') && rhs_type !is Unknown
		&& !tc.type_compatible(rhs_type, lhs_type) {
		tc.record_error(.call_arg_mismatch,
			'infix expr: cannot use `${rhs_type.name()}` (right expression) as `thread ${rhs_type.name()}`',
			arg_id)
	}
}

fn (tc &TypeChecker) sort_first_invalid_ident(id flat.NodeId) ?flat.NodeId {
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value !in ['a', 'b'] {
		return id
	}
	for i in 0 .. node.children_count {
		if invalid := tc.sort_first_invalid_ident(tc.a.child(&node, i)) {
			return invalid
		}
	}
	return none
}

fn (tc &TypeChecker) sort_operand_has_valid_shape(id flat.NodeId) bool {
	mut node := tc.a.nodes[int(id)]
	for node.kind == .paren && node.children_count > 0 {
		node = tc.a.nodes[int(tc.a.child(&node, 0))]
	}
	return node.kind in [.ident, .index, .selector, .call]
}

fn (tc &TypeChecker) expr_is_direct_fn_value(id flat.NodeId) bool {
	node := tc.a.nodes[int(id)]
	if tc.fn_value_shadowed_by_value(node) {
		return false
	}
	if _ := tc.fn_value_key(node) {
		return true
	}
	return false
}

fn (tc &TypeChecker) array_insert_value_compatible(value_id flat.NodeId, value_type Type, array_type Array, receiver_type Type) bool {
	clean_value := unalias_type(value_type)
	clean_elem := unalias_type(array_type.elem_type)
	if clean_elem is ArrayFixed
		&& (clean_value is Array || tc.a.nodes[int(value_id)].kind == .array_literal) {
		return false
	}
	return tc.expr_compatible(value_id, value_type, array_type.elem_type)
		|| tc.type_compatible(value_type, receiver_type)
}

fn (tc &TypeChecker) array_insert_value_type_name(value_id flat.NodeId, value_type Type) string {
	value := tc.a.nodes[int(value_id)]
	if value.kind == .array_literal {
		return '[]${tc.array_literal_elem_type(value).name()}'
	}
	return tc.diagnostic_expr_type_name(value_id, value_type)
}

fn (mut tc TypeChecker) check_builtin_array_mutable_receiver(receiver_id flat.NodeId) {
	if !tc.valid_node_id(receiver_id) || tc.mut_receiver_expr_is_mutable_lvalue(receiver_id) {
		return
	}
	receiver := tc.a.nodes[int(receiver_id)]
	if receiver.kind != .ident || receiver.value.len == 0 {
		if tc.should_diagnose(receiver_id) {
			tc.record_error_at(.call_arg_mismatch, 'cannot pass expression as `mut`', receiver_id,
				tc.mutable_receiver_expression_pos(receiver_id))
		}
		return
	}
	tc.record_error(.call_arg_mismatch,
		'`${receiver.value}` is immutable, declare it with `mut` to make it mutable', receiver_id)
}

fn (tc &TypeChecker) mutable_receiver_expression_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .selector {
			return tc.method_call_name_pos(*node, *callee)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) method_call_name_pos(call flat.Node, selector flat.Node) token.Pos {
	file := tc.a.source_files[selector.pos.id] or { return selector.pos }
	source := tc.source_texts_by_file[file.name] or { return selector.pos }
	mut start := int_max(0, selector.pos.offset)
	if selector.children_count > 0 {
		receiver := tc.a.child_node(&selector, 0)
		start = int_max(start, receiver.pos.end)
	}
	mut end := int_min(call.pos.end, source.len)
	if end <= start {
		end = int_min(source.len, selector.pos.end + 2)
	}
	if start < end {
		if relative := source[start..end].index(selector.value) {
			name_start := start + relative
			if open_relative := source[name_start..end].index('(') {
				mut depth := 0
				for index := name_start + open_relative; index < end; index++ {
					if source[index] == `(` {
						depth++
					} else if source[index] == `)` {
						depth--
						if depth == 0 {
							return token.new_span(selector.pos.id, name_start, index + 1)
						}
					}
				}
			}
		}
	}
	return tc.node_value_diagnostic_pos(tc.a.child(&call, 0))
}

fn (tc &TypeChecker) mut_optional_pointer_arg_compatible(actual Type, expected Type) bool {
	if actual !is OptionType {
		return false
	}
	actual_option := actual as OptionType
	if expected !is Pointer {
		return false
	}
	expected_pointer := expected as Pointer
	actual_payload := actual_option.base_type
	expected_option_type := expected_pointer.base_type
	if actual_payload !is Pointer {
		return false
	}
	actual_pointer := actual_payload as Pointer
	if expected_option_type !is OptionType {
		return false
	}
	expected_option := expected_option_type as OptionType
	return tc.type_compatible(actual_pointer.base_type, expected_option.base_type)
}

fn enum_from_input_param(typ Type) bool {
	return typ is Unknown && typ.reason == 'enum from input'
}

fn (tc &TypeChecker) is_generic_enum_from_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'from' || fn_node.children_count == 0 {
		return false
	}
	base_node := tc.a.child_node(fn_node, 0)
	return base_node.kind == .ident && base_node.value in tc.fn_context.generic_params
}

fn enum_from_input_arg_compatible(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	return clean is Unknown || clean is String || clean.is_integer()
}

fn (tc &TypeChecker) shared_arg_pointer_compatible(actual Type, expected Type) bool {
	clean := fn_param_unalias_type(actual)
	if clean is Pointer {
		return tc.type_compatible(clean.base_type, expected)
	}
	return false
}

fn (tc &TypeChecker) unresolved_array_dsl_call_name(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return ''
	}
	name := 'array.${callee.value}'
	return if is_array_dsl_call_name(name) { name } else { '' }
}

fn (tc &TypeChecker) chan_try_pop_destination_is_valid(arg_id flat.NodeId, actual Type) bool {
	if int(arg_id) < 0 || int(arg_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(arg_id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.chan_try_pop_destination_is_valid(tc.a.child(&node, 0), actual)
	}
	if node.kind == .prefix && node.op == .amp {
		if node.children_count == 0 {
			return false
		}
		child_id := tc.a.child(&node, 0)
		return tc.expr_can_take_address(child_id) && tc.expr_root_is_mutable_lvalue(child_id)
	}
	if actual is Pointer {
		return true
	}
	return tc.expr_can_take_address(arg_id) && tc.expr_root_is_mutable_lvalue(arg_id)
}

fn json_runtime_voidptr_accepts_arg(name string, param_idx int, expected Type, actual Type) bool {
	if param_idx != 0 || name !in ['json.decode', 'json.encode', 'json.encode_pretty'] {
		return false
	}
	if expected is Pointer {
		if expected.base_type is Void {
			return type_has_runtime_value(actual)
		}
	}
	return false
}

fn free_array_arg_compatible(name string, param_idx int, expected Type, actual Type) bool {
	if param_idx != 0 || name !in ['free', 'builtin.free'] || !fn_param_is_voidptr_type(expected) {
		return false
	}
	mut clean := unwrap_pointer(actual)
	if clean is Alias {
		clean = clean.base_type
	}
	return clean is Array
}

fn (tc &TypeChecker) c_call_arg_compatible(name string, arg_id flat.NodeId, expected Type, actual Type) bool {
	if !name.starts_with('C.') {
		return false
	}
	clean := fn_param_unalias_type(expected)
	if clean.is_integer() {
		actual_clean := fn_param_unalias_type(actual)
		return actual_clean.is_integer()
			|| (actual_clean is Primitive && actual_clean.props.has(.boolean))
			|| tc.c_scalar_byte_literal_arg(arg_id)
	}
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		if base is Char || (base is Primitive && base.name() == 'u8') {
			return tc.c_literal_arg(arg_id)
		}
		// C APIs commonly spell an opaque pointer parameter as `voidptr` and
		// accept a V struct value as storage (for example gg's native drawing
		// config structs). Scalars still require an explicit `voidptr(...)` cast.
		if base is Void && fn_param_unalias_type(actual) is Struct {
			return tc.expr_can_take_address(arg_id)
		}
	}
	return false
}

fn (tc &TypeChecker) c_scalar_byte_literal_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .char_literal {
		return c_literal_is_single_byte(node.value)
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.c_scalar_byte_literal_arg(tc.a.child(&node, 0))
	}
	return false
}

fn c_literal_is_single_byte(value string) bool {
	if !value.starts_with('c:') {
		return false
	}
	literal := value[2..]
	if literal.len == 1 {
		return true
	}
	if literal.len < 2 || literal[0] != `\\` {
		return false
	}
	if literal.len == 2 {
		return true
	}
	if literal[1] == `x` {
		mut decoded := 0
		for ch in literal[2..].bytes() {
			digit := if ch >= `0` && ch <= `9` {
				int(ch - `0`)
			} else if ch >= `a` && ch <= `f` {
				int(ch - `a`) + 10
			} else if ch >= `A` && ch <= `F` {
				int(ch - `A`) + 10
			} else {
				return false
			}
			decoded = decoded * 16 + digit
			if decoded > 0xff {
				return false
			}
		}
		return true
	}
	if literal[1] < `0` || literal[1] > `7` || literal.len > 4 {
		return false
	}
	mut decoded := 0
	for digit in literal[1..].bytes() {
		if digit < `0` || digit > `7` {
			return false
		}
		decoded = decoded * 8 + int(digit - `0`)
	}
	return decoded <= 0xff
}

fn (tc &TypeChecker) c_literal_arg(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .char_literal {
		return node.value.starts_with('c:')
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.c_literal_arg(tc.a.child(&node, 0))
	}
	return false
}

fn voidptr_arg_compatible(expected Type, actual Type) bool {
	if !fn_param_is_voidptr_type(expected) {
		return false
	}
	return fn_param_is_voidptr_type(actual) || voidptr_arg_type_passes_direct(actual)
}

fn voidptr_arg_type_passes_direct(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	return clean is Pointer || clean is Nil || clean.name() == 'voidptr'
}

fn variadic_elem_accepts_any(typ Type) bool {
	if typ is Pointer {
		return typ.base_type is Void
	}
	return false
}

fn variadic_any_arg_has_value(typ Type) bool {
	return type_has_runtime_value(typ)
}

fn type_has_runtime_value(typ Type) bool {
	if typ is OptionType {
		return typ.base_type !is Void
	}
	if typ is ResultType {
		return typ.base_type !is Void
	}
	if typ is MultiReturn {
		return false
	}
	return typ !is Void && typ !is None && typ !is Unknown && !type_contains_unknown(typ)
}

fn (tc &TypeChecker) arg_is_spread(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .prefix && (node.value == '...' || node.op == .none) && node.children_count > 0 {
		return true
	}
	return false
}

fn (tc &TypeChecker) spread_arg_value(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .prefix && (node.value == '...' || node.op == .none) && node.children_count > 0 {
		return tc.a.child(&node, 0)
	}
	return none
}

fn (mut tc TypeChecker) call_has_spread_covering_fixed_variadic_args(node flat.Node, info CallInfo, ctx_count int, ctx_omitted bool) bool {
	if !info.is_variadic || info.params.len == 0 {
		return false
	}
	if info.params[info.params.len - 1] !is Array {
		return false
	}
	for i in 1 + info.arg_offset .. node.children_count {
		if _ := tc.spread_arg_value(tc.a.child(&node, i)) {
			arg_shift := if ctx_omitted { ctx_count } else { 0 }
			param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) +
				arg_shift
			if param_idx >= 0 && param_idx < info.params.len - 1 {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) spread_elem_compatible(actual Type, expected Type) bool {
	return tc.receiver_compatible(actual, expected) || tc.type_compatible(actual, expected)
}

fn (tc &TypeChecker) variadic_spread_arg_compatible(actual Type, expected_array Array) bool {
	if actual_array := array_type_from_receiver(actual) {
		if actual_array.elem_type.name() != expected_array.elem_type.name() {
			return false
		}
	}
	expected := Type(expected_array)
	if tc.receiver_compatible(actual, expected) || tc.type_compatible(actual, expected) {
		return true
	}
	actual_elem := array_like_elem_type(unwrap_pointer(actual)) or { return false }
	return tc.spread_elem_compatible(actual_elem, expected_array.elem_type)
}

fn (tc &TypeChecker) fixed_variadic_spread_tail_compatible(actual_elem Type, expected_array Array) bool {
	expected := Type(expected_array)
	if tc.spread_elem_compatible(actual_elem, expected) {
		return true
	}
	return tc.spread_elem_compatible(actual_elem, expected_array.elem_type)
}

fn (mut tc TypeChecker) check_spread_over_fixed_variadic_tail(call_id flat.NodeId, node flat.Node, info CallInfo, arg_child_idx int, param_idx int) bool {
	if !info.is_variadic || info.params.len == 0 || param_idx < 0
		|| param_idx >= info.params.len - 1 {
		return false
	}
	variadic_raw := info.params[info.params.len - 1]
	variadic_array := if variadic_raw is Array {
		variadic_raw
	} else {
		return false
	}
	spread_id := tc.spread_arg_value(tc.a.child(&node, arg_child_idx)) or { return false }
	actual := tc.resolve_expr(spread_id, variadic_raw)
	actual_elem := array_like_elem_type(unwrap_pointer(actual)) or {
		tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
			param_idx + 1} to `${tc.call_display_name(node)}`; expected `${variadic_raw.name()}`',
			call_id)
		return true
	}
	for fixed_idx in param_idx .. info.params.len - 1 {
		expected := info.params[fixed_idx]
		if !tc.spread_elem_compatible(actual_elem, expected) {
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				call_id)
			break
		}
	}
	if !tc.fixed_variadic_spread_tail_compatible(actual_elem, variadic_array) {
		tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
			param_idx + 1} to `${tc.call_display_name(node)}`; expected `${variadic_raw.name()}`',
			call_id)
	}
	return true
}

fn (tc &TypeChecker) variadic_any_arg_is_scalar(id flat.NodeId) bool {
	if tc.arg_is_spread(id) {
		return false
	}
	if !tc.valid_node_id(id) {
		return false
	}
	if tc.a.nodes[int(id)].kind == .enum_val {
		return false
	}
	actual := tc.resolve_type(id)
	if !variadic_any_arg_has_value(actual) {
		return false
	}
	if _ := array_type_from_receiver(actual) {
		return false
	}
	return true
}

fn (mut tc TypeChecker) specialized_plain_generic_call_info(node flat.Node, info CallInfo) CallInfo {
	if tc.call_has_explicit_generic_type_args(node) {
		return info
	}
	generic_params := tc.fn_generic_params[info.name] or { return info }
	param_texts := tc.fn_param_type_texts[info.name] or { return info }
	if generic_params.len == 0 || node.children_count <= 1
		|| tc.call_has_explicit_generic_args(node) {
		return info
	}
	mut inferred := map[string]string{}
	mut inferred_types := map[string]Type{}
	mut first_param_idx := 0
	if info.has_receiver && param_texts.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_id := tc.a.child(fn_node, 0)
			resolved_receiver := tc.resolve_type(recv_id)
			// resolve_generic_struct_method has already substituted receiver type
			// arguments. Keep that declared specialization for aliases such as
			// `type Vec4 = vec.Vec4[f32]`; a short struct initializer can otherwise
			// make the expression cache look like `Vec4[int]` from its literal fields.
			actual := if info.name.contains('[') && info.params.len > 0
				&& !generic_semantic_type_has_placeholder(info.params[0]) {
				info.params[0]
			} else {
				resolved_receiver
			}
			tc.infer_generic_type_text_from_type(param_texts[0], actual, generic_params, mut
				inferred)
			tc.infer_generic_type_value_from_type(param_texts[0], actual, generic_params, mut
				inferred_types)
			first_param_idx = 1
		}
	}
	for param_idx in first_param_idx .. param_texts.len {
		arg_idx := param_idx - first_param_idx + 1 + info.arg_offset
		if arg_idx >= node.children_count {
			break
		}
		if info.is_variadic && param_idx == param_texts.len - 1 {
			elem_text := generic_variadic_elem_param_text(param_texts[param_idx])
			for call_arg_idx in arg_idx .. node.children_count {
				arg_id := tc.call_arg_value(tc.a.child(&node, call_arg_idx))
				if spread_id := tc.spread_arg_child(arg_id) {
					actual := tc.resolve_type(spread_id)
					tc.infer_generic_type_text_from_type(param_texts[param_idx], actual,
						generic_params, mut inferred)
					continue
				}
				actual := tc.resolve_type(arg_id)
				tc.infer_generic_type_text_from_type(elem_text, actual, generic_params, mut
					inferred)
			}
			break
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, arg_idx))
		actual := tc.resolve_type(arg_id)
		tc.infer_generic_type_text_from_type(param_texts[param_idx], actual, generic_params, mut
			inferred)
		tc.infer_generic_type_value_from_type(param_texts[param_idx], actual, generic_params, mut
			inferred_types)
	}
	mut concrete_args := []string{cap: generic_params.len}
	mut concrete_types := []Type{cap: generic_params.len}
	for param in generic_params {
		arg := inferred[param] or { return info }
		concrete_args << arg
		// Retain the caller-side semantic type. Re-parsing a bare caller type such
		// as `User` in the generic declaration's module would incorrectly turn it
		// into `json2.User`.
		concrete_types << inferred_types[param] or {
			tc.type_from_known_symbol(arg) or { tc.parse_type(arg) }
		}
	}
	mut sub_params := []Type{}
	for i, param_text in param_texts {
		if generic_type_application(param_text) {
			// Named applications such as `Box[T]` lose their arguments in the
			// open parsed type, so retain the textual reconstruction for them.
			sub_params << tc.parse_fn_signature_type(info.name, subst_generic_text(param_text,
				concrete_args, generic_params))
		} else if i < info.params.len {
			sub_params << tc.substitute_generic_type_values(info.params[i], concrete_types,
				generic_params)
		} else {
			sub_params << tc.parse_fn_signature_type(info.name, subst_generic_text(param_text,
				concrete_args, generic_params))
		}
	}
	ret_text := tc.fn_ret_type_texts[info.name] or { '' }
	sub_ret := if ret_text.len > 0 {
		if generic_type_application(ret_text) {
			tc.parse_fn_signature_type(info.name, subst_generic_text(ret_text, concrete_args,
				generic_params))
		} else {
			tc.substitute_generic_type_values(info.return_type, concrete_types, generic_params)
		}
	} else {
		tc.substitute_generic_type_values(info.return_type, concrete_types, generic_params)
	}
	return CallInfo{
		name:                 info.name
		params:               sub_params
		shared_params:        info.shared_params.clone()
		return_type:          sub_ret
		has_receiver:         info.has_receiver
		is_variadic:          info.is_variadic
		is_c_variadic:        info.is_c_variadic
		params_known:         true
		has_implicit_veb_ctx: info.has_implicit_veb_ctx
		arg_offset:           info.arg_offset
	}
}

fn (tc &TypeChecker) call_has_explicit_generic_args(node flat.Node) bool {
	if node.value.len > 0 {
		return true
	}
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	return fn_node.kind == .index && tc.generic_call_type_arg_names(fn_node).len > 0
}

fn (tc &TypeChecker) call_has_explicit_generic_type_args(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	return fn_node.kind == .index && fn_node.value != 'range'
		&& tc.generic_call_type_arg_names(fn_node).len > 0
}

fn generic_variadic_elem_param_text(param_text string) string {
	clean := trimmed_space(param_text)
	if clean.starts_with('...') {
		return trimmed_space(clean[3..])
	}
	if clean.starts_with('[]') {
		return trimmed_space(clean[2..])
	}
	return clean
}

fn (tc &TypeChecker) parse_fn_signature_type(name string, typ string) Type {
	decl_file := tc.fn_type_files[name] or { return tc.parse_type(typ) }
	decl_module := tc.fn_type_modules[name] or { tc.file_modules[decl_file] or { tc.cur_module } }
	mut scoped := tc.fork_type_parse_view(decl_file, decl_module)
	// Fully qualify symbols owned by the declaration module before parsing the
	// substituted signature. A bare concrete type can belong to the generic
	// call site (notably a type from `main`), so resolution deliberately leaves
	// that spelling bare and the neutral parse context preserves its authority.
	return scoped.parse_resolution_type(typ)
}

fn (mut tc TypeChecker) infer_generic_type_text_from_type(param_text string, actual Type, generic_params []string, mut inferred map[string]string) {
	clean := trimmed_space(param_text)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		if actual is Pointer {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		} else {
			tc.infer_generic_type_text_from_type(clean[1..], actual, generic_params, mut inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_text_from_type(clean[4..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('...') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[3..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') {
		if actual is Array {
			tc.infer_generic_type_text_from_type(clean[2..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') {
		if actual is OptionType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') {
		if actual is ResultType {
			tc.infer_generic_type_text_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if actual is FnType {
			tc.infer_generic_fn_type_text_from_type(clean, actual, generic_params, mut inferred)
		}
		return
	}
	if generic_type_application(clean) {
		actual_text := tc.generic_infer_type_text(actual)
		tc.infer_generic_type_text_from_text(clean, actual_text, generic_params, mut inferred)
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			mut actual_text := tc.generic_infer_type_text(actual)
			if actual_text == 'unknown' || actual_text == 'generic' {
				actual_text = param
			}
			inferred[param] = actual_text
			return
		}
	}
}

// infer_generic_type_value_from_type retains the resolved caller-side Type for
// each generic placeholder. The text inference remains useful for reconstructing
// named applications, but semantic substitution must not parse a caller-local or
// canonical qualified name again in the callee's import context.
fn (mut tc TypeChecker) infer_generic_type_value_from_type(param_text string, actual Type, generic_params []string, mut inferred map[string]Type) {
	clean := trimmed_space(param_text)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		if actual is Pointer {
			tc.infer_generic_type_value_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		} else {
			tc.infer_generic_type_value_from_type(clean[1..], actual, generic_params, mut inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_value_from_type(clean[4..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('...') {
		if actual is Array {
			tc.infer_generic_type_value_from_type(clean[3..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') {
		if actual is Array {
			tc.infer_generic_type_value_from_type(clean[2..], actual.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') {
		if actual is OptionType {
			tc.infer_generic_type_value_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') {
		if actual is ResultType {
			tc.infer_generic_type_value_from_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			inferred[param] = actual
			return
		}
	}
}

fn (tc &TypeChecker) generic_infer_type_text(actual Type) string {
	if actual is Unknown {
		if name := generic_placeholder_from_unknown(actual) {
			return name
		}
	}
	return actual.name()
}

fn (tc &TypeChecker) infer_generic_type_text_from_text(param_text string, actual_text string, generic_params []string, mut inferred map[string]string) {
	clean := trimmed_space(param_text)
	actual := trimmed_space(actual_text)
	if clean.len == 0 || actual.len == 0 {
		return
	}
	for param in generic_params {
		if clean == param && param !in inferred {
			inferred[param] = if actual == 'unknown' || actual == 'generic' { param } else { actual }
			return
		}
	}
	if clean.starts_with('&') || actual.starts_with('&') {
		if clean.starts_with('&') && actual.starts_with('&') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('mut ') {
		tc.infer_generic_type_text_from_text(clean[4..], actual.trim_left('&'), generic_params, mut
			inferred)
		return
	}
	if clean.starts_with('...') || actual.starts_with('...') {
		if clean.starts_with('...') && actual.starts_with('...') {
			tc.infer_generic_type_text_from_text(clean[3..], actual[3..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('[]') || actual.starts_with('[]') {
		if clean.starts_with('[]') && actual.starts_with('[]') {
			tc.infer_generic_type_text_from_text(clean[2..], actual[2..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('?') || actual.starts_with('?') {
		if clean.starts_with('?') && actual.starts_with('?') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') || actual.starts_with('!') {
		if clean.starts_with('!') && actual.starts_with('!') {
			tc.infer_generic_type_text_from_text(clean[1..], actual[1..], generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('map[') || actual.starts_with('map[') {
		if !clean.starts_with('map[') || !actual.starts_with('map[') {
			return
		}
		clean_end := find_matching_bracket(clean, 3)
		actual_end := find_matching_bracket(actual, 3)
		if clean_end >= clean.len || actual_end >= actual.len {
			return
		}
		tc.infer_generic_type_text_from_text(clean[4..clean_end], actual[4..actual_end],
			generic_params, mut inferred)
		tc.infer_generic_type_text_from_text(clean[clean_end + 1..], actual[actual_end + 1..],
			generic_params, mut inferred)
		return
	}
	if clean.starts_with('[') || actual.starts_with('[') {
		if !clean.starts_with('[') || !actual.starts_with('[') {
			return
		}
		clean_end := find_matching_bracket(clean, 0)
		actual_end := find_matching_bracket(actual, 0)
		if clean_end >= clean.len || actual_end >= actual.len
			|| clean[..clean_end + 1] != actual[..actual_end + 1] {
			return
		}
		tc.infer_generic_type_text_from_text(clean[clean_end + 1..], actual[actual_end + 1..],
			generic_params, mut inferred)
		return
	}
	param_base, param_args, param_is_generic := generic_type_application_parts(clean)
	actual_base, actual_args, actual_is_generic := generic_type_application_parts(actual)
	if param_is_generic || actual_is_generic {
		if !param_is_generic || !actual_is_generic || param_args.len != actual_args.len
			|| !tc.generic_type_base_matches(param_base, actual_base) {
			return
		}
		for i in 0 .. param_args.len {
			tc.infer_generic_type_text_from_text(param_args[i], actual_args[i], generic_params, mut
				inferred)
		}
	}
}

fn (mut tc TypeChecker) infer_generic_fn_type_text_from_type(param_text string, actual FnType, generic_params []string, mut inferred map[string]string) {
	params_start := param_text.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < param_text.len {
		if param_text[params_end] == `(` {
			depth++
		} else if param_text[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= param_text.len {
		return
	}
	parts := split_params(param_text[params_start..params_end])
	for i, part in parts {
		if i >= actual.params.len {
			break
		}
		tc.infer_generic_type_text_from_type(normalize_fn_type_param_text(part), fn_param_type(actual,
			i), generic_params, mut inferred)
	}
	ret := trimmed_space(param_text[params_end + 1..])
	if ret.len > 0 {
		tc.infer_generic_type_text_from_type(ret, actual.return_type, generic_params, mut inferred)
	}
}

// array_map_return_elem_type supports array map return elem type handling for TypeChecker.
fn (mut tc TypeChecker) array_map_return_elem_type(node flat.Node) Type {
	if node.children_count < 2 {
		return Type(void_)
	}
	tc.push_array_dsl_scope(node, 'array.map')
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	elem_type := tc.resolve_type(arg_id)
	tc.pop_scope()
	if fn_typ := fn_type_from_type(elem_type) {
		if !tc.expr_uses_ident(arg_id, 'it') {
			return fn_typ.return_type
		}
	}
	if elem_type is Void || elem_type is Unknown {
		return Type(void_)
	}
	return elem_type
}

fn (tc &TypeChecker) expr_uses_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len || name.len == 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := tc.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if tc.expr_uses_ident(tc.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

// array_map_result_borrows_element reports whether a mapper result must be made
// independent before its source array is destroyed. Opaque function values are handled
// conservatively because their body cannot be inspected at the call site.
fn (tc &TypeChecker) array_map_result_borrows_element(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	arg_id := tc.call_arg_value(tc.a.child(&node, 1))
	arg := tc.a.nodes[int(arg_id)]
	if arg.kind == .fn_literal {
		return true
	}
	mut body_id := arg_id
	mut elem_name := 'it'
	if arg.kind == .lambda_expr && arg.children_count > 0 {
		body_id = tc.a.child(&arg, arg.children_count - 1)
		if arg.children_count > 1 {
			param := tc.a.child_node(&arg, 0)
			if param.kind == .ident && param.value.len > 0 {
				elem_name = param.value
			}
		}
	} else if _ := fn_type_from_type(tc.resolve_type(arg_id)) {
		return true
	}
	return tc.array_map_expr_references_ident(body_id, elem_name)
		&& !tc.ownership_expr_creates_owned_value(body_id)
}

fn (tc &TypeChecker) array_map_expr_references_ident(id flat.NodeId, name string) bool {
	if !tc.valid_node_id(id) || name.len == 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := tc.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if tc.array_map_expr_references_ident(tc.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) array_contains_elem_type(base_node flat.Node, array_type Array) Type {
	if base_node.kind == .selector && base_node.value == 'args' && base_node.children_count > 0 {
		parent := tc.a.child_node(&base_node, 0)
		if parent.kind == .ident && parent.value == 'os' {
			return Type(String{})
		}
	}
	return array_type.elem_type
}

fn (tc &TypeChecker) is_os_args_contains_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'contains' || fn_node.children_count == 0 {
		return false
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind != .selector || base_node.value != 'args' || base_node.children_count == 0 {
		return false
	}
	parent := tc.a.child_node(base_node, 0)
	return parent.kind == .ident && parent.value == 'os'
}

fn (tc &TypeChecker) pointer_builtin_method_call_info(base_type Type, method string) ?CallInfo {
	receiver := pointer_builtin_receiver_name(base_type)
	if receiver.len == 0 {
		return none
	}
	if receiver in ['charptr', 'byteptr'] && method in ['vstring', 'vstring_with_len'] {
		mut params := tarr1(base_type)
		if method == 'vstring_with_len' {
			params << Type(int_)
		}
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       params
			return_type:  Type(string_)
			has_receiver: true
			params_known: true
		}
	}
	if receiver in ['byteptr', 'voidptr'] && method == 'vbytes' {
		return CallInfo{
			name:         '${receiver}.${method}'
			params:       [base_type, Type(int_)]
			return_type:  Type(Array{
				elem_type: Type(u8_)
			})
			has_receiver: true
			params_known: true
		}
	}
	if receiver == 'voidptr' && method == 'hex_full' {
		return CallInfo{
			name:         'voidptr.hex_full'
			params:       [base_type]
			return_type:  Type(string_)
			has_receiver: true
			params_known: true
		}
	}
	return none
}

fn pointer_builtin_receiver_name(typ Type) string {
	if typ is Alias {
		if typ.name in ['charptr', 'byteptr', 'voidptr'] {
			return typ.name
		}
		return pointer_builtin_receiver_name(typ.base_type)
	}
	if typ is Pointer {
		base := typ.base_type
		if base is Alias {
			if base.name == 'byte' {
				return 'byteptr'
			}
			return pointer_builtin_receiver_name(base)
		}
		if base is Char {
			return 'charptr'
		}
		if base is Void {
			return 'voidptr'
		}
		if base is Primitive && prim_name(base) == 'u8' {
			return 'byteptr'
		}
	}
	if typ is Primitive && prim_name(typ) == 'u8' {
		return 'byteptr'
	}
	return ''
}

// min_required_arg_count supports min required arg count handling for TypeChecker.
fn (tc &TypeChecker) min_required_arg_count(info CallInfo) int {
	if info.is_variadic && info.params.len > 0 {
		return info.params.len - 1
	}
	mut n := info.params.len
	for n > 0 {
		param := info.params[n - 1]
		if param is OptionType || tc.is_params_struct_type(param) {
			n--
			continue
		}
		break
	}
	return n
}

fn c_variadic_fixed_param_count(info CallInfo) int {
	if info.is_c_variadic && info.params.len > 0 && info.params[info.params.len - 1] is Array {
		return info.params.len - 1
	}
	return info.params.len
}

fn (tc &TypeChecker) is_params_struct_type(typ Type) bool {
	if typ is Struct {
		if typ.name in tc.params_structs {
			return true
		}
		base, _, is_generic := generic_type_application_parts(typ.name)
		if is_generic
			&& (base in tc.params_structs || base.all_after_last('.') in tc.params_structs) {
			return true
		}
		qname := tc.qualify_name(typ.name)
		return qname in tc.params_structs
	}
	if typ is Alias {
		return tc.is_params_struct_type(typ.base_type)
	}
	// A `&Cfg` param (pointer to an `@[params]` struct, e.g. `audio.setup(desc
	// &C.saudio_desc)`) is still optional: a zero-arg or `key: value` call
	// constructs a default and passes its address, matching v1.
	if typ is Pointer {
		return tc.is_params_struct_type(typ.base_type)
	}
	return false
}

// call_arg_needs_array_dsl_scope updates call arg needs array dsl scope state for TypeChecker.
fn (tc &TypeChecker) call_arg_needs_array_dsl_scope(name string, param_idx int) bool {
	return param_idx == 1 && is_array_dsl_call_name(name)
}

fn (tc &TypeChecker) array_dsl_fn_arg_compatible(node flat.Node, info CallInfo, param_idx int, actual Type) bool {
	if param_idx != 1 {
		return false
	}
	if info.name == 'array.count' {
		return array_count_dsl_predicate_compatible(actual)
	}
	if !is_array_filter_or_map_call_name(info.name) {
		return false
	}
	fn_typ := fn_type_from_type(actual) or { return false }
	if fn_typ.params.len != 1 {
		return false
	}
	arr := tc.call_receiver_array_type(node) or { return false }
	param := fn_param_type(fn_typ, 0)
	if !tc.receiver_compatible(param, arr.elem_type)
		&& !tc.receiver_compatible(arr.elem_type, param) {
		return false
	}
	if is_array_filter_call_name(info.name) {
		return tc.type_compatible(fn_typ.return_type, Type(bool_))
	}
	return true
}

fn array_count_dsl_predicate_compatible(actual Type) bool {
	if actual is Alias {
		return array_count_dsl_predicate_compatible(actual.base_type)
	}
	return actual is Primitive
}

// is_array_dsl_call_name reports whether is array dsl call name applies in types.
fn is_array_dsl_call_name(name string) bool {
	if name.len < 9 || name.len > 12 || !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	len := name.len - start
	return match len {
		3 {
			(name[start] == `a` && ((name[start + 1] == `n` && name[start + 2] == `y`)
				|| (name[start + 1] == `l` && name[start + 2] == `l`)))
				|| (name[start] == `m` && name[start + 1] == `a` && name[start + 2] == `p`)
		}
		4 {
			name[start] == `s` && name[start + 1] == `o` && name[start + 2] == `r`
				&& name[start + 3] == `t`
		}
		5 {
			name[start] == `c` && name[start + 1] == `o` && name[start + 2] == `u`
				&& name[start + 3] == `n` && name[start + 4] == `t`
		}
		6 {
			(name[start] == `f` && name[start + 1] == `i` && name[start + 2] == `l`
				&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `r`)
				|| (name[start] == `s` && name[start + 1] == `o` && name[start + 2] == `r`
				&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `d`)
		}
		else {
			false
		}
	}
}

fn has_array_dot_prefix(name string) bool {
	return name[0] == `a` && name[1] == `r` && name[2] == `r` && name[3] == `a` && name[4] == `y`
		&& name[5] == `.`
}

fn is_array_filter_or_map_call_name(name string) bool {
	if name.len != 9 && name.len != 12 {
		return false
	}
	if !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	if name.len == 9 {
		return name[start] == `m` && name[start + 1] == `a` && name[start + 2] == `p`
	}
	return is_array_filter_method_name(name, start)
}

fn is_array_filter_call_name(name string) bool {
	return name.len == 12 && has_array_dot_prefix(name)
		&& is_array_filter_method_name(name, 'array.'.len)
}

fn is_array_filter_method_name(name string, start int) bool {
	return name[start] == `f` && name[start + 1] == `i` && name[start + 2] == `l`
		&& name[start + 3] == `t` && name[start + 4] == `e` && name[start + 5] == `r`
}

fn array_insert_prepend_many_param_idx(name string) int {
	if name.len != 12 && name.len != 13 {
		return -1
	}
	if !has_array_dot_prefix(name) {
		return -1
	}
	start := 'array.'.len
	if name.len == 12 {
		if name[start] == `i` && name[start + 1] == `n` && name[start + 2] == `s`
			&& name[start + 3] == `e` && name[start + 4] == `r` && name[start + 5] == `t` {
			return 2
		}
		return -1
	}
	if name[start] == `p` && name[start + 1] == `r` && name[start + 2] == `e`
		&& name[start + 3] == `p` && name[start + 4] == `e` && name[start + 5] == `n`
		&& name[start + 6] == `d` {
		return 1
	}
	return -1
}

fn is_map_keys_values_call_name(name string) bool {
	if name.len != 8 && name.len != 10 {
		return false
	}
	if name[0] != `m` || name[1] != `a` || name[2] != `p` || name[3] != `.` {
		return false
	}
	if name.len == 8 {
		return name[4] == `k` && name[5] == `e` && name[6] == `y` && name[7] == `s`
	}
	return name[4] == `v` && name[5] == `a` && name[6] == `l` && name[7] == `u` && name[8] == `e`
		&& name[9] == `s`
}

// call_explicit_arg_count updates call explicit arg count state for types.
fn call_explicit_arg_count(node flat.Node) int {
	if node.children_count <= 1 {
		return 0
	}
	mut n := 0
	for i in 1 .. node.children_count {
		if int(node.children_start) + i < 0 {
			continue
		}
		n++
	}
	return n
}

// push_array_dsl_scope updates push array dsl scope state for TypeChecker.
fn (mut tc TypeChecker) push_array_dsl_scope(node flat.Node, name string) {
	tc.push_scope()
	arr := tc.call_receiver_array_type(node) or { return }
	if is_array_sort_dsl_call_name(name) {
		tc.cur_scope.insert('a', arr.elem_type)
		tc.cur_scope.insert('b', arr.elem_type)
		$if ownership ? {
			tc.ownership_bind_array_dsl_element(node, 'a', arr.elem_type)
			tc.ownership_bind_array_dsl_element(node, 'b', arr.elem_type)
		}
		return
	}
	tc.cur_scope.insert('it', arr.elem_type)
	$if ownership ? {
		tc.ownership_bind_array_dsl_element(node, 'it', arr.elem_type)
	}
}

fn is_array_sort_dsl_call_name(name string) bool {
	if name.len != 10 && name.len != 12 {
		return false
	}
	if !has_array_dot_prefix(name) {
		return false
	}
	start := 'array.'.len
	if name[start] != `s` || name[start + 1] != `o` || name[start + 2] != `r`
		|| name[start + 3] != `t` {
		return false
	}
	return name.len == 10 || (name[start + 4] == `e` && name[start + 5] == `d`)
}

// call_receiver_array_type updates call receiver array type state for TypeChecker.
fn (tc &TypeChecker) call_receiver_array_type(node flat.Node) ?Array {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := tc.a.child(fn_node, 0)
	if resolved := tc.expr_type(base_id) {
		if arr := call_receiver_array_from_type(resolved) {
			return arr
		}
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.typ.len > 0 {
		if arr := call_receiver_array_from_type(tc.parse_resolution_type(base_node.typ)) {
			return arr
		}
	}
	if arr := call_receiver_array_from_type(tc.resolve_type(base_id)) {
		return arr
	}
	return none
}

fn call_receiver_array_from_type(typ Type) ?Array {
	return match typ {
		Array {
			typ
		}
		ArrayFixed {
			Array{
				elem_type: typ.elem_type
			}
		}
		Alias {
			call_receiver_array_from_type(typ.base_type)
		}
		Pointer {
			call_receiver_array_from_type(typ.base_type)
		}
		else {
			none
		}
	}
}

// call_arg_value updates call arg value state for TypeChecker.
fn (tc &TypeChecker) call_arg_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .field_init && node.children_count > 0 {
		return tc.a.child(&node, 0)
	}
	return id
}

fn (tc &TypeChecker) spread_arg_child(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .prefix && node.value == '...' && node.children_count > 0 {
		return tc.a.child(&node, 0)
	}
	return none
}

fn (mut tc TypeChecker) check_array_decompose_counts() {
	for index, node in tc.a.nodes {
		if node.kind != .call || node.children_count < 2 {
			continue
		}
		callee := tc.a.child_node(&node, 0)
		if callee.kind != .ident {
			continue
		}
		name := tc.qualify_fn_name(callee.value)
		params := tc.fn_param_types[name] or { tc.fn_param_types[callee.value] or { continue } }
		if tc.fn_variadic[name] || tc.fn_variadic[callee.value] {
			continue
		}
		mut preceding := 0
		for i in 1 .. node.children_count {
			arg_id := tc.call_arg_value(tc.a.child(&node, i))
			spread_id := tc.spread_arg_child(arg_id) or {
				preceding++
				continue
			}
			spread := tc.a.nodes[int(spread_id)]
			if spread.kind != .array_literal {
				break
			}
			needed := params.len - preceding
			actual := int(spread.children_count)
			if actual < needed {
				element_word := if actual == 1 { 'element' } else { 'elements' }
				pos := token.new_span(spread.pos.id, int_max(0, spread.pos.offset - 3),
					spread.pos.end)
				tc.errors << tc.make_type_error_at(.call_arg_mismatch,
					'array decompose has ${actual} ${element_word} but ${needed} are needed for `${callee.value}`',
					arg_id, pos)
			}
			break
		}
		_ = index
	}
}

fn (tc &TypeChecker) nonvariadic_spread_extra_arg_count(node flat.Node, info CallInfo, recv_extra int) int {
	mut extra := 0
	mut logical_arg_idx := 0
	for i in 1 + info.arg_offset .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			continue
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if _ := tc.spread_arg_child(arg_id) {
			param_idx := logical_arg_idx + recv_extra
			count := info.params.len - param_idx
			if count > 1 {
				extra += count - 1
			}
			if count > 0 {
				logical_arg_idx += count
				continue
			}
		}
		logical_arg_idx++
	}
	return extra
}

// receiver_compatible supports receiver compatible handling for TypeChecker.
fn (tc &TypeChecker) receiver_compatible(actual Type, expected Type) bool {
	if tc.type_compatible(actual, expected) {
		return true
	}
	// Check the generic-base relaxation before the pointer fallbacks: it unwraps
	// pointers itself, so a bare `&Box{...}` literal still matches an expected
	// `&Box[int]` (while `&Box[string]` vs `&Box[int]` stays rejected).
	if tc.generic_receiver_base_match(actual, expected) {
		return true
	}
	if expected is Pointer {
		if actual is Pointer {
			if expected.base_type is Alias && expected.base_type.base_type is Pointer {
				return tc.type_compatible(actual, expected.base_type.base_type)
			}
			return false
		}
		if expected.base_type is Pointer {
			return false
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if actual is Pointer {
		return tc.type_compatible(actual.base_type, expected)
	}
	return false
}

fn (tc &TypeChecker) method_receiver_compatible(actual Type, expected Type, method_name string) bool {
	if tc.receiver_compatible(actual, expected) {
		return true
	}
	// A method receiver rendered as the bare/unqualified generic form (`Vec3[T]`)
	// matches the declaring module's qualified form (`vec.Vec3[T]`) — same generic,
	// only the module qualifier differs. This arises when an operator-overload result
	// on an aliased generic instance (`type Vec = vec.Vec3[f64]`) is re-checked in the
	// annotate pass with an unqualified receiver spelling.
	if tc.generic_receiver_qualifier_mismatch(actual, expected) {
		return true
	}
	actual_depth, actual_base := type_pointer_depth_and_base(actual)
	expected_depth, expected_base := type_pointer_depth_and_base(expected)
	// C receiver lowering can dereference a pointer for a value receiver, but it
	// does not adjust depth when both the actual and expected receivers are pointers.
	if actual_depth == 1 && expected_depth == 0 && (tc.type_compatible(actual_base, expected_base)
		|| tc.generic_receiver_base_match(actual_base, expected_base)
		|| tc.receiver_embeds(actual_base, expected_base)) {
		return true
	}
	// Runtime array builtins are declared with the internal `array` receiver, which is
	// represented here as `[]void`. Keep that erasure scoped to raw `array.*`
	// methods; user receivers like `fn (xs []void) touch()` must not accept `[]int`.
	if checker_is_raw_collection_method_name(method_name, 'array.') && actual is Array
		&& expected is Array && expected.elem_type is Void {
		return true
	}
	return false
}

// is_locally_declared_bare_type reports whether the bare `name` is a type declared in
// the current program (main module), which is bare-keyed in the type tables. Such a name
// denotes that local type, not an unqualified spelling of some other module's type.
pub fn (tc &TypeChecker) is_locally_declared_bare_type(name string) bool {
	return name in tc.structs || name in tc.interface_names || name in tc.sum_types
		|| name in tc.enum_names || name in tc.flag_enums || name in tc.type_aliases
}

// qualifier_relaxed_type_name_match reports whether `a` and `b` name the same type
// allowing ONLY a missing module qualifier on one side: they are equal, or one is the
// unqualified spelling of the other (`Vec3` vs `vec.Vec3`). Two *different* qualified
// names that merely share a short name (`a.Box` vs `b.Box`, `foo.User` vs `bar.User`)
// are distinct types and do NOT match. A bare name that is itself a locally-declared
// type (`User` alongside another module's `foo.User`) denotes that local type, so it
// does NOT match a qualified name from a different module.
fn (tc &TypeChecker) qualifier_relaxed_type_name_match(a string, b string) bool {
	if a == b {
		return true
	}
	a_qualified := a.contains('.')
	b_qualified := b.contains('.')
	// Both bare (but unequal) or both qualified (but unequal) name different types.
	if a_qualified == b_qualified {
		return false
	}
	if a.all_after_last('.') != b.all_after_last('.') {
		return false
	}
	bare := if a_qualified { b } else { a }
	qualified := if a_qualified { a } else { b }
	qualifier := qualified.all_before_last('.')
	// A locally-declared bare type only relaxes to a qualified name that is its own
	// main spelling; against another module's same-short-named type it is distinct.
	if qualifier != 'main' && qualifier.len > 0 && tc.is_locally_declared_bare_type(bare) {
		return false
	}
	return true
}

// generic_receiver_qualifier_mismatch accepts a method receiver that differs from
// the declared receiver ONLY by a missing module qualifier on the same generic form
// (`Vec3[T]` vs `vec.Vec3[T]`, or `Vec3[int]` vs `vec.Vec3[int]`). It requires the base
// and every generic argument to be qualifier-relaxed matches (an unqualified spelling
// of the same qualified name), and the base to be a known generic struct, so it never
// conflates two different instantiations (`Vec3[string]` vs `vec.Vec3[int]`), two
// different modules' same-short-named types (`a.Box[int]` vs `b.Box[int]`,
// `Vec[foo.User]` vs `Vec[bar.User]`), or non-generic types.
fn (tc &TypeChecker) generic_receiver_qualifier_mismatch(actual Type, expected Type) bool {
	if (actual is Pointer) != (expected is Pointer) {
		return false
	}
	a_full := unwrap_pointer(actual).name()
	e_full := unwrap_pointer(expected).name()
	if a_full == e_full || a_full.len == 0 || e_full.len == 0 {
		return false
	}
	a_base, a_args, a_ok := generic_type_application_parts(a_full)
	e_base, e_args, e_ok := generic_type_application_parts(e_full)
	if !a_ok || !e_ok || a_args.len != e_args.len || a_args.len == 0 {
		return false
	}
	if !tc.qualifier_relaxed_type_name_match(a_base, e_base) {
		return false
	}
	for i in 0 .. a_args.len {
		if !tc.qualifier_relaxed_type_name_match(trimmed_space(a_args[i]), trimmed_space(e_args[i])) {
			return false
		}
	}
	short := e_base.all_after_last('.')
	return e_base in tc.struct_generic_params || a_base in tc.struct_generic_params
		|| short in tc.struct_generic_params
}

// generic_receiver_base_match relaxes compatibility between two same-base generic
// struct types when at least one side is the open/bare generic form — a bare
// `Vec4{...}` literal specializing to the expected `Vec4[f32]`, or a concrete
// `Vec4[f32]` value matching the open `Vec4` / `Vec4[T]` method-receiver form.
//
// It deliberately does NOT relax two *different concrete* instantiations. Because
// `receiver_compatible` is also used for ordinary call arguments, field inits,
// array literals, and expected-type propagation, conflating them would let
// `Box[string]` satisfy an expected `Box[int]` and emit incompatible C structs.
fn (tc &TypeChecker) generic_receiver_base_match(actual Type, expected Type) bool {
	// Both sides must share pointer shape before unwrapping: a `&Box{...}` value must
	// not satisfy an expected `Box[int]` value. The C argument path only
	// auto-dereferences when the actual/expected type names match exactly, so the bare
	// `Box` vs `Box[int]` mismatch would otherwise be emitted as a pointer where a value
	// is required. (A pointer receiver on a value-receiver method is handled by
	// receiver_compatible's pointer fallbacks, not here.)
	if (actual is Pointer) != (expected is Pointer) {
		return false
	}
	a_full := unwrap_pointer(actual).name()
	e_full := unwrap_pointer(expected).name()
	a := strip_generic_args_name(a_full)
	if a.len == 0 || a != strip_generic_args_name(e_full) {
		return false
	}
	if !(a in tc.struct_generic_params || tc.sum_params_for_base(a).len > 0) {
		return false
	}
	if a_full == e_full {
		return false
	}
	// Reject two *different concrete* instantiations (`Box[string]` vs `Box[int]`),
	// which produce incompatible C structs. Relax only when at least one side is
	// the open/bare generic form: a bare `Box{...}` literal specializing to the
	// expected `Box[int]`, or a concrete `Box[int]` value matching the open
	// `Box`/`Box[T]` method-receiver form.
	if tc.is_concrete_generic_instance(a_full) && tc.is_concrete_generic_instance(e_full) {
		return false
	}
	return true
}

// is_concrete_generic_instance reports whether `name` is a fully concrete generic
// instantiation (e.g. `Box[int]`), as opposed to the bare base (`Box`) or an open
// parameter form (`Box[T]`).
fn (tc &TypeChecker) is_concrete_generic_instance(name string) bool {
	_, args, ok := generic_type_application_parts(name)
	if !ok {
		return false
	}
	return tc.generic_args_are_concrete(args)
}

// bare_generic_literal_adopts reports whether a struct literal written as the bare
// generic base (`Box{...}`, no type args) should adopt the concrete `expected`
// instance (`Box[int]`, optionally behind a pointer). The base short-names must match
// and the base must be a known generic struct, so a non-generic same-named struct is
// left to ordinary checking.
fn (tc &TypeChecker) bare_generic_literal_adopts(lit_value string, expected Type) bool {
	if lit_value.len == 0 || lit_value.contains('[') {
		return false
	}
	e_base, _, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok || e_base.all_after_last('.') != lit_value.all_after_last('.') {
		return false
	}
	return e_base in tc.struct_generic_params
		|| e_base.all_after_last('.') in tc.struct_generic_params
}

fn is_anonymous_struct_name(name string) bool {
	return name.all_after_last('.').starts_with('AnonStruct_')
}

fn is_contextual_anonymous_struct_literal(name string) bool {
	return name == 'struct' || is_anonymous_struct_name(name)
}

fn (mut tc TypeChecker) anonymous_struct_literal_compatible(node flat.Node, expected Type) bool {
	struct_type := struct_type_from_type(expected) or { return false }
	if !is_anonymous_struct_name(struct_type.name) {
		return false
	}
	fields := tc.struct_fields_for_init(struct_type.name)
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			return false
		}
		mut field_type := Type(void_)
		if field.value.len > 0 {
			field_type = tc.struct_field_type(struct_type.name, field.value) or { return false }
		} else if i < fields.len {
			field_type = fields[i].typ
		} else {
			return false
		}
		value_id := tc.a.child(field, 0)
		actual := tc.resolve_expr(value_id, field_type)
		if !tc.expr_compatible(value_id, actual, field_type)
			&& !tc.pointer_value_compatible(actual, field_type) {
			return false
		}
	}
	return true
}

// generic_literal_fields_compatible checks a bare generic struct literal's named
// field initializers against the expected concrete instantiation (`Box[int]`),
// substituting the struct's type parameters into each field's declared type. It
// returns false only on a *definite* mismatch (e.g. `Box{v: 'str'}` for `Box[int]`),
// so a clearly-unrelated literal yields a clean checker error instead of adopting the
// type and emitting broken C; unresolvable fields stay lenient.
fn (mut tc TypeChecker) generic_literal_fields_compatible(node flat.Node, expected Type) bool {
	e_base, e_args, e_ok := generic_type_application_parts(unwrap_pointer(expected).name())
	if !e_ok {
		return true
	}
	params := tc.struct_generic_params[e_base] or {
		tc.struct_generic_params[e_base.all_after_last('.')] or { return true }
	}
	if params.len != e_args.len {
		return true
	}
	fields := tc.structs[e_base] or { tc.structs[e_base.all_after_last('.')] or { return true } }
	for i in 0 .. node.children_count {
		fi := tc.a.child_node(&node, i)
		if fi.kind != .field_init || fi.children_count == 0 {
			continue
		}
		mut decl_typ := Type(void_)
		mut found := false
		if fi.value.len > 0 {
			// Named initializer (`Box{v: 'x'}`): match by field name.
			for f in fields {
				if f.name == fi.value {
					decl_typ = f.typ
					found = true
					break
				}
			}
		} else if i < fields.len {
			// Positional initializer (`Box{'x'}`): the parser emits a `field_init` with
			// an empty name, so match by field order like `check_struct_init` does.
			decl_typ = fields[i].typ
			found = true
		}
		if !found {
			continue
		}
		sub := tc.substitute_generic_type(decl_typ, e_args, params)
		if sub is Unknown || sub is Void {
			continue
		}
		actual := tc.resolve_expr(tc.a.child(fi, 0), sub)
		if !tc.expr_receiver_compatible(tc.a.child(fi, 0), actual, sub) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) expr_receiver_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if !tc.receiver_compatible(actual, expected)
		&& !tc.implicit_ref_arg_compatible(expr_id, actual, expected) {
		return false
	}
	if actual is Pointer && expected !is Pointer
		&& !tc.explicit_address_arg_compatible(expr_id, actual, expected) {
		return false
	}
	return tc.generic_expected_expr_fields_compatible(expr_id, expected)
}

fn (tc &TypeChecker) implicit_ref_arg_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if !tc.expr_can_take_address(expr_id) {
		return false
	}
	actual_depth, actual_base := type_pointer_depth_and_base(actual)
	expected_depth, expected_base := type_pointer_depth_and_base(expected)
	// V permits implicit reference arguments to add every missing pointer layer.
	// Cgen materializes a typed `__ref_arg_N` chain rather than emitting only `&expr`.
	if expected_depth <= actual_depth {
		return false
	}
	return tc.type_compatible(actual_base, expected_base)
}

fn type_pointer_depth_and_base(typ Type) (int, Type) {
	mut depth := 0
	mut cur := typ
	for {
		if cur is Alias {
			cur = cur.base_type
			continue
		}
		if cur is Pointer {
			depth++
			cur = cur.base_type
			continue
		}
		break
	}
	return depth, cur
}

fn (tc &TypeChecker) explicit_address_arg_compatible(expr_id flat.NodeId, actual Type, expected Type) bool {
	if actual is Pointer {
		if tc.addressed_bare_generic_value_mismatch(expr_id, actual, expected) {
			return false
		}
		if expected is Pointer && tc.valid_node_id(expr_id) {
			node := tc.a.nodes[int(expr_id)]
			if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
				child_id := tc.a.child(&node, 0)
				child_type := tc.smartcast_type(child_id) or { actual.base_type }
				return tc.type_compatible(child_type, expected.base_type)
			}
		}
		return tc.type_compatible(actual.base_type, expected)
	}
	return false
}

fn (tc &TypeChecker) addressed_bare_generic_value_mismatch(expr_id flat.NodeId, _actual Type, expected Type) bool {
	if expected is Pointer || !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return false
	}
	child := tc.a.child_node(&node, 0)
	return child.kind == .struct_init && tc.bare_generic_literal_adopts(child.value, expected)
}

fn (tc &TypeChecker) explicit_mut_pointer_arg_compatible(expr_id flat.NodeId, expected Type) bool {
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if !node.is_mut || expected !is Pointer || !tc.expr_root_is_mutable_lvalue(expr_id) {
		return false
	}
	expected_ptr := expected as Pointer
	return tc.type_compatible(tc.resolve_type(expr_id), expected_ptr.base_type)
}

fn (tc &TypeChecker) expr_is_addressed_byvalue_arg(expr_id flat.NodeId) bool {
	if int(expr_id) < 0 || int(expr_id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_is_addressed_byvalue_arg(tc.a.child(&node, 0))
	}
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return false
	}
	child := tc.a.child_node(&node, 0)
	return child.kind in [.struct_init, .cast_expr, .call]
		|| (child.kind == .index && child.value == 'range')
}

fn (mut tc TypeChecker) expr_generic_expected_match(expr_id flat.NodeId, actual Type, expected Type) bool {
	return tc.generic_expected_type_match(actual, expected)
		&& tc.generic_expected_expr_fields_compatible(expr_id, expected)
}

fn (mut tc TypeChecker) generic_expected_expr_fields_compatible(expr_id flat.NodeId, expected Type) bool {
	if !tc.valid_node_id(expr_id) {
		return true
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.field_init, .expr_stmt, .paren {
			if node.children_count > 0 {
				return tc.generic_expected_expr_fields_compatible(tc.a.child(&node, 0), expected)
			}
		}
		.struct_init {
			if tc.bare_generic_literal_adopts(node.value, expected) {
				return tc.generic_literal_fields_compatible(node, expected)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count == 1 && expected is Pointer {
				child := tc.a.nodes[int(tc.a.child(&node, 0))]
				if child.kind == .struct_init
					&& tc.bare_generic_literal_adopts(child.value, expected) {
					return tc.generic_literal_fields_compatible(child, expected)
				}
			}
		}
		else {}
	}

	return true
}

// strip_generic_args_name returns the base name of a generic instance type
// (`Box[int]` -> `Box`); array/map types (leading `[`) yield the name unchanged.
fn strip_generic_args_name(name string) string {
	bracket := name.index_u8(`[`)
	if bracket <= 0 {
		return name
	}
	return name[..bracket]
}

// is_zero_literal reports whether is zero literal applies in types.
fn (tc &TypeChecker) is_zero_literal(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	return node.kind == .int_literal && node.value == '0'
}

// is_fn_pointer_type reports whether is fn pointer type applies in types.
fn is_fn_pointer_type(typ Type) bool {
	clean0 := typ
	mut clean := clean0
	if clean0 is Alias {
		clean = clean0.base_type
	}
	return clean is FnType
}

// fn_type_from_type converts fn type from type data for types.
fn fn_type_from_type(typ Type) ?FnType {
	if typ is FnType {
		return typ
	}
	if typ is Alias {
		return fn_type_from_type(typ.base_type)
	}
	if typ is Pointer {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn struct_type_from_type(typ Type) ?Struct {
	if typ is Struct {
		return typ
	}
	if typ is Alias {
		return struct_type_from_type(typ.base_type)
	}
	return none
}

fn (tc &TypeChecker) selector_declared_value_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	if !valid_string_data(node.value) {
		return none
	}
	if typ := tc.const_type_for_selector(node) {
		return typ
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.selector_fn_base_type(base_id) or { return none }
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		if typ := tc.struct_field_type(clean.name, node.value) {
			return typ
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return typ
		}
	}
	return none
}

fn (tc &TypeChecker) selector_const_fn_type(node flat.Node) ?FnType {
	if typ := tc.const_type_for_selector(node) {
		return fn_type_from_type(typ)
	}
	return none
}

fn (tc &TypeChecker) selector_field_fn_type(node flat.Node, base_type Type) ?FnType {
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		if typ := tc.struct_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return fn_type_from_type(typ)
		}
	}
	return none
}

// selector_fn_type supports selector fn type handling for TypeChecker.
fn (tc &TypeChecker) selector_fn_type(node flat.Node) ?FnType {
	typ := tc.selector_declared_value_type(node) or { return none }
	return fn_type_from_type(typ)
}

fn (tc &TypeChecker) selector_wrapped_fn_type(node flat.Node) ?FnType {
	typ := tc.selector_declared_value_type(node) or { return none }
	payload := match typ {
		OptionType { typ.base_type }
		ResultType { typ.base_type }
		else { return none }
	}
	return fn_type_from_type(payload)
}

fn (tc &TypeChecker) method_value_type(receiver_name string, method string) ?Type {
	method_name := '${receiver_name}.${method}'
	mut ret_type := tc.fn_ret_types[method_name] or { Type(void_) }
	mut params := tc.fn_param_types[method_name] or { []Type{} }
	if method_name !in tc.fn_ret_types && method_name !in tc.fn_param_types {
		// A concrete generic receiver (`Box[int]`) has its methods registered under the
		// open key (`Box[T].method`); resolve and substitute so a method *value* on a
		// generic struct is typed instead of reported as an unknown field.
		ci := tc.resolve_generic_struct_method(receiver_name, method) or { return none }
		ret_type = ci.return_type
		params = ci.params.clone()
	}
	mut bound_params := []Type{}
	if params.len > 1 {
		bound_params = params[1..].clone()
	}
	return Type(FnType{
		params:      bound_params
		return_type: ret_type
	})
}

fn (mut tc TypeChecker) check_pointer_receiver_method_value_safety(id flat.NodeId, node flat.Node, base_type Type) {
	if tc.unsafe_depth > 0 {
		return
	}
	clean := unalias_type(unwrap_pointer(base_type))
	if clean !is Struct || tc.type_has_declaration_attribute(clean, 'heap') {
		return
	}
	for method_name in receiver_method_name_candidates(clean, node.value, tc.cur_module) {
		params := tc.fn_param_types[method_name] or { continue }
		struct_name := method_name.all_before_last('.').all_after_last('.')
		if params.len == 0 || unalias_type(params[0]) !is Pointer {
			continue
		}
		tc.record_error_at(.assignment_mismatch,
			'method `${struct_name}.${node.value}` cannot be used as a variable outside `unsafe` blocks as its receiver might refer to an object stored on stack. Consider declaring `${struct_name}` as `@[heap]`.',
			id, node.pos)
		return
	}
}

fn (tc &TypeChecker) builtin_method_value_type(base_type Type, method string) ?Type {
	clean := unalias_and_unwrap_pointer_type(base_type)
	receiver_name := if clean is String {
		'string'
	} else if clean is Array {
		'array'
	} else if clean is Map {
		'map'
	} else {
		builtin_receiver_method_type_name(clean)
	}
	if receiver_name.len > 0 {
		if typ := tc.method_value_type(receiver_name, method) {
			return typ
		}
	}
	if method == 'str' && (clean is Primitive || clean is Char || clean is Rune) {
		return Type(FnType{
			params:      []Type{}
			return_type: Type(string_)
		})
	}
	info := tc.builtin_receiver_method_call_info(base_type, method) or { return none }
	mut bound_params := []Type{}
	if info.params.len > 1 {
		bound_params = info.params[1..].clone()
	}
	return Type(FnType{
		params:      bound_params
		return_type: info.return_type
	})
}

// selector_fn_base_type supports selector fn base type handling for TypeChecker.
fn (tc &TypeChecker) selector_fn_base_type(base_id flat.NodeId) ?Type {
	if typ := tc.smartcast_type(base_id) {
		return typ
	}
	if int(base_id) >= 0 {
		base_node := tc.a.nodes[int(base_id)]
		if base_node.kind == .ident {
			if typ := tc.non_file_scope_type(base_node.value) {
				if typ is Alias {
					return typ
				}
			}
		}
	}
	if typ := tc.cached_expr_type(base_id) {
		return typ
	}
	if int(base_id) < 0 {
		return none
	}
	base_node := tc.a.nodes[int(base_id)]
	if base_node.typ.len > 0 && base_node.typ != 'unknown' {
		return tc.parse_type(base_node.typ)
	}
	if base_node.kind == .call {
		if typ := tc.resolved_call_type(base_id) {
			return typ
		}
		if typ := tc.direct_call_return_type(base_node) {
			return typ
		}
		return none
	}
	return tc.resolve_type(base_id)
}

// direct_call_return_type supports direct call return type handling for TypeChecker.
fn (tc &TypeChecker) direct_call_return_type(node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .index && fn_node.children_count >= 2 && fn_node.value != 'range' {
		base_node := tc.a.child_node(fn_node, 0)
		name := tc.generic_call_base_name(base_node) or { return none }
		type_args := tc.generic_call_type_arg_names(fn_node)
		if type_args.len == 0 {
			return none
		}
		if is_decode_call_name(name) && type_args.len == 1 {
			return Type(ResultType{
				base_type: tc.parse_type(type_args[0])
			})
		}
		if info := tc.explicit_generic_call_info(name, false, type_args) {
			return info.return_type
		}
		return none
	}
	if fn_node.kind == .ident {
		if local_name := tc.local_bare_fn_key(fn_node.value) {
			if typ := tc.fn_ret_types[local_name] {
				return typ
			}
		}
		if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
			if typ := tc.fn_ret_types[imported_name] {
				return typ
			}
		}
		if typ := tc.fn_ret_types[fn_node.value] {
			return typ
		}
		return none
	}
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_node := tc.a.child_node(fn_node, 0)
	if base_node.kind == .ident {
		base_is_value := tc.ident_resolves_to_value(base_node.value)
		if !base_is_value {
			if resolved := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved}.${fn_node.value}'
				if typ := tc.fn_ret_types[mod_name] {
					return typ
				}
			}
			if static_name := tc.static_assoc_fn_key_for_base(base_node.value, fn_node.value) {
				return tc.fn_ret_types[static_name] or { none }
			}
		}
		return none
	}
	if base_node.kind == .selector {
		inner := tc.a.child_node(base_node, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
			if typ := tc.fn_ret_types[full_name] {
				return typ
			}
			if static_name := tc.static_assoc_fn_key_for_base('${mod_name}.${base_node.value}',
				fn_node.value)
			{
				return tc.fn_ret_types[static_name] or { none }
			}
		}
	}
	return none
}

fn (tc &TypeChecker) spawn_child_call_return_type(node flat.Node) ?Type {
	if ret := tc.direct_call_return_type(node) {
		return ret
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base_type := tc.resolve_type(tc.a.child(fn_node, 0))
		clean_base := unwrap_all_pointers(base_type)
		for candidate in receiver_method_name_candidates(clean_base, fn_node.value, tc.cur_module) {
			if ret := tc.fn_ret_types[candidate] {
				return ret
			}
		}
		if method_key := tc.concrete_method_signature_key(clean_base.name(), fn_node.value) {
			if ret := tc.fn_ret_types[method_key] {
				return ret
			}
		}
	}
	if fn_node.kind != .ident {
		return none
	}
	mut candidates := []string{}
	candidates << fn_node.value
	qname := tc.qualify_fn_name(fn_node.value)
	if qname != fn_node.value {
		candidates << qname
	}
	if tc.cur_module.len > 0 {
		candidates << '${tc.cur_module}.${fn_node.value}'
	}
	candidates << 'main.${fn_node.value}'
	for candidate in candidates {
		if ret := tc.fn_ret_types[candidate] {
			return ret
		}
	}
	return none
}

// module_const_receiver_method_name supports module_const_receiver_method_name handling in types.
fn (tc &TypeChecker) module_const_receiver_method_name(base_node flat.Node, method string) ?string {
	if base_node.kind != .selector || base_node.children_count == 0 || method.len == 0 {
		return none
	}
	inner := tc.a.child_node(&base_node, 0)
	if inner.kind != .ident {
		return none
	}
	mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
	const_name := '${mod_name}.${base_node.value}'
	mut const_type := tc.const_types[const_name] or { Type(Unknown{}) }
	const_type = tc.const_type_from_initializer(const_name, const_type)
	if const_type is Unknown && base_node.value == 'scanner_matcher' {
		const_type = Type(Struct{
			name: '${mod_name}.KeywordsMatcherTrie'
		})
	}
	clean := unwrap_pointer(const_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	for method_name in receiver_method_name_candidates(clean, method, mod_name) {
		if method_name in tc.fn_ret_types {
			return method_name
		}
	}
	return none
}

// valid_string_data supports valid string data handling for types.
fn valid_string_data(s string) bool {
	if s.len == 0 {
		return true
	}
	ptr := unsafe { u64(voidptr(s.str)) }
	return ptr >= 4096 && ptr < 281474976710656 && s.len < 1048576
}

// clone_smartcasts supports clone smartcasts handling for types.
fn clone_smartcasts(src map[string]Type) map[string]Type {
	mut dst := map[string]Type{}
	for key, typ in src {
		if valid_string_data(key) {
			dst[key] = typ
		}
	}
	return dst
}

// array_elem_type supports array elem type handling for types.
fn array_elem_type(arr Array) Type {
	return arr.elem_type
}

// array_like_elem_type returns the element type of an `Array` or `ArrayFixed`.
fn array_like_elem_type(t Type) ?Type {
	if t is Array {
		return t.elem_type
	}
	if t is ArrayFixed {
		return t.elem_type
	}
	if t is Alias {
		return array_like_elem_type(t.base_type)
	}
	return none
}

// if_branch_types_compatible reports whether two if-expression branch types are
// compatible. Bare array literals (`[a, b, c]`) resolve to a fixed `T[n]`, but V
// treats them as dynamic `[]T`; two *literal* branches with compatible element
// types must therefore not be flagged as a mismatch merely because their lengths
// differ. The length-agnostic relaxation is limited to literal tails: genuine
// fixed-array values keep their length (handled by `type_compatible` above), so
// e.g. `[2]int` vs `[3]int` branches still mismatch.
fn (tc &TypeChecker) if_branch_types_compatible(a Type, b Type, a_is_array_lit bool, b_is_array_lit bool) bool {
	if (a is None && b is ResultType) || (b is None && a is ResultType) {
		return false
	}
	if (a is OptionType) != (b is OptionType) || (a is ResultType) != (b is ResultType) {
		return false
	}
	if tc.type_compatible(a, b) || tc.type_compatible(b, a) {
		return true
	}
	if !a_is_array_lit || !b_is_array_lit {
		return false
	}
	a_elem := array_like_elem_type(a) or { return false }
	b_elem := array_like_elem_type(b) or { return false }
	return tc.type_compatible(a_elem, b_elem) || tc.type_compatible(b_elem, a_elem)
}

fn (tc &TypeChecker) if_branch_multi_return_compatible(a Type, a_branch flat.NodeId, b Type, b_branch flat.NodeId) bool {
	if a_multi := multi_return_payload_type(a) {
		b_types := tc.branch_explicit_comma_tail_types(b_branch) or {
			if b_multi := multi_return_payload_type(b) {
				return tc.multi_return_types_compatible(a_multi.types, b_multi.types)
			}
			return false
		}
		return tc.multi_return_types_compatible(a_multi.types, b_types)
	}
	if b_multi := multi_return_payload_type(b) {
		a_types := tc.branch_explicit_comma_tail_types(a_branch) or { return false }
		return tc.multi_return_types_compatible(a_types, b_multi.types)
	}
	return false
}

fn (tc &TypeChecker) multi_return_types_compatible(a []Type, b []Type) bool {
	if a.len != b.len {
		return false
	}
	for i, typ in a {
		if !tc.type_compatible(typ, b[i]) && !tc.type_compatible(b[i], typ) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) if_branch_types_compatible_with_expected(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId, expected Type) bool {
	if expected is Void || expected is Unknown {
		return false
	}
	return tc.if_branch_type_compatible_with_context(a, a_tail, expected)
		&& tc.if_branch_type_compatible_with_context(b, b_tail, expected)
}

fn (tc &TypeChecker) if_branch_type_compatible_with_context(actual Type, tail_id flat.NodeId, expected Type) bool {
	if actual is None {
		return (expected is OptionType || is_ierror_type(expected))
			&& tc.branch_tail_is_none_literal(tail_id)
	}
	if is_option_void_type(actual) {
		return (expected is OptionType || is_ierror_type(expected))
			&& tc.branch_tail_is_none_literal(tail_id)
	}
	if is_result_void_type(actual) {
		return (expected is OptionType || expected is ResultType || is_ierror_type(expected))
			&& tc.branch_tail_is_error_literal(tail_id)
	}
	if is_ierror_type(actual) {
		return (expected is OptionType || expected is ResultType || is_ierror_type(expected))
			&& tc.branch_tail_is_error_literal(tail_id)
	}
	if expected is OptionType && tc.type_compatible(actual, expected.base_type) {
		return true
	}
	return tc.type_compatible(actual, expected)
}

fn (mut tc TypeChecker) constant_if_selected_branch_compatible_with_expected(node flat.Node, expected Type) bool {
	selected_tail := tc.constant_if_selected_tail(node) or { return false }
	if !tc.valid_node_id(selected_tail) {
		return false
	}
	actual := tc.resolve_expr(selected_tail, expected)
	return tc.return_type_compatible(selected_tail, actual, expected)
		|| tc.if_branch_type_compatible_with_context(actual, selected_tail, expected)
}

fn (tc &TypeChecker) constant_if_selected_tail(node flat.Node) ?flat.NodeId {
	if node.kind != .if_expr || node.children_count <= 2 {
		return none
	}
	cond_id := tc.a.child(&node, 0)
	if !tc.valid_node_id(cond_id) {
		return none
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind != .bool_literal {
		return none
	}
	selected_id := tc.a.child(&node, if cond.value == 'true' { 1 } else { 2 })
	if tc.valid_node_id(selected_id) && tc.a.nodes[int(selected_id)].kind == .if_expr {
		return selected_id
	}
	return tc.branch_tail_expr_id(selected_id)
}

fn (tc &TypeChecker) branch_tail_is_none_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.branch_tail_is_none_literal(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) branch_tail_is_error_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .call {
		return tc.call_display_name(node) in ['error', 'error_with_code']
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.branch_tail_is_error_literal(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) branch_failure_literal_matches_context(id flat.NodeId, expected Type) bool {
	if tc.branch_tail_is_none_literal(id) {
		return expected is OptionType || is_ierror_type(expected)
	}
	if tc.branch_tail_is_error_literal(id) {
		return expected is OptionType || expected is ResultType || is_ierror_type(expected)
	}
	return true
}

fn if_branch_type_needs_context(typ Type) bool {
	if typ is None || is_ierror_type(typ) {
		return true
	}
	if typ is OptionType {
		return typ.base_type is Void
	}
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

fn inferred_contextual_if_type(a Type, b Type) ?Type {
	if a is None {
		return optional_if_type_from_value(b)
	}
	if b is None {
		return optional_if_type_from_value(a)
	}
	if is_option_void_type(a) {
		return optional_if_type_from_value(b)
	}
	if is_option_void_type(b) {
		return optional_if_type_from_value(a)
	}
	if is_ierror_type(a) {
		return result_if_type_from_value(b)
	}
	if is_ierror_type(b) {
		return result_if_type_from_value(a)
	}
	if is_result_void_type(a) {
		return result_if_type_from_value(b)
	}
	if is_result_void_type(b) {
		return result_if_type_from_value(a)
	}
	return none
}

fn is_option_void_type(typ Type) bool {
	if typ is OptionType {
		return typ.base_type is Void
	}
	return false
}

fn is_result_void_type(typ Type) bool {
	if typ is ResultType {
		return typ.base_type is Void
	}
	return false
}

fn optional_if_type_from_value(value Type) ?Type {
	if value is OptionType {
		if value.base_type is Void {
			return none
		}
		return value
	}
	if value is ResultType {
		return none
	}
	if if_branch_type_needs_context(value) || value is Void || value is Unknown {
		return none
	}
	return Type(OptionType{
		base_type: value
	})
}

fn result_if_type_from_value(value Type) ?Type {
	if value is ResultType {
		if value.base_type is Void {
			return none
		}
		return value
	}
	if value is OptionType || if_branch_type_needs_context(value) || value is Void
		|| value is Unknown {
		return none
	}
	return Type(ResultType{
		base_type: value
	})
}

// branch_tail_is_array_literal reports whether a branch's value tail is a bare
// array literal (`[a, b, c]`) — directly, or through a const whose initializer is
// one. V types such values as dynamic `[]T` regardless of element count, so they
// must not constrain if-branch length compatibility. Explicit fixed-array
// initializers (`[N]T{...}`, parsed as `.array_init`) are genuine fixed arrays and
// keep their length.
fn (tc &TypeChecker) branch_tail_is_array_literal(id flat.NodeId) bool {
	return tc.expr_is_bare_array_literal(tc.branch_tail_expr_id(id))
}

// expr_is_bare_array_literal reports whether `id` is a bare `[a, b, c]` literal,
// directly or through a single const reference.
fn (tc &TypeChecker) expr_is_bare_array_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .array_literal {
		return true
	}
	if node.kind == .ident {
		for cand in [tc.qualify_name(node.value), node.value] {
			if expr_id := tc.const_exprs[cand] {
				if tc.valid_node_id(expr_id) {
					return tc.a.nodes[int(expr_id)].kind == .array_literal
				}
			}
		}
	}
	return false
}

// fixed_array_elem_type supports fixed array elem type handling for types.
fn fixed_array_elem_type(arr ArrayFixed) Type {
	return arr.elem_type
}

fn fixed_array_type_contains_map(typ Type) bool {
	if typ is ArrayFixed {
		return fixed_array_type_contains_map(typ.elem_type)
	}
	return typ is Map
}

// map_value_type supports map value type handling for types.
fn map_value_type(m Map) Type {
	return m.value_type
}

// pointer_base_type supports pointer base type handling for types.
fn pointer_base_type(p Pointer) Type {
	return p.base_type
}

// fn_param_type supports fn param type handling for types.
fn fn_param_type(f FnType, idx int) Type {
	return f.params[idx]
}

// is_known_call reports whether is known call applies in types.
fn (tc &TypeChecker) is_known_call(node flat.Node) bool {
	if node.children_count == 0 {
		return true
	}
	fn_node := tc.a.child_node(&node, 0)
	if node.typ.len > 0 {
		if fn_node.kind == .index && fn_node.value != 'range' {
			return tc.explicit_generic_call_target_is_known(node)
		}
		if fn_node.kind != .ident {
			return true
		}
	}
	if fn_node.kind == .selector {
		base_node := tc.a.child_node(fn_node, 0)
		if base_node.kind == .ident {
			if base_node.value == 'C' {
				return true
			}
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types || mod_name in tc.sum_types || mod_name in tc.structs
					|| mod_name in tc.enum_names {
					return true
				}
			}
			if base_node.value in tc.structs || base_node.value in tc.enum_names {
				qname := tc.qualify_name(base_node.value)
				if '${qname}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			} else {
				qname := tc.qualify_name(base_node.value)
				if qname in tc.structs || qname in tc.enum_names {
					if '${qname}.${fn_node.value}' in tc.fn_ret_types {
						return true
					}
				}
			}
		} else if base_node.kind == .selector {
			inner := tc.a.child_node(base_node, 0)
			if inner.kind == .ident {
				mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
				if '${mod_name}.${base_node.value}.${fn_node.value}' in tc.fn_ret_types {
					return true
				}
			}
		}
		if _ := tc.selector_const_fn_type(fn_node) {
			return true
		}
		base_id := tc.a.child(fn_node, 0)
		base_type := tc.selector_fn_base_type(base_id) or { tc.resolve_type(base_id) }
		if _ := tc.selector_field_fn_type(fn_node, base_type) {
			return true
		}
		if fn_node.value == 'hex' && tc.type_is_pointer_receiver(base_type) {
			return false
		}
		clean_type := unwrap_pointer(base_type)
		if clean_type is Array || clean_type is ArrayFixed {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return tc.is_known_array_receiver_method(clean_type, fn_node.value)
		}
		if clean_type is Map {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return true
		}
		if clean_type is String {
			if fn_node.value == 'hex' {
				return tc.is_builtin_hex_receiver(base_type)
			}
			return 'string.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Alias {
			mname := '${clean_type.name}.${fn_node.value}'
			if mname in tc.fn_ret_types {
				return true
			}
			base_name := resolve_type_name_for_method(clean_type.base_type)
			if base_name.len > 0 {
				for base_mname in receiver_method_name_candidates(clean_type.base_type,
					fn_node.value, tc.cur_module) {
					if base_mname in tc.fn_ret_types {
						return true
					}
				}
			}
		}
		if clean_type is Struct {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Interface {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is SumType {
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Enum {
			if fn_node.value == 'str' {
				return true
			}
			return '${clean_type.name}.${fn_node.value}' in tc.fn_ret_types
		}
		if clean_type is Primitive {
			mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
			return mname in tc.fn_ret_types
		}
		return false
	}
	if fn_node.kind == .ident {
		if typ := tc.cur_scope.lookup(fn_node.value) {
			return typ is FnType
		}
		qfn := tc.qualify_fn_name(fn_node.value)
		if qfn in tc.fn_ret_types || fn_node.value in tc.fn_ret_types {
			return true
		}
		if _ := tc.resolve_selective_import_symbol(fn_node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) is_known_array_receiver_method(receiver Type, method string) bool {
	if receiver is Array {
		for mname in receiver_method_name_candidates(receiver, method, tc.cur_module) {
			if mname in tc.fn_ret_types {
				return true
			}
		}
		// Keep this in sync with the synthetic array receiver methods handled in
		// resolve_call_info/resolve_type, including `[]thread T.wait()`.
		return method in ['first', 'last', 'pop', 'pop_left', 'contains', 'join', 'index',
			'last_index', 'repeat', 'repeat_to_depth', 'delete', 'delete_last', 'clear', 'insert',
			'prepend', 'filter', 'map', 'any', 'all', 'count', 'sort_with_compare',
			'sorted_with_compare', 'sort', 'sorted', 'clone', 'reverse', 'reverse_in_place', 'equals',
			'bytestr', 'wait']
	}
	if receiver is ArrayFixed {
		array_type := Type(Array{
			elem_type: receiver.elem_type
		})
		for mname in receiver_method_name_candidates(array_type, method, tc.cur_module) {
			if mname in tc.fn_ret_types {
				return true
			}
		}
		return method == 'pointers'
	}
	return false
}

fn (tc &TypeChecker) is_unsupported_hex_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'hex' || fn_node.children_count == 0 {
		return false
	}
	base_id := tc.a.child(fn_node, 0)
	if tc.receiver_expr_is_pointer(base_id) {
		return true
	}
	base_type := tc.resolve_type(base_id)
	return !tc.is_builtin_hex_receiver(base_type)
}

fn (tc &TypeChecker) call_has_ambiguous_selective_import(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .index && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		return base.kind == .ident && tc.selective_import_symbol_is_ambiguous(base.value)
	}
	return fn_node.kind == .ident && tc.selective_import_symbol_is_ambiguous(fn_node.value)
}

// call_display_name updates call display name state for TypeChecker.
fn (tc &TypeChecker) call_display_name(node flat.Node) string {
	if node.children_count == 0 {
		return '<missing>'
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind == .ident {
		return fn_node.value
	}
	if fn_node.kind == .selector && fn_node.children_count > 0 {
		base := tc.a.child_node(fn_node, 0)
		if base.value.len > 0 {
			return '${base.value}.${fn_node.value}'
		}
	}
	if fn_node.kind in [.index, .prefix, .array_init] {
		type_name := tc.type_expr_name(tc.a.child(&node, 0))
		if type_name.len > 0 {
			return type_name
		}
	}
	return fn_node.value
}

// check_if_expr validates check if expr state for types.
fn (mut tc TypeChecker) check_if_expr(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	value_context := !tc.is_statement_node(id)
	cond_id := tc.a.child(&node, 0)
	condition := tc.a.node(cond_id)
	if condition.kind == .paren {
		tc.record_warning_at(.condition_mismatch,
			'unnecessary `()` in `if` condition, use `if expr {` instead of `if (expr) {`.',
			cond_id, tc.if_parenthesized_condition_pos(condition))
	}
	saved_mut_local_owners := tc.fn_context.mut_local_owners.clone()
	defer {
		tc.fn_context.mut_local_owners = saved_mut_local_owners.clone()
	}
	tc.enable_explicit_mut_smartcasts(cond_id)
	guard_bindings := tc.check_condition(cond_id)
	tc.record_constant_condition_diagnostics(cond_id)
	unsafe_alias_base := tc.fn_context.unsafe_reference_alias_owners.clone()
	mut unsafe_alias_paths := []map[string]bool{}
	mut condition_is_true := false
	mut condition_is_false := false
	if value := tc.constant_bool_value(cond_id) {
		condition_is_true = value
		condition_is_false = !value
	}
	smartcasts := tc.extract_smartcasts(cond_id)
	then_id := tc.a.child(&node, 1)
	then_uses_block_scope := guard_bindings.len == 0 && tc.valid_node_id(then_id)
		&& tc.a.nodes[int(then_id)].kind == .block
	saved_smartcasts := clone_smartcasts(tc.smartcasts)
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			tc.smartcasts[sc.name] = sc.typ
		}
	}
	$if ownership ? {
		if value_context {
			tc.ownership_begin_value_branch_group()
		} else {
			tc.ownership_begin_branch_group()
		}
	}
	tc.push_scope()
	$if ownership ? {
		if !then_uses_block_scope {
			tc.ownership_mark_scope_node(then_id)
		}
	}
	for binding in guard_bindings {
		$if ownership ? {
			tc.ownership_note_binding(binding.name, binding.typ, cond_id)
		}
		owner := tc.cur_scope.insert_with_owner(binding.name, binding.typ)
		if binding.is_mut {
			tc.fn_context.mut_local_owners[binding.name] = owner
		}
	}
	tc.check_branch_node(then_id, value_context)
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_branch(then_id)
	}
	if !condition_is_false && !tc.stmt_definitely_returns(then_id) {
		unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
	}
	tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_base.clone()
	tc.smartcasts = clone_smartcasts(saved_smartcasts)
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		else_smartcasts := tc.extract_else_branch_smartcasts(cond_id)
		for sc in else_smartcasts {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		$if ownership ? {
			tc.ownership_begin_branch()
		}
		if tc.valid_node_id(else_id) && tc.a.node(else_id).kind == .if_expr {
			tc.fn_context.mut_local_owners = saved_mut_local_owners.clone()
		}
		tc.check_branch_node(else_id, value_context)
		$if ownership ? {
			tc.ownership_end_branch(else_id)
		}
		if !condition_is_true && !tc.stmt_definitely_returns(else_id) {
			unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
		}
		if else_smartcasts.len > 0 {
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
		}
	} else {
		if !condition_is_true {
			unsafe_alias_paths << unsafe_alias_base.clone()
		}
		$if ownership ? {
			tc.ownership_add_branch_group_base()
		}
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_base)
	$if ownership ? {
		tc.ownership_end_branch_group()
	}
	if value_context {
		tc.check_if_value_requirements(id, node, then_id)
	}
	if !value_context {
		return
	}
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			tc.smartcasts[sc.name] = sc.typ
		}
	}
	then_type := tc.branch_tail_type(then_id)
	tc.smartcasts = clone_smartcasts(saved_smartcasts)
	mut else_type := Type(void_)
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		else_type = tc.branch_tail_type(else_id)
	}
	if then_type !is Void && else_type !is Void {
		else_id := tc.a.child(&node, 2)
		if tc.branch_has_value_tail(then_id) && tc.branch_has_value_tail(else_id)
			&& !tc.if_branch_types_compatible(then_type, else_type, tc.branch_tail_is_array_literal(then_id), tc.branch_tail_is_array_literal(else_id))
			&& !tc.if_branch_multi_return_compatible(then_type, then_id, else_type, else_id) {
			if tc.if_branch_empty_array_compatible(then_type, then_id, else_type, else_id) {
				return
			}
			then_tail := tc.branch_tail_expr_id(then_id)
			else_tail := tc.branch_tail_expr_id(else_id)
			if tc.if_branch_none_has_option_context(then_type, then_tail, else_type, else_tail) {
				if expected := tc.expected_context_for_expr(id) {
					if expected is OptionType {
						return
					}
				}
			}
			if tc.if_branch_error_has_result_context(then_type, else_type) {
				if expected := tc.expected_context_for_expr(id) {
					if expected is ResultType || is_ierror_type(expected) {
						return
					}
				}
			}
			if tc.if_branch_enum_shorthand_compatible(then_type, then_tail, else_type, else_tail) {
				return
			}
			if expected := tc.expected_context_for_expr(id) {
				if tc.constant_if_selected_branch_compatible_with_expected(node, expected) {
					return
				}
				branches_match_expected := tc.if_branch_types_compatible_with_expected(then_type,
					then_tail, else_type, else_tail, expected)
				if branches_match_expected {
					return
				}
			}
			if tc.should_diagnose(id) {
				pos := token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 2)
				if (then_type is OptionType) != (else_type is OptionType)
					|| (then_type is ResultType) != (else_type is ResultType) {
					mut then_name := then_type.name()
					mut else_name := else_type.name()
					if then_types := tc.branch_explicit_comma_tail_types(then_id) {
						if else_types := tc.branch_explicit_comma_tail_types(else_id) {
							if then_types.len == else_types.len {
								then_name = Type(MultiReturn{
									types: then_types
								}).name()
								else_name = Type(MultiReturn{
									types: else_types
								}).name()
							}
						}
					}
					tc.record_error_at(.if_branch_mismatch,
						'mismatched types `${then_name}` and `${else_name}`', id, pos)
				} else {
					then_name := tc.diagnostic_expr_type_name(then_tail, then_type)
					else_name := tc.diagnostic_expr_type_name(else_tail, else_type)
					tc.record_error_at(.if_branch_mismatch,
						'mismatched types `${then_name}` and `${else_name}`', id, pos)
				}
			}
		}
	}
}

fn (tc &TypeChecker) if_parenthesized_condition_pos(condition flat.Node) token.Pos {
	file := tc.a.source_files[condition.pos.id] or { return condition.pos }
	source := tc.source_texts_by_file[file.name] or { return condition.pos }
	condition_start := int_max(0, int_min(condition.pos.offset, source.len))
	condition_end := int_max(condition_start, int_min(condition.pos.end, source.len))
	position := file.position(condition.pos)
	line_start := file.line_start(position.line)
	mut start := condition_start
	if line_start < condition_start {
		prefix := source[line_start..condition_start]
		if relative_if := prefix.last_index('if') {
			start = line_start + relative_if
			before_if := prefix[..relative_if].trim_right(' \t')
			if before_if.ends_with('else') {
				if relative_else := prefix[..relative_if].last_index('else') {
					start = line_start + relative_else
				}
			}
		}
	}
	return token.new_span(condition.pos.id, start, condition_end)
}

fn (tc &TypeChecker) branch_explicit_comma_tail_types(id flat.NodeId) ?[]Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .block && node.value == 'comma_exprs' {
		mut types := []Type{cap: node.children_count}
		for i in 0 .. node.children_count {
			stmt_id := tc.a.child(node, i)
			if !tc.valid_node_id(stmt_id) {
				return none
			}
			stmt := tc.a.node(stmt_id)
			if stmt.kind != .expr_stmt || stmt.children_count != 1 {
				return none
			}
			value_id := tc.a.child(stmt, 0)
			typ := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
			if typ is Unknown || !type_has_runtime_value(typ) {
				return none
			}
			types << typ
		}
		return if types.len > 1 { types } else { none }
	}
	if node.kind !in [.block, .match_branch] || node.children_count == 0 {
		return none
	}
	return tc.branch_explicit_comma_tail_types(tc.a.child(node, node.children_count - 1))
}

fn (mut tc TypeChecker) check_if_value_requirements(id flat.NodeId, node flat.Node, then_id flat.NodeId) {
	tc.check_empty_or_value_tail(then_id)
	if !tc.branch_has_value_tail(then_id) && !tc.stmt_definitely_returns(then_id) {
		tc.record_error_at(.if_branch_mismatch,
			'`if` expression requires an expression as the last statement of every branch',
			then_id, tc.if_branch_missing_value_pos(then_id))
	}
	if node.children_count <= 2 {
		pos := token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 2)
		tc.record_error_at(.if_branch_mismatch, '`if` expression needs `else` clause', id, pos)
		return
	}
	else_id := tc.a.child(&node, 2)
	tc.check_empty_or_value_tail(else_id)
	if !tc.branch_has_value_tail(else_id) && !tc.stmt_definitely_returns(else_id) {
		tc.record_error_at(.if_branch_mismatch,
			'`if` expression requires an expression as the last statement of every branch',
			else_id, tc.if_branch_missing_value_pos(else_id))
	}
	if tc.if_expr_all_tails_are_none(node) {
		pos := token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 2)
		tc.record_error_at(.if_branch_mismatch,
			'invalid if expression, must supply at least one value other than `none`', id, pos)
	}
}

fn (tc &TypeChecker) if_branch_missing_value_pos(branch_id flat.NodeId) token.Pos {
	if !tc.valid_node_id(branch_id) {
		return token.Pos{}
	}
	branch := tc.a.node(branch_id)
	if branch.kind != .block || branch.children_count == 0 {
		return tc.if_branch_header_pos(branch_id)
	}
	tail_id := tc.a.child(branch, branch.children_count - 1)
	if !tc.valid_node_id(tail_id) {
		return tc.if_branch_header_pos(branch_id)
	}
	tail := tc.a.node(tail_id)
	if tail.kind in [.assign, .decl_assign, .selector_assign, .index_assign]
		&& tail.children_count >= 2 {
		lhs_id := tc.a.child(tail, 0)
		rhs_id := tc.a.child(tail, 1)
		if tail.op != .assign {
			return tc.compound_assignment_operator_pos(lhs_id, rhs_id,
				assignment_operator_text(tail.op))
		}
		return tc.assignment_operator_pos(*tail, lhs_id, rhs_id)
	}
	return tail.pos
}

fn (mut tc TypeChecker) check_empty_or_value_tail(branch_id flat.NodeId) {
	tail_id := tc.branch_tail_expr_id(branch_id)
	if !tc.valid_node_id(tail_id) {
		return
	}
	tail := tc.a.node(tail_id)
	if tail.kind == .if_expr
		&& tc.errors.any(it.msg == '`if` expression requires an expression as the last statement of every branch'
		&& it.pos.id == tail.pos.id && it.pos.offset >= tail.pos.offset
		&& it.pos.end <= tail.pos.end) {
		return
	}
	if tail.kind == .or_expr && tail.children_count >= 2 {
		fallback := tc.a.child_node(tail, 1)
		if fallback.kind == .block && fallback.children_count == 0 {
			source_id := tc.a.child(tail, 0)
			source := tc.a.node(source_id)
			tc.record_error_at(.if_branch_mismatch,
				'the final expression in `if` or `match`, must have a value of a non-void type',
				source_id, source.pos)
			return
		}
	}
	if unalias_type(tc.resolve_type(tail_id)) is Void && !tc.branch_tail_never_returns(branch_id)
		&& !tc.expr_subtree_has_undefined_variable_error(tail_id) {
		tc.record_error_at(.if_branch_mismatch,
			'the final expression in `if` or `match`, must have a value of a non-void type',
			tail_id, tail.pos)
	}
}

fn (tc &TypeChecker) if_expr_all_tails_are_none(node flat.Node) bool {
	if node.children_count <= 2 {
		return false
	}
	then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
	if !tc.branch_tail_is_none_literal(then_tail) {
		return false
	}
	else_id := tc.a.child(&node, 2)
	else_node := tc.a.node(else_id)
	if else_node.kind == .if_expr {
		return tc.if_expr_all_tails_are_none(else_node)
	}
	return tc.branch_tail_is_none_literal(tc.branch_tail_expr_id(else_id))
}

fn (tc &TypeChecker) if_branch_header_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	brace := node.pos.offset
	if brace < 0 || brace >= source.len {
		return node.pos
	}
	prefix := source[..brace]
	line_start := if idx := prefix.last_index('\n') { idx + 1 } else { 0 }
	line_prefix := source[line_start..brace]
	start := if idx := line_prefix.last_index('else') {
		line_start + idx
	} else if idx := line_prefix.last_index('if') {
		line_start + idx
	} else {
		brace
	}
	mut end := brace
	for end > start && source[end - 1] in [` `, `\t`] {
		end--
	}
	return token.new_span(node.pos.id, start, end)
}

fn (mut tc TypeChecker) enable_explicit_mut_smartcasts(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .is_expr && node.children_count > 0 {
		tc.enable_explicit_mut_smartcast_target(tc.a.child(node, 0))
		return
	}
	if node.kind == .infix && node.op in [.logical_and, .logical_or] {
		for i in 0 .. node.children_count {
			tc.enable_explicit_mut_smartcasts(tc.a.child(node, i))
		}
		return
	}
	if node.kind == .infix && node.op in [.eq, .ne] {
		for i in 0 .. node.children_count {
			tc.enable_explicit_mut_smartcast_target(tc.a.child(node, i))
		}
		return
	}
	if node.kind == .paren && node.children_count > 0 {
		tc.enable_explicit_mut_smartcasts(tc.a.child(node, 0))
	}
}

fn (mut tc TypeChecker) enable_explicit_mut_smartcast_target(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if !node.is_mut {
		return
	}
	root_id := tc.lvalue_root_ident(id) or { return }
	root := tc.a.node(root_id)
	if root.value.len == 0 || root.value == '_' {
		return
	}
	owner := tc.cur_scope.lookup_owner(root.value) or { return }
	if !tc.ident_is_mutable_lvalue(root.value) {
		tc.record_error_at(.assignment_mismatch,
			'`${root.value}` is immutable, declare it with `mut` to make it mutable', root_id,
			tc.node_value_diagnostic_pos(root_id))
	}
	tc.fn_context.mut_local_owners[root.value] = owner
}

fn (mut tc TypeChecker) record_constant_condition_diagnostics(id flat.NodeId) {
	value := tc.constant_bool_value(id) or { return }
	pos := tc.constant_condition_diagnostic_pos(id)
	tc.record_notice_at(.condition_mismatch, if value {
		'condition is always true'
	} else {
		'condition is always false'
	}, id, pos)
	if value && tc.is_always_true_self_comparison(id) {
		tc.record_warning_at(.condition_mismatch,
			'self-comparison in `if` condition is always true; following branches may be unreachable',
			id, pos)
	}
}

fn (tc &TypeChecker) constant_condition_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind != .infix || node.op !in [.logical_and, .logical_or] {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	if node.pos.offset < 0 || node.pos.offset > source.len {
		return node.pos
	}
	prefix := source[..node.pos.offset]
	line_start := if idx := prefix.last_index('\n') { idx + 1 } else { 0 }
	return token.new_span(node.pos.id, line_start, int_min(line_start + 1, source.len))
}

fn (tc &TypeChecker) constant_bool_value(id flat.NodeId) ?bool {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.constant_bool_value(tc.a.child(node, 0))
	}
	if node.kind == .bool_literal {
		return node.value == 'true'
	}
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	lhs_id := tc.a.child(node, 0)
	rhs_id := tc.a.child(node, 1)
	if node.op == .logical_and {
		lhs := tc.constant_bool_value(lhs_id)?
		rhs := tc.constant_bool_value(rhs_id)?
		return lhs && rhs
	}
	if node.op == .logical_or {
		lhs := tc.constant_bool_value(lhs_id)?
		rhs := tc.constant_bool_value(rhs_id)?
		return lhs || rhs
	}
	return tc.constant_comparison_value(lhs_id, rhs_id, node.op)
}

fn (tc &TypeChecker) constant_comparison_value(lhs_id flat.NodeId, rhs_id flat.NodeId, op flat.Op) ?bool {
	if op !in [.eq, .ne, .lt, .le, .gt, .ge] {
		return none
	}
	if tc.same_simple_comparison_expr(lhs_id, rhs_id) {
		return match op {
			.eq, .le, .ge { true }
			.ne, .lt, .gt { false }
			else { false }
		}
	}
	lhs := tc.constant_scalar_value(lhs_id)?
	rhs := tc.constant_scalar_value(rhs_id)?
	if lhs.kind != rhs.kind {
		return none
	}
	if lhs.kind in [u8(1), 2] {
		return match op {
			.eq { lhs.number == rhs.number }
			.ne { lhs.number != rhs.number }
			.lt { lhs.number < rhs.number }
			.le { lhs.number <= rhs.number }
			.gt { lhs.number > rhs.number }
			.ge { lhs.number >= rhs.number }
			else { false }
		}
	}
	if lhs.kind == 3 {
		return match op {
			.eq { lhs.text == rhs.text }
			.ne { lhs.text != rhs.text }
			.lt { lhs.text < rhs.text }
			.le { lhs.text <= rhs.text }
			.gt { lhs.text > rhs.text }
			.ge { lhs.text >= rhs.text }
			else { false }
		}
	}
	if lhs.kind == 4 {
		return match op {
			.eq { lhs.bool_value == rhs.bool_value }
			.ne { lhs.bool_value != rhs.bool_value }
			else { none }
		}
	}
	if lhs.kind == 5 {
		return match op {
			.eq { true }
			.ne { false }
			else { none }
		}
	}
	return none
}

fn (tc &TypeChecker) constant_scalar_value(id flat.NodeId) ?ConstantScalar {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.constant_scalar_value(tc.a.child(node, 0))
	}
	return match node.kind {
		.int_literal {
			ConstantScalar{
				kind:   1
				number: f64(v_int_literal_value(node.value)?)
			}
		}
		.float_literal {
			ConstantScalar{
				kind:   1
				number: node.value.f64()
			}
		}
		.char_literal {
			ConstantScalar{
				kind:   2
				number: f64(match_char_literal_value(node.value)?)
			}
		}
		.string_literal {
			ConstantScalar{
				kind: 3
				text: node.value
			}
		}
		.bool_literal {
			ConstantScalar{
				kind:       4
				bool_value: node.value == 'true'
			}
		}
		.none_expr {
			ConstantScalar{
				kind: 5
			}
		}
		else {
			none
		}
	}
}

fn (tc &TypeChecker) same_simple_comparison_expr(lhs_id flat.NodeId, rhs_id flat.NodeId) bool {
	lhs := tc.simple_comparison_expr(lhs_id) or { return false }
	rhs := tc.simple_comparison_expr(rhs_id) or { return false }
	if tc.resolve_type(lhs_id).is_float() || tc.resolve_type(rhs_id).is_float() {
		return false
	}
	return lhs == rhs
}

fn (tc &TypeChecker) is_always_true_self_comparison(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.is_always_true_self_comparison(tc.a.child(node, 0))
	}
	return node.kind == .infix && node.op == .eq && node.children_count >= 2
		&& tc.same_simple_comparison_expr(tc.a.child(node, 0), tc.a.child(node, 1))
}

fn (tc &TypeChecker) simple_comparison_expr(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.simple_comparison_expr(tc.a.child(node, 0))
	}
	if node.kind !in [.ident, .selector] {
		return none
	}
	key := tc.expr_key(id)
	return if key.len > 0 { key } else { none }
}

fn (tc &TypeChecker) if_branch_empty_array_compatible(a Type, a_branch flat.NodeId, b Type, b_branch flat.NodeId) bool {
	a_empty := tc.expr_is_empty_bare_array_literal(tc.branch_tail_expr_id(a_branch))
	b_empty := tc.expr_is_empty_bare_array_literal(tc.branch_tail_expr_id(b_branch))
	return (a_empty && array_like_elem_type(b) != none)
		|| (b_empty && array_like_elem_type(a) != none)
}

fn (tc &TypeChecker) expr_is_empty_bare_array_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	return node.kind == .array_literal && node.children_count == 0
}

fn (tc &TypeChecker) if_branch_none_has_option_context(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId) bool {
	if (a is None || is_option_void_type(a)) && tc.branch_tail_is_none_literal(a_tail) {
		return b !is Void && b !is None && !is_option_void_type(b)
	}
	if (b is None || is_option_void_type(b)) && tc.branch_tail_is_none_literal(b_tail) {
		return a !is Void && a !is None && !is_option_void_type(a)
	}
	return false
}

fn (tc &TypeChecker) if_branch_error_has_result_context(a Type, b Type) bool {
	_ = tc
	if is_ierror_type(a) {
		return b !is Void && !is_ierror_type(b)
	}
	if is_ierror_type(b) {
		return a !is Void && !is_ierror_type(a)
	}
	return false
}

fn (mut tc TypeChecker) if_branch_enum_shorthand_compatible(a Type, a_tail flat.NodeId, b Type, b_tail flat.NodeId) bool {
	if a is Enum && tc.valid_node_id(b_tail) && tc.a.nodes[int(b_tail)].kind == .enum_val {
		return tc.type_compatible(tc.resolve_expr(b_tail, a), a)
	}
	if b is Enum && tc.valid_node_id(a_tail) && tc.a.nodes[int(a_tail)].kind == .enum_val {
		return tc.type_compatible(tc.resolve_expr(a_tail, b), b)
	}
	return false
}
