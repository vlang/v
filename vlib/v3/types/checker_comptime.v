module types

import strconv
import strings
import v3.errors as v3errors
import v3.flat
import v3.token
import v3.util

fn (mut tc TypeChecker) check_comptime_static_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if loop_kind == 'values' {
		tc.check_comptime_static_value_metadata_if(node, var_name, loop_kind, field_cases,
			value_cases)
		return
	}
	if !field_cases.known {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	mut then_cases := []ComptimeStaticFieldCase{}
	mut else_cases := []ComptimeStaticFieldCase{}
	for f in field_cases.cases {
		cond := tc.comptime_static_subst_field_cond(node.value, var_name, f)
		if comptime_text_references_var(cond, var_name) {
			then_cases << f
			else_cases << f
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			then_cases << f
			else_cases << f
			continue
		}
		if taken {
			then_cases << f
		} else {
			else_cases << f
		}
	}
	if then_cases.len > 0 && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, ComptimeStaticFieldCases{
			known: true
			cases: then_cases
		}, value_cases)
	}
	if else_cases.len > 0 && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, ComptimeStaticFieldCases{
			known: true
			cases: else_cases
		}, value_cases)
	}
}

fn (mut tc TypeChecker) check_comptime_static_value_metadata_if(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if !value_cases.known {
		for i in 0 .. node.children_count {
			tc.check_comptime_static_body(tc.a.child(&node, i), var_name, loop_kind, field_cases,
				value_cases)
		}
		return
	}
	mut check_then := false
	mut check_else := false
	for item in value_cases.cases {
		cond := comptime_static_subst_value_cond(node.value, var_name, item)
		if comptime_text_references_var(cond, var_name) {
			check_then = true
			check_else = true
			continue
		}
		taken := tc.comptime_static_eval_field_cond(cond) or {
			check_then = true
			check_else = true
			continue
		}
		if taken {
			check_then = true
		} else {
			check_else = true
		}
	}
	if check_then && node.children_count > 0 {
		tc.check_comptime_static_body(tc.a.child(&node, 0), var_name, loop_kind, field_cases,
			value_cases)
	}
	if check_else && node.children_count > 1 {
		tc.check_comptime_static_body(tc.a.child(&node, 1), var_name, loop_kind, field_cases,
			value_cases)
	}
}

fn (tc &TypeChecker) comptime_subtree_references_var(id flat.NodeId, var_name string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_for && comptime_for_declares_var_in_value(node.value, var_name) {
		return false
	}
	if node.kind == .ident && node.value == var_name {
		return true
	}
	if node.kind == .comptime_if && comptime_text_references_var(node.value, var_name) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.comptime_subtree_references_var(tc.a.child(&node, i), var_name) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_comptime_static_call(id flat.NodeId, node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	if node.children_count == 0 {
		return
	}
	callee_id := tc.a.child(&node, 0)
	if loop_kind == 'methods' && tc.comptime_static_is_method_var_call(node, var_name) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		tc.check_comptime_static_method_var_call(id, node, value_cases)
		return
	}
	callee_node := tc.a.child_node(&node, 0)
	if loop_kind == 'methods' && callee_node.kind == .ident
		&& callee_node.value in ['print', 'println', 'eprint', 'eprintln']
		&& node.children_count > 1
		&& tc.comptime_static_expr_has_void_method_call(tc.call_arg_value(tc.a.child(&node, 1)), var_name, value_cases) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		tc.record_error_at(.call_arg_mismatch,
			'`${callee_node.value}` can not print void expressions', id, node.pos)
		return
	}
	if tc.comptime_subtree_references_var(callee_id, var_name) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if _ := tc.sum_constructor_call_name(node) {
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
		return
	}
	if info0 := tc.resolve_call_info(id, node) {
		if tc.record_empty_array_generic_call_errors(node, info0) {
			return
		}
		info := tc.specialized_plain_generic_call_info(node, info0)
		if info.name in tc.a.disabled_fns || tc.canonical_symbol(info.name) in tc.a.disabled_fns
			|| info.name in tc.source_no_body_fns {
			mut callee := tc.a.child_node(&node, 0)
			if callee.kind == .index && callee.children_count > 0 {
				callee = tc.a.child_node(callee, 0)
			}
			if callee.kind == .selector {
				name_pos := tc.method_call_name_pos(node, callee)
				tc.record_error_at(.unknown_fn, 'cannot call a method that does not have a body',
					id, token.new_span(name_pos.id, name_pos.offset, node.pos.end))
			} else {
				tc.record_error_at(.unknown_fn, 'cannot call a function that does not have a body',
					id, node.pos)
			}
			return
		}
		if info.name.len > 0 && !is_array_dsl_call_name(info.name) {
			tc.remember_resolved_call(id, info.name)
		}
		if info.return_type !is Void && info.return_type !is Unknown {
			tc.remember_expr_type(id, info.return_type)
		}
		tc.check_comptime_static_call_metadata_arg_types(id, node, info, var_name, loop_kind)
		tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
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
	if tc.should_diagnose(id) && !tc.is_known_call(node)
		&& !tc.call_generic_args_have_placeholders(node) {
		tc.record_error(.unknown_fn, 'unknown function: ${tc.call_display_name(node)}', id)
	}
	tc.check_comptime_static_call_args(node, var_name, loop_kind, field_cases, value_cases)
}

fn (tc &TypeChecker) comptime_static_is_method_var_call(node flat.Node, var_name string) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := tc.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value != '$' || callee.children_count < 2 {
		return false
	}
	method_name := tc.a.child_node(callee, 1)
	return method_name.kind == .ident && method_name.value == var_name
}

fn (tc &TypeChecker) comptime_static_expr_has_void_method_call(id flat.NodeId, var_name string, value_cases ComptimeStaticValueCases) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if tc.comptime_static_is_method_var_call(*node, var_name) {
		return value_cases.cases.any(it.return_type.len == 0 || it.return_type == 'void')
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.comptime_static_expr_has_void_method_call(tc.a.child(node, 0), var_name,
			value_cases)
	}
	return false
}

fn (mut tc TypeChecker) check_comptime_static_method_var_call(id flat.NodeId, node flat.Node, value_cases ComptimeStaticValueCases) {
	if !value_cases.known || value_cases.cases.len == 0 || node.children_count == 0 {
		return
	}
	callee := tc.a.child_node(&node, 0)
	if callee.children_count == 0 {
		return
	}
	receiver_id := tc.a.child(callee, 0)
	receiver_type := unalias_and_unwrap_pointer_type(tc.resolve_type(receiver_id))
	receiver_name := receiver_type.name()
	actual_count := int(node.children_count) - 1
	for method in value_cases.cases {
		if actual_count != method.param_types.len {
			mut pos := node.pos
			if file := tc.a.source_files[node.pos.id] {
				if source := tc.source_texts_by_file[file.name] {
					start := int_max(0, node.pos.offset)
					end := int_min(source.len, node.pos.end)
					if start < end {
						if relative := source[start..end].index('.$') {
							pos = token.new_span(node.pos.id, start + relative + 1, end)
						}
					}
				}
			}
			tc.record_error_severity_at(.call_arg_mismatch,
				'expected ${method.param_types.len} arguments to method ${receiver_name}.${method.name}, but got ${actual_count}',
				id, pos, 'cgen error:')
			return
		}
		for arg_index in 0 .. actual_count {
			raw_arg_id := tc.a.child(&node, arg_index + 1)
			raw_arg := tc.a.child_node(&node, arg_index + 1)
			arg_id := tc.call_arg_value(raw_arg_id)
			actual := tc.resolve_type(arg_id)
			if raw_arg.kind != .prefix && unalias_type(actual).name() == '[]string' {
				tc.record_error_at(.call_arg_mismatch,
					'to auto-expand `[]string` arguments in comptime method calls, use `...${tc.source_text_for_node(arg_id)}`',
					arg_id, tc.a.node(arg_id).pos)
				return
			}
			if arg_index < method.param_is_mut.len && method.param_is_mut[arg_index]
				&& tc.a.node(arg_id).is_mut && arg_index < method.param_types.len
				&& method.param_types[arg_index].starts_with('&') {
				expected_name := '&${method.param_types[arg_index]}'
				actual_name := actual.name()
				tc.record_error_at(.call_arg_mismatch, 'cannot use `${actual_name}` as `${expected_name}` in argument ${
					arg_index + 1} to `${receiver_name}.${method.name}`', arg_id,
					tc.a.node(arg_id).pos)
				return
			}
		}
	}
}

fn (mut tc TypeChecker) check_comptime_static_call_metadata_arg_types(id flat.NodeId, node flat.Node, info CallInfo, var_name string, loop_kind string) {
	if !info.params_known {
		return
	}
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
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	ctx_omitted := ctx_count > 0 && actual_count < info.params.len
	for i in 1 + info.arg_offset .. node.children_count {
		raw_arg := tc.a.child_node(&node, i)
		if raw_arg.kind == .field_init {
			continue
		}
		arg_shift := if ctx_omitted { ctx_count } else { 0 }
		param_idx := i - 1 - info.arg_offset + (if info.has_receiver { 1 } else { 0 }) + arg_shift
		if info.is_c_variadic && param_idx >= c_variadic_fixed_param_count(info) {
			continue
		}
		if param_idx >= info.params.len {
			continue
		}
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		actual := tc.comptime_static_metadata_expr_type(arg_id, var_name, loop_kind) or { continue }
		expected := tc.call_arg_expected_type(info, param_idx)
		if !tc.receiver_compatible(actual, expected) && !tc.type_compatible(actual, expected) {
			if expected is Pointer && tc.expr_tail_is_nil(arg_id) {
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
			tc.type_mismatch(.call_arg_mismatch, 'cannot use `${actual.name()}` as argument ${
				param_idx + 1} to `${tc.call_display_name(node)}`; expected `${expected.name()}`',
				id)
		}
	}
}

fn (tc &TypeChecker) comptime_static_metadata_expr_type(id flat.NodeId, var_name string, loop_kind string) ?Type {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return tc.comptime_static_metadata_expr_type(tc.a.child(&node, 0), var_name, loop_kind)
	}
	if node.kind == .ident && node.value == var_name {
		metadata_type := match loop_kind {
			'methods' { 'FunctionData' }
			'params' { 'FunctionParam' }
			'attributes' { 'VAttribute' }
			'values' { 'EnumData' }
			'variants' { 'VariantData' }
			else { 'FieldData' }
		}

		return tc.parse_type(metadata_type)
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(&node, 0)
		if base.kind == .ident && base.value == var_name {
			return tc.comptime_static_metadata_member_type(node.value, loop_kind)
		}
	}
	return none
}

fn (tc &TypeChecker) comptime_static_metadata_member_type(member string, loop_kind string) ?Type {
	if loop_kind == 'methods' {
		return match member {
			'name', 'location' {
				tc.parse_type('string')
			}
			'is_pub' {
				tc.parse_type('bool')
			}
			'return_type', 'typ' {
				tc.parse_type('int')
			}
			'args', 'params' {
				tc.parse_type('[]FunctionParam')
			}
			'attrs' {
				tc.parse_type('[]string')
			}
			'attributes' {
				tc.parse_type('[]VAttribute')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'params' {
		return match member {
			'name' {
				tc.parse_type('string')
			}
			'typ' {
				tc.parse_type('int')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'attributes' {
		return match member {
			'name', 'arg' {
				tc.parse_type('string')
			}
			'has_arg' {
				tc.parse_type('bool')
			}
			'kind' {
				tc.parse_type('AttributeKind')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'variants' {
		return match member {
			'typ' {
				tc.parse_type('int')
			}
			else {
				none
			}
		}
	}
	if loop_kind == 'values' {
		return match member {
			'name' {
				tc.parse_type('string')
			}
			'value' {
				tc.parse_type('i64')
			}
			'attrs' {
				tc.parse_type('[]string')
			}
			else {
				none
			}
		}
	}
	return match member {
		'name' {
			tc.parse_type('string')
		}
		'typ', 'unaliased_typ' {
			tc.parse_type('int')
		}
		'attrs' {
			tc.parse_type('[]string')
		}
		'indirections' {
			tc.parse_type('u8')
		}
		'is_option', 'is_opt', 'is_embed', 'is_array', 'is_map', 'is_chan', 'is_struct', 'is_enum',
		'is_alias', 'is_shared', 'is_atomic', 'is_mut', 'is_pub' {
			tc.parse_type('bool')
		}
		else {
			none
		}
	}
}

fn (mut tc TypeChecker) check_comptime_static_call_args(node flat.Node, var_name string, loop_kind string, field_cases ComptimeStaticFieldCases, value_cases ComptimeStaticValueCases) {
	for i in 1 .. node.children_count {
		tc.check_comptime_static_body(tc.call_arg_value(tc.a.child(&node, i)), var_name, loop_kind,
			field_cases, value_cases)
	}
}

fn (mut tc TypeChecker) comptime_static_enum_value_cases(base_type string) ComptimeStaticValueCases {
	source_type := tc.comptime_static_for_base_type(base_type)
	enum_name := tc.comptime_static_enum_name(source_type) or { return ComptimeStaticValueCases{} }
	names := tc.enum_fields[enum_name] or { return ComptimeStaticValueCases{
		known: true
	} }
	metas := tc.comptime_static_enum_decl_value_cases(enum_name)
	if metas.len > 0 {
		return ComptimeStaticValueCases{
			known: true
			cases: metas
		}
	}
	mut cases := []ComptimeStaticValueCase{cap: names.len}
	is_flag := enum_name in tc.flag_enums
	for idx, name in names {
		cases << ComptimeStaticValueCase{
			name:      name
			value:     if is_flag { 1 << idx } else { idx }
			has_value: true
		}
	}
	return ComptimeStaticValueCases{
		known: true
		cases: cases
	}
}

fn (tc &TypeChecker) comptime_static_enum_name(raw string) ?string {
	mut cur := trimmed_space(raw)
	mut seen := map[string]bool{}
	for cur.len > 0 && cur !in seen {
		seen[cur] = true
		mut candidates := [cur, tc.qualify_name(cur)]
		if resolved := tc.resolve_selective_import_type_symbol(cur) {
			candidates << resolved
		}
		for candidate in candidates {
			if candidate in tc.enum_names {
				return candidate
			}
		}
		next := tc.alias_target_type_text(cur) or { break }
		if next == cur {
			break
		}
		cur = trimmed_space(next)
	}
	return none
}

fn comptime_static_subst_value_cond(cond string, var_name string, item ComptimeStaticValueCase) string {
	mut c := cond
	if item.has_value {
		c = c.replace('${var_name}.value', item.value.str())
	}
	c = c.replace('${var_name}.name', "'${item.name}'")
	c = comptime_static_replace_bare_ident(c, var_name, 'EnumData')
	return c
}

fn comptime_text_references_member(cond string, var_name string) bool {
	prefix := '${var_name}.'
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + prefix.len > cond.len || cond[offset..offset + prefix.len] != prefix {
			offset++
			continue
		}
		if offset > 0 && comptime_cond_name_char(cond[offset - 1]) {
			offset++
			continue
		}
		member_start := offset + prefix.len
		if member_start < cond.len && comptime_cond_name_char(cond[member_start]) {
			return true
		}
		offset = member_start
	}
	return false
}

fn comptime_text_references_var(cond string, var_name string) bool {
	if var_name.len == 0 {
		return false
	}
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			offset = comptime_cond_skip_string(cond, offset)
			continue
		}
		if offset + var_name.len <= cond.len && cond[offset..offset + var_name.len] == var_name {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + var_name.len
			after_ok := after >= cond.len || !comptime_cond_name_char(cond[after])
			if before_ok && after_ok {
				return true
			}
		}
		offset++
	}
	return false
}

fn (mut tc TypeChecker) comptime_static_field_cases(base_type string) ComptimeStaticFieldCases {
	source_type := tc.comptime_static_for_base_type(base_type)
	struct_name := tc.comptime_static_struct_name(source_type) or {
		return ComptimeStaticFieldCases{}
	}
	fields := tc.structs[struct_name] or { return ComptimeStaticFieldCases{
		known: true
	} }
	decl_metas := tc.comptime_static_field_decl_metas(struct_name)
	mut cases := []ComptimeStaticFieldCase{cap: fields.len}
	for field in fields {
		typ := field.typ.name()
		unaliased_typ := tc.comptime_type_match_type(typ).name()
		core_type := comptime_static_unwrap_field_type(field.typ)
		decl_meta := decl_metas[field.name] or { ComptimeStaticFieldDeclMeta{} }
		raw_typ := if decl_meta.raw_typ.len > 0 { decl_meta.raw_typ } else { typ }
		type_flags := comptime_static_field_type_flags(raw_typ)
		cases << ComptimeStaticFieldCase{
			name:          field.name
			typ:           typ
			unaliased_typ: unaliased_typ
			is_option:     field.typ is OptionType || tc.comptime_type_match_type(typ) is OptionType
			is_embed:      decl_meta.is_embed
			is_array:      core_type is Array || core_type is ArrayFixed
			is_map:        core_type is Map
			is_chan:       core_type is Channel
			is_struct:     tc.comptime_static_type_is_struct(core_type, typ, raw_typ)
			is_enum:       core_type is Enum && core_type.name() in tc.enum_names
			is_alias:      field.typ is Alias || typ in tc.type_aliases
				|| tc.qualify_name(typ) in tc.type_aliases
			is_shared:     type_flags.is_shared
			is_atomic:     type_flags.is_atomic
			is_mut:        decl_meta.is_mut
			is_pub:        decl_meta.is_pub
			has_decl_meta: field.name in decl_metas
			indirections:  type_flags.indirections
		}
	}
	return ComptimeStaticFieldCases{
		known: true
		cases: cases
	}
}

fn (tc &TypeChecker) comptime_static_type_is_struct(typ Type, typ_name string, raw_typ string) bool {
	if typ is Struct && tc.comptime_static_type_name_is_struct(typ.name()) {
		return true
	}
	if tc.comptime_static_type_name_is_struct(typ_name) {
		return true
	}
	return tc.comptime_static_type_name_is_struct(raw_typ)
}

fn (tc &TypeChecker) comptime_static_type_name_is_struct(name string) bool {
	clean := comptime_static_unwrap_type_text(name)
	if clean.len == 0 {
		return false
	}
	if clean in tc.structs {
		return true
	}
	base, _, is_generic := generic_type_application_parts(clean)
	if !is_generic {
		return false
	}
	if base in tc.structs || base in tc.struct_generic_params {
		return true
	}
	qbase := tc.qualify_name(base)
	if qbase in tc.structs || qbase in tc.struct_generic_params {
		return true
	}
	if resolved := tc.resolve_selective_import_type_symbol(base) {
		return resolved in tc.structs || resolved in tc.struct_generic_params
	}
	return false
}

fn comptime_static_unwrap_type_text(name string) string {
	mut clean := trimmed_space(name)
	for _ in 0 .. 16 {
		if clean.starts_with('?') || clean.starts_with('!') {
			clean = trimmed_space(clean[1..])
			continue
		}
		if clean.starts_with('shared ') {
			clean = trimmed_space(clean[7..])
			continue
		}
		if clean.starts_with('atomic ') {
			clean = trimmed_space(clean[7..])
			continue
		}
		if clean.starts_with('&') {
			clean = trimmed_space(clean[1..])
			continue
		}
		break
	}
	return clean
}

fn (tc &TypeChecker) comptime_static_for_base_type(raw string) string {
	if source := tc.comptime_static_for_value_source_type(raw) {
		return source
	}
	return raw
}

fn (tc &TypeChecker) comptime_static_for_value_source_type(raw string) ?string {
	clean := trimmed_space(raw)
	if clean.len == 0 {
		return none
	}
	parts := clean.split('.')
	if parts.len == 0 {
		return none
	}
	mut typ := tc.comptime_static_for_var_source_type(parts[0]) or { return none }
	for field in parts[1..] {
		typ = tc.comptime_static_for_field_source_type(typ, field) or { return none }
	}
	return typ
}

fn (tc &TypeChecker) comptime_static_for_var_source_type(name string) ?string {
	if tc.cur_scope != unsafe { nil } {
		if typ := tc.cur_scope.lookup(name) {
			return comptime_static_source_type_name(typ)
		}
	}
	if tc.file_scope != unsafe { nil } {
		if typ := tc.file_scope.lookup(name) {
			return comptime_static_source_type_name(typ)
		}
		qname := tc.qualify_name(name)
		if qname != name {
			if typ := tc.file_scope.lookup(qname) {
				return comptime_static_source_type_name(typ)
			}
		}
	}
	return none
}

fn (tc &TypeChecker) comptime_static_enum_decl_value_cases(enum_name string) []ComptimeStaticValueCase {
	mut cur_mod := ''
	if tc.top_level_idx.len > 0 {
		for idx in tc.top_level_idx {
			kind := tc.a.nodes[idx].kind
			if kind == .module_decl {
				cur_mod = tc.a.nodes[idx].value
				continue
			}
			if kind != .enum_decl {
				continue
			}
			if cases := tc.comptime_static_enum_decl_value_cases_for_node(enum_name, cur_mod,
				tc.a.nodes[idx])
			{
				return cases
			}
		}
		return []ComptimeStaticValueCase{}
	}
	for idx in 0 .. tc.a.nodes.len {
		kind := tc.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = tc.a.nodes[idx].value
			continue
		}
		if kind != .enum_decl {
			continue
		}
		if cases := tc.comptime_static_enum_decl_value_cases_for_node(enum_name, cur_mod,
			tc.a.nodes[idx])
		{
			return cases
		}
	}
	return []ComptimeStaticValueCase{}
}

fn (tc &TypeChecker) comptime_static_enum_decl_value_cases_for_node(enum_name string, cur_mod string, node flat.Node) ?[]ComptimeStaticValueCase {
	qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
		'${cur_mod}.${node.value}'
	} else {
		node.value
	}
	if enum_name != node.value && enum_name != qualified {
		return none
	}
	is_flag := node.typ == 'flag'
	mut field_order := []string{}
	mut field_exprs := map[string]flat.NodeId{}
	for i in 0 .. node.children_count {
		f := tc.a.child_node(&node, i)
		if f.kind != .enum_field {
			continue
		}
		field_order << f.value
		if f.children_count > 0 {
			field_exprs[f.value] = tc.a.child(f, 0)
		}
	}
	mut out := []ComptimeStaticValueCase{}
	mut field_values := map[string]int{}
	mut next_val := 0
	for field_name in field_order {
		mut val := next_val
		if expr_id := field_exprs[field_name] {
			mut resolving := map[string]bool{}
			if ev := tc.comptime_static_enum_field_value(expr_id, cur_mod, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				val = ev
			}
		}
		field_values[field_name] = val
		out << ComptimeStaticValueCase{
			name:      field_name
			value:     if is_flag { 1 << val } else { val }
			has_value: true
		}
		next_val = val + 1
	}
	return out
}

fn (tc &TypeChecker) comptime_static_enum_field_value(id flat.NodeId, enum_module string, enum_name string, mut field_values map[string]int, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?int {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if v := v_int_literal_value(node.value) {
				return v
			}
		}
		.ident {
			if value := tc.const_int_value_in_module(node.value, enum_module, []string{}) {
				return value
			}
			if ev := tc.comptime_static_enum_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				return ev
			}
			return none
		}
		.enum_val {
			if ev := tc.comptime_static_enum_field_ref_value(node.value, enum_module, enum_name, mut
				field_values, field_exprs, mut resolving)
			{
				return ev
			}
			return none
		}
		.paren {
			if node.children_count > 0 {
				return tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
					enum_name, mut field_values, field_exprs, mut resolving)
			}
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			cast_type := unalias_type(tc.parse_type(node.value))
			if !cast_type.is_integer() {
				return none
			}
			return tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.comptime_static_enum_field_value(tc.a.child(&node, 0), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			right := tc.comptime_static_enum_field_value(tc.a.child(&node, 1), enum_module,
				enum_name, mut field_values, field_exprs, mut resolving) or { return none }
			if (node.op == .div || node.op == .mod) && right == 0 {
				return none
			}
			if (node.op == .left_shift || node.op == .right_shift
				|| node.op == .right_shift_unsigned) && (right < 0 || right >= 64) {
				return none
			}
			return match node.op {
				.plus { left + right }
				.minus { left - right }
				.mul { left * right }
				.div { left / right }
				.mod { left % right }
				.left_shift { int(u64(left) << right) }
				.right_shift { left >> right }
				.right_shift_unsigned { int(u64(left) >> right) }
				.amp { left & right }
				.pipe { left | right }
				.xor { left ^ right }
				.power { const_int_power(left, right) }
				else { none }
			}
		}
		.selector {
			if field := tc.comptime_static_enum_selector_ref_field(id, enum_module, enum_name) {
				return tc.comptime_static_enum_field_ref_value(field, enum_module, enum_name, mut
					field_values, field_exprs, mut resolving)
			}
			return none
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) comptime_static_enum_field_ref_value(field_name string, enum_module string, enum_name string, mut field_values map[string]int, field_exprs map[string]flat.NodeId, mut resolving map[string]bool) ?int {
	if field_name in field_values {
		return field_values[field_name]
	}
	expr_id := field_exprs[field_name] or { return none }
	if resolving[field_name] {
		return none
	}
	resolving[field_name] = true
	val := tc.comptime_static_enum_field_value(expr_id, enum_module, enum_name, mut field_values,
		field_exprs, mut resolving) or {
		resolving.delete(field_name)
		return none
	}
	resolving.delete(field_name)
	field_values[field_name] = val
	return val
}

fn (tc &TypeChecker) comptime_static_enum_selector_ref_field(id flat.NodeId, enum_module string, enum_name string) ?string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	prefix := tc.comptime_static_enum_selector_base_text(tc.a.child(&node, 0))
	if !comptime_static_enum_ref_prefix_matches(prefix, enum_module, enum_name) {
		return none
	}
	return node.value
}

fn (tc &TypeChecker) comptime_static_enum_selector_base_text(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
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
			base := tc.comptime_static_enum_selector_base_text(tc.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		else {
			return ''
		}
	}
}

fn comptime_static_enum_ref_prefix_matches(prefix string, enum_module string, enum_name string) bool {
	if prefix.len == 0 || enum_name.len == 0 {
		return false
	}
	short := enum_name.all_after_last('.')
	if prefix == enum_name || prefix == short {
		return true
	}
	if enum_module.len > 0 && prefix == '${enum_module}.${short}' {
		return true
	}
	return false
}

fn (tc &TypeChecker) comptime_static_for_field_source_type(owner_type string, field_name string) ?string {
	struct_name := tc.comptime_static_struct_name(owner_type) or { return none }
	typ := tc.struct_field_type(struct_name, field_name) or { return none }
	return comptime_static_source_type_name(typ)
}

fn comptime_static_source_type_name(typ Type) string {
	mut cur := typ
	for _ in 0 .. 16 {
		if cur is Pointer {
			cur = cur.base_type
			continue
		}
		if cur is OptionType {
			cur = cur.base_type
			continue
		}
		if cur is ResultType {
			cur = cur.base_type
			continue
		}
		return cur.name()
	}
	return cur.name()
}

fn (tc &TypeChecker) comptime_static_struct_name(raw string) ?string {
	mut cur := trimmed_space(raw)
	mut seen := map[string]bool{}
	for cur.len > 0 && cur !in seen {
		seen[cur] = true
		mut candidates := [cur, tc.qualify_name(cur)]
		if resolved := tc.resolve_selective_import_type_symbol(cur) {
			candidates << resolved
		}
		for candidate in candidates {
			if candidate in tc.structs {
				return candidate
			}
		}
		next := tc.alias_target_type_text(cur) or { break }
		if next == cur {
			break
		}
		cur = trimmed_space(next)
	}
	return none
}

fn (tc &TypeChecker) comptime_static_field_decl_metas(base_type string) map[string]ComptimeStaticFieldDeclMeta {
	mut out := map[string]ComptimeStaticFieldDeclMeta{}
	mut decl_name := trimmed_space(base_type)
	if idx := decl_name.index('[') {
		decl_name = decl_name[..idx]
	}
	mut cur_mod := ''
	if tc.top_level_idx.len > 0 {
		for idx in tc.top_level_idx {
			kind := tc.a.nodes[idx].kind
			if kind == .module_decl {
				cur_mod = tc.a.nodes[idx].value
				continue
			}
			if kind != .struct_decl {
				continue
			}
			if metas := tc.comptime_static_field_decl_metas_for_node(decl_name, cur_mod,
				tc.a.nodes[idx])
			{
				return metas
			}
		}
		return out
	}
	for idx in 0 .. tc.a.nodes.len {
		kind := tc.a.nodes[idx].kind
		if kind == .module_decl {
			cur_mod = tc.a.nodes[idx].value
			continue
		}
		if kind != .struct_decl {
			continue
		}
		if metas := tc.comptime_static_field_decl_metas_for_node(decl_name, cur_mod,
			tc.a.nodes[idx])
		{
			return metas
		}
	}
	return out
}

fn (tc &TypeChecker) comptime_static_field_decl_metas_for_node(decl_name string, cur_mod string, node flat.Node) ?map[string]ComptimeStaticFieldDeclMeta {
	qualified := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
		'${cur_mod}.${node.value}'
	} else {
		node.value
	}
	if decl_name != node.value && decl_name != qualified {
		return none
	}
	mut out := map[string]ComptimeStaticFieldDeclMeta{}
	for i in 0 .. node.children_count {
		f := tc.a.child_node(&node, i)
		if f.kind != .field_decl {
			continue
		}
		raw_typ := if f.typ.len > 0 { f.typ } else { f.value }
		mut is_mut := false
		mut is_pub := false
		if f.generic_params().len > 0 {
			flags := f.generic_params()[0]
			is_mut = flags.contains('m')
			is_pub = flags.contains('p')
		}
		out[f.value] = ComptimeStaticFieldDeclMeta{
			is_mut:   is_mut
			is_pub:   is_pub
			is_embed: source_field_decl_is_embed(f, raw_typ)
			raw_typ:  raw_typ
		}
	}
	return out
}

fn comptime_static_unwrap_field_type(typ Type) Type {
	mut cur := typ
	for _ in 0 .. 16 {
		if cur is Alias {
			cur = cur.base_type
			continue
		}
		if cur is OptionType {
			cur = cur.base_type
			continue
		}
		if cur is Pointer {
			cur = cur.base_type
			continue
		}
		return cur
	}
	return cur
}

fn comptime_static_field_type_flags(raw string) ComptimeStaticFieldTypeFlags {
	mut core := trimmed_space(raw)
	mut flags := ComptimeStaticFieldTypeFlags{}
	if core.starts_with('?') {
		core = trimmed_space(core[1..])
	}
	if core.starts_with('shared ') {
		flags.is_shared = true
		flags.indirections++
		core = trimmed_space(core[7..])
	} else if core.starts_with('atomic ') {
		flags.is_atomic = true
		core = trimmed_space(core[7..])
	}
	for core.starts_with('&') {
		flags.indirections++
		core = trimmed_space(core[1..])
	}
	return flags
}

fn (mut tc TypeChecker) comptime_static_subst_field_cond(cond string, var_name string, field ComptimeStaticFieldCase) string {
	mut c := cond
	c = c.replace('${var_name}.unaliased_typ', field.unaliased_typ)
	c = c.replace('${var_name}.is_option', field.is_option.str())
	c = c.replace('${var_name}.is_opt', field.is_option.str())
	if field.has_decl_meta {
		c = c.replace('${var_name}.is_embed', field.is_embed.str())
		c = c.replace('${var_name}.is_mut', field.is_mut.str())
		c = c.replace('${var_name}.is_pub', field.is_pub.str())
	}
	c = c.replace('${var_name}.is_array', field.is_array.str())
	c = c.replace('${var_name}.is_map', field.is_map.str())
	c = c.replace('${var_name}.is_chan', field.is_chan.str())
	c = c.replace('${var_name}.is_struct', field.is_struct.str())
	c = c.replace('${var_name}.is_enum', field.is_enum.str())
	c = c.replace('${var_name}.is_alias', field.is_alias.str())
	c = c.replace('${var_name}.is_shared', field.is_shared.str())
	c = c.replace('${var_name}.is_atomic', field.is_atomic.str())
	c = c.replace('${var_name}.indirections', field.indirections.str())
	c = c.replace('${var_name}.typ', field.typ)
	c = c.replace('${var_name}.name', "'${field.name}'")
	c = comptime_static_replace_bare_ident(c, var_name, field.typ)
	return c
}

fn comptime_static_replace_bare_ident(cond string, ident string, replacement string) string {
	if ident.len == 0 {
		return cond
	}
	mut out := ''
	mut offset := 0
	for offset < cond.len {
		if cond[offset] == `'` || cond[offset] == `"` {
			end := comptime_cond_skip_string(cond, offset)
			out += cond[offset..end]
			offset = end
			continue
		}
		if offset + ident.len <= cond.len && cond[offset..offset + ident.len] == ident {
			before_ok := offset == 0 || !comptime_cond_name_char(cond[offset - 1])
			after := offset + ident.len
			after_ok := after >= cond.len
				|| (!comptime_cond_name_char(cond[after]) && cond[after] != `.`)
			if before_ok && after_ok {
				out += replacement
				offset = after
				continue
			}
		}
		out += cond[offset..offset + 1]
		offset++
	}
	return out
}

fn (mut tc TypeChecker) comptime_static_eval_field_cond(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(trimmed_space(cond))
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_static_eval_field_cond(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_static_eval_field_cond(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_static_eval_field_cond(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_static_eval_field_cond(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := trimmed_space(clean[..op_idx])
			right := trimmed_space(clean[op_idx + op.len..])
			matches := tc.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	for op in [' != ', ' == '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := comptime_static_unquote(trimmed_space(clean[..op_idx]))
			right := comptime_static_unquote(trimmed_space(clean[op_idx + op.len..]))
			eq := left == right
			return if op == ' == ' { eq } else { !eq }
		}
	}
	for op in [' !in', ' in'] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			after := op_idx + op.len
			if after < clean.len && clean[after] != ` ` && clean[after] != `[`
				&& clean[after] != `(` {
				continue
			}
			needle := comptime_static_unquote(trimmed_space(clean[..op_idx]))
			found := comptime_static_list_contains(trimmed_space(clean[after..]), needle)
			return if op == ' in' { found } else { !found }
		}
	}
	for op in [' <= ', ' >= ', ' < ', ' > '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := trimmed_space(clean[..op_idx])
			right := trimmed_space(clean[op_idx + op.len..])
			if !comptime_static_is_int(left) || !comptime_static_is_int(right) {
				return none
			}
			l := left.int()
			r := right.int()
			return match op {
				' <= ' { l <= r }
				' >= ' { l >= r }
				' < ' { l < r }
				else { l > r }
			}
		}
	}
	if clean.starts_with('!') {
		value := tc.comptime_static_eval_field_cond(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn comptime_static_list_contains(list_text string, needle string) bool {
	clean := trimmed_space(list_text)
	if !clean.starts_with('[') || !clean.ends_with(']') {
		return false
	}
	inner := clean[1..clean.len - 1]
	for part in inner.split(',') {
		if comptime_static_unquote(trimmed_space(part)) == needle {
			return true
		}
	}
	return false
}

fn comptime_static_is_int(s string) bool {
	if s.len == 0 {
		return false
	}
	start := if s[0] == `-` || s[0] == `+` { 1 } else { 0 }
	if start >= s.len {
		return false
	}
	for i in start .. s.len {
		if !s[i].is_digit() {
			return false
		}
	}
	return true
}

fn comptime_static_unquote(s string) string {
	if s.len >= 2 && (s[0] == `'` || s[0] == `"`) && s[s.len - 1] == s[0] {
		return comptime_static_unescape(s[1..s.len - 1])
	}
	return s
}

fn comptime_static_string_literal(value string) string {
	mut out := strings.new_builder(value.len + 2)
	out.write_u8(`'`)
	for i := 0; i < value.len; i++ {
		if value[i] == `\\` || value[i] == `'` {
			out.write_u8(`\\`)
		}
		out.write_u8(value[i])
	}
	out.write_u8(`'`)
	return out.str()
}

fn comptime_static_unescape(value string) string {
	if !value.contains('\\') {
		return value
	}
	mut out := strings.new_builder(value.len)
	mut i := 0
	for i < value.len {
		if value[i] != `\\` || i + 1 >= value.len {
			out.write_u8(value[i])
			i++
			continue
		}
		next := value[i + 1]
		if next == `\n` {
			i += 2
			for i < value.len && value[i] in [` `, `\t`, `\r`] {
				i++
			}
			continue
		}
		if next == `\r` && i + 2 < value.len && value[i + 2] == `\n` {
			i += 3
			for i < value.len && value[i] in [` `, `\t`] {
				i++
			}
			continue
		}
		if next == `x` && i + 3 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 2) {
				out.write_u8(u8(code))
				i += 4
				continue
			}
		}
		if next == `u` && i + 5 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 4) {
				out.write_rune(rune(code))
				i += 6
				continue
			}
		}
		if next == `U` && i + 9 < value.len {
			if code := comptime_static_fixed_hex(value, i + 2, 8) {
				out.write_rune(rune(code))
				i += 10
				continue
			}
		}
		decoded := match next {
			`n` { int(`\n`) }
			`t` { int(`\t`) }
			`r` { int(`\r`) }
			`\\` { int(`\\`) }
			`'` { int(`'`) }
			`"` { int(`"`) }
			`$` { int(`$`) }
			`0` { 0 }
			`a` { 7 }
			`b` { 8 }
			`f` { 12 }
			`v` { 11 }
			else { -1 }
		}

		if decoded >= 0 {
			out.write_u8(u8(decoded))
		} else {
			out.write_u8(`\\`)
			out.write_u8(next)
		}
		i += 2
	}
	return out.str()
}

fn comptime_static_fixed_hex(value string, start int, count int) ?u32 {
	mut code := u32(0)
	for i in 0 .. count {
		if start + i >= value.len {
			return none
		}
		digit := comptime_static_hex_digit(value[start + i]) or { return none }
		code = (code << 4) | digit
	}
	return code
}

fn comptime_static_hex_digit(c u8) ?u32 {
	if c >= `0` && c <= `9` {
		return u32(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u32(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u32(c - `A` + 10)
	}
	return none
}

// check_node validates check node state for types.
@[direct_array_access]
fn (mut tc TypeChecker) check_node(id flat.NodeId) {
	idx := int(id)
	if idx < 0 {
		return
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) && idx < tc.checking_nodes.len {
			if tc.checking_nodes[idx] {
				return
			}
			tc.checking_nodes[idx] = true
			defer {
				tc.checking_nodes[idx] = false
			}
		} else {
			if tc.sparse_checking_nodes[idx] {
				return
			}
			tc.sparse_checking_nodes[idx] = true
			defer {
				tc.sparse_checking_nodes.delete(idx)
			}
		}
	} else {
		if idx >= tc.checking_nodes.len {
			tc.extend_node_caches(tc.a.nodes.len)
		}
		if idx < tc.checking_nodes.len {
			if tc.checking_nodes[idx] {
				return
			}
			tc.checking_nodes[idx] = true
			defer {
				tc.checking_nodes[idx] = false
			}
		}
	}
	node := tc.a.nodes[idx]
	if node.kind == .string_literal {
		literal_pos := tc.string_literal_diagnostic_pos(node)
		if !tc.string_literal_source_is_valid_utf8(literal_pos) {
			tc.record_notice_at(.unknown_ident, 'invalid utf8 byte sequence in string literal', id,
				literal_pos)
		}
		return
	}
	kind_id := node_kind_id(node)
	if kind_id == 1 {
		tc.check_untyped_integer_literal_overflow(id)
		return
	}
	if kind_id == 2 || kind_id == 3 || kind_id == 4 || kind_id == 5 || kind_id == 28
		|| kind_id == 29 {
		return
	}
	if node.kind in [.break_stmt, .continue_stmt] {
		tc.check_loop_control_statement(id, node)
		return
	}
	if node.kind == .const_decl {
		tc.record_error_at(.duplicate_decl,
			'const can only be defined at the top level (outside of functions)', id, token.new_span(node.pos.id,
			node.pos.offset, node.pos.offset + 5))
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .const_field || field.value.len == 0 || field.value == '_' {
				continue
			}
			typ := if field.children_count > 0 {
				expr_id := tc.a.child(field, 0)
				tc.check_node(expr_id)
				tc.resolve_type(expr_id)
			} else {
				tc.parse_type(field.typ)
			}
			tc.cur_scope.insert(field.value, typ)
		}
		return
	}
	if kind_id == 45 {
		tc.check_block(id, node)
		return
	}
	if node.kind == .comptime_if {
		tc.check_comptime_if(id, node)
		return
	}
	if node.kind == .comptime_for {
		// The body references the loop variable (`field.name`, `field.typ`, ...) which only
		// exists once the transformer unrolls the loop against a concrete type, so it cannot
		// be type-checked here. Validate the known compile-time member surface, then skip it;
		// the unrolled statements are concrete.
		tc.check_comptime_for_members(id, node)
		return
	}
	if node.kind == .asm_stmt {
		tc.check_asm_stmt(id, node)
		return
	}
	if node.kind == .sql_expr {
		tc.check_sql_expr(id, node)
		return
	}
	if node.kind == .lock_expr {
		tc.check_lock_expr(id, node)
		return
	}
	if kind_id == 46 {
		tc.check_for_stmt(node)
		return
	}
	if kind_id == 47 {
		tc.check_for_in_stmt(node)
		return
	}
	if kind_id == 41 {
		tc.check_decl_assign(id, node)
		return
	}
	if kind_id == 40 || kind_id == 42 || kind_id == 43 {
		tc.check_assign(id, node)
		return
	}
	if kind_id == 44 {
		tc.check_return(id, node)
		return
	}
	if kind_id == 12 {
		tc.check_call(id, node)
		return
	}
	if kind_id == 21 {
		tc.check_fn_literal(id, node)
		return
	}
	if kind_id == 32 {
		tc.check_lambda_expr(id, node)
		return
	}
	if kind_id == 15 {
		tc.check_if_expr(id, node)
		return
	}
	if kind_id == 22 {
		tc.check_or_expr(id, node)
		return
	}
	if kind_id == 50 {
		tc.check_match_stmt(id, node)
		return
	}
	if kind_id == 37 {
		tc.check_is_expr(id, node)
		return
	}
	if kind_id == 10 {
		tc.check_postfix(id, node)
		return
	}
	if kind_id == 16 || kind_id == 26 {
		tc.check_struct_init(id, node)
		return
	}
	if kind_id == 13 {
		tc.check_selector(id, node)
		$if ownership ? {
			tc.ownership_check_expr(id)
		}
		return
	}
	if kind_id == 14 {
		tc.check_index(id, node)
		$if ownership ? {
			tc.ownership_check_expr(id)
		}
		return
	}
	if kind_id == 7 {
		tc.check_ident(id, node)
		return
	}
	if node.kind == .defer_result {
		tc.check_defer_result(id, node)
		return
	}
	if node.kind == .cast_expr {
		tc.check_cast_expr(id, node)
		return
	}
	if node.kind == .prefix {
		tc.check_prefix_expr(id, node)
		return
	}
	if node.kind == .as_expr {
		tc.check_as_expr(id, node)
		return
	}
	if node.kind == .sizeof_expr {
		if should_check_named_type(node.value) && !tc.type_name_known(node.value) {
			pos := tc.sizeof_type_diagnostic_pos(id, node.value)
			if node.value.len == 1 {
				// v1 classifies a lone unresolved generic-looking placeholder
				// in sizeof as a cgen error.
				tc.record_error_severity_at(.compile_error, 'unknown type `${node.value}`', id,
					pos, 'cgen error:')
			} else {
				tc.record_error_at(.unknown_type, tc.unknown_type_message(node.value, id), id, pos)
			}
		}
		return
	}
	if node.kind == .in_expr {
		tc.check_in_expr(id, node)
		return
	}
	if node.kind == .dump_expr {
		tc.check_dump_expr(node)
		return
	}
	if node.kind == .assert_stmt {
		tc.check_assert_stmt(node)
		return
	}
	if node.kind == .array_init {
		tc.check_missing_array_init_interface_type_args(id, node)
		tc.check_array_init(id, node)
		$if ownership ? {
			if !tc.ownership_aggregate_consumption_deferred(id) {
				tc.ownership_consume_array_init_expr(node)
			}
		}
		return
	}
	if node.kind == .select_stmt {
		tc.check_select_stmt(node)
		return
	}
	if node.kind == .defer_stmt {
		if node.value.starts_with('invalid:') {
			mode := node.value.all_after(':')
			tc.record_error_at(.unknown_ident, 'unknown `defer` mode: `${mode}`', id, tc.defer_mode_diagnostic_pos(node,
				mode))
			return
		}
		if node.value == 'function' && tc.fn_context.node_id >= 0 {
			if tc.lock_depth > 0 {
				tc.record_error_at(.assignment_mismatch,
					'`defer(fn)`s are not allowed in lock statements', id, node.pos)
			} else if !tc.current_fn_has_invalid_defer_mode() {
				tc.record_warning_at(.assignment_mismatch,
					'`defer` is already in function scope; just use `defer {` instead', id,
					node.pos)
			}
		}
		unsafe_alias_state := tc.fn_context.unsafe_reference_alias_owners.clone()
		$if ownership ? {
			tc.ownership_check_defer_stmt(id, node)
		} $else {
			tc.check_defer_stmt(node)
		}
		tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_state.clone()
		return
	}
	if node.kind == .spawn_expr {
		tc.check_spawn_expr(id, node)
		return
	}
	if node.kind == .string_interp {
		mut specialized_invalid_selector := false
		for i in 0 .. node.children_count {
			part_id := tc.a.child(&node, i)
			part := tc.a.node(part_id)
			expr_id := if part.kind == .directive && part.value == 'string_interp_format'
				&& part.children_count > 0 {
				tc.a.child(part, 0)
			} else {
				part_id
			}
			tc.check_node(expr_id)
			if access := tc.unlocked_shared_access(expr_id) {
				tc.record_error_at(.call_arg_mismatch,
					'`${access.name}` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut interpolation object',
					expr_id, access.pos)
			}
			expr := tc.a.node(expr_id)
			expr_type := tc.resolve_type(expr_id)
			invalid_selector := expr.kind == .selector && expr.children_count > 0
				&& tc.resolve_type(tc.a.child(expr, 0)) !is Unknown
				&& tc.selector_type(expr_id, *expr) == none
			undefined_self_reference := expr_type is Unknown && expr.kind == .ident
				&& tc.errors.any(it.node == expr_id && it.msg.starts_with('undefined variable:'))
			if (expr_type is Void && !tc.expr_subtree_has_undefined_variable_error(expr_id))
				|| (expr_type is MultiReturn && expr_type.types.len == 0)
				|| invalid_selector
				|| (expr_type is Unknown && expr.kind == .ident && tc.cur_scope.lookup(expr.value) == none
				&& !undefined_self_reference) {
				message := if expr.kind == .ident && tc.ident_is_mutable_lvalue(expr.value) {
					'no known default format for type `void`'
				} else {
					'expression does not return a value'
				}
				diagnostic_pos := if invalid_selector {
					tc.node_value_diagnostic_pos(expr_id)
				} else {
					tc.string_interpolation_expr_pos(expr_id)
				}
				tc.record_error_at(.call_arg_mismatch, message, expr_id, diagnostic_pos)
				specialized_invalid_selector = specialized_invalid_selector
					|| (invalid_selector && tc.current_fn_is_specialized_generic())
			} else if unalias_type(expr_type) is Char {
				tc.record_error_at(.call_arg_mismatch,
					'expression returning type `char` cannot be used in string interpolation directly, print its address or cast it to an integer instead',
					expr_id, tc.string_interpolation_expr_pos(expr_id))
			}
			if part.kind == .directive && part.value == 'string_interp_format' {
				tc.check_string_interpolation_format(part_id, part, expr_id)
			}
		}
		if specialized_invalid_selector {
			tc.record_enclosing_print_void(id)
		}
		return
	}
	if node.kind == .paren && node.value == '__v3_comptime_d' && node.children_count > 0 {
		default_id := tc.a.child(&node, 0)
		default := tc.a.node(default_id)
		if default.kind !in [.bool_literal, .char_literal, .float_literal, .string_literal,
			.int_literal] {
			tc.record_error_at(.assignment_mismatch, '$d() values can only be pure literals', id,
				node.pos)
		}
		tc.check_node(default_id)
		return
	}
	if node.kind == .paren && tc.paren_expr_has_redundant_parentheses(id) {
		tc.record_notice_at(.unknown_ident, 'redundant parentheses are used', id, node.pos)
	}
	// A method value stored in a container escapes the single-use guarantee of its per-site
	// static receiver, so reject `[obj.method]` / `arr << obj.method` / `{'k': obj.method}`.
	if node.kind == .array_literal {
		for i in 0 .. node.children_count {
			tc.reject_stored_method_value(tc.a.child(&node, i))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, i))
		}
		$if ownership ? {
			tc.check_ownership_array_spread_clone(id, node)
		}
	} else if node.kind == .map_init {
		map_type := unalias_type(tc.parse_type(node.value))
		if map_type is Map {
			if fn_type := fn_type_from_type(unalias_type(map_type.value_type)) {
				if fn_type.params.any(unalias_type(it) is ResultType) {
					tc.record_error_at(.unknown_type, 'result type arguments are not supported',
						id, node.pos)
				}
			}
		}
		if generic_name := tc.bare_generic_decl_type_name(node.value) {
			qualified := tc.qualify_name(generic_name)
			if generic_name in tc.struct_generic_params || qualified in tc.struct_generic_params {
				tc.record_error_at(.unknown_type,
					'generic struct `${generic_name}` must specify type parameter, e.g. ${generic_name}[int]',
					id, node.pos)
			}
		}
		if type_text_contains_any(node.value) {
			tc.record_notice_at(.unknown_type,
				'the `any` type is deprecated and will be removed soon - either use an empty interface, or a sum type',
				id, node.pos)
			tc.record_error(.unknown_type, 'cannot use type `any` here', id)
		}
		tc.check_map_duplicate_keys(node)
		// children alternate key, value, key, value, ...; check the value positions.
		for j := 1; j < node.children_count; j += 2 {
			tc.reject_stored_method_value(tc.a.child(&node, j))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, j))
		}
		$if ownership ? {
			tc.check_ownership_map_spread_clone(id, node)
		}
	} else if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
		if unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))) is Array {
			tc.reject_stored_method_value(tc.a.child(&node, 1))
			tc.reject_stored_capturing_fn_literal(tc.a.child(&node, 1))
		}
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		lhs_id := tc.a.child(&node, 0)
		rhs_id := tc.a.child(&node, 1)
		smartcasts := tc.extract_smartcasts(lhs_id)
		if smartcasts.len > 0 {
			tc.check_node(lhs_id)
			unsafe_alias_skipped_rhs := tc.fn_context.unsafe_reference_alias_owners.clone()
			saved_smartcasts := clone_smartcasts(tc.smartcasts)
			for sc in smartcasts {
				if valid_string_data(sc.name) {
					tc.smartcasts[sc.name] = sc.typ
				}
			}
			tc.check_node(rhs_id)
			tc.merge_unsafe_reference_alias_short_circuit_state(node.op, lhs_id,
				unsafe_alias_skipped_rhs)
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
			return
		}
	}

	mut unsafe_alias_skipped_rhs := map[string]bool{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if node.kind == .infix && node.op in [.logical_and, .logical_or] && i == 1 {
			unsafe_alias_skipped_rhs = tc.fn_context.unsafe_reference_alias_owners.clone()
		}
		previous_channel_send_or_expr_id := tc.channel_send_or_expr_id
		if node.kind == .infix && node.op == .arrow && i == 1 {
			child := tc.a.node(child_id)
			if child.kind == .or_expr {
				tc.channel_send_or_expr_id = int(child_id)
			}
		}
		$if ownership ? {
			defer_append_rhs := node.kind == .infix && node.op == .left_shift
				&& node.children_count >= 2 && i == 1
				&& unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0))) is Array
			tc.ownership_check_node_with_aggregate_consumption_mode(child_id, defer_append_rhs)
		} $else {
			tc.check_node(child_id)
		}
		if node.kind == .infix && node.op in [.logical_and, .logical_or] && i == 1 {
			tc.merge_unsafe_reference_alias_short_circuit_state(node.op, tc.a.child(&node, 0),
				unsafe_alias_skipped_rhs)
		}
		tc.channel_send_or_expr_id = previous_channel_send_or_expr_id
	}
	if node.kind == .array_literal {
		tc.check_array_literal_element_types(id, node)
	}
	if node.kind == .map_init {
		tc.check_map_none_values(node)
		has_unhandled_result := tc.check_map_literal_result_elements(node)
		map_type := unalias_type(tc.resolve_type(id))
		if map_type is Map && unalias_type(map_type.value_type) is ResultType
			&& node.children_count == 0 {
			tc.record_error_at(.unknown_type, 'cannot use Result type as map value type', id,
				node.pos)
		}
		if map_type is Map && unalias_type(map_type.key_type) is Struct {
			if map_type.key_type.name() != 'any' {
				tc.record_error_at(.unknown_type,
					'map key type `${map_type.key_type.name()}` not supported', id, node.pos)
			}
		}
		if !has_unhandled_result {
			tc.check_map_literal_element_types(id, node)
		}
	}
	if node.kind == .infix {
		tc.check_infix(id, node)
	}
	$if ownership ? {
		if !tc.ownership_aggregate_consumption_deferred(id) {
			if node.kind == .array_literal {
				for i in 0 .. node.children_count {
					elem_id := tc.a.child(&node, i)
					tc.ownership_consume_expr(elem_id, 'array element', elem_id)
				}
			} else if node.kind == .map_init {
				for j := 0; j < node.children_count; j += 2 {
					key_id := tc.a.child(&node, j)
					tc.ownership_consume_expr(key_id, 'map key', key_id)
					if j + 1 < node.children_count {
						val_id := tc.a.child(&node, j + 1)
						tc.ownership_consume_expr(val_id, 'map value', val_id)
					}
				}
			}
		}
		if node.kind == .infix && node.op == .left_shift && node.children_count >= 2 {
			array_id := tc.a.child(&node, 0)
			if unwrap_pointer(tc.resolve_type(array_id)) is Array {
				elem_id := tc.a.child(&node, 1)
				tc.ownership_mark_array_append_expr(array_id, elem_id, id)
			}
		} else if node.kind == .infix && node.op == .arrow && node.children_count >= 2 {
			value_id := tc.a.child(&node, 1)
			tc.ownership_consume_expr(value_id, 'channel send', id)
		}
	}
}

fn (mut tc TypeChecker) check_loop_control_statement(id flat.NodeId, node flat.Node) {
	if node.value.len > 0 {
		if tc.diagnostic_files.len == 0 && !tc.valid_labelled_loop_control(id, node.value) {
			tc.record_invalid_loop_label(id, node)
		}
		return
	}
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			break
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.for_stmt, .for_in_stmt] {
			if node.kind == .break_stmt {
				tc.record_unsafe_reference_alias_loop_break_state()
			}
			return
		}
		if parent.kind == .comptime_for {
			keyword := if node.kind == .break_stmt { 'break' } else { 'continue' }
			tc.record_error_at(.assignment_mismatch,
				'${keyword} is not allowed within a compile-time loop', id, tc.loop_control_keyword_pos(id,
				keyword))
			return
		}
		if parent.kind == .defer_stmt {
			keyword := if node.kind == .break_stmt { 'break' } else { 'continue' }
			tc.record_error_at(.assignment_mismatch,
				'`${keyword}` is not allowed in defer statements', id, tc.loop_control_keyword_pos(id,
				keyword))
			break
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			break
		}
		current = parent_id
	}
	keyword := if node.kind == .break_stmt { 'break' } else { 'continue' }
	tc.record_error_at(.assignment_mismatch, '${keyword} statement not within a loop', id, tc.loop_control_keyword_pos(id,
		keyword))
}

fn (mut tc TypeChecker) check_labelled_loop_controls() {
	if tc.diagnostic_files.len == 0 {
		return
	}
	mut diagnosed := map[string]bool{}
	for index, node in tc.a.nodes {
		if index < tc.a.user_code_start || node.kind !in [.break_stmt, .continue_stmt]
			|| node.value.len == 0 || !tc.node_is_in_selected_input_file(flat.NodeId(index))
			|| tc.valid_labelled_loop_control(flat.NodeId(index), node.value) {
			continue
		}
		key := '${node.pos.id}:${node.pos.offset}:${node.kind}'
		if diagnosed[key] {
			continue
		}
		diagnosed[key] = true
		tc.record_invalid_loop_label(flat.NodeId(index), node)
	}
}

fn (mut tc TypeChecker) record_invalid_loop_label(id flat.NodeId, node flat.Node) {
	keyword := if node.kind == .break_stmt { 'break' } else { 'continue' }
	pos := tc.loop_control_keyword_pos(id, keyword)
	message := 'invalid label name `${node.value}`'
	if tc.errors.any(it.msg == message && it.pos.id == pos.id && it.pos.offset == pos.offset
		&& it.pos.end == pos.end)
	{
		return
	}
	tc.errors << tc.make_type_error_at(.unknown_ident, message, id, pos)
}

// valid_labelled_loop_control walks the break/continue statement's ancestor
// chain looking for an enclosing loop whose preceding sibling is a matching
// label. The old implementation scanned every AST node per candidate label and
// brace-matched raw source text, which multiplied into full-AST scans for every
// labelled loop control statement.
fn (tc &TypeChecker) valid_labelled_loop_control(id flat.NodeId, name string) bool {
	mut current := id
	for _ in 0 .. 256 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		if parent.kind in [.for_stmt, .for_in_stmt] && tc.loop_has_label(parent_id, name) {
			return true
		}
		current = parent_id
	}
	return false
}

// loop_has_label reports whether the label statement immediately preceding
// loop_id among its parent's children carries the given name.
fn (tc &TypeChecker) loop_has_label(loop_id flat.NodeId, name string) bool {
	parent_id := tc.direct_parent_id(loop_id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	for i in 1 .. int(parent.children_count) {
		if tc.a.child(parent, i) == loop_id {
			prev := tc.a.node(tc.a.child(parent, i - 1))
			return prev.kind == .label_stmt && prev.value == name
		}
	}
	return false
}

fn (tc &TypeChecker) label_starts_loop(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	for i in 0 .. int(parent.children_count) - 1 {
		if tc.a.child(parent, i) == id {
			return tc.a.node(tc.a.child(parent, i + 1)).kind in [.for_stmt, .for_in_stmt]
		}
	}
	return false
}

fn (tc &TypeChecker) loop_control_keyword_pos(id flat.NodeId, keyword string) token.Pos {
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	return closest_identifier_span(source, keyword, node.pos.offset, node.pos.id) or { node.pos }
}

fn (tc &TypeChecker) statement_exits_sequence(id flat.NodeId, node flat.Node) bool {
	if node.kind == .return_stmt {
		return true
	}
	if node.kind !in [.break_stmt, .continue_stmt] {
		return false
	}
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.for_stmt, .for_in_stmt] {
			return true
		}
		if parent.kind in [.comptime_for, .fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
	}
	return false
}

fn (mut tc TypeChecker) check_missing_array_init_interface_type_args(id flat.NodeId, node flat.Node) {
	name := node.value.trim_space()
	if name.len == 0 || name.contains('[') || !tc.should_diagnose(id) {
		return
	}
	qualified := tc.qualify_name(name)
	if name !in tc.interface_generic_params && qualified !in tc.interface_generic_params {
		return
	}
	tc.record_error_at(.unknown_type,
		'generic interface `${name}` must specify type parameter, e.g. ${name}[int]', id, tc.type_diagnostic_pos(id,
		name))
}

fn (mut tc TypeChecker) check_map_none_values(node flat.Node) {
	mut first_none_id := flat.empty_node
	mut inferred := Type(void_)
	mut none_ids := []flat.NodeId{}
	for i := 1; i < node.children_count; i += 2 {
		value_id := tc.a.child(node, i)
		value := tc.a.node(value_id)
		if value.kind == .none_expr {
			if !tc.valid_node_id(first_none_id) {
				first_none_id = value_id
			}
			none_ids << value_id
			continue
		}
		if inferred is Void {
			inferred = tc.resolve_type(value_id)
		}
	}
	if none_ids.len == 0 {
		return
	}
	if inferred is Void {
		tc.record_error_at(.assignment_mismatch, 'map value cannot be only `none`', first_none_id,
			tc.a.node(first_none_id).pos)
		return
	}
	if unalias_type(inferred) is OptionType {
		return
	}
	for none_id in none_ids {
		tc.record_error_at(.assignment_mismatch,
			'invalid map value: expected `${inferred.name()}`, not `none`', none_id,
			tc.a.node(none_id).pos)
	}
}

fn (mut tc TypeChecker) check_map_literal_result_elements(node flat.Node) bool {
	mut found := false
	for i in 0 .. node.children_count {
		element_id := tc.a.child(&node, i)
		element := tc.a.node(element_id)
		element_type := unalias_type(tc.resolve_type(element_id))
		if element.kind != .call || tc.call_has_postfix_propagation(element) {
			continue
		}
		if element_type is ResultType {
			tc.record_unhandled_result_call(element_id, element_type)
			found = true
		}
	}
	return found
}

fn (tc &TypeChecker) map_literal_has_element_diagnostic(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.pos.id == root.pos.id && it.pos.offset >= root.pos.offset
		&& it.pos.end <= root.pos.end && (it.msg.starts_with('invalid map value: ')
		|| it.msg.starts_with('invalid map key: ')
		|| it.msg.starts_with('invalid map update: ')))
}

fn (mut tc TypeChecker) check_map_literal_element_types(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	for i := 0; i + 1 < node.children_count; i += 2 {
		key_id := tc.a.child(&node, i)
		key := tc.a.node(key_id)
		if key.kind != .prefix || key.value != '...' || key.children_count == 0 {
			continue
		}
		update_id := tc.a.child(key, 0)
		if map_type_from_receiver(tc.resolve_type(update_id)) == none {
			update := tc.a.node(update_id)
			tc.record_error_at(.assignment_mismatch, 'invalid map update: non-map type', update_id,
				update.pos)
		}
	}
	mut expected_key := Type(void_)
	mut expected_value := Type(void_)
	if expected := tc.expected_context_for_expr(id) {
		if expected_map := map_type_from_receiver(expected) {
			expected_key = expected_map.key_type
			expected_value = expected_map.value_type
		}
	}
	if expected_key is Void || expected_value is Void {
		for i := 0; i + 1 < node.children_count; i += 2 {
			key_id := tc.a.child(&node, i)
			key := tc.a.node(key_id)
			if key.kind == .prefix && key.value == '...' && key.children_count > 0 {
				update_id := tc.a.child(key, 0)
				if update_type := map_type_from_receiver(tc.resolve_type(update_id)) {
					expected_key = update_type.key_type
					expected_value = update_type.value_type
					break
				}
				continue
			}
			expected_key = tc.resolve_type(key_id)
			expected_value = tc.resolve_type(tc.a.child(&node, i + 1))
			break
		}
	}
	if expected_key is Void || expected_value is Void {
		return
	}
	for i := 0; i + 1 < node.children_count; i += 2 {
		key_id := tc.a.child(&node, i)
		key := tc.a.node(key_id)
		if key.kind == .prefix && key.value == '...' && key.children_count > 0 {
			update_id := tc.a.child(key, 0)
			if update_type := map_type_from_receiver(tc.resolve_type(update_id)) {
				if !tc.receiver_compatible(update_type.key_type, expected_key)
					|| !tc.receiver_compatible(update_type.value_type, expected_value) {
					expected_map := Type(Map{
						key_type:   expected_key
						value_type: expected_value
					})
					actual_map := Type(update_type)
					tc.record_error_at(.assignment_mismatch,
						'invalid map update: expected `${expected_map.name()}`, not `${actual_map.name()}`',
						update_id, tc.node_value_diagnostic_pos(update_id))
				}
			}
			continue
		}
		value_id := tc.a.child(&node, i + 1)
		tc.check_map_literal_slot_type(key_id, expected_key, 'key')
		if tc.a.node(value_id).kind != .none_expr {
			tc.check_map_literal_slot_type(value_id, expected_value, 'value')
		}
	}
}

fn (mut tc TypeChecker) check_map_literal_slot_type(value_id flat.NodeId, expected Type, slot string) {
	actual := tc.resolve_type(value_id)
	value := tc.a.node(value_id)
	mut compatible := tc.expr_compatible(value_id, actual, expected)
	if expected is OptionType && actual !is OptionType {
		compatible = false
	}
	if value.kind == .float_literal && expected.name() != actual.name() {
		compatible = false
	}
	if value.kind == .char_literal && unalias_type(expected) !is Rune {
		compatible = false
	}
	if compatible {
		return
	}
	tc.record_error_at(.assignment_mismatch, 'invalid map ${slot}: expected `${expected.name()}`, not `${tc.diagnostic_expr_type_name(value_id,
		actual)}`', value_id, value.pos)
}

fn (mut tc TypeChecker) check_spawn_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(node, 0)
	child := tc.a.node(child_id)
	if child.kind == .or_expr && child.children_count >= 2 {
		call_id := tc.a.child(child, 0)
		call := tc.a.node(call_id)
		if call.kind == .call {
			tc.check_node(child_id)
			fallback_id := tc.a.child(child, 1)
			tc.record_error_at(.call_arg_mismatch,
				'option handling cannot be done in `spawn` call. Do it when calling `.wait()`',
				child_id, tc.or_block_operator_pos(call_id, fallback_id))
			return
		}
	}
	error_count := tc.errors.len
	if child.kind != .call {
		tc.record_error_at(.call_arg_mismatch, 'expression in `spawn` must be a function call',
			child_id, child.pos)
	}
	if child.kind == .call {
		if info := tc.resolve_call_info(child_id, *child) {
			callee := tc.a.child_node(child, 0)
			if callee.kind == .selector && callee.children_count > 0
				&& tc.mut_receiver_methods[info.name] {
				receiver_id := tc.a.child(callee, 0)
				tc.record_error_at(.call_arg_mismatch,
					'method in `spawn` statement cannot have non-reference mutable receiver',
					receiver_id, tc.a.node(receiver_id).pos)
			} else {
				for i in 1 .. child.children_count {
					arg_id := tc.call_arg_value(tc.a.child(child, i))
					arg := tc.a.node(arg_id)
					param_idx := i - 1 + if info.has_receiver { 1 } else { 0 }
					if arg.is_mut && tc.call_param_is_mut(info, param_idx)
						&& !tc.call_param_requires_mut_pointer_slot(info, param_idx)
						&& unalias_type(tc.resolve_type(arg_id)) !is Pointer {
						tc.record_error_at(.call_arg_mismatch,
							'function in `spawn` statement cannot contain mutable non-reference arguments',
							arg_id, arg.pos)
					}
				}
			}
		}
	}
	tc.check_node(child_id)
	if child.kind == .call && tc.new_error_kind_since(error_count, .unknown_fn)
		&& !tc.call_targets_later_local_binding(child) {
		tc.record_error_at(.assignment_mismatch, 'invalid expr', child_id, child.pos)
	}
	_ = id
}

fn (tc &TypeChecker) call_targets_later_local_binding(call flat.Node) bool {
	if call.children_count == 0 || tc.fn_context.node_id < 0 {
		return false
	}
	callee := tc.a.child_node(&call, 0)
	if callee.kind != .ident {
		return false
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	mut stack := []flat.NodeId{}
	stack << fn_id
	for stack.len > 0 {
		current_id := stack.pop()
		current := tc.a.node(current_id)
		if current.kind == .decl_assign && current.pos.offset > call.pos.offset {
			for i := 0; i + 1 < current.children_count; i += 2 {
				lhs := tc.a.child_node(current, i)
				if lhs.kind == .ident && lhs.value == callee.value {
					return true
				}
			}
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return false
}

fn (tc &TypeChecker) paren_expr_has_redundant_parentheses(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .paren {
		return false
	}
	text := tc.source_text_for_node(id).trim_space()
	return text.starts_with('((')
}

fn (mut tc TypeChecker) check_map_duplicate_keys(node flat.Node) {
	mut seen := map[string]bool{}
	mut seen_floats := []f64{}
	for i := 0; i + 1 < node.children_count; i += 2 {
		key_id := tc.a.child(&node, i)
		key := tc.a.node(key_id)
		mut duplicate := false
		mut display := key.value
		match key.kind {
			.string_literal {
				map_key := 'string:${key.value}'
				duplicate = seen[map_key]
				seen[map_key] = true
			}
			.int_literal {
				value := v_int_literal_value(key.value) or { continue }
				map_key := 'int:${value}'
				duplicate = seen[map_key]
				seen[map_key] = true
			}
			.float_literal {
				value := key.value.f64()
				for previous in seen_floats {
					if previous == value {
						duplicate = true
						break
					}
				}
				seen_floats << value
			}
			else {
				continue
			}
		}
		if duplicate {
			tc.record_error_at(.duplicate_decl, 'duplicate key "${display}" in map literal',
				key_id, key.pos)
		}
	}
}

fn (mut tc TypeChecker) check_array_literal_element_types(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		if node.typ.len == 0 {
			if expected := tc.expected_context_for_expr(id) {
				if array_like_elem_type(expected) != none {
					tc.register_synth_type(id, expected)
					return
				}
			}
			tc.record_error_at(.assignment_mismatch,
				'array_init: no type specified (maybe: `[]Type{}` instead of `[]`)', id, node.pos)
		}
		return
	}
	if node.typ.len > 0 {
		declared := unalias_type(tc.parse_type(node.typ))
		if declared is ArrayFixed && declared.len != int(node.children_count) {
			tc.record_error_at(.assignment_mismatch,
				'fixed array expects ${declared.len} value(s), but got ${node.children_count}', id,
				tc.fixed_array_value_list_pos(node))
		}
	}
	first_id := tc.a.child(&node, 0)
	if tc.a.nodes[int(first_id)].kind == .none_expr {
		tc.record_error(.assignment_mismatch,
			'invalid expression `none`, it is not an array of Option type', first_id)
		return
	}
	array_type := unalias_type(tc.resolve_type(id))
	elem_type := match array_type {
		Array { array_type.elem_type }
		ArrayFixed { array_type.elem_type }
		else { return }
	}
	if elem_type is Unknown {
		return
	}
	if elem_type is Void {
		if tc.expr_subtree_has_undefined_variable_error(first_id) {
			return
		}
		tc.record_error(.assignment_mismatch, 'invalid void array element type', tc.a.child(&node,
			0))
		return
	}
	if node.children_count < 2 {
		return
	}
	for i in 1 .. node.children_count {
		child_id := tc.a.child(&node, i)
		actual := tc.resolve_type(child_id)
		is_option_mismatch := (actual is OptionType) != (elem_type is OptionType)
		is_pointer_mismatch := elem_type is Pointer && actual !is Pointer
		if actual is Unknown || (tc.expr_compatible(child_id, actual, elem_type)
			&& !is_option_mismatch && !is_pointer_mismatch) {
			continue
		}
		actual_name := tc.array_element_diagnostic_type_name(child_id, actual)
		elem_name := tc.array_element_diagnostic_type_name(tc.a.child(&node, 0), elem_type)
		if actual is OptionType && elem_type !is OptionType {
			tc.record_error_at(.assignment_mismatch,
				'cannot use `?${actual.base_type.name()}` as `${elem_name}`, it must be unwrapped first',
				child_id, tc.array_element_value_pos(child_id))
			continue
		}
		if elem_type is OptionType && actual !is OptionType {
			tc.record_error_at(.assignment_mismatch,
				'cannot use `${actual_name}` as `?${elem_type.base_type.name()}`', child_id,
				tc.array_element_value_pos(child_id))
			continue
		}
		if elem_type is Pointer && elem_type.base_type !is Void && actual !is Pointer {
			tc.record_error(.assignment_mismatch,
				'cannot have non-pointer of type `${actual_name}` in a pointer array of type `&${elem_type.base_type.name()}`',
				child_id)
			continue
		}
		tc.record_error_at(.assignment_mismatch,
			'invalid array element: expected `${elem_name}`, not `${actual_name}`', child_id,
			tc.array_element_diagnostic_pos(child_id))
	}
}

fn (tc &TypeChecker) fixed_array_value_list_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if start < end {
		first := source[start..end].index('[') or { return node.pos }
		after_first := start + first + 1
		if after_first < end {
			if second := source[after_first..end].index('[') {
				value_start := after_first + second
				return token.new_span(node.pos.id, value_start, end)
			}
		}
	}
	return node.pos
}

fn (tc &TypeChecker) array_element_value_pos(id flat.NodeId) token.Pos {
	node := tc.a.nodes[int(id)]
	if node.kind in [.ident, .selector] {
		return tc.node_value_diagnostic_pos(id)
	}
	return node.pos
}

fn (tc &TypeChecker) index_suffix_diagnostic_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if relative := source[start..end].last_index('[') {
		suffix_start := start + relative
		return token.new_span(node.pos.id, suffix_start, end)
	}
	return node.pos
}

fn (tc &TypeChecker) slice_expr_base_is_mutable(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .index || node.value != 'range' || node.children_count < 1 {
		return false
	}
	return tc.expr_root_is_mutable_lvalue(tc.a.child(node, 0))
}

fn (mut tc TypeChecker) record_implicit_slice_clone_notice(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind != .index || node.value != 'range' || node.children_count < 1 {
		return
	}
	pos := tc.index_suffix_diagnostic_pos(id)
	file := tc.a.source_files[pos.id] or { return }
	detail := v3errors.formatted_source_error('details:',
		'To silence this notice, use either an explicit `a[..].clone()`,\nor use an explicit `unsafe{ a[..] }`, if you do not want a copy of the slice.',
		file, pos)
	tc.record_notice_with_details_at(.assignment_mismatch,
		'an implicit clone of the slice was done here', id, pos, [detail])
}

fn (tc &TypeChecker) array_element_diagnostic_type_name(id flat.NodeId, typ Type) string {
	node := tc.a.nodes[int(id)]
	if node.kind == .block && node.value == 'unsafe' && tc.node_contains_nil_literal(id) {
		return 'voidptr'
	}
	if node.kind == .int_literal {
		return typ.name()
	}
	return tc.diagnostic_expr_type_name(id, typ)
}

fn (tc &TypeChecker) node_contains_nil_literal(id flat.NodeId) bool {
	node := tc.a.nodes[int(id)]
	if node.kind == .nil_literal {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.node_contains_nil_literal(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) array_element_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.nodes[int(id)]
	if node.kind != .block || node.value != 'unsafe' {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	position := file.position(node.pos)
	line_start := file.line_start(position.line)
	line_end := source.index_after('\n', line_start) or { source.len }
	line := source[line_start..line_end]
	if unsafe_start := line.index('unsafe {') {
		start := line_start + unsafe_start
		return token.new_span(node.pos.id, start, start + 'unsafe'.len)
	}
	return node.pos
}

fn (mut tc TypeChecker) check_dump_expr(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	expr_id := tc.a.child(&node, 0)
	invalid_import_function := tc.invalid_unknown_import_function_print_arg(expr_id)
	error_count := tc.errors.len
	tc.check_node(expr_id)
	expr_type := tc.resolve_type(expr_id)
	expr := tc.a.node(expr_id)
	deprecation_error := tc.errors.any(it.pos.id == expr.pos.id && it.pos.offset >= expr.pos.offset
		&& it.pos.end <= expr.pos.end && it.msg.contains(' has been deprecated since '))
	if unalias_type(expr_type) is Char {
		tc.record_error_at(.assignment_mismatch,
			'`char` values cannot be dumped directly, use dump(u8(x)) or dump(int(x)) instead',
			expr_id, tc.a.node(expr_id).pos)
		return
	}
	mut private_type := unalias_type(expr_type)
	for private_type is OptionType || private_type is ResultType || private_type is Pointer {
		private_type = match private_type {
			OptionType { unalias_type(private_type.base_type) }
			ResultType { unalias_type(private_type.base_type) }
			Pointer { unalias_type(private_type.base_type) }
			else { private_type }
		}
	}
	if private_type is Struct {
		if visibility := tc.private_declaration(private_type.name) {
			if tc.concrete_method_signature_key(private_type.name, 'str') == none {
				type_name := tc.diagnostic_type_name(Type(private_type))
				module_name := if type_name.contains('.') {
					type_name.all_before_last('.')
				} else {
					visibility.module_name
				}
				tc.record_error_at(.assignment_mismatch,
					'cannot dump private type `${type_name}` outside module `${module_name}` without an explicit `str()` method',
					expr_id, tc.a.node(expr_id).pos)
				return
			}
		}
	}
	if !deprecation_error && (expr_type is Void || (expr_type is Unknown && (invalid_import_function
		|| tc.expr_subtree_has_error(expr_id)))
		|| tc.new_error_kind_since(error_count, .unknown_fn)) {
		pos := if expr.kind == .call {
			if callee, _, _ := tc.unknown_import_function_call_parts(expr) {
				tc.method_call_name_pos(expr, callee)
			} else {
				expr.pos
			}
		} else {
			expr.pos
		}
		tc.record_error_at(.assignment_mismatch, 'dump expression can not be void', expr_id, pos)
	}
}

fn (mut tc TypeChecker) check_assert_stmt(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	unsafe_alias_state := tc.fn_context.unsafe_reference_alias_owners.clone()
	condition_id := tc.a.child(&node, 0)
	tc.check_node(condition_id)
	tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_state.clone()
	if node.children_count > 1 {
		message_id := tc.a.child(&node, 1)
		tc.check_node(message_id)
		tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_state.clone()
		message_type := tc.resolve_type(message_id)
		if !unalias_type(message_type).is_string() {
			tc.record_error_at(.condition_mismatch, 'assert allows only a single string as its second argument, but found `${tc.diagnostic_expr_type_name(message_id,
				message_type)}` instead', message_id, tc.a.node(message_id).pos)
		}
	}
	condition_type := if tc.expr_has_multi_match_member_error(condition_id) {
		Type(void_)
	} else {
		tc.resolve_type(condition_id)
	}
	if tc.expr_contains_invalid_variable_error(condition_id)
		|| tc.expr_contains_unknown_enum_error(condition_id) {
		tc.record_error_at(.condition_mismatch,
			'assert can be used only with `bool` expressions, but found `void` instead',
			condition_id, tc.assert_condition_diagnostic_pos(condition_id))
		return
	}
	if condition_type is Unknown {
		if condition_type.reason != 'invalid variable'
			&& !tc.expr_contains_invalid_variable_binding(condition_id) {
			return
		}
		tc.record_error_at(.condition_mismatch,
			'assert can be used only with `bool` expressions, but found `void` instead',
			condition_id, tc.assert_condition_diagnostic_pos(condition_id))
		return
	}
	if unalias_type(condition_type).name() == 'bool' {
		return
	}
	clean_condition := unalias_type(condition_type)
	condition_name := if clean_condition is OptionType
		&& unalias_type(clean_condition.base_type).name() == 'bool' {
		'bool'
	} else {
		condition_type.name()
	}
	tc.record_error_at(.condition_mismatch,
		'assert can be used only with `bool` expressions, but found `${condition_name}` instead',
		condition_id, tc.assert_condition_diagnostic_pos(condition_id))
}

fn (tc &TypeChecker) expr_contains_unknown_enum_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if tc.errors.any(it.node == id && it.msg.starts_with('unknown enum `')) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_contains_unknown_enum_error(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_contains_invalid_variable_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if tc.errors.any(it.node == id && it.msg.starts_with('invalid variable `')) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_contains_invalid_variable_error(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_has_multi_match_member_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.pos.id == root.pos.id && it.pos.offset >= root.pos.offset
		&& it.pos.end <= root.pos.end && (it.msg.contains(' has no field or method `')
		|| it.msg.starts_with('unknown method: `') || (it.msg.starts_with('field `')
		&& it.msg.contains(' does not exist or have the same type in these sumtype `'))))
}

fn (tc &TypeChecker) expr_contains_multi_pattern_subject_member(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .selector && node.children_count > 0
		&& tc.ident_is_multi_pattern_match_subject(tc.a.child(node, 0)) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_contains_multi_pattern_subject_member(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) assert_condition_diagnostic_pos(condition_id flat.NodeId) token.Pos {
	condition := tc.a.nodes[int(condition_id)]
	file := tc.a.source_files[condition.pos.id] or { return condition.pos }
	source := tc.source_texts_by_file[file.name] or { return condition.pos }
	position := file.position(condition.pos)
	line_start := file.line_start(position.line)
	line_end := source.index_after('\n', line_start) or { source.len }
	if line_start < line_end {
		line := source[line_start..line_end]
		assert_relative := line.index('assert ') or { return condition.pos }
		start := line_start + assert_relative + 'assert '.len
		end := line_start + line.trim_right(' \t\r\n').len
		return token.new_span(condition.pos.id, start, end)
	}
	return condition.pos
}

fn valid_utf8_string(value string) bool {
	mut i := 0
	for i < value.len {
		first := value[i]
		if first < 0x80 {
			i++
			continue
		}
		mut continuation_count := 0
		mut second_min := u8(0x80)
		mut second_max := u8(0xbf)
		if first >= 0xc2 && first <= 0xdf {
			continuation_count = 1
		} else if first >= 0xe0 && first <= 0xef {
			continuation_count = 2
			if first == 0xe0 {
				second_min = 0xa0
			} else if first == 0xed {
				second_max = 0x9f
			}
		} else if first >= 0xf0 && first <= 0xf4 {
			continuation_count = 3
			if first == 0xf0 {
				second_min = 0x90
			} else if first == 0xf4 {
				second_max = 0x8f
			}
		} else {
			return false
		}
		if i + continuation_count >= value.len {
			return false
		}
		second := value[i + 1]
		if second < second_min || second > second_max {
			return false
		}
		for j in 2 .. continuation_count + 1 {
			if value[i + j] < 0x80 || value[i + j] > 0xbf {
				return false
			}
		}
		i += continuation_count + 1
	}
	return true
}

fn (tc &TypeChecker) string_literal_source_is_valid_utf8(pos token.Pos) bool {
	file := tc.a.source_files[pos.id] or { return true }
	source := tc.source_texts_by_file[file.name] or { return true }
	if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
		return true
	}
	return valid_utf8_string(source[pos.offset..pos.end])
}

fn (tc &TypeChecker) string_literal_diagnostic_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	if start >= source.len || source[start] !in [`'`, `"`] {
		return node.pos
	}
	quote := source[start]
	mut i := start + 1
	for i < source.len {
		if source[i] == `\\` {
			i += 2
			continue
		}
		if source[i] == quote {
			return token.new_span(node.pos.id, start, i + 1)
		}
		if source[i] == `\n` {
			break
		}
		i++
	}
	return node.pos
}

fn (tc &TypeChecker) string_interpolation_expr_pos(expr_id flat.NodeId) token.Pos {
	expr := tc.a.nodes[int(expr_id)]
	if expr.kind == .call && expr.children_count > 0 {
		callee := tc.a.child_node(&expr, 0)
		if callee.kind == .selector {
			return tc.method_call_name_pos(expr, callee)
		}
	}
	file := tc.a.source_files[expr.pos.id] or { return expr.pos }
	source := tc.source_texts_by_file[file.name] or { return expr.pos }
	mut start := int_max(0, expr.pos.offset)
	mut end := int_min(expr.pos.end, source.len)
	if start + 3 <= end && source[start] in [`'`, `"`] && source[start + 1] == `$`
		&& source[start + 2] == `{` {
		start += 2
	} else if start + 2 <= end && source[start] == `$` && source[start + 1] == `{` {
		start += 2
	}
	if start < end && source[end - 1] == `}` {
		end--
	}
	return token.new_span(expr.pos.id, start, end)
}

fn (mut tc TypeChecker) check_string_interpolation_format(id flat.NodeId, node flat.Node, expr_id flat.NodeId) {
	fmt := node.typ
	if fmt.len == 0 {
		return
	}
	mut letters := []u8{}
	for ch in fmt {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) {
			letters << ch
		}
	}
	if letters.len > 1 {
		tc.record_error_at(.call_arg_mismatch, 'format specifier may only be one letter', id,
			node.pos)
		return
	}
	if letters.len == 0 {
		return
	}
	spec := letters[0]
	spec_pos := token.new_span(node.pos.id, node.pos.end - 1, node.pos.end)
	known := spec in [`d`, `u`, `x`, `X`, `o`, `b`, `c`, `s`, `e`, `E`, `f`, `F`, `g`, `G`]
	if !known {
		tc.record_error_at(.call_arg_mismatch, 'unknown format specifier `${spec.ascii_str()}`',
			id, spec_pos)
	}
	actual := tc.resolve_type(expr_id)
	clean := unalias_type(actual)
	if fmt.contains('.') && !clean.is_float() {
		tc.record_error_at(.call_arg_mismatch,
			'precision specification only valid for float types', id, spec_pos)
		return
	}
	mut allowed := false
	if clean.is_string() {
		allowed = spec == `s`
	} else if clean.is_float() {
		allowed = spec in [`e`, `E`, `f`, `F`, `g`, `G`]
	} else if clean.is_integer() {
		is_unsigned := if clean is Primitive { clean.props.has(.unsigned) } else { clean is USize }
		allowed = if is_unsigned {
			spec in [`u`, `x`, `X`, `o`, `b`, `c`]
		} else {
			spec in [`d`, `x`, `X`, `o`, `b`, `c`]
		}
	}
	if !allowed {
		actual_name := if tc.a.node(expr_id).kind == .none_expr { 'none' } else { actual.name() }
		tc.record_error_at(.call_arg_mismatch,
			'illegal format specifier `${spec.ascii_str()}` for type `${actual_name}`', id,
			spec_pos)
	}
}

fn (mut tc TypeChecker) check_prefix_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	child := tc.a.node(child_id)
	if node.op == .minus && child.kind == .int_literal {
		tc.check_untyped_integer_literal_overflow(id)
	}
	if node.value == 'unexpected_amp' {
		tc.record_error_at(.assignment_mismatch, 'unexpected `&`, expecting expression', id,
			node.pos)
		return
	}
	if node.op == .amp && child.kind == .nil_literal {
		tc.record_error_at(.assignment_mismatch, 'invalid operation: cannot take address of nil',
			id, tc.address_operator_pos(id))
		return
	}
	if node.op == .amp && child.kind == .paren && child.children_count == 1 {
		inner := tc.a.child_node(child, 0)
		if inner.kind == .struct_init {
			tc.record_notice_at(.assignment_mismatch,
				'unnecessary `()`, use `&${inner.value}{....}` instead of `&(${inner.value}{....})`',
				child_id, child.pos)
		}
	}
	if node.op == .amp && tc.address_operand_is_method_value(child_id) {
		display := strip_redundant_outer_parens(tc.source_text_for_node(child_id))
		tc.record_error_at(.assignment_mismatch, 'cannot take the address of ${display}', id,
			tc.address_operator_pos(id))
		return
	}
	tc.check_node(child_id)
	child_type := unalias_type(tc.resolve_type(child_id))
	if child_type is Unknown {
		return
	}
	if child_type is Void && tc.expr_subtree_has_error(child_id) {
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op == .amp && tc.node_source_starts_with(id, '&') && tc.unsafe_depth == 0
		&& !tc.expr_is_inside_unsafe_block(id) {
		if fixed_array_id := tc.fixed_array_reference_ident(child_id) {
			name := tc.a.node(fixed_array_id).value
			tc.record_error_at(.assignment_mismatch,
				'cannot reference fixed array `${name}` outside `unsafe` blocks as it is supposed to be stored on stack',
				fixed_array_id, tc.node_value_diagnostic_pos(fixed_array_id))
			return
		}
	}
	if node.op == .arrow {
		channel_type := unalias_and_unwrap_pointer_type(child_type)
		if channel_type !is Channel {
			tc.record_error_at(.assignment_mismatch,
				'operator `<-` can only be used with `chan` types, but the value after `<-` is of type `${child_type.name()}` instead',
				id, tc.prefix_operator_pos(id, '<-'))
		}
		return
	}
	if node.op in [.plus, .minus] && !infix_power_type_is_numeric(child_type) {
		op := if node.op == .minus { '-' } else { '+' }
		tc.record_error_at(.assignment_mismatch,
			'operator `${op}` can only be used with numeric types, but the value after `${op}` is of type `${child_type.name()}` instead',
			id, tc.prefix_operator_pos(id, op))
		return
	}
	if node.op == .bit_not && child_type is Enum {
		if !child_type.is_flag {
			tc.record_error_at(.assignment_mismatch,
				'operator `~` can only be used with `@[flag]` tagged enums', id, tc.prefix_operator_pos(id,
				'~'))
		}
		return
	}
	if node.op == .bit_not && !child_type.is_integer() {
		tc.record_error_at(.assignment_mismatch, 'operator `~` can only be used with integer types, but the value after `~` is of type `${tc.diagnostic_expr_type_name(child_id,
			child_type)}` instead', id, tc.prefix_operator_pos(id, '~'))
		return
	}
	if node.op == .not && !tc.type_compatible(child_type, Type(bool_)) {
		tc.record_error_at(.assignment_mismatch, 'operator `!` can only be used with bool types, but the value after `!` is of type `${tc.diagnostic_expr_type_name(child_id,
			child_type)}` instead', id, tc.prefix_operator_pos(id, '!'))
		return
	}
	if node.op == .mul && child_type !is Pointer && child_type !is OptionType {
		tc.record_error_at(.assignment_mismatch,
			'invalid indirect of `${child_type.name()}`, the type `${child_type.name()}` is not a pointer',
			id, tc.prefix_operator_pos(id, '*'))
		return
	}
	if node.op == .mul && child_type is Pointer {
		if child.kind == .nil_literal {
			tc.record_error_at(.assignment_mismatch, 'cannot deference a `nil` pointer', child_id,
				child.pos)
			return
		}
		diagnostic_name := tc.pointer_diagnostic_binding_type_name(child_id, child_type)
		if diagnostic_name == 'nil' {
			tc.record_error_at(.assignment_mismatch, 'cannot deference a `nil` pointer', id,
				tc.node_value_diagnostic_pos(child_id))
			return
		}
		if unalias_type(child_type.base_type) is Void {
			tc.record_error_at(.assignment_mismatch, 'cannot dereference to void', id, tc.prefix_operator_pos(id,
				'*'))
			return
		}
	}
	if node.op == .mul && child_type is OptionType {
		payload := unalias_type(child_type.base_type)
		if payload is Pointer {
			child_text := tc.source_text_for_node(child_id)
			tc.record_error_at(.assignment_mismatch,
				'type `?${payload.base_type.name()}` is an Option, it must be unwrapped first; use `*${child_text}?` to do it',
				child_id, tc.node_value_diagnostic_pos(child_id))
			tc.register_synth_type(id, Type(OptionType{
				base_type: payload.base_type
			}))
			return
		}
	}
	mut address_child := tc.a.nodes[int(child_id)]
	if address_child.kind == .paren && address_child.children_count > 0 {
		address_child = *tc.a.child_node(&address_child, 0)
	}
	if node.op == .amp && address_child.kind == .prefix && address_child.op == .amp {
		tc.record_error_at(.assignment_mismatch, 'cannot take the address of this expression', id, token.new_span(address_child.pos.id,
			address_child.pos.offset, address_child.pos.offset + 1))
		return
	}
	if node.op == .amp && address_child.kind == .selector && address_child.children_count > 0 {
		selector_base_id := tc.a.child(&address_child, 0)
		selector_base := tc.a.node(selector_base_id)
		if selector_base.kind in [.int_literal, .float_literal, .bool_literal, .char_literal,
			.string_literal, .string_interp] {
			operator_pos := tc.address_operator_pos(id)
			tc.record_error_at(.assignment_mismatch, 'cannot take the address of a literal value',
				id, token.new_span(node.pos.id, operator_pos.offset, address_child.pos.end))
			return
		}
		if declared := tc.selector_declared_value_type(address_child) {
			if unalias_type(declared) is OptionType {
				tc.record_error_at(.assignment_mismatch,
					'cannot take the address of an Option field', id, node.pos)
				return
			}
		}
		if selector_base.kind == .struct_init {
			tc.record_error_at(.assignment_mismatch,
				'should not create object instance on the heap to simply access a member', id,
				node.pos)
			return
		}
	}
	if node.op == .amp && address_child.kind == .index && address_child.children_count > 0 {
		base_id := tc.a.child(&address_child, 0)
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(base_id))
		if base_type is Map && tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id) {
			tc.record_error_at(.assignment_mismatch,
				'cannot take the address of map values outside `unsafe`', child_id,
				tc.index_brackets_pos(address_child))
			return
		}
		base := tc.a.node(base_id)
		if base_type is Array && base.kind == .ident && tc.ident_is_mutable_lvalue(base.value)
			&& tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id) {
			tc.record_error_at(.assignment_mismatch,
				'cannot take the address of mutable array elements outside unsafe blocks',
				child_id, tc.index_brackets_pos(address_child))
			return
		}
	}
	if node.op == .amp && tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id)
		&& tc.address_path_contains_map_index(child_id) {
		tc.record_error_at(.assignment_mismatch,
			'cannot take the address of map values outside `unsafe`', id,
			tc.address_operator_pos(id))
		return
	}
	if node.op == .amp && tc.pointer_to_array_reinterpret_cast(child_id) {
		return
	}
	if node.op == .amp && address_child.kind == .array_literal {
		return
	}
	if node.op != .amp || tc.expr_can_take_address(child_id) || address_child.kind == .struct_init {
		return
	}
	display := strip_redundant_outer_parens(tc.source_text_for_node(child_id))
	tc.record_error_at(.assignment_mismatch, 'cannot take the address of ${display}', id,
		tc.address_operator_pos(id))
}

fn (tc &TypeChecker) pointer_to_array_reinterpret_cast(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .cast_expr || node.children_count == 0 {
		return false
	}
	target := unalias_type(tc.parse_type(node.value))
	if target !is Array && target !is ArrayFixed {
		return false
	}
	actual := unalias_type(tc.resolve_type(tc.a.child(node, 0)))
	if actual !is Pointer {
		return false
	}
	actual_pointer := actual as Pointer
	return unalias_type(actual_pointer.base_type).name() == target.name()
}

fn (tc &TypeChecker) fixed_array_reference_ident(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .index] && node.children_count > 0 {
		return tc.fixed_array_reference_ident(tc.a.child(node, 0))
	}
	if node.kind != .ident {
		return none
	}
	raw := tc.resolve_type(id)
	candidate := if raw is Pointer {
		raw.base_type
	} else {
		unalias_type(raw)
	}
	if unalias_type(candidate) is ArrayFixed {
		return id
	}
	return none
}

fn (tc &TypeChecker) address_path_contains_map_index(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind !in [.index, .selector, .paren] || node.children_count == 0 {
		return false
	}
	base_id := tc.a.child(node, 0)
	if node.kind == .index {
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(base_id))
		if base_type is Map {
			return true
		}
	}
	return tc.address_path_contains_map_index(base_id)
}

fn strip_redundant_outer_parens(text string) string {
	mut clean := text.trim_space()
	for clean.len >= 2 && clean[0] == `(` && clean[clean.len - 1] == `)` {
		mut depth := 0
		mut encloses_all := true
		for i, ch in clean {
			if ch == `(` {
				depth++
			} else if ch == `)` {
				depth--
				if depth == 0 && i != clean.len - 1 {
					encloses_all = false
					break
				}
			}
		}
		if !encloses_all || depth != 0 {
			break
		}
		clean = clean[1..clean.len - 1].trim_space()
	}
	return clean
}

fn (tc &TypeChecker) address_operand_is_method_value(id flat.NodeId) bool {
	mut operand_id := id
	for tc.valid_node_id(operand_id) {
		operand := tc.a.node(operand_id)
		if operand.kind != .paren || operand.children_count == 0 {
			break
		}
		operand_id = tc.a.child(operand, 0)
	}
	if !tc.valid_node_id(operand_id) {
		return false
	}
	selector := tc.a.node(operand_id)
	if selector.kind != .selector || selector.children_count == 0 {
		return false
	}
	base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(selector, 0)))
	type_name := resolve_type_name_for_method(base_type)
	if type_name.len == 0 {
		return false
	}
	method_name := '${type_name}.${selector.value}'
	return method_name in tc.fn_ret_types || tc.cached_c_name(method_name) in tc.fn_ret_types
}

fn (tc &TypeChecker) index_brackets_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if open_relative := source[start..end].last_index('[') {
			open := start + open_relative
			if close_relative := source[open..end].index(']') {
				return token.new_span(node.pos.id, open, open + close_relative + 1)
			}
		}
	}
	return node.pos
}

fn (tc &TypeChecker) invalid_address_expr_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	mut node := tc.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		node = *tc.a.child_node(&node, 0)
	}
	if node.kind == .prefix && node.op == .amp {
		file := tc.a.source_files[node.pos.id] or { return node.pos }
		source := tc.source_texts_by_file[file.name] or { return node.pos }
		start := int_max(0, node.pos.offset)
		end := int_min(node.pos.end, source.len)
		if start < end {
			if relative := source[start..end].index('&') {
				op_start := start + relative
				return token.new_span(node.pos.id, op_start, op_start + 1)
			}
		}
	}
	return node.pos
}

fn (mut tc TypeChecker) check_ownership_array_spread_clone(id flat.NodeId, node flat.Node) {
	mut has_spread := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			has_spread = true
			break
		}
	}
	if !has_spread {
		return
	}
	array_type := array_type_from_receiver(tc.resolve_type(id)) or { return }
	if bad_type := tc.ownership_default_clone_missing_method(array_type.elem_type) {
		tc.record_error(.call_arg_mismatch,
			'cannot clone array spread elements: `${bad_type}` requires ownership destruction but has no `clone()` method',
			id)
	}
}

fn (mut tc TypeChecker) check_ownership_map_spread_clone(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	first := tc.a.child_node(&node, 0)
	if first.kind != .prefix || first.value != '...' || first.children_count == 0 {
		return
	}
	map_type := map_type_from_receiver(tc.resolve_type(id)) or { return }
	if bad_type := tc.ownership_default_clone_missing_method(map_type.key_type) {
		tc.record_error(.call_arg_mismatch,
			'cannot clone map spread keys: `${bad_type}` requires ownership destruction but has no `clone()` method',
			id)
	}
	if bad_type := tc.ownership_default_clone_missing_method(map_type.value_type) {
		tc.record_error(.call_arg_mismatch,
			'cannot clone map spread values: `${bad_type}` requires ownership destruction but has no `clone()` method',
			id)
	}
}

fn (mut tc TypeChecker) check_cast_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	if node.value == 'any' {
		tc.record_error(.unknown_type, 'cannot use type `any` here', id)
		return
	}
	target := tc.parse_type(node.value)
	if generic_name := tc.bare_generic_decl_type_name(node.value) {
		qualified := tc.qualify_name(generic_name)
		sum_params := tc.sum_generic_params[generic_name] or {
			tc.sum_generic_params[qualified] or { []string{} }
		}
		if sum_params.len > 0 {
			tc.record_error_at(.unknown_type,
				'generic sumtype `${generic_name}` must specify type parameter, e.g. ${generic_name}[int]',
				id, node.pos)
			return
		}
		params := tc.interface_generic_params[generic_name] or {
			tc.interface_generic_params[qualified] or { []string{} }
		}
		if params.len > 0 {
			actual := tc.resolve_type(child_id)
			actual_name := method_type_name(unwrap_pointer(actual))
			interface_name := if generic_name in tc.interface_names {
				generic_name
			} else {
				qualified
			}
			for method in tc.interface_abstract_method_names(interface_name) {
				if tc.concrete_method_signature_key(actual_name, method) == none {
					tc.record_error_at(.assignment_mismatch,
						'can not find method `${method}` on `${actual_name.all_after_last('.')}`, needed for interface: `${generic_name.all_after_last('.')}`',
						id, node.pos)
					return
				}
			}
			tc.record_error_at(.unknown_type,
				'could not infer generic type `${params[0]}` in interface `${generic_name}`', id,
				node.pos)
			return
		}
	}
	generic_name := node.value.all_after_last('.')
	target_is_generic_param := is_bare_generic_param(generic_name)
		&& (generic_name in tc.fn_context.generic_params || tc.active_generic_param(generic_name)
		|| tc.node_has_enclosing_generic_param(id, generic_name)
		|| tc.source_enclosing_fn_has_generic_param(id, generic_name))
	if target is Unknown {
		if target_is_generic_param {
			actual_generic_source := tc.resolve_type(child_id)
			if fn_param_is_voidptr_type(actual_generic_source) {
				tc.record_error_at(.assignment_mismatch, 'cannot cast `voidptr` to struct', id,
					node.pos)
			}
			return
		}
		tc.record_error_at(.unknown_type, 'unknown type `${node.value}`', id, node.pos)
		return
	}
	if target is Struct && should_check_named_type(node.value) && !tc.type_name_known(node.value)
		&& !target_is_generic_param {
		tc.record_error_at(.unknown_type, 'unknown type `${node.value}`', id, node.pos)
		return
	}
	if target is Pointer {
		target_base := fn_param_unalias_type(target.base_type)
		target_base_name := node.value.trim_left('&')
		if (target_base is Struct || target_base is Unknown)
			&& should_check_named_type(target_base_name) && !tc.type_name_known(target_base_name)
			&& !target_is_generic_param {
			tc.record_error_at(.unknown_type, 'unknown type `${target_base_name}`', id, node.pos)
			return
		}
	}
	if target is Alias && target.base_type is OptionType {
		tc.record_error(.assignment_mismatch,
			'alias to Option type requires to be used as Option type (?${node.value}(...))', id)
		return
	}
	actual := tc.resolve_type(child_id)
	if actual is Void {
		tc.record_error_at(.assignment_mismatch,
			'expression does not return a value so it cannot be cast', child_id,
			tc.a.nodes[int(child_id)].pos)
		return
	}
	if target_struct := struct_type_from_type(target) {
		actual_is_voidptr := fn_param_is_voidptr_type(actual)
			|| (tc.expr_tail_is_nil(child_id) && tc.node_source_starts_with(child_id, 'unsafe'))
		if actual_is_voidptr {
			if target is Alias {
				tc.record_error_at(.assignment_mismatch,
					'cannot cast `voidptr` to `${target.name}` (alias to `${target.base_type.name()}`)',
					id, node.pos)
			} else {
				tc.record_error_at(.assignment_mismatch, 'cannot cast `voidptr` to struct', id,
					node.pos)
			}
			_ = target_struct
			return
		}
	}
	if target is OptionType && (actual is None || tc.a.node(child_id).kind == .none_expr) {
		tc.register_synth_type(id, target)
		return
	}
	if actual is None || tc.a.nodes[int(child_id)].kind == .none_expr {
		target_name := if target_fn := fn_type_from_type(target) {
			Type(target_fn).name().replace('fn(', 'fn (')
		} else if node.value.len > 0 {
			node.value
		} else {
			target.name()
		}
		tc.record_error_at(.assignment_mismatch, 'cannot cast `none` to `${target_name}`', id,
			node.pos)
		return
	}
	if target is OptionType {
		clean_payload := unalias_type(target.base_type)
		if clean_payload is SumType {
			if tc.sum_type_contains_variant(clean_payload, actual) {
				tc.register_synth_type(id, target)
				return
			}
			target_name := '?${target.base_type.name()}'
			tc.record_error_at(.assignment_mismatch,
				'cannot cast `${actual.name()}` to `${target_name}`', id, tc.cast_expression_diagnostic_pos(node,
				target.base_type.name()))
			return
		}
	}
	if actual is OptionType {
		if target is Alias {
			return
		}
		tc.record_error_at(.assignment_mismatch, 'cannot type cast an Option', id, node.pos)
		return
	}
	if actual is ResultType && tc.a.nodes[int(child_id)].kind == .call {
		tc.record_unhandled_result_call(child_id, actual)
		return
	}
	if tc.check_function_cast(id, node, child_id, actual, target) {
		return
	}
	if tc.expr_tail_is_nil(child_id) {
		return
	}
	if actual is Unknown || type_contains_unknown(actual) {
		return
	}
	clean_pointer_target := fn_param_unalias_type(target)
	if clean_pointer_target is Pointer {
		target_pointer := clean_pointer_target as Pointer
		target_name := if node.value.len > 0 { node.value } else { target.name() }
		target_base := fn_param_unalias_type(target_pointer.base_type)
		alias_name := target_name.trim_left('&')
		alias_target_text := tc.type_aliases[alias_name] or {
			tc.type_aliases[tc.qualify_name(alias_name)] or { '' }
		}
		if target_name.starts_with('&') && alias_target_text.starts_with('map[')
			&& unalias_type(actual) is Map {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast to alias pointer `${target_name}` because `${alias_target_text}` is a value',
				id, tc.cast_expression_diagnostic_pos(node, target_name))
			return
		}
		if target_base is Alias && unalias_type(target_base.base_type) is Map
			&& unalias_type(actual) is Map {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast to alias pointer `${target_name}` because `${target_base.base_type.name()}` is a value',
				id, node.pos)
			return
		}
		if infix_power_type_is_numeric(actual) && target_name !in ['voidptr', 'byteptr', 'charptr'] {
			if tc.cast_operand_is_zero(child_id) {
				kind := if struct_type_from_type(target_base) != none {
					'a struct pointer'
				} else {
					'a pointer'
				}
				tc.record_error_at(.assignment_mismatch,
					'cannot null cast ${kind}, use ${target_name}(unsafe { nil })', id, tc.cast_expression_diagnostic_pos(node,
					target_name))
				return
			}
			if tc.unsafe_depth == 0 {
				message := if struct_type_from_type(target_base) != none {
					'cannot cast int to a struct pointer outside `unsafe`'
				} else {
					'cannot cast a number to `${target_name}` outside `unsafe`'
				}
				tc.record_error_at(.assignment_mismatch, message, id, node.pos)
				return
			}
		}
		if tc.unsafe_depth == 0 && fn_param_is_voidptr_type(actual) {
			if unalias_type(target_base) is SumType {
				tc.record_error_at(.assignment_mismatch,
					'cannot cast voidptr to `${target_name}` outside `unsafe`', id, node.pos)
			} else {
				tc.record_warning_at(.assignment_mismatch,
					'casting voidptr to `${target_name}` is only allowed in `unsafe` code', id,
					node.pos)
			}
			return
		}
		if tc.unsafe_depth == 0 && unalias_type(actual) is Pointer
			&& struct_type_from_type(target_base) != none && actual.name() != target_name {
			tc.record_warning_at(.assignment_mismatch,
				'casting `${actual.name()}` to `${target_name}` is only allowed in `unsafe` code',
				id, node.pos)
			return
		}
	}
	tc.check_integer_literal_cast_overflow(id, node, child_id, target)
	clean_target := unalias_type(target)
	clean_actual := unalias_type(actual)
	if clean_actual is ArrayFixed && clean_target is Pointer && !tc.node_is_in_translated_file(id) {
		tc.record_warning_at(.assignment_mismatch,
			'cannot cast a fixed array (use e.g. `&arr[0]` instead)', id, node.pos)
	}
	if (clean_target is Array || clean_target is ArrayFixed)
		&& (clean_actual is Pointer || fn_param_is_voidptr_type(clean_actual)) {
		mut points_to_target := false
		if clean_actual is Pointer {
			actual_pointer := clean_actual as Pointer
			actual_base := unalias_type(actual_pointer.base_type)
			points_to_target = actual_base.name() == clean_target.name()
		}
		if !points_to_target {
			full_pos := tc.cast_expression_diagnostic_pos(node, node.value)
			tc.record_error_at(.assignment_mismatch,
				'cannot cast pointer type `${actual.name()}` to array type `${target.name()}`', id, token.new_pos(full_pos.id,
				full_pos.offset))
			return
		}
	}
	if clean_target is SumType && !tc.direct_sum_assignment_variant_matches(actual, clean_target) {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast `${actual.name()}` to `${target.name()}`', id, node.pos)
		return
	}
	if clean_target is Enum && infix_power_type_is_numeric(clean_actual) && tc.unsafe_depth == 0 {
		tc.record_error_at(.assignment_mismatch,
			'casting numbers to enums, should be done inside `unsafe{}` blocks', id, node.pos)
		return
	}
	if clean_target is Enum && infix_power_type_is_numeric(clean_actual) && tc.unsafe_depth > 0 {
		tc.warn_invalid_enum_cast(id, node, child_id, clean_target)
	}
	if tc.a.node(child_id).kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal]
		&& (clean_target is Array || clean_target is ArrayFixed || clean_target is Map) {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast literal value to ${target.name()} type', id, node.pos)
		return
	}
	if clean_target is Primitive && clean_target.props.has(.boolean) {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast to bool - use e.g. `some_int != 0` instead', id, node.pos)
		return
	}
	if clean_target is Primitive
		&& (clean_target.props.has(.integer) || clean_target.props.has(.float)) {
		if clean_actual is Array {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast array `${actual.name()}` to `${target.name()}`', id, node.pos)
			return
		}
		if clean_actual is FnType {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast function `${tc.source_text_for_node(child_id)}` to `${target.name()}`',
				id, node.pos)
			return
		}
		if clean_actual is Struct {
			message := if clean_actual.name.starts_with('C.') {
				'cannot cast type `${actual.name()}` to `${target.name()}`'
			} else {
				'cannot cast struct `${actual.name()}` to `${target.name()}`'
			}
			tc.record_error_at(.assignment_mismatch, message, id, node.pos)
			return
		}
		if clean_actual is SumType {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast `${actual.name()}` sum type value to `${target.name()}`, use `${tc.source_text_for_node(child_id)} as ${target.name()}` instead',
				id, node.pos)
			return
		}
	}
	if clean_target is Rune && clean_actual is Struct {
		tc.record_error_at(.assignment_mismatch, 'cannot cast struct `${actual.name()}` to `rune`',
			id, node.pos)
		return
	}
	if target is Alias && unalias_type(target.base_type) is String && clean_actual !is String {
		tc.record_error_at(.assignment_mismatch, 'cannot cast `${tc.diagnostic_expr_type_name(child_id,
			actual)}` to `${target.name}` (alias to `${target.base_type.name()}`)', id, node.pos)
		return
	}
	if tc.check_cast_to_string(id, node, child_id, actual, target) {
		return
	}
	if tc.check_cast_from_string(id, node, child_id, actual, target) {
		return
	}
	if is_ierror_type(target)
		&& (actual is None || (actual is OptionType && actual.base_type is Void)) {
		return
	}
	if target_struct := struct_type_from_type(target) {
		clean_actual_pointer := fn_param_unalias_type(actual)
		if target is Alias && clean_actual_pointer is Pointer {
			actual_base := struct_type_from_type(clean_actual_pointer.base_type)
			if source_struct := actual_base {
				if source_struct.name == target_struct.name {
					tc.record_error_at(.assignment_mismatch,
						'cannot cast `${actual.name()}` to `${target.name}`, you must dereference it first (e.g. ${target.name}(*var))',
						id, node.pos)
					return
				}
			}
		}
		actual_struct := struct_type_from_type(actual)
		if source_struct := actual_struct {
			if target is Alias && source_struct.name == target_struct.name {
				return
			}
			tc.record_warning_at(.assignment_mismatch,
				'casting to struct is deprecated, use e.g. `Struct{...expr}` instead', id, node.pos)
			if source_struct.name != target_struct.name {
				tc.record_error(.assignment_mismatch,
					'cannot convert struct `${actual.name()}` to struct `${target.name()}`', id)
			}
			return
		}
		actual_name := tc.diagnostic_expr_type_name(child_id, actual)
		tc.record_error(.assignment_mismatch, 'cannot cast `${actual_name}` to struct', id)
		if unalias_type(actual) is SumType {
			tc.record_error(.assignment_mismatch,
				'cannot cast `${actual.name()}` sum type value to `${target.name()}`, use `${tc.source_text_for_node(child_id)} as ${target.name()}` instead',
				id)
		}
		return
	}
	target_iface := cast_target_interface(target) or { return }
	mut embedding_path := map[string]bool{}
	if tc.interface_embedding_exceeds(target_iface.name, 0, mut embedding_path) {
		decl_id, decl_pos := tc.interface_declaration_diagnostic(target_iface.name)
		tc.record_error_at(.unknown_type,
			'too many interface embedding levels: 101, for interface `${target_iface.name.all_after_last('.')}`',
			decl_id, decl_pos)
		return
	}
	if clean_actual is FnType {
		tc.record_error_at(.assignment_mismatch,
			'cannot implement interface `${target_iface.name}` using function', child_id,
			tc.a.node(child_id).pos)
		return
	}
	if clean_actual is Interface
		&& tc.interface_metadata_name(clean_actual.name) != tc.interface_metadata_name(target_iface.name) {
		tc.record_error_at(.assignment_mismatch,
			'cannot implement interface `${target_iface.name}` with a different interface `${clean_actual.name}`',
			id, node.pos)
		return
	}
	// A pointer to an interface may be explicitly reconstructed from a raw pointer.
	// The pointee already has interface storage, so this is a pointer cast rather
	// than an interface implementation conversion.
	if target is Pointer && fn_param_is_voidptr_type(actual) {
		return
	}
	if tc.interface_field_list(target_iface.name).any(it.is_mut) {
		child := tc.a.node(child_id)
		if child.kind == .ident && !tc.ident_is_mutable_lvalue(child.value) {
			tc.record_error_at(.assignment_mismatch,
				'`${child.value}` is immutable, declare it with `mut` to make it mutable',
				child_id, tc.node_value_diagnostic_pos(child_id))
			return
		}
	}
	if !tc.type_implements_interface(actual, target_iface) {
		actual_name := actual.name()
		tc.record_interface_implementation_error(.assignment_mismatch, actual, target_iface, id,
			node.pos)
		tc.record_error_at(.assignment_mismatch,
			'`${actual_name}` does not implement interface `${target_iface.name}`, cannot cast `${actual_name}` to interface `${target_iface.name}`',
			id, node.pos)
	}
}

fn (tc &TypeChecker) interface_embedding_exceeds(iface_name string, depth int, mut path map[string]bool) bool {
	if depth > 100 {
		return true
	}
	name := tc.interface_metadata_name(iface_name)
	if path[name] {
		return false
	}
	path[name] = true
	for embed in tc.interface_embeds[name] or { []string{} } {
		if tc.interface_embedding_exceeds(embed, depth + 1, mut path) {
			path.delete(name)
			return true
		}
	}
	path.delete(name)
	return false
}

fn (tc &TypeChecker) interface_declaration_diagnostic(iface_name string) (flat.NodeId, token.Pos) {
	short_name := iface_name.all_after_last('.')
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .interface_decl && node.value.all_after_last('.') == short_name {
			node_id := flat.NodeId(idx)
			return node_id, tc.source_line_declaration_pos(node_id)
		}
	}
	return flat.empty_node, token.Pos{}
}

// check_interface_embedding_limits rejects deep interface casts before requirement
// indexes recursively expand the embedding chain.
pub fn (mut tc TypeChecker) check_interface_embedding_limits() bool {
	mut embedded := map[string]bool{}
	for _, embeds in tc.interface_embeds {
		for embed in embeds {
			embedded[tc.interface_metadata_name(embed)] = true
		}
	}
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind != .interface_decl {
			continue
		}
		iface_name := tc.interface_metadata_name(node.value)
		if embedded[iface_name] {
			continue
		}
		mut embedding_path := map[string]bool{}
		if !tc.interface_embedding_exceeds(iface_name, 0, mut embedding_path) {
			continue
		}
		decl_id, decl_pos := tc.interface_declaration_diagnostic(iface_name)
		tc.errors << tc.make_type_error_at(.unknown_type,
			'too many interface embedding levels: 101, for interface `${iface_name.all_after_last('.')}`',
			decl_id, decl_pos)
		return true
	}
	return false
}

fn (tc &TypeChecker) interface_diagnostic_owner(signature_key string, fallback string) string {
	mut owner := signature_key.all_before_last('.')
	if owner.len == 0 {
		owner = fallback
	}
	if owner.contains('.') {
		return owner
	}
	mut mod_name := tc.fn_type_modules[signature_key] or { '' }
	if mod_name.len == 0 {
		mod_name = tc.struct_modules[fallback] or { tc.cur_module }
	}
	if mod_name.len == 0 {
		mod_name = 'main'
	}
	return '${mod_name}.${owner}'
}

fn interface_diagnostic_receiver_name(owner string, is_interface bool) string {
	if is_interface {
		return 'x'
	}
	short := owner.all_after_last('.')
	if short.len == 0 {
		return 'x'
	}
	return short[..1].to_lower()
}

fn (tc &TypeChecker) method_receiver_flags(signature_key string) (bool, bool) {
	shared_params := tc.fn_shared_params[signature_key] or {
		return tc.mut_receiver_methods[signature_key], false
	}
	is_shared := shared_params.len > 0 && shared_params[0]
	return tc.mut_receiver_methods[signature_key] || is_shared, is_shared
}

fn (tc &TypeChecker) interface_diagnostic_param_names(signature_key string, method string, is_interface bool) []string {
	owner := signature_key.all_before_last('.').all_after_last('.')
	if is_interface {
		for candidate in tc.a.nodes {
			if candidate.kind != .interface_decl || candidate.value.all_after_last('.') != owner {
				continue
			}
			for i in 0 .. candidate.children_count {
				field_id := tc.a.child(&candidate, i)
				field := tc.a.node(field_id)
				if field.kind != .interface_field || field.op != .dot || field.value != method {
					continue
				}
				pos := tc.source_line_declaration_pos(field_id)
				file := tc.a.source_files[pos.id] or { return ['x'] }
				source := tc.source_texts_by_file[file.name] or { return ['x'] }
				if pos.offset < 0 || pos.end > source.len || pos.offset >= pos.end {
					return ['x']
				}
				line := source[pos.offset..pos.end]
				open := line.index_u8(`(`)
				close := line.last_index_u8(`)`)
				if open < 0 || close <= open {
					return ['x']
				}
				params, _ := fn_diagnostic_type_parts('fn (${line[open + 1..close]})')
				mut names := ['x']
				for param in params {
					names << param.name
				}
				return names
			}
		}
		return ['x']
	}
	for candidate in tc.a.nodes {
		if candidate.kind != .fn_decl || candidate.value.all_after_last('.') != method
			|| candidate.value.all_before_last('.').all_after_last('.') != owner {
			continue
		}
		mut names := []string{}
		for i in 0 .. candidate.children_count {
			param := tc.a.child_node(&candidate, i)
			if param.kind == .param {
				names << param.value
			}
		}
		return names
	}
	return []string{}
}

fn (tc &TypeChecker) interface_diagnostic_method_signature(signature_key string, owner string, method string, is_interface bool) string {
	params := tc.fn_param_types[signature_key] or { []Type{} }
	param_names := tc.interface_diagnostic_param_names(signature_key, method, is_interface)
	alias_module := owner.all_before_last('.')
	mut rendered := []string{cap: params.len}
	if params.len > 0 {
		receiver_is_mut, receiver_is_shared := tc.method_receiver_flags(signature_key)
		receiver_name := if param_names.len > 0 && param_names[0].len > 0 {
			param_names[0]
		} else {
			interface_diagnostic_receiver_name(owner, is_interface)
		}
		receiver_param := if receiver_is_shared {
			'mut ${receiver_name} shared ${owner}'
		} else if receiver_is_mut {
			'mut ${receiver_name} ${owner}'
		} else {
			'${receiver_name} ${owner}'
		}
		rendered << receiver_param
	}
	for i in 1 .. params.len {
		param_name := if i < param_names.len && param_names[i].len > 0 {
			param_names[i]
		} else if i == 1 {
			's'
		} else {
			'p${i}'
		}
		rendered << '${param_name} ${tc.interface_diagnostic_type_name(params[i], alias_module)}'
	}
	ret := tc.fn_ret_types[signature_key] or { Type(void_) }
	ret_suffix := if ret is Void {
		''
	} else {
		' ${tc.interface_diagnostic_type_name(ret, alias_module)}'
	}
	return 'fn ${method}(${rendered.join(', ')})${ret_suffix}'
}

fn (tc &TypeChecker) interface_diagnostic_type_name(typ Type, alias_module string) string {
	if !tc.checker_fixture_mode {
		return typ.name()
	}
	if typ is Alias {
		if fn_type_from_type(typ) != none {
			return tc.interface_diagnostic_type_name(typ.base_type, alias_module)
		}
		if alias_module.len > 0 && !typ.name.contains('.') {
			return '${alias_module}.${typ.name}'
		}
		return typ.name
	}
	if typ is FnType {
		mut params := []string{cap: typ.params.len}
		for param in typ.params {
			params << tc.interface_diagnostic_type_name(param, alias_module)
		}
		ret := if typ.return_type is Void {
			''
		} else {
			' ${tc.interface_diagnostic_type_name(typ.return_type, alias_module)}'
		}
		return 'fn (${params.join(', ')})${ret}'
	}
	if typ is OptionType {
		return '?${tc.interface_diagnostic_type_name(typ.base_type, alias_module)}'
	}
	if typ is ResultType {
		return '!${tc.interface_diagnostic_type_name(typ.base_type, alias_module)}'
	}
	return typ.name()
}

fn (tc &TypeChecker) interface_actual_field(concrete_name string, field_name string) ?StructField {
	mut candidates := [concrete_name]
	if !concrete_name.contains('.') {
		qname := tc.qualify_name(concrete_name)
		if qname != concrete_name {
			candidates << qname
		}
	}
	for candidate in candidates {
		for field in tc.structs[candidate] or { []StructField{} } {
			if field.name == field_name {
				return field
			}
		}
	}
	return none
}

fn (mut tc TypeChecker) record_interface_implementation_error(kind TypeErrorKind, actual Type, expected Interface, id flat.NodeId, pos token.Pos) bool {
	actual_name := method_type_name(unwrap_pointer(actual))
	actual_display := if unalias_type(actual) is Pointer {
		'&${actual_name.all_after_last('.')}'
	} else {
		actual_name.all_after_last('.')
	}
	expected_name := tc.interface_metadata_name(expected.name)
	expected_display := expected_name.all_after_last('.')
	mut missing_methods := false
	for method in tc.interface_abstract_method_names(expected_name) {
		expected_key := tc.interface_method_signature_key(expected_name, method) or {
			'${expected_name}.${method}'
		}
		if actual_name == 'char' && method == 'str'
			&& tc.interface_method_is_str_requirement(expected_key) {
			tc.record_error_at(kind,
				'`${actual_display}` doesn\'t implement method `${method}` of interface `${expected_display}`',
				id, pos)
			missing_methods = true
			continue
		}
		actual_key := tc.concrete_method_signature_key(actual_name, method) or {
			tc.record_error_at(kind,
				'`${actual_display}` doesn\'t implement method `${method}` of interface `${expected_display}`',
				id, pos)
			missing_methods = true
			continue
		}
		expected_params := tc.fn_param_types[expected_key] or { []Type{} }
		actual_params := tc.fn_param_types[actual_key] or { []Type{} }
		mut message := ''
		expected_receiver_mut, expected_receiver_shared := tc.method_receiver_flags(expected_key)
		actual_receiver_mut, actual_receiver_shared := tc.method_receiver_flags(actual_key)
		if expected_receiver_mut && actual_receiver_shared && !expected_receiver_shared {
			message = '`${actual_display}` incorrectly implements method `${method}` of interface `${expected_display}`: expected `mut ${expected_display}`, not `mut shared ${actual_display}` for parameter 0'
		} else if !expected_receiver_mut && actual_receiver_mut && !actual_receiver_shared {
			expected_receiver := if expected_receiver_mut {
				'mut ${expected_display}'
			} else {
				expected_display
			}
			actual_receiver := if actual_receiver_mut {
				'mut ${actual_display}'
			} else {
				actual_display
			}
			message = '`${actual_display}` incorrectly implements method `${method}` of interface `${expected_display}`: expected `${expected_receiver}`, not `${actual_receiver}` for parameter 0'
		} else if expected_params.len != actual_params.len {
			message = '`${actual_display}` incorrectly implements method `${method}` of interface `${expected_display}`: expected ${expected_params.len} parameter(s), not ${actual_params.len}'
		} else {
			for i in 1 .. expected_params.len {
				if !tc.method_param_signature_compatible(actual_params[i], expected_params[i]) {
					message = '`${actual_display}` incorrectly implements method `${method}` of interface `${expected_display}`: expected `${expected_params[i].name()}`, not `${actual_params[i].name()}` for parameter ${i}'
					break
				}
			}
			if message.len == 0 {
				expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
				actual_ret := tc.fn_ret_types[actual_key] or { Type(void_) }
				if actual_ret.name() != expected_ret.name() {
					message = '`${actual_display}` incorrectly implements method `${method}` of interface `${expected_display}`: expected return type `${tc.interface_diagnostic_type_name(expected_ret, '')}`'
				}
			}
		}
		if message.len > 0 {
			expected_owner := tc.interface_diagnostic_owner('${expected_name}.${method}',
				expected_name)
			actual_owner := tc.interface_diagnostic_owner(actual_key, actual_name)
			tc.record_error_with_details_at(kind, message, id, pos, [
				'${expected_owner} has `${tc.interface_diagnostic_method_signature(expected_key,
					expected_owner, method, true)}`',
				'         ${actual_owner} has `${tc.interface_diagnostic_method_signature(actual_key,
					actual_owner, method, false)}`',
			])
			return true
		}
	}
	if missing_methods {
		return true
	}
	for field in tc.interface_field_list(expected_name) {
		actual_field := tc.interface_actual_field(actual_name, field.name) or {
			tc.record_error_at(kind,
				'`${actual_display}` doesn\'t implement field `${field.name}` of interface `${expected_display}`',
				id, pos)
			return true
		}
		if !tc.type_compatible(actual_field.typ, field.typ)
			|| !tc.type_compatible(field.typ, actual_field.typ) {
			tc.record_error_at(kind,
				'`${actual_display}` incorrectly implements field `${field.name}` of interface `${expected_display}`, expected `${field.typ.name()}`, got `${actual_field.typ.name()}`',
				id, pos)
			return true
		}
		if field.is_mut && !actual_field.is_mut {
			tc.record_error_at(kind,
				'`${actual_display}` incorrectly implements interface `${expected_display}`, field `${field.name}` must be mutable',
				id, pos)
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) named_type_implements_interface_method(concrete_name string, iface_name string, method string) bool {
	expected_key := tc.interface_method_signature_key(iface_name, method) or {
		'${iface_name}.${method}'
	}
	if concrete_name == 'char' && method == 'str'
		&& tc.interface_method_is_str_requirement(expected_key) {
		return false
	}
	if concrete_key := tc.concrete_method_signature_key(concrete_name, method) {
		return tc.method_signature_compatible(concrete_key, expected_key)
	}
	if info := tc.resolve_generic_struct_method(concrete_name, method) {
		return tc.method_call_info_signature_compatible(info, expected_key)
	}
	if info := tc.resolve_generic_sum_method(concrete_name, method) {
		return tc.method_call_info_signature_compatible(info, expected_key)
	}
	return method == 'str' && tc.interface_method_is_str_requirement(expected_key)
		&& tc.type_has_implicit_str_method(concrete_name)
}

fn (mut tc TypeChecker) check_function_cast(id flat.NodeId, node flat.Node, child_id flat.NodeId, actual Type, target Type) bool {
	mut is_option := false
	target_fn := fn_type_from_type(target) or {
		if target is OptionType {
			is_option = true
			fn_type_from_type(target.base_type) or { return false }
		} else {
			return false
		}
	}
	if tc.expr_tail_is_nil(child_id) {
		if tc.unsafe_depth > 0 {
			return false
		}
		if !is_option {
			tc.record_warning_at(.assignment_mismatch,
				'casting `nil` to function value should be done inside `unsafe{}` blocks', id,
				node.pos)
		}
		tc.record_error_at(.assignment_mismatch, '`nil` is only allowed in `unsafe` code',
			child_id, tc.a.node(child_id).pos)
		if is_option {
			tc.record_error_at(.assignment_mismatch,
				'casting number to Option function is not allowed, only compatible function or `none`',
				id, tc.cast_expression_diagnostic_pos(node, node.value.trim_left('?')))
		}
		return true
	}
	if infix_power_type_is_numeric(actual) {
		if tc.unsafe_depth > 0 {
			return false
		}
		if is_option {
			tc.record_error_at(.assignment_mismatch,
				'casting number to Option function is not allowed, only compatible function or `none`',
				id, tc.cast_expression_diagnostic_pos(node, node.value.trim_left('?')))
		} else {
			tc.record_warning_at(.assignment_mismatch,
				'casting number to function value should be done inside `unsafe{}` blocks', id,
				node.pos)
		}
		return true
	}
	if unalias_type(actual) is String {
		tc.record_error_at(.assignment_mismatch, 'invalid casting value to function', id, node.pos)
		return true
	}
	if actual_fn := fn_type_from_type(actual) {
		if tc.unsafe_depth == 0 && (target_fn.params != actual_fn.params
			|| target_fn.return_type.name() != actual_fn.return_type.name()) {
			tc.record_error_at(.assignment_mismatch,
				'casting a function value from one function signature, to another function signature, should be done inside `unsafe{}` blocks',
				id, if is_option {
				tc.cast_expression_diagnostic_pos(node, node.value.trim_left('?'))
			} else {
				node.pos
			})
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) warn_invalid_enum_cast(id flat.NodeId, node flat.Node, child_id flat.NodeId, target Enum) {
	value := tc.const_int_expr(child_id, tc.cur_module, []string{}) or { return }
	cases := tc.comptime_static_enum_decl_value_cases(target.name)
	if cases.len == 0 {
		return
	}
	mut valid := false
	if target.is_flag {
		mut mask := 0
		for enum_case in cases {
			if enum_case.has_value {
				mask |= enum_case.value
			}
		}
		valid = value >= 0 && value & ~mask == 0
	} else {
		valid = cases.any(it.has_value && it.value == value)
	}
	if !valid {
		tc.record_warning_at(.assignment_mismatch,
			'${value} does not represent a value of enum ${target.name}', id, node.pos)
	}
}

fn (tc &TypeChecker) cast_operand_is_zero(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .int_literal {
		return numeric_literal_is_zero(node.value)
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.cast_operand_is_zero(tc.a.child(node, 0))
	}
	if node.kind != .ident || node.value.len == 0 {
		return false
	}
	if value := tc.const_int_value(node.value, []string{}) {
		return value == 0
	}
	file := tc.a.source_files[node.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	end := int_min(int_max(node.pos.offset, 0), source.len)
	for needle in ['${node.value} := 0', '${node.value} = 0'] {
		if source[..end].last_index(needle) != none {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) cast_expression_diagnostic_pos(node flat.Node, target_name string) token.Pos {
	if !node.pos.is_valid() || target_name.len == 0 {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	offset := int_min(int_max(node.pos.offset, 0), source.len)
	line_start := if relative := source[..offset].last_index('\n') {
		relative + 1
	} else {
		0
	}
	line_end := source.index_after('\n', offset) or { source.len }
	line := source[line_start..line_end]
	relative := line.index('${target_name}(') or { return node.pos }
	start := line_start + relative
	mut depth := 0
	for i in start + target_name.len .. line_end {
		if source[i] == `(` {
			depth++
		} else if source[i] == `)` {
			depth--
			if depth == 0 {
				return token.new_span(node.pos.id, start, i + 1)
			}
		}
	}
	return node.pos
}

fn (tc &TypeChecker) source_enclosing_fn_has_generic_param(id flat.NodeId, name string) bool {
	if name.len == 0 || !tc.valid_node_id(id) {
		return false
	}
	node_idx := int(id)
	if name.len == 1 && name[0] >= `A` && name[0] <= `Z`
		&& node_idx < tc.enclosing_generic_param_masks.len {
		mask := u32(1) << u32(name[0] - `A`)
		return tc.enclosing_generic_param_masks[node_idx] & mask != 0
	}
	if tc.fn_context.node_id >= 0 && int(id) >= tc.check_range_lo && int(id) <= tc.check_range_hi {
		params := tc.enclosing_generic_params_by_node[tc.fn_context.node_id] or { return false }
		return name in params
	}
	if tc.direct_parent_index_trusted {
		mut current_idx := int(id)
		for current_idx >= 0 && current_idx < tc.a.nodes.len {
			node := tc.a.nodes[current_idx]
			if node.kind == .fn_decl {
				params := tc.enclosing_generic_params_by_node[current_idx] or { return false }
				return name in params
			}
			if current_idx >= tc.direct_parent_ids.len {
				return false
			}
			parent_idx := int(tc.direct_parent_ids[current_idx])
			if parent_idx == current_idx {
				return false
			}
			current_idx = parent_idx
		}
		return false
	}
	node := tc.a.nodes[int(id)]
	file := tc.a.source_files[node.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	offset := int_min(int_max(node.pos.offset, 0), source.len)
	// Scan by index: substr copies of the file prefix/suffix here made every
	// call allocate whole-file-sized temporaries, which multiplied into
	// gigabytes on self-host builds.
	mut fn_start := -1
	for i := offset - 3; i >= 0; i-- {
		if source[i] == `f` && source[i + 1] == `n` && source[i + 2] == ` ` {
			fn_start = i
			break
		}
	}
	if fn_start < 0 {
		return false
	}
	mut header_end := -1
	for i := fn_start; i < source.len; i++ {
		if source[i] == `{` {
			header_end = i
			break
		}
	}
	if header_end < 0 {
		return false
	}
	header := source[fn_start..header_end]
	mut search_start := 0
	for search_start < header.len {
		open_relative := header[search_start..].index_u8(`[`)
		if open_relative < 0 {
			break
		}
		open := search_start + open_relative
		close_relative := header[open + 1..].index_u8(`]`)
		if close_relative < 0 {
			break
		}
		close := open + 1 + close_relative
		for param in header[open + 1..close].split(',') {
			if param.trim_space() == name {
				return true
			}
		}
		search_start = close + 1
	}
	return false
}

fn (mut tc TypeChecker) check_cast_to_string(id flat.NodeId, node flat.Node, child_id flat.NodeId, actual Type, target Type) bool {
	if target !is String || actual is String {
		return false
	}
	expr := tc.source_text_for_node(child_id)
	clean_actual := unalias_type(actual)
	if clean_actual is Primitive {
		if clean_actual.props.has(.boolean) || clean_actual.props.has(.integer)
			|| clean_actual.props.has(.float) {
			actual_name := actual.name()
			message := if clean_actual.props.has(.boolean)
				|| (clean_actual.props.has(.integer) && clean_actual.props.has(.unsigned)
				&& clean_actual.size == 8) {
				'cannot cast type `${actual_name}` to string, use `${expr}.str()` instead.'
			} else {
				'cannot cast number to string, use `${expr}.str()` instead.'
			}
			tc.record_error_at(.assignment_mismatch, message, id, node.pos)
			return true
		}
	}
	if clean_actual is Enum {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast enum to string, use ${expr}.str() instead.', id, node.pos)
		return true
	}
	if clean_actual is SumType {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast sumtype `${actual.name()}` to string, use `${expr}.str()` instead.', id,
			node.pos)
		tc.record_error_at(.assignment_mismatch,
			'cannot cast `${actual.name()}` sum type value to `string`, use `${expr} as string` instead',
			id, node.pos)
		return true
	}
	if clean_actual is Struct {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast struct `${actual.name()}` to `string`', id, node.pos)
		return true
	}
	if clean_actual is Map {
		tc.record_error_at(.assignment_mismatch, 'cannot cast map to string.', id, node.pos)
		return true
	}
	if clean_actual is Array && is_byte_type(clean_actual.elem_type) {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast []u8 to string, use `${expr}.bytestr()` or `${expr}.str()` instead.', id,
			node.pos)
		return true
	}
	if clean_actual is Pointer {
		if is_byte_type(unalias_type(clean_actual.base_type)) {
			tc.record_error_at(.assignment_mismatch,
				'to convert a C string buffer pointer to a V string, use x.vstring() instead of string(x)',
				id, node.pos)
		}
		tc.record_error_at(.assignment_mismatch,
			'cannot cast pointer type `${actual.name()}` to string, use `&u8(${expr}).vstring()` or `cstring_to_vstring(${expr})` instead.',
			id, node.pos)
		return true
	}
	if clean_actual is FnType {
		tc.record_error_at(.assignment_mismatch, 'cannot cast function `${expr}` to string', id,
			node.pos)
		return true
	}
	return false
}

fn (mut tc TypeChecker) check_cast_from_string(id flat.NodeId, node flat.Node, child_id flat.NodeId, actual Type, target Type) bool {
	if unalias_type(actual) !is String || target is String {
		return false
	}
	expr := tc.source_text_for_node(child_id)
	clean_target := unalias_type(target)
	target_name := if node.value.len > 0 { node.value } else { target.name() }
	if clean_target is Primitive
		&& (clean_target.props.has(.integer) || clean_target.props.has(.float)) {
		if clean_target.props.has(.integer) && clean_target.props.has(.unsigned)
			&& clean_target.size == 8 {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast type `${actual.name()}` to `${target_name}`', id, node.pos)
			return true
		}
		tc.record_error_at(.assignment_mismatch,
			'cannot cast string to `${target_name}`, use `${expr}.${target_name}()` instead.', id,
			node.pos)
		return true
	}
	if clean_target is Char {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast string to `char`, use `${expr}[index]` instead.', id, node.pos)
		return true
	}
	if clean_target is Rune {
		tc.record_error_at(.assignment_mismatch,
			'cannot cast `string` to rune, use `${expr}.runes()` instead.', id, node.pos)
		return true
	}
	if clean_target is Enum {
		enum_name := if clean_target.name.starts_with('.') {
			'main${clean_target.name}'
		} else if clean_target.name.contains('.') {
			clean_target.name
		} else {
			'${if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }}.${clean_target.name}'
		}
		tc.record_error_with_details_at(.assignment_mismatch, 'cannot cast `string` to `enum`', id,
			node.pos, [
			'use ${enum_name}.from_string(${expr}) instead',
		])
		return true
	}
	if clean_target is Pointer {
		if target_name in ['voidptr', 'byteptr', 'charptr'] {
			outside_unsafe := if target_name == 'voidptr' || tc.unsafe_depth > 0 {
				''
			} else {
				' outside `unsafe`'
			}
			tc.record_error_at(.assignment_mismatch,
				'cannot cast string to `${target_name}`${outside_unsafe}, use ${target_name}(s.str) instead',
				id, node.pos)
		} else if struct_type_from_type(clean_target.base_type) != none {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast `${actual.name()}` to `${target_name}`', id, node.pos)
		} else {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast string to `${target_name}`, use `${expr}.str` instead.', id, node.pos)
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) check_integer_literal_cast_overflow(id flat.NodeId, node flat.Node, child_id flat.NodeId, target Type) {
	clean_target := unalias_type(target)
	if clean_target !is Primitive || !clean_target.props.has(.integer) {
		return
	}
	mut literal := tc.integer_literal_source(child_id) or { return }
	literal = literal.replace('_', '')
	if literal.len == 0 {
		return
	}
	is_negative := literal[0] == `-`
	mut magnitude := literal
	if literal[0] == `-` || literal[0] == `+` {
		magnitude = literal[1..]
	}
	if magnitude.len == 0 {
		return
	}
	bit_size := if clean_target.size == 0 { 32 } else { int(clean_target.size) }
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, bit_size)
	target_name := target.name()
	if parse_error == -3 {
		tc.record_error_at(.assignment_mismatch, 'value `${literal}` overflows `${target_name}`',
			id, node.pos)
		return
	}
	if parse_error != 0 || clean_target.props.has(.unsigned) {
		return
	}
	unsigned_max := if bit_size >= 64 {
		max_u64
	} else {
		(u64(1) << bit_size) - 1
	}
	signed_max := (u64(1) << (bit_size - 1)) - 1
	overflows_sign_bit := value == unsigned_max
		|| (is_negative && value == signed_max + 2)
		|| (!is_negative && value == signed_max + 1)
	if overflows_sign_bit {
		tc.record_warning_at(.assignment_mismatch,
			'value `${literal}` overflows `${target_name}`, this will be considered hard error soon',
			id, node.pos)
	}
}

fn (tc &TypeChecker) integer_literal_source(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .int_literal {
		text := tc.source_text_for_node(id)
		return if text.len > 0 { text } else { node.value }
	}
	if node.kind == .prefix && node.op in [.plus, .minus] && node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		if tc.a.nodes[int(child_id)].kind == .int_literal {
			return tc.source_text_for_node(id)
		}
	}
	return none
}

fn (mut tc TypeChecker) check_untyped_integer_literal_overflow(id flat.NodeId) {
	mut parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .prefix && parent.op in [.plus, .minus] {
			parent_id = tc.direct_parent_id(parent_id)
		}
	}
	if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .cast_expr {
		return
	}
	if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .enum_field {
		enum_id := tc.direct_parent_id(parent_id)
		if tc.valid_node_id(enum_id) && tc.a.node(enum_id).kind == .enum_decl
			&& tc.a.node(enum_id).generic_params().len > 0 {
			return
		}
	}
	mut literal := tc.integer_literal_source(id) or { return }
	literal = literal.replace('_', '')
	if literal.len == 0 {
		return
	}
	is_negative := literal[0] == `-`
	mut magnitude := literal
	if literal[0] in [`-`, `+`] {
		magnitude = literal[1..]
	}
	if magnitude.len == 0 {
		return
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, 64)
	overflows := parse_error == -3 || (parse_error == 0 && is_negative && value > (u64(1) << 63))
	if overflows {
		tc.record_error_at(.assignment_mismatch, 'integer literal ${literal} overflows int', id,
			tc.a.node(id).pos)
	}
}

fn (tc &TypeChecker) implicit_int_literal_overflows(id flat.NodeId) bool {
	mut literal := tc.integer_literal_source(id) or { return false }
	literal = literal.replace('_', '')
	if literal.len == 0 {
		return false
	}
	is_negative := literal[0] == `-`
	mut magnitude := literal
	if literal[0] == `-` || literal[0] == `+` {
		magnitude = literal[1..]
	}
	if magnitude.len == 0 {
		return false
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, 32)
	if parse_error == -3 {
		return true
	}
	if parse_error != 0 {
		return false
	}
	if is_negative {
		return value > u64(2_147_483_648)
	}
	return value > u64(2_147_483_647)
}

struct IntegerTypeRange {
	bits        int
	is_unsigned bool
	name        string
}

fn integer_type_range(typ Type) ?IntegerTypeRange {
	clean := unalias_type(typ)
	if clean is Primitive {
		if !clean.props.has(.integer) {
			return none
		}
		return IntegerTypeRange{
			bits:        if clean.size == 0 { 32 } else { int(clean.size) }
			is_unsigned: clean.props.has(.unsigned)
			name:        typ.name()
		}
	}
	if clean is ISize {
		return IntegerTypeRange{
			bits: 64
			name: typ.name()
		}
	}
	if clean is USize {
		return IntegerTypeRange{
			bits:        64
			is_unsigned: true
			name:        typ.name()
		}
	}
	if clean is Rune {
		return IntegerTypeRange{
			bits: 32
			name: typ.name()
		}
	}
	return none
}

fn (mut tc TypeChecker) warn_if_integer_literal_outside_known_type_range(id flat.NodeId, expected Type, warning_pos token.Pos) {
	if expected is Alias {
		return
	}
	type_range := integer_type_range(expected) or { return }
	mut literal := tc.integer_literal_source(id) or { return }
	literal = literal.replace('_', '')
	if !integer_literal_outside_range(literal, type_range) {
		return
	}
	tc.record_warning_at(.call_arg_mismatch,
		'value `${literal}` is outside the range of `${type_range.name}` in argument, this will be considered hard error soon',
		id, warning_pos)
	if integer_literal_overflows_signed_64(literal) {
		tc.record_error_at(.assignment_mismatch, 'integer literal ${literal} overflows int', id,
			tc.a.nodes[int(id)].pos)
	}
}

fn integer_literal_outside_range(literal string, type_range IntegerTypeRange) bool {
	if literal.len == 0 || type_range.bits <= 0 {
		return false
	}
	is_negative := literal[0] == `-`
	mut magnitude := literal
	if literal[0] == `-` || literal[0] == `+` {
		magnitude = literal[1..]
	}
	if magnitude.len == 0 {
		return false
	}
	value, parse_error := strconv.common_parse_uint2(magnitude, 0, 64)
	if parse_error == -3 {
		return true
	}
	if parse_error != 0 {
		return false
	}
	if type_range.is_unsigned {
		return is_negative || value > enum_backing_unsigned_max(type_range.bits)
	}
	minimum_magnitude := if type_range.bits >= 64 {
		u64(1) << 63
	} else {
		u64(1) << (type_range.bits - 1)
	}
	maximum := minimum_magnitude - 1
	if is_negative {
		return value > minimum_magnitude
	}
	return value > maximum
}

fn integer_literal_overflows_signed_64(literal string) bool {
	if literal.len < 2 || literal[0] != `-` {
		return false
	}
	value, parse_error := strconv.common_parse_uint2(literal[1..], 0, 64)
	return parse_error == -3 || (parse_error == 0 && value > (u64(1) << 63))
}

fn (tc &TypeChecker) call_has_postfix_propagation(call flat.Node) bool {
	file := tc.a.source_files[call.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	mut after_call := int_min(int_max(call.pos.end, 0), source.len)
	for after_call < source.len && source[after_call].is_space() {
		after_call++
	}
	if after_call + 3 <= source.len && source[after_call..after_call + 3] == '!in' {
		return false
	}
	return after_call < source.len && source[after_call] in [`!`, `?`]
}

fn (mut tc TypeChecker) record_unhandled_result_call(call_id flat.NodeId, result_type ResultType) {
	if !tc.valid_node_id(call_id) {
		return
	}
	call := tc.a.nodes[int(call_id)]
	if call.kind != .call {
		return
	}
	parent_id := tc.direct_parent_id(call_id)
	if tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .or_expr && parent.value in ['!', '?'] && parent.children_count > 0
			&& tc.a.child(parent, 0) == call_id {
			return
		}
	}
	if tc.call_has_postfix_propagation(call) {
		return
	}
	name := tc.call_display_name(call)
	tc.record_error_at(.call_arg_mismatch,
		'${name}() returns `${Type(result_type).name()}`, so it should have either an `or {}` block, or `!` at the end',
		call_id, tc.wrapped_operand_diagnostic_pos(call_id))
}

fn type_text_contains_any(typ string) bool {
	clean := trimmed_space(typ)
	if clean.len < 3 {
		return clean == 'any'
	}
	for i in 0 .. clean.len - 2 {
		if clean[i..i + 3] != 'any' {
			continue
		}
		left_is_name := i > 0 && (clean[i - 1].is_alnum() || clean[i - 1] == `_`)
		right_is_name := i + 3 < clean.len && (clean[i + 3].is_alnum() || clean[i + 3] == `_`)
		if !left_is_name && !right_is_name {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_as_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	child_id := tc.a.child(&node, 0)
	tc.check_node(child_id)
	child_type := tc.resolve_type(child_id)
	if child_type is ResultType && tc.a.node(child_id).kind == .call {
		tc.record_unhandled_result_call(child_id, child_type)
		return
	}
	if child_type is OptionType && tc.a.node(child_id).kind == .ident {
		child := tc.a.node(child_id)
		tc.record_error_at(.assignment_mismatch,
			'variable `${child.value}` is an Option, it must be unwrapped first', child_id,
			tc.node_value_diagnostic_pos(child_id))
		return
	}
	if node.value.contains('.') {
		target := node.value.all_before_last('.')
		field := node.value.all_after_last('.')
		if target.len > 0 && field.len > 0 {
			resolved_target := tc.unique_qualified_type_name(target) or { target }
			qualified_target := if resolved_target.contains('.') {
				resolved_target
			} else {
				'${if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }}.${resolved_target}'
			}
			tc.record_error_with_details_at(.condition_mismatch,
				'indeterminate `as` cast, use parenthesis to clarity', id, tc.as_operator_pos(id,
				node, child_id), [
				'for example `(${tc.source_text_for_node(child_id)} as ${qualified_target}).${field}`',
			])
			return
		}
	}
	if node.value.len > 0 && !interface_pattern_is_collapsed_container(node.value)
		&& should_check_named_type(node.value) && !tc.type_name_known(node.value) {
		pos := tc.as_operator_pos(id, node, child_id)
		tc.record_error_at(.unknown_type, tc.unknown_type_message(node.value, id), id, pos)
		tc.record_error_at(.assignment_mismatch,
			'cannot cast `${tc.diagnostic_type_name(child_type)}` to `${node.value}`', id, pos)
		return
	}
	as_child_type := if child_type is OptionType { child_type.base_type } else { child_type }
	clean_child := unalias_type(unwrap_pointer(as_child_type))
	if clean_child !is SumType && clean_child !is Interface {
		target := unalias_type(tc.parse_type(node.value))
		suffix := if target is SumType {
			' - use e.g. `${node.value}(some_expr)` instead.'
		} else {
			''
		}
		tc.record_error_at(.assignment_mismatch,
			'cannot cast non-sum type `${clean_child.name()}` using `as`${suffix}', id, tc.as_operator_pos(id,
			node, child_id))
		return
	}
	if clean_child is SumType {
		target := tc.parse_type(node.value)
		target_is_generic_param := node.value in tc.fn_context.generic_params
			|| tc.active_generic_param(node.value)
			|| tc.node_has_enclosing_generic_param(id, node.value)
			|| tc.source_enclosing_fn_has_generic_param(id, node.value)
		if target is Unknown && target_is_generic_param {
			return
		}
		if !tc.sum_type_contains_variant(clean_child, target) {
			tc.record_error_at(.assignment_mismatch,
				'cannot cast `${clean_child.name}` to `${target.name()}`', id, tc.as_operator_pos(id,
				node, child_id))
			return
		}
	}
	if node.value.len == 0 || !interface_pattern_is_collapsed_container(node.value) {
		return
	}
	expr_type := unalias_type(unwrap_pointer(tc.resolve_type(child_id)))
	if expr_type is Interface && !tc.interface_has_no_requirements(expr_type.name)
		&& tc.should_diagnose(id) {
		tc.record_error(.condition_mismatch,
			'`${node.value}` is not compatible with interface `${expr_type.name}`', id)
	}
}

fn (tc &TypeChecker) as_operator_pos(id flat.NodeId, node flat.Node, child_id flat.NodeId) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	child := tc.a.node(child_id)
	start := int_max(0, child.pos.end)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if relative := source[start..end].index(' as ') {
			op_start := start + relative + 1
			return token.new_span(node.pos.id, op_start, op_start + 2)
		}
	}
	return tc.node_value_diagnostic_pos(id)
}

fn (tc &TypeChecker) sizeof_type_diagnostic_pos(id flat.NodeId, name string) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	anchor := int_max(0, int_min(node.pos.offset, source.len))
	if span := closest_text_span(source, name, anchor, node.pos.id) {
		end := if span.end < source.len && source[span.end] == `]` {
			span.end + 1
		} else {
			span.end
		}
		return token.new_span(span.id, span.offset, end)
	}
	return node.pos
}

fn (tc &TypeChecker) generic_type_arg_diagnostic_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	if !node.pos.is_valid() {
		return node.pos
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	return token.new_span(node.pos.id, int_max(0, node.pos.offset - 1), int_min(node.pos.end + 1,
		source.len))
}

fn (mut tc TypeChecker) check_in_expr(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	value_id := tc.a.child(&node, 0)
	container_id := tc.a.child(&node, 1)
	tc.check_node(value_id)
	value_type_raw := tc.resolve_type(value_id)
	value_type := unalias_type(value_type_raw)
	container := tc.a.node(container_id)
	if value_type is SumType && container.kind == .array_literal {
		for i in 0 .. container.children_count {
			variant_id := tc.a.child(container, i)
			variant := tc.a.node(variant_id)
			if variant.kind == .ident && should_check_named_type(variant.value)
				&& !tc.type_name_known(variant.value) {
				tc.record_error_at(.unknown_type,
					tc.unknown_type_message(variant.value, variant_id), variant_id,
					tc.node_value_diagnostic_pos(variant_id))
				return
			}
		}
	}
	if container.kind == .array_literal {
		expected_container := Type(Array{
			elem_type: value_type
		})
		_ = tc.resolve_expr(container_id, expected_container)
		tc.check_node_with_expected_context(container_id, expected_container)
	} else {
		tc.check_node(container_id)
	}
	container_type_raw := tc.resolve_type(container_id)
	container_type := unalias_type(tc.mut_param_expr_base(container_id, container_type_raw) or {
		container_type_raw
	})
	op := tc.in_operator_name(value_id, container_id)
	if value_type is MultiReturn || container_type is MultiReturn {
		tc.record_error_at(.condition_mismatch,
			'invalid number of operand for `${op}`. Only one allowed on each side.', id, node.pos)
	}
	if tc.a.node(value_id).kind == .none_expr && container.kind == .none_expr {
		tc.record_error_at(.condition_mismatch, 'invalid operator `${op}` to `none` and `none`',
			id, node.pos)
		tc.record_error_at(.condition_mismatch, '`${op}` can only be used with arrays and maps',
			id, tc.in_operator_pos(value_id, container_id))
		return
	}
	if value_type is OptionType || value_type is ResultType {
		kind := if value_type is OptionType { 'Option' } else { 'Result' }
		tc.record_error_at(.condition_mismatch, 'unwrapped ${kind} cannot be used with `${op}`',
			value_id, tc.wrapped_operand_diagnostic_pos(value_id))
		if value_type is ResultType {
			tc.record_unhandled_result_call(value_id, value_type)
		}
		return
	}
	if container_type is Array || container_type is ArrayFixed {
		if container.kind == .array_literal {
			tc.check_in_array_duplicate_items(container)
		}
		element_type := if container_type is Array {
			container_type.elem_type
		} else {
			(container_type as ArrayFixed).elem_type
		}
		if !tc.expr_compatible(value_id, value_type, element_type) {
			tc.record_error_at(.condition_mismatch, 'left operand to `${op}` does not match the array element type: expected `${element_type.name()}`, not `${tc.diagnostic_expr_type_name(value_id,
				value_type_raw)}`', id, node.pos)
		}
		return
	}
	if container_type is Map {
		if !tc.expr_compatible(value_id, value_type, container_type.key_type) {
			tc.record_error_at(.condition_mismatch, 'left operand to `${op}` does not match the map key type: expected `${container_type.key_type.name()}`, not `${tc.diagnostic_expr_type_name(value_id,
				value_type_raw)}`', id, node.pos)
		}
		return
	}
	if container.kind != .range {
		if container_type !is Unknown {
			tc.record_error_at(.condition_mismatch,
				'`${op}` can only be used with arrays and maps', id, tc.in_operator_pos(value_id,
				container_id))
		}
		return
	}
	if value_type is Unknown || value_type.is_integer() {
		return
	}
	tc.record_error_at(.condition_mismatch,
		'`${tc.diagnostic_expr_type_name(value_id, value_type)}` is an invalid type for range expression',
		id, tc.in_operator_pos(value_id, container_id))
}

fn (mut tc TypeChecker) check_in_array_duplicate_items(array flat.Node) {
	mut seen := map[string]bool{}
	mut reported := map[string]bool{}
	for i in 0 .. array.children_count {
		item_id := tc.a.child(&array, i)
		item := tc.a.node(item_id)
		if item.kind !in [.ident, .int_literal, .float_literal, .string_literal, .char_literal,
			.bool_literal] {
			continue
		}
		key := '${int(item.kind)}:${item.value}'
		if seen[key] && !reported[key] {
			display := tc.source_text_for_node(item_id).trim_space()
			tc.record_notice_at(.duplicate_decl, 'item `${display}` is duplicated in the list',
				item_id, item.pos)
			reported[key] = true
		}
		seen[key] = true
	}
}

fn (tc &TypeChecker) in_operator_name(value_id flat.NodeId, container_id flat.NodeId) string {
	value := tc.a.node(value_id)
	container := tc.a.node(container_id)
	if value.pos.id != container.pos.id || !value.pos.is_valid() || !container.pos.is_valid() {
		return 'in'
	}
	file := tc.a.source_files[value.pos.id] or { return 'in' }
	source := tc.source_texts_by_file[file.name] or { return 'in' }
	start := int_min(int_max(0, value.pos.end), source.len)
	end := int_min(int_max(start, container.pos.offset), source.len)
	if start < end && source[start..end].contains('!in') {
		return '!in'
	}
	return 'in'
}

fn (tc &TypeChecker) in_operator_pos(value_id flat.NodeId, container_id flat.NodeId) token.Pos {
	value := tc.a.node(value_id)
	container := tc.a.node(container_id)
	if value.pos.id != container.pos.id || !value.pos.is_valid() || !container.pos.is_valid() {
		return value.pos
	}
	file := tc.a.source_files[value.pos.id] or { return value.pos }
	source := tc.source_texts_by_file[file.name] or { return value.pos }
	start := int_max(0, value.pos.end)
	end := int_min(container.pos.offset, source.len)
	if start < end {
		operator := tc.in_operator_name(value_id, container_id)
		if relative := source[start..end].index(operator) {
			pos := start + relative
			return token.new_span(value.pos.id, pos, pos + operator.len)
		}
	}
	return value.pos
}

fn cast_target_interface(target Type) ?Interface {
	mut current := target
	for _ in 0 .. 8 {
		if current is Interface {
			return current
		}
		if current is Alias {
			current = current.base_type
			continue
		}
		if current is Pointer {
			current = current.base_type
			continue
		}
		return none
	}
	return none
}

fn (mut tc TypeChecker) check_comptime_if(id flat.NodeId, node flat.Node) {
	if tc.check_comptime_match_diagnostics(id, node) {
		return
	}
	if tc.check_comptime_condition_diagnostics(id, node) {
		return
	}
	take_then := tc.comptime_type_condition_value(node.value) or { return }
	branch_index := if take_then { 0 } else { 1 }
	if branch_index >= node.children_count {
		return
	}
	tc.check_node(tc.a.child(&node, branch_index))
}

fn (mut tc TypeChecker) check_comptime_match_diagnostics(id flat.NodeId, node flat.Node) bool {
	metadata := node.generic_params()
	if metadata.len < 6 || metadata[0] != '__v3_comptime_match' {
		return false
	}
	subject := metadata[1]
	subject_start := metadata[2].int()
	subject_end := metadata[3].int()
	explicit_mut := metadata[4] == 'true'
	match_kind := metadata[5]
	subject_pos := token.new_span(node.pos.id, subject_start, subject_end)
	mut has_error := false
	if explicit_mut {
		tc.record_error_at(.condition_mismatch,
			'`$match` condition `${subject}` can not be mutable', id, subject_pos)
		has_error = true
	} else if tc.ident_is_mutable_lvalue(subject) {
		tc.record_error_at(.condition_mismatch,
			'`${subject}` is mut and may have changed since its definition', id, subject_pos)
		has_error = true
	}
	subject_type := if subject.starts_with('$') {
		match subject {
			'$float' { 'f64' }
			'$string' { 'string' }
			else { 'int' }
		}
	} else if typ := tc.cur_scope.lookup(subject) {
		unalias_type(typ).name()
	} else {
		'unknown'
	}
	for offset := 6; offset + 3 < metadata.len; offset += 4 {
		pattern := metadata[offset]
		pattern_start := metadata[offset + 1].int()
		pattern_end := metadata[offset + 2].int()
		pattern_kind := metadata[offset + 3]
		pattern_pos := token.new_span(node.pos.id, pattern_start, pattern_end)
		if pattern_kind != match_kind {
			message := if match_kind == 'value' {
				'can not matching a type in a value `$match`'
			} else {
				'can not matching a value in a type `$match`'
			}
			tc.record_error_at(.condition_mismatch, message, id, pattern_pos)
			has_error = true
			continue
		}
		if match_kind == 'value' && pattern.len >= 2 && pattern[0] in [`'`, `"`]
			&& subject_type != 'string' {
			tc.record_error_at(.condition_mismatch,
				'can not matching a string value(`${pattern}`) in a non string type `$match`, `${subject}` type is `${subject_type}`',
				id, pattern_pos)
			has_error = true
		}
	}
	return has_error
}

fn (mut tc TypeChecker) check_comptime_condition_diagnostics(id flat.NodeId, node flat.Node) bool {
	condition := comptime_condition_strip_outer_parens(trimmed_space(node.value))
	for op in [' == ', ' != ', ' <= ', ' >= ', ' < ', ' > '] {
		op_idx := comptime_condition_top_level_index(condition, op)
		if op_idx < 0 {
			continue
		}
		left := trimmed_space(condition[..op_idx])
		if left.len == 0 || left.contains('.') {
			return false
		}
		if tc.lvalue_ident_is_known(left) {
			if tc.ident_is_mutable_lvalue(left) {
				tc.record_error_at(.condition_mismatch,
					'`${left}` is mut and may have changed since its definition', id, tc.comptime_condition_part_pos(node,
					left))
				return true
			}
			if initializer_id := tc.comptime_local_initializer(left, node) {
				if !tc.comptime_initializer_is_static(initializer_id) {
					tc.record_error_at(.condition_mismatch,
						'definition of `${left}` is unknown at compile time', id, tc.comptime_condition_part_pos(node,
						left))
					return true
				}
			}
		}
		return false
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(condition, op)
		if op_idx < 0 {
			continue
		}
		left := trimmed_space(condition[..op_idx])
		right := trimmed_space(condition[op_idx + op.len..])
		root := left.all_before('.')
		mut has_error := false
		root_is_generic_type := root in tc.fn_context.generic_params
			|| tc.active_generic_param(root)
		if root.len > 0 && !root_is_generic_type && !tc.lvalue_ident_is_known(root) {
			tc.record_error_at(.unknown_ident, 'undefined ident: `${root}`', id, tc.comptime_condition_part_pos(node,
				root))
			has_error = true
			if left.contains('.') {
				member := left.all_after_last('.')
				tc.record_error_at(.condition_mismatch, '`${root}` does not return a value', id, tc.comptime_condition_part_pos(node,
					member))
			}
		}
		if right.len == 0 || right[0].is_digit() || right[0] in [`'`, `"`] {
			tc.record_error_at(.unknown_type, 'invalid $if right expr: expected a type', id, tc.comptime_condition_part_pos(node,
				right))
			return true
		}
		if !right.starts_with('$') && should_check_named_type(right) && !tc.type_name_known(right) {
			tc.record_error_at(.unknown_type, 'unknown type `${right}`', id, tc.comptime_condition_part_pos(node,
				right))
			return true
		}
		return has_error
	}
	return false
}

fn (tc &TypeChecker) comptime_local_initializer(name string, condition flat.Node) ?flat.NodeId {
	mut found := flat.empty_node
	mut found_offset := -1
	for node in tc.a.nodes {
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs := tc.a.child_node(&node, 0)
		if lhs.kind != .ident || lhs.value != name || lhs.pos.id != condition.pos.id
			|| lhs.pos.offset >= condition.pos.offset || lhs.pos.offset <= found_offset {
			continue
		}
		found = tc.a.child(&node, 1)
		found_offset = lhs.pos.offset
	}
	if int(found) >= 0 {
		return found
	}
	return none
}

fn (tc &TypeChecker) comptime_initializer_is_static(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	return node.kind in [.int_literal, .float_literal, .string_literal, .char_literal, .bool_literal,
		.enum_val, .nil_literal, .none_expr]
}

fn (tc &TypeChecker) comptime_condition_part_pos(node flat.Node, part string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if part.len > 0 {
		if relative := source[start..end].index(part) {
			part_start := start + relative
			return token.new_span(node.pos.id, part_start, part_start + part.len)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) mark_inactive_comptime_subtree(id flat.NodeId, mut inactive []bool) {
	if !tc.valid_node_id(id) {
		return
	}
	if inactive.len == 0 {
		inactive = []bool{len: tc.a.nodes.len}
	}
	inactive[int(id)] = true
	node := tc.a.nodes[int(id)]
	for i in 0 .. node.children_count {
		tc.mark_inactive_comptime_subtree(tc.a.child(&node, i), mut inactive)
	}
}

fn (tc &TypeChecker) mark_inactive_top_level_comptime(id flat.NodeId, mut inactive []bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_if {
		take_then := tc.comptime_threads_condition_value(node.value) or { return }
		active_branch := if take_then { 0 } else { 1 }
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			if i == active_branch {
				tc.mark_inactive_top_level_comptime(child_id, mut inactive)
			} else {
				tc.mark_inactive_comptime_subtree(child_id, mut inactive)
			}
		}
		return
	}
	if node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		tc.mark_inactive_top_level_comptime(tc.a.child(&node, i), mut inactive)
	}
}

fn (tc &TypeChecker) inactive_top_level_comptime_nodes() []bool {
	mut inactive := []bool{}
	if file_index_usable(tc.a) {
		// Only trailing .file nodes have children; the recorded list visits
		// them in node order, matching the full scan.
		for fid in tc.a.file_node_ids {
			node := tc.a.nodes[fid]
			if node.kind != .file || node.children_count == 0 {
				continue
			}
			for i in 0 .. node.children_count {
				tc.mark_inactive_top_level_comptime(tc.a.child(&node, i), mut inactive)
			}
		}
		return inactive
	}
	for node in tc.a.nodes {
		if node.kind != .file || node.children_count == 0 {
			continue
		}
		for i in 0 .. node.children_count {
			tc.mark_inactive_top_level_comptime(tc.a.child(&node, i), mut inactive)
		}
	}
	return inactive
}

// prune_inactive_top_level_comptime removes declarations and expressions from inactive
// top-level compile-time branches after semantic checking and before later compiler stages.
pub fn (tc &TypeChecker) prune_inactive_top_level_comptime(mut a flat.FlatAst) {
	for i in tc.inactive_top_level_node_ids {
		if i < a.nodes.len {
			a.nodes[i] = flat.Node{}
		}
	}
}

fn (tc &TypeChecker) subtree_has_spawn_expr(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .comptime_if && comptime_condition_is_builtin_threads_guarded(node.value) {
		return false
	}
	if node_kind_id(node) == int(flat.NodeKind.spawn_expr) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.subtree_has_spawn_expr(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn comptime_condition_is_builtin_threads_guarded(cond string) bool {
	return comptime_condition_is_builtin_threads_guarded_with_negation(cond, false)
}

// A conjunction is guarded when either side is guarded; every alternative of a
// disjunction must be guarded. `negated` applies De Morgan's operator swap.
fn comptime_condition_is_builtin_threads_guarded_with_negation(cond string, negated bool) bool {
	clean := comptime_condition_strip_outer_parens(cond)
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := comptime_condition_is_builtin_threads_guarded_with_negation(clean[..or_idx],
			negated)
		right := comptime_condition_is_builtin_threads_guarded_with_negation(clean[or_idx + 2..],
			negated)
		return if negated { left || right } else { left && right }
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := comptime_condition_is_builtin_threads_guarded_with_negation(clean[..and_idx],
			negated)
		right := comptime_condition_is_builtin_threads_guarded_with_negation(clean[and_idx + 2..],
			negated)
		return if negated { left && right } else { left || right }
	}
	if clean.starts_with('!') {
		return comptime_condition_is_builtin_threads_guarded_with_negation(clean[1..], !negated)
	}
	return clean == 'threads'
}

fn module_file_matches_import_path(file string, imported_module string) bool {
	if !imported_module.contains('.') {
		return false
	}
	normalized_file := file.replace('\\', '/')
	module_dir := imported_module.replace('.', '/')
	file_dir := normalized_file.all_before_last('/')
	return file_dir == module_dir || file_dir.ends_with('/${module_dir}')
}

fn (tc &TypeChecker) scan_has_spawn_expr() bool {
	if tc.diagnostic_files.len == 0 {
		start := if tc.a.user_code_start > 0 { tc.a.user_code_start } else { 0 }
		mut ignored := []bool{}
		for node in tc.a.nodes[start..] {
			if node.kind == .comptime_if
				&& comptime_condition_is_builtin_threads_guarded(node.value) {
				for i in 0 .. node.children_count {
					tc.mark_inactive_comptime_subtree(tc.a.child(&node, i), mut ignored)
				}
			}
		}
		for idx, node in tc.a.nodes[start..] {
			absolute_idx := start + idx
			if (ignored.len == 0 || !ignored[absolute_idx])
				&& node_kind_id(node) == int(flat.NodeKind.spawn_expr) {
				return true
			}
		}
		return false
	}
	mut file_nodes := map[string]flat.NodeId{}
	mut file_imports := map[string][]string{}
	mut module_files := map[string][]string{}
	for idx, node in tc.a.nodes {
		if node.kind != .file || node.children_count == 0 || node.value.len == 0 {
			continue
		}
		file_nodes[node.value] = flat.NodeId(idx)
		mut imports := []string{}
		mut module_name := ''
		for i in 0 .. node.children_count {
			child := tc.a.child_node(&node, i)
			if child.kind == .module_decl {
				module_name = child.value
			} else if child.kind == .import_decl && child.value.len > 0 {
				imports << child.value
			}
		}
		file_imports[node.value] = imports
		if module_name.len > 0 {
			mut files := module_files[module_name] or { []string{} }
			files << node.value
			module_files[module_name] = files
		}
	}
	mut reachable_files := []string{}
	mut seen_files := map[string]bool{}
	for file, selected in tc.diagnostic_files {
		if selected && !file.starts_with('generic:') {
			reachable_files << file
			seen_files[file] = true
		}
	}
	mut pos := 0
	for pos < reachable_files.len {
		file := reachable_files[pos]
		pos++
		if file_id := file_nodes[file] {
			if tc.subtree_has_spawn_expr(file_id) {
				return true
			}
		}
		for imported_module in file_imports[file] or { []string{} } {
			mut imported_files := module_files[imported_module] or { []string{} }
			if imported_files.len == 0 && imported_module.contains('.') {
				short_name := imported_module.all_after_last('.')
				for candidate in module_files[short_name] or { []string{} } {
					if module_file_matches_import_path(candidate, imported_module) {
						imported_files << candidate
					}
				}
			}
			for imported_file in imported_files {
				if !seen_files[imported_file] {
					seen_files[imported_file] = true
					reachable_files << imported_file
				}
			}
		}
	}
	return false
}

// prepare_threads_condition caches whether selected inputs or their reachable imports use spawn.
pub fn (mut tc TypeChecker) prepare_threads_condition() {
	if tc.has_spawn_expr < 0 {
		tc.has_spawn_expr = if tc.scan_has_spawn_expr() { 1 } else { 0 }
	}
}

// threads_condition_value reports the cached `$if threads` condition, scanning lazily for
// direct TypeChecker users that do not run the regular compiler setup.
pub fn (tc &TypeChecker) threads_condition_value() bool {
	if tc.has_spawn_expr >= 0 {
		return tc.has_spawn_expr == 1
	}
	return tc.scan_has_spawn_expr()
}

fn (tc &TypeChecker) comptime_threads_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'threads' {
		return tc.threads_condition_value()
	}
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_threads_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_threads_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_threads_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_threads_condition_value(clean[and_idx + 2..])
	}
	if clean.starts_with('!') {
		value := tc.comptime_threads_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut tc TypeChecker) comptime_type_condition_value(cond string) ?bool {
	clean := comptime_condition_strip_outer_parens(cond)
	if clean == 'threads' {
		return tc.threads_condition_value()
	}
	if clean == 'true' {
		return true
	}
	if clean == 'false' {
		return false
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..or_idx]) or { return none }
		if left {
			return true
		}
		return tc.comptime_type_condition_value(clean[or_idx + 2..])
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := tc.comptime_type_condition_value(clean[..and_idx]) or { return none }
		if !left {
			return false
		}
		return tc.comptime_type_condition_value(clean[and_idx + 2..])
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := trimmed_space(clean[..op_idx])
			right := trimmed_space(clean[op_idx + op.len..])
			matches := tc.comptime_type_matches(left, right) or { return none }
			return if op == ' is ' { matches } else { !matches }
		}
	}
	if clean.starts_with('!') {
		value := tc.comptime_type_condition_value(clean[1..]) or { return none }
		return !value
	}
	return none
}

fn (mut tc TypeChecker) comptime_type_matches(actual string, expected string) ?bool {
	clean_actual := trimmed_space(actual)
	clean_expected := trimmed_space(expected)
	if clean_actual.len == 0 || clean_expected.len == 0
		|| (is_bare_generic_param(clean_actual) && !tc.type_name_known(clean_actual)) {
		return none
	}
	actual_type := tc.comptime_type_match_type(clean_actual)
	normalized := actual_type.name()
	match clean_expected {
		'$array' {
			return actual_type is Array || actual_type is ArrayFixed
		}
		'$array_dynamic' {
			return actual_type is Array
		}
		'$array_fixed' {
			return actual_type is ArrayFixed
		}
		'$map' {
			return actual_type is Map
		}
		'$function' {
			return actual_type is FnType
		}
		'$option' {
			return actual_type is OptionType
		}
		'$shared' {
			return tc.comptime_type_text_is_shared(clean_actual)
		}
		'$pointer' {
			return actual_type is Pointer
		}
		'$voidptr' {
			return fn_param_is_voidptr_type(actual_type)
		}
		'$int' {
			// Comptime `$int` follows v1's type group, which deliberately excludes
			// the distinct `rune` type even though it has integer operations.
			return actual_type.is_integer() && actual_type !is Rune
		}
		'$float' {
			return actual_type.is_float()
		}
		'$string' {
			return actual_type is String
		}
		'$struct' {
			return actual_type is Struct && normalized in tc.structs
		}
		'$enum' {
			return normalized in tc.enum_names
		}
		'$alias' {
			return clean_actual in tc.type_aliases
				|| tc.qualify_name(clean_actual) in tc.type_aliases
		}
		'$sumtype' {
			return normalized in tc.sum_types
		}
		'$interface' {
			return normalized in tc.interface_names
		}
		else {}
	}

	expected_type := tc.comptime_type_match_type(clean_expected)
	if expected_type is Interface {
		return tc.type_implements_interface(actual_type, expected_type)
	}
	if expected_type.name() in tc.interface_names {
		return tc.type_implements_interface(actual_type, Interface{
			name: expected_type.name()
		})
	}
	return normalized == expected_type.name()
}

fn (tc &TypeChecker) comptime_type_text_is_shared(type_text string) bool {
	mut cur := trimmed_space(type_text)
	for _ in 0 .. 16 {
		if cur.starts_with('shared ') {
			return true
		}
		target := tc.alias_target_type_text(cur) or { return false }
		if target == cur {
			return false
		}
		cur = trimmed_space(target)
	}
	return false
}

fn (mut tc TypeChecker) comptime_type_match_type(type_text string) Type {
	typ := tc.parse_type(type_text)
	if typ is Alias {
		return typ.base_type
	}
	return typ
}

// type_text_implements_interface reports whether a concrete type expression
// satisfies an interface type expression in the current checker module context.
pub fn (mut tc TypeChecker) type_text_implements_interface(actual_text string, iface_text string) bool {
	actual := tc.parse_type(actual_text)
	expected := tc.comptime_type_match_type(iface_text)
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	expected_name := expected.name()
	if expected_name in tc.interface_names {
		return tc.type_implements_interface(actual, Interface{
			name: expected_name
		})
	}
	return false
}

fn comptime_condition_matching_paren(s string, start int) int {
	mut paren_depth := 0
	mut bracket_depth := 0
	for i in start .. s.len {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
				if paren_depth == 0 && bracket_depth == 0 {
					return i
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			else {}
		}
	}
	return s.len
}

fn comptime_condition_strip_outer_parens(cond string) string {
	mut clean := trimmed_space(cond)
	for clean.len >= 2 && clean.starts_with('(') {
		end := comptime_condition_matching_paren(clean, 0)
		if end != clean.len - 1 {
			break
		}
		clean = trimmed_space(clean[1..clean.len - 1])
	}
	return clean
}

fn comptime_condition_top_level_index(s string, needle string) int {
	if needle.len == 0 || s.len < needle.len {
		return -1
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	for i := 0; i <= s.len - needle.len; i++ {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				if paren_depth > 0 {
					paren_depth--
				}
			}
			`[` {
				bracket_depth++
			}
			`]` {
				if bracket_depth > 0 {
					bracket_depth--
				}
			}
			else {}
		}

		if paren_depth == 0 && bracket_depth == 0 && s[i..].starts_with(needle) {
			return i
		}
	}
	return -1
}

// check_infix validates type-sensitive infix operations that would otherwise reach CGen
// as raw helper calls with incompatible arguments.
fn (mut tc TypeChecker) check_infix(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 || !tc.should_diagnose(id) {
		return
	}
	lhs_id := tc.a.child(&node, 0)
	rhs_id := tc.a.child(&node, 1)
	lhs_node := tc.a.node(lhs_id)
	rhs_node := tc.a.node(rhs_id)
	mut lhs_type := tc.infix_read_type(lhs_id)
	mut rhs_type := tc.infix_read_type(rhs_id)
	if (lhs_type is Void && tc.expr_subtree_has_undefined_variable_error(lhs_id))
		|| (rhs_type is Void && tc.expr_subtree_has_undefined_variable_error(rhs_id)) {
		tc.register_synth_type(id, Type(void_))
		return
	}
	if tc.expr_has_multi_match_member_error(lhs_id) || tc.expr_has_multi_match_member_error(rhs_id)
		|| tc.expr_contains_multi_pattern_subject_member(lhs_id)
		|| tc.expr_contains_multi_pattern_subject_member(rhs_id) {
		return
	}
	if rhs_node.kind == .enum_val && unalias_type(lhs_type) is Enum {
		rhs_type = tc.resolve_expr(rhs_id, lhs_type)
	} else if lhs_node.kind == .enum_val && unalias_type(rhs_type) is Enum {
		lhs_type = tc.resolve_expr(lhs_id, rhs_type)
	}
	if lhs_node.kind == .none_expr && rhs_node.kind == .none_expr {
		op := infix_operator_name(node.op) or { '' }
		tc.record_error_at(.condition_mismatch, 'invalid operator `${op}` to `none` and `none`',
			id, node.pos)
		if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
			tc.record_error_at(.condition_mismatch, 'invalid operation: shift on type `none`',
				lhs_id, lhs_node.pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		if node.op in [.logical_and, .logical_or] {
			tc.record_error_at(.condition_mismatch, 'left operand for `${op}` is not a boolean',
				lhs_id, lhs_node.pos)
			tc.record_error_at(.condition_mismatch, 'right operand for `${op}` is not a boolean',
				rhs_id, rhs_node.pos)
			tc.register_synth_type(id, Type(bool_))
			return
		}
		if node.op == .arrow {
			tc.record_error_at(.assignment_mismatch, 'cannot push on non-channel `none`', lhs_id,
				lhs_node.pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		return
	}
	if (lhs_node.kind == .as_expr && lhs_node.value.contains('.'))
		|| (rhs_node.kind == .as_expr && rhs_node.value.contains('.')) {
		return
	}
	lhs_is_nil := lhs_node.kind == .nil_literal || tc.expr_is_unsafe_nil(lhs_id)
	rhs_is_nil := rhs_node.kind == .nil_literal || tc.expr_is_unsafe_nil(rhs_id)
	if lhs_is_nil || rhs_is_nil {
		op := infix_operator_name(node.op) or { '' }
		other_type := if lhs_is_nil { rhs_type } else { lhs_type }
		clean_other := unalias_type(other_type)
		if node.op in [.eq, .ne] {
			if clean_other !is Pointer && clean_other !is FnType {
				tc.record_error_at(.condition_mismatch,
					'cannot compare with `nil` because `${other_type.name()}` is not a pointer',
					id, tc.infix_operator_pos(node, op))
				return
			}
		} else {
			tc.record_error_at(.condition_mismatch, 'cannot use `${op}` with `nil`', id, tc.infix_operator_pos(node,
				op))
			return
		}
	}
	if lhs_node.kind == .index && lhs_node.children_count > 0
		&& unalias_type(tc.resolve_type(tc.a.child(lhs_node, 0))) is OptionType {
		return
	}
	lhs_clean := unalias_type(lhs_type)
	rhs_clean := unalias_type(rhs_type)
	if lhs_clean is MultiReturn || rhs_clean is MultiReturn {
		op := infix_operator_name(node.op) or { '' }
		tc.record_error_at(.assignment_mismatch,
			'invalid number of operand for `${op}`. Only one allowed on each side.', id, node.pos)
		if node.op !in [.left_shift, .right_shift, .right_shift_unsigned, .logical_and, .logical_or,
			.arrow] {
			return
		}
	}
	if node.op == .plus && lhs_clean is Map && rhs_clean is Map {
		tc.record_error_at(.assignment_mismatch,
			'undefined operation `${lhs_type.name()}` + `${rhs_type.name()}`', id, node.pos)
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op == .left_shift && lhs_clean is OptionType
		&& unalias_type(lhs_clean.base_type) is Array {
		tc.record_error_at(.assignment_mismatch,
			'cannot push to Option array that was not unwrapped first', lhs_id,
			tc.wrapped_operand_diagnostic_pos(lhs_id))
		if tc.a.node(lhs_id).kind != .selector {
			tc.record_wrapped_infix_operand_error(lhs_id, lhs_type, lhs_clean.base_type.name())
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op == .left_shift && array_type_from_receiver(lhs_type) != none
		&& unalias_type(rhs_type) is OptionType {
		tc.record_error_at(.assignment_mismatch,
			'unwrapped Option cannot be used in an infix expression', rhs_id, tc.infix_operator_pos(node,
			'<<'))
		return
	}
	if node.op == .plus && lhs_clean is OptionType && rhs_clean !is OptionType
		&& rhs_clean !is ResultType {
		payload := lhs_clean.base_type
		compatible := tc.type_compatible(rhs_type, payload)
		if !compatible || type_is_string_like(payload) || type_is_string_like(rhs_type) {
			tc.record_error(.assignment_mismatch, 'mismatched types `${lhs_type.name()}` and `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}`', id)
		}
		tc.record_wrapped_infix_operand_error(lhs_id, lhs_type, tc.diagnostic_expr_type_name(rhs_id,
			rhs_type))
		if !compatible {
			tc.record_error(.assignment_mismatch, 'infix expr: cannot use `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}` (right expression) as `${payload.name()}`', id)
			tc.register_synth_type(id, Type(void_))
		}
		return
	}
	if node.op == .plus && rhs_clean is OptionType && lhs_clean !is OptionType
		&& lhs_clean !is ResultType {
		payload := rhs_clean.base_type
		compatible := tc.type_compatible(lhs_type, payload)
		if !compatible || type_is_string_like(payload) || type_is_string_like(lhs_type) {
			tc.record_error(.assignment_mismatch, 'mismatched types `${tc.diagnostic_expr_type_name(lhs_id,
				lhs_type)}` and `${rhs_type.name()}`', id)
		}
		tc.record_wrapped_infix_operand_error(rhs_id, rhs_type, tc.diagnostic_expr_type_name(lhs_id,
			lhs_type))
		if !compatible {
			tc.register_synth_type(id, Type(void_))
		}
		return
	}
	if node.op in [.eq, .ne]
		&& (lhs_node.kind == .none_expr || tc.a.node(rhs_id).kind == .none_expr) {
		return
	}
	lhs_pointer_arithmetic := lhs_clean is Pointer && lhs_type.name() != 'voidptr'
	rhs_pointer_arithmetic := rhs_clean is Pointer && rhs_type.name() != 'voidptr'
	pointer_arithmetic := if node.op == .plus {
		(lhs_pointer_arithmetic && rhs_clean.is_integer())
			|| (rhs_pointer_arithmetic && lhs_clean.is_integer())
	} else if node.op == .minus {
		lhs_pointer_arithmetic && (rhs_clean.is_integer() || rhs_pointer_arithmetic)
	} else {
		false
	}
	if pointer_arithmetic {
		if tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file] {
			tc.record_warning_at(.assignment_mismatch,
				'pointer arithmetic is only allowed in `unsafe` blocks', id, node.pos)
		}
		return
	}
	if tc.check_wrapped_infix_operand(lhs_id, lhs_type, rhs_id, rhs_type) {
		return
	}
	enum_operands := lhs_clean is Enum && rhs_clean is Enum
	flag_enum_bit_op := enum_operands && lhs_clean.is_flag && rhs_clean.is_flag
		&& node.op in [.pipe, .amp, .xor]
	if flag_enum_bit_op {
		return
	}
	invalid_enum_op := enum_operands && node.op !in [.eq, .ne]
	if invalid_enum_op && node.op !in [.logical_and, .logical_or] {
		tc.record_invalid_enum_infix(id, node, lhs_clean)
		return
	}
	if node.op in [.eq, .ne, .lt, .gt, .le, .ge]
		&& tc.check_signed_unsigned_comparison(node.op, lhs_id, lhs_type, rhs_id, rhs_type) {
		return
	}
	if node.op in [.eq, .ne, .lt, .gt, .le, .ge] && (lhs_type is Void || rhs_type is Void) {
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op in [.logical_and, .logical_or] {
		bool_type := Type(bool_)
		lhs_is_bool := tc.type_compatible(lhs_type, bool_type)
			&& !tc.expr_has_unresolved_generic_name_ident(lhs_id)
		rhs_is_bool := tc.type_compatible(rhs_type, bool_type)
			&& !tc.expr_has_unresolved_generic_name_ident(rhs_id)
		op := if node.op == .logical_and { '&&' } else { '||' }
		if !lhs_is_bool {
			tc.record_error_at(.condition_mismatch, 'left operand for `${op}` is not a boolean',
				lhs_id, lhs_node.pos)
		}
		if !rhs_is_bool {
			tc.record_error_at(.condition_mismatch, 'right operand for `${op}` is not a boolean',
				rhs_id, tc.a.node(rhs_id).pos)
			if node.op == .logical_or && rhs_clean !is MultiReturn {
				tc.record_error(.condition_mismatch, 'infix expr: cannot use `${tc.diagnostic_expr_type_name(rhs_id,
					rhs_type)}` (right expression) as `bool`', id)
			}
		}
		if node.op == .logical_or && lhs_node.kind == .infix && lhs_node.op == .logical_and {
			tc.record_error_at(.condition_mismatch,
				'ambiguous boolean expression. use `()` to ensure correct order of operations', id, tc.infix_operator_pos(node,
				'||'))
		}
		if invalid_enum_op {
			tc.record_invalid_enum_infix(id, node, lhs_clean)
		}
		if !lhs_is_bool || !rhs_is_bool {
			tc.register_synth_type(id, Type(void_))
		}
		return
	}
	if node.op == .plus && (lhs_clean is SumType || rhs_clean is SumType) {
		sum_name := if lhs_clean is SumType { lhs_clean.name } else { (rhs_clean as SumType).name }
		tc.record_error_at(.assignment_mismatch, 'cannot use operator `+` with `${sum_name}`', id, tc.infix_operator_pos(node,
			'+'))
		if lhs_clean !is SumType {
			tc.record_error(.assignment_mismatch, 'infix expr: cannot use `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}` (right expression) as `${tc.diagnostic_expr_type_name(lhs_id, lhs_type)}`',
				id)
		}
		return
	}
	if node.op in [.eq, .ne] {
		lhs_is_sum := lhs_clean is SumType
		rhs_is_sum := rhs_clean is SumType
		compatible := if lhs_is_sum != rhs_is_sum {
			false
		} else {
			tc.type_compatible(lhs_type, rhs_type) || tc.type_compatible(rhs_type, lhs_type)
				|| tc.expr_compatible(lhs_id, lhs_type, rhs_type)
				|| tc.expr_compatible(rhs_id, rhs_type, lhs_type)
		}
		unsafe_zero_struct_comparison :=
			((lhs_clean is Struct && tc.zero_literal_expr_id(rhs_id) != none)
			|| (rhs_clean is Struct && tc.zero_literal_expr_id(lhs_id) != none))
			&& (tc.unsafe_depth > 0 || tc.expr_is_inside_unsafe_block(id))
		if !compatible && !unsafe_zero_struct_comparison
			&& lhs_node.kind !in [.none_expr, .nil_literal]
			&& tc.a.node(rhs_id).kind !in [.none_expr, .nil_literal] {
			lhs_name := if lhs_clean is Pointer && unalias_type(lhs_clean.base_type) is Struct
				&& rhs_clean !is Pointer {
				tc.diagnostic_type_name(lhs_clean.base_type)
			} else {
				tc.diagnostic_expr_type_name(lhs_id, lhs_type)
			}
			rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
			pointer_value_comparison :=
				(rhs_clean is Pointer && tc.type_compatible(rhs_clean.base_type, lhs_clean))
				|| (lhs_clean is Pointer && tc.type_compatible(lhs_clean.base_type, rhs_clean))
			suffix := if tc.unsafe_depth == 0 && lhs_clean is Struct
				&& tc.zero_literal_expr_id(rhs_id) != none {
				'  (you can use it inside an `unsafe` block)'
			} else if tc.unsafe_depth == 0 && pointer_value_comparison {
				' (you can use it inside an `unsafe` block)'
			} else {
				''
			}
			diagnostic_pos := if rhs_node.kind == .prefix && rhs_node.op == .amp
				&& node.pos.is_valid() && node.pos.end > node.pos.offset {
				token.new_span(node.pos.id, node.pos.offset, node.pos.end - 1)
			} else {
				node.pos
			}
			tc.record_error_at(.assignment_mismatch,
				'infix expr: cannot use `${rhs_name}` (right expression) as `${lhs_name}`${suffix}',
				id, diagnostic_pos)
		}
		return
	}
	if node.op in [.div, .mod] {
		if zero_id := tc.zero_literal_expr_id(rhs_id) {
			message := if node.op == .div { 'division by zero' } else { 'modulo by zero' }
			tc.record_error(.assignment_mismatch, message, zero_id)
			return
		}
	}
	if node.op in [.lt, .gt, .le, .ge] && array_type_from_receiver(lhs_type) != none
		&& array_type_from_receiver(rhs_type) != none {
		op := infix_operator_name(node.op) or { '' }
		tc.record_error_at(.assignment_mismatch, 'only `==` and `!=` are defined on arrays', id, tc.infix_operator_pos(node,
			op))
		return
	}
	lhs_is_fn_pointer := fn_type_from_type(lhs_clean) != none
		|| (lhs_clean is Pointer && fn_type_from_type(lhs_clean.base_type) != none)
	rhs_is_fn_pointer := fn_type_from_type(rhs_clean) != none
		|| (rhs_clean is Pointer && fn_type_from_type(rhs_clean.base_type) != none)
	if node.op in [.lt, .gt, .le, .ge] && lhs_is_fn_pointer && rhs_is_fn_pointer {
		op := infix_operator_name(node.op) or { '' }
		lhs_name := if lhs_clean is FnType {
			'&${lhs_type.name().replace_once('fn(', 'fn (')}'
		} else {
			lhs_type.name()
		}
		rhs_name := if rhs_clean is FnType {
			'&${rhs_type.name().replace_once('fn(', 'fn (')}'
		} else {
			rhs_type.name()
		}
		tc.record_error_at(.condition_mismatch,
			'undefined operation `${lhs_name}` ${op} `${rhs_name}`', id, node.pos)
		return
	}
	lhs_order_type := unalias_and_unwrap_pointer_type(lhs_type)
	rhs_order_type := unalias_and_unwrap_pointer_type(rhs_type)
	if node.op in [.lt, .gt, .le, .ge] && lhs_order_type is Struct && rhs_order_type is Struct {
		required_op := match node.op {
			.gt, .ge, .le, .lt { flat.Op.lt }
			else { node.op }
		}
		if !tc.type_has_infix_operator_method(lhs_order_type, required_op) {
			op := infix_operator_name(node.op) or { '' }
			diagnostic_pos := if lhs_node.kind == .index && lhs_node.children_count > 1 {
				index_id := tc.a.child(lhs_node, 1)
				index_node := tc.a.node(index_id)
				token.new_span(node.pos.id, int_max(node.pos.offset, index_node.pos.offset - 1),
					node.pos.end)
			} else {
				node.pos
			}
			lhs_name := tc.diagnostic_expr_type_name(lhs_id, lhs_type)
			rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
			if lhs_name == rhs_name {
				tc.record_error_at(.condition_mismatch,
					'undefined operation `${lhs_name}` ${op} `${rhs_name}`', id, diagnostic_pos)
			} else {
				tc.record_error_at(.condition_mismatch,
					'mismatched types `${lhs_name}` and `${rhs_name}`', id, diagnostic_pos)
				tc.record_error_at(.condition_mismatch,
					'infix expr: cannot use `${rhs_name}` (right expression) as `${lhs_name}`', id,
					diagnostic_pos)
			}
		}
		return
	}
	if node.op == .arrow {
		channel_type := unalias_and_unwrap_pointer_type(lhs_type)
		if channel_type !is Channel {
			tc.record_error_at(.assignment_mismatch,
				'cannot push on non-channel `${lhs_type.name()}`', lhs_id, lhs_node.pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		channel := channel_type as Channel
		if channel.is_mut {
			tc.check_lvalue_mutability(rhs_id)
		}
		if !tc.expr_compatible(rhs_id, rhs_type, channel.elem_type) {
			message := if unalias_type(channel.elem_type) is Pointer
				&& unalias_type(rhs_type) !is Pointer {
				'cannot push non-reference `${rhs_type.name()}` on `${Type(channel).name()}`'
			} else {
				'cannot push `${rhs_type.name()}` on `${Type(channel).name()}`'
			}
			rhs_pos := if rhs_node.kind == .prefix && rhs_node.op == .amp {
				tc.prefix_operator_pos(rhs_id, '&')
			} else {
				tc.array_element_diagnostic_pos(rhs_id)
			}
			tc.record_error_at(.assignment_mismatch, message, rhs_id, rhs_pos)
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op in [.amp, .pipe, .xor] {
		op := infix_operator_name(node.op) or { '' }
		if !unalias_type(lhs_type).is_integer() {
			tc.record_error_at(.assignment_mismatch, 'left type of `${op}` cannot be non-integer type `${tc.diagnostic_expr_type_name(lhs_id,
				lhs_type)}`', lhs_id, lhs_node.pos)
			return
		}
		if !unalias_type(rhs_type).is_integer() {
			tc.record_error_at(.assignment_mismatch, 'right type of `${op}` cannot be non-integer type `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}`', rhs_id, tc.a.node(rhs_id).pos)
			return
		}
	}
	if node.op == .left_shift {
		if lhs_array := array_type_from_receiver(lhs_type) {
			tc.check_lvalue_mutability(lhs_id)
			if const_id := tc.addressed_const_ident(rhs_id) {
				const_node := tc.a.node(const_id)
				tc.record_error_at(.assignment_mismatch,
					'cannot have mutable reference to const `${const_node.value}`', const_id,
					tc.node_value_diagnostic_pos(const_id))
			}
			append_rhs_type := tc.array_append_diagnostic_rhs_type(rhs_id, rhs_type)
			if !tc.array_append_rhs_compatible(rhs_id, append_rhs_type, lhs_array.elem_type)
				|| (unalias_type(append_rhs_type) is ArrayFixed
				&& unalias_type(lhs_array.elem_type) is Interface) {
				rhs_name := tc.array_append_rhs_diagnostic_name(rhs_id, append_rhs_type)
				diagnostic_id := tc.array_append_rhs_diagnostic_id(rhs_id)
				elem_type := unalias_type(lhs_array.elem_type)
				if elem_type is Interface {
					if !tc.record_interface_implementation_error(.assignment_mismatch,
						append_rhs_type, elem_type, diagnostic_id, tc.a.node(diagnostic_id).pos) {
						tc.record_error(.assignment_mismatch,
							'cannot append `${rhs_name}` to `${lhs_type.name()}`', diagnostic_id)
					}
				} else {
					tc.record_error(.assignment_mismatch,
						'cannot append `${rhs_name}` to `${lhs_type.name()}`', diagnostic_id)
				}
			}
			if !tc.array_append_is_standalone_statement(id) {
				tc.record_error_at(.assignment_mismatch,
					'array append cannot be used in an expression', id, tc.infix_operator_pos(node,
					'<<'))
			}
			if tc.a.nodes[int(lhs_id)].kind == .array_literal {
				tc.record_error(.assignment_mismatch, 'array literal can not be modified', lhs_id)
			}
			return
		}
	}
	if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
		if !unalias_type(lhs_type).is_integer() {
			tc.record_error_at(.assignment_mismatch, 'invalid operation: shift on type `${tc.diagnostic_expr_type_name(lhs_id,
				lhs_type)}`', lhs_id, lhs_node.pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		if !unalias_type(rhs_type).is_integer() {
			tc.record_error_at(.assignment_mismatch, 'cannot shift non-integer type `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}` into type `${tc.diagnostic_expr_type_name(lhs_id, lhs_type)}`', rhs_id,
				tc.a.node(rhs_id).pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		skip_inactive_compact_else := tc.shift_is_in_inactive_compact_else(id)
		bit_size := tc.integer_shift_bit_size(lhs_type)
		if !skip_inactive_compact_else && bit_size > 0 && rhs_node.kind == .int_literal {
			if shift_count := v_int_literal_value(rhs_node.value) {
				if shift_count >= bit_size {
					tc.record_error_at(.assignment_mismatch,
						'shift count for type `${lhs_type.name()}` too large (maximum: ${bit_size - 1} bits)',
						rhs_id, rhs_node.pos)
				}
			}
		}
		if !skip_inactive_compact_else && node.op == .left_shift && lhs_node.kind == .ident
			&& tc.ident_is_mutable_lvalue(lhs_node.value) && !type_is_unsigned_integer(lhs_type) {
			tc.record_notice_at(.assignment_mismatch,
				'shifting a value from a signed type `${lhs_type.name()}` can change the sign',
				lhs_id, tc.node_value_diagnostic_pos(lhs_id))
		}
		if node.op == .left_shift && tc.array_append_is_standalone_statement(id) {
			tc.record_error_at(.assignment_mismatch, 'unused expression', id, node.pos)
		}
	}
	if node.op in [.minus, .mul, .div, .mod] && (lhs_type is Void || rhs_type is Void) {
		lhs_name := tc.diagnostic_expr_type_name(lhs_id, lhs_type)
		rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
		tc.record_error(.assignment_mismatch, 'mismatched types `${lhs_name}` and `${rhs_name}`',
			id)
		tc.register_synth_type(id, Type(void_))
		return
	}
	if node.op in [.plus, .minus] && lhs_type.name() == 'voidptr' {
		op := infix_operator_name(node.op) or { '' }
		tc.record_error_at(.assignment_mismatch, '`${op}` cannot be used with `voidptr`', lhs_id,
			lhs_node.pos)
		return
	}
	if node.op == .mod && (unalias_type(lhs_type).is_float() || unalias_type(rhs_type).is_float()) {
		float_id := if unalias_type(lhs_type).is_float() { lhs_id } else { rhs_id }
		tc.record_error_at(.assignment_mismatch,
			'float modulo not allowed, use math.fmod() instead', float_id, tc.a.node(float_id).pos)
		return
	}
	if node.op in [.mul, .div, .mod] {
		lhs_pointer := lhs_clean is Pointer && lhs_type.name() != 'voidptr'
		rhs_pointer := rhs_clean is Pointer && rhs_type.name() != 'voidptr'
		if lhs_pointer || rhs_pointer {
			op := infix_operator_name(node.op) or { '' }
			if lhs_pointer {
				tc.record_error(.assignment_mismatch, 'invalid operator `${op}` to `${lhs_type.name()}` and `${tc.diagnostic_expr_type_name(rhs_id,
					rhs_type)}`', id)
			}
			tc.record_error(.assignment_mismatch,
				'infix `${op}` is not defined for pointer values', id)
			return
		}
	}
	if node.op in [.minus, .mul, .div, .mod] {
		if lhs_type is Unknown || rhs_type is Unknown {
			return
		}
		if node.op == .minus && lhs_type is Alias && rhs_type is Alias
			&& lhs_type.name == rhs_type.name
			&& (unalias_type(lhs_type) is Array || unalias_type(lhs_type) is Map) {
			tc.record_error(.assignment_mismatch,
				'undefined operation `${lhs_type.name}` - `${rhs_type.name}`', id)
			return
		}
		if (infix_power_type_is_numeric(lhs_type) && infix_power_type_is_numeric(rhs_type))
			|| tc.infix_operator_return_type(node.op, lhs_type, rhs_type) != none {
			return
		}
		lhs_name := tc.diagnostic_expr_type_name(lhs_id, lhs_type)
		rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
		if lhs_clean is Struct && rhs_clean is Struct && lhs_name == rhs_name
			&& !tc.type_has_infix_operator_method(lhs_type, node.op)
			&& !tc.type_has_infix_operator_method(rhs_type, node.op) {
			op := infix_operator_name(node.op) or { '' }
			tc.record_error(.assignment_mismatch,
				'undefined operation `${lhs_name}` ${op} `${rhs_name}`', id)
			return
		}
		if !tc.type_has_infix_operator_method(lhs_type, node.op)
			&& !tc.type_has_infix_operator_method(rhs_type, node.op) {
			tc.record_error(.assignment_mismatch,
				'mismatched types `${lhs_name}` and `${rhs_name}`', id)
		}
		tc.record_error(.assignment_mismatch,
			'infix expr: cannot use `${rhs_name}` (right expression) as `${lhs_name}`', id)
		return
	}
	if node.op == .power {
		if lhs_type is Unknown || rhs_type is Unknown {
			return
		}
		if _ := tc.infix_operator_return_type(node.op, lhs_type, rhs_type) {
			return
		}
		if !infix_power_type_is_numeric(lhs_type) || !infix_power_type_is_numeric(rhs_type) {
			tc.record_error(.assignment_mismatch,
				'operator `**` requires numeric operands; got `${lhs_type.name()}` and `${rhs_type.name()}`',
				id)
		}
		return
	}
	if node.op != .plus {
		return
	}
	lhs_pointer := unalias_type(lhs_type) is Pointer && lhs_type.name() != 'voidptr'
	rhs_pointer := unalias_type(rhs_type) is Pointer && rhs_type.name() != 'voidptr'
	if lhs_pointer && rhs_pointer {
		tc.record_error(.assignment_mismatch,
			'invalid operator `+` to `${lhs_type.name()}` and `${rhs_type.name()}`', id)
		return
	}
	if (lhs_pointer && !rhs_type.is_integer()) || (rhs_pointer && !lhs_type.is_integer()) {
		tc.record_error_at(.assignment_mismatch,
			'mismatched types `${lhs_type.name()}` and `${rhs_type.name()}`', id, token.new_span(node.pos.id,
			node.pos.offset, int_max(node.pos.offset + 1, node.pos.end - 1)))
		tc.register_synth_type(id, Type(void_))
		return
	}
	if signature := tc.infix_operator_signature(.plus, lhs_type) {
		if signature.param_count >= 2
			&& signature.param_type.name() != unwrap_pointer(lhs_type).name()
			&& rhs_type.name() == signature.param_type.name() {
			tc.record_error(.assignment_mismatch, 'infix expr: cannot use `${tc.diagnostic_expr_type_name(rhs_id,
				rhs_type)}` (right expression) as `${tc.diagnostic_expr_type_name(lhs_id, lhs_type)}`',
				id)
			return
		}
	}
	lhs_is_string := type_is_string_like(lhs_type)
	rhs_is_string := type_is_string_like(rhs_type)
	if lhs_is_string || rhs_is_string {
		if lhs_is_string != rhs_is_string && lhs_type !is Unknown && rhs_type !is Unknown {
			tc.record_error(.assignment_mismatch,
				'operator `+` cannot concatenate `${lhs_type.name()}` and `${rhs_type.name()}`', id)
		}
		return
	}
	if lhs_type is Unknown || rhs_type is Unknown {
		return
	}
	if (infix_power_type_is_numeric(lhs_type) && infix_power_type_is_numeric(rhs_type))
		|| tc.infix_operator_return_type(node.op, lhs_type, rhs_type) != none
		|| (lhs_type is Pointer && rhs_type.is_integer())
		|| (rhs_type is Pointer && lhs_type.is_integer()) {
		return
	}
	lhs_name := tc.diagnostic_expr_type_name(lhs_id, lhs_type)
	rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
	tc.record_error(.assignment_mismatch, 'mismatched types `${lhs_name}` and `${rhs_name}`', id)
	if lhs_type is Void || rhs_type is Void {
		return
	}
	tc.record_error(.assignment_mismatch,
		'infix expr: cannot use `${rhs_name}` (right expression) as `${lhs_name}`', id)
}

fn (tc &TypeChecker) integer_shift_bit_size(typ Type) int {
	clean := unalias_type(typ)
	if clean is Primitive {
		if typ.name() == 'int' || clean.size == 0 {
			return 32
		}
		return int(clean.size)
	}
	if clean is ISize || clean is USize {
		return 64
	}
	return 0
}

fn (tc &TypeChecker) shift_is_in_inactive_compact_else(id flat.NodeId) bool {
	mut current_id := id
	for _ in 0 .. 8 {
		parent_id := tc.direct_parent_id(current_id)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .if_expr && parent.children_count >= 3 {
			condition_id := tc.a.child(parent, 0)
			condition_value := tc.constant_bool_value(condition_id) or { return false }
			if !condition_value {
				return false
			}
			else_id := tc.a.child(parent, 2)
			if current_id != else_id {
				return false
			}
			else_node := tc.a.node(else_id)
			return else_node.kind == .block && else_node.children_count == 1
		}
		current_id = parent_id
	}
	return false
}

fn (tc &TypeChecker) expr_has_unresolved_generic_name_ident(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .ident && is_bare_generic_param(node.value) && !tc.type_name_known(node.value)
		&& node.value !in tc.fn_context.generic_params && !tc.active_generic_param(node.value)
		&& !tc.node_has_enclosing_generic_param(id, node.value)
		&& !tc.source_enclosing_fn_has_generic_param(id, node.value) {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_has_unresolved_generic_name_ident(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_wrapped_infix_operand(lhs_id flat.NodeId, lhs_type Type, rhs_id flat.NodeId, rhs_type Type) bool {
	lhs_clean := unalias_type(lhs_type)
	rhs_clean := unalias_type(rhs_type)
	if lhs_clean is ResultType {
		tc.record_unwrapped_result_infix_error(lhs_id, lhs_clean)
		return true
	}
	if rhs_clean is ResultType {
		tc.record_unwrapped_result_infix_error(rhs_id, rhs_clean)
		return true
	}
	if lhs_clean is OptionType {
		if tc.a.node(rhs_id).kind == .none_expr {
			return false
		}
		expected_name := if rhs_clean is OptionType {
			lhs_clean.base_type.name()
		} else {
			tc.diagnostic_expr_type_name(rhs_id, rhs_type)
		}
		tc.record_wrapped_infix_operand_error(lhs_id, lhs_type, expected_name)
		return true
	}
	if rhs_clean is OptionType {
		if tc.a.node(lhs_id).kind == .none_expr {
			return false
		}
		tc.record_wrapped_infix_operand_error(rhs_id, rhs_type, tc.diagnostic_expr_type_name(lhs_id,
			lhs_type))
		option_base := unalias_type(rhs_clean.base_type)
		interface_base := if option_base is Pointer {
			unalias_type(option_base.base_type)
		} else {
			option_base
		}
		if unalias_and_unwrap_pointer_type(lhs_type) is Struct && interface_base is Interface {
			parent_id := tc.direct_parent_id(rhs_id)
			if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .infix {
				tc.record_error(.assignment_mismatch, 'infix expr: cannot use `${interface_base.name}` (right expression) as `${tc.diagnostic_expr_type_name(lhs_id,
					lhs_type)}`', parent_id)
			}
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) record_invalid_enum_infix(id flat.NodeId, node flat.Node, typ Type) {
	clean := unalias_type(typ)
	is_flag := clean is Enum && clean.is_flag
	message := if is_flag {
		'only `==`, `!=`, `|`, `&`, `^` and `~` are defined on `@[flag]` tagged `enum`, use an explicit cast to `int` if needed'
	} else {
		'only `==` and `!=` are defined on `enum`, use an explicit cast to `int` if needed'
	}
	op := infix_operator_name(node.op) or { '' }
	tc.record_error_at(.assignment_mismatch, message, id, tc.infix_operator_pos(node, op))
}

fn (tc &TypeChecker) expr_is_inside_unsafe_block(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .block && parent.value == 'unsafe' {
			return true
		}
		current = parent_id
	}
	return false
}

fn (mut tc TypeChecker) check_signed_unsigned_comparison(op flat.Op, lhs_id flat.NodeId, lhs_type Type, rhs_id flat.NodeId, rhs_type Type) bool {
	lhs_unsigned := type_is_unsigned_integer(lhs_type)
	rhs_unsigned := type_is_unsigned_integer(rhs_type)
	lhs_negative := tc.expr_is_negative_integer_literal(lhs_id)
	rhs_negative := tc.expr_is_negative_integer_literal(rhs_id)
	is_equality := op in [.eq, .ne]
	if lhs_unsigned && rhs_negative {
		message := if is_equality {
			'`${lhs_type.name()}` cannot be compared with negative value'
		} else {
			'unsigned integer cannot be compared with negative value'
		}
		tc.record_error_at(.condition_mismatch, message, rhs_id, tc.a.node(rhs_id).pos)
		return true
	}
	if lhs_negative && rhs_unsigned {
		message := if is_equality {
			'negative value cannot be compared with `${rhs_type.name()}`'
		} else {
			'unsigned integer cannot be compared with negative value'
		}
		tc.record_error_at(.condition_mismatch, message, lhs_id, tc.a.node(lhs_id).pos)
		return true
	}
	lhs_clean := unalias_type(lhs_type)
	rhs_clean := unalias_type(rhs_type)
	if !lhs_clean.is_integer() || !rhs_clean.is_integer() || lhs_unsigned == rhs_unsigned {
		return false
	}
	if ((lhs_clean is Char || lhs_clean is Rune) && rhs_clean.name() == 'u8')
		|| ((rhs_clean is Char || rhs_clean is Rune) && lhs_clean.name() == 'u8') {
		return false
	}
	lhs := tc.a.node(lhs_id)
	rhs := tc.a.node(rhs_id)
	if (lhs_unsigned && rhs.kind in [.int_literal, .char_literal] && !rhs_negative)
		|| (rhs_unsigned && lhs.kind in [.int_literal, .char_literal] && !lhs_negative) {
		return false
	}
	if (lhs_unsigned && (tc.const_int_expr(rhs_id, tc.cur_module, []) or { -1 }) >= 0) || (rhs_unsigned && (tc.const_int_expr(lhs_id, tc.cur_module, []) or {
		-1
	}) >= 0) {
		return false
	}
	signed_bits := if lhs_unsigned {
		comparison_integer_bits(rhs_clean)
	} else {
		comparison_integer_bits(lhs_clean)
	}
	unsigned_bits := if lhs_unsigned {
		comparison_integer_bits(lhs_clean)
	} else {
		comparison_integer_bits(rhs_clean)
	}
	if signed_bits >= unsigned_bits {
		return false
	}
	op_text := infix_operator_name(op) or { '' }
	parent_id := tc.direct_parent_id(lhs_id)
	parent := if tc.valid_node_id(parent_id) { *tc.a.node(parent_id) } else { flat.Node{} }
	pos := if parent.kind == .infix {
		tc.infix_operator_pos(parent, op_text)
	} else {
		tc.a.node(rhs_id).pos
	}
	tc.record_error_at(.condition_mismatch,
		'`${lhs_type.name()}` cannot be compared with `${rhs_type.name()}`', rhs_id, pos)
	return true
}

fn comparison_integer_bits(typ Type) int {
	clean := unalias_type(typ)
	if clean is Primitive {
		return if clean.size == 0 { 32 } else { int(clean.size) }
	}
	if clean is ISize || clean is USize {
		return 64
	}
	return 0
}

fn (mut tc TypeChecker) record_unwrapped_result_infix_error(id flat.NodeId, result_type ResultType) {
	tc.record_error_at(.assignment_mismatch,
		'unwrapped Result cannot be used in an infix expression', id,
		tc.wrapped_operand_diagnostic_pos(id))
	tc.record_unhandled_result_call(id, result_type)
}

fn (mut tc TypeChecker) record_wrapped_infix_operand_error(id flat.NodeId, wrapped Type, expected_name string) {
	clean := unalias_type(wrapped)
	display_name := if clean is OptionType {
		base := unalias_type(clean.base_type)
		if base is Pointer && unalias_type(base.base_type) is Interface {
			'?${base.base_type.name()}'
		} else {
			wrapped.name()
		}
	} else {
		wrapped.name()
	}
	tc.record_error_at(.assignment_mismatch,
		'`${display_name}` cannot be used as `${expected_name}`, unwrap the option first', id,
		tc.wrapped_operand_diagnostic_pos(id))
}

fn (tc &TypeChecker) array_append_rhs_diagnostic_name(id flat.NodeId, typ Type) string {
	name := tc.diagnostic_expr_type_name(id, typ)
	if tc.a.node(id).kind == .spawn_expr && name.ends_with('!void') {
		return name[..name.len - 4]
	}
	return name
}

fn (tc &TypeChecker) array_append_rhs_diagnostic_id(id flat.NodeId) flat.NodeId {
	node := tc.a.node(id)
	if node.kind == .spawn_expr && node.children_count > 0 {
		return tc.a.child(node, 0)
	}
	return id
}

fn (tc &TypeChecker) wrapped_operand_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .prefix && node.op == .mul {
		return token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 1)
	}
	if node.kind == .selector {
		return tc.node_value_diagnostic_pos(id)
	}
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := tc.a.child_node(callee, 0)
			if base.kind == .ident && !tc.ident_resolves_to_value(base.value)
				&& tc.resolve_import_alias(base.value) != none {
				return tc.method_call_name_pos(*node, *callee)
			}
		}
	}
	return node.pos
}

fn (tc &TypeChecker) array_append_diagnostic_rhs_type(rhs_id flat.NodeId, fallback Type) Type {
	if !tc.valid_node_id(rhs_id) {
		return fallback
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind != .array_literal || rhs.children_count == 0 {
		return fallback
	}
	for i in 0 .. rhs.children_count {
		if tc.a.child_node(&rhs, i).kind != .int_literal {
			return fallback
		}
	}
	return Type(Array{
		elem_type: Type(int_)
	})
}

fn (tc &TypeChecker) array_append_rhs_compatible(rhs_id flat.NodeId, rhs_type Type, elem_type Type) bool {
	if tc.expr_compatible(rhs_id, rhs_type, elem_type) {
		return true
	}
	clean_rhs := unalias_type(rhs_type)
	clean_elem := unalias_type(elem_type)
	if clean_elem is Pointer {
		pointer_base := unalias_type(clean_elem.base_type)
		if pointer_base is Interface && tc.type_compatible(clean_rhs, clean_elem.base_type) {
			return true
		}
	}
	if clean_rhs is Array {
		clean_rhs_elem := unalias_type(clean_rhs.elem_type)
		if clean_rhs_elem.is_integer() && clean_elem.is_integer() {
			return clean_rhs_elem.name() == clean_elem.name()
		}
		return tc.type_compatible(clean_rhs.elem_type, elem_type)
	}
	return tc.expr_compatible(rhs_id, rhs_type, elem_type)
}

fn (tc &TypeChecker) array_append_is_standalone_statement(id flat.NodeId) bool {
	if tc.is_statement_node(id) {
		return true
	}
	// Source ASTs already have a direct-parent index. Nearly every `<<` expression
	// wrapped by an expr_stmt can be answered from it, without walking the entire
	// program once per append. Keep the scan below as a fallback for shared or
	// transform-created nodes after the index is no longer authoritative.
	if tc.direct_parent_index_trusted {
		idx := int(id)
		if idx >= 0 && idx < tc.direct_parent_ids.len {
			parent_id := tc.direct_parent_ids[idx]
			if tc.valid_node_id(parent_id) {
				parent := tc.a.node(parent_id)
				return parent.kind == .expr_stmt && parent.children_count == 1
					&& tc.a.child(parent, 0) == id && tc.is_statement_node(parent_id)
			}
		}
		return false
	}
	for index, candidate in tc.a.nodes {
		if candidate.kind == .expr_stmt && candidate.children_count == 1
			&& tc.a.child(&candidate, 0) == id {
			return tc.is_statement_node(flat.NodeId(index))
		}
	}
	return false
}

fn (tc &TypeChecker) zero_literal_expr_id(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.int_literal, .float_literal] && numeric_literal_is_zero(node.value) {
		return id
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return tc.zero_literal_expr_id(tc.a.child(&node, 0))
	}
	return none
}

fn numeric_literal_is_zero(value string) bool {
	mut clean := value.to_lower().replace('_', '')
	for suffix in ['f32', 'f64', 'u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64'] {
		if clean.ends_with(suffix) {
			clean = clean[..clean.len - suffix.len]
			break
		}
	}
	if clean.starts_with('0x') || clean.starts_with('0b') || clean.starts_with('0o') {
		clean = clean[2..]
	}
	if clean.len == 0 {
		return false
	}
	for c in clean {
		if c !in [`0`, `.`] {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) infix_operator_pos(node flat.Node, op string) token.Pos {
	if node.children_count < 2 {
		return node.pos
	}
	lhs := tc.a.child_node(&node, 0)
	rhs := tc.a.child_node(&node, 1)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(lhs.pos.end, node.pos.offset)
	end := int_min(rhs.pos.offset, node.pos.end)
	if start < end {
		if relative := source[start..end].index(op) {
			op_start := start + relative
			return token.new_span(node.pos.id, op_start, op_start + op.len)
		}
	}
	if node.pos.offset >= 0 && node.pos.end <= source.len && node.pos.offset < node.pos.end {
		if relative := source[node.pos.offset..node.pos.end].index(op) {
			op_start := node.pos.offset + relative
			return token.new_span(node.pos.id, op_start, op_start + op.len)
		}
	}
	return node.pos
}

fn infix_power_type_is_numeric(typ Type) bool {
	clean := unalias_type(typ)
	return clean.is_integer() || clean.is_float()
}

fn optional_payload_is_string(typ Type) bool {
	if typ is OptionType {
		return type_is_string_like(typ.base_type)
	}
	if typ is ResultType {
		return type_is_string_like(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) infix_read_type(id flat.NodeId) Type {
	typ := tc.resolve_type(id)
	if int(id) < 0 {
		return typ
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident {
		if base := tc.mut_param_base_for_current_ident(node.value, typ) {
			return base
		}
	}
	return typ
}

fn (tc &TypeChecker) mut_param_base_for_current_ident(name string, typ Type) ?Type {
	if tc.fn_context.mut_param_base_types.len == 0 {
		// Most functions have no mut params; skip the string-keyed probe that
		// otherwise runs for every infix operand.
		return none
	}
	if isnil(tc.cur_scope) || tc.cur_scope == tc.file_scope || tc.fn_context.node_id < 0 {
		return none
	}
	base := tc.fn_context.mut_param_base_types[name] or { return none }
	if !tc.lvalue_matches_mut_param(typ, base) {
		return none
	}
	if !tc.mut_param_binding_matches_lvalue(name) {
		return none
	}
	return base
}

fn type_is_string_like(typ Type) bool {
	if typ is String {
		return true
	}
	if typ is Alias {
		return type_is_string_like(typ.base_type)
	}
	return false
}

fn (tc &TypeChecker) select_branch_is_timeout(branch flat.Node) bool {
	if branch.kind != .select_branch || branch.value == 'else' || branch.children_count == 0
		|| branch.value in ['recv', 'recv_assign'] || branch.value.starts_with('recv_compound:') {
		return false
	}
	first := tc.a.child_node(&branch, 0)
	return !((first.kind == .infix && first.op == .arrow)
		|| (first.kind == .prefix && first.op == .arrow))
}

// check_select_stmt validates a `select { ... }` statement. A receive declaration
// binds the channel element type in the branch scope, while a receive assignment
// is checked against its existing lvalue type before the branch body.
fn (mut tc TypeChecker) check_select_stmt(node flat.Node) {
	mut has_else := false
	mut has_timeout := false
	mut conflict_id := flat.empty_node
	mut duplicate_timeout_id := flat.empty_node
	for i in 0 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			continue
		}
		if branch.value == 'else' {
			if has_timeout && int(conflict_id) < 0 {
				conflict_id = branch_id
			}
			has_else = true
		} else if tc.select_branch_is_timeout(branch) {
			if has_else && int(conflict_id) < 0 {
				conflict_id = branch_id
			}
			if has_timeout && int(duplicate_timeout_id) < 0 {
				duplicate_timeout_id = branch_id
			}
			has_timeout = true
		}
	}
	if tc.valid_node_id(conflict_id) {
		tc.record_error(.condition_mismatch,
			'`else` and timeout value are mutually exclusive `select` keys', conflict_id)
	}
	if tc.valid_node_id(duplicate_timeout_id) {
		tc.record_error(.condition_mismatch,
			'at most one timeout branch allowed in `select` block', duplicate_timeout_id)
	}
	$if ownership ? {
		tc.ownership_begin_branch_group()
	}
	base_smartcasts := clone_smartcasts(tc.smartcasts)
	mut invalidated_smartcasts := map[string]bool{}
	unsafe_alias_base := tc.fn_context.unsafe_reference_alias_owners.clone()
	mut unsafe_alias_paths := []map[string]bool{}
	for i in 0 .. node.children_count {
		tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_base.clone()
		if base_smartcasts.len > 0 {
			tc.smartcasts = clone_smartcasts(base_smartcasts)
		}
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			tc.check_node(branch_id)
			for key, _ in base_smartcasts {
				if key !in tc.smartcasts {
					invalidated_smartcasts[key] = true
				}
			}
			continue
		}
		$if ownership ? {
			tc.ownership_begin_branch()
			if branch.value == 'else' {
				tc.ownership_note_branch_group_else()
			}
		}
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(branch_id)
		}
		mut body_start := 0
		is_assignment_case := branch.value in ['recv', 'recv_assign']
			|| branch.value.starts_with('recv_compound:')
		mut has_receive_rhs := false
		if is_assignment_case && branch.children_count >= 2 {
			second := tc.a.child_node(&branch, 1)
			has_receive_rhs = second.kind == .prefix && second.op == .arrow
		}
		if is_assignment_case && !has_receive_rhs {
			tc.record_error(.assignment_mismatch,
				'select assignment case requires a channel receive on the right side', branch_id)
			body_start = if branch.children_count >= 2 { 2 } else { int(branch.children_count) }
		}
		if branch.value.starts_with('recv_compound:') && has_receive_rhs {
			recv_id := tc.a.child(&branch, 1)
			tc.check_node(recv_id)
			op := branch.value.all_after('recv_compound:')
			tc.record_error(.assignment_mismatch,
				'compound receive assignment `${op}` is not supported in `select`; use `=` or `:=`',
				branch_id)
			body_start = 2
		}
		receive_assignment := branch.value in ['recv', 'recv_assign'] && has_receive_rhs
		if receive_assignment {
			// children[0] = bound/assigned lvalue, children[1] = `<-ch` receive expr.
			var_id := tc.a.child(&branch, 0)
			recv_id := tc.a.child(&branch, 1)
			if branch.value == 'recv' {
				tc.check_node(recv_id)
				elem_type := tc.resolve_type(recv_id)
				if tc.valid_node_id(var_id) {
					var_node := tc.a.nodes[int(var_id)]
					if var_node.kind == .ident && var_node.value.len > 0 {
						tc.cur_scope.insert(var_node.value, elem_type)
						tc.remember_expr_type(var_id, elem_type)
						$if ownership ? {
							tc.ownership_note_binding(var_node.value, elem_type, var_id)
						}
					} else {
						tc.record_error(.assignment_mismatch,
							'select receive declaration requires a plain identifier on the left side',
							var_id)
					}
				}
			} else {
				var_node := tc.a.node(var_id)
				if var_node.kind == .ident && var_node.value == '_' {
					tc.record_error_at(.assignment_mismatch,
						'cannot send on `_`, use `_ := <- quit` instead', var_id,
						tc.node_value_diagnostic_pos(var_id))
					tc.check_node(recv_id)
				} else {
					lhs_type := tc.resolve_lvalue_type(var_id)
					tc.remember_expr_type(var_id, lhs_type)
					expected_type := tc.assignment_expected_type(var_id, lhs_type)
					tc.annotate_expected_expr(recv_id, expected_type)
					tc.check_node(recv_id)
					rhs_type := tc.resolve_expr(recv_id, expected_type)
					if !tc.assignment_types_compatible(recv_id, rhs_type, expected_type, .assign) {
						tc.type_mismatch(.assignment_mismatch,
							'cannot assign `${rhs_type.name()}` to `${expected_type.name()}`',
							branch_id)
					}
					lhs_key := tc.expr_key(var_id)
					if lhs_key.len > 0 {
						tc.smartcasts.delete(lhs_key)
					}
				}
			}
			body_start = 2
		}
		tc.check_statement_sequence(branch, body_start, false)
		tc.pop_scope()
		$if ownership ? {
			tc.ownership_end_branch(branch_id)
		}
		if !tc.stmt_sequence_definitely_returns(&branch, body_start) {
			unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
		}
		for key, _ in base_smartcasts {
			if key !in tc.smartcasts {
				invalidated_smartcasts[key] = true
			}
		}
	}
	tc.smartcasts = clone_smartcasts(base_smartcasts)
	for key, _ in invalidated_smartcasts {
		tc.smartcasts.delete(key)
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_base)
	$if ownership ? {
		tc.ownership_end_branch_group()
	}
}

// check_array_init validates an `[]T{len: ..., init: ...}` initializer. The `init:`
// expression may reference the magic `index` variable (the current element index),
// so it is checked in a scope where `index` is bound to an int.
fn (mut tc TypeChecker) check_array_init(id flat.NodeId, node flat.Node) {
	if tc.fn_context.generic_params.len == 0
		&& tc.type_text_has_generic_struct_placeholder_application(node.typ) {
		tc.discard_unknown_type_errors_inside_node(node)
		tc.record_error_at(.unsupported_generic,
			'generic struct cannot be used in non-generic function', id,
			tc.array_init_head_pos(node))
		tc.register_synth_type(id, unknown_type('generic struct in non-generic function'))
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.discard_unknown_type_errors_inside_node(node)
		return
	}
	if generic_name := tc.bare_generic_decl_type_name(node.typ) {
		qualified := tc.qualify_name(generic_name)
		if generic_name in tc.struct_generic_params || qualified in tc.struct_generic_params {
			tc.record_error_at(.unknown_type,
				'generic struct `${generic_name}` must specify type parameter, e.g. ${generic_name}[int]',
				id, tc.type_diagnostic_pos(id, generic_name))
		}
	}
	if bound := fixed_array_bound_text(node.typ) {
		bound_pos := tc.fixed_array_bound_pos(node, bound)
		if message := tc.fixed_array_bound_type_diagnostic(bound) {
			tc.record_error_at(.assignment_mismatch, message, id, bound_pos)
			if message.ends_with('has to be casted to integer to be used as size')
				&& bound.contains('.') {
				tc.record_error_at(.assignment_mismatch,
					'fixed size cannot be zero or negative (fixed_size: 0)', id, bound_pos)
			}
		} else if fixed_size := tc.const_int_value(bound, []string{}) {
			if fixed_size <= 0 {
				tc.record_error_at(.assignment_mismatch,
					'fixed size cannot be zero or negative (fixed_size: ${fixed_size})', id,
					bound_pos)
			}
		} else {
			tc.record_error_at(.assignment_mismatch, 'non-constant array bound `${bound}`', id,
				bound_pos)
			tc.record_error_at(.assignment_mismatch,
				'fixed size cannot be zero or negative (fixed_size: 0)', id, bound_pos)
		}
	}
	if type_text_contains_any(node.typ) {
		end := int_max(node.pos.offset + 1, node.pos.end - 1)
		tc.record_error_at(.unknown_type, 'cannot use type `any` here', id, token.new_span(node.pos.id,
			node.pos.offset, end))
	}
	tc.check_array_elements_initialized(id, node)
	tc.check_array_element_reference_fields(id, node)
	declared_array_type := unalias_type(tc.parse_type(node.typ))
	init_elem_type := array_like_elem_type(declared_array_type)
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .field_init && child.value !in ['init', 'len', 'cap'] {
			tc.record_error_at(.unknown_field,
				'wrong field `${child.value}`, expecting `len`, `cap`, or `init`', child_id,
				tc.node_value_diagnostic_pos(child_id))
		}
	}
	for field_name in ['init', 'len', 'cap'] {
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			child := tc.a.nodes[int(child_id)]
			if child.kind != .field_init || child.value != field_name {
				continue
			}
			if child.children_count == 0 {
				tc.check_node(child_id)
				continue
			}
			expr_id := tc.a.child(&child, 0)
			if field_name == 'init' {
				tc.reject_stored_method_value(expr_id)
				tc.reject_stored_capturing_fn_literal(expr_id)
				tc.push_scope()
				tc.cur_scope.insert('index', Type(int_))
				tc.check_node(child_id)
				tc.pop_scope()
			} else {
				tc.check_node(child_id)
			}
			if field_name == 'init' {
				if expected := init_elem_type {
					actual := tc.resolve_type(expr_id)
					clean_actual := unalias_type(actual)
					if actual !is Unknown && clean_actual !is OptionType
						&& clean_actual !is ResultType
						&& !tc.expr_subtree_has_undefined_variable_error(expr_id)
						&& !tc.expr_compatible(expr_id, actual, expected) {
						tc.record_error_at(.assignment_mismatch, 'expected `${expected.name()}`, not `${tc.diagnostic_expr_type_name(expr_id,
							actual)}`', expr_id, tc.a.node(expr_id).pos)
					}
				}
			}
			if field_name in ['len', 'cap'] {
				if value := tc.index_literal_value(expr_id) {
					if value < 0 {
						tc.record_error(.assignment_mismatch,
							'array ${field_name} can not be negative', expr_id)
					}
				}
			}
			if tc.expr_has_option_result_handler(expr_id) {
				continue
			}
			expr_type := unalias_type(tc.resolve_type(expr_id))
			wrapper := if expr_type is OptionType {
				'Option'
			} else if expr_type is ResultType {
				'Result'
			} else {
				continue
			}
			use_name := match field_name {
				'init' { 'initializer' }
				'len' { 'length' }
				else { 'capacity' }
			}
			tc.record_error(.assignment_mismatch, 'cannot use unwrapped ${wrapper} as ${use_name}',
				expr_id)
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.nodes[int(child_id)].kind != .field_init {
			tc.check_node(child_id)
		}
	}
}

fn (mut tc TypeChecker) discard_unknown_type_errors_inside_node(node flat.Node) {
	mut i := tc.errors.len
	for i > 0 {
		i--
		diagnostic := tc.errors[i]
		if diagnostic.kind == .unknown_type && diagnostic.msg.starts_with('unknown type `')
			&& diagnostic.pos.id == node.pos.id && diagnostic.pos.offset >= node.pos.offset
			&& diagnostic.pos.end <= node.pos.end {
			tc.errors.delete(i)
		}
	}
}

fn (tc &TypeChecker) array_init_head_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if start < end {
		if relative := source[start..end].index('{') {
			return token.new_span(node.pos.id, start, start + relative + 1)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) fixed_array_bound_type_diagnostic(bound string) ?string {
	if key := tc.const_key_for_name(bound) {
		typ := unalias_type(tc.const_type_for_name(bound) or { Type(Unknown{}) })
		if typ is Enum {
			expr_id := tc.const_exprs[key] or {
				return '${typ.name}.${bound} has to be casted to integer to be used as size'
			}
			expr := tc.source_text_for_node(expr_id)
			return '${expr} has to be casted to integer to be used as size'
		}
		if typ !is Unknown && !typ.is_integer() {
			return 'only integer types are allowed'
		}
	}
	if bound.contains('.') {
		enum_name := bound.all_before('.')
		if tc.resolve_enum_name(enum_name) != none {
			return '${bound} has to be casted to integer to be used as size'
		}
	}
	if open := bound.index('(') {
		cast_name := trimmed_space(bound[..open])
		cast_type := unalias_type(tc.parse_type(cast_name))
		if cast_type !is Unknown && !cast_type.is_integer() {
			return 'only integer types are allowed'
		}
	}
	return none
}

fn fixed_array_bound_text(typ string) ?string {
	clean := trimmed_space(typ)
	if clean.len < 3 || clean[0] != `[` {
		return none
	}
	close := find_matching_bracket(clean, 0)
	if close <= 1 || close >= clean.len {
		return none
	}
	bound := trimmed_space(clean[1..close])
	if bound.len == 0 {
		return none
	}
	return bound
}

fn (tc &TypeChecker) fixed_array_bound_pos(node flat.Node, bound string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if bracket := source[start..end].index('[') {
			bound_start := start + bracket + 1
			return token.new_span(node.pos.id, bound_start, bound_start + bound.len)
		}
	}
	return node.pos
}

fn (mut tc TypeChecker) check_array_elements_initialized(id flat.NodeId, node flat.Node) {
	if tc.unsafe_depth > 0 || tc.translated_files[tc.cur_file] {
		return
	}
	container_type := unalias_type(tc.resolve_type(id))
	mut elem_type := Type(Unknown{
		reason: 'not an array'
	})
	mut is_fixed := false
	match container_type {
		Array {
			elem_type = container_type.elem_type
		}
		ArrayFixed {
			elem_type = container_type.elem_type
			is_fixed = true
		}
		else {
			return
		}
	}
	mut has_len := false
	mut has_init := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .field_init {
			continue
		}
		if child.value == 'len' {
			has_len = true
		} else if child.value == 'init' {
			has_init = true
		}
	}
	if !is_fixed && (!has_len || has_init) {
		return
	}
	mut seen := map[string]bool{}
	mut category := tc.array_uninitialized_element_category(elem_type, mut seen)
	if category.len == 0 && node.typ.contains('shared ') {
		category = 'references'
	}
	if category.len == 0 {
		return
	}
	pos := if is_fixed {
		node.pos
	} else {
		tc.array_init_type_prefix_pos(node)
	}
	message := if is_fixed {
		'fixed arrays of ${category} need to be initialized right away (unless inside `unsafe`)'
	} else {
		'arrays of ${category} need to be initialized right away, therefore `len:` cannot be used (unless inside `unsafe`, or if you also use `init:`)'
	}
	tc.record_warning_at(.assignment_mismatch, message, id, pos)
}

fn (tc &TypeChecker) node_is_from_translated_file(node flat.Node) bool {
	file := tc.a.source_files[node.pos.id] or { return false }
	return tc.translated_files[file.name]
}

fn (tc &TypeChecker) node_is_in_translated_file(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	return tc.node_is_from_translated_file(tc.a.node(id))
}

fn (tc &TypeChecker) array_uninitialized_element_category(typ Type, mut seen map[string]bool) string {
	type_name := typ.name()
	if type_name in seen {
		return ''
	}
	seen[type_name] = true
	return match typ {
		Alias {
			tc.array_uninitialized_element_category(typ.base_type, mut seen)
		}
		Pointer {
			'references'
		}
		Interface {
			'interfaces'
		}
		SumType {
			'sumtypes'
		}
		Array {
			tc.array_uninitialized_element_category(typ.elem_type, mut seen)
		}
		ArrayFixed {
			tc.array_uninitialized_element_category(typ.elem_type, mut seen)
		}
		Map {
			key_category := tc.array_uninitialized_element_category(typ.key_type, mut seen)
			if key_category.len > 0 {
				key_category
			} else {
				tc.array_uninitialized_element_category(typ.value_type, mut seen)
			}
		}
		else {
			''
		}
	}
}

fn (mut tc TypeChecker) check_array_element_reference_fields(id flat.NodeId, node flat.Node) {
	container_type := unalias_type(tc.resolve_type(id))
	mut elem_type := Type(Unknown{
		reason: 'not an array'
	})
	mut is_fixed := false
	match container_type {
		Array {
			elem_type = container_type.elem_type
		}
		ArrayFixed {
			elem_type = container_type.elem_type
			is_fixed = true
		}
		else {
			return
		}
	}
	if !is_fixed {
		mut has_len := false
		for i in 0 .. node.children_count {
			child := tc.a.child_node(&node, i)
			if child.kind == .field_init && child.value == 'len' {
				has_len = true
				break
			}
		}
		if !has_len {
			return
		}
	}
	clean_elem := unalias_type(elem_type)
	if clean_elem !is Struct {
		return
	}
	struct_elem := clean_elem as Struct
	pos := if is_fixed {
		node.pos
	} else {
		tc.array_init_type_prefix_pos(node)
	}
	mut checked := map[string]bool{}
	tc.record_uninitialized_reference_field_notices(struct_elem, struct_elem.name, id, pos, mut
		checked)
}

fn (tc &TypeChecker) array_init_type_prefix_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(source.len, node.pos.end)
	if start < end {
		if brace := source[start..end].index('{') {
			return token.new_span(node.pos.id, start, start + brace + 1)
		}
	}
	return node.pos
}

fn (mut tc TypeChecker) record_uninitialized_reference_field_notices(struct_type Struct, linked_name string,
	node_id flat.NodeId, pos token.Pos, mut checked map[string]bool) {
	if struct_type.name in checked {
		return
	}
	checked[struct_type.name] = true
	for field in tc.struct_fields_for_init(struct_type.name) {
		if field.has_default {
			continue
		}
		if field.typ is OptionType {
			continue
		}
		field_type := unalias_type(field.typ)
		if field_type is Pointer {
			tc.record_notice_at(.assignment_mismatch,
				'reference field `${linked_name}.${field.name}` must be initialized (part of struct `${struct_type.name}`)',
				node_id, pos)
			continue
		}
		if field_type is Struct && !field.is_embed {
			tc.record_uninitialized_reference_field_notices(field_type,
				'${linked_name}.${field.name}', node_id, pos, mut checked)
		}
	}
}

// check_or_expr validates check or expr state for types.
fn (mut tc TypeChecker) check_or_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	inner_id := tc.a.child(&node, 0)
	tc.check_node(inner_id)
	if node.children_count >= 2 {
		inner := tc.a.node(inner_id)
		if inner.kind == .call && inner.children_count > 0 {
			callee := tc.a.child_node(inner, 0)
			if callee.kind == .selector && callee.children_count > 0
				&& unalias_type(tc.resolve_type(tc.a.child(callee, 0))) is ResultType {
				fallback_id := tc.a.child(&node, 1)
				tc.record_error_at(.assignment_mismatch,
					'unexpected `or` block, the function `${callee.value}` does not return an Option or a Result',
					id, tc.or_block_operator_pos(inner_id, fallback_id))
				return
			}
		}
	}
	if node.children_count >= 2 && node.value !in ['!', '?']
		&& int(id) != tc.channel_send_or_expr_id && !tc.or_expr_source_can_fail(inner_id) {
		mut source_type := unalias_type(tc.resolve_type(inner_id))
		if tc.a.node(inner_id).kind == .none_expr {
			if enclosing_type := tc.enclosing_infix_type_for_or_expr(id) {
				source_type = unalias_type(enclosing_type)
			}
		}
		inner := tc.a.node(inner_id)
		unresolved_method_call := inner.kind == .call && inner.children_count > 0
			&& tc.a.child_node(inner, 0).kind == .selector
		if (source_type !is Unknown || unresolved_method_call) && tc.should_diagnose(inner_id) {
			if inner.kind == .call {
				fallback_id := tc.a.child(&node, 1)
				name := tc.or_block_call_display_name(inner)
				tc.record_error_at(.assignment_mismatch,
					'unexpected `or` block, the function `${name}` does not return an Option or a Result',
					id, tc.or_block_operator_pos(inner_id, fallback_id))
				return
			} else if inner.kind == .ident {
				if smart_type := tc.smartcast_type(inner_id) {
					tc.record_error(.assignment_mismatch,
						'cannot use `or {}` block on non-option variable `${inner.value}` (it was already unwrapped to `${smart_type.name()}` by an earlier none check; use `${inner.value}` directly)',
						inner_id)
				} else {
					tc.record_error(.assignment_mismatch,
						'cannot use `or {}` block on non-option variable', inner_id)
				}
			} else {
				tc.record_error(.assignment_mismatch,
					'unexpected `or` block, expression of type `${source_type.name()}` is not an Option or a Result',
					inner_id)
			}
		}
	}
	$if ownership ? {
		if node.value in ['!', '?'] {
			tc.ownership_record_propagation_drops()
		}
	}
	if node.value == '?' {
		tc.check_option_propagation(id, inner_id)
		return
	}
	if node.value == '!' {
		tc.check_result_propagation(id, inner_id)
		return
	}
	if node.children_count < 2 || node.value in ['!', '?'] {
		return
	}
	unsafe_alias_success := tc.fn_context.unsafe_reference_alias_owners.clone()
	$if ownership ? {
		tc.ownership_begin_value_branch_group()
		tc.ownership_begin_branch()
	}
	fallback_id := tc.a.child(&node, 1)
	tc.push_scope()
	tc.cur_scope.insert('err', tc.parse_type('IError'))
	saved_expected_expr_id := tc.expected_expr_id
	saved_expected_expr_type := tc.expected_expr_type
	if payload := tc.or_expr_payload_type(inner_id) {
		tc.expected_expr_id = int(fallback_id)
		tc.expected_expr_type = payload
	}
	require_fallback_value := !tc.call_has_argument_count_error(inner_id)
	tc.check_or_fallback_branch_node(fallback_id, require_fallback_value)
	tc.expected_expr_id = saved_expected_expr_id
	tc.expected_expr_type = saved_expected_expr_type
	tc.pop_scope()
	mut unsafe_alias_paths := [unsafe_alias_success]
	if !tc.stmt_definitely_returns(fallback_id) {
		unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_success)
	inner := tc.a.node(inner_id)
	if inner.kind == .selector {
		if source := tc.selector_declared_value_type(*inner) {
			if source is OptionType {
				tail_id := tc.branch_tail_expr_id(fallback_id)
				if tc.valid_node_id(tail_id) {
					tail := tc.a.node(tail_id)
					if tail.kind in [.assign, .selector_assign, .index_assign]
						&& tail.children_count >= 2 {
						value_id := tc.a.child(tail, 0)
						rhs_id := tc.a.child(tail, tail.children_count - 1)
						tc.record_error_at(.assignment_mismatch,
							'last statement in the `or {}` block should be an expression of type `${source.base_type.name()}` or exit parent scope',
							value_id, tc.assignment_operator_pos(*tail, value_id, rhs_id))
						return
					}
				}
			}
		}
	}
	tc.check_or_fallback_type(id, inner_id, fallback_id)
	$if ownership ? {
		tc.ownership_end_branch(fallback_id)
		tc.ownership_add_branch_group_base()
		tc.ownership_end_branch_group()
	}
}

fn (tc &TypeChecker) or_block_call_display_name(call &flat.Node) string {
	if call.children_count > 0 {
		callee := tc.a.child_node(call, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := tc.a.child_node(callee, 0)
			if base.kind == .ident && base.value.len > 0 && base.value[0].is_capital()
				&& tc.resolve_enum_name(base.value) == none
				&& !tc.ident_resolves_to_value(base.value) {
				return '${base.value}__static__${callee.value}'
			}
		}
	}
	return tc.call_display_name(*call).all_after_last('.')
}

fn (mut tc TypeChecker) check_option_propagation(id flat.NodeId, source_id flat.NodeId) {
	source := tc.a.node(source_id)
	source_type := tc.resolve_type(source_id)
	if source_type is ResultType {
		source_text := tc.source_text_for_node(source_id)
		tc.record_error_at(.return_mismatch,
			'propagating a Result like an Option is deprecated, use `${source_text}!` instead of `${source_text}?`',
			id, tc.propagation_operator_pos(source_id, id, '?'))
		return
	}
	main_allows_propagation := tc.current_fn_is_main()
	propagation_parent_id := tc.direct_parent_id(id)
	if source.kind == .call && source_type is OptionType {
		if tc.valid_node_id(propagation_parent_id)
			&& tc.a.node(propagation_parent_id).kind == .return_stmt
			&& tc.fn_context.return_type is OptionType
			&& tc.return_type_compatible(source_id, source_type, tc.fn_context.return_type) {
			tc.record_error_at(.return_mismatch,
				'`?` is not needed, use `return ${tc.call_display_name(*source)}()`', source_id,
				source.pos)
			return
		}
		if tc.fn_context.return_type !is OptionType && !main_allows_propagation {
			tc.record_nested_propagation_return_errors(id, source_id, 'Option', '?')
		}
		return
	}
	if source.kind == .call && source_type !is OptionType && source_type !is Unknown {
		name := tc.call_display_name(*source).all_after_last('.')
		tc.record_error_at(.assignment_mismatch,
			'unexpected `?`, the function `${name}` does not return an Option', id, tc.propagation_operator_pos(source_id,
			id, '?'))
		return
	}
	if source.kind == .ident {
		if smart_type := tc.smartcast_type(source_id) {
			parent_id := tc.direct_parent_id(id)
			if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .return_stmt
				&& tc.fn_context.return_type is OptionType {
				parent := tc.a.node(parent_id)
				operator_pos := tc.propagation_operator_pos(source_id, id, '?')
				return_line := tc.previous_source_line_matching(parent.pos, 'return')
				tc.record_warning_at(.return_mismatch,
					'unwrapping option is redundant as the function returns option', parent_id, token.new_span(return_line.id,
					return_line.offset, operator_pos.offset))
			}
			tc.record_error_at(.assignment_mismatch,
				'cannot use `?` on non-option variable `${source.value}` (it was already unwrapped to `${smart_type.name()}` by an earlier none check; use `${source.value}` directly)',
				source_id, tc.node_value_diagnostic_pos(source_id))
			return
		}
		if source_type is OptionType {
			parent_id := tc.direct_parent_id(id)
			if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .return_stmt
				&& tc.fn_context.return_type !is OptionType && !main_allows_propagation {
				fn_id := flat.NodeId(tc.fn_context.node_id)
				if tc.valid_node_id(fn_id) {
					fn_node := tc.a.node(fn_id)
					fn_name := fn_node.value.all_after_last('.')
					return_pos := tc.fn_return_type_diagnostic_pos(*fn_node)
					detail := v3errors.formatted_source_error('details:',
						'prepend ? before the declaration of the return type of `${fn_name}`', tc.a.source_files[return_pos.id] or {
						return
					}, return_pos)
					tc.record_error_with_details_at(.return_mismatch,
						'to propagate the Option, `${fn_name}` must return an Option type', id,
						tc.node_value_diagnostic_pos(source_id), [detail])
				}
			}
			return
		}
		if source_type !is ResultType {
			tc.record_error_at(.assignment_mismatch, 'cannot use `?` on non-option variable',
				source_id, tc.wrapped_operand_diagnostic_pos(source_id))
			return
		}
	}
	if source.kind == .selector && source_type !is OptionType {
		// Selectors expanded from `$for field in T.fields` are synthetic. Cgen reports
		// the propagation error for their concrete field after comptime substitution.
		if !source.pos.is_valid() {
			return
		}
		tc.record_error_at(.assignment_mismatch,
			'unexpected `?`, the field `${source.value}` is not an Option', id, tc.propagation_operator_pos(source_id,
			id, '?'))
		return
	}
	if source.kind != .selector || source_type !is OptionType
		|| tc.fn_context.return_type is OptionType || main_allows_propagation {
		return
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return
	}
	fn_node := tc.a.node(fn_id)
	fn_name := fn_node.value.all_after_last('.')
	return_pos := tc.fn_return_type_diagnostic_pos(*fn_node)
	detail := v3errors.formatted_source_error('details:',
		'prepend ? before the declaration of the return type of `${fn_name}`', tc.a.source_files[return_pos.id] or {
		return
	}, return_pos)
	tc.record_error_with_details_at(.return_mismatch,
		'to propagate the call, `${fn_name}` must return an Option type', id, tc.propagation_operator_pos(source_id,
		id, '?'), [detail])
}

fn (mut tc TypeChecker) check_result_propagation(id flat.NodeId, source_id flat.NodeId) {
	source := tc.a.node(source_id)
	source_type := tc.resolve_type(source_id)
	if source_type is OptionType {
		tc.record_error_at(.return_mismatch,
			'to propagate a Result, the call must also return a Result type', id, tc.propagation_operator_pos(source_id,
			id, '!'))
		return
	}
	if source.kind == .call && tc.call_display_name(*source) == 'json.decode'
		&& source.children_count < 3 {
		tc.record_error_at(.return_mismatch,
			'unexpected `!`, the function `json.decode` does not return a Result', id, tc.propagation_operator_pos(source_id,
			id, '!'))
		return
	}
	if source.kind == .call && source_type !is ResultType && source_type !is Unknown {
		if tc.fn_context.return_type !is ResultType && !tc.current_fn_is_main() {
			tc.record_specific_propagation_return_error(id, source_id, 'Result', '!')
		}
		tc.record_error_at(.return_mismatch,
			'unexpected `!`, the function `${tc.call_display_name(*source)}` does not return a Result',
			id, tc.propagation_operator_pos(source_id, id, '!'))
		return
	}
	if source.kind == .call && source_type is ResultType && tc.fn_context.return_type !is ResultType
		&& !tc.current_fn_is_main() {
		tc.record_nested_propagation_return_errors(id, source_id, 'Result', '!')
	}
}

fn (tc &TypeChecker) current_fn_is_main() bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	node := tc.a.node(fn_id)
	return node.kind == .fn_decl && node.value.all_after_last('.') == 'main'
}

fn (mut tc TypeChecker) record_nested_propagation_return_errors(id flat.NodeId, source_id flat.NodeId, wrapper string, marker string) {
	tc.record_specific_propagation_return_error(id, source_id, wrapper, marker)
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return
	}
	fn_node := tc.a.node(fn_id)
	fn_name := fn_node.value.all_after_last('.')
	return_pos := tc.fn_return_type_diagnostic_pos(*fn_node)
	detail := v3errors.formatted_source_error('details:',
		'prepend ${marker} before the declaration of the return type of `${fn_name}`', tc.a.source_files[return_pos.id] or {
		return
	}, return_pos)
	operator_pos := tc.propagation_operator_pos(source_id, id, marker)
	article := if wrapper == 'Option' { 'an' } else { 'a' }
	tc.record_error_with_details_at(.return_mismatch,
		'to propagate the call, `${fn_name}` must return ${article} ${wrapper} type', id,
		operator_pos, [
		detail,
	])
}

fn (mut tc TypeChecker) record_specific_propagation_return_error(id flat.NodeId, source_id flat.NodeId, wrapper string, marker string) {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return
	}
	fn_node := tc.a.node(fn_id)
	fn_name := fn_node.value.all_after_last('.')
	return_pos := tc.fn_return_type_diagnostic_pos(*fn_node)
	detail := v3errors.formatted_source_error('details:',
		'prepend ${marker} before the declaration of the return type of `${fn_name}`', tc.a.source_files[return_pos.id] or {
		return
	}, return_pos)
	operator_pos := tc.propagation_operator_pos(source_id, id, marker)
	article := if wrapper == 'Option' { 'an' } else { 'a' }
	tc.record_error_with_details_at(.return_mismatch,
		'to propagate the ${wrapper} call, `${fn_name}` must return ${article} ${wrapper}', id,
		operator_pos, [detail])
}

fn (tc &TypeChecker) fn_return_type_diagnostic_pos(node flat.Node) token.Pos {
	header_pos := tc.fn_declaration_diagnostic_pos(node)
	file := tc.a.source_files[header_pos.id] or { return header_pos }
	source := tc.source_texts_by_file[file.name] or { return header_pos }
	if header_pos.offset < 0 || header_pos.end > source.len || header_pos.end <= header_pos.offset {
		return header_pos
	}
	header := source[header_pos.offset..header_pos.end]
	if node.typ == 'void' {
		line_start := file.line_start(file.position(header_pos).line)
		line_end := source.index_after('\n', line_start) or { source.len }
		if relative := source[line_start..line_end].last_index('{') {
			start := line_start + relative
			return token.new_span(header_pos.id, start, start + 1)
		}
	}
	if relative := header.last_index(node.typ) {
		start := header_pos.offset + relative
		return token.new_span(header_pos.id, start, start + node.typ.len)
	}
	return header_pos
}

fn (tc &TypeChecker) option_void_payload_diagnostic_pos(node flat.Node) token.Pos {
	return_type := tc.fn_return_type_diagnostic_pos(node)
	if return_type.end - return_type.offset > 1 {
		return token.new_span(return_type.id, return_type.offset + 1, return_type.end)
	}
	return return_type
}

fn (tc &TypeChecker) suffix_option_return_type_diagnostic_pos(node flat.Node) token.Pos {
	return_type := tc.fn_return_type_diagnostic_pos(node)
	if return_type.end - return_type.offset > 1 {
		return token.new_span(return_type.id, return_type.offset, return_type.end - 1)
	}
	return return_type
}

fn (tc &TypeChecker) nested_option_result_marker_pos(node flat.Node) token.Pos {
	return_type := tc.fn_return_type_diagnostic_pos(node)
	if return_type.end - return_type.offset >= 2 {
		return token.new_span(return_type.id, return_type.offset + 1, return_type.offset + 2)
	}
	return return_type
}

fn (tc &TypeChecker) propagation_operator_pos(source_id flat.NodeId, expr_id flat.NodeId, op string) token.Pos {
	source_node := tc.a.node(source_id)
	expr := tc.a.node(expr_id)
	file := tc.a.source_files[source_node.pos.id] or { return expr.pos }
	text := tc.source_texts_by_file[file.name] or { return expr.pos }
	start := int_min(int_max(source_node.pos.end, 0), text.len)
	end := int_min(text.len, int_max(expr.pos.end, start + op.len))
	if start < end {
		if relative := text[start..end].index(op) {
			op_start := start + relative
			return token.new_span(source_node.pos.id, op_start, op_start + op.len)
		}
	}
	return expr.pos
}

fn (mut tc TypeChecker) check_or_fallback_type(or_id flat.NodeId, source_id flat.NodeId, fallback_id flat.NodeId) {
	if !tc.should_diagnose(or_id) {
		return
	}
	expected := tc.or_expr_payload_type(source_id) or { return }
	fallback := tc.a.node(fallback_id)
	if fallback.kind == .block && fallback.children_count == 0 {
		parent_id := tc.direct_parent_id(or_id)
		if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .expr_stmt
			&& tc.is_statement_node(parent_id) {
			return
		}
		tc.record_error_at(.assignment_mismatch, 'expression requires a non empty `or {}` block',
			or_id, tc.or_block_operator_pos(source_id, fallback_id))
		return
	}
	tail_id := tc.branch_tail_expr_id(fallback_id)
	if !tc.valid_node_id(tail_id) {
		return
	}
	tail := tc.a.node(tail_id)
	if tail.kind in [.return_stmt, .break_stmt, .continue_stmt] || tc.expr_never_returns(tail_id) {
		return
	}
	if tail.kind == .assert_stmt && tc.direct_parent_kind(or_id) != .expr_stmt {
		cond_id := if tail.children_count > 0 { tc.a.child(tail, 0) } else { tail_id }
		tc.record_error_at(.assignment_mismatch,
			'last statement in the `or {}` block should be an expression of type `${expected.name()}` or exit parent scope',
			cond_id, token.new_span(tc.a.node(cond_id).pos.id, tc.a.node(cond_id).pos.offset,
			fallback.pos.end))
		return
	}
	if tail.kind == .block && tail.value == 'unsafe' && expected is Pointer
		&& tc.node_contains_nil_literal(tail_id) {
		return
	}
	if tail.kind == .block && expected !is MultiReturn {
		tc.record_error_at(.assignment_mismatch,
			'last statement in the `or {}` block should be an expression of type `${expected.name()}` or exit parent scope',
			tail_id, token.new_span(tail.pos.id, tail.pos.offset, tail.pos.offset + 1))
		return
	}
	if expected is MultiReturn {
		if groups := tc.tuple_tail_value_groups(fallback_id, expected.types.len, false) {
			if groups.len > 0 {
				values := groups[0]
				mut actual_types := []Type{cap: values.len}
				mut compatible := values.len == expected.types.len
				for i, value_id in values {
					actual_type := tc.resolve_type(value_id)
					actual_types << actual_type
					if i >= expected.types.len
						|| !tc.type_compatible(actual_type, expected.types[i]) {
						compatible = false
					}
				}
				if !compatible {
					actual_multi := Type(MultiReturn{
						types: actual_types
					})
					tc.record_error_at(.assignment_mismatch,
						'wrong return type `${actual_multi.name()}` in the `or {}` block, expected `${Type(expected).name()}`',
						values[0], tc.a.node(values[0]).pos)
					return
				}
			}
		}
	}
	mut actual := tc.resolve_type(tail_id)
	if tail.kind == .infix && tail.op == .plus && tail.children_count >= 2 {
		lhs_type := unalias_type(tc.resolve_type(tc.a.child(tail, 0)))
		rhs_type := unalias_type(tc.resolve_type(tc.a.child(tail, 1)))
		if lhs_type is OptionType && !tc.type_compatible(rhs_type, lhs_type.base_type) {
			actual = Type(void_)
		} else if rhs_type is OptionType && !tc.type_compatible(lhs_type, rhs_type.base_type) {
			actual = Type(void_)
		}
	}
	if tc.expr_is_empty_bare_array_literal(tail_id) && array_like_elem_type(expected) != none {
		return
	}
	if expected is Void {
		return
	}
	if tail.kind == .none_expr {
		mut context := tc.expected_context_for_expr(or_id) or { Type(void_) }
		if context is Void && tc.direct_parent_kind(or_id) == .return_stmt {
			context = tc.fn_context.return_type
		}
		clean_context := unalias_type(context)
		if clean_context is OptionType && tc.type_compatible(expected, clean_context.base_type) {
			return
		}
	}
	if tc.or_expr_payload_is_shared(source_id) && !tc.expr_is_explicit_shared_arg(tail_id) {
		actual_name := tc.diagnostic_expr_type_name(tail_id, actual)
		tc.record_error_at(.assignment_mismatch,
			'wrong return type `${actual_name}` in the `or {}` block, expected `shared ${expected.name()}`',
			tail_id, tc.or_fallback_value_pos(tail_id, tail))
		return
	}
	is_strict_numeric_mismatch := actual.is_integer() && expected.is_float()
		&& tail.kind != .int_literal
	if tc.expr_compatible(tail_id, actual, expected) && !is_strict_numeric_mismatch {
		return
	}
	actual_name := if tail.kind == .none_expr {
		'none'
	} else {
		tc.diagnostic_expr_type_name(tail_id, actual)
	}
	expected_name := expected.name()
	parent_kind := tc.direct_parent_kind(or_id)
	source := tc.a.node(source_id)
	if actual is Void && parent_kind == .expr_stmt {
		return
	}
	message := if actual is Void {
		'`or` block must provide a default value of type `${expected_name}`, or return/continue/break or call a @[noreturn] function like panic(err) or exit(1)'
	} else if tail.kind == .none_expr || actual is OptionType || source.kind == .index
		|| parent_kind == .return_stmt {
		'`or` block must provide a value of type `${expected_name}`, not `${actual_name}`'
	} else if parent_kind == .expr_stmt {
		'the default expression type in the `or` block should be `${expected_name}`, instead you gave a value of type `${actual_name}`'
	} else {
		'wrong return type `${actual_name}` in the `or {}` block, expected `${expected_name}`'
	}
	pos := tc.or_fallback_value_pos(tail_id, tail)
	tc.record_error_at(.assignment_mismatch, message, tail_id, pos)
}

fn (tc &TypeChecker) or_expr_payload_is_shared(source_id flat.NodeId) bool {
	source := tc.a.node(source_id)
	if source.kind != .call {
		return false
	}
	name := tc.resolved_call_name(source_id) or { return false }
	raw := trimmed_space(tc.fn_ret_type_texts[name] or { return false })
	payload := if raw.starts_with('?') || raw.starts_with('!') {
		trimmed_space(raw[1..])
	} else {
		raw
	}
	return payload.starts_with('shared ')
}

fn (tc &TypeChecker) or_block_operator_pos(source_id flat.NodeId, fallback_id flat.NodeId) token.Pos {
	source_node := tc.a.node(source_id)
	fallback := tc.a.node(fallback_id)
	file := tc.a.source_files[source_node.pos.id] or { return fallback.pos }
	source := tc.source_texts_by_file[file.name] or { return fallback.pos }
	if source_node.kind == .sql_expr {
		anchor := int_min(int_max(fallback.pos.offset, 0), source.len)
		line_start := if relative := source[..anchor].last_index('\n') {
			relative + 1
		} else {
			0
		}
		line_end := source.index_after('\n', anchor) or { source.len }
		if relative := source[line_start..line_end].index('or {') {
			start := line_start + relative
			return token.new_span(source_node.pos.id, start, start + 4)
		}
	}
	start := int_min(int_max(source_node.pos.end, 0), source.len)
	end := int_min(source.len, int_max(fallback.pos.end, start))
	if start < end {
		if relative := source[start..end].index('or') {
			op_start := start + relative
			if open_relative := source[op_start..end].index('{') {
				mut depth := 0
				for cursor in op_start + open_relative .. end {
					if source[cursor] == `{` {
						depth++
					} else if source[cursor] == `}` {
						depth--
						if depth == 0 {
							return token.new_span(source_node.pos.id, op_start, cursor + 1)
						}
					}
				}
			}
		}
	}
	return fallback.pos
}

fn (tc &TypeChecker) or_fallback_value_pos(id flat.NodeId, node flat.Node) token.Pos {
	if node.kind == .selector {
		return tc.node_value_diagnostic_pos(id)
	}
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(&node, 0)
		if callee.kind == .selector {
			name_pos := tc.method_call_name_pos(node, callee)
			return token.new_span(name_pos.id, name_pos.offset, node.pos.end)
		}
	}
	if node.kind == .cast_expr && node.children_count > 0 {
		child := tc.a.child_node(&node, 0)
		file := tc.a.source_files[node.pos.id] or { return node.pos }
		source := tc.source_texts_by_file[file.name] or { return node.pos }
		child_start := int_min(int_max(child.pos.offset, 0), source.len)
		search_start := int_max(0, child_start - node.value.len - 2)
		if relative := source[search_start..child_start].last_index('${node.value}(') {
			mut start := search_start + relative
			if node.value.starts_with('?') || node.value.starts_with('!') {
				start++
			}
			mut end := int_min(source.len, int_max(child.pos.end, child_start))
			for end < source.len && source[end] in [` `, `\t`] {
				end++
			}
			if end < source.len && source[end] == `)` {
				end++
			}
			return token.new_span(node.pos.id, start, end)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) or_expr_payload_type(source_id flat.NodeId) ?Type {
	source := tc.a.node(source_id)
	source_type := unalias_type(tc.resolve_type(source_id))
	if source_type is OptionType {
		return source_type.base_type
	}
	if source_type is ResultType {
		return source_type.base_type
	}
	if source.kind == .selector {
		if declared := tc.selector_declared_value_type(*source) {
			clean_declared := unalias_type(declared)
			if clean_declared is OptionType {
				return clean_declared.base_type
			}
			if clean_declared is ResultType {
				return clean_declared.base_type
			}
		}
	}
	if source.kind == .index && source.children_count > 0 {
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(source, 0)))
		if source.value == 'range' {
			// A gated range index yields a slice of the container, not an element.
			if base_type is ArrayFixed {
				return Type(Array{
					elem_type: base_type.elem_type
				})
			}
			if base_type is Array || base_type is String {
				return base_type
			}
			return none
		}
		if base_type is Map {
			return base_type.value_type
		}
		if base_type is Array {
			return base_type.elem_type
		}
		if base_type is ArrayFixed {
			return base_type.elem_type
		}
		if base_type is String {
			return Type(u8_)
		}
	}
	return none
}

fn (tc &TypeChecker) direct_parent_kind(id flat.NodeId) flat.NodeKind {
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) {
		return tc.a.node(parent_id).kind
	}
	return .empty
}

fn (tc &TypeChecker) direct_parent_id(id flat.NodeId) flat.NodeId {
	idx := int(id)
	if idx >= 0 && idx < tc.direct_parent_ids.len {
		parent_id := tc.direct_parent_ids[idx]
		if tc.direct_parent_index_trusted {
			return parent_id
		}
		if parent_id != flat.empty_node {
			parent := tc.a.node(parent_id)
			for i in 0 .. parent.children_count {
				if tc.a.child(parent, i) == id {
					return parent_id
				}
			}
		}
	}
	for parent_idx, candidate in tc.a.nodes {
		for i in 0 .. candidate.children_count {
			if tc.a.child(&candidate, i) == id {
				return flat.NodeId(parent_idx)
			}
		}
	}
	return flat.empty_node
}

fn (tc &TypeChecker) enclosing_infix_type_for_or_expr(id flat.NodeId) ?Type {
	for idx, candidate in tc.a.nodes {
		if candidate.kind != .infix {
			continue
		}
		for i in 0 .. candidate.children_count {
			if tc.a.child(&candidate, i) == id {
				return tc.resolve_type(flat.NodeId(idx))
			}
		}
	}
	return none
}

fn (tc &TypeChecker) or_expr_source_can_fail(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt, .or_expr] && node.children_count > 0 {
		return tc.or_expr_source_can_fail(tc.a.child(node, 0))
	}
	if node.kind == .or_expr && node.value in ['!', '?'] && node.children_count > 0 {
		return tc.or_expr_source_can_fail(tc.a.child(node, 0))
	}
	// `none` can adopt an optional context, but it is a value, not an
	// expression whose evaluation can fail.
	if node.kind == .none_expr {
		return false
	}
	if node.kind == .selector {
		if declared := tc.selector_declared_value_type(*node) {
			if type_is_option_or_result(declared) {
				return true
			}
		}
	}
	if node.kind == .infix {
		for i in 0 .. node.children_count {
			if tc.or_expr_source_can_fail(tc.a.child(node, i)) {
				return true
			}
		}
	}
	if node.kind == .index && node.children_count > 0 {
		if node.op == .gated_index {
			return true
		}
		index_type := tc.resolve_type(id)
		if type_is_option_or_result(index_type) || index_type.name().starts_with('?')
			|| index_type.name().starts_with('!') {
			return true
		}
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(node, 0)))
		return base_type is Map || base_type is Array || base_type is ArrayFixed
			|| base_type is String
	}
	if node.kind == .prefix && node.op == .arrow && node.children_count > 0 {
		source_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(node, 0)))
		return source_type is Channel
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		index := tc.a.child_node(node, 0)
		if index.kind == .index && index.children_count > 0 {
			base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(index, 0)))
			return base_type is Map
		}
	}
	typ := tc.resolve_type(id)
	if type_is_option_or_result(typ) {
		return true
	}
	// Thread payloads are retained in the synthetic `thread T` handle name.
	// When T is optional/result, indexing the joined array can still carry that
	// wrapper spelling even if its semantic type has not been reconstructed yet.
	name := typ.name()
	return name.starts_with('?') || name.starts_with('!')
}

fn type_is_option_or_result(typ Type) bool {
	clean := unalias_type(typ)
	return clean is OptionType || clean is ResultType
}

fn (mut tc TypeChecker) check_sql_expr(id flat.NodeId, node flat.Node) {
	if node.children_count > 0 {
		db_id := tc.a.child(&node, 0)
		tc.check_node(db_id)
		db_type := unalias_type(unwrap_pointer(tc.resolve_type(db_id)))
		if db_type is OptionType {
			tc.record_error_at(.call_arg_mismatch,
				'expected `${db_type.base_type.name()}`, not `?${db_type.base_type.name()}`',
				db_id, tc.a.node(db_id).pos)
		} else if db_type is ResultType {
			tc.record_error_at(.call_arg_mismatch,
				'expected `${db_type.base_type.name()}`, not `!${db_type.base_type.name()}`',
				db_id, tc.a.node(db_id).pos)
		} else {
			db_name := db_type.name()
			if db_name.len > 0 && db_type !is Unknown
				&& !tc.named_type_implements_interface(db_name, 'orm.Connection') {
				for method in tc.interface_abstract_method_names('orm.Connection') {
					tc.record_error_at(.call_arg_mismatch,
						'`${db_name}` doesn\'t implement method `${method}` of interface `orm.Connection`',
						db_id, tc.a.node(db_id).pos)
				}
			}
		}
	}
	tokens := node.value.split(' ')
	tc.check_sql_orm_constraints(id, node, tokens)
	if tokens.len > 0 && tokens[0] == 'dynamic'
		&& tc.check_dynamic_sql_query_data_fields(id, tokens) {
		return
	}
	tc.check_sql_anonymous_struct_fields(tokens)
	tc.check_sql_like_operands(id, node, tokens)
	for i in 0 .. tokens.len {
		op := tokens[i]
		if op !in ['==', '!=', '<', '>', '<=', '>='] || i + 1 >= tokens.len {
			continue
		}
		rhs := tokens[i + 1]
		if rhs == 'none' {
			tc.record_warning_at(.condition_mismatch,
				'comparison with none probably isn\'t intended; use "is none" and "!is none" to select by NULL',
				id, tc.sql_expr_text_pos(node, ' ${op} ', 1, op.len))
			continue
		}
		if rhs_type := tc.cur_scope.lookup(rhs) {
			if unalias_type(rhs_type) is OptionType {
				tc.record_warning_at(.condition_mismatch,
					'comparison with Option value probably isn\'t intended; use "is none" and "!is none" to select by NULL',
					id, tc.sql_expr_text_pos(node, ' ${op} ', 1, op.len))
			}
		}
	}
	for i := 0; i + 3 < tokens.len; i++ {
		if tokens[i + 1] != '.' || tokens[i + 3] !in ['==', '!=', '<', '>', '<=', '>='] {
			continue
		}
		base_name := tokens[i]
		field_name := tokens[i + 2]
		base_type := tc.cur_scope.lookup(base_name) or { continue }
		field_type := tc.sql_struct_field_type(base_type, field_name) or { continue }
		clean_field := unalias_type(field_type)
		if clean_field is OptionType {
			tc.record_error_at(.assignment_mismatch,
				'`?${clean_field.base_type.name()}` cannot be used as `${clean_field.base_type.name()}`, unwrap the option first',
				id, tc.sql_expr_text_pos(node, '${base_name}.${field_name}', base_name.len + 1,
				field_name.len))
		}
	}
}

fn (mut tc TypeChecker) check_sql_orm_constraints(id flat.NodeId, node flat.Node, tokens []string) {
	tc.check_sql_aggregate_constraints(id, node, tokens)
	tc.check_sql_bulk_pointer_arrays(id, node, tokens)
	tc.check_sql_statement_constraints(id, node, tokens)
	for table_name in sql_orm_table_names(tokens) {
		tc.check_sql_orm_table_kind(id, node, table_name)
		tc.check_sql_orm_struct_constraints(id, table_name)
	}
	for i := 0; i + 2 < tokens.len; i++ {
		if tokens[i] != 'select' || tokens[i + 1] != 'from' {
			continue
		}
		table_name := tokens[i + 2]
		decl := tc.source_struct_decl_for_name(table_name) or { continue }
		mut field_count := 0
		for j in 0 .. decl.children_count {
			if tc.a.child_node(&decl, j).kind == .field_decl {
				field_count++
			}
		}
		if field_count == 0 {
			tc.record_error_at(.assignment_mismatch,
				'ORM: select: empty fields in `${table_name}`', id, tc.sql_expr_text_pos(node,
				' ${table_name}', 1, table_name.len))
		}
	}
}

fn (mut tc TypeChecker) check_sql_orm_table_kind(id flat.NodeId, node flat.Node, table_name string) {
	if tc.source_struct_decl_for_name(table_name) != none {
		return
	}
	table_type := unalias_type(tc.parse_type(table_name))
	if table_type is Unknown || table_type is Struct {
		return
	}
	tc.record_sql_error_at(.assignment_mismatch,
		'ORM: the table symbol `${table_name}` has to be a struct', id, tc.sql_expr_text_pos(node,
		' ${table_name}', 1, table_name.len))
}

fn sql_orm_table_names(tokens []string) []string {
	mut names := []string{}
	for i := 0; i + 1 < tokens.len; i++ {
		if tokens[i] !in ['from', 'into', 'update', 'table'] {
			continue
		}
		name := tokens[i + 1]
		if should_check_named_type(name) && name !in names {
			names << name
		}
	}
	return names
}

fn (mut tc TypeChecker) check_sql_orm_struct_constraints(id flat.NodeId, table_name string) {
	decl_id := tc.source_struct_decl_id_for_name(table_name) or { return }
	decl := tc.a.node(decl_id)
	if pos := tc.declaration_attribute_without_value_pos(decl_id, 'table') {
		tc.record_sql_error_at(.assignment_mismatch, 'ORM: table attribute must have an argument',
			id, pos)
		return
	}
	mut fields := []flat.Node{}
	mut has_primary := false
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(decl, i)
		if field.kind != .field_decl || orm_field_is_skipped(field) {
			continue
		}
		fields << field
		if struct_field_has_attr(field, 'primary') {
			if has_primary {
				tc.record_sql_error_at(.assignment_mismatch,
					'ORM: a struct can only have one primary key', id,
					tc.sql_orm_field_declaration_pos(field))
				return
			}
			has_primary = true
		}
	}
	for field in fields {
		info := sql_orm_field_type_info(field.typ)
		if info.is_multidim {
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: multi-dimension array fields are not supported', id,
				tc.sql_orm_field_declaration_pos(field))
			return
		}
		if info.base_name.len > 0 && tc.sql_orm_field_is_sub_struct(info.base_name)
			&& tc.sql_orm_struct_reaches(info.base_name, table_name, []string{}) {
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: invalid recursive struct `${info.base_name.all_after_last('.')}`', id,
				tc.sql_orm_field_declaration_pos(field))
			return
		}
	}
	for field in fields {
		info := sql_orm_field_type_info(field.typ)
		if info.is_array && !struct_field_has_attr(field, 'fkey') {
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: array fields must have an fkey attribute', id,
				tc.sql_orm_field_declaration_pos(field))
			return
		}
		if info.is_array && !has_primary {
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: a struct that has a field that holds an array must have a primary key', id,
				tc.sql_orm_field_declaration_pos(field))
			return
		}
		meta := field.generic_params()
		if meta.len > 1 {
			for raw_attr in meta[1..] {
				if raw_attr.trim_space() == 'fkey' {
					tc.record_sql_error_at(.assignment_mismatch,
						'ORM: the `fkey` attribute must have an argument', id, tc.sql_orm_field_attribute_pos(field,
						'fkey'))
					return
				}
			}
		}
		if !info.is_array && tc.sql_orm_field_is_sub_struct(info.base_name) {
			foreign_decl := tc.source_struct_decl_for_name(info.base_name) or { continue }
			mut foreign_has_primary := false
			for i in 0 .. foreign_decl.children_count {
				foreign_field := tc.a.child_node(&foreign_decl, i)
				if foreign_field.kind == .field_decl
					&& struct_field_has_attr(*foreign_field, 'primary') {
					foreign_has_primary = true
					break
				}
			}
			if !foreign_has_primary {
				tc.record_sql_error_at(.assignment_mismatch,
					'ORM: struct `${info.base_name.all_after_last('.')}` used as ORM sub-struct field `${field.value}` must have a `@[primary]` field, or use `@[sql: \'-\']` to skip this field',
					id, tc.sql_orm_field_declaration_pos(field))
				return
			}
		}
	}
}

fn sql_orm_field_type_info(raw_type string) SqlOrmFieldTypeInfo {
	mut clean := trimmed_space(raw_type)
	for clean.starts_with('?') || clean.starts_with('!') || clean.starts_with('&') {
		clean = trimmed_space(clean[1..])
	}
	mut dimensions := 0
	for clean.starts_with('[]') {
		dimensions++
		clean = trimmed_space(clean[2..])
	}
	return SqlOrmFieldTypeInfo{
		base_name:   clean
		is_array:    dimensions > 0
		is_multidim: dimensions > 1
	}
}

fn sql_orm_type_names_match(a string, b string) bool {
	return a == b || a.all_after_last('.') == b.all_after_last('.')
}

fn (tc &TypeChecker) sql_orm_field_is_sub_struct(name string) bool {
	if name == 'time.Time' {
		return false
	}
	return unalias_type(tc.parse_type(name)) is Struct
		&& tc.source_struct_decl_for_name(name) != none
}

fn (tc &TypeChecker) sql_orm_struct_reaches(current string, target string, seen []string) bool {
	if sql_orm_type_names_match(current, target) {
		return true
	}
	if current in seen {
		return false
	}
	decl := tc.source_struct_decl_for_name(current) or { return false }
	mut next_seen := seen.clone()
	next_seen << current
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl || orm_field_is_skipped(field) {
			continue
		}
		info := sql_orm_field_type_info(field.typ)
		if info.base_name.len == 0 || !tc.sql_orm_field_is_sub_struct(info.base_name) {
			continue
		}
		if tc.sql_orm_struct_reaches(info.base_name, target, next_seen) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) check_sql_statement_constraints(id flat.NodeId, node flat.Node, tokens []string) {
	tc.check_sql_insert_type(id, node, tokens)
	tc.check_sql_update_fields(id, node, tokens)
	tc.check_sql_where_constraints(id, node, tokens)
	tc.check_sql_limit_constraint(id, node, tokens)
	tc.check_sql_order_by_constraint(id, node, tokens)
}

fn (mut tc TypeChecker) check_sql_insert_type(id flat.NodeId, node flat.Node, tokens []string) {
	for i := 0; i + 3 < tokens.len; i++ {
		if tokens[i] != 'insert' || tokens[i + 2] != 'into' {
			continue
		}
		value_name := tokens[i + 1]
		table_name := tokens[i + 3]
		mut actual := tc.cur_scope.lookup(value_name) or {
			tc.record_sql_error_at(.unknown_ident, 'undefined ident: `${value_name}`', id, tc.sql_expr_text_pos(node,
				'insert ${value_name}', 'insert '.len, value_name.len))
			continue
		}
		clean_actual := unalias_type(actual)
		if clean_actual is Array {
			elem_type := unalias_type(clean_actual.elem_type)
			if elem_type is Pointer {
				continue
			}
			actual = elem_type
		}
		expected := tc.parse_type(table_name)
		if tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual) {
			continue
		}
		tc.record_sql_error_at(.assignment_mismatch,
			'cannot use `${actual.name()}` as `${expected.name()}`', id, tc.sql_expr_text_pos(node,
			'insert ${value_name}', 'insert '.len, value_name.len))
	}
}

fn (mut tc TypeChecker) check_sql_update_fields(id flat.NodeId, node flat.Node, tokens []string) {
	for update_idx := 0; update_idx + 3 < tokens.len; update_idx++ {
		if tokens[update_idx] != 'update' {
			continue
		}
		table_name := tokens[update_idx + 1]
		mut set_idx := -1
		for j in update_idx + 2 .. tokens.len {
			if tokens[j] == 'set' {
				set_idx = j
				break
			}
		}
		if set_idx < 0 {
			continue
		}
		for i := set_idx + 1; i + 1 < tokens.len && tokens[i] != 'where'; i++ {
			if tokens[i + 1] != '=' {
				continue
			}
			field := tc.sql_orm_source_field(table_name, tokens[i]) or { continue }
			if !struct_field_has_attr(field, 'fkey') {
				continue
			}
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: `${field.value}` is a foreign column of `${table_name}`, it can\'t update here',
				id, tc.sql_expr_text_pos(node, 'update ${table_name}', 'update '.len,
				table_name.len))
			return
		}
	}
}

fn (mut tc TypeChecker) check_sql_where_constraints(id flat.NodeId, node flat.Node, tokens []string) {
	from_idx := tokens.index('from')
	where_idx := tokens.index('where')
	if from_idx < 0 || where_idx < 0 || from_idx + 1 >= tokens.len || where_idx + 1 >= tokens.len {
		return
	}
	table_name := tokens[from_idx + 1]
	mut field_names := []string{}
	if decl := tc.source_struct_decl_for_name(table_name) {
		for i in 0 .. decl.children_count {
			field := tc.a.child_node(&decl, i)
			if field.kind == .field_decl && !orm_field_is_skipped(field) {
				field_names << field.value
			}
		}
	}
	field_names.sort()
	mut has_comparison := false
	for i := where_idx + 1; i + 1 < tokens.len; i++ {
		if tokens[i + 1] !in ['==', '!=', '<', '>', '<=', '>=', 'in', '!in', 'is', '!is', 'like',
			'ilike'] {
			continue
		}
		has_comparison = true
		if !sql_like_identifier(tokens[i]) || tokens[i] in field_names {
			continue
		}
		if i > where_idx + 1 && tokens[i - 1] !in ['&&', '||', '('] {
			continue
		}
		base := 'ORM: left side of the `${tokens[i + 1]}` expression must be one of the `${table_name}`\'s fields'
		message := util.new_suggestion(tokens[i], field_names).say(base)
		tc.record_sql_error_at(.unknown_field, message, id, tc.sql_expr_text_pos(node,
			' ${tokens[i]} ', 1, tokens[i].len))
		break
	}
	for i := where_idx + 1; i + 2 < tokens.len; i++ {
		op := tokens[i + 1]
		if op !in ['==', '!=', '<', '>', '<=', '>=', 'in', '!in', 'is', '!is'] {
			continue
		}
		rhs := tokens[i + 2]
		if !sql_like_identifier(rhs) {
			continue
		}
		if rhs in ['none', 'true', 'false'] || (i + 3 < tokens.len && tokens[i + 3] == '(') {
			continue
		}
		if _ := tc.cur_scope.lookup(rhs) {
			continue
		}
		if rhs in field_names && rhs != tokens[i] {
			tc.record_sql_error_at(.unknown_field,
				'ORM: right side of the `${op}` expression cannot reference another `${table_name}` field; field-to-field comparisons are not supported',
				id, tc.sql_expr_text_pos(node, ' ${rhs}', 1, rhs.len))
		} else {
			tc.record_sql_error_at(.unknown_ident, 'undefined variable: `${rhs}`', id, tc.sql_expr_text_last_pos(node,
				rhs, rhs.len))
		}
		break
	}
	if !has_comparison && (tokens.len == 0 || tokens[0] != 'dynamic') {
		value := tokens[where_idx + 1]
		tc.record_sql_error_at(.condition_mismatch,
			'ORM: `where` expression must have at least one comparison for filtering rows', id, tc.sql_expr_text_pos(node,
			'where ${value}', 'where '.len, value.len))
	}
	for i := where_idx + 1; i + 1 < tokens.len; i++ {
		fn_name := tokens[i]
		if tokens[i + 1] != '(' || !should_check_named_type(fn_name) {
			continue
		}
		ret_type := tc.sql_orm_fn_return_type(fn_name) or { continue }
		if sql_orm_fn_return_type_is_allowed(ret_type) {
			continue
		}
		call_length := tc.sql_source_call_length(node, fn_name)
		tc.record_sql_error_at(.assignment_mismatch,
			'ORM: function calls must return only primitive types and time.Time, but `${fn_name}` returns `${ret_type.name()}`',
			id, tc.sql_expr_text_pos(node, '${fn_name}(', 0, call_length))
	}
}

fn (mut tc TypeChecker) check_sql_order_by_constraint(id flat.NodeId, node flat.Node, tokens []string) {
	from_idx := tokens.index('from')
	if from_idx < 0 || from_idx + 1 >= tokens.len {
		return
	}
	table_name := tokens[from_idx + 1]
	mut field_names := []string{}
	if decl := tc.source_struct_decl_for_name(table_name) {
		for i in 0 .. decl.children_count {
			field := tc.a.child_node(&decl, i)
			if field.kind == .field_decl && !orm_field_is_skipped(field) {
				field_names << field.value
			}
		}
	}
	field_names.sort()
	for i := from_idx + 2; i + 2 < tokens.len; i++ {
		if tokens[i] != 'order' || tokens[i + 1] != 'by' {
			continue
		}
		field_name := tokens[i + 2]
		if field_name in field_names {
			continue
		}
		message :=
			util.new_suggestion(field_name, field_names).say('ORM: `${table_name}` structure has no field with name `${field_name}`')
		tc.record_sql_error_at(.unknown_field, message, id, tc.sql_expr_text_pos(node,
			'by ${field_name}', 'by '.len, field_name.len))
		tc.mark_invalid_sql_result_void(id)
		return
	}
}

fn (mut tc TypeChecker) mark_invalid_sql_result_void(id flat.NodeId) {
	tc.register_synth_type(id, Type(void_))
	mut result_id := id
	parent_id := tc.direct_parent_id(result_id)
	if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .or_expr {
		tc.register_synth_type(parent_id, Type(void_))
		result_id = parent_id
	}
	assign_id := tc.direct_parent_id(result_id)
	if !tc.valid_node_id(assign_id) {
		return
	}
	assign := tc.a.node(assign_id)
	if assign.kind != .decl_assign {
		return
	}
	for i := 0; i + 1 < assign.children_count; i += 2 {
		lhs := tc.a.child_node(assign, i)
		if tc.a.child(assign, i + 1) == result_id && lhs.kind == .ident && lhs.value != '_' {
			tc.cur_scope.insert(lhs.value, Type(void_))
			return
		}
	}
}

fn (tc &TypeChecker) sql_orm_fn_return_type(name string) ?Type {
	for candidate in [name, tc.qualify_name(name)] {
		if typ := tc.fn_ret_types[candidate] {
			return typ
		}
	}
	return none
}

fn sql_orm_fn_return_type_is_allowed(typ Type) bool {
	clean := unalias_type(typ)
	if clean is OptionType || clean is ResultType || clean is Pointer {
		return false
	}
	return clean is Primitive || clean is String || clean is Char || clean is Rune || clean is ISize
		|| clean is USize || clean is Enum || clean.name() == 'time.Time'
}

fn (mut tc TypeChecker) check_sql_limit_constraint(id flat.NodeId, node flat.Node, tokens []string) {
	for i := 0; i + 1 < tokens.len; i++ {
		if tokens[i] != 'limit' {
			continue
		}
		value := tokens[i + 1]
		limit := tc.const_int_value(value, []string{}) or { continue }
		if limit < 0 {
			tc.record_sql_error_at(.assignment_mismatch,
				'ORM: `limit` must be greater than or equal to zero', id, tc.sql_expr_text_pos(node,
				'limit ${value}', 'limit '.len, value.len))
		}
	}
}

fn (tc &TypeChecker) sql_orm_source_field(table_name string, field_name string) ?flat.Node {
	decl := tc.source_struct_decl_for_name(table_name) or { return none }
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind == .field_decl && field.value == field_name {
			return *field
		}
	}
	return none
}

fn (mut tc TypeChecker) record_sql_error_at(kind TypeErrorKind, msg string, node flat.NodeId, pos token.Pos) {
	if tc.errors.any(it.msg == msg && it.pos == pos) {
		return
	}
	tc.record_error_at(kind, msg, node, pos)
}

fn (mut tc TypeChecker) check_sql_aggregate_constraints(id flat.NodeId, node flat.Node, tokens []string) {
	for select_idx, token in tokens {
		if token != 'select' {
			continue
		}
		mut aggregate_idx := select_idx + 1
		if aggregate_idx < tokens.len && tokens[aggregate_idx] == 'distinct' {
			aggregate_idx++
		}
		if aggregate_idx >= tokens.len || tokens[aggregate_idx] !in ['sum', 'avg'] {
			continue
		}
		aggregate := tokens[aggregate_idx]
		if aggregate_idx + 1 >= tokens.len || tokens[aggregate_idx + 1] != '(' {
			continue
		}
		mut close_idx := -1
		mut depth := 0
		for i in aggregate_idx + 1 .. tokens.len {
			if tokens[i] == '(' {
				depth++
			} else if tokens[i] == ')' {
				depth--
				if depth == 0 {
					close_idx = i
					break
				}
			}
		}
		if close_idx < 0 {
			continue
		}
		if close_idx != aggregate_idx + 3 || !should_check_named_type(tokens[aggregate_idx + 2]) {
			mut operator := '+'
			for i in aggregate_idx + 2 .. close_idx {
				if tokens[i] in ['+', '-', '*', '/', '%'] {
					operator = tokens[i]
					break
				}
			}
			tc.record_error_at(.assignment_mismatch,
				'ORM aggregate functions only support a single field name argument', id, tc.sql_expr_text_pos(node,
				' ${operator} ', 1, operator.len))
			continue
		}
		field_name := tokens[aggregate_idx + 2]
		mut table_name := ''
		for i in close_idx + 1 .. tokens.len - 1 {
			if tokens[i] == 'from' {
				table_name = tokens[i + 1]
				break
			}
		}
		if table_name.len == 0 {
			continue
		}
		field_type := tc.sql_aggregate_field_type(table_name, field_name) or { continue }
		clean_field := sql_aggregate_base_type(field_type)
		if !clean_field.is_integer() && !clean_field.is_float() {
			tc.record_error_at(.assignment_mismatch,
				'ORM: `${aggregate}` aggregate requires a numeric field', id,
				tc.sql_expr_head_pos(node))
		}
	}
}

fn sql_aggregate_base_type(typ Type) Type {
	if typ is Alias {
		return sql_aggregate_base_type(typ.base_type)
	}
	if typ is OptionType {
		return sql_aggregate_base_type(typ.base_type)
	}
	return typ
}

fn (mut tc TypeChecker) check_sql_bulk_pointer_arrays(id flat.NodeId, node flat.Node, tokens []string) {
	for i := 0; i + 3 < tokens.len; i++ {
		if tokens[i] == 'insert' {
			value_name := tokens[i + 1]
			if tokens[i + 2] != 'into' || !tc.sql_ident_is_pointer_array(value_name) {
				continue
			}
			table_name := tokens[i + 3]
			tc.record_error_at(.assignment_mismatch,
				'ORM: bulk insert currently supports only arrays of `${table_name}` values', id, tc.sql_expr_text_pos(node,
				' ${value_name} ', 1, value_name.len))
		}
		if tokens[i] == 'update' {
			table_name := tokens[i + 1]
			mut value_name := ''
			for j in i + 2 .. tokens.len - 1 {
				if tokens[j + 1] == '.' && tc.sql_ident_is_pointer_array(tokens[j]) {
					value_name = tokens[j]
				}
			}
			if value_name.len > 0 {
				tc.record_error_at(.assignment_mismatch,
					'ORM: bulk update currently supports only arrays of `${table_name}` values',
					id, tc.sql_expr_text_last_pos(node, value_name, value_name.len))
			}
		}
	}
}

fn (tc &TypeChecker) sql_ident_is_pointer_array(name string) bool {
	typ := tc.cur_scope.lookup(name) or { return false }
	clean := unalias_type(typ)
	if clean is Array {
		return unalias_type(clean.elem_type) is Pointer
	}
	return false
}

fn (mut tc TypeChecker) check_sql_like_operands(id flat.NodeId, node flat.Node, tokens []string) {
	mut table_name := ''
	for i in 0 .. tokens.len - 1 {
		if tokens[i] in ['from', 'into', 'update'] {
			table_name = tokens[i + 1]
			break
		}
	}
	table_type := tc.parse_type(table_name)
	for i := 1; i + 1 < tokens.len; i++ {
		if tokens[i] !in ['like', 'ilike'] {
			continue
		}
		lhs := tokens[i - 1]
		rhs := tokens[i + 1]
		lhs_type := tc.sql_like_token_type(lhs, table_type)
		lhs_is_string_field := sql_like_identifier(lhs) && unalias_type(lhs_type) is String
		if !lhs_is_string_field {
			tc.record_error_at(.condition_mismatch,
				'the left operand of the `${tokens[i]}` operator must be an identifier with a string type',
				id, tc.sql_expr_text_pos(node, ' ${lhs} ', 1, lhs.len))
		}
		rhs_type := tc.sql_like_token_type(rhs, table_type)
		if !((rhs.len >= 2 && rhs[0] in [`'`, `"`] && rhs[rhs.len - 1] == rhs[0])
			|| unalias_type(rhs_type) is String) {
			tc.record_error_at(.condition_mismatch,
				'the right operand of the `${tokens[i]}` operator must be a string type', id, tc.sql_expr_text_pos(node,
				' ${rhs}', 1, rhs.len))
		}
		if table_name.len > 0 && !tc.sql_table_has_field(table_type, lhs) {
			tc.record_error_at(.unknown_field,
				'ORM: left side of the `${tokens[i]}` expression must be one of the `${table_name}`\'s fields',
				id, tc.sql_expr_text_pos(node, ' ${lhs} ', 1, lhs.len))
		}
	}
}

fn (tc &TypeChecker) sql_like_token_type(value string, table_type Type) Type {
	if value == 'true' || value == 'false' {
		return Type(bool_)
	}
	if value.len > 0 && value[0].is_digit() {
		return Type(int_)
	}
	if typ := tc.cur_scope.lookup(value) {
		return typ
	}
	return tc.sql_struct_field_type(table_type, value) or { unknown_type('unknown SQL operand') }
}

fn (tc &TypeChecker) sql_table_has_field(table_type Type, name string) bool {
	return tc.sql_struct_field_type(table_type, name) != none
}

fn sql_like_identifier(value string) bool {
	if value.len == 0 || value[0].is_digit() {
		return false
	}
	for ch in value.bytes() {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) check_sql_anonymous_struct_fields(tokens []string) {
	if tokens.len < 2 {
		return
	}
	mut table_name := ''
	for i in 0 .. tokens.len - 1 {
		if tokens[i] in ['table', 'from', 'into', 'update'] {
			table_name = tokens[i + 1]
			break
		}
	}
	if table_name.len == 0 {
		return
	}
	decl := tc.source_struct_decl_for_name(table_name) or { return }
	for i in 0 .. decl.children_count {
		field_id := tc.a.child(&decl, i)
		field := tc.a.node(field_id)
		if field.kind != .field_decl || !is_anonymous_struct_name(field.typ)
			|| orm_field_is_skipped(field) {
			continue
		}
		tc.record_error_at(.assignment_mismatch,
			'ORM: field `${field.value}` uses an anonymous struct type, which ORM does not support; use a named struct, or skip it with `@[skip]` or `@[sql: \'-\']`',
			field_id, tc.struct_field_declaration_pos(field))
	}
}

fn orm_field_is_skipped(field flat.Node) bool {
	meta := field.generic_params()
	if meta.len < 2 {
		return false
	}
	for attr in meta[1..] {
		name := attr.all_before(':').trim_space()
		if name == 'skip' {
			return true
		}
		if name == 'sql' && attr.contains(':') {
			value := attr.all_after(':').trim_space().trim('\'"')
			if value == '-' {
				return true
			}
		}
	}
	return false
}

fn (mut tc TypeChecker) check_dynamic_sql_query_data_fields(id flat.NodeId, tokens []string) bool {
	from_idx := tokens.index('from')
	where_idx := tokens.index('where')
	if from_idx < 0 || where_idx < 0 || from_idx + 1 >= tokens.len || where_idx + 1 >= tokens.len {
		return false
	}
	table_name := tokens[from_idx + 1]
	alias_name := tokens[where_idx + 1]
	mut fields := []StructField{}
	for candidate in [table_name, tc.qualify_name(table_name)] {
		if known := tc.structs[candidate] {
			fields = known.clone()
			break
		}
	}
	if fields.len == 0 {
		return false
	}
	query_id := tc.dynamic_sql_query_data_node(alias_name) or { return false }
	query := tc.a.node(query_id)
	query_tokens := query.value.split(' ')
	mut field_names := fields.map(it.name)
	field_names.sort()
	for i := 0; i + 1 < query_tokens.len; i++ {
		field_name := query_tokens[i]
		if query_tokens[i + 1] !in ['==', '!=', '<', '>', '<=', '>=', 'in', '!in', 'like', 'ilike', 'is', '!is', '=']
			|| !should_check_named_type(field_name) || field_name.contains('.')
			|| field_name in field_names {
			continue
		}
		message :=
			util.new_suggestion(field_name, field_names).say('ORM: `${table_name}` structure has no field with name `${field_name}`')
		tc.record_error_at(.unknown_field, message, id, tc.dynamic_sql_field_pos(query_id,
			field_name))
		return true
	}
	return false
}

fn (tc &TypeChecker) dynamic_sql_query_data_node(alias_name string) ?flat.NodeId {
	for node in tc.a.nodes {
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs := tc.a.child_node(&node, 0)
		if lhs.kind != .ident || lhs.value != alias_name {
			continue
		}
		rhs_id := tc.a.child(&node, 1)
		rhs := tc.a.node(rhs_id)
		if rhs.kind == .sql_expr && rhs.value.starts_with('querydata ') {
			return rhs_id
		}
	}
	return none
}

fn (tc &TypeChecker) dynamic_sql_field_pos(query_id flat.NodeId, field_name string) token.Pos {
	if !tc.valid_node_id(query_id) {
		return token.Pos{}
	}
	node := tc.a.node(query_id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	if span := closest_identifier_span(source, field_name, node.pos.offset, node.pos.id) {
		return span
	}
	return node.pos
}

fn (tc &TypeChecker) sql_struct_field_type(owner Type, field_name string) ?Type {
	clean := unalias_type(unwrap_pointer(owner))
	name := clean.name()
	for candidate in [name, tc.qualify_name(name)] {
		for field in tc.structs[candidate] or { continue } {
			if field.name == field_name {
				return field.typ
			}
		}
	}
	return none
}

fn (tc &TypeChecker) sql_expr_text_pos(node flat.Node, needle string, relative_start int, length int) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	mut anchor := int_min(int_max(node.pos.offset, 0), source.len)
	if node.children_count > 0 {
		db := tc.a.child_node(&node, 0)
		anchor = int_min(int_max(db.pos.offset, 0), source.len)
	}
	line_start := if relative := source[..anchor].last_index('\n') {
		relative + 1
	} else {
		0
	}
	mut block_end := int_min(int_max(node.pos.end, anchor), source.len)
	if block_end <= anchor {
		block_end = source.index_after('}!', anchor) or { source.len }
	}
	if relative := source[line_start..block_end].index(needle) {
		start := line_start + relative + relative_start
		return token.new_span(node.pos.id, start, start + length)
	}
	return node.pos
}

fn (tc &TypeChecker) sql_orm_field_declaration_pos(field flat.Node) token.Pos {
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	anchor := int_min(int_max(field.pos.offset, 0), source.len)
	line_start := if relative := source[..anchor].last_index('\n') {
		relative + 1
	} else {
		0
	}
	line_end := source.index_after('\n', anchor) or { source.len }
	line := source[line_start..line_end]
	name_start := line.index(field.value) or { return field.pos }
	type_start := line.index_after(field.typ, name_start + field.value.len) or { return field.pos }
	start := line_start + name_start
	return token.new_span(field.pos.id, start, line_start + type_start + field.typ.len)
}

fn (tc &TypeChecker) sql_orm_field_attribute_pos(field flat.Node, attr_name string) token.Pos {
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	anchor := int_min(int_max(field.pos.offset, 0), source.len)
	line_start := if relative := source[..anchor].last_index('\n') {
		relative + 1
	} else {
		0
	}
	line_end := source.index_after('\n', anchor) or { source.len }
	line := source[line_start..line_end]
	exact := '@[${attr_name}]'
	if relative := line.index(exact) {
		start := line_start + relative
		return token.new_span(field.pos.id, start, start + exact.len)
	}
	if relative := line.index(attr_name) {
		start := line_start + relative
		return token.new_span(field.pos.id, start, start + attr_name.len)
	}
	return field.pos
}

fn (tc &TypeChecker) sql_source_call_length(node flat.Node, fn_name string) int {
	file := tc.a.source_files[node.pos.id] or { return fn_name.len + 2 }
	source := tc.source_texts_by_file[file.name] or { return fn_name.len + 2 }
	mut anchor := int_min(int_max(node.pos.offset, 0), source.len)
	if node.children_count > 0 {
		db := tc.a.child_node(&node, 0)
		anchor = int_min(int_max(db.pos.offset, 0), source.len)
	}
	mut block_end := int_min(int_max(node.pos.end, anchor), source.len)
	if block_end <= anchor {
		block_end = source.index_after('}!', anchor) or { source.len }
	}
	needle := '${fn_name}('
	relative := source[anchor..block_end].index(needle) or { return fn_name.len + 2 }
	start := anchor + relative
	mut depth := 0
	for i in start + fn_name.len .. block_end {
		if source[i] == `(` {
			depth++
		} else if source[i] == `)` {
			depth--
			if depth == 0 {
				return i - start + 1
			}
		}
	}
	return fn_name.len + 2
}

fn (tc &TypeChecker) sql_expr_head_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	mut anchor := int_min(int_max(node.pos.offset, 0), source.len)
	if node.children_count > 0 {
		db := tc.a.child_node(&node, 0)
		anchor = int_min(int_max(db.pos.offset, 0), source.len)
	}
	line_start := if relative := source[..anchor].last_index('\n') {
		relative + 1
	} else {
		0
	}
	line_end := source.index_after('\n', anchor) or { source.len }
	line := source[line_start..line_end]
	sql_start := line.index('sql ') or { return node.pos }
	sql_head := line[sql_start..]
	brace := sql_head.index('{') or { return node.pos }
	start := line_start + sql_start
	return token.new_span(node.pos.id, start, start + brace + 1)
}

fn (tc &TypeChecker) sql_expr_text_last_pos(node flat.Node, needle string, length int) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	mut anchor := int_min(int_max(node.pos.offset, 0), source.len)
	if node.children_count > 0 {
		db := tc.a.child_node(&node, 0)
		anchor = int_min(int_max(db.pos.offset, 0), source.len)
	}
	line_start := if relative := source[..anchor].last_index('\n') {
		relative + 1
	} else {
		0
	}
	mut block_end := int_min(int_max(node.pos.end, anchor), source.len)
	if block_end <= anchor {
		block_end = source.index_after('}!', anchor) or { source.len }
	}
	if relative := source[line_start..block_end].last_index(needle) {
		start := line_start + relative
		return token.new_span(node.pos.id, start, start + length)
	}
	return node.pos
}

fn (tc &TypeChecker) sql_aggregate_or_expr_type(node flat.Node) ?Type {
	if node.kind != .or_expr || node.value != '!' || node.children_count < 1 {
		return none
	}
	child_id := tc.a.child(&node, 0)
	if int(child_id) < 0 || int(child_id) >= tc.a.nodes.len {
		return none
	}
	child := tc.a.nodes[int(child_id)]
	if child.kind != .sql_expr {
		return none
	}
	tokens := child.value.split(' ')
	for start, token in tokens {
		if token != 'select' {
			continue
		}
		mut select_start := start + 1
		if tokens.len > select_start && tokens[select_start] == 'distinct' {
			select_start++
		}
		if tokens.len <= select_start || tokens[select_start] !in ['sum', 'avg', 'min', 'max'] {
			continue
		}
		if select_start + 5 >= tokens.len || tokens[select_start + 1] != '('
			|| tokens[select_start + 3] != ')' || tokens[select_start + 4] != 'from' {
			continue
		}
		field_name := tokens[select_start + 2]
		table_name := tokens[select_start + 5]
		field_type := tc.sql_aggregate_field_type(table_name, field_name) or { continue }
		return tc.parse_canonical_type(sql_aggregate_optional_type_name(tokens[select_start],
			field_type))
	}
	return none
}

fn (tc &TypeChecker) sql_aggregate_field_type(table_name string, field_name string) ?Type {
	for candidate in [table_name, tc.qualify_name(table_name)] {
		fields := tc.structs[candidate] or { continue }
		for field in fields {
			if field.name == field_name {
				return field.typ
			}
		}
	}
	return none
}

fn sql_aggregate_optional_type_name(aggregate string, field_type Type) string {
	if field_type is OptionType {
		return sql_aggregate_optional_type_name(aggregate, field_type.base_type)
	}
	if field_type is Alias {
		return sql_aggregate_optional_type_name(aggregate, field_type.base_type)
	}
	if aggregate == 'avg' {
		return '?f64'
	}
	if field_type is String {
		return '?string'
	}
	if field_type is Struct && field_type.name == 'time.Time' {
		return '?time.Time'
	}
	if field_type is Primitive && field_type.props.has(.float) {
		return '?f64'
	}
	return '?int'
}

fn (tc &TypeChecker) call_has_argument_count_error(id flat.NodeId) bool {
	return tc.errors.any(it.node == id && it.msg.starts_with('expected ')
		&& it.msg.contains(' arguments, but got '))
}

fn (mut tc TypeChecker) check_or_fallback_branch_node(id flat.NodeId, require_value bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(id)
		}
		tc.check_statement_sequence(node, 0, require_value)
		tc.ownership_record_or_fallback_error_return_drops(id)
		tc.pop_scope()
		return
	}
	tc.check_node(id)
	tc.ownership_record_or_fallback_error_return_drops(id)
}

fn (mut tc TypeChecker) ownership_record_or_fallback_error_return_drops(id flat.NodeId) {
	$if ownership ? {
		if tc.fn_context.return_type !is OptionType && tc.fn_context.return_type !is ResultType {
			return
		}
		tail_id := tc.branch_tail_expr_id(id)
		if !tc.branch_tail_is_error_literal(tail_id) {
			return
		}
		tc.ownership_record_propagation_drops()
	}
}

fn (mut tc TypeChecker) check_defer_stmt(node flat.Node) {
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) check_asm_stmt(id flat.NodeId, node flat.Node) {
	if !node.pos.is_valid() {
		return
	}
	file := tc.a.source_files[node.pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	start := int_max(0, node.pos.offset)
	end := int_min(source.len, node.pos.end)
	mut line_start := start
	for line_start < end {
		line_end := source.index_after('\n', line_start) or { end }
		content_end := int_min(line_end, end)
		line := source[line_start..content_end].trim_right('\r\n')
		trimmed := line.trim_space()
		if trimmed == 'mov' {
			relative := line.index('mov') or { 0 }
			tc.record_error_at(.assignment_mismatch,
				'asm instruction `mov` expects 2 operands, but got 0', id, token.new_span(node.pos.id,

				line_start + relative, line_start + relative + 'mov'.len))
		}
		if trimmed.starts_with(';') && trimmed.contains('=') {
			open := line.index_u8(`(`)
			close := if open >= 0 { line.index_after(')', open + 1) or { -1 } } else { -1 }
			if open >= 0 && close > open + 1 {
				mut name_start := open + 1
				for name_start < close && line[name_start] in [` `, `\t`] {
					name_start++
				}
				mut name_end := close
				for name_end > name_start && line[name_end - 1] in [` `, `\t`] {
					name_end--
				}
				name := line[name_start..name_end]
				if name.len > 0 && tc.cur_scope.lookup(name) != none
					&& !tc.ident_is_mutable_lvalue(name) {
					tc.record_error_at(.assignment_mismatch,
						'`${name}` is immutable, declare it with `mut` to make it mutable', id, token.new_span(node.pos.id,

						line_start + name_start, line_start + name_end))
				}
			}
		}
		if line_end >= end {
			break
		}
		line_start = line_end + 1
	}
}

// check_fn_literal validates check fn literal state for types.
fn (mut tc TypeChecker) check_fn_literal(id flat.NodeId, node flat.Node) {
	if node.value == 'missing_body' {
		tc.record_error(.return_mismatch, 'anonymous function must declare a body', id)
		return
	}
	mut param_names := map[string]bool{}
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		param := tc.a.node(param_id)
		if param.kind != .param {
			continue
		}
		if param.value.len == 0 {
			tc.record_error_at(.duplicate_decl, 'use `_` to name an unused parameter', param_id,
				param.pos)
			continue
		}
		if param_names[param.value] {
			tc.record_error_at(.duplicate_decl, 'redefinition of parameter `${param.value}`',
				param_id, tc.node_value_diagnostic_pos(param_id))
			continue
		}
		param_names[param.value] = true
		tc.check_import_symbol_conflict(param_id, param.value)
	}
	mut closure_copy_owners := map[string]ScopeBindingOwner{}
	mut explicit_captures := map[string]bool{}
	mut missing_capture_generic := false
	literal_generic_params := node.generic_params()
	for i in 0 .. node.children_count {
		capture := tc.a.child_node(&node, i)
		if capture.kind == .ident && capture.value.len > 0 {
			explicit_captures[capture.value] = true
		}
		if capture.kind == .ident && capture.is_mut && capture.value.len > 0
			&& !tc.ident_is_mutable_lvalue(capture.value) {
			capture_id := tc.a.child(&node, i)
			tc.record_error_at(.assignment_mismatch,
				'original `${capture.value}` is immutable, declare it with `mut` to make it mutable',
				capture_id, tc.node_value_diagnostic_pos(capture_id))
		} else if capture.kind == .ident && !capture.is_mut && capture.value.len > 0 {
			if owner := tc.cur_scope.lookup_owner(capture.value) {
				closure_copy_owners[capture.value] = owner
			}
		}
		if capture.kind == .ident && capture.value.len > 0 {
			if capture_type := tc.cur_scope.lookup(capture.value) {
				if tc.type_has_declaration_attribute(capture_type, 'nocopy') {
					capture_id := tc.a.child(&node, i)
					tc.record_error_at(.assignment_mismatch,
						'cannot capture @[nocopy] struct by value: use a reference instead',
						capture_id, tc.node_value_diagnostic_pos(capture_id))
				}
				capture_type_text := tc.current_fn_param_type_text(capture.value) or {
					capture_type.name()
				}
				for generic_name in tc.fn_context.generic_params {
					if generic_name !in literal_generic_params
						&& type_text_contains_symbol(capture_type_text, generic_name) {
						current_list := literal_generic_params.join(', ')
						if !missing_capture_generic {
							if tc.checker_fixture_mode && literal_generic_params.len == 0 {
								tc.record_error_at(.unsupported_generic,
									'generic closure fn must specify type parameter, e.g. fn [foo] [T]()',
									id, node.pos)
							} else {
								capture_id := tc.a.child(&node, i)
								tc.record_error_at(.unsupported_generic,
									'Add the generic type `${generic_name}` to the anon fn generic list type, that is currently `[${current_list}]`',
									capture_id, tc.node_value_diagnostic_pos(capture_id))
							}
						}
						missing_capture_generic = true
					}
				}
			}
		}
	}
	if missing_capture_generic {
		tc.check_invalid_fn_literal_generic_calls(node, literal_generic_params)
	}
	mut forbidden_captures := map[string]bool{}
	mut outer_scope := tc.cur_scope
	for outer_scope != unsafe { nil } {
		for name in outer_scope.names {
			qname := tc.qualify_name(name)
			is_global := name in tc.global_names || qname in tc.global_names
			if !is_global && !explicit_captures[name] {
				forbidden_captures[name] = true
			}
		}
		outer_scope = outer_scope.parent
	}
	saved_fn_context := tc.fn_context
	// Keep the enclosing function id so lambda dependencies are attributed to
	// the declaration that owns the generated closure.
	tc.fn_context = new_function_check_context()
	tc.fn_context.node_id = saved_fn_context.node_id
	tc.fn_context.generic_params = saved_fn_context.generic_params.clone()
	tc.fn_context.return_type = tc.parse_type(node.typ)
	tc.fn_context.method_value_locals = saved_fn_context.method_value_locals.clone()
	tc.fn_context.method_value_local_owners =
		clone_scope_binding_owner_map(saved_fn_context.method_value_local_owners)
	tc.fn_context.method_value_local_depth = saved_fn_context.method_value_local_depth.clone()
	tc.fn_context.closure_copy_owners = closure_copy_owners.clone()
	tc.fn_context.closure_forbidden_captures = forbidden_captures.clone()
	tc.fn_context.method_value_stack_mut_owners =
		saved_fn_context.method_value_stack_mut_owners.clone()
	$if ownership ? {
		tc.ownership_begin_fn_literal(id, node)
	}
	tc.push_scope()
	tc.fn_context.closure_scope = tc.cur_scope
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		tc.insert_fn_param_binding(child)
		if child.kind == .ident && child.is_mut && child.value.len > 0 {
			if owner := tc.cur_scope.lookup_owner(child.value) {
				tc.fn_context.mut_local_owners[child.value] = owner
			}
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param || child.kind == .ident {
			continue
		}
		tc.check_stmt_node(child_id)
	}
	return_type := unalias_type(tc.parse_type(node.typ))
	if !type_allows_implicit_return(return_type) && !missing_capture_generic {
		mut body_start := 0
		for body_start < node.children_count {
			child := tc.a.child_node(&node, body_start)
			if child.kind !in [.param, .ident] {
				break
			}
			body_start++
		}
		if !tc.stmt_sequence_definitely_returns(&node, body_start) {
			tc.record_error_at(.return_mismatch,
				'missing return at the end of an anonymous function', id, node.pos)
		}
	}
	for i in 0 .. node.children_count {
		capture_id := tc.a.child(&node, i)
		capture := tc.a.node(capture_id)
		capture_type_text := tc.current_fn_param_type_text(capture.value) or { '' }
		capture_has_open_generic :=
			saved_fn_context.generic_params.any(type_text_contains_symbol(capture_type_text, it))
		if capture.kind == .ident && capture.value.len > 0 && !capture_has_open_generic
			&& !tc.fn_literal_body_uses_ident(node, capture.value) {
			tc.record_notice_at(.unknown_ident, 'unused parameter: `${capture.value}`', capture_id,
				tc.node_value_diagnostic_pos(capture_id))
		}
	}
	for i in 0 .. node.children_count {
		param_id := tc.a.child(&node, i)
		param := tc.a.node(param_id)
		if param.kind == .param && param.value.len == 0 && param.typ.len > 0
			&& should_check_named_type(param.typ) && !tc.type_name_known(param.typ) {
			tc.record_error_at(.unknown_type, 'unknown type `${param.typ}`', param_id, param.pos)
			tc.record_malformed_fn_literal_call_mismatch(node, param_id, param)
		}
	}
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.fn_context = saved_fn_context
}

fn (tc &TypeChecker) fn_literal_body_uses_ident(node flat.Node, name string) bool {
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind !in [.param, .ident] {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := tc.a.node(id)
		if child.kind == .ident && child.value == name {
			return true
		}
		for i in 0 .. child.children_count {
			stack << tc.a.child(child, i)
		}
	}
	return false
}

fn (mut tc TypeChecker) check_invalid_fn_literal_generic_calls(node flat.Node, generic_params []string) {
	if generic_params.len == 0 || !node.pos.is_valid() {
		return
	}
	file := tc.a.source_files[node.pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	literal_source := source[start..end]
	for generic_name in generic_params {
		needle := '[${generic_name}]'
		mut search_start := 0
		for search_start < literal_source.len {
			relative := literal_source[search_start..].index(needle) or { break }
			index := search_start + relative
			mut name_start := index
			for name_start > 0 && literal_source[name_start - 1] in [` `, `\t`] {
				name_start--
			}
			mut name_end := name_start
			for name_start > 0 && is_type_symbol_byte(literal_source[name_start - 1]) {
				name_start--
			}
			call_name := literal_source[name_start..name_end]
			if call_name.len > 0 {
				decl_module := tc.fn_type_modules[call_name] or { tc.cur_module }
				if decl := tc.visible_mutation_fn_decl(call_name, decl_module) {
					if tc.record_invalid_comptime_for_type(decl, generic_name) {
						return
					}
				}
			}
			search_start = index + needle.len
		}
	}
}

fn (mut tc TypeChecker) record_invalid_comptime_for_type(decl VisibleMutationFnDecl, generic_name string) bool {
	if decl.idx < 0 || decl.idx >= tc.a.nodes.len {
		return false
	}
	mut stack := [flat.NodeId(decl.idx)]
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind == .comptime_for && node.typ.len > 0 {
			file := tc.a.source_files[node.pos.id] or { return false }
			source := tc.source_texts_by_file[file.name] or { return false }
			start := int_max(0, int_min(node.pos.offset, source.len))
			end := int_max(start, int_min(node.pos.end, source.len))
			relative := source[start..end].index('${node.typ}.') or { -1 }
			pos := if relative >= 0 {
				token.new_span(node.pos.id, start + relative, start + relative + node.typ.len)
			} else {
				node.pos
			}
			tc.record_error_at(.unknown_type,
				'$for expects a type name or variable name to be used here, but ${generic_name} is not a type or variable name',
				id, pos)
			return true
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return false
}

fn (tc &TypeChecker) current_fn_param_type_text(name string) ?string {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return none
	}
	fn_node := tc.a.node(fn_id)
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(fn_node, i)
		if param.kind == .param && param.value == name {
			return param.typ
		}
	}
	return none
}

fn (mut tc TypeChecker) record_malformed_fn_literal_call_mismatch(literal flat.Node, param_id flat.NodeId, param flat.Node) {
	if !literal.pos.is_valid() || param.typ.len == 0 {
		return
	}
	file := tc.a.source_files[literal.pos.id] or { return }
	source := tc.source_texts_by_file[file.name] or { return }
	literal_start := int_max(0, int_min(literal.pos.offset, source.len))
	literal_end := int_max(literal_start, int_min(literal.pos.end, source.len))
	before := source[..literal_start]
	assign := before.last_index(':=') or { return }
	mut name_end := assign
	for name_end > 0 && before[name_end - 1] in [` `, `\t`] {
		name_end--
	}
	mut name_start := name_end
	for name_start > 0 && is_type_symbol_byte(before[name_start - 1]) {
		name_start--
	}
	name := before[name_start..name_end]
	if name.len == 0 {
		return
	}
	relative_call := source[literal_end..].index('${name}(') or { return }
	call_start := literal_end + relative_call
	arg_start_raw := call_start + name.len + 1
	close := source.index_after(')', arg_start_raw) or { return }
	mut arg_start := arg_start_raw
	for arg_start < close && source[arg_start] in [` `, `\t`] {
		arg_start++
	}
	mut arg_end := close
	for arg_end > arg_start && source[arg_end - 1] in [` `, `\t`] {
		arg_end--
	}
	if arg_start >= arg_end || source[arg_start..arg_end].contains(',') {
		return
	}
	arg_name := source[arg_start..arg_end]
	actual := tc.cur_scope.lookup(arg_name) or { return }
	tc.record_error_at(.call_arg_mismatch,
		'cannot use `${actual.name()}` as `${param.typ}` in argument 1 to `${name}`', param_id, token.new_span(literal.pos.id,
		arg_start, arg_end))
}

// check_lambda_expr validates check lambda expr state for types.
fn (mut tc TypeChecker) check_lambda_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.lambda_is_uncontextualized_if_branch_value(id) {
		tc.record_error_at(.call_arg_mismatch,
			'lambda expressions are allowed only in places expecting function callbacks', id,
			node.pos)
	}
	expected_fn := tc.lambda_expected_fn_type(id)
	mut forbidden_captures := map[string]bool{}
	if tc.checker_fixture_mode {
		mut outer_scope := tc.cur_scope
		for outer_scope != unsafe { nil } {
			for name in outer_scope.names {
				qname := tc.qualify_name(name)
				if name !in tc.global_names && qname !in tc.global_names {
					forbidden_captures[name] = true
				}
			}
			outer_scope = outer_scope.parent
		}
	}
	saved_fn_context := tc.fn_context
	tc.fn_context = new_function_check_context()
	tc.fn_context.node_id = saved_fn_context.node_id
	tc.fn_context.generic_params = saved_fn_context.generic_params.clone()
	tc.fn_context.return_type = expected_fn.return_type
	tc.fn_context.method_value_locals = saved_fn_context.method_value_locals.clone()
	tc.fn_context.method_value_local_owners =
		clone_scope_binding_owner_map(saved_fn_context.method_value_local_owners)
	tc.fn_context.method_value_local_depth = saved_fn_context.method_value_local_depth.clone()
	tc.fn_context.method_value_stack_mut_owners =
		saved_fn_context.method_value_stack_mut_owners.clone()
	tc.fn_context.closure_forbidden_captures = forbidden_captures.clone()
	tc.fn_context.lambda_no_captures = tc.checker_fixture_mode
	$if ownership ? {
		tc.ownership_begin_lambda_expr(id, node)
	}
	tc.push_scope()
	for i in 0 .. node.children_count - 1 {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			param_type := if i < expected_fn.params.len {
				fn_param_type(expected_fn, i)
			} else {
				unknown_type('lambda parameter `${child.value}`')
			}
			owner := tc.cur_scope.insert_with_owner(child.value, param_type)
			if child.is_mut {
				tc.fn_context.mut_param_base_types[child.value] = mut_param_base_type(param_type)
				tc.fn_context.mut_param_owners[child.value] = owner
			}
		}
	}
	tc.fn_context.closure_scope = tc.cur_scope
	body_id := tc.a.child(&node, node.children_count - 1)
	if expected_fn.return_type !is Unknown {
		tc.check_node_with_expected_context(body_id, expected_fn.return_type)
	} else {
		tc.check_node(body_id)
	}
	if invalid_name := tc.lambda_invalid_value_name(body_id) {
		tc.record_error_at(.return_mismatch, '`${invalid_name}` used as value', id, node.pos)
	}
	body := tc.a.node(body_id)
	body_type := if body.kind == .call {
		callee := tc.a.child_node(body, 0)
		if callee.kind == .ident {
			if call_name := tc.local_bare_fn_key(callee.value) {
				tc.fn_ret_types[call_name] or { tc.resolve_type(body_id) }
			} else {
				tc.resolve_type(body_id)
			}
		} else if call_name := tc.resolved_call_name(body_id) {
			tc.fn_ret_types[call_name] or { tc.resolve_type(body_id) }
		} else {
			tc.resolve_type(body_id)
		}
	} else {
		tc.resolve_type(body_id)
	}
	if (body_type is OptionType || body_type is ResultType)
		&& expected_fn.return_type !is OptionType && expected_fn.return_type !is ResultType
		&& !tc.type_compatible(body_type, expected_fn.return_type) {
		tc.record_error_at(.return_mismatch,
			'cannot use `${body_type.name()}` as type `${expected_fn.return_type.name()}` in return argument',
			body_id, tc.a.node(body_id).pos)
	}
	tc.pop_scope()
	$if ownership ? {
		tc.ownership_end_fn()
	}
	tc.fn_context = saved_fn_context
}

fn (tc &TypeChecker) lambda_invalid_value_name(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && tc.errors.any(it.node == id && it.kind == .unknown_ident
		&& (it.msg.starts_with('undefined ident')
		|| it.msg.starts_with('undefined variable'))) {
		return node.value
	}
	for i in 0 .. node.children_count {
		if name := tc.lambda_invalid_value_name(tc.a.child(node, i)) {
			return name
		}
	}
	return none
}

fn (tc &TypeChecker) lambda_is_uncontextualized_if_branch_value(id flat.NodeId) bool {
	mut branch_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(branch_id) {
		return false
	}
	mut branch := tc.a.node(branch_id)
	if branch.kind == .expr_stmt {
		branch_id = tc.direct_parent_id(branch_id)
		if !tc.valid_node_id(branch_id) {
			return false
		}
		branch = tc.a.node(branch_id)
	}
	if branch.kind != .block {
		return false
	}
	if_id := tc.direct_parent_id(branch_id)
	if !tc.valid_node_id(if_id) {
		return false
	}
	if_node := tc.a.node(if_id)
	return if_node.kind == .if_expr && if_node.children_count > 1
		&& tc.a.child(if_node, 1) == branch_id
}

fn (mut tc TypeChecker) lambda_expected_fn_type(id flat.NodeId) FnType {
	if expected := tc.expected_context_for_expr(id) {
		if fn_typ := fn_type_from_type(expected) {
			return fn_typ
		}
	}
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .call {
			if info := tc.resolve_call_info(parent_id, *parent) {
				for i in 1 .. parent.children_count {
					if tc.call_arg_value(tc.a.child(parent, i)) != id {
						continue
					}
					param_idx := i - 1 + if info.has_receiver { 1 } else { 0 }
					if param_idx >= 0 && param_idx < info.params.len {
						if fn_typ := fn_type_from_type(info.params[param_idx]) {
							return fn_typ
						}
					}
				}
			}
		}
	}
	return FnType{
		return_type: unknown_type('unknown lambda return type')
	}
}

// check_block validates check block state for types.
fn (mut tc TypeChecker) check_block(id flat.NodeId, node flat.Node) {
	tc.push_scope()
	is_unsafe := node.value == 'unsafe'
	if is_unsafe {
		tc.unsafe_depth++
	}
	$if ownership ? {
		tc.ownership_mark_scope_node(id)
	}
	if node.value == 'comma_exprs' {
		if tc.is_statement_node(id) {
			for i in 0 .. node.children_count {
				mut value_id := tc.a.child(&node, i)
				value := tc.a.node(value_id)
				if value.kind == .expr_stmt && value.children_count > 0 {
					value_id = tc.a.child(value, 0)
				}
				if unalias_type(tc.resolve_type(value_id)) is Void {
					tc.record_error_at(.return_mismatch,
						'type `void` cannot be used in multi-return', value_id,
						tc.a.node(value_id).pos)
					break
				}
			}
		}
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
	} else {
		tc.check_statement_sequence(node, 0, is_unsafe && !tc.is_statement_node(id))
	}
	if is_unsafe && tc.unsafe_depth > 0 {
		tc.unsafe_depth--
	}
	tc.pop_scope()
}

// check_for_stmt validates check for stmt state for types.
fn (mut tc TypeChecker) check_for_stmt(node flat.Node) {
	tc.push_scope()
	$if ownership ? {
		if node.children_count > 0 {
			init_id := tc.a.child(&node, 0)
			if int(init_id) >= 0 && tc.a.nodes[int(init_id)].kind != .empty {
				tc.ownership_mark_scope_node(init_id)
			}
		}
	}
	if node.children_count > 0 {
		init_id := tc.a.child(&node, 0)
		if int(init_id) >= 0 {
			tc.check_node(init_id)
		}
	}
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		if int(cond_id) >= 0 {
			tc.check_for_condition(cond_id, node)
		}
	}
	mut saved_smartcasts := map[string]Type{}
	mut has_cond_smartcasts := false
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		smartcasts := tc.extract_smartcasts(cond_id)
		if smartcasts.len > 0 {
			saved_smartcasts = clone_smartcasts(tc.smartcasts)
			has_cond_smartcasts = true
			for sc in smartcasts {
				if valid_string_data(sc.name) {
					tc.smartcasts[sc.name] = sc.typ
				}
			}
		}
	}
	unsafe_alias_base := tc.fn_context.unsafe_reference_alias_owners.clone()
	loop_may_skip_body := node.children_count > 1 && tc.a.child_node(&node, 1).kind != .empty
	$if ownership ? {
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				if has_cond_smartcasts {
					body_smartcasts := clone_smartcasts(tc.smartcasts)
					tc.smartcasts = clone_smartcasts(saved_smartcasts)
					tc.ownership_begin_suppressed_checks()
					tc.check_node(post_id)
					tc.ownership_end_suppressed_checks()
					tc.smartcasts = clone_smartcasts(body_smartcasts)
				} else {
					tc.ownership_begin_suppressed_checks()
					tc.check_node(post_id)
					tc.ownership_end_suppressed_checks()
				}
			}
		}
		tc.ownership_begin_loop_branch_group()
	} $else {
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				if has_cond_smartcasts {
					body_smartcasts := clone_smartcasts(tc.smartcasts)
					tc.smartcasts = clone_smartcasts(saved_smartcasts)
					tc.check_node(post_id)
					tc.smartcasts = clone_smartcasts(body_smartcasts)
				} else {
					tc.check_node(post_id)
				}
			}
		}
	}
	unsafe_alias_post := tc.fn_context.unsafe_reference_alias_owners.clone()
	tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_base.clone()
	tc.fn_context.unsafe_alias_break_states << []map[string]bool{}
	mut sequence_exited := false
	mut unreachable_id := flat.empty_node
	for i in 3 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .label_stmt {
			sequence_exited = false
			unreachable_id = flat.empty_node
		} else if sequence_exited && !tc.valid_node_id(unreachable_id) {
			unreachable_id = child_id
		}
		tc.check_stmt_node(child_id)
		if tc.statement_exits_sequence(child_id, child) {
			sequence_exited = true
		}
	}
	unsafe_alias_break_states := tc.take_unsafe_reference_alias_loop_break_states()
	unsafe_alias_body := tc.fn_context.unsafe_reference_alias_owners.clone()
	if tc.valid_node_id(unreachable_id) && tc.should_diagnose(unreachable_id) {
		tc.record_error_at(.return_mismatch, 'unreachable code', unreachable_id,
			tc.unreachable_statement_diagnostic_pos(unreachable_id))
	}
	$if ownership ? {
		body_reaches_post := tc.ownership_statement_sequence_can_reach_loop_post(node, 3)
		if node.children_count > 2 {
			post_id := tc.a.child(&node, 2)
			if int(post_id) >= 0 {
				post_frame := tc.ownership_snapshot_frame()
				tc.check_node(post_id)
				if !body_reaches_post {
					tc.ownership_restore_frame(post_frame)
				}
			}
			tc.ownership_apply_loop_continue_snapshots(post_id)
		} else {
			tc.ownership_merge_loop_continue_snapshots()
		}
		tc.ownership_record_current_loop_iteration_drops()
		if body_reaches_post {
			tc.ownership_end_loop_branch(node, 3)
		}
		if loop_may_skip_body {
			tc.ownership_add_branch_group_base()
		}
		tc.ownership_end_branch_group()
	}
	body_may_break := tc.unsafe_alias_statement_sequence_may_break(node, 3)
	body_reaches_post := tc.unsafe_alias_statement_sequence_can_reach_loop_post(node, 3)
	mut unsafe_alias_paths := []map[string]bool{}
	if loop_may_skip_body {
		unsafe_alias_paths << unsafe_alias_base
	}
	if unsafe_alias_break_states.len > 0 {
		for state in unsafe_alias_break_states {
			unsafe_alias_paths << state.clone()
		}
	} else if body_may_break {
		unsafe_alias_paths << unsafe_alias_body
	}
	if body_reaches_post && (loop_may_skip_body || body_may_break) {
		unsafe_alias_paths << apply_unsafe_reference_alias_state_delta(unsafe_alias_base,
			unsafe_alias_post, unsafe_alias_body)
	}
	if unsafe_alias_paths.len == 0 {
		unsafe_alias_paths << unsafe_alias_base
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_base)
	if has_cond_smartcasts {
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
	}
	tc.pop_scope()
}

fn (mut tc TypeChecker) record_unsafe_reference_alias_loop_break_state() {
	if tc.fn_context.unsafe_alias_break_states.len == 0 {
		return
	}
	index := tc.fn_context.unsafe_alias_break_states.len - 1
	tc.fn_context.unsafe_alias_break_states[index] << tc.fn_context.unsafe_reference_alias_owners.clone()
}

fn (mut tc TypeChecker) take_unsafe_reference_alias_loop_break_states() []map[string]bool {
	if tc.fn_context.unsafe_alias_break_states.len == 0 {
		return []map[string]bool{}
	}
	index := tc.fn_context.unsafe_alias_break_states.len - 1
	mut result := []map[string]bool{cap: tc.fn_context.unsafe_alias_break_states[index].len}
	for state in tc.fn_context.unsafe_alias_break_states[index] {
		result << state.clone()
	}
	tc.fn_context.unsafe_alias_break_states.delete_last()
	return result
}

fn (tc &TypeChecker) unsafe_alias_statement_sequence_may_break(node flat.Node, body_start int) bool {
	for i in body_start .. node.children_count {
		if tc.unsafe_alias_stmt_may_break(tc.a.child(&node, i)) {
			return true
		}
		if tc.unsafe_alias_stmt_definitely_exits_before_loop_post(tc.a.child(&node, i)) {
			return false
		}
	}
	return false
}

fn (tc &TypeChecker) unsafe_alias_stmt_may_break(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	match node.kind {
		.break_stmt {
			return true
		}
		.for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return tc.unsafe_alias_statement_sequence_may_break(*node, 0)
		}
		.if_expr {
			for i in 1 .. node.children_count {
				if tc.unsafe_alias_stmt_may_break(tc.a.child(node, i)) {
					return true
				}
			}
			return false
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				branch := tc.a.child_node(node, i)
				if branch.kind == .match_branch && tc.unsafe_alias_match_branch_may_break(*branch) {
					return true
				}
			}
			return false
		}
		else {}
	}
	for i in 0 .. node.children_count {
		if tc.unsafe_alias_stmt_may_break(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) unsafe_alias_statement_sequence_can_reach_loop_post(node flat.Node, body_start int) bool {
	for i in body_start .. node.children_count {
		if tc.unsafe_alias_stmt_definitely_exits_before_loop_post(tc.a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) unsafe_alias_stmt_definitely_exits_before_loop_post(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	match node.kind {
		.break_stmt, .return_stmt {
			return true
		}
		.continue_stmt, .for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return !tc.unsafe_alias_statement_sequence_can_reach_loop_post(*node, 0)
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.unsafe_alias_stmt_definitely_exits_before_loop_post(tc.a.child(node, 1))
				&& tc.unsafe_alias_stmt_definitely_exits_before_loop_post(tc.a.child(node, 2))
		}
		.match_stmt {
			if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(*node) {
				return false
			}
			for i in 1 .. node.children_count {
				branch := tc.a.child_node(node, i)
				if branch.kind != .match_branch
					|| !tc.unsafe_alias_match_branch_definitely_exits_before_loop_post(*branch) {
					return false
				}
			}
			return true
		}
		else {}
	}
	return false
}

fn (tc &TypeChecker) unsafe_alias_match_branch_may_break(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return tc.unsafe_alias_statement_sequence_may_break(branch, body_start)
}

fn (tc &TypeChecker) unsafe_alias_match_branch_definitely_exits_before_loop_post(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return !tc.unsafe_alias_statement_sequence_can_reach_loop_post(branch, body_start)
}

fn (mut tc TypeChecker) check_lock_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.lock_depth > 0 {
		tc.record_error_at(.assignment_mismatch, 'nested `lock`/`rlock` not allowed', id,
			tc.lock_keyword_diagnostic_pos(node))
	}
	mut locked_names := []string{}
	for i in 0 .. node.children_count - 1 {
		object_id := tc.a.child(&node, i)
		object := tc.a.node(object_id)
		mode := tc.lock_object_mode(node, i)
		tc.check_node(object_id)
		if !tc.expr_is_shared_arg(object_id) {
			object_type := if object.kind == .ident { 'variable' } else { 'struct element' }
			tc.record_error_at(.assignment_mismatch,
				'`${tc.source_text_for_node(object_id)}` must be declared as `shared` ${object_type} to be locked',
				object_id, object.pos)
		}
		if object.kind == .ident && object.value.len > 0 {
			existing_modes := tc.fn_context.locked_shared_modes[object.value] or { []u8{} }
			if existing_modes.len > 0 {
				message := if existing_modes.last() == `r` {
					'`${object.value}` is already read-locked'
				} else {
					'`${object.value}` is already locked'
				}
				tc.record_error_at(.assignment_mismatch, message, object_id,
					tc.node_value_diagnostic_pos(object_id))
			}
			mut modes := existing_modes.clone()
			modes << mode
			tc.fn_context.locked_shared_modes[object.value] = modes
			tc.fn_context.locked_shared_names[object.value]++
			locked_names << object.value
		}
	}
	tc.lock_depth++
	tc.check_stmt_node(tc.a.child(&node, node.children_count - 1))
	tc.lock_depth--
	for name in locked_names {
		tc.fn_context.locked_shared_names[name]--
		mut modes := (tc.fn_context.locked_shared_modes[name] or { []u8{} }).clone()
		if modes.len > 0 {
			modes.delete_last()
		}
		if modes.len == 0 {
			tc.fn_context.locked_shared_modes.delete(name)
		} else {
			tc.fn_context.locked_shared_modes[name] = modes
		}
	}
}

fn (tc &TypeChecker) lock_object_mode(node flat.Node, index int) u8 {
	if node.value == 'rlock' {
		return `r`
	}
	if node.value.starts_with('lock_modes:') {
		modes := node.value['lock_modes:'.len..]
		if index < modes.len {
			return modes[index]
		}
	}
	return `w`
}

fn (tc &TypeChecker) lock_keyword_diagnostic_pos(node flat.Node) token.Pos {
	if node.children_count == 0 {
		return node.pos
	}
	object := tc.a.child_node(&node, 0)
	file := tc.a.source_files[object.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	line_start := if relative := source[..int_min(object.pos.offset, source.len)].last_index('\n') {
		relative + 1
	} else {
		0
	}
	keyword := if node.value == 'rlock' { 'rlock' } else { 'lock' }
	if relative := source[line_start..int_min(object.pos.offset, source.len)].last_index(keyword) {
		start := line_start + relative
		return token.new_span(object.pos.id, start, start + keyword.len)
	}
	return node.pos
}

// check_for_in_stmt validates check for in stmt state for types.
fn (mut tc TypeChecker) check_for_in_stmt(node flat.Node) {
	header := node.value.int()
	if header < 3 || node.children_count < 3 {
		return
	}
	tc.push_scope()
	key_id := tc.a.child(&node, 0)
	val_id := tc.a.child(&node, 1)
	container_id := tc.a.child(&node, 2)
	has_val := int(val_id) >= 0
	key_conflicts := tc.check_loop_var_const_conflict(key_id)
	mut val_conflicts := false
	if has_val {
		val_conflicts = tc.check_loop_var_const_conflict(val_id)
	}
	if key_conflicts || val_conflicts {
		tc.pop_scope()
		return
	}
	tc.check_for_in_binding_name(key_id, if has_val { 'key' } else { 'value' })
	if has_val {
		tc.check_for_in_binding_name(val_id, 'value')
	}
	same_low, same_high := tc.check_for_in_same_variable(header, key_id, val_id, container_id, if header == 4 {
		tc.a.child(&node, 3)
	} else {
		flat.NodeId(-1)
	})
	if !same_low && !(header == 3 && same_high) {
		tc.check_node(container_id)
	}
	container_node := tc.a.node(container_id)
	if header == 3 && container_node.kind == .range && container_node.children_count >= 2 {
		tc.check_for_in_range_types(tc.a.child(container_node, 0), tc.a.child(container_node, 1))
	}
	if header == 4 {
		range_end_id := tc.a.child(&node, 3)
		if !same_high {
			tc.check_node(range_end_id)
		}
		tc.check_for_in_range_types(container_id, range_end_id)
		tc.insert_loop_var(key_id, tc.range_loop_var_type(container_id))
	} else {
		raw_container_type := unalias_type(unwrap_pointer(tc.resolve_type(container_id)))
		raw_container_name := tc.resolve_type(container_id).name()
		mut clean := tc.for_in_iterable_type(container_id)
		if raw_container_type is ResultType {
			pos := token.new_span(container_node.pos.id, container_node.pos.offset + 3,

				container_node.pos.offset + 10)
			tc.record_error_at(.cannot_index, 'for in: cannot index `${raw_container_name}`',
				container_id, pos)
			clean = unalias_type(unwrap_pointer(raw_container_type.base_type))
		} else if raw_container_type is OptionType {
			tc.record_error_at(.cannot_index, 'for in: cannot index `${raw_container_name}`',
				container_id, container_node.pos)
			clean = unalias_type(unwrap_pointer(raw_container_type.base_type))
		}
		yields_ref := node.op == .amp || tc.for_in_iterable_yields_ref(container_id)
		if same_low && container_node.kind == .range {
			tc.insert_loop_var(key_id, Type(int_))
		} else if same_low {
			if has_val {
				tc.record_error_at(.cannot_index, 'for in: cannot index `int`', container_id,
					tc.node_value_diagnostic_pos(container_id))
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(void_))
			} else {
				tc.insert_loop_var(key_id, Type(void_))
			}
		} else if clean is Array {
			tc.check_for_in_mutable_container(node, key_id, val_id, container_id, clean)
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, elem_type)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, elem_type)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is ArrayFixed {
			tc.check_for_in_mutable_container(node, key_id, val_id, container_id, clean)
			elem_type := for_in_ref_binding_type(clean.elem_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, elem_type)
				} else {
					tc.insert_loop_var(val_id, elem_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, elem_type)
				} else {
					tc.insert_loop_var(key_id, elem_type)
				}
			}
		} else if clean is Map {
			tc.check_for_in_mutable_container(node, key_id, val_id, container_id, clean)
			if !has_val {
				tc.record_error_at(.cannot_index,
					'declare a key and a value variable when ranging a map: `for key, val in map {`\nuse `_` if you do not need the variable',
					key_id, tc.node_value_diagnostic_pos(key_id))
			}
			value_type := for_in_ref_binding_type(clean.value_type, yields_ref)
			if has_val {
				tc.insert_loop_var(key_id, clean.key_type)
				if node.op == .amp {
					tc.insert_mut_loop_var(val_id, value_type)
				} else {
					tc.insert_loop_var(val_id, value_type)
				}
			} else {
				if node.op == .amp {
					tc.insert_mut_loop_var(key_id, value_type)
				} else {
					tc.insert_loop_var(key_id, value_type)
				}
			}
		} else if clean is String {
			tc.check_for_in_mutable_container(node, key_id, val_id, container_id, clean)
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, Type(u8_))
			} else {
				tc.insert_loop_var(key_id, Type(u8_))
			}
		} else if generic_name := tc.iterator_unbounded_next_generic(clean) {
			tc.record_error_with_details_at(.cannot_index,
				'cannot infer from generic type `${generic_name}`', key_id,
				tc.node_value_diagnostic_pos(key_id), [
				'type parameters defined by `next()` method should be bounded by method owner type',
			])
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, unknown_type('unbounded iterator generic'))
			} else {
				tc.insert_loop_var(key_id, unknown_type('unbounded iterator generic'))
			}
		} else if elem_type := tc.iterator_for_in_elem_type(clean) {
			if unalias_type(elem_type) is MultiReturn {
				tc.record_error_at(.cannot_index,
					'iterator method `next()` must not return multiple values', container_id,
					container_node.pos)
			}
			if has_val {
				tc.insert_loop_var(key_id, Type(int_))
				tc.insert_loop_var(val_id, elem_type)
			} else {
				tc.insert_loop_var(key_id, elem_type)
			}
		} else {
			container := tc.a.nodes[int(container_id)]
			if container.kind == .range {
				if same_low || same_high {
					tc.insert_loop_var(key_id, Type(int_))
				} else {
					tc.insert_loop_var(key_id, tc.range_loop_var_type(tc.a.child(&container, 0)))
				}
			} else if (clean is Unknown || clean is Void) && tc.expr_subtree_has_error(container_id) {
				if has_val {
					if container.kind == .selector {
						tc.record_for_in_invalid_selector(container_id, container)
					}
					tc.insert_loop_var(key_id, Type(int_))
					if node.op == .amp {
						tc.insert_mut_loop_var(val_id, Type(void_))
					} else {
						tc.insert_loop_var(val_id, Type(void_))
					}
				} else {
					tc.insert_loop_var(key_id, Type(void_))
				}
			} else if tc.should_diagnose(container_id) {
				type_name := if container.kind == .int_literal {
					'int literal'
				} else {
					clean.name()
				}
				tc.record_error(.cannot_index, 'for in: cannot index `${type_name}`', container_id)
				if has_val {
					tc.insert_loop_var(key_id, Type(int_))
					if node.op == .amp {
						tc.insert_mut_loop_var(val_id, Type(void_))
					} else {
						tc.insert_loop_var(val_id, Type(void_))
					}
				} else {
					tc.insert_loop_var(key_id, Type(void_))
				}
			}
		}
		$if ownership ? {
			if node.op != .amp {
				tc.check_for_in_binding_clones(clean, key_id, val_id, has_val)
			}
			if clean is Map && tc.for_in_body_contains_map_delete(node, header, container_id) {
				tc.check_map_delete_snapshot_clones(clean, container_id)
			}
		}
	}
	unsafe_alias_base := tc.fn_context.unsafe_reference_alias_owners.clone()
	tc.fn_context.unsafe_alias_break_states << []map[string]bool{}
	$if ownership ? {
		tc.ownership_begin_loop_branch_group()
		if node.op != .amp {
			tc.ownership_bind_for_in_vars(key_id, val_id, container_id, has_val)
		} else {
			tc.ownership_bind_mut_for_in_var(key_id, val_id, container_id, has_val)
		}
	}
	mut sequence_exited := false
	mut unreachable_id := flat.empty_node
	for i in header .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .label_stmt {
			sequence_exited = false
			unreachable_id = flat.empty_node
		} else if sequence_exited && !tc.valid_node_id(unreachable_id) {
			unreachable_id = child_id
		}
		tc.check_stmt_node(child_id)
		if tc.statement_exits_sequence(child_id, child) {
			sequence_exited = true
		}
	}
	unsafe_alias_break_states := tc.take_unsafe_reference_alias_loop_break_states()
	if tc.valid_node_id(unreachable_id) && tc.should_diagnose(unreachable_id) {
		tc.record_error_at(.return_mismatch, 'unreachable code', unreachable_id,
			tc.unreachable_statement_diagnostic_pos(unreachable_id))
	}
	$if ownership ? {
		tc.ownership_record_current_loop_iteration_drops()
		tc.ownership_merge_loop_continue_snapshots()
		tc.ownership_end_loop_branch(node, header)
		tc.ownership_add_branch_group_base()
		tc.ownership_end_branch_group()
	}
	mut unsafe_alias_paths := [unsafe_alias_base]
	for state in unsafe_alias_break_states {
		unsafe_alias_paths << state.clone()
	}
	if !tc.stmt_sequence_definitely_returns(&node, header) {
		unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_base)
	tc.pop_scope()
}

fn (mut tc TypeChecker) check_loop_var_const_conflict(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .ident || node.value.len == 0 || node.value == '_' {
		return false
	}
	if tc.should_check_source_name(id) && !snake_case_name_is_valid(node.value) {
		tc.check_snake_case_name(id, node.value, 'variable name', tc.node_value_diagnostic_pos(id))
	}
	qname := if tc.cur_module in ['', 'main', 'builtin'] {
		node.value
	} else {
		'${tc.cur_module}.${node.value}'
	}
	if qname in tc.const_types || node.value in tc.const_types
		|| tc.const_key_for_name(node.value) != none {
		message := 'duplicate of a const name `${node.value}`'
		pos := tc.node_value_diagnostic_pos(id)
		if !tc.errors.any(it.msg == message && it.pos == pos) {
			tc.record_error_at(.duplicate_decl, message, id, pos)
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) check_for_in_const_conflicts_preflight() {
	for i, node in tc.a.nodes {
		if node.kind != .for_in_stmt || node.value.int() < 3 || node.children_count < 2 {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		if tc.diagnostic_files.len > 0 && file.name !in tc.diagnostic_files {
			continue
		}
		module_name := tc.file_modules[file.name] or { 'main' }
		has_value := int(tc.a.child(&node, 1)) >= 0
		count := if has_value { 2 } else { 1 }
		for child_index in 0 .. count {
			binding_id := tc.a.child(&node, child_index)
			binding := tc.a.node(binding_id)
			if binding.kind != .ident || binding.value.len == 0 || binding.value == '_' {
				continue
			}
			qname := if module_name in ['', 'main', 'builtin'] {
				binding.value
			} else {
				'${module_name}.${binding.value}'
			}
			full_qname := '${if module_name.len > 0 { module_name } else { 'main' }}.${binding.value}'
			if qname !in tc.const_types && full_qname !in tc.const_types
				&& binding.value !in tc.const_types {
				continue
			}
			saved_file := tc.cur_file
			saved_module := tc.cur_module
			tc.cur_file = file.name
			tc.cur_module = module_name
			tc.record_error_at(.duplicate_decl, 'duplicate of a const name `${binding.value}`',
				flat.NodeId(i), tc.node_value_diagnostic_pos(binding_id))
			tc.cur_file = saved_file
			tc.cur_module = saved_module
		}
	}
}

fn (mut tc TypeChecker) check_for_in_binding_name(id flat.NodeId, role string) {
	if !tc.valid_node_id(id) || !tc.should_check_source_name(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind != .ident || node.value.len == 0 || node.value == '_' {
		return
	}
	if reserved_const_type_name(node.value) {
		tc.record_error_at(.duplicate_decl,
			'invalid use of reserved type `${node.value}` as ${role} name', id,
			tc.node_value_diagnostic_pos(id))
		return
	}
	parent := tc.cur_scope.parent
	if parent != unsafe { nil } {
		for name in parent.names {
			if name == node.value {
				tc.record_error_at(.duplicate_decl,
					'redefinition of ${role} iteration variable `${node.value}`', id,
					tc.node_value_diagnostic_pos(id))
				return
			}
		}
	}
}

fn (mut tc TypeChecker) check_for_in_same_variable(header int, key_id flat.NodeId, val_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId) (bool, bool) {
	if !tc.valid_node_id(key_id) || !tc.valid_node_id(low_id) {
		return false, false
	}
	mut actual_low_id := low_id
	mut actual_high_id := high_id
	mut is_range := header == 4
	container := tc.a.node(low_id)
	if header == 3 && container.kind == .range && container.children_count >= 2 {
		actual_low_id = tc.a.child(container, 0)
		actual_high_id = tc.a.child(container, 1)
		is_range = true
	}
	if !tc.should_check_source_name(actual_low_id) {
		return false, false
	}
	key := tc.a.node(key_id)
	if key.kind != .ident {
		return false, false
	}
	mut iteration_name := key.value
	if tc.valid_node_id(val_id) {
		val := tc.a.node(val_id)
		if val.kind == .ident {
			iteration_name = val.value
		}
	}
	low := tc.a.node(actual_low_id)
	mut same_low := low.kind == .ident && low.value == key.value
	if !same_low && tc.valid_node_id(val_id) {
		val := tc.a.node(val_id)
		same_low = low.kind == .ident && low.value == val.value
	}
	if same_low {
		kind := if is_range { '<range>' } else { 'array' }
		tc.record_error_at(.cannot_index,
			'in a `for x in ${kind}` loop, the key or value iteration variable `${iteration_name}` can not be the same as the low variable',
			actual_low_id, tc.node_value_diagnostic_pos(actual_low_id))
	}
	if !is_range || !tc.valid_node_id(actual_high_id) {
		return same_low, false
	}
	high := tc.a.node(actual_high_id)
	mut same_high := high.kind == .ident && high.value == key.value
	if !same_high && tc.valid_node_id(val_id) {
		val := tc.a.node(val_id)
		same_high = high.kind == .ident && high.value == val.value
	}
	if same_high && !same_low {
		tc.record_error_at(.cannot_index,
			'in a `for x in <range>` loop, the key or value iteration variable `${iteration_name}` can not be the same as the high variable',
			actual_high_id, tc.node_value_diagnostic_pos(actual_high_id))
	}
	return same_low, same_high
}

fn (mut tc TypeChecker) check_for_in_mutable_container(loop flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, container_type Type) {
	if loop.op != .amp || !tc.valid_node_id(container_id) {
		return
	}
	mut binding_id := key_id
	if tc.valid_node_id(val_id) {
		binding_id = val_id
	}
	if unalias_type(container_type) is String {
		tc.record_error_at(.assignment_mismatch, 'string type is immutable, it cannot be changed',
			binding_id, tc.for_in_mut_keyword_pos(binding_id))
		return
	}
	if tc.expr_root_is_mutable_lvalue(container_id) {
		return
	}
	mut diagnostic_id := container_id
	mut container := tc.a.node(diagnostic_id)
	for container.kind in [.paren, .or_expr, .postfix] && container.children_count > 0 {
		diagnostic_id = tc.a.child(container, 0)
		container = tc.a.node(diagnostic_id)
	}
	if container.kind == .ident {
		tc.record_error_at(.assignment_mismatch,
			'`${container.value}` is immutable, it cannot be changed', container_id,
			tc.node_value_diagnostic_pos(diagnostic_id))
		return
	}
	if container.kind == .selector && container.children_count > 0 {
		base_id := tc.a.child(container, 0)
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(base_id))
		if base_type is Struct {
			tc.record_error_at(.assignment_mismatch,
				'field `${base_type.name}.${container.value}` is immutable, it cannot be changed',
				container_id, tc.node_value_diagnostic_pos(diagnostic_id))
			return
		}
	}
	if container.kind == .map_init {
		pos := token.new_span(container.pos.id, container.pos.offset, container.pos.offset + 1)
		tc.record_error_at(.assignment_mismatch, 'map literal is immutable, it cannot be changed',
			container_id, pos)
		return
	}
	if container.kind in [.array_literal, .array_init] {
		tc.record_error_at(.assignment_mismatch,
			'array literal is immutable, it cannot be changed', container_id,
			tc.a.node(container_id).pos)
	}
}

fn (tc &TypeChecker) for_in_mut_keyword_pos(binding_id flat.NodeId) token.Pos {
	if !tc.valid_node_id(binding_id) {
		return token.Pos{}
	}
	binding := tc.a.node(binding_id)
	file := tc.a.source_files[binding.pos.id] or { return binding.pos }
	source := tc.source_texts_by_file[file.name] or { return binding.pos }
	end := int_min(int_max(binding.pos.end, 0), source.len)
	mut line_start := int_min(int_max(binding.pos.offset, 0), end)
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	if relative := source[line_start..end].last_index('mut ') {
		start := line_start + relative
		return token.new_span(binding.pos.id, start, start + 3)
	}
	return binding.pos
}

fn (mut tc TypeChecker) record_for_in_invalid_selector(id flat.NodeId, node flat.Node) {
	if node.kind != .selector || node.children_count == 0 {
		return
	}
	base := tc.a.child_node(&node, 0)
	if base.kind != .ident {
		return
	}
	tc.record_error_at(.cannot_index, '`${base.value}` does not return a value', id,
		tc.node_value_diagnostic_pos(id))
}

fn (mut tc TypeChecker) check_for_in_range_types(low_id flat.NodeId, high_id flat.NodeId) {
	low_type := unalias_type(tc.resolve_type(low_id))
	high_type := unalias_type(tc.resolve_type(high_id))
	if low_type is Unknown || high_type is Unknown {
		return
	}
	if low_type is MultiReturn || high_type is MultiReturn {
		tc.record_error_at(.condition_mismatch,
			'multi-returns cannot be used in ranges. A range is from a single value to a single higher value.',
			low_id, tc.range_endpoints_pos(low_id, high_id))
		return
	}
	if tc.a.node(low_id).kind == .none_expr && tc.a.node(high_id).kind == .none_expr {
		tc.record_error_at(.condition_mismatch, 'range type can only be an integer type', low_id, tc.range_endpoints_pos(low_id,
			high_id))
		return
	}
	if low_type is OptionType || low_type is ResultType {
		tc.record_error_at(.condition_mismatch,
			'the `low` value in a `for x in low..high {` loop, cannot be Result or Option', low_id,
			tc.a.node(low_id).pos)
		return
	}
	if high_type is OptionType || high_type is ResultType {
		tc.record_error_at(.condition_mismatch,
			'the `high` value in a `for x in low..high {` loop, cannot be Result or Option',
			high_id, tc.range_bound_diagnostic_pos(high_id))
		return
	}
	low_is_numeric := low_type.is_integer() || low_type.is_float()
	high_is_numeric := high_type.is_integer() || high_type.is_float()
	if low_is_numeric && high_is_numeric && (low_type.is_integer() != high_type.is_integer()
		|| (!tc.range_endpoint_is_literal(low_id) && !tc.range_endpoint_is_literal(high_id)
		&& low_type.name() != high_type.name())) {
		tc.record_error_at(.condition_mismatch, 'range types do not match', low_id,
			tc.a.node(low_id).pos)
		return
	}
	if !low_type.is_integer() || !high_type.is_integer() {
		tc.record_error_at(.condition_mismatch, 'range type can only be an integer type', low_id, tc.range_endpoints_pos(low_id,
			high_id))
		return
	}
	if low := tc.match_condition_int_value(low_id) {
		if high := tc.match_condition_int_value(high_id) {
			if low > high {
				tc.record_error_at(.condition_mismatch,
					'empty range: `${low} .. ${high}` will never execute', low_id, tc.range_endpoints_pos(low_id,
					high_id))
			}
		}
	}
}

fn (tc &TypeChecker) range_bound_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .call && node.children_count > 0 {
		callee := tc.a.child_node(node, 0)
		if callee.kind == .selector {
			return tc.method_call_name_pos(node, callee)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) range_endpoint_is_literal(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.int_literal, .float_literal, .char_literal] {
		return true
	}
	if node.kind == .ident {
		for candidate in [tc.qualify_name(node.value), node.value] {
			if expr_id := tc.const_exprs[candidate] {
				return tc.range_endpoint_is_literal(expr_id)
			}
		}
	}
	return false
}

fn (tc &TypeChecker) range_endpoints_pos(low_id flat.NodeId, high_id flat.NodeId) token.Pos {
	low := tc.a.node(low_id)
	high := tc.a.node(high_id)
	if low.pos.id == high.pos.id && low.pos.is_valid() && high.pos.is_valid() {
		return token.new_span(low.pos.id, low.pos.offset, high.pos.end)
	}
	return low.pos
}

fn (tc &TypeChecker) for_in_body_contains_map_delete(node flat.Node, body_start int, container_id flat.NodeId) bool {
	container_key := tc.for_in_map_storage_key(container_id)
	if container_key.len == 0 {
		return false
	}
	for i in body_start .. node.children_count {
		if tc.for_in_node_contains_map_delete(tc.a.child(&node, i), container_key) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) for_in_node_contains_map_delete(id flat.NodeId, container_key string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if node.kind == .call && node.children_count > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.value == 'delete' && fn_node.children_count > 0 {
			receiver_id := tc.a.child(fn_node, 0)
			if tc.for_in_map_storage_key(receiver_id) == container_key {
				return true
			}
		}
		if fn_node.kind == .ident && fn_node.value in ['map.delete', 'map__delete']
			&& node.children_count > 1 {
			receiver_id := tc.a.child(&node, 1)
			if tc.for_in_map_storage_key(receiver_id) == container_key {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if tc.for_in_node_contains_map_delete(tc.a.child(&node, i), container_key) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) for_in_map_storage_key(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt, .cast_expr, .as_expr] && node.children_count > 0 {
		return tc.for_in_map_storage_key(tc.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op in [.amp, .mul] && node.children_count > 0 {
		return tc.for_in_map_storage_key(tc.a.child(&node, 0))
	}
	return tc.expr_key(id)
}

fn (mut tc TypeChecker) check_map_delete_snapshot_clones(map_type Map, pos flat.NodeId) {
	if bad_type := tc.ownership_default_clone_missing_method(map_type.key_type) {
		tc.record_error(.call_arg_mismatch,
			'cannot snapshot map keys for iteration with delete: `${bad_type}` requires ownership destruction but has no `clone()` method',
			pos)
	}
	if bad_type := tc.ownership_default_clone_missing_method(map_type.value_type) {
		tc.record_error(.call_arg_mismatch,
			'cannot snapshot map values for iteration with delete: `${bad_type}` requires ownership destruction but has no `clone()` method',
			pos)
	}
}

fn (mut tc TypeChecker) check_for_in_binding_clones(container Type, key_id flat.NodeId, val_id flat.NodeId, has_val bool) {
	match container {
		Array {
			target_id := if has_val { val_id } else { key_id }
			tc.check_for_in_binding_clone(target_id, container.elem_type, 'array element')
		}
		ArrayFixed {
			target_id := if has_val { val_id } else { key_id }
			tc.check_for_in_binding_clone(target_id, container.elem_type, 'fixed-array element')
		}
		Map {
			if has_val {
				tc.check_for_in_binding_clone(key_id, container.key_type, 'map key')
				tc.check_for_in_binding_clone(val_id, container.value_type, 'map value')
			} else {
				tc.check_for_in_binding_clone(key_id, container.value_type, 'map value')
			}
		}
		else {}
	}
}

fn (mut tc TypeChecker) check_for_in_binding_clone(target_id flat.NodeId, typ Type, role string) {
	if !tc.valid_node_id(target_id) || tc.a.nodes[int(target_id)].value == '_' {
		return
	}
	if bad_type := tc.ownership_default_clone_missing_method(typ) {
		tc.record_error(.call_arg_mismatch,
			'cannot iterate over ownership-bearing ${role}: `${bad_type}` requires ownership destruction but has no `clone()` method',
			target_id)
	}
}

fn (mut tc TypeChecker) check_storage_path_base_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	$if ownership ? {
		node := tc.a.nodes[int(id)]
		if node.kind in [.ident, .selector, .index] {
			tc.ownership_begin_suppressed_checks()
			tc.check_node(id)
			tc.ownership_end_suppressed_checks()
			return
		}
	}
	tc.check_node(id)
}

// check_decl_assign validates check decl assign state for types.
fn (mut tc TypeChecker) check_decl_assign(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.check_multi_return_decl_assign(id, node) {
		return
	}
	if tc.check_multi_value_list_decl_assign(id, node) {
		return
	}
	if tc.check_assignment_marker(id, node) {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_node := tc.a.nodes[int(lhs_id)]
		if lhs_node.kind != .ident {
			tc.check_node(rhs_id)
			if tc.record_invalid_decl_assign_lhs(node, lhs_id, rhs_id) {
				i += 2
				continue
			}
			if lhs_node.kind == .selector && lhs_node.children_count > 0 {
				base := tc.a.child_node(&lhs_node, 0)
				if !tc.is_namespace_selector(lhs_node, base) {
					tc.record_error_at(.assignment_mismatch,
						'use assignment `=` instead of declaration `:=` when modifying struct fields',
						lhs_id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
					i += 2
					continue
				}
			}
			tc.record_error_at(.assignment_mismatch,
				'non-name `${tc.source_text_for_node(lhs_id)}` on left side of `:=`', lhs_id,
				tc.decl_non_name_lhs_pos(lhs_id))
			i += 2
			continue
		}
		if node.value == 'static' && tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
			&& !tc.current_fn_declared_unsafe() {
			tc.record_error_at(.assignment_mismatch,
				'static variables are supported only in -translated mode, `unsafe{}` blocks, or in `@[unsafe] fn`',
				lhs_id, tc.node_value_diagnostic_pos(lhs_id))
		}
		tc.check_import_symbol_conflict(lhs_id, lhs_node.value)
		tc.check_module_name_conflict(lhs_id, lhs_node.value)
		if reserved_const_type_name(lhs_node.value) && tc.should_diagnose(lhs_id) {
			tc.record_error_at(.duplicate_decl,
				'invalid use of reserved type `${lhs_node.value}` as a variable name', lhs_id,
				lhs_node.pos)
		}
		if tc.should_check_source_name(lhs_id) && !snake_case_name_is_valid(lhs_node.value) {
			tc.check_snake_case_name(lhs_id, lhs_node.value, 'variable name',
				tc.node_value_diagnostic_pos(lhs_id))
		}
		if lhs_node.value != '_' {
			if _ := tc.const_key_for_name(lhs_node.value) {
				tc.record_warning_at(.duplicate_decl,
					'duplicate of a const name `${tc.qualify_name(lhs_node.value)}`', lhs_id,
					tc.node_value_diagnostic_pos(lhs_id))
			}
		}
		mut shadows_fn := lhs_node.value in tc.fn_ret_types
			|| tc.qualify_fn_name(lhs_node.value) in tc.fn_ret_types
		if shadows_fn && tc.imported_module_prefix(lhs_id, lhs_node.value) != none
			&& !tc.source_module_declares_fn(lhs_node.value) {
			shadows_fn = false
		}
		if shadows_fn {
			tc.record_notice_at(.unknown_ident,
				'variable `${lhs_node.value}` shadows a function declaration', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
		}
		explicit_expected := if node.children_count == 2 && node.typ.len > 0 {
			tc.parse_type(node.typ)
		} else {
			Type(void_)
		}
		saved_expected_expr_id := tc.expected_expr_id
		saved_expected_expr_type := tc.expected_expr_type
		if explicit_expected !is Void {
			tc.expected_expr_id = int(rhs_id)
			tc.expected_expr_type = explicit_expected
		}
		error_count := tc.errors.len
		tc.fn_context.undefined_variable_context_depth++
		$if ownership ? {
			if tc.ownership_should_defer_aggregate_consumption(lhs_id, .assign) {
				tc.ownership_begin_defer_aggregate_consumption(rhs_id)
				tc.check_node(rhs_id)
				tc.ownership_end_defer_aggregate_consumption(rhs_id)
			} else {
				tc.check_node(rhs_id)
			}
		} $else {
			tc.check_node(rhs_id)
		}
		tc.fn_context.undefined_variable_context_depth--
		tc.expected_expr_id = saved_expected_expr_id
		tc.expected_expr_type = saved_expected_expr_type
		rhs_node := tc.a.nodes[int(rhs_id)]
		if rhs_node.kind == .struct_init && tc.struct_init_has_positional_fields(rhs_node)
			&& !tc.type_name_known(rhs_node.value) {
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, Type(void_))
			}
			i += 2
			continue
		}
		if rhs_node.kind == .call {
			rhs_type := unalias_type(tc.resolve_type(rhs_id))
			if rhs_type is ResultType {
				tc.record_unhandled_result_call(rhs_id, rhs_type)
			}
		}
		if rhs_node.kind == .ident && tc.lock_depth == 0
			&& tc.current_binding_is_shared(rhs_node.value) {
			tc.record_error_at(.assignment_mismatch,
				'`${rhs_node.value}` is `shared` and must be `rlock`ed or `lock`ed to be used as non-mut right-hand side of assignment',
				rhs_id, rhs_node.pos)
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, tc.resolve_type(rhs_id))
			}
			i += 2
			continue
		}
		if rhs_node.kind == .map_init && rhs_node.children_count == 0 && rhs_node.value.len == 0 {
			tc.record_error_at(.assignment_mismatch,
				'invalid empty map initialisation syntax, use e.g. map[string]int{} instead',
				rhs_id, rhs_node.pos)
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, Type(void_))
			}
			i += 2
			continue
		}
		if rhs_node.kind == .fn_literal && rhs_node.generic_params().len > 0 {
			tc.record_error_at(.assignment_mismatch,
				'cannot assign generic function to a variable', rhs_id, rhs_node.pos)
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, tc.resolve_type(rhs_id))
			}
			i += 2
			continue
		}
		if rhs_node.kind == .infix && rhs_node.op == .arrow {
			tc.record_error_at(.assignment_mismatch,
				'cannot use `<-` on the right-hand side of an assignment, as it does not return any values',
				rhs_id, tc.infix_operator_pos(rhs_node, '<-'))
			i += 2
			continue
		}
		if unsafe_none_id := tc.unsafe_block_none_expr_id(rhs_id) {
			none_node := tc.a.node(unsafe_none_id)
			tc.record_error_at(.assignment_mismatch, 'cannot use `none` in `unsafe` blocks',
				unsafe_none_id, none_node.pos)
		} else if rhs_node.kind == .none_expr {
			tc.record_error_at(.assignment_mismatch, 'cannot assign a `none` value to a variable',
				rhs_id, rhs_node.pos)
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, Type(none_))
			}
			i += 2
			continue
		}
		if rhs_node.kind == .nil_literal && tc.unsafe_depth == 0 {
			tc.record_error_at(.assignment_mismatch, '`nil` is only allowed in `unsafe` code',
				rhs_id, rhs_node.pos)
			tc.record_error_at(.assignment_mismatch,
				'use of untyped nil in assignment (use `unsafe` | false)', rhs_id, rhs_node.pos)
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, Type(voidptr_))
			}
			i += 2
			continue
		}
		tc.check_const_reference_assignment(lhs_id, rhs_id, true)
		if lhs_node.value != '_' && tc.unsafe_depth == 0 && (tc.decl_lhs_is_mut(node, lhs_id)
			|| tc.slice_expr_base_is_mutable(rhs_id)) {
			tc.record_implicit_slice_clone_notice(rhs_id)
		}
		if tc.new_error_kind_since(error_count, .unknown_ident) {
			if tc.new_errors_are_forward_decl_unknowns(error_count) {
				if lhs_node.value != '_' {
					tc.cur_scope.insert(lhs_node.value,
						unknown_type('initializer contains an unresolved forward declaration'))
				}
				i += 2
				continue
			}
			return
		}
		if tc.new_error_kind_since(error_count, .unknown_fn) {
			has_unknown_method :=
				tc.errors[error_count..].any(it.msg.starts_with('unknown method or field:'))
			if lhs_node.value == '_' && !has_unknown_method {
				i += 2
				continue
			}
		}
		if tc.assignment_rhs_is_void_selector(rhs_id) && !tc.unknown_imported_enum_selector(rhs_id)
			&& !tc.errors.any(it.node == rhs_id && it.msg.ends_with('must be initialized')) {
			tc.record_error_at(.assignment_mismatch, 'assignment mismatch: 1 variable 0 values',
				id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
		}
		mut rhs_type := tc.decl_assign_inferred_type(rhs_id)
		clean_rhs_type := unalias_type(rhs_type)
		if tc.unsafe_depth == 0 && lhs_node.value != '_' && rhs_node.kind == .ident
			&& clean_rhs_type is ArrayFixed && unalias_type(clean_rhs_type.elem_type) is Pointer {
			tc.record_error_at(.assignment_mismatch,
				'assignment from one fixed array to another with a pointer element type is prohibited outside of `unsafe`',
				id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
		}
		if rhs_node.kind == .ident && tc.type_has_declaration_attribute(rhs_type, 'nocopy') {
			tc.record_error_at(.assignment_mismatch,
				'cannot copy @[nocopy] struct: use a reference instead', rhs_id, rhs_node.pos)
		}
		tc.record_non_heap_pointer_param_escape(rhs_id)
		if node.value == 'static' && !tc.static_initializer_is_constant(rhs_id) {
			tc.record_error_at(.assignment_mismatch,
				'cannot initialized static variable with non-constant value', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
		}
		if node.value == 'static' && unalias_type(rhs_type) is Map {
			tc.record_error_at(.assignment_mismatch, 'maps cannot be static', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
		}
		if assigned_fn := fn_type_from_type(rhs_type) {
			return_name := assigned_fn.return_type.name()
			unknown_return := assigned_fn.return_type is Unknown
				|| (should_check_named_type(return_name) && !tc.type_name_known(return_name))
			if unknown_return && rhs_node.kind in [.index, .selector] {
				tc.record_error_at(.unknown_type,
					'unknown return type: cannot assign `${tc.source_text_for_node(rhs_id)}` as a function variable',
					rhs_id, tc.index_suffix_diagnostic_pos(rhs_id))
				if lhs_node.value != '_' {
					tc.cur_scope.insert(lhs_node.value, rhs_type)
				}
				i += 2
				continue
			}
		}
		if rhs_type is MultiReturn {
			call_name := tc.assignment_rhs_call_name(rhs_id) or { '' }
			message := if rhs_node.kind == .dump_expr || call_name.len == 0 {
				'assignment mismatch: 1 variable ${rhs_type.types.len} values'
			} else {
				'assignment mismatch: 1 variable but `${call_name}()` returns ${rhs_type.types.len} values'
			}
			tc.record_error_at(.assignment_mismatch, message, id, tc.assignment_operator_pos(node,
				lhs_id, rhs_id))
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, rhs_type)
			}
			i += 2
			continue
		}
		if rhs_type is Void {
			if tc.expr_calls_invalid_option_void_fn(rhs_id) {
				i += 2
				continue
			}
			if call_name := tc.assignment_rhs_call_name(rhs_id) {
				message := if rhs_node.kind == .paren {
					'assignment mismatch: expected 1 value(s) but `${call_name}()` returns 0 value(s)'
				} else {
					'assignment mismatch: 1 variable but `${call_name}()` returns 0 values'
				}
				tc.record_error_at(.assignment_mismatch, message, id, tc.assignment_operator_pos(node,
					lhs_id, rhs_id))
				if lhs_node.value != '_' {
					tc.cur_scope.insert(lhs_node.value, Type(void_))
				}
				i += 2
				continue
			}
		}
		if rhs_node.kind == .array_literal && rhs_node.children_count == 0 && rhs_node.typ.len == 0 {
			if lhs_node.value != '_' {
				tc.cur_scope.insert(lhs_node.value, Type(void_))
			}
			i += 2
			continue
		}
		if explicit_expected is Void && rhs_type.name() == 'int'
			&& tc.implicit_int_literal_overflows(rhs_id) {
			tc.record_error_at(.assignment_mismatch,
				'overflow in implicit type `int`, use explicit type casting instead', rhs_id,
				rhs_node.pos)
		}
		rhs_is_map_value := unalias_type(rhs_type) is Map
			|| (rhs_node.kind == .ident && tc.mut_value_param_binding_matches_lvalue(rhs_node.value)
			&& unalias_and_unwrap_pointer_type(rhs_type) is Map)
		if tc.unsafe_depth == 0 && !tc.current_fn_declared_unsafe() && rhs_is_map_value
			&& rhs_node.kind == .ident && !tc.expr_is_unsafe_reference_alias(rhs_id) {
			tc.record_error_at(.assignment_mismatch,
				'cannot copy map: call `move` or `clone` method (or use a reference)', rhs_id,
				rhs_node.pos)
		}
		if tc.unsafe_depth == 0 && tc.decl_lhs_is_mut(node, lhs_id)
			&& unalias_type(rhs_type) is Array && rhs_node.kind == .selector {
			tc.record_error_at(.assignment_mismatch,
				'use `mut array2 := array1.clone()` instead of `mut array2 := array1` (or use `unsafe`)',
				id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
		}
		if rhs_type is Void && rhs_node.kind == .infix && rhs_node.op == .left_shift {
			tc.cur_scope.insert(lhs_node.value, rhs_type)
			tc.remember_expr_type(lhs_id, rhs_type)
			i += 2
			continue
		}
		mut expected := rhs_type
		if explicit_expected !is Void {
			expected = explicit_expected
			rhs_type = tc.resolve_expr(rhs_id, expected)
			if !tc.expr_compatible(rhs_id, rhs_type, expected)
				&& !tc.pointer_value_compatible(rhs_type, expected) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${rhs_type.name()}` to `${expected.name()}`', id)
			}
		}
		owner := tc.insert_decl_lhs(lhs_id, expected, tc.decl_lhs_is_mut(node, lhs_id))
		if owner.storage_key().len > 0 && unalias_type(expected) is Map
			&& tc.expr_is_unsafe_reference_alias(rhs_id) {
			tc.fn_context.unsafe_reference_alias_owners[owner.storage_key()] = true
		}
		if lhs_node.value != '_' && tc.decl_lhs_is_mut(node, lhs_id) {
			tc.check_mutable_array_immutable_references(rhs_id)
			tc.record_mutable_decl_branch_immutable_notices(rhs_id)
			if immutable_source_id := tc.immutable_reference_source(rhs_id, rhs_type) {
				source := tc.a.node(immutable_source_id)
				tc.record_notice_at(.assignment_mismatch,
					'`${source.value}` is immutable, cannot have a mutable reference to an immutable object',
					immutable_source_id, tc.node_value_diagnostic_pos(immutable_source_id))
				tc.fn_context.immutable_reference_aliases[lhs_node.value] = true
			}
			if tc.call_immutable_alias_source(rhs_id) != none {
				tc.fn_context.immutable_reference_aliases[lhs_node.value] = true
			}
			if rhs_node.kind == .ident && !tc.ident_is_mutable_lvalue(rhs_node.value)
				&& tc.type_contains_mutable_reference_data(rhs_type) {
				tc.fn_context.immutable_reference_aliases[lhs_node.value] = true
			}
		}
		if owner.storage_key().len > 0 && decl_assign_is_shared_marker(node.value) {
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value.len > 0 {
				tc.mark_shared_binding_owner(lhs.value, owner)
			}
		}
		if owner.storage_key().len > 0 && tc.expr_initializes_shared_array(rhs_id) {
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value.len > 0 {
				tc.mark_shared_array_binding_owner(lhs.value, owner)
			}
		}
		$if ownership ? {
			tc.ownership_after_decl_assign(lhs_id, rhs_id, expected, id)
		}
		tc.track_method_value_local(lhs_id, rhs_id)
		tc.track_variadic_fn_value_local(lhs_id, rhs_id)
		tc.track_capturing_fn_literal_local(lhs_id, rhs_id, owner)
		i += 2
	}
}

fn (tc &TypeChecker) current_fn_declared_unsafe() bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	return tc.valid_node_id(fn_id) && tc.declaration_has_attribute(fn_id, 'unsafe')
}

fn (tc &TypeChecker) expr_is_unsafe_reference_alias(id flat.NodeId) bool {
	return tc.expr_is_reference_alias(id, false)
}

fn (tc &TypeChecker) expr_is_reference_alias(id flat.NodeId, unsafe_context bool) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.block, .match_branch] {
		if node.children_count == 0 {
			return false
		}
		return tc.expr_is_reference_alias(tc.a.child(node, node.children_count - 1), unsafe_context
			|| node.value == 'unsafe')
	}
	if node.kind == .ident {
		if unsafe_context {
			return true
		}
		owner := tc.cur_scope.lookup_owner(node.value) or { return false }
		return tc.fn_context.unsafe_reference_alias_owners[owner.storage_key()]
	}
	if unsafe_context && node.kind in [.selector, .index] {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_is_reference_alias(tc.a.child(node, node.children_count - 1), unsafe_context)
	}
	if node.kind == .if_expr {
		if node.children_count < 3 {
			return false
		}
		return tc.expr_is_reference_alias(tc.a.child(node, 1), unsafe_context)
			&& tc.expr_is_reference_alias(tc.a.child(node, 2), unsafe_context)
	}
	if node.kind == .match_stmt {
		if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(*node) {
			return false
		}
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(node, i)
			if branch.kind != .match_branch
				|| !tc.expr_is_reference_alias(tc.a.child(node, i), unsafe_context) {
				return false
			}
		}
		return true
	}
	return false
}

fn (tc &TypeChecker) static_initializer_is_constant(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.enum_val, .nil_literal, .none_expr, .sizeof_expr, .typeof_expr] {
		return true
	}
	if node.kind == .ident {
		return tc.const_key_for_name(node.value) != none
	}
	if node.kind !in [.infix, .prefix, .paren, .cast_expr, .array_literal, .array_init, .map_init,
		.struct_init, .field_init] {
		return false
	}
	for i in 0 .. node.children_count {
		if !tc.static_initializer_is_constant(tc.a.child(node, i)) {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) check_multi_value_list_decl_assign(id flat.NodeId, node flat.Node) bool {
	lhs_ids := tc.multi_assign_lhs_ids(node)
	rhs_count := tc.multi_assign_rhs_count(node)
	if lhs_ids.len == 0 || rhs_count <= 1 {
		return false
	}
	mut rhs_ids := []flat.NodeId{cap: rhs_count}
	mut multi_ids := []flat.NodeId{}
	mut multi_types := []MultiReturn{}
	mut right_len := rhs_count
	for i in 0 .. rhs_count {
		rhs_id := tc.multi_assign_rhs_id(node, i)
		rhs_ids << rhs_id
		rhs_type := tc.resolve_type(rhs_id)
		if multi := tc.multi_return_assignment_type(rhs_id, rhs_type) {
			multi_ids << rhs_id
			multi_types << multi
			right_len = multi.types.len
		}
	}
	if multi_ids.len == 0 && lhs_ids.len == right_len {
		return false
	}
	for rhs_id in rhs_ids {
		tc.check_node(rhs_id)
	}
	if multi_ids.len > 0 {
		diagnostic_index := 0
		multi_id := multi_ids[diagnostic_index]
		multi_type := multi_types[diagnostic_index]
		tc.record_error_at(.assignment_mismatch,
			'cannot use multi-value ${Type(multi_type).name()} in single-value context', multi_id,
			tc.multi_value_single_context_pos(multi_id))
	}
	if lhs_ids.len != right_len {
		variables := if lhs_ids.len == 1 { 'variable' } else { 'variables' }
		values := if right_len == 1 { 'value' } else { 'values' }
		first_rhs_id := rhs_ids[0]
		message := if call_name := tc.assignment_rhs_call_name(first_rhs_id) {
			'assignment mismatch: ${lhs_ids.len} ${variables} but `${call_name}()` returns ${right_len} ${values}'
		} else {
			'assignment mismatch: ${lhs_ids.len} ${variables} ${right_len} ${values}'
		}
		tc.record_error_at(.assignment_mismatch, message, id, tc.assignment_operator_pos(node,
			lhs_ids[0], first_rhs_id))
	}
	return true
}

fn (tc &TypeChecker) multi_value_single_context_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .if_expr {
		return token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 2)
	}
	if node.kind == .match_stmt {
		file := tc.a.source_files[node.pos.id] or { return node.pos }
		source := tc.source_texts_by_file[file.name] or { return node.pos }
		start := int_max(0, node.pos.offset)
		end := int_min(source.len, node.pos.end)
		if start < end {
			if relative := source[start..end].index('{') {
				return token.new_span(node.pos.id, start, start + relative + 1)
			}
		}
	}
	return tc.array_element_diagnostic_pos(id)
}

fn (mut tc TypeChecker) check_mutable_array_immutable_references(id flat.NodeId) {
	if tc.unsafe_depth > 0 || !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind != .array_literal {
		return
	}
	for i in 0 .. node.children_count {
		elem_id := tc.a.child(node, i)
		elem := tc.a.node(elem_id)
		if elem.kind != .prefix || elem.op != .amp || elem.children_count == 0 {
			continue
		}
		source_id := tc.a.child(elem, 0)
		source := tc.a.node(source_id)
		if source.kind == .ident && !tc.ident_is_mutable_lvalue(source.value)
			&& tc.const_key_for_name(source.value) == none {
			tc.record_warning_at(.assignment_mismatch,
				'cannot add a reference to an immutable object to a mutable array', elem_id,
				tc.address_operator_pos(elem_id))
		}
	}
}

fn (mut tc TypeChecker) record_mutable_decl_branch_immutable_notices(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .if_expr || node.kind == .match_stmt {
		for i in 1 .. node.children_count {
			tc.record_mutable_decl_branch_immutable_notices(tc.branch_tail_expr_id(tc.a.child(node,
				i)))
		}
		return
	}
	if node.kind != .ident || tc.ident_is_mutable_lvalue(node.value) {
		return
	}
	typ := unalias_type(tc.resolve_type(id))
	if typ is Array || typ is ArrayFixed || typ is Map {
		tc.record_notice_at(.assignment_mismatch,
			'left-side of assignment expects a mutable reference, but variable `${node.value}` is immutable, declare it with `mut` to make it mutable or clone it',
			id, tc.node_value_diagnostic_pos(id))
	}
}

fn (tc &TypeChecker) type_contains_mutable_reference_data(typ Type) bool {
	clean := unalias_type(typ)
	if clean is Array || clean is Map {
		return true
	}
	if clean is ArrayFixed {
		return unalias_type(clean.elem_type) is Pointer
	}
	if clean is Struct {
		for field in tc.struct_fields_for_init(clean.name) {
			field_type := unalias_type(field.typ)
			if field_type is Array || field_type is Map {
				return true
			}
		}
	}
	return false
}

fn (mut tc TypeChecker) check_mutable_alias_assignment_lhs(id flat.NodeId) {
	if tc.unsafe_depth > 0 || !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .index && node.children_count > 0 {
		base_id := tc.a.child(node, 0)
		base := tc.a.node(base_id)
		mut aliases :=
			(base.kind == .ident && tc.fn_context.immutable_reference_aliases[base.value])
			|| (base.kind == .call && tc.call_immutable_alias_source(base_id) != none)
		if base.kind == .selector {
			if root_id := tc.lvalue_root_ident(base_id) {
				root := tc.a.node(root_id)
				aliases = aliases || tc.fn_context.immutable_reference_aliases[root.value]
			}
		}
		if aliases {
			tc.record_error_at(.assignment_mismatch,
				'`${tc.source_text_for_node(base_id)}` aliases mutable data from an immutable value, clone it first (or use `unsafe`)',
				base_id, if base.kind in [.ident, .selector] {
				tc.node_value_diagnostic_pos(base_id)
			} else {
				base.pos
			})
		}
		return
	}
	if node.kind != .selector || node.children_count == 0 {
		return
	}
	base_id := tc.a.child(node, 0)
	base := tc.a.node(base_id)
	mut aliases := (base.kind == .ident && tc.fn_context.immutable_reference_aliases[base.value])
		|| (base.kind == .call && tc.call_immutable_alias_source(base_id) != none)
	if root_id := tc.lvalue_root_ident(base_id) {
		root := tc.a.node(root_id)
		aliases = aliases || tc.fn_context.immutable_reference_aliases[root.value]
	}
	if aliases {
		tc.record_error_at(.assignment_mismatch,
			'`${tc.source_text_for_node(id)}` aliases mutable data from an immutable value', id,
			tc.node_value_diagnostic_pos(id))
	}
}

fn (mut tc TypeChecker) call_immutable_alias_source(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	call := tc.a.node(id)
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	return_type := unalias_type(tc.resolve_type(id))
	if return_type !is Array && return_type !is Map && return_type !is Pointer {
		return none
	}
	info := tc.resolve_call_info(id, *call) or { return none }
	decl_module := tc.fn_type_modules[info.name] or { tc.cur_module }
	decl := tc.visible_mutation_fn_decl(info.name, decl_module) or { return none }
	fn_node := tc.a.node(flat.NodeId(decl.idx))
	mut callee := tc.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 0 {
		callee = tc.a.child_node(callee, 0)
	}
	mut args_by_param := map[string]flat.NodeId{}
	mut call_arg_index := 1
	mut param_index := 0
	for i in 0 .. fn_node.children_count {
		param := tc.a.child_node(fn_node, i)
		if param.kind != .param {
			break
		}
		if info.has_receiver && param_index == 0 {
			if callee.kind == .selector && callee.children_count > 0 {
				args_by_param[param.value] = tc.a.child(callee, 0)
			}
		} else if call_arg_index < call.children_count {
			args_by_param[param.value] = tc.call_arg_value(tc.a.child(call, call_arg_index))
			call_arg_index++
		}
		param_index++
	}
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(fn_node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		node_id := stack.pop()
		node := tc.a.node(node_id)
		if node.kind == .return_stmt {
			for i in 0 .. node.children_count {
				returned_id := tc.a.child(node, i)
				param_name := tc.returned_alias_param_name(returned_id, args_by_param) or {
					continue
				}
				arg_id := args_by_param[param_name] or { continue }
				if immutable := tc.immutable_alias_argument(arg_id) {
					return immutable
				}
			}
			continue
		}
		if node.kind in [.fn_literal, .lambda_expr] {
			continue
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return none
}

fn (tc &TypeChecker) returned_alias_param_name(id flat.NodeId, args_by_param map[string]flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && node.value in args_by_param {
		return node.value
	}
	if node.kind in [.index, .selector, .prefix, .paren, .cast_expr, .as_expr, .expr_stmt]
		&& node.children_count > 0 {
		return tc.returned_alias_param_name(tc.a.child(node, 0), args_by_param)
	}
	return none
}

fn (tc &TypeChecker) immutable_alias_argument(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && !tc.ident_is_mutable_lvalue(node.value)
		&& tc.const_key_for_name(node.value) == none {
		return id
	}
	if node.kind in [.prefix, .paren, .cast_expr, .as_expr] && node.children_count > 0 {
		return tc.immutable_alias_argument(tc.a.child(node, 0))
	}
	return none
}

fn (mut tc TypeChecker) immutable_reference_source(id flat.NodeId, typ Type) ?flat.NodeId {
	if unalias_type(typ) !is Pointer || !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .ident && !tc.ident_is_mutable_lvalue(node.value)
		&& tc.const_key_for_name(node.value) == none {
		return id
	}
	if node.kind != .call {
		return none
	}
	info := tc.resolve_call_info(id, *node) or { return none }
	arg_count := int(node.children_count) - 1
	for i in 0 .. arg_count {
		param_idx := i + if info.has_receiver { 1 } else { 0 }
		if param_idx >= info.params.len || unalias_type(info.params[param_idx]) !is Pointer {
			continue
		}
		arg_id := tc.call_arg_value(tc.a.child(node, i + 1))
		arg := tc.a.node(arg_id)
		if arg.kind == .ident && !tc.ident_is_mutable_lvalue(arg.value)
			&& tc.const_key_for_name(arg.value) == none {
			return arg_id
		}
	}
	return none
}

fn (mut tc TypeChecker) record_invalid_decl_assign_lhs(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) bool {
	lhs := tc.a.node(lhs_id)
	if lhs.kind == .paren {
		tc.record_error_at(.assignment_mismatch,
			'parentheses are not supported on the left side of `:=`', lhs_id, lhs.pos)
		if lhs.children_count == 0 {
			return true
		}
		inner_id := tc.a.child(lhs, 0)
		inner := tc.a.node(inner_id)
		if inner.kind == .prefix && inner.op == .mul {
			if tc.unsafe_depth == 0 {
				tc.record_error_at(.assignment_mismatch,
					'modifying variables via dereferencing can only be done in `unsafe` blocks',
					lhs_id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
			}
			tc.record_error_at(.assignment_mismatch, 'non-name on the left side of `:=`', inner_id, tc.prefix_operator_pos(inner_id,
				'*'))
		}
		return true
	}
	if lhs.kind != .prefix {
		return false
	}
	op := tc.source_text_for_node(lhs_id).trim_space()[..1]
	if lhs.op == .amp {
		tc.record_error_at(.assignment_mismatch, 'cannot use a reference on the left side of `:=`',
			lhs_id, tc.prefix_operator_pos(lhs_id, op))
	} else {
		tc.record_error_at(.assignment_mismatch, 'cannot use `${op}` on the left of `:=`', lhs_id, tc.prefix_operator_pos(lhs_id,
			op))
	}
	tc.record_error_at(.assignment_mismatch, 'non-name on the left side of `:=`', lhs_id, tc.prefix_operator_pos(lhs_id,
		op))
	return true
}

fn (tc &TypeChecker) assignment_rhs_call_name(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .call {
		callee_node := tc.a.child_node(node, 0)
		if callee_node.kind == .selector {
			if callee_node.children_count > 0 {
				base := tc.a.child_node(callee_node, 0)
				if base.kind == .ident && !tc.ident_resolves_to_value(base.value)
					&& tc.type_symbol_known(tc.qualify_name(base.value)) {
					return tc.call_display_name(node)
				}
			}
			return callee_node.value
		}
		if callee, _, _ := tc.unknown_method_call_parts(node) {
			return callee.value
		}
		return tc.call_display_name(node)
	}
	if node.kind in [.paren, .expr_stmt, .or_expr] && node.children_count > 0 {
		return tc.assignment_rhs_call_name(tc.a.child(node, 0))
	}
	return none
}

fn (tc &TypeChecker) unresolved_multi_assign_method_call_name(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt, .or_expr] && node.children_count > 0 {
		return tc.unresolved_multi_assign_method_call_name(tc.a.child(node, 0))
	}
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := tc.a.child_node(node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base := tc.a.child_node(callee, 0)
	if base.kind == .ident && !tc.lvalue_ident_is_known(base.value) {
		return callee.value
	}
	return none
}

fn (tc &TypeChecker) unsafe_block_none_expr_id(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind != .block || node.value != 'unsafe' {
		return none
	}
	tail_id := tc.branch_tail_expr_id(id)
	if tc.valid_node_id(tail_id) && tc.a.node(tail_id).kind == .none_expr {
		return tail_id
	}
	return none
}

fn (mut tc TypeChecker) check_const_reference_assignment(lhs_id flat.NodeId, rhs_id flat.NodeId, is_decl bool) {
	lhs := tc.a.node(lhs_id)
	if const_id := tc.addressed_const_ident(rhs_id) {
		const_node := tc.a.node(const_id)
		key := tc.const_key_for_name(const_node.value) or { return }
		if is_decl {
			if expr_id := tc.const_exprs[key] {
				expr := tc.a.node(expr_id)
				if expr.kind == .int_literal {
					value := tc.source_text_for_node(expr_id)
					tc.record_error_with_details_at(.assignment_mismatch,
						'cannot assign a pointer to a constant with an integer literal value',
						const_id, tc.node_value_diagnostic_pos(const_id), [
						'Specify the type for the constant value. Example:',
						'         `const ${const_node.value} = int(${value})`',
					])
				}
			}
		}
		if lhs.kind == .ident && lhs.value != '_' {
			tc.record_error_at(.assignment_mismatch,
				'cannot have mutable reference to const `${const_node.value}`', rhs_id,
				tc.address_operator_pos(rhs_id))
		}
	}
	addressed_id := tc.addressed_ident(rhs_id) or { return }
	addressed := tc.a.node(addressed_id)
	if tc.assignment_target_requests_mutable_reference(lhs_id, is_decl)
		&& !tc.ident_is_mutable_lvalue(addressed.value) {
		tc.record_error_at(.assignment_mismatch,
			'`${addressed.value}` is immutable, cannot have a mutable reference to it', rhs_id,
			tc.address_operator_pos(rhs_id))
	}
}

fn (tc &TypeChecker) addressed_const_ident(id flat.NodeId) ?flat.NodeId {
	ident_id := tc.addressed_ident(id) or { return none }
	if _ := tc.const_key_for_name(tc.a.node(ident_id).value) {
		return ident_id
	}
	return none
}

fn (tc &TypeChecker) addressed_ident(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.addressed_const_ident(tc.a.child(node, 0))
	}
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return none
	}
	mut child_id := tc.a.child(node, 0)
	mut child := tc.a.node(child_id)
	for child.kind == .paren && child.children_count > 0 {
		child_id = tc.a.child(child, 0)
		child = tc.a.node(child_id)
	}
	if child.kind != .ident {
		return none
	}
	return child_id
}

fn (tc &TypeChecker) assignment_target_requests_mutable_reference(lhs_id flat.NodeId, is_decl bool) bool {
	lhs := tc.a.node(lhs_id)
	if lhs.kind != .ident || lhs.value == '_' {
		return false
	}
	if is_decl {
		parent_id := tc.direct_parent_id(lhs_id)
		if tc.valid_node_id(parent_id) {
			parent := tc.a.node(parent_id)
			if parent.kind == .decl_assign {
				return tc.decl_lhs_is_mut(parent, lhs_id)
			}
		}
	}
	return tc.ident_is_mutable_lvalue(lhs.value)
}

fn (tc &TypeChecker) address_operator_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if relative := source[start..end].index('&') {
			op_start := start + relative
			return token.new_span(node.pos.id, op_start, op_start + 1)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) decl_non_name_lhs_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .selector {
		return tc.node_value_diagnostic_pos(id)
	}
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if relative := source[start..end].last_index('[') {
			suffix_start := start + relative
			return token.new_span(node.pos.id, suffix_start, end)
		}
	}
	return node.pos
}

// cur_scope_depth returns the number of enclosing scopes (the current scope's parent-chain
// length), used to tell a dominating top-level reassignment from one nested in a branch/loop.
fn (tc &TypeChecker) cur_scope_depth() int {
	mut d := 0
	mut s := tc.cur_scope
	for s != unsafe { nil } {
		d++
		s = s.parent
	}
	return d
}

fn clone_scope_binding_owner_map(src map[string][]ScopeBindingOwner) map[string][]ScopeBindingOwner {
	mut out := map[string][]ScopeBindingOwner{}
	for name, owners in src {
		out[name] = owners.clone()
	}
	return out
}

// track_method_value_local records (or clears) a local variable bound to a method value, so a
// later `return cb` / `arr << cb` retains any stack-backed mutable-receiver lifetime hazard.
fn (mut tc TypeChecker) track_method_value_local(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	if int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if tc.expr_is_method_value(rhs_id) {
		tc.fn_context.method_value_locals[lhs.value] = true
		tc.fn_context.method_value_local_depth[lhs.value] = tc.cur_scope_depth()
		tc.mark_method_value_local_owner(lhs.value)
		if tc.method_value_has_stack_mut_receiver(rhs_id) {
			if owner := tc.cur_scope.lookup_owner(lhs.value) {
				owner_key := owner.storage_key()
				if owner_key.len > 0 {
					tc.fn_context.method_value_stack_mut_owners[owner_key] = true
				}
			}
		}
	} else if lhs.value in tc.fn_context.method_value_locals {
		// Reassigned to a non-method-value. Only clear the marker when this reassignment
		// dominates later uses — at the same or a shallower scope than where the local was
		// marked. A reassignment in a deeper conditional/loop scope does not run on every path
		// (`mut cb := c.report; if x { cb = plain }; return cb`), so the local may still hold the
		// method value; keep the maybe-method marker and let the later escape be rejected.
		marked_depth := tc.fn_context.method_value_local_depth[lhs.value] or { 0 }
		if tc.cur_scope_depth() <= marked_depth {
			tc.unmark_current_method_value_local_owner(lhs.value)
		}
	}
}

fn (mut tc TypeChecker) mark_method_value_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	mut owners := tc.fn_context.method_value_local_owners[name] or { []ScopeBindingOwner{} }
	for existing in owners {
		if existing.storage_key() == owner_key {
			return
		}
	}
	owners << owner
	tc.fn_context.method_value_local_owners[name] = owners
}

fn (mut tc TypeChecker) unmark_current_method_value_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	owners := tc.fn_context.method_value_local_owners[name] or { return }
	mut keep := []ScopeBindingOwner{cap: owners.len}
	for existing in owners {
		if existing.storage_key() != owner_key {
			keep << existing
		} else {
			tc.fn_context.method_value_stack_mut_owners.delete(owner_key)
		}
	}
	if keep.len == 0 {
		tc.fn_context.method_value_locals.delete(name)
		tc.fn_context.method_value_local_depth.delete(name)
		tc.fn_context.method_value_local_owners.delete(name)
	} else {
		tc.fn_context.method_value_local_owners[name] = keep
	}
}

fn (tc &TypeChecker) current_binding_is_method_value_local(name string) bool {
	if name.len == 0 || name !in tc.fn_context.method_value_locals {
		return false
	}
	if tc.cur_scope == unsafe { nil } {
		return true
	}
	owners := tc.fn_context.method_value_local_owners[name] or { return true }
	current_owner := tc.cur_scope.lookup_owner(name) or {
		// Fn literals can be checked after their body has been lifted away from the
		// lexical scope that declared the captured alias. If there is no nearer
		// binding, keep treating the tracked name as the outer method-value local so
		// mutable-receiver escape checks can still follow it.
		return true
	}
	current_key := current_owner.storage_key()
	for owner in owners {
		if current_key.len > 0 && owner.storage_key() == current_key {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) current_method_value_local_has_stack_mut_receiver(name string) bool {
	if name.len == 0 {
		return false
	}
	owners := tc.fn_context.method_value_local_owners[name] or { return false }
	if tc.cur_scope != unsafe { nil } {
		if current_owner := tc.cur_scope.lookup_owner(name) {
			current_key := current_owner.storage_key()
			if current_key.len > 0 {
				return tc.fn_context.method_value_stack_mut_owners[current_key]
			}
		}
	}
	// A lifted nested literal can be checked without the outer lexical scope that
	// owns a captured method-value alias. Conservatively retain its mutable-receiver
	// marker.
	for owner in owners {
		if tc.fn_context.method_value_stack_mut_owners[owner.storage_key()] {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) track_variadic_fn_value_local(lhs_id flat.NodeId, rhs_id flat.NodeId) {
	if int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	if tc.expr_is_variadic_fn_value(rhs_id) {
		tc.fn_context.fn_value_variadic_locals[lhs.value] = true
		tc.fn_context.fn_value_variadic_local_depth[lhs.value] = tc.cur_scope_depth()
		tc.mark_variadic_fn_value_local_owner(lhs.value)
	} else if lhs.value in tc.fn_context.fn_value_variadic_locals {
		marked_depth := tc.fn_context.fn_value_variadic_local_depth[lhs.value] or { 0 }
		if tc.cur_scope_depth() <= marked_depth {
			tc.unmark_current_variadic_fn_value_local_owner(lhs.value)
		}
	}
}

fn (mut tc TypeChecker) mark_variadic_fn_value_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	mut owners := tc.fn_context.fn_value_variadic_local_owners[name] or { []ScopeBindingOwner{} }
	for existing in owners {
		if existing.storage_key() == owner_key {
			return
		}
	}
	owners << owner
	tc.fn_context.fn_value_variadic_local_owners[name] = owners
}

fn (mut tc TypeChecker) unmark_current_variadic_fn_value_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	owners := tc.fn_context.fn_value_variadic_local_owners[name] or { return }
	mut keep := []ScopeBindingOwner{cap: owners.len}
	for existing in owners {
		if existing.storage_key() != owner_key {
			keep << existing
		}
	}
	if keep.len == 0 {
		tc.fn_context.fn_value_variadic_locals.delete(name)
		tc.fn_context.fn_value_variadic_local_depth.delete(name)
		tc.fn_context.fn_value_variadic_local_owners.delete(name)
	} else {
		tc.fn_context.fn_value_variadic_local_owners[name] = keep
	}
}

fn (tc &TypeChecker) current_binding_is_variadic_fn_value_local(name string) bool {
	if name.len == 0 || name !in tc.fn_context.fn_value_variadic_locals {
		return false
	}
	if tc.cur_scope == unsafe { nil } {
		return true
	}
	owners := tc.fn_context.fn_value_variadic_local_owners[name] or { return true }
	for owner in owners {
		if tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) mark_capturing_fn_literal_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	mut owners := tc.fn_context.capturing_fn_literal_local_owners[name] or { []ScopeBindingOwner{} }
	for existing in owners {
		if existing.storage_key() == owner_key {
			return
		}
	}
	owners << owner
	tc.fn_context.capturing_fn_literal_local_owners[name] = owners
}

fn (mut tc TypeChecker) unmark_current_capturing_fn_literal_local_owner(name string) {
	if tc.cur_scope == unsafe { nil } {
		return
	}
	owner := tc.cur_scope.lookup_owner(name) or { return }
	owner_key := owner.storage_key()
	if owner_key.len == 0 {
		return
	}
	owners := tc.fn_context.capturing_fn_literal_local_owners[name] or { return }
	mut keep := []ScopeBindingOwner{cap: owners.len}
	for existing in owners {
		if existing.storage_key() != owner_key {
			keep << existing
		}
	}
	if keep.len == 0 {
		tc.fn_context.capturing_fn_literal_locals.delete(name)
		tc.fn_context.capturing_fn_literal_local_depth.delete(name)
		tc.fn_context.capturing_fn_literal_local_owners.delete(name)
	} else {
		tc.fn_context.capturing_fn_literal_local_owners[name] = keep
	}
}

fn (tc &TypeChecker) current_binding_is_capturing_fn_literal_local(name string) bool {
	if name.len == 0 || name !in tc.fn_context.capturing_fn_literal_locals {
		return false
	}
	if tc.cur_scope == unsafe { nil } {
		return true
	}
	owners := tc.fn_context.capturing_fn_literal_local_owners[name] or { return true }
	for owner in owners {
		if tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) mark_shared_binding_owner(name string, owner ScopeBindingOwner) {
	owner_key := owner.storage_key()
	if name.len == 0 || owner_key.len == 0 {
		return
	}
	mut owners := tc.fn_context.shared_owners[name] or { []ScopeBindingOwner{} }
	for existing in owners {
		if existing.storage_key() == owner_key {
			return
		}
	}
	owners << owner
	tc.fn_context.shared_owners[name] = owners
}

fn (mut tc TypeChecker) mark_shared_array_binding_owner(name string, owner ScopeBindingOwner) {
	owner_key := owner.storage_key()
	if name.len == 0 || owner_key.len == 0 {
		return
	}
	mut owners := tc.fn_context.shared_array_owners[name] or { []ScopeBindingOwner{} }
	for existing in owners {
		if existing.storage_key() == owner_key {
			return
		}
	}
	owners << owner
	tc.fn_context.shared_array_owners[name] = owners
}

fn (tc &TypeChecker) current_binding_is_shared(name string) bool {
	if name.len == 0 || tc.cur_scope == unsafe { nil } {
		return false
	}
	owners := tc.fn_context.shared_owners[name] or { return false }
	for owner in owners {
		if tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) current_binding_is_shared_array(name string) bool {
	if name.len == 0 || tc.cur_scope == unsafe { nil } {
		return false
	}
	owners := tc.fn_context.shared_array_owners[name] or { return false }
	for owner in owners {
		if tc.cur_scope.nearest_binding_owned_by(name, owner) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_initializes_shared_array(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.expr_initializes_shared_array(tc.a.child(node, 0))
	}
	if node.kind !in [.array_init, .array_literal] {
		return false
	}
	return node.typ.contains('shared ') || tc.node_source_starts_with(id, '[]shared ')
}

fn (tc &TypeChecker) shared_array_element_index(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .index && node.children_count > 0 {
		base_id := tc.a.child(node, 0)
		base := tc.a.node(base_id)
		if base.kind == .ident && tc.current_binding_is_shared_array(base.value) {
			return id
		}
		return tc.shared_array_element_index(base_id)
	}
	if node.kind in [.selector, .paren] && node.children_count > 0 {
		return tc.shared_array_element_index(tc.a.child(node, 0))
	}
	return none
}

fn (tc &TypeChecker) expr_is_variadic_fn_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	if name := tc.resolved_fn_value_name(id) {
		return tc.fn_variadic[name] or { false }
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			if tc.current_binding_is_variadic_fn_value_local(node.value) {
				return true
			}
			if !tc.name_bound_as_value(node.value) {
				key := tc.ident_fn_value_key(node.value) or { return false }
				return tc.fn_variadic[key] or { false }
			}
			return false
		}
		.selector {
			if tc.selector_base_bound_as_value(node) {
				return false
			}
			key := tc.selector_fn_value_key(node) or { return false }
			return tc.fn_variadic[key] or { false }
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_is_variadic_fn_value(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (mut tc TypeChecker) track_capturing_fn_literal_local(lhs_id flat.NodeId, rhs_id flat.NodeId, owner ScopeBindingOwner) {
	if int(lhs_id) < 0 {
		return
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
		return
	}
	mut binding_owner := owner
	if binding_owner.storage_key().len == 0 {
		binding_owner = tc.cur_scope.lookup_owner(lhs.value) or { ScopeBindingOwner{} }
	}
	if binding_owner.storage_key().len == 0 {
		return
	}
	binding_key := binding_owner.storage_key()
	if tc.expr_is_capturing_fn_literal_value(rhs_id) {
		tc.capturing_fn_literal_locals[binding_key] = true
		tc.capturing_fn_literal_local_depth[binding_key] = tc.cur_scope_depth()
		tc.capturing_fn_literal_return_unsupported[binding_key] =
			tc.expr_is_unsupported_returned_capturing_fn_literal_value(rhs_id)
		return
	}
	if !tc.capturing_fn_literal_locals[binding_key] {
		return
	}
	marked_depth := tc.capturing_fn_literal_local_depth[binding_key] or { 0 }
	if tc.cur_scope_depth() <= marked_depth {
		tc.capturing_fn_literal_locals.delete(binding_key)
		tc.capturing_fn_literal_local_depth.delete(binding_key)
		tc.capturing_fn_literal_return_unsupported.delete(binding_key)
	}
}

fn (mut tc TypeChecker) decl_assign_inferred_type(rhs_id flat.NodeId) Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return unknown_type('missing declaration initializer')
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind == .cast_expr && rhs.value.len > 0 {
		typ := tc.parse_type(rhs.value)
		if typ is Alias {
			return typ
		}
	}
	if smartcast := tc.smartcast_type(rhs_id) {
		return smartcast
	}
	if rhs.kind == .selector {
		if declared := tc.selector_declared_value_type(rhs) {
			if declared is OptionType || declared is ResultType {
				return declared
			}
		}
	}
	if typ := tc.infer_fn_value_decl_type(rhs_id) {
		return typ
	}
	if rhs.kind == .if_expr {
		return tc.if_expr_tail_type(rhs_id)
	}
	return tc.resolve_type(rhs_id)
}

fn (mut tc TypeChecker) infer_fn_value_decl_type(rhs_id flat.NodeId) ?Type {
	if int(rhs_id) < 0 || int(rhs_id) >= tc.a.nodes.len {
		return none
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind == .cast_expr {
		cast_type := tc.parse_type(rhs.value)
		if cast_type is OptionType || cast_type is ResultType {
			return none
		}
	}
	if tc.fn_value_shadowed_by_value(rhs) {
		return none
	}
	key := tc.fn_value_key(rhs) or { return none }
	typ := tc.fn_type_from_key(key) or { return none }
	tc.remember_resolved_fn_value_chain(rhs_id, key)
	tc.register_synth_type(rhs_id, typ)
	return typ
}

fn (tc &TypeChecker) fn_value_shadowed_by_value(node flat.Node) bool {
	match node.kind {
		.ident {
			return tc.name_bound_as_value(node.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(&node, 0))
		}
		else {
			return false
		}
	}
}

// lvalue_is_local_var reports whether an assignment target is safe to receive a method value:
// the blank discard `_` (stores nothing) or a plain function-local variable bound under its bare
// name in the current scope. Non-local storage (a struct field `h.cb`, an array/map element
// `cbs[i]`, or a module-level global, which lives in file_scope under its qualified name and so
// misses a bare lookup) is not. A method value may alias a local (tracked for a later escape) but
// must not be stored into anything that outlives the call site.
fn (tc &TypeChecker) lvalue_is_local_var(lhs_id flat.NodeId) bool {
	if int(lhs_id) < 0 {
		return false
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return false
	}
	if lhs.value == '_' {
		return true
	}
	return tc.cur_scope.lookup(lhs.value) != none
}

fn (tc &TypeChecker) selector_base_bound_as_value(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	base := tc.a.child_node(&node, 0)
	match base.kind {
		.ident {
			return tc.name_bound_as_value(base.value)
		}
		.selector {
			return tc.selector_base_bound_as_value(base)
		}
		.cast_expr, .paren, .expr_stmt {
			if base.children_count == 0 {
				return false
			}
			return tc.fn_value_shadowed_by_value(tc.a.child_node(base, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) name_bound_as_value(name string) bool {
	if name.len == 0 {
		return false
	}
	if typ := tc.cur_scope.lookup(name) {
		return typ !is Void
	}
	if typ := tc.file_scope.lookup(name) {
		return typ !is Void
	}
	return false
}

// check_multi_return_decl_assign validates check multi return decl assign state for types.
fn (mut tc TypeChecker) check_multi_return_decl_assign(id flat.NodeId, node flat.Node) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs := tc.a.nodes[int(rhs_id)]
	lhs_ids := tc.multi_assign_lhs_ids(node)
	if tc.multi_assign_rhs_count(node) != 1 {
		return false
	}
	if lhs_ids.len > 1 {
		if call_name := tc.unresolved_multi_assign_method_call_name(rhs_id) {
			tc.check_node(rhs_id)
			if tc.should_diagnose(id) {
				tc.record_error_at(.assignment_mismatch,
					'assignment mismatch: ${lhs_ids.len} variables but `${call_name}()` returns 0 values',
					id, tc.assignment_operator_pos(node, lhs_ids[0], rhs_id))
			}
			for lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, unknown_type('invalid variable'), tc.decl_lhs_is_mut(node,
					lhs_id))
			}
			tc.fn_context.continue_after_unknown_ident = true
			return true
		}
	}
	if lhs_ids.len > 1 && rhs.kind == .ident && !tc.lvalue_ident_is_known(rhs.value) {
		variables := if lhs_ids.len == 1 { 'variable' } else { 'variables' }
		tc.record_error_at(.assignment_mismatch,
			'assignment mismatch: ${lhs_ids.len} ${variables} 1 value', id, tc.assignment_operator_pos(node,
			lhs_ids[0], rhs_id))
		for lhs_id in lhs_ids {
			tc.insert_decl_lhs(lhs_id, unknown_type('invalid variable'), tc.decl_lhs_is_mut(node,
				lhs_id))
		}
		return true
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
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
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
		// Tuple tails (`.a { c, '.zst', 'zstd' }`) resolve like if-expr branches.
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
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
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
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
		if rhs_types := tc.multi_expr_tail_types(rhs_id, lhs_ids.len) {
			tc.register_synth_type(rhs_id, MultiReturn{
				types: rhs_types
			})
			for i, lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, rhs_types[i], tc.decl_lhs_is_mut(node, lhs_id))
			}
			$if ownership ? {
				tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, MultiReturn{
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
	invalid_return_count := rhs_type is Unknown
		|| (rhs_type is MultiReturn && rhs_type.types.len == 0)
		|| rhs_type_name == 'unknown'
	if lhs_ids.len > 1 && invalid_return_count {
		if call_name := tc.assignment_rhs_call_name(rhs_id) {
			if tc.should_diagnose(id) {
				tc.record_error_at(.assignment_mismatch,
					'assignment mismatch: ${lhs_ids.len} variables but `${call_name}()` returns 0 values',
					id, tc.assignment_operator_pos(node, lhs_ids[0], rhs_id))
			}
			for lhs_id in lhs_ids {
				tc.insert_decl_lhs(lhs_id, unknown_type('invalid variable'), tc.decl_lhs_is_mut(node,
					lhs_id))
			}
			return true
		}
	}
	if unhandled_multi {
		if !rhs_checked {
			tc.check_node(rhs_id)
		}
		if tc.should_diagnose(id) {
			if rhs.kind == .call {
				handler := if rhs_type is OptionType { '?' } else { '!' }
				tc.record_error_at(.assignment_mismatch,
					'${tc.call_display_name(rhs)}() returns `${rhs_type_name}`, so it should have either an `or {}` block, or `${handler}` at the end',
					rhs_id, rhs.pos)
			} else {
				tc.record_error(.assignment_mismatch,
					'multi-return assignment from `${rhs_type_name}` requires `or {}`, `!`, or `?` handling',
					id)
			}
		}
		if wrapped_multi := multi_return_payload_type(rhs_type) {
			for i, lhs_id in lhs_ids {
				if i < wrapped_multi.types.len {
					tc.insert_decl_lhs(lhs_id, wrapped_multi.types[i], tc.decl_lhs_is_mut(node,
						lhs_id))
				}
			}
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
			tc.insert_decl_lhs(lhs_id, rhs_multi.types[i], tc.decl_lhs_is_mut(node, lhs_id))
		}
		$if ownership ? {
			tc.ownership_after_multi_return_decl_assign(lhs_ids, rhs_id, rhs_multi, id)
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

fn (tc &TypeChecker) multi_expr_tail_types(expr_id flat.NodeId, count int) ?[]Type {
	groups := tc.multi_expr_tail_type_groups(expr_id, count) or { return none }
	if groups.len == 0 {
		return none
	}
	mut tail_types := []Type{cap: count}
	for typ in groups[0] {
		tail_types << typ
	}
	for i in 1 .. groups.len {
		group := groups[i]
		if group.len != tail_types.len {
			return none
		}
		for j, actual in group {
			promoted := tc.promoted_multi_tail_type(tail_types[j], actual) or { return none }
			tail_types[j] = promoted
		}
	}
	return tail_types
}

fn (tc &TypeChecker) multi_expr_tail_type_groups(expr_id flat.NodeId, count int) ?[][]Type {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count < 3 {
				return none
			}
			mut groups := tc.multi_expr_branch_tail_type_groups(tc.a.child(&node, 1), count, false) or {
				return none
			}
			else_groups := tc.multi_expr_tail_type_groups(tc.a.child(&node, 2), count) or {
				return none
			}
			groups << else_groups
			return groups
		}
		.match_stmt {
			if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(node) {
				return none
			}
			mut groups := [][]Type{}
			for i in 1 .. node.children_count {
				branch_groups := tc.multi_expr_branch_tail_type_groups(tc.a.child(&node, i), count, true) or {
					return none
				}
				groups << branch_groups
			}
			return groups
		}
		.block, .match_branch {
			return tc.multi_expr_branch_tail_type_groups(expr_id, count, false)
		}
		.lock_expr {
			if node.children_count > 0 {
				return tc.multi_expr_tail_type_groups(tc.a.child(&node, node.children_count - 1),
					count)
			}
		}
		.expr_stmt {
			if node.children_count > 0 {
				return tc.multi_expr_tail_type_groups(tc.a.child(&node, 0), count)
			}
		}
		else {}
	}
	return none
}

fn (tc &TypeChecker) multi_expr_branch_tail_type_groups(branch_id flat.NodeId, count int, explicit_comma_tail bool) ?[][]Type {
	if groups := tc.tuple_tail_value_groups(branch_id, count, explicit_comma_tail) {
		mut result := [][]Type{cap: groups.len}
		for group in groups {
			mut types := []Type{cap: group.len}
			for value_id in group {
				typ := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
				if !type_has_runtime_value(typ) {
					return none
				}
				types << typ
			}
			result << types
		}
		return result
	}
	tail_id := tc.branch_tail_expr_id(branch_id)
	if !tc.valid_node_id(tail_id) {
		return none
	}
	if tc.branch_tail_never_returns(branch_id) {
		return [][]Type{}
	}
	tail := tc.a.nodes[int(tail_id)]
	if tail.kind in [.if_expr, .match_stmt] {
		return tc.multi_expr_tail_type_groups(tail_id, count)
	}
	tail_type := tc.expr_type(tail_id) or { tc.resolve_type(tail_id) }
	multi := tc.multi_return_assignment_type(tail_id, tail_type) or { return none }
	if multi.types.len != count {
		return none
	}
	for typ in multi.types {
		if !type_has_runtime_value(typ) {
			return none
		}
	}
	return [multi.types.clone()]
}

fn (mut tc TypeChecker) multi_expr_tail_assign_types(id flat.NodeId, expr_id flat.NodeId, lhs_ids []flat.NodeId) ?[]Type {
	groups := tc.multi_expr_tail_value_groups(expr_id, lhs_ids.len, false) or { return none }
	if groups.len == 0 {
		return none
	}
	mut lhs_types := []Type{cap: lhs_ids.len}
	for lhs_id in lhs_ids {
		lhs_types << tc.resolve_lvalue_type(lhs_id)
	}
	for group in groups {
		if group.len != lhs_types.len {
			return none
		}
		for i, value_id in group {
			actual := tc.resolve_expr(value_id, lhs_types[i])
			if !tc.type_compatible(actual, lhs_types[i]) {
				tc.type_mismatch(.assignment_mismatch,
					'cannot assign `${actual.name()}` to `${lhs_types[i].name()}`', id)
			}
		}
	}
	return lhs_types
}

fn (tc &TypeChecker) match_multi_return_types(expr_id flat.NodeId, count int) ?[]Type {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return none
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		return none
	}
	mut match_types := []Type{}
	mut saw_value_branch := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		tail_id := tc.branch_tail_expr_id(branch_id)
		if !tc.valid_node_id(tail_id) {
			return none
		}
		tail := tc.a.nodes[int(tail_id)]
		if tail.kind == .return_stmt || tc.branch_tail_never_returns(branch_id) {
			continue
		}
		multi := multi_return_payload_type(tc.resolve_type(tail_id)) or { return none }
		if multi.types.len != count {
			return none
		}
		for typ in multi.types {
			if !type_has_runtime_value(typ) {
				return none
			}
		}
		if !saw_value_branch {
			match_types = multi.types.clone()
			saw_value_branch = true
			continue
		}
		for j, actual in multi.types {
			if actual.name() != match_types[j].name() {
				return none
			}
		}
	}
	if !saw_value_branch {
		return none
	}
	return match_types
}

fn (tc &TypeChecker) match_has_incompatible_multi_return_branches(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return false
	}
	mut expected := []Type{}
	mut saw_value_branch := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		tail_id := tc.branch_tail_expr_id(branch_id)
		if !tc.valid_node_id(tail_id) || tc.branch_tail_never_returns(branch_id) {
			continue
		}
		multi := multi_return_payload_type(tc.resolve_type(tail_id)) or { return false }
		if multi.types.len != count {
			return false
		}
		if !saw_value_branch {
			expected = multi.types.clone()
			saw_value_branch = true
			continue
		}
		for j, actual in multi.types {
			promoted := tc.promoted_multi_tail_type(expected[j], actual) or { return true }
			expected[j] = promoted
		}
	}
	return false
}

fn (tc &TypeChecker) match_has_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return false
	}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if groups := tc.tuple_tail_value_groups(branch_id, count, false) {
			if groups.len > 0 {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) expr_has_noncomma_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count > 1
				&& tc.expr_has_noncomma_tuple_tail_values(tc.a.child(&node, 1), count) {
				return true
			}
			return node.children_count > 2
				&& tc.expr_has_noncomma_tuple_tail_values(tc.a.child(&node, 2), count)
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				if tc.expr_has_noncomma_tuple_tail_values(tc.a.child(&node, i), count) {
					return true
				}
			}
		}
		.block, .match_branch {
			body_start := if node.kind == .match_branch {
				if node.value == 'else' { 0 } else { node.value.int() }
			} else {
				0
			}
			if node.children_count <= body_start {
				return false
			}
			last_id := tc.a.child(&node, node.children_count - 1)
			last := tc.a.nodes[int(last_id)]
			if last.kind in [.block, .match_branch, .if_expr, .match_stmt] {
				return tc.expr_has_noncomma_tuple_tail_values(last_id, count)
			}
			if node.value == 'comma_exprs' {
				return false
			}
			if groups := tc.tuple_tail_value_groups(expr_id, count, false) {
				if groups.len > 0 {
					return true
				}
			}
			return tc.expr_has_noncomma_tuple_tail_values(last_id, count)
		}
		.expr_stmt {
			return node.children_count > 0
				&& tc.expr_has_noncomma_tuple_tail_values(tc.a.child(&node, 0), count)
		}
		else {}
	}
	return false
}

fn (tc &TypeChecker) expr_has_tuple_tail_values(expr_id flat.NodeId, count int) bool {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	if groups := tc.tuple_tail_value_groups(expr_id, count, false) {
		if groups.len > 0 {
			return true
		}
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count > 1 && tc.expr_has_tuple_tail_values(tc.a.child(&node, 1), count) {
				return true
			}
			return node.children_count > 2
				&& tc.expr_has_tuple_tail_values(tc.a.child(&node, 2), count)
		}
		.match_stmt {
			return tc.match_has_tuple_tail_values(expr_id, count)
		}
		.block, .match_branch {
			body_start := if node.kind == .match_branch {
				if node.value == 'else' { 0 } else { node.value.int() }
			} else {
				0
			}
			if node.children_count <= body_start {
				return false
			}
			return tc.expr_has_tuple_tail_values(tc.a.child(&node, node.children_count - 1), count)
		}
		.expr_stmt {
			return node.children_count > 0
				&& tc.expr_has_tuple_tail_values(tc.a.child(&node, 0), count)
		}
		else {
			return false
		}
	}
}

// multi_expr_tail_types_for_transform returns promoted multi-expression tail
// types for transform lowering without duplicating checker compatibility rules.
pub fn (tc &TypeChecker) multi_expr_tail_types_for_transform(expr_id flat.NodeId, count int) ?[]Type {
	return tc.multi_expr_tail_types(expr_id, count)
}

fn (tc &TypeChecker) promoted_multi_tail_type(current Type, actual Type) ?Type {
	if tc.type_compatible(actual, current) {
		return current
	}
	if tc.type_compatible(current, actual) {
		return actual
	}
	return none
}

fn (tc &TypeChecker) multi_expr_tail_value_groups(expr_id flat.NodeId, count int, explicit_comma_tail bool) ?[][]flat.NodeId {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count < 3 {
				return none
			}
			mut groups := [][]flat.NodeId{}
			then_groups := tc.tuple_tail_value_groups(tc.a.child(&node, 1), count,
				explicit_comma_tail) or { return none }
			for group in then_groups {
				groups << group
			}
			else_id := tc.a.child(&node, 2)
			else_groups := tc.multi_expr_tail_value_groups(else_id, count, explicit_comma_tail) or {
				return none
			}
			for group in else_groups {
				groups << group
			}
			return groups
		}
		.match_stmt {
			if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(node) {
				return none
			}
			mut groups := [][]flat.NodeId{}
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				// Match multi-value tails must be comma expressions, not adjacent statements.
				branch_groups := tc.tuple_tail_value_groups(branch_id, count, true) or {
					return none
				}
				for group in branch_groups {
					groups << group
				}
			}
			return groups
		}
		.block, .match_branch {
			return tc.tuple_tail_value_groups(expr_id, count, explicit_comma_tail)
		}
		.lock_expr {
			if node.children_count > 0 {
				return tc.multi_expr_tail_value_groups(tc.a.child(&node, node.children_count - 1),
					count, explicit_comma_tail)
			}
		}
		.expr_stmt {
			if node.children_count > 0 {
				return tc.multi_expr_tail_value_groups(tc.a.child(&node, 0), count,
					explicit_comma_tail)
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) wrapped_multi_return_value_groups(expr_id flat.NodeId, count int, explicit_comma_tail bool, wrapper Type) ?[][]flat.NodeId {
	if count <= 0 || !tc.valid_node_id(expr_id) {
		return none
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.if_expr {
			if node.children_count < 3 {
				return none
			}
			mut groups := [][]flat.NodeId{}
			then_groups := tc.wrapped_multi_return_branch_groups(tc.a.child(&node, 1), count,
				explicit_comma_tail, wrapper) or { return none }
			for group in then_groups {
				groups << group
			}
			else_id := tc.a.child(&node, 2)
			else_groups := tc.wrapped_multi_return_branch_groups(else_id, count,
				explicit_comma_tail, wrapper) or { return none }
			for group in else_groups {
				groups << group
			}
			return groups
		}
		.match_stmt {
			if node.children_count < 2 || !tc.match_has_else_or_exhaustive_coverage(node) {
				return none
			}
			mut groups := [][]flat.NodeId{}
			for i in 1 .. node.children_count {
				branch_groups := tc.wrapped_multi_return_branch_groups(tc.a.child(&node, i), count,
					true, wrapper) or { return none }
				for group in branch_groups {
					groups << group
				}
			}
			return groups
		}
		.block, .match_branch {
			return tc.wrapped_multi_return_branch_groups(expr_id, count, explicit_comma_tail,
				wrapper)
		}
		.lock_expr {
			if node.children_count > 0 {
				return tc.wrapped_multi_return_value_groups(tc.a.child(&node,
					node.children_count - 1), count, explicit_comma_tail, wrapper)
			}
		}
		.expr_stmt {
			if node.children_count > 0 {
				return tc.wrapped_multi_return_value_groups(tc.a.child(&node, 0), count,
					explicit_comma_tail, wrapper)
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) wrapped_multi_return_branch_groups(branch_id flat.NodeId, count int, explicit_comma_tail bool, wrapper Type) ?[][]flat.NodeId {
	if !tc.valid_node_id(branch_id) {
		return none
	}
	if tc.wrapped_multi_return_tail_is_error(branch_id, wrapper) {
		return [][]flat.NodeId{}
	}
	return tc.multi_expr_tail_value_groups(branch_id, count, explicit_comma_tail)
}

fn (tc &TypeChecker) wrapped_multi_return_tail_is_error(branch_id flat.NodeId, wrapper Type) bool {
	if wrapper !is OptionType && wrapper !is ResultType {
		return false
	}
	tail_id := tc.branch_tail_expr_id(branch_id)
	if !tc.valid_node_id(tail_id) {
		return false
	}
	if tc.branch_tail_never_returns(branch_id) {
		return true
	}
	raw_type := tc.resolve_type(tail_id)
	return is_ierror_type(raw_type) || tc.type_compatible_with_ierror_payload(raw_type)
}

fn (tc &TypeChecker) tuple_tail_value_groups(body_id flat.NodeId, count int, explicit_comma_tail bool) ?[][]flat.NodeId {
	if count <= 0 || !tc.valid_node_id(body_id) {
		return none
	}
	body := tc.a.nodes[int(body_id)]
	body_start := if body.kind == .match_branch {
		if body.value == 'else' { 0 } else { body.value.int() }
	} else {
		0
	}
	if body.kind !in [.block, .match_branch] || body.children_count <= body_start {
		return none
	}
	last_id := tc.a.child(&body, body.children_count - 1)
	if tc.valid_node_id(last_id) {
		last := tc.a.nodes[int(last_id)]
		if last.kind in [.block, .match_branch, .if_expr, .match_stmt] {
			return tc.multi_expr_tail_value_groups(last_id, count, explicit_comma_tail)
		}
		if last.kind == .return_stmt {
			return [][]flat.NodeId{}
		}
		if tc.branch_tail_never_returns(body_id) {
			return [][]flat.NodeId{}
		}
	}
	is_comma_tail := body.value == 'comma_exprs'
	if explicit_comma_tail && !is_comma_tail {
		return none
	}
	mut values := []flat.NodeId{}
	for i := int(body.children_count) - 1; i >= body_start; i-- {
		child_id := tc.a.child(&body, i)
		if !tc.valid_node_id(child_id) {
			return none
		}
		child := tc.a.nodes[int(child_id)]
		if child.kind != .expr_stmt || child.children_count == 0 {
			if is_comma_tail {
				return none
			}
			break
		}
		for j := int(child.children_count) - 1; j >= 0; j-- {
			values.prepend(tc.a.child(&child, j))
			if !is_comma_tail && values.len == count {
				break
			}
		}
		if !is_comma_tail && values.len == count {
			mut groups := [][]flat.NodeId{}
			groups << values
			return groups
		}
	}
	if is_comma_tail && values.len == count {
		return [values]
	}
	return none
}

// multi_assign_lhs_ids supports multi assign lhs ids handling for TypeChecker.
fn (tc &TypeChecker) multi_assign_lhs_ids(node flat.Node) []flat.NodeId {
	lhs_count := tc.multi_assign_lhs_count(node)
	mut lhs_ids := []flat.NodeId{cap: lhs_count}
	for i in 0 .. lhs_count {
		lhs_ids << tc.multi_assign_lhs_id(node, i)
	}
	return lhs_ids
}

fn (tc &TypeChecker) multi_assign_lhs_count(node flat.Node) int {
	if node.value.is_int() {
		count := node.value.int()
		if count > 0 && count <= int(node.children_count) {
			return count
		}
	}
	if node.children_count <= 2 {
		return if node.children_count > 0 { 1 } else { 0 }
	}
	return int(node.children_count) - 1
}

fn (tc &TypeChecker) multi_assign_rhs_count(node flat.Node) int {
	lhs_count := tc.multi_assign_lhs_count(node)
	rhs_count := int(node.children_count) - lhs_count
	return if rhs_count > 0 { rhs_count } else { 0 }
}

fn (tc &TypeChecker) multi_assign_lhs_id(node flat.Node, index int) flat.NodeId {
	rhs_count := tc.multi_assign_rhs_count(node)
	child_index := if index < rhs_count { index * 2 } else { rhs_count + index }
	return tc.a.child(&node, child_index)
}

fn (tc &TypeChecker) multi_assign_rhs_id(node flat.Node, index int) flat.NodeId {
	lhs_count := tc.multi_assign_lhs_count(node)
	child_index := if index < lhs_count { index * 2 + 1 } else { lhs_count + index }
	return tc.a.child(&node, child_index)
}

// insert_decl_lhs updates insert decl lhs state for types.
fn (tc &TypeChecker) decl_lhs_is_mut(node flat.Node, lhs_id flat.NodeId) bool {
	if node.is_mut {
		return true
	}
	if int(lhs_id) < 0 || int(lhs_id) >= tc.a.nodes.len {
		return false
	}
	return tc.a.nodes[int(lhs_id)].is_mut
}

fn (mut tc TypeChecker) insert_decl_lhs(lhs_id flat.NodeId, typ Type, is_mut bool) ScopeBindingOwner {
	if int(lhs_id) < 0 || typ is Void {
		return ScopeBindingOwner{}
	}
	lhs := tc.a.nodes[int(lhs_id)]
	if lhs.kind == .ident && lhs.value.len > 0 {
		if lhs.value != '_' && (tc.visible_local_scope_owns_name(lhs.value)
			|| tc.visible_mut_param_binding_owns_name(lhs.value)) {
			tc.record_error(.assignment_mismatch, 'redefinition of `${lhs.value}`', lhs_id)
			return ScopeBindingOwner{}
		}
		owner := tc.cur_scope.insert_with_owner(lhs.value, typ)
		if is_mut && lhs.value != '_' {
			tc.fn_context.mut_local_owners[lhs.value] = owner
		}
		tc.register_synth_type(lhs_id, typ)
		return owner
	}
	return ScopeBindingOwner{}
}

fn (tc &TypeChecker) visible_mut_param_binding_owns_name(name string) bool {
	param_owner := tc.fn_context.mut_param_owners[name] or { return false }
	owner := tc.cur_scope.lookup_owner(name) or { return false }
	return owner.scope == param_owner.scope && owner.index == param_owner.index
		&& owner.generation == param_owner.generation
}

fn (tc &TypeChecker) visible_local_scope_owns_name(name string) bool {
	if name.len == 0 || tc.cur_scope == unsafe { nil } {
		return false
	}
	scope := tc.cur_scope
	for i := scope.names.len - 1; i >= 0; i-- {
		if scope.names[i] == name {
			return true
		}
	}
	return false
}

// check_assign validates check assign state for types.
fn (mut tc TypeChecker) check_assign(id flat.NodeId, node flat.Node) {
	if node.children_count < 2 {
		return
	}
	if _ := tc.malformed_const_keyword_pos(id) {
		return
	}
	if node.kind == .index_assign && tc.reject_unlowered_map_mutation
		&& tc.index_assign_lhs_is_map(node) {
		if tc.should_diagnose(id) {
			tc.record_error(.assignment_mismatch,
				'internal compiler error: unlowered map index assignment reached post-transform checker',
				id)
		}
		for i := 1; i < node.children_count; i += 2 {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if tc.check_assignment_marker(id, node) {
		return
	}
	if node.is_mut {
		lhs_id := tc.a.child(&node, 0)
		rhs_id := tc.a.child(&node, 1)
		tc.record_error_at(.assignment_mismatch, 'expecting `:=` (e.g. `mut x :=`)', id, tc.assignment_operator_pos(node,
			lhs_id, rhs_id))
		return
	}
	if tc.check_multi_return_assign(id, node) {
		return
	}
	mut i := 0
	mut ownership_lhs_ids := []flat.NodeId{}
	mut ownership_rhs_ids := []flat.NodeId{}
	mut ownership_lhs_types := []Type{}
	mut ownership_rhs_types := []Type{}
	mut smartcast_write_keys := []string{}
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_node := tc.a.nodes[int(lhs_id)]
		effective_lhs_id := tc.unwrap_paren_expr_id(lhs_id)
		effective_lhs_node := tc.a.node(effective_lhs_id)
		if tc.record_invalid_literal_assign_lhs(lhs_id, node.op) {
			tc.check_node(rhs_id)
			i += 2
			continue
		}
		if node.kind == .index_assign && effective_lhs_node.kind == .index
			&& effective_lhs_node.value == 'range' {
			tc.check_node(rhs_id)
			tc.record_error_at(.assignment_mismatch,
				'cannot reassign using range expression on the left side of an assignment',
				effective_lhs_id, tc.index_brackets_pos(effective_lhs_node))
			i += 2
			continue
		}
		if effective_lhs_node.kind == .call {
			tc.check_node(effective_lhs_id)
			tc.check_node(rhs_id)
			callee := tc.a.child_node(effective_lhs_node, 0)
			name := if callee.value.len > 0 {
				callee.value
			} else {
				tc.source_text_for_node(effective_lhs_id)
			}
			tc.record_error_at(.assignment_mismatch,
				'cannot call function `${name}()` on the left side of an assignment',
				effective_lhs_id, effective_lhs_node.pos)
			i += 2
			continue
		}
		if effective_lhs_node.kind == .prefix && effective_lhs_node.op != .mul {
			tc.check_node(rhs_id)
			op := tc.source_text_for_node(effective_lhs_id).trim_space()[..1]
			message := if effective_lhs_node.op == .amp {
				'cannot use a reference on the left side of `=`'
			} else {
				'cannot use `${op}` on the left of `=`'
			}
			tc.record_error_at(.assignment_mismatch, message, effective_lhs_id, tc.prefix_operator_pos(effective_lhs_id,
				op))
			i += 2
			continue
		}
		if effective_lhs_node.kind == .prefix && effective_lhs_node.op == .mul
			&& effective_lhs_node.children_count > 0 {
			deref_child := tc.a.child_node(effective_lhs_node, 0)
			if deref_child.kind == .call {
				tc.record_error_at(.assignment_mismatch,
					'cannot dereference a function call on the left side of an assignment, use a temporary variable',
					effective_lhs_id, tc.prefix_operator_pos(effective_lhs_id, '*'))
			}
			if tc.unsafe_depth == 0 {
				tc.record_error_at(.assignment_mismatch,
					'modifying variables via dereferencing can only be done in `unsafe` blocks',
					id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
			}
		}
		if lhs_node.kind == .selector && lhs_node.children_count > 0 {
			base_type := unalias_type(tc.resolve_type(tc.a.child(&lhs_node, 0)))
			if lhs_node.value == 'len' && (base_type is String || base_type is Array) {
				kind := if base_type is String { 'string' } else { 'array' }
				tc.check_node(rhs_id)
				tc.record_error_at(.assignment_mismatch, '`${kind}` can not be modified', lhs_id,
					tc.node_value_diagnostic_pos(lhs_id))
				i += 2
				continue
			}
		}
		if lhs_node.kind == .ident && lhs_node.value == '_' && node.op != .assign {
			tc.record_error_at(.assignment_mismatch, 'cannot modify blank `_` identifier', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
			tc.check_node(rhs_id)
			i += 2
			continue
		}
		nonmut_smartcast_assignment := lhs_node.kind == .ident && lhs_node.value in tc.smartcasts
			&& !tc.ident_is_mutable_lvalue(lhs_node.value)
		unknown_assign_ident := lhs_node.kind == .ident && lhs_node.value != '_'
			&& !tc.lvalue_ident_is_known(lhs_node.value)
		if nonmut_smartcast_assignment {
			tc.record_error_at(.assignment_mismatch,
				'cannot mutate `${lhs_node.value}` in a non-mut smartcast, use `if mut ${lhs_node.value} ...`',
				lhs_id, tc.node_value_diagnostic_pos(lhs_id))
		} else if unknown_assign_ident {
			tc.record_error_at(.unknown_ident,
				'undefined ident: `${lhs_node.value}` (use `:=` to declare a variable)', lhs_id,
				tc.node_value_diagnostic_pos(lhs_id))
		} else {
			tc.check_lvalue_mutability(lhs_id)
		}
		lhs_type := if unknown_assign_ident {
			Type(void_)
		} else if node.kind == .index_assign {
			tc.resolve_index_lvalue_type(lhs_id, node.op)
		} else {
			tc.resolve_lvalue_type(lhs_id)
		}
		if lhs_node.kind == .selector && lhs_type is Void && lhs_node.children_count > 0 {
			base_id := tc.a.child(&lhs_node, 0)
			if tc.expr_subtree_has_error(base_id) {
				tc.record_error_at(.assignment_mismatch, 'unexpected symbol `void`', lhs_id,
					tc.node_value_diagnostic_pos(lhs_id))
			}
		}
		tc.remember_expr_type(lhs_id, lhs_type)
		expected_type := tc.assignment_expected_type(lhs_id, lhs_type)
		source_rhs_type := tc.resolve_type(rhs_id)
		tc.annotate_expected_expr(rhs_id, expected_type)
		$if ownership ? {
			if tc.ownership_should_defer_aggregate_consumption(lhs_id, node.op) {
				tc.ownership_begin_defer_aggregate_consumption(rhs_id)
				tc.check_node_with_expected_context(rhs_id, expected_type)
				tc.ownership_end_defer_aggregate_consumption(rhs_id)
			} else {
				tc.check_node_with_expected_context(rhs_id, expected_type)
			}
		} $else {
			tc.check_node_with_expected_context(rhs_id, expected_type)
		}
		if lhs_node.kind == .ident && tc.ident_is_mutable_lvalue(lhs_node.value) {
			tc.check_mutable_array_immutable_references(rhs_id)
		}
		tc.check_mutable_alias_assignment_lhs(lhs_id)
		rhs_node := tc.a.nodes[int(rhs_id)]
		if lhs_node.kind == .ident && lhs_node.value == '_' && rhs_node.kind == .none_expr {
			tc.record_error_at(.assignment_mismatch,
				'cannot assign a `none` value to blank `_` identifier', rhs_id, rhs_node.pos)
			i += 2
			continue
		}
		if lhs_node.kind == .index && lhs_node.children_count > 0
			&& unalias_type(tc.resolve_type(tc.a.child(&lhs_node, 0))) is String {
			tc.record_error_at(.assignment_mismatch,
				'cannot assign to s[i] since V strings are immutable\n(note, that variables may be mutable but string values are always immutable, like in Go and Java)',
				lhs_id, tc.decl_non_name_lhs_pos(lhs_id))
			i += 2
			continue
		}
		tc.check_const_reference_assignment(lhs_id, rhs_id, false)
		dynamic_array_to_fixed := unalias_type(expected_type) is ArrayFixed
			&& rhs_node.kind == .array_literal && rhs_node.typ.len == 0
		mut rhs_type := tc.resolve_expr(rhs_id, expected_type)
		if rhs_node.kind == .or_expr && type_contains_unknown(rhs_type)
			&& !type_contains_unknown(source_rhs_type) {
			rhs_type = source_rhs_type
		}
		if rhs_type is Void {
			if tc.expr_calls_invalid_option_void_fn(rhs_id) {
				i += 2
				continue
			}
			if call_name := tc.assignment_rhs_call_name(rhs_id) {
				tc.record_error_at(.assignment_mismatch,
					'assignment mismatch: 1 variable but `${call_name}()` returns 0 values', id, tc.assignment_operator_pos(node,
					lhs_id, rhs_id))
				i += 2
				continue
			}
			if rhs_node.kind in [.if_expr, .match_stmt] {
				tc.record_error_at(.assignment_mismatch,
					'assignment mismatch: 1 variable 0 values', id, tc.assignment_operator_pos(node,
					lhs_id, rhs_id))
				i += 2
				continue
			}
		}
		if type_is_unsigned_integer(expected_type) && tc.expr_is_negative_integer_literal(rhs_id) {
			tc.record_error_at(.assignment_mismatch,
				'cannot assign negative value to unsigned integer type', rhs_id, rhs_node.pos)
			i += 2
			continue
		}
		if dynamic_array_to_fixed {
			lhs_name := tc.source_text_for_node(lhs_id)
			actual_type := if rhs_node.children_count > 0 {
				Type(Array{
					elem_type: tc.resolve_type(tc.a.child(&rhs_node, 0))
				})
			} else {
				Type(Array{
					elem_type: Type(void_)
				})
			}
			tc.record_error_with_details_at(.assignment_mismatch,
				'cannot assign to `${lhs_name}`: expected `${expected_type.name()}`, not `${actual_type.name()}`',
				rhs_id, rhs_node.pos, [
				'try adding `!` after the array literal, e.g.: `${lhs_name} = [...]!`',
			])
			i += 2
			continue
		}
		clean_expected_type := unalias_type(expected_type)
		clean_rhs_type := unalias_type(rhs_type)
		clean_source_rhs_type := unalias_type(source_rhs_type)
		if rhs_node.kind == .match_stmt && tc.expr_has_match_branch_type_error(rhs_id) {
			i += 2
			continue
		}
		if node.op == .assign && rhs_node.kind == .ident {
			if tc.unsafe_depth == 0 && ((clean_expected_type is Array && clean_rhs_type is Array
				&& lhs_node.kind == .ident)
				|| (clean_expected_type is Map && clean_rhs_type is Map)) {
				if !tc.ident_is_mutable_lvalue(rhs_node.value) {
					tc.record_notice_at(.assignment_mismatch,
						'left-side of assignment expects a mutable reference, but variable `${rhs_node.value}` is immutable, declare it with `mut` to make it mutable or clone it',
						rhs_id, rhs_node.pos)
				}
				if clean_expected_type is Array {
					tc.record_error_at(.assignment_mismatch,
						'use `array2 = array1.clone()` instead of `array2 = array1` (or use `unsafe`)',
						id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
				} else {
					tc.record_error_at(.assignment_mismatch,
						'cannot copy map: call `move` or `clone` method (or use a reference)',
						rhs_id, rhs_node.pos)
				}
			} else if clean_source_rhs_type is ArrayFixed
				&& (clean_expected_type is Pointer || expected_type.name() == 'voidptr')
				&& !tc.ident_is_explicitly_mutable_lvalue(rhs_node.value) {
				tc.record_notice_at(.assignment_mismatch,
					'left-side of assignment expects a mutable reference, but variable `${rhs_node.value}` is immutable, declare it with `mut` to make it mutable or clone it',
					rhs_id, rhs_node.pos)
			}
		}
		if clean_expected_type is OptionType && tc.expr_tail_is_nil(rhs_id) {
			if lhs_node.kind == .ident {
				base_type := unalias_type(clean_expected_type.base_type)
				tc.record_warning_at(.assignment_mismatch,
					'cannot assign a reference to a value (this will be an error soon) left=${base_type.name()} false right=nil true ptr=false',
					id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
			}
			tc.record_error_at(.assignment_mismatch, 'cannot assign `nil` to option value', rhs_id,
				tc.array_element_diagnostic_pos(rhs_id))
			i += 2
			continue
		}
		if tc.anonymous_struct_assignment_mismatch(rhs_id, rhs_type, expected_type) {
			tc.record_error_at(.assignment_mismatch,
				'cannot assign anonymous `struct` to a typed `struct`', rhs_id,
				tc.anonymous_struct_literal_brace_pos(rhs_id))
			i += 2
			continue
		}
		if tc.assignment_rhs_is_void_selector(rhs_id) {
			tc.record_error_at(.assignment_mismatch, 'assignment mismatch: 1 variable 0 values',
				id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
			i += 2
			continue
		}
		rhs_call_has_no_value := rhs_type is Unknown || rhs_type is Void
			|| (rhs_type is MultiReturn && rhs_type.types.len == 0)
		if node.op != .assign && rhs_node.kind == .call && rhs_call_has_no_value
			&& tc.expr_subtree_has_error(rhs_id) {
			call_name := tc.call_display_name(rhs_node)
			op_text := tc.compound_assignment_source_operator(lhs_id, rhs_id, node.op)
			tc.record_error_at(.assignment_mismatch,
				'assignment mismatch: 1 variable but `${call_name}()` returns 0 values', id, tc.compound_assignment_operator_pos(lhs_id,
				rhs_id, op_text))
			i += 2
			continue
		}
		if node.op != .assign {
			tc.record_compound_assignment_operand_errors(node.op, lhs_id, rhs_id, expected_type,
				rhs_type)
		}
		deref_pointer_mismatch := effective_lhs_node.kind == .prefix
			&& effective_lhs_node.op == .mul
			&& type_pointer_depth(expected_type) != type_pointer_depth(rhs_type)
			&& expected_type.name() != 'voidptr' && rhs_type.name() !in ['voidptr', 'nil']
		sum_variant_mismatch := if clean_expected_type is SumType {
			!tc.direct_sum_assignment_variant_matches(source_rhs_type, clean_expected_type)
		} else {
			false
		}
		invalid_comptime_selector_lhs := effective_lhs_node.kind == .selector
			&& effective_lhs_node.value == '$' && lhs_type is Void
		defer_open_generic_mismatch := tc.fn_context.generic_params.len > 0
			&& type_contains_unknown(rhs_type) && !invalid_comptime_selector_lhs
		if node.op == .assign && clean_rhs_type is ArrayFixed
			&& (clean_expected_type is Pointer || expected_type.name() == 'voidptr') {
			tc.record_error_at(.assignment_mismatch,
				'mismatched types `${expected_type.name()}` and `${rhs_type.name()}`', id, tc.assignment_operator_pos(node,
				lhs_id, rhs_id))
		}
		if deref_pointer_mismatch {
			tc.record_error_at(.assignment_mismatch,
				'cannot use `${rhs_type.name()}` (right side) as `${expected_type.name()}` (left side) in assignment',
				id, tc.assignment_operator_pos(node, lhs_id, rhs_id))
		} else if !defer_open_generic_mismatch && (sum_variant_mismatch
			|| !tc.assignment_types_compatible(rhs_id, rhs_type, expected_type, node.op)) {
			if clean_expected_type is Pointer && unalias_type(rhs_type) is Struct {
				tc.record_error_at(.assignment_mismatch,
					'mismatched types `${expected_type.name()}` and `${rhs_type.name()}`', id, tc.assignment_operator_pos(node,
					lhs_id, rhs_id))
			} else if unalias_type(rhs_type) is OptionType
				&& unalias_type(expected_type) !is OptionType && lhs_node.kind == .ident {
				tc.record_error_at(.assignment_mismatch,
					'cannot assign an Option value to a non-option variable', rhs_id,
					tc.array_element_diagnostic_pos(rhs_id))
			} else {
				lhs_source := if lhs_node.pos.is_valid() {
					tc.source_text_for_node(lhs_id)
				} else {
					''
				}
				lhs_name := if lhs_source.len > 0 {
					lhs_source
				} else {
					tc.assignment_lhs_source_text(node, lhs_id)
				}
				diagnostic_rhs_type := if sum_variant_mismatch {
					source_rhs_type
				} else if mut_base := tc.mut_param_expr_base(rhs_id, rhs_type) {
					mut_base
				} else {
					rhs_type
				}
				rhs_name := tc.diagnostic_expr_type_name(rhs_id, diagnostic_rhs_type)
				expected_name := expected_type.name().replace_once('fn(', 'fn (')
				diagnostic_id := if tc.should_diagnose(rhs_id) {
					rhs_id
				} else if tc.should_diagnose(lhs_id) {
					lhs_id
				} else {
					id
				}
				diagnostic_pos := if rhs_node.kind == .or_expr && rhs_node.children_count > 0
					&& tc.a.child_node(&rhs_node, 0).kind == .selector {
					selector_id := tc.a.child(&rhs_node, 0)
					tc.assignment_or_selector_diagnostic_pos(node, lhs_id, selector_id)
				} else if rhs_node.kind == .prefix && rhs_node.op == .amp
					&& rhs_node.children_count > 0
					&& tc.a.child_node(&rhs_node, 0).kind == .array_literal {
					tc.address_operator_pos(rhs_id)
				} else if rhs_node.kind == .prefix && rhs_node.op == .arrow {
					tc.prefix_operator_pos(rhs_id, '<-')
				} else if rhs_node.pos.is_valid() {
					tc.array_element_diagnostic_pos(rhs_id)
				} else {
					tc.assignment_rhs_diagnostic_pos(node, lhs_id, rhs_id)
				}
				message := if lhs_name.len == 0
					&& tc.assignment_source_line_contains_or_block(node, lhs_id) {
					'wrong return type `${rhs_name}` in the `or {}` block, expected `${expected_name}`'
				} else {
					'cannot assign to `${lhs_name}`: expected `${expected_name}`, not `${rhs_name}`'
				}
				tc.record_error_at(.assignment_mismatch, message, diagnostic_id, diagnostic_pos)
			}
		}
		if node.op == .power_assign && expected_type !is Unknown && rhs_type !is Unknown
			&& tc.should_diagnose(id) && (!infix_power_type_is_numeric(expected_type)
			|| !infix_power_type_is_numeric(rhs_type)) {
			_ := tc.infix_operator_return_type(.power, expected_type, rhs_type) or {
				tc.record_error(.assignment_mismatch,
					'operator `**=` requires numeric operands; got `${expected_type.name()}` and `${rhs_type.name()}`', id)
				Type(void_)
			}
		}
		$if ownership ? {
			tc.check_ownership_map_assignment_key(lhs_id, node.op)
			tc.check_ownership_uncloneable_overlapping_map_assignment(lhs_id, rhs_id, lhs_type,
				node.op, id)
			ownership_lhs_ids << lhs_id
			ownership_rhs_ids << rhs_id
			ownership_lhs_types << lhs_type
			ownership_rhs_types << rhs_type
		}
		if node.kind in [.assign, .selector_assign, .index_assign] {
			if tc.expr_is_method_value(rhs_id) && !tc.lvalue_is_local_var(lhs_id) {
				// Storing a method value into a struct field (`h.cb = ..`), an array/map element
				// (`cbs[i] = ..`), or a global lets it outlive the per-site static `_mvctx_N`
				// receiver slot, which the next evaluation of the same site overwrites — so every
				// stored callback would use the last receiver. Reject it like the other escapes.
				tc.reject_stored_method_value(rhs_id)
			} else {
				tc.track_method_value_local(lhs_id, rhs_id)
			}
			if tc.expr_is_capturing_fn_literal_value(rhs_id) && !tc.lvalue_is_local_var(lhs_id) {
				if node.kind == .index_assign {
					tc.reject_stored_capturing_fn_literal(rhs_id)
				} else {
					tc.reject_stored_or_returned_capturing_fn_literal(rhs_id)
				}
			} else {
				tc.track_capturing_fn_literal_local(lhs_id, rhs_id, ScopeBindingOwner{})
			}
			tc.track_variadic_fn_value_local(lhs_id, rhs_id)
		}
		tc.update_unsafe_reference_alias_assignment(lhs_id, rhs_id, expected_type, node.op)
		lhs_key := tc.expr_key(lhs_id)
		if lhs_key.len > 0 {
			lhs := tc.a.node(lhs_id)
			if lhs.kind == .ident && expected_type is OptionType && rhs_type !is OptionType
				&& tc.expr_compatible(rhs_id, rhs_type, expected_type.base_type) {
				tc.smartcasts[lhs_key] = expected_type.base_type
			} else if !tc.assignment_preserves_smartcast(lhs_id, rhs_id, rhs_type) {
				smartcast_write_keys << lhs_key
			}
		}
		i += 2
	}
	for key in smartcast_write_keys {
		tc.invalidate_smartcasts_for_write_key(key)
	}
	$if ownership ? {
		tc.ownership_after_assign_pairs(ownership_lhs_ids, ownership_rhs_ids, ownership_lhs_types,
			ownership_rhs_types, node.op, id)
	} $else {
		_ = ownership_lhs_ids
		_ = ownership_rhs_ids
		_ = ownership_lhs_types
		_ = ownership_rhs_types
	}
}

fn (mut tc TypeChecker) update_unsafe_reference_alias_assignment(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type Type, op flat.Op) {
	if op != .assign || unalias_type(lhs_type) !is Map {
		return
	}
	lhs := tc.a.node(tc.unwrap_paren_expr_id(lhs_id))
	if lhs.kind != .ident || lhs.value == '_' {
		return
	}
	owner := tc.cur_scope.lookup_owner(lhs.value) or { return }
	key := owner.storage_key()
	if key.len == 0 {
		return
	}
	if tc.expr_is_unsafe_reference_alias(rhs_id) {
		tc.fn_context.unsafe_reference_alias_owners[key] = true
	} else {
		tc.fn_context.unsafe_reference_alias_owners.delete(key)
	}
}

fn intersect_unsafe_reference_alias_states(states []map[string]bool, fallback map[string]bool) map[string]bool {
	if states.len == 0 {
		return fallback.clone()
	}
	mut result := states[0].clone()
	for key in result.keys() {
		if states.any(!it[key]) {
			result.delete(key)
		}
	}
	return result
}

fn apply_unsafe_reference_alias_state_delta(before map[string]bool, after map[string]bool, target map[string]bool) map[string]bool {
	mut result := target.clone()
	for key, _ in before {
		if !after[key] {
			result.delete(key)
		}
	}
	for key, _ in after {
		if !before[key] {
			result[key] = true
		}
	}
	return result
}

fn (mut tc TypeChecker) merge_unsafe_reference_alias_short_circuit_state(op flat.Op, lhs_id flat.NodeId, skipped_rhs map[string]bool) {
	executed_rhs := tc.fn_context.unsafe_reference_alias_owners.clone()
	if lhs := tc.constant_bool_value(lhs_id) {
		rhs_executes := (op == .logical_and && lhs) || (op == .logical_or && !lhs)
		tc.fn_context.unsafe_reference_alias_owners = if rhs_executes {
			executed_rhs.clone()
		} else {
			skipped_rhs.clone()
		}
		return
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states([
		skipped_rhs,
		executed_rhs,
	], skipped_rhs)
}

fn (tc &TypeChecker) unwrap_paren_expr_id(id flat.NodeId) flat.NodeId {
	mut current_id := id
	for tc.valid_node_id(current_id) {
		current := tc.a.node(current_id)
		if current.kind != .paren || current.children_count == 0 {
			break
		}
		current_id = tc.a.child(current, 0)
	}
	return current_id
}

fn (mut tc TypeChecker) record_invalid_literal_assign_lhs(lhs_id flat.NodeId, op flat.Op) bool {
	mut diagnostic_id := lhs_id
	mut lhs := tc.a.node(lhs_id)
	if lhs.kind == .paren && lhs.children_count > 0 {
		diagnostic_id = tc.a.child(lhs, 0)
		lhs = tc.a.node(diagnostic_id)
	}
	if lhs.kind == .infix {
		op_text := infix_operator_name(lhs.op) or { '' }
		tc.record_error_at(.assignment_mismatch,
			'cannot use infix expression on the left side of `${assignment_operator_text(op)}`',
			diagnostic_id, tc.infix_operator_pos(lhs, op_text))
		if !tc.expr_subtree_contains_ident(diagnostic_id) {
			display := tc.source_text_for_node(diagnostic_id)
			tc.record_error_at(.assignment_mismatch,
				'non-name literal value `${display}` on left side of `${assignment_operator_text(op)}`',
				diagnostic_id, lhs.pos)
		}
		return true
	} else if lhs.kind !in [.int_literal, .float_literal, .bool_literal, .char_literal,
		.string_literal, .struct_init, .cast_expr] {
		return false
	}
	mut display := tc.source_text_for_node(diagnostic_id)
	if lhs.kind == .struct_init {
		display = '${lhs.value}{....}'
	}
	tc.record_error_at(.assignment_mismatch,
		'non-name literal value `${display}` on left side of `${assignment_operator_text(op)}`',
		diagnostic_id, lhs.pos)
	return true
}

fn (tc &TypeChecker) expr_subtree_contains_ident(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return true
	}
	for i in 0 .. node.children_count {
		if tc.expr_subtree_contains_ident(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) prefix_operator_pos(id flat.NodeId, op string) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if relative := source[start..end].index(op) {
			op_start := start + relative
			return token.new_span(node.pos.id, op_start, op_start + op.len)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) anonymous_struct_assignment_mismatch(expr_id flat.NodeId, actual Type, expected Type) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	expr := tc.a.nodes[int(expr_id)]
	if expr.kind != .struct_init {
		return false
	}
	expected_struct := struct_type_from_type(expected) or { return false }
	actual_is_anonymous := if actual_struct := struct_type_from_type(actual) {
		is_anonymous_struct_name(actual_struct.name)
	} else {
		false
	}
	return (is_anonymous_struct_name(expr.value) || actual_is_anonymous)
		&& !is_anonymous_struct_name(expected_struct.name)
}

fn (tc &TypeChecker) anonymous_struct_literal_brace_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.nodes[int(id)]
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if start < end {
		if relative := source[start..end].index('{') {
			brace := start + relative
			return token.new_span(node.pos.id, brace, brace + 1)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) empty_map_literal_diagnostic_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	anchor := int_min(int_max(node.pos.offset, 0), source.len)
	mut line_start := anchor
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	line_end := source.index_after('\n', anchor) or { source.len }
	if relative := source[line_start..line_end].last_index('{}') {
		start := line_start + relative
		return token.new_span(node.pos.id, start, start + 2)
	}
	return node.pos
}

fn (tc &TypeChecker) assignment_rhs_is_void_selector(rhs_id flat.NodeId) bool {
	if !tc.valid_node_id(rhs_id) {
		return false
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind != .selector || rhs.children_count == 0 {
		return false
	}
	if tc.errors.any(it.node == rhs_id && it.kind == .unknown_field
		&& it.msg.starts_with('ambiguous field `'))
	{
		return true
	}
	resolved := tc.resolve_type(rhs_id)
	if resolved !is Void && resolved !is Unknown {
		return false
	}
	base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
	if base_type is Struct && tc.embedded_field_candidates(base_type.name, rhs.value).len > 1 {
		return true
	}
	return base_type is Void || resolved is Void
		|| (base_type !is Unknown && tc.selector_type(rhs_id, rhs) == none)
}

fn (tc &TypeChecker) unknown_imported_enum_selector(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base := tc.a.child_node(node, 0)
	if base.kind != .selector || base.children_count == 0 || base.value.len == 0
		|| !base.value[0].is_capital() {
		return false
	}
	module_node := tc.a.child_node(base, 0)
	if module_node.kind != .ident || !tc.has_active_import(module_node.value) {
		return false
	}
	module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
	return tc.resolve_enum_name('${module_name}.${base.value}') == none
}

fn (tc &TypeChecker) assignment_operator_pos(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) token.Pos {
	if !tc.valid_node_id(lhs_id) || !tc.valid_node_id(rhs_id) {
		return node.pos
	}
	lhs := tc.a.nodes[int(lhs_id)]
	rhs := tc.a.nodes[int(rhs_id)]
	file := tc.a.source_files[lhs.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, lhs.pos.end)
	end := int_min(rhs.pos.offset, source.len)
	if start < end {
		if relative := source[start..end].index(':=') {
			op_start := start + relative
			return token.new_span(lhs.pos.id, op_start, op_start + 2)
		}
		if relative := source[start..end].index('=') {
			op_start := start + relative
			return token.new_span(lhs.pos.id, op_start, op_start + 1)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) assignment_rhs_diagnostic_pos(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) token.Pos {
	if tc.valid_node_id(rhs_id) {
		rhs := tc.a.node(rhs_id)
		if rhs.kind == .prefix && rhs.op == .arrow {
			return tc.prefix_operator_pos(rhs_id, '<-')
		}
		if rhs.pos.is_valid() {
			return tc.array_element_diagnostic_pos(rhs_id)
		}
	}
	line_pos := tc.assignment_source_line_pos(node, lhs_id) or { return node.pos }
	file := tc.a.source_files[line_pos.id] or { return line_pos }
	source := tc.source_texts_by_file[file.name] or { return line_pos }
	line := source[line_pos.offset..line_pos.end]
	op_relative := line.index('=') or { return line_pos }
	mut start := line_pos.offset + op_relative + 1
	for start < line_pos.end && source[start] in [` `, `\t`] {
		start++
	}
	mut end := line_pos.end
	for end > start && source[end - 1] in [` `, `\t`, `\r`, `\n`] {
		end--
	}
	if relative := source[start..end].index('[') {
		bracket_start := start + relative
		if close := source[bracket_start..end].index(']') {
			return token.new_span(line_pos.id, bracket_start, bracket_start + close + 1)
		}
	}
	return token.new_span(line_pos.id, start, end)
}

fn (tc &TypeChecker) assignment_or_selector_diagnostic_pos(node flat.Node, lhs_id flat.NodeId, selector_id flat.NodeId) token.Pos {
	line_pos := tc.assignment_source_line_pos(node, lhs_id) or { return node.pos }
	file := tc.a.source_files[line_pos.id] or { return line_pos }
	source := tc.source_texts_by_file[file.name] or { return line_pos }
	line := source[line_pos.offset..line_pos.end]
	op_relative := line.index('=') or { return line_pos }
	mut diagnostic_selector_id := selector_id
	for {
		current := tc.a.node(diagnostic_selector_id)
		if current.kind != .selector || current.value !in ['ok', 'value']
			|| current.children_count == 0 {
			break
		}
		base_id := tc.a.child(current, 0)
		base := tc.a.node(base_id)
		if base.kind != .selector {
			break
		}
		diagnostic_selector_id = base_id
	}
	selector := tc.a.node(diagnostic_selector_id)
	mut needle := selector.value
	mut value_offset := 0
	if selector.children_count > 0 {
		base := tc.a.child_node(selector, 0)
		if base.kind == .ident && base.value.len > 0 {
			needle = '${base.value}.${selector.value}'
			value_offset = base.value.len + 1
		}
	}
	rhs_start := line_pos.offset + op_relative + 1
	relative := source[rhs_start..line_pos.end].index(needle) or { return line_pos }
	start := rhs_start + relative + value_offset
	lhs := tc.a.node(lhs_id)
	reported_column := if lhs.pos.is_valid() && lhs.pos.offset >= line_pos.offset {
		source[line_pos.offset..lhs.pos.offset].replace('\t', '    ').len + 1
	} else {
		0
	}
	return token.new_span(line_pos.id, start, start + selector.value.len).with_reported_column(reported_column)
}

fn (tc &TypeChecker) assignment_lhs_source_text(node flat.Node, lhs_id flat.NodeId) string {
	if tc.valid_node_id(lhs_id) {
		lhs := tc.a.node(lhs_id)
		if lhs.pos.is_valid() {
			return tc.source_text_for_node(lhs_id)
		}
	}
	line_pos := tc.assignment_source_line_pos(node, lhs_id) or { return '' }
	file := tc.a.source_files[line_pos.id] or { return '' }
	source := tc.source_texts_by_file[file.name] or { return '' }
	line := source[line_pos.offset..line_pos.end]
	op_relative := line.index('=') or { return '' }
	return line[..op_relative].trim_space()
}

fn (tc &TypeChecker) assignment_source_line_contains_or_block(node flat.Node, lhs_id flat.NodeId) bool {
	line_pos := tc.assignment_source_line_pos(node, lhs_id) or { return false }
	file := tc.a.source_files[line_pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	return source[line_pos.offset..line_pos.end].contains('or {')
}

fn (tc &TypeChecker) assignment_source_line_pos(node flat.Node, lhs_id flat.NodeId) ?token.Pos {
	mut source_pos := node.pos
	if tc.valid_node_id(lhs_id) {
		lhs := tc.a.node(lhs_id)
		if lhs.pos.is_valid() {
			source_pos = lhs.pos
		}
	}
	if !source_pos.is_valid() {
		return none
	}
	file := tc.a.source_files[source_pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	mut cursor := int_min(int_max(source_pos.offset, 0), source.len)
	for _ in 0 .. 4 {
		line_start := if relative := source[..cursor].last_index('\n') {
			relative + 1
		} else {
			0
		}
		line_end := source.index_after('\n', line_start) or { source.len }
		if source[line_start..line_end].contains('=') {
			return token.new_span(source_pos.id, line_start, line_end)
		}
		if line_start == 0 {
			break
		}
		cursor = line_start - 1
	}
	return none
}

fn (mut tc TypeChecker) record_compound_assignment_operand_errors(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type Type, rhs_type Type) {
	mut op_text := tc.compound_assignment_source_operator(lhs_id, rhs_id, op)
	if op_text.len == 0 {
		return
	}
	lhs_binding_name := tc.pointer_diagnostic_binding_type_name(lhs_id, lhs_type)
	if op in [.plus_assign, .minus_assign] && unalias_type(lhs_type) is Pointer
		&& lhs_binding_name !in ['voidptr', 'nil']
		&& (rhs_type.is_integer() || tc.a.node(rhs_id).kind == .int_literal) {
		lhs := tc.a.node(lhs_id)
		is_implicitly_dereferenced_mut_param := lhs.kind == .ident
			&& lhs.value in tc.fn_context.mut_param_base_types
		if tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
			&& !is_implicitly_dereferenced_mut_param {
			tc.record_warning_at(.assignment_mismatch,
				'pointer arithmetic is only allowed in `unsafe` blocks', lhs_id, tc.compound_assignment_operator_pos(lhs_id,
				rhs_id, op_text))
		}
		return
	}
	if infix_op := compound_assignment_infix_op(op) {
		if signature := tc.infix_operator_signature(infix_op, lhs_type) {
			lhs_name := unwrap_pointer(lhs_type).name()
			if signature.param_count < 2 {
				message := if op in [.plus_assign, .minus_assign] {
					'operator `${op_text}` not defined on left operand type `${lhs_name}`'
				} else {
					'operator ${op_text} not defined on left operand type `${lhs_name}`'
				}
				tc.record_error(.assignment_mismatch, message, lhs_id)
				if signature.return_type.name() != lhs_name {
					operator_name := infix_operator_name(infix_op) or { '' }
					tc.record_error_at(.assignment_mismatch,
						'operator `${operator_name}` must return `${lhs_name}` to be used as an assignment operator',
						lhs_id, tc.compound_assignment_operator_pos(lhs_id, rhs_id, op_text))
				}
				return
			}
			rhs := tc.a.node(rhs_id)
			rhs_is_literal := rhs.kind in [.int_literal, .float_literal, .char_literal,
				.string_literal, .bool_literal]
			operand_matches := rhs_type.name() == signature.param_type.name()
				|| (rhs_is_literal && tc.type_compatible(rhs_type, signature.param_type))
			if operand_matches {
				if signature.return_type.name() != lhs_name {
					operator_name := infix_operator_name(infix_op) or { '' }
					tc.record_error_at(.assignment_mismatch,
						'operator `${operator_name}` must return `${lhs_name}` to be used as an assignment operator',
						lhs_id, tc.compound_assignment_operator_pos(lhs_id, rhs_id, op_text))
				}
				return
			}
			if lhs_type is Alias && rhs_type is Alias && lhs_type.name != rhs_type.name
				&& tc.assignment_types_compatible(rhs_id, rhs_type, lhs_type, op) {
				lhs_text := tc.source_text_for_node(lhs_id)
				tc.record_error_at(.assignment_mismatch,
					'cannot assign to `${lhs_text}`: expected `${lhs_type.name}`, not `${rhs_type.name}`',
					rhs_id, tc.array_element_diagnostic_pos(rhs_id))
			} else if op in [.plus_assign, .minus_assign] {
				if unalias_type(lhs_type) !is Struct {
					rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
					tc.record_error_at(.assignment_mismatch,
						'invalid right operand: ${lhs_name} ${op_text} ${rhs_name}', rhs_id,
						tc.array_element_diagnostic_pos(rhs_id))
				}
			}
			return
		}
	}
	if op_text in ['&&=', '||='] {
		if unalias_type(lhs_type) !is Primitive
			|| !(unalias_type(lhs_type) as Primitive).props.has(.boolean) {
			tc.record_error(.assignment_mismatch,
				'operator ${op_text} not defined on left operand type `${lhs_type.name()}`', lhs_id)
			return
		}
	}
	if op == .minus_assign && (array_type_from_receiver(lhs_type) != none
		|| map_type_from_receiver(unalias_type(lhs_type)) != none) {
		tc.record_error_at(.assignment_mismatch,
			'undefined operation `${lhs_type.name()}` - `${rhs_type.name()}`', lhs_id, tc.compound_assignment_operator_pos(lhs_id,
			rhs_id, op_text))
		return
	}
	lhs_name := lhs_binding_name
	rhs_name := tc.diagnostic_expr_type_name(rhs_id, rhs_type)
	rhs_node := tc.a.node(rhs_id)
	rhs_is_integer := rhs_type.is_integer() || rhs_node.kind == .int_literal
	lhs_is_primitive_alias := lhs_type is Alias
		&& (unalias_type(lhs_type) is Primitive || unalias_type(lhs_type) is String)
	rhs_is_primitive_alias := rhs_type is Alias
		&& (unalias_type(rhs_type) is Primitive || unalias_type(rhs_type) is String)
	lhs_supports := match op {
		.plus_assign {
			infix_power_type_is_numeric(lhs_type) || lhs_name == 'string'
				|| lhs_name == 'rune' || (unalias_type(lhs_type) is Pointer
				&& lhs_name !in ['voidptr', 'nil'] && rhs_is_integer)
				|| lhs_is_primitive_alias
		}
		.minus_assign, .mul_assign, .div_assign, .mod_assign {
			infix_power_type_is_numeric(lhs_type)
				|| (op == .minus_assign && unalias_type(lhs_type) is Pointer
				&& lhs_name !in ['voidptr', 'nil'] && rhs_is_integer)
				|| lhs_is_primitive_alias
		}
		.amp_assign, .pipe_assign, .xor_assign, .left_shift_assign, .right_shift_assign,
		.right_shift_unsigned_assign {
			lhs_type.is_integer()
		}
		else {
			true
		}
	}
	if !lhs_supports {
		if lhs_type is Unknown && tc.fn_context.generic_params.len > 0 {
			return
		}
		message := if op in [.plus_assign, .minus_assign] {
			'operator `${op_text}` not defined on left operand type `${lhs_name}`'
		} else {
			'operator ${op_text} not defined on left operand type `${lhs_name}`'
		}
		tc.record_error(.assignment_mismatch, message, lhs_id)
	}
	if !lhs_supports && tc.assignment_types_compatible(rhs_id, rhs_type, lhs_type, op) {
		return
	}
	rhs_supports := match op {
		.plus_assign {
			if lhs_name == 'string' {
				rhs_name in ['string', 'rune']
			} else {
				infix_power_type_is_numeric(rhs_type) || rhs_is_primitive_alias
			}
		}
		.minus_assign {
			lhs_name != 'string'
				&& (infix_power_type_is_numeric(rhs_type) || rhs_is_primitive_alias)
		}
		.mul_assign, .div_assign, .mod_assign {
			infix_power_type_is_numeric(rhs_type) || rhs_is_primitive_alias
		}
		.amp_assign, .pipe_assign, .xor_assign, .left_shift_assign, .right_shift_assign,
		.right_shift_unsigned_assign {
			rhs_is_integer
		}
		else {
			true
		}
	}
	if rhs_supports {
		return
	}
	if op in [.plus_assign, .minus_assign] {
		tc.record_error(.assignment_mismatch,
			'invalid right operand: ${lhs_name} ${op_text} ${rhs_name}', rhs_id)
	} else {
		tc.record_error(.assignment_mismatch,
			'operator ${op_text} not defined on right operand type `${rhs_name}`', rhs_id)
	}
}
