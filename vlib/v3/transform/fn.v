module transform

import v3.flat
import v3.types

// resolve_call_name resolves the function name from a .call node.
// child[0] is the function expression: .ident for plain calls, .selector for method calls.
fn (t &Transformer) resolve_call_name(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return ''
	}
	fn_node := t.a.nodes[int(fn_id)]
	match fn_node.kind {
		.ident {
			name := fn_node.value
			if t.var_type(name).len > 0 {
				return name
			}
			// Try qualified with current module
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qname := '${t.cur_module}.${name}'
				if t.is_known_fn_name(qname) {
					return qname
				}
			}
			// Try unqualified name after current-module authority.
			if t.is_known_fn_name(name) {
				return name
			}
			return name
		}
		.selector {
			if fn_node.children_count > 0 {
				base_id := t.a.children[fn_node.children_start]
				base := t.a.nodes[int(base_id)]
				if base.kind == .ident {
					full := '${base.value}.${fn_node.value}'
					if t.is_known_fn_name(full) {
						return full
					}
				}
				method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
				if method_name.len > 0 {
					return method_name
				}
				if base.kind == .ident {
					return '${base.value}.${fn_node.value}'
				}
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) local_fn_decl_return_type(name string) ?string {
	if name.len == 0 {
		return none
	}
	qname := transform_qualified_fn_name(t.cur_module, name)
	if ret := t.fn_ret_types[qname] {
		return ret
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[qname] {
			return ret.name()
		}
	}
	return none
}

fn (t &Transformer) local_fn_value_return_type_from_type(typ string) ?string {
	if typ.len == 0 {
		return none
	}
	normalized := t.normalize_type_alias(typ)
	return fn_type_return_type_text(normalized)
}

fn fn_type_return_type_text(typ string) ?string {
	clean := typ.trim_space()
	if !(clean.starts_with('fn(') || clean.starts_with('fn (')) {
		return none
	}
	open_idx := clean.index_u8(`(`)
	if open_idx < 0 {
		return none
	}
	mut depth := 0
	for i in open_idx .. clean.len {
		if clean[i] == `(` {
			depth++
		} else if clean[i] == `)` {
			depth--
			if depth == 0 {
				ret := clean[i + 1..].trim_space()
				if ret.len == 0 {
					return 'void'
				}
				return ret
			}
		}
	}
	return none
}

// is_known_fn_name reports whether is known fn name applies in transform.
fn (t &Transformer) is_known_fn_name(name string) bool {
	if name in t.fn_ret_types {
		return true
	}
	if !isnil(t.tc) {
		return name in t.tc.fn_ret_types || name in t.tc.fn_param_types
	}
	return false
}

// resolve_receiver_method_name resolves resolve receiver method name information for transform.
fn (t &Transformer) resolve_receiver_method_name(base_id flat.NodeId, method string) string {
	if method.len == 0 {
		return ''
	}
	mut base_type := t.lvalue_type(base_id)
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.len == 0 {
		return ''
	}
	mut raw_var_clean := ''
	if raw_var_type := t.raw_var_type_for_expr(base_id) {
		raw_clean := if raw_var_type.starts_with('&') {
			raw_var_type[1..]
		} else {
			raw_var_type
		}
		raw_var_clean = raw_clean
		if raw_clean.len > 0 && raw_clean != base_type {
			if alias_method := t.resolve_alias_receiver_method(raw_clean, method) {
				if t.receiver_method_matches_base_type(alias_method, base_id) {
					return alias_method
				}
			}
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				if t.receiver_method_matches_base_type(method_name, base_id) {
					return method_name
				}
			}
		}
	}
	if raw_const_type := t.raw_const_type_name_for_expr(base_id) {
		raw_clean := if raw_const_type.starts_with('&') {
			raw_const_type[1..]
		} else {
			raw_const_type
		}
		if raw_clean.len > 0 && raw_clean != base_type && raw_clean != raw_var_clean {
			if alias_method := t.resolve_alias_receiver_method(raw_clean, method) {
				if t.receiver_method_matches_base_type(alias_method, base_id) {
					return alias_method
				}
			}
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				if t.receiver_method_matches_base_type(method_name, base_id) {
					return method_name
				}
			}
		}
	}
	if alias_method := t.resolve_alias_receiver_method(base_type, method) {
		if t.receiver_method_matches_base_type(alias_method, base_id) {
			return alias_method
		}
	}
	if method_name := t.resolve_receiver_method_for_type(base_type, method) {
		if t.receiver_method_matches_base_type(method_name, base_id) {
			return method_name
		}
	}
	if embedded_method := t.resolve_embedded_receiver_method(base_type, method) {
		if t.receiver_method_matches_base_type(embedded_method, base_id) {
			return embedded_method
		}
	}
	return ''
}

fn (t &Transformer) resolve_collection_receiver_method_name(base_id flat.NodeId, method string, clean_base_type string) string {
	if method.len == 0 {
		return ''
	}
	if raw_var_type := t.raw_var_type_for_expr(base_id) {
		raw_clean := if raw_var_type.starts_with('&') { raw_var_type[1..] } else { raw_var_type }
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				return method_name
			}
		}
	}
	if raw_const_type := t.raw_const_type_name_for_expr(base_id) {
		raw_clean := if raw_const_type.starts_with('&') {
			raw_const_type[1..]
		} else {
			raw_const_type
		}
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				return method_name
			}
		}
	}
	if method_name := t.resolve_receiver_method_for_type(clean_base_type, method) {
		return method_name
	}
	return ''
}

// resolve_receiver_method_for_type resolves resolve_receiver_method_for_type logic in transform.
fn (t &Transformer) resolve_receiver_method_for_type(receiver_type string, method string) ?string {
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		for candidate in t.map_receiver_method_candidates(clean_type, method) {
			if t.is_known_fn_name(candidate) {
				return candidate
			}
		}
	} else {
		if method_name := t.resolve_specialized_generic_receiver_method(clean_type, method) {
			return method_name
		}
		direct := '${clean_type}.${method}'
		if t.is_known_fn_name(direct) {
			return direct
		}
		for receiver in generic_receiver_flat_type_variants(clean_type) {
			flat_method := '${receiver}.${method}'
			if t.is_known_fn_name(flat_method) {
				return flat_method
			}
		}
		for receiver in flattened_generic_receiver_short_variants(clean_type) {
			flat_method := '${receiver}.${method}'
			if t.is_known_fn_name(flat_method) {
				return flat_method
			}
		}
	}
	if clean_type.starts_with('[]') {
		elem_type := clean_type[2..]
		short_elem := if elem_type.contains('.') { elem_type.all_after_last('.') } else { elem_type }
		short_array := '[]${short_elem}.${method}'
		if t.is_known_fn_name(short_array) {
			return short_array
		}
		if elem_type.contains('.') {
			qualified_array := '${elem_type.all_before_last('.')}.[]${short_elem}.${method}'
			if t.is_known_fn_name(qualified_array) {
				return qualified_array
			}
		} else if transform_can_prefix_collection_receiver(t.cur_module) {
			current_module_array := '${t.cur_module}.[]${short_elem}.${method}'
			if t.is_known_fn_name(current_module_array) {
				return current_module_array
			}
		}
	} else if clean_type.contains('.') {
		short_type := clean_type.all_after_last('.')
		short_method := '${short_type}.${method}'
		if t.is_known_fn_name(short_method) {
			return short_method
		}
		qualified_short_method := '${clean_type.all_before_last('.')}.${short_type}.${method}'
		if t.is_known_fn_name(qualified_short_method) {
			return qualified_short_method
		}
	}
	if method_name := t.unique_receiver_method_suffix_match(t.receiver_method_candidates(clean_type,
		method))
	{
		return method_name
	}
	if !isnil(t.tc) {
		if target := t.tc.type_aliases[clean_type] {
			alias_method := '${target}.${method}'
			if t.is_known_fn_name(alias_method) {
				return alias_method
			}
		}
	}
	return none
}

fn (t &Transformer) unique_receiver_method_suffix_match(candidates []string) ?string {
	mut found := ''
	for candidate in candidates {
		name := t.receiver_method_suffix_index[candidate] or { continue }
		if name == receiver_method_suffix_ambiguous {
			return none
		}
		if found.len > 0 && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn (t &Transformer) resolve_specialized_generic_receiver_method(receiver_type string, method string) ?string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 {
		return none
	}
	short_args := generic_type_args_short(args)
	suffix := generic_type_suffixes(args)
	mut candidates := []string{}
	candidates << '${base}[${short_args}].${method}'
	candidates << '${base}_${suffix}.${method}'
	candidates << c_name('${base}_${suffix}.${method}')
	if base.contains('.') {
		module_name := base.all_before_last('.')
		short_base := base.all_after_last('.')
		candidates << '${short_base}[${short_args}].${method}'
		candidates << '${short_base}_${suffix}.${method}'
		candidates << '${module_name}.${short_base}_${suffix}.${method}'
		candidates << c_name('${module_name}.${short_base}_${suffix}.${method}')
	}
	for candidate in candidates {
		if t.is_known_fn_name(candidate) {
			return candidate
		}
	}
	return none
}

// resolve_alias_receiver_method converts resolve alias receiver method data for transform.
fn (t &Transformer) resolve_alias_receiver_method(base_type string, method string) ?string {
	if isnil(t.tc) || base_type.len == 0 || method.len == 0 {
		return none
	}
	clean_base := t.normalize_type_alias(base_type)
	if alias_method := t.alias_methods['${clean_base}.${method}'] {
		return alias_method
	}
	if !t.is_integer_type_name(clean_base) {
		return none
	}
	for name, params in t.tc.fn_param_types {
		if !name.ends_with('.${method}') || params.len == 0 {
			continue
		}
		receiver_name := name.all_before_last('.')
		if receiver_name.len == 0 || receiver_name !in t.tc.type_aliases {
			continue
		}
		param_name := params[0].name()
		if t.alias_receiver_type_matches(clean_base, param_name) {
			return name
		}
	}
	return none
}

fn (t &Transformer) resolve_embedded_receiver_method(base_type string, method string) ?string {
	if base_type.len == 0 || method.len == 0 {
		return none
	}
	mut lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	info := t.lookup_struct_info(lookup_type) or { return none }
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		clean_field := if field_type.starts_with('&') { field_type[1..] } else { field_type }
		if method_name := t.resolve_receiver_method_for_type(clean_field, method) {
			return method_name
		}
		if method_name := t.resolve_embedded_receiver_method(clean_field, method) {
			return method_name
		}
	}
	return none
}

// alias_receiver_type_matches converts alias receiver type matches data for transform.
fn (t &Transformer) alias_receiver_type_matches(base_type string, alias_type string) bool {
	if base_type.len == 0 || alias_type.len == 0 {
		return false
	}
	clean_alias := if alias_type.starts_with('&') { alias_type[1..] } else { alias_type }
	alias_target := t.normalize_type_alias(clean_alias)
	if alias_target == base_type {
		return true
	}
	if !isnil(t.tc) {
		alias_c_type := t.tc.c_type(t.tc.parse_type(alias_target))
		base_c_type := t.tc.c_type(t.tc.parse_type(base_type))
		if alias_c_type == base_c_type {
			return true
		}
	}
	return t.is_integer_type_name(alias_target) && t.is_integer_type_name(base_type)
}

// is_integer_type_name reports whether is integer type name applies in transform.
fn (t &Transformer) is_integer_type_name(typ string) bool {
	return typ in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'rune',
		'isize', 'usize']
}

// raw_var_type_for_expr supports raw var type for expr handling for Transformer.
fn (t &Transformer) raw_var_type_for_expr(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		typ := t.raw_var_type(node.value)
		if typ.len > 0 {
			return typ
		}
	}
	if node.typ.len > 0 {
		return node.typ
	}
	return none
}

// raw_const_type_name_for_expr supports raw const type name for expr handling for Transformer.
fn (t &Transformer) raw_const_type_name_for_expr(id flat.NodeId) ?string {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(id)]
	if t.selector_const_base_is_value(node) {
		return none
	}
	name := t.expr_key(id)
	if name.len == 0 {
		return none
	}
	key := t.const_type_key_in_context(name, t.cur_module, t.cur_file) or { return none }
	typ := t.tc.const_types[key] or { return none }
	return typ.name()
}

// resolve_method_receiver_type determines the receiver type for method calls.
// For a call where child[0] is a .selector, resolves the type of the selector's base expression.
fn (t &Transformer) resolve_method_receiver_type(call_node flat.Node) string {
	if call_node.children_count == 0 {
		return ''
	}
	fn_id := t.a.children[call_node.children_start]
	if int(fn_id) < 0 {
		return ''
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return ''
	}
	base_id := t.a.children[fn_node.children_start]
	return t.resolve_expr_type(base_id)
}

// normalize_generic_call_expr transforms normalize generic call expr data for transform.
fn (mut t Transformer) normalize_generic_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return id
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return id
	}
	if t.index_callee_is_value_index(fn_node) {
		return id
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind !in [.ident, .selector] {
		return id
	}
	type_arg := t.generic_call_type_args_name(fn_node)
	if type_arg.len == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << base_id
	for i in 1 .. node.children_count {
		children << t.a.child(&node, i)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          type_arg
		typ:            node.typ
	})
}

// generic_call_type_arg_name supports generic call type arg name handling for Transformer.
fn (t &Transformer) generic_call_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			// Map type arguments are represented by a synthetic `Map_key_value`
			// identifier, while `typ` retains the source-level `map[key]value`.
			// Keep the latter so comptime type groups and generic substitution can
			// still inspect both map components.
			if node.typ.starts_with('map[') {
				return node.typ
			}
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.generic_call_type_arg_name(t.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := t.generic_call_type_arg_name(t.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := t.generic_call_type_arg_name(t.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len > 0 {
				if node.value.starts_with('[]') {
					return '[]${node.value}'
				}
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
			child := t.generic_call_type_arg_name(t.a.child(&node, 0))
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

fn (t &Transformer) generic_call_type_args_name(index_node flat.Node) string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return ''
	}
	if t.index_callee_is_value_index(index_node) {
		return ''
	}
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := t.generic_call_type_arg_name(t.a.child(&index_node, i))
		if arg.len == 0 {
			return ''
		}
		args << arg
	}
	return args.join(', ')
}

fn (t &Transformer) index_callee_is_value_index(index_node flat.Node) bool {
	if index_node.kind != .index || index_node.children_count == 0 || index_node.value == 'range' {
		return false
	}
	base_id := t.a.child(&index_node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := t.a.nodes[int(base_id)]
	if base.kind == .ident && t.type_name_is_indexable(t.var_type(base.value)) {
		return true
	}
	if base.kind == .ident || base.kind == .selector {
		base_type := t.resolve_expr_type(base_id)
		if t.type_name_is_indexable(base_type) {
			return true
		}
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(base_id) {
			if transform_type_is_indexable(typ) {
				return true
			}
		}
		if transform_type_is_indexable(t.tc.resolve_type(base_id)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) type_name_is_indexable(name string) bool {
	mut clean := t.normalize_type_alias(name)
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		clean = t.normalize_type_alias(clean[1..])
	}
	return clean == 'string' || clean.starts_with('[]') || clean.starts_with('map[')
		|| t.is_fixed_array_type(clean)
}

fn transform_type_is_indexable(typ types.Type) bool {
	match typ {
		types.Array, types.ArrayFixed, types.Map, types.String {
			return true
		}
		types.Alias {
			return transform_type_is_indexable(typ.base_type)
		}
		types.Pointer {
			return transform_type_is_indexable(typ.base_type)
		}
		else {
			return false
		}
	}
}

// transform_call_args transforms all children of a call expression.
// child[0] is the function expression, children[1..n] are arguments.
fn (mut t Transformer) transform_call_args(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.a.add_node(flat.Node{
			kind:  .call
			op:    node.op
			pos:   node.pos
			value: node.value
			typ:   node.typ
		})
	}
	call_name := t.call_name_for_node(id, node)
	mut params := t.call_param_types_for_node(call_name, node)
	if concrete_params := t.concrete_generic_call_param_types(id, node) {
		params = concrete_params.clone()
	}
	param_offset := t.call_param_offset(call_name, node, params)
	is_variadic := t.call_is_variadic(call_name)
	variadic_idx := if is_variadic && params.len > 0 && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	saved_in_call_callee := t.in_call_callee
	t.in_call_callee = true
	callee_id := t.a.children[node.children_start]
	new_children << (t.const_fn_call_target(callee_id) or { t.transform_expr(callee_id) })
	t.in_call_callee = saved_in_call_callee
	mut i := 1
	for i < node.children_count {
		arg_idx := new_children.len - 1
		param_idx := arg_idx + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := if param_idx < params.len { params[param_idx].name() } else { '' }
		if arg_node.kind == .field_init {
			// Trailing `key: value` args against the variadic `...Struct` slot
			// (surfacing as `[]Struct`) desugar to one element of the elem
			// struct type; a non-variadic `[]Struct` param must not collapse.
			struct_param_type := if variadic_idx >= 0 && param_idx == variadic_idx
				&& param_type.starts_with('[]') {
				param_type[2..]
			} else {
				param_type
			}
			if packed_arg := t.transform_params_struct_call_arg(node, i, struct_param_type) {
				new_children << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
			if packed_arg := t.transform_struct_call_arg(node, i, struct_param_type) {
				new_children << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
		}
		if variadic_idx >= 0 && param_idx == variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					new_children << t.transform_call_arg_for_param(spread_id, param_type)
					i++
					break
				}
				remaining := int(node.children_count) - i
				if remaining == 1 {
					arg_type := t.node_type(arg_id)
					if arg_type.starts_with('[]') {
						new_children << t.transform_call_arg_for_param(arg_id, param_type)
					} else {
						new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
					}
				} else {
					new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
				}
				break
			}
		}
		new_children << t.transform_call_arg_for_param(arg_id, param_type)
		i++
	}
	explicit_args := int(node.children_count) - 1
	if variadic_idx >= 0 && explicit_args == variadic_idx - param_offset {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			new_children << t.pack_variadic_args(node, int(node.children_count),
				variadic_type.elem_type)
		}
	}
	t.append_missing_params_struct_args(mut new_children, params, param_offset)
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	mut typ := node.typ
	concrete_ret := t.concrete_generic_call_return_type(id, node)
	if concrete_ret.len > 0 {
		typ = concrete_ret
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          if int(id) >= 0 && int(id) < t.a.nodes.len {
			t.a.nodes[int(id)].value
		} else {
			node.value
		}
		typ:            typ
	})
	t.copy_cloned_resolution(id, new_id)
	return new_id
}

fn (mut t Transformer) const_fn_call_target(callee_id flat.NodeId) ?flat.NodeId {
	if int(callee_id) < 0 || isnil(t.tc) {
		return none
	}
	raw_type := t.raw_const_type_name_for_expr(callee_id) or { return none }
	if !raw_type.starts_with('fn ') {
		return none
	}
	expr_id := t.const_expr_for_arg(callee_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	if expr.kind !in [.ident, .selector] {
		return none
	}
	return t.transform_expr(expr_id)
}

// try_lower_join_path_call supports try lower join path call handling for Transformer.
fn (mut t Transformer) try_lower_join_path_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	call_name := t.call_name_for_node(id, node)
	if call_name != 'join_path' && call_name != 'os.join_path' {
		return none
	}
	// A spread argument (`...rest`) has a runtime-determined length and cannot be
	// unrolled into nested join_path_single calls at compile time. Defer to the real
	// variadic os.join_path in that case.
	for i in 1 .. node.children_count {
		arg := t.a.child_node(&node, i)
		if arg.kind == .prefix && arg.value == '...' {
			return none
		}
	}
	if node.children_count <= 1 {
		return t.make_string_literal('')
	}
	t.mark_fn_used('os.join_path_single')
	mut result := t.transform_expr(t.a.child(&node, 1))
	for i in 2 .. node.children_count {
		arg := t.transform_expr(t.a.child(&node, i))
		result = t.make_call_typed('os.join_path_single', arr2(result, arg), 'string')
	}
	return result
}

// transform_params_struct_call_arg transforms transform params struct call arg data for transform.
fn (mut t Transformer) transform_params_struct_call_arg(node flat.Node, field_start int, param_type string) ?flat.NodeId {
	struct_type := t.params_struct_type_name(param_type) or { return none }
	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type)
}

fn (mut t Transformer) transform_struct_call_arg(node flat.Node, field_start int, param_type string) ?flat.NodeId {
	struct_type := t.struct_arg_type_name(param_type) or { return none }
	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type)
}

fn (mut t Transformer) transform_trailing_field_init_struct_arg(node flat.Node, field_start int, struct_type string) ?flat.NodeId {
	mut field_ids := []flat.NodeId{}
	for i in field_start .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init {
			break
		}
		field_ids << field_id
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          struct_type
		typ:            struct_type
	})
	return t.transform_struct_fields(struct_id, t.a.nodes[int(struct_id)])
}

fn (t &Transformer) struct_arg_type_name(param_type string) ?string {
	if param_type.len == 0 {
		return none
	}
	mut typ := param_type
	if typ.starts_with('&') {
		return none
	}
	typ = t.normalize_type_alias(typ)
	if _ := t.lookup_struct_info(typ) {
		return typ
	}
	return none
}

// next_non_field_init_arg returns next non field init arg data for Transformer.
fn (t &Transformer) next_non_field_init_arg(node flat.Node, field_start int) int {
	mut i := field_start
	for i < node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		i++
	}
	return i
}

// params_struct_type_name supports params struct type name handling for Transformer.
fn (t &Transformer) params_struct_type_name(param_type string) ?string {
	if param_type.len == 0 {
		return none
	}
	mut typ := param_type
	if typ.starts_with('&') {
		typ = typ[1..]
	}
	if info := t.lookup_struct_info(typ) {
		if info.is_params {
			return typ
		}
	}
	normalized := t.normalize_type_alias(typ)
	if normalized != typ {
		if info := t.lookup_struct_info(normalized) {
			if info.is_params {
				return normalized
			}
		}
	}
	return none
}

// call_name_for_node updates call name for node state for Transformer.
fn (t &Transformer) call_name_for_node(id flat.NodeId, node flat.Node) string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			return name
		}
	}
	return t.resolve_call_name(node)
}

// call_param_offset updates call param offset state for Transformer.
fn (t &Transformer) call_param_offset(call_name string, node flat.Node, params []types.Type) int {
	if params.len == 0 || node.children_count == 0 {
		return 0
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector {
		return 0
	}
	if fn_node.children_count == 0 {
		if t.selector_call_name_has_receiver_param(call_name, fn_node.value, params) {
			return 1
		}
		return 0
	}
	base_id := t.a.child(fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .ident && (base_node.value == 'C' || t.is_import_alias_ident(base_id)) {
		return 0
	}
	// `module.Type.fn(...)` / `Type.fn(...)` is a static associated function call, not a
	// method: the base names a type, not a value, so no receiver must be prepended.
	if _ := t.static_assoc_fn_name(base_id, fn_node.value) {
		return 0
	}
	method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
	if method_name.len == 0 {
		if t.receiver_method_param_offset(base_id, node, params, '') == 1 {
			return 1
		}
		return 0
	}
	if t.receiver_method_param_offset(base_id, node, params, method_name) == 1 {
		return 1
	}
	if call_name.len == 0 || call_name == method_name || call_name == c_name(method_name) {
		return 1
	}
	return 0
}

fn (t &Transformer) selector_call_name_has_receiver_param(call_name string, method string, params []types.Type) bool {
	if params.len == 0 || method.len == 0 || call_name.len == 0
		|| !call_name.ends_with('.${method}') {
		return false
	}
	first_type := t.normalize_type_alias(params[0].name())
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	if receiver_param_matches_method_name(clean_first, call_name) {
		return true
	}
	receiver := call_name.all_before_last('.')
	clean_receiver := if receiver.starts_with('&') { receiver[1..] } else { receiver }
	if receiver_param_types_match(clean_first, clean_receiver)
		|| t.normalize_type_alias(clean_first) == t.normalize_type_alias(clean_receiver)
		|| clean_first.all_after_last('.') == clean_receiver.all_after_last('.') {
		return true
	}
	return false
}

// static_assoc_fn_name returns the name of the static associated function a selector
// call resolves to, if the base names a type (`Type.fn` or `module.Type.fn`) and that
// function exists. Such calls take no receiver, so the base must not be prepended as an
// argument. Returns none for ordinary method calls (base is a value).
fn (t &Transformer) static_assoc_fn_name(base_id flat.NodeId, method string) ?string {
	if method.len == 0 {
		return none
	}
	base := t.a.nodes[int(base_id)]
	if base.kind == .ident {
		if base.value == 'C' || t.is_import_alias_ident(base_id) {
			return none
		}
		if t.var_type(base.value).len > 0 {
			return none
		}
		base_type := t.node_type(base_id)
		clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
		if clean_base_type.len > 0 && clean_base_type != 'unknown' && clean_base_type != 'void'
			&& base.value != clean_base_type && base.value != clean_base_type.all_after_last('.') {
			return none
		}
		for type_name in t.static_assoc_type_candidates(base.value) {
			name := '${type_name}.${method}'
			if t.is_known_fn_name(name) {
				return name
			}
		}
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind == .ident {
			type_ident := '${inner.value}.${base.value}'
			for type_name in t.static_assoc_type_candidates(type_ident) {
				name := '${type_name}.${method}'
				if t.is_known_fn_name(name) {
					return name
				}
			}
		}
	}
	return none
}

fn (t &Transformer) is_import_alias_ident(id flat.NodeId) bool {
	if int(id) < 0 || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value !in t.tc.imports {
		return false
	}
	if t.var_type(node.value).len > 0 {
		return false
	}
	if typ := t.tc.expr_type(id) {
		name := typ.name()
		if name.len > 0 && name != 'unknown' && name != 'void' {
			return false
		}
	}
	return true
}

fn (t &Transformer) static_assoc_type_candidates(type_ident string) []string {
	if type_ident.len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	candidates << type_ident
	if !type_ident.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${type_ident}'
	}
	if !isnil(t.tc) {
		qname := t.tc.qualify_name(type_ident)
		if qname !in candidates {
			candidates << qname
		}
	}
	mut result := []string{}
	for candidate in candidates {
		if t.is_static_assoc_type_name(candidate) && candidate !in result {
			result << candidate
		}
	}
	return result
}

fn (t &Transformer) is_static_assoc_type_name(type_name string) bool {
	if type_name in t.structs || type_name in t.enum_types || type_name in t.sum_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return type_name in t.tc.structs || type_name in t.tc.enum_names || type_name in t.tc.sum_types
		|| type_name in t.tc.interface_names || type_name in t.tc.type_aliases
}

// try_lower_static_assoc_call lowers `Type.method(args)` to a direct call to
// `Type.method(args)` once the checker/transformer has proven that the base is a
// type, not a value receiver.
fn (mut t Transformer) try_lower_static_assoc_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	static_fn := t.static_assoc_fn_name(base_id, fn_node.value) or { return none }
	normalized := t.transform_call_args(id, node)
	normalized_node := t.a.nodes[int(normalized)]
	mut args := []flat.NodeId{cap: int(normalized_node.children_count) - 1}
	for i in 1 .. normalized_node.children_count {
		args << t.a.child(&normalized_node, i)
	}
	ret_type := t.receiver_method_return_type(static_fn, node.typ)
	return t.make_call_typed(static_fn, args, ret_type)
}

// call_param_types updates call param types state for Transformer.
fn (mut t Transformer) call_param_types(call_name string) []types.Type {
	if call_name.len == 0 || isnil(t.tc) {
		return []types.Type{}
	}
	if params := t.call_param_types_from_decl(call_name) {
		return params
	}
	params := t.tc.fn_param_types[call_name] or { return []types.Type{} }
	return params
}

fn (mut t Transformer) call_param_types_for_node(call_name string, node flat.Node) []types.Type {
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
			if method_name.len > 0 {
				if params := t.call_param_types_from_decl(method_name) {
					return params
				}
			}
			mut base_type := t.lvalue_type(base_id)
			if base_type.starts_with('&') {
				base_type = base_type[1..]
			}
			for candidate in ['${base_type}.${fn_node.value}',
				'${t.normalize_type_in_module(base_type, t.cur_module)}.${fn_node.value}'] {
				if params := t.call_param_types_from_decl(candidate) {
					return params
				}
			}
		}
	}
	return t.call_param_types(call_name)
}

fn (mut t Transformer) call_param_types_from_decl(call_name string) ?[]types.Type {
	if call_name.len == 0 || isnil(t.tc) {
		return none
	}
	if params := t.call_param_types_decl_cache[call_name] {
		return params
	}
	if t.call_param_types_decl_misses[call_name] {
		return none
	}
	t.ensure_call_param_types_decl_index()
	decl := t.call_param_types_decl_index[call_name] or {
		t.call_param_types_decl_misses[call_name] = true
		return none
	}
	node := t.a.nodes[decl.idx]
	mut params := []types.Type{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			continue
		}
		mut param_type := t.parse_decl_param_type(child.typ, decl.module, decl.file)
		if param_type is types.Unknown || (param_type is types.Void && child.typ != 'void') {
			t.call_param_types_decl_misses[call_name] = true
			return none
		}
		if (child.is_mut || child.typ.starts_with('mut ')
			|| child.typ.starts_with('&')) && param_type !is types.Pointer {
			param_type = types.Type(types.Pointer{
				base_type: param_type
			})
		}
		params << param_type
	}
	t.call_param_types_decl_cache[call_name] = params.clone()
	return params
}

fn (mut t Transformer) ensure_call_param_types_decl_index() {
	if t.call_param_types_index_ready {
		return
	}
	t.call_param_types_decl_index.clear()
	mut file_name := ''
	mut module_name := ''
	for i, node in t.a.nodes {
		if node.kind == .file {
			file_name = node.value
			module_name = t.tc.file_modules[file_name] or { '' }
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		t.add_call_param_types_decl_key(node.value, i, file_name, module_name)
		qname := transform_qualified_fn_name(module_name, node.value)
		if qname != node.value {
			t.add_call_param_types_decl_key(qname, i, file_name, module_name)
		}
	}
	t.call_param_types_index_ready = true
}

fn (mut t Transformer) prepare_parallel_call_param_types() {
	t.ensure_call_param_types_decl_index()
	names := t.call_param_types_decl_index.keys()
	for name in names {
		_ = t.call_param_types_from_decl(name) or { continue }
	}
}

fn (mut t Transformer) add_call_param_types_decl_key(key string, idx int, file string, module_name string) {
	if key.len == 0 {
		return
	}
	if key !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[key] = FnParamDeclRef{
			idx:    idx
			file:   file
			module: module_name
		}
	}
	t.call_param_types_decl_misses.delete(key)
	cname := c_name(key)
	if cname != key && cname !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[cname] = FnParamDeclRef{
			idx:    idx
			file:   file
			module: module_name
		}
	}
	t.call_param_types_decl_misses.delete(cname)
}

fn (mut t Transformer) parse_decl_param_type(typ string, module_name string, file_name string) types.Type {
	scoped := t.decl_param_type_in_module(typ, module_name)
	old_file := t.tc.cur_file
	old_module := t.tc.cur_module
	t.tc.cur_file = file_name
	t.tc.cur_module = module_name
	parsed := t.tc.parse_type(scoped)
	t.tc.cur_file = old_file
	t.tc.cur_module = old_module
	return parsed
}

fn (t &Transformer) decl_param_type_in_module(typ string, module_name string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + t.decl_param_type_in_module(clean[4..], module_name)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.decl_param_type_in_module(clean[7..], module_name)
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.decl_param_type_in_module(clean[7..], module_name)
	}
	if clean.starts_with('?') {
		return '?' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('!') {
		return '!' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + t.decl_param_type_in_module(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + t.decl_param_type_in_module(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.decl_param_type_in_module(clean[4..bracket_end], module_name)
			value := t.decl_param_type_in_module(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + t.decl_param_type_in_module(clean[bracket_end +
				1..], module_name)
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if scoped_fn_type := t.decl_fn_type_in_module(clean, module_name) {
			return scoped_fn_type
		}
		return clean
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		mut scoped_args := []string{}
		for arg in args {
			scoped_args << t.decl_param_type_in_module(arg, module_name)
		}
		scoped_base := t.decl_param_type_in_module(base, module_name)
		return scoped_base + '[' + scoped_args.join(', ') + ']'
	}
	if clean.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' || types.is_builtin_type_name(clean)
		|| is_generic_placeholder_type_name(clean) {
		return clean
	}
	qualified := '${module_name}.${clean}'
	if t.type_authority_has(qualified) {
		return qualified
	}
	if !isnil(t.tc) && (qualified in t.tc.type_aliases || qualified in t.tc.structs
		|| qualified in t.tc.enum_names || qualified in t.tc.sum_types
		|| qualified in t.tc.interface_names) {
		return qualified
	}
	return clean
}

fn (t &Transformer) decl_fn_type_in_module(typ string, module_name string) ?string {
	params, ret := fn_type_text_parts(typ) or { return none }
	mut scoped_params := []string{cap: params.len}
	for param in params {
		scoped_params << t.decl_fn_type_param_in_module(param, module_name)
	}
	if ret.len > 0 {
		return 'fn(${scoped_params.join(', ')}) ${t.decl_param_type_in_module(ret, module_name)}'
	}
	return 'fn(${scoped_params.join(', ')})'
}

fn (t &Transformer) decl_fn_type_param_in_module(param string, module_name string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
		marker_idx := text.index(marker) or { continue }
		if marker_idx <= 0 {
			continue
		}
		head := text[..marker_idx].trim_space()
		tail := text[marker_idx..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
		break
	}
	scoped := t.decl_param_type_in_module(text, module_name)
	if is_mut && scoped.len > 0 && !scoped.starts_with('&') {
		return '&' + scoped
	}
	return scoped
}

// call_is_variadic updates call is variadic state for Transformer.
fn (t &Transformer) call_is_variadic(call_name string) bool {
	if call_name.len == 0 || isnil(t.tc) {
		return false
	}
	if t.tc.c_variadic_fns[call_name] {
		return false
	}
	if is_variadic := t.tc.fn_variadic[call_name] {
		return is_variadic
	}
	// Import-aliased call names (`http.new_header` for module `net.http`)
	// register under the full module path; resolve the alias exactly first.
	if call_name.contains('.') {
		resolved_call := t.tc.resolve_imported_type_text_in_file(call_name, t.cur_file)
		if resolved_call != call_name {
			if t.tc.c_variadic_fns[resolved_call] {
				return false
			}
			if is_variadic := t.tc.fn_variadic[resolved_call] {
				return is_variadic
			}
		}
		suffix := '.' + call_name
		mut suffix_match := false
		mut suffix_variadic := false
		for key, is_variadic in t.tc.fn_variadic {
			if key.ends_with(suffix) {
				if suffix_match {
					return false
				}
				suffix_match = true
				suffix_variadic = is_variadic
			}
		}
		if suffix_match {
			return suffix_variadic
		}
	}
	return false
}

// call_param_type_name updates call param type name state for Transformer.
fn (t &Transformer) call_param_type_name(call_name string, idx int) string {
	if idx < 0 || call_name.len == 0 || isnil(t.tc) {
		return ''
	}
	params := t.tc.fn_param_types[call_name] or { return '' }
	if idx >= params.len {
		return ''
	}
	return params[idx].name()
}

// transform_call_arg_for_param transforms transform call arg for param data for transform.
fn (mut t Transformer) transform_call_arg_for_param(arg_id flat.NodeId, param_type string) flat.NodeId {
	if int(arg_id) < 0 {
		return arg_id
	}
	mut arg_node := &t.a.nodes[int(arg_id)]
	if param_type.starts_with('&') && arg_node.kind == .or_expr && arg_node.value == '?'
		&& arg_node.children_count > 0 {
		source_id := t.a.child(arg_node, 0)
		source_type := t.node_type(source_id)
		if t.is_optional_type_name(source_type) {
			payload_type := t.optional_base_type(source_type)
			// Evaluate the optional once, keeping it addressable so mutations
			// through the reference still write back to the payload (a stable
			// ident/selector is reused directly, a call is materialized once).
			opt_expr := t.stable_transformed_expr_for_reuse(t.transform_expr(source_id),
				source_type, 'opt_ref')
			if t.is_optional_type_name(t.cur_fn_ret_type) {
				// `opt?` propagates: return none/err when it is none, instead of
				// unconditionally treating it as present.
				not_ok := t.make_prefix(.not, t.make_selector(opt_expr, 'ok', 'bool'))
				err_expr := t.make_selector(opt_expr, 'err', 'IError')
				else_stmts := t.lower_or_body_to_stmts_with_err_expr(flat.empty_node, '',
					payload_type, '?', err_expr)
				t.pending_stmts << t.make_if(not_ok, t.make_block(else_stmts), t.make_empty())
			} else {
				// Comptime option-payload-mut (e.g. `decode(mut result.field?)` in a
				// `$for` decoder): the callee fills the payload, so mark it present and
				// expose its address. Normal `?` propagation in a non-option-returning
				// function would have been rejected by the checker, so this is the only
				// way we get here without an option return type.
				ok_target := t.make_selector(opt_expr, 'ok', 'bool')
				t.pending_stmts << t.make_assign(ok_target, t.make_bool_literal(true))
			}
			payload := t.make_selector(opt_expr, 'value', payload_type)
			addr := t.make_prefix(.amp, payload)
			t.set_node_typ(int(addr), param_type)
			return addr
		}
	}
	if arg_node.kind == .lambda_expr {
		if lifted := t.lift_lambda_expr_for_fn_param(arg_id, *arg_node, param_type) {
			return lifted
		}
	}
	if arg_node.kind == .array_literal && arg_node.typ.len == 0 && param_type.starts_with('[]') {
		arg_node.typ = param_type
	}
	if arg_node.kind == .enum_val && param_type in t.enum_types {
		return t.transform_enum_shorthand(arg_id, *arg_node, param_type)
	}
	if param_type.starts_with('&') && arg_node.kind == .selector
		&& t.selector_chain_has_sum_variant_field(arg_id) {
		value := t.transform_expr(arg_id)
		mut value_type := t.node_type(arg_id)
		if value_type.len == 0 {
			value_type = t.node_type(value)
		}
		stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
		addr := t.make_prefix(.amp, stable)
		t.set_node_typ(int(addr), param_type)
		return addr
	}
	if param_type.starts_with('&') && t.is_sum_type_name(param_type[1..]) {
		target_sum := param_type[1..]
		resolved_target_sum := t.resolve_sum_name(target_sum)
		arg_type := t.node_type(arg_id)
		arg_key := t.expr_key(arg_id)
		has_smartcast := t.has_smartcast(arg_key)
		if has_smartcast {
			raw_arg_type := t.raw_expr_type_without_smartcast(arg_id)
			if t.resolve_sum_name(t.trim_pointer_type(raw_arg_type)) == resolved_target_sum {
				return t.make_plain_expr_for_smartcast(arg_id)
			}
		}
		if !has_smartcast
			&& t.resolve_sum_name(t.trim_pointer_type(arg_type)) == resolved_target_sum {
			return t.transform_expr(arg_id)
		}
		if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
			inner_id := t.a.child(arg_node, 0)
			inner_key := t.expr_key(inner_id)
			inner_has_smartcast := t.has_smartcast(inner_key)
			if inner_has_smartcast {
				raw_inner_type := t.raw_expr_type_without_smartcast(inner_id)
				if t.resolve_sum_name(t.trim_pointer_type(raw_inner_type)) == resolved_target_sum {
					inner := t.make_plain_expr_for_smartcast(inner_id)
					addr := t.make_prefix(.amp, inner)
					t.set_node_typ(int(addr), param_type)
					return addr
				}
			}
			inner_type := t.node_type(inner_id)
			if t.resolve_sum_name(t.trim_pointer_type(inner_type)) == resolved_target_sum {
				return t.transform_expr(arg_id)
			}
		}
		wrapped := t.wrap_sum_value(arg_id, target_sum)
		tmp_name := t.new_temp('sum_arg')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, target_sum)
		return t.make_prefix(.amp, t.make_ident(tmp_name))
	}
	if param_type.starts_with('&') && arg_node.kind == .prefix && arg_node.op == .amp
		&& arg_node.children_count > 0 {
		child_id := t.a.child(arg_node, 0)
		child_type := t.node_type(child_id)
		if child_type.len > 0
			&& t.normalize_type_alias(child_type) == t.normalize_type_alias(param_type[1..]) {
			return t.transform_expr(arg_id)
		}
	}
	if param_type.starts_with('&') && !t.is_sum_type_name(param_type[1..])
		&& !t.is_interface_type(param_type) {
		arg_type := t.node_type(arg_id)
		if arg_type.len > 0 && !arg_type.starts_with('&')
			&& t.normalize_type_alias(arg_type) == t.normalize_type_alias(param_type[1..])
			&& t.expr_can_take_address(arg_id) {
			value := t.transform_expr(arg_id)
			addr := t.make_prefix(.amp, value)
			t.set_node_typ(int(addr), param_type)
			return addr
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .ident
		&& t.pointer_global_arg_matches_param(arg_node.value, param_type) {
		return t.transform_expr(arg_id)
	}
	if !t.in_spawn_expr {
		if ptr_arg := t.transform_pointer_rvalue_arg(arg_id, *arg_node, param_type) {
			return ptr_arg
		}
	}
	if t.is_sum_type_name(param_type) {
		return t.wrap_sum_value(arg_id, param_type)
	}
	if param_type.starts_with('[]') {
		arg_type := t.node_type(arg_id)
		if t.is_fixed_array_type(arg_type) {
			return t.fixed_array_value_to_array(arg_id, arg_type, param_type)
		}
		if const_arg := t.transform_const_array_arg_for_param(arg_id, param_type) {
			return const_arg
		}
	}
	if param_type.len > 0 && (param_type.contains('unknown')
		|| t.type_text_has_generic_placeholder(param_type, t.cur_module)) {
		return t.transform_expr(arg_id)
	}
	if param_type.starts_with('&') && t.is_interface_type(param_type) {
		// A `mut`/reference interface parameter must alias the caller's concrete
		// value so mutations through the interface are visible to the caller.
		// The caller's frame outlives the call, so boxing a reference to the
		// source (instead of a heap copy) is safe here.
		if boxed := t.transform_interface_value_for_type(arg_id, param_type, true) {
			return boxed
		}
	}
	return t.transform_expr_for_type(arg_id, param_type)
}

fn (t &Transformer) pointer_global_arg_matches_param(name string, param_type string) bool {
	arg_type := t.global_ident_type(name) or { return false }
	if !arg_type.starts_with('&') {
		return false
	}
	param_base := t.normalize_type_alias(param_type[1..])
	arg_base := t.normalize_type_alias(arg_type[1..])
	return param_base == arg_base || param_base.all_after_last('.') == arg_base.all_after_last('.')
}

fn (t &Transformer) global_ident_type(name string) ?string {
	if typ := t.globals[name] {
		return t.normalize_type_alias(typ)
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if typ := t.globals[qname] {
			return t.normalize_type_alias(typ)
		}
	}
	return none
}

fn (mut t Transformer) lift_lambda_expr_for_fn_param(_id flat.NodeId, node flat.Node, param_type string) ?flat.NodeId {
	if node.kind != .lambda_expr || node.children_count == 0 || param_type.len == 0 || isnil(t.tc) {
		return none
	}
	fn_type := t.fn_type_from_type_text(param_type) or { return none }
	body_id := t.a.child(&node, node.children_count - 1)
	lambda_param_count := int(node.children_count) - 1
	if lambda_param_count != fn_type.params.len {
		return none
	}
	mut lambda_params := map[string]bool{}
	for i in 0 .. lambda_param_count {
		param_id := t.a.child(&node, i)
		param_node := t.a.nodes[int(param_id)]
		if param_node.value.len > 0 {
			lambda_params[param_node.value] = true
		}
	}
	capture_ids := t.lambda_capture_ids(body_id, lambda_params)
	mut children := []flat.NodeId{cap: capture_ids.len + fn_type.params.len + 1}
	for capture_id in capture_ids {
		children << capture_id
	}
	for i in 0 .. lambda_param_count {
		param_id := t.a.child(&node, i)
		param_node := t.a.nodes[int(param_id)]
		param_type_name := fn_type.params[i].name()
		children << t.a.add_node(flat.Node{
			kind:  .param
			value: param_node.value
			typ:   param_type_name
			op:    if param_type_name.starts_with('&') { .amp } else { .none }
		})
	}
	ret_type := fn_type.return_type.name()
	if ret_type.len > 0 && ret_type != 'void' {
		children << t.make_return(body_id, ret_type)
	} else {
		children << t.make_expr_stmt(body_id)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	fn_id := t.a.add_node(flat.Node{
		kind:           .fn_literal
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
	})
	return t.lift_fn_literal(fn_id, t.a.nodes[int(fn_id)])
}

fn (mut t Transformer) lambda_capture_ids(body_id flat.NodeId, params map[string]bool) []flat.NodeId {
	mut names := map[string]flat.NodeId{}
	t.collect_lambda_capture_names(body_id, params, mut names)
	mut sorted_names := names.keys()
	sorted_names.sort()
	mut ids := []flat.NodeId{cap: sorted_names.len}
	for name in sorted_names {
		ids << names[name]
	}
	return ids
}

fn (mut t Transformer) collect_lambda_capture_names(id flat.NodeId, locals map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 && node.value !in locals && t.var_type(node.value).len > 0 {
				names[node.value] = id
			}
			return
		}
		.block {
			t.collect_lambda_capture_sequence(node, 0, locals, mut names)
			return
		}
		.decl_assign {
			mut local_scope := locals.clone()
			t.collect_lambda_decl_assign_captures(node, mut local_scope, mut names)
			return
		}
		.for_in_stmt {
			t.collect_lambda_for_in_captures(node, locals, mut names)
			return
		}
		.for_stmt {
			t.collect_lambda_for_captures(node, locals, mut names)
			return
		}
		.if_expr {
			t.collect_lambda_if_captures(node, locals, mut names)
			return
		}
		.match_stmt {
			t.collect_lambda_match_captures(node, locals, mut names)
			return
		}
		.fn_literal, .lambda_expr {
			return
		}
		.call {
			if node.children_count > 0 {
				callee_id := t.a.child(&node, 0)
				t.collect_lambda_capture_names(callee_id, locals, mut names)
			}
			for i in 1 .. node.children_count {
				t.collect_lambda_capture_names(t.a.child(&node, i), locals, mut names)
			}
			return
		}
		.selector {
			if node.children_count > 0 {
				t.collect_lambda_capture_names(t.a.child(&node, 0), locals, mut names)
			}
			return
		}
		else {}
	}

	for i in 0 .. node.children_count {
		t.collect_lambda_capture_names(t.a.child(&node, i), locals, mut names)
	}
}

fn (mut t Transformer) collect_lambda_capture_sequence(node flat.Node, start int, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut local_scope := locals.clone()
	for i in start .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut local_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_capture_stmt(id flat.NodeId, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.decl_assign {
			t.collect_lambda_decl_assign_captures(node, mut locals, mut names)
		}
		.for_in_stmt {
			t.collect_lambda_for_in_captures(node, locals, mut names)
		}
		.for_stmt {
			t.collect_lambda_for_captures(node, locals, mut names)
		}
		else {
			t.collect_lambda_capture_names(id, locals, mut names)
		}
	}
}

fn (mut t Transformer) collect_lambda_decl_assign_captures(node flat.Node, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	if node.children_count >= 3 && !isnil(t.tc) {
		rhs_id := t.a.child(&node, 1)
		if _ := t.multi_return_types_for_expr(rhs_id, node.children_count - 1) {
			t.collect_lambda_capture_names(rhs_id, locals, mut names)
			t.note_lambda_local_binding(t.a.child(&node, 0), mut locals)
			for i in 2 .. node.children_count {
				t.note_lambda_local_binding(t.a.child(&node, i), mut locals)
			}
			return
		}
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		t.collect_lambda_capture_names(rhs_id, locals, mut names)
		t.note_lambda_local_binding(lhs_id, mut locals)
		i += 2
	}
}

fn (mut t Transformer) collect_lambda_for_in_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count < 3 {
		return
	}
	header := node.value.int()
	container_id := t.a.child(&node, 2)
	t.collect_lambda_capture_names(container_id, locals, mut names)
	if header > 3 && node.children_count > 3 {
		t.collect_lambda_capture_names(t.a.child(&node, 3), locals, mut names)
	}
	mut loop_scope := locals.clone()
	t.note_lambda_local_binding(t.a.child(&node, 0), mut loop_scope)
	t.note_lambda_local_binding(t.a.child(&node, 1), mut loop_scope)
	for i in header .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_for_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut loop_scope := locals.clone()
	if node.children_count > 0 {
		init_id := t.a.child(&node, 0)
		if t.valid_node_id(init_id) {
			init := t.a.nodes[int(init_id)]
			if init.kind == .decl_assign {
				t.collect_lambda_decl_assign_captures(init, mut loop_scope, mut names)
			} else {
				t.collect_lambda_capture_names(init_id, loop_scope, mut names)
			}
		}
	}
	if node.children_count > 1 {
		t.collect_lambda_capture_names(t.a.child(&node, 1), loop_scope, mut names)
	}
	if node.children_count > 2 {
		t.collect_lambda_capture_names(t.a.child(&node, 2), loop_scope, mut names)
	}
	for i in 3 .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_if_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	cond_id := t.a.child(&node, 0)
	mut then_scope := locals.clone()
	t.collect_lambda_condition_captures(cond_id, locals, mut then_scope, mut names)
	if node.children_count > 1 {
		t.collect_lambda_capture_names(t.a.child(&node, 1), then_scope, mut names)
	}
	if node.children_count > 2 {
		t.collect_lambda_capture_names(t.a.child(&node, 2), locals, mut names)
	}
}

fn (mut t Transformer) collect_lambda_condition_captures(id flat.NodeId, locals map[string]bool, mut then_scope map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .decl_assign {
		mut cond_scope := locals.clone()
		t.collect_lambda_decl_assign_captures(node, mut cond_scope, mut names)
		for name, _ in cond_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		return
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		mut lhs_scope := locals.clone()
		t.collect_lambda_condition_captures(t.a.child(&node, 0), locals, mut lhs_scope, mut names)
		for name, _ in lhs_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		t.collect_lambda_condition_captures(t.a.child(&node, 1), lhs_scope, mut then_scope, mut
			names)
		return
	}
	t.collect_lambda_capture_names(id, locals, mut names)
}

fn (mut t Transformer) collect_lambda_match_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	t.collect_lambda_capture_names(t.a.child(&node, 0), locals, mut names)
	for i in 1 .. node.children_count {
		branch_id := t.a.child(&node, i)
		if !t.valid_node_id(branch_id) {
			continue
		}
		branch := t.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			t.collect_lambda_capture_names(branch_id, locals, mut names)
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
		for j in 0 .. body_start {
			t.collect_lambda_capture_names(t.a.child(&branch, j), locals, mut names)
		}
		mut branch_scope := locals.clone()
		for j in body_start .. branch.children_count {
			t.collect_lambda_capture_stmt(t.a.child(&branch, j), mut branch_scope, mut names)
		}
	}
}

fn (mut t Transformer) note_lambda_local_binding(id flat.NodeId, mut locals map[string]bool) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 && node.value != '_' {
		locals[node.value] = true
	}
}

fn (t &Transformer) valid_node_id(id flat.NodeId) bool {
	return int(id) >= 0 && int(id) < t.a.nodes.len
}

fn (t &Transformer) fn_type_from_type_text(type_text string) ?types.FnType {
	typ := t.tc.parse_type(type_text)
	return fn_type_from_type(typ)
}

fn fn_type_from_type(typ types.Type) ?types.FnType {
	if typ is types.FnType {
		return typ
	}
	if typ is types.Alias {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn (mut t Transformer) transform_pointer_rvalue_arg(arg_id flat.NodeId, arg_node flat.Node, param_type string) ?flat.NodeId {
	if !param_type.starts_with('&') {
		return none
	}
	mut value_id := arg_id
	mut value_node := arg_node
	if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
		value_id = t.a.child(&arg_node, 0)
		value_node = t.a.nodes[int(value_id)]
	}
	if !is_pointer_arg_temp_rvalue(value_node) {
		return none
	}
	value_type := param_type[1..]
	if value_type.len == 0 || value_type == 'void' || value_type == 'unknown' {
		return none
	}
	if t.c_type_nil_call_for_type(value_node, value_type) {
		nil_id := t.a.add(.nil_literal)
		t.set_node_typ(int(nil_id), param_type)
		return nil_id
	}
	arg_type := t.node_type(value_id)
	if arg_type.len == 0 || arg_type == 'void' || arg_type == 'unknown'
		|| is_pointer_like_type_name(arg_type) {
		return none
	}
	value := t.transform_expr_for_type(value_id, value_type)
	tmp_name := t.new_temp('ptr_arg')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(addr), param_type)
	return addr
}

fn (t &Transformer) c_type_nil_call_for_type(node flat.Node, value_type string) bool {
	if node.kind != .call || node.children_count != 2 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	arg := t.a.child_node(&node, 1)
	if arg.kind != .nil_literal {
		return false
	}
	short_type := value_type.all_after_last('.')
	if callee.kind == .ident {
		return callee.value == value_type || callee.value == short_type
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		return base.kind == .ident && callee.value.len > 0
			&& ((base.value == 'C' && (value_type.starts_with('C.') || callee.value == short_type))
			|| '${base.value}.${callee.value}' == value_type)
	}
	return false
}

fn is_pointer_arg_temp_rvalue(node flat.Node) bool {
	return node.kind == .call || (node.kind == .index && node.value == 'range')
}

fn is_pointer_like_type_name(typ string) bool {
	mut clean := typ
	if clean.starts_with('mut ') {
		clean = clean[4..]
	}
	return clean.starts_with('&') || clean in ['voidptr', 'byteptr', 'charptr']
}

fn (mut t Transformer) append_missing_params_struct_args(mut args []flat.NodeId, params []types.Type, param_offset int) {
	mut param_idx := param_offset
	if args.len > 0 {
		param_idx = args.len - 1 + param_offset
	}
	for param_idx < params.len {
		param_type := params[param_idx].name()
		struct_type := t.params_struct_type_name(param_type) or { break }
		args << t.zero_value_for_type(struct_type)
		param_idx++
	}
}

// is_fn_pointer_type_name reports whether is fn pointer type name applies in transform.
fn (t &Transformer) is_fn_pointer_type_name(type_name string) bool {
	if type_name.len == 0 || isnil(t.tc) {
		return false
	}
	typ := t.tc.parse_type(type_name)
	if typ is types.FnType {
		return true
	}
	if typ is types.Alias {
		return typ.base_type is types.FnType
	}
	return false
}

// is_named_fn_value_arg reports whether is named fn value arg applies in transform.
fn (t &Transformer) is_named_fn_value_arg(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .ident {
		if _ := t.resolve_fn_value_ident(node.value) {
			return true
		}
		return false
	}
	if node.kind == .selector && node.children_count > 0 {
		key := t.expr_key(arg_id)
		return key in t.tc.fn_ret_types || key in t.tc.fn_param_types
	}
	return false
}

// transform_const_array_arg_for_param supports transform_const_array_arg_for_param handling.
fn (mut t Transformer) transform_const_array_arg_for_param(arg_id flat.NodeId, param_type string) ?flat.NodeId {
	if raw_const_type := t.raw_const_type_name_for_expr(arg_id) {
		if t.normalize_type_alias(raw_const_type) == t.normalize_type_alias(param_type) {
			return t.transform_expr(arg_id)
		}
	}
	expr_id := t.const_expr_for_arg(arg_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	elem_type := param_type[2..]
	if expr.kind == .array_init && expr.children_count == 0 {
		return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
	}
	if expr.kind != .array_literal {
		return none
	}
	if expr.children_count == 0 {
		return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
	}
	mut values := []flat.NodeId{cap: int(expr.children_count)}
	for i in 0 .. expr.children_count {
		values << t.transform_expr(t.a.child(&expr, i))
	}
	return t.make_array_literal_typed(values, param_type)
}

// const_expr_for_arg supports const expr for arg handling for Transformer.
fn (t &Transformer) const_expr_for_arg(arg_id flat.NodeId) ?flat.NodeId {
	if isnil(t.tc) || int(arg_id) < 0 {
		return none
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .ident {
		if t.var_type(node.value).len > 0 {
			return none
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			if expr_id := t.const_expr_for_name('${t.cur_module}.${node.value}') {
				return expr_id
			}
		}
		return t.const_expr_for_name(node.value)
	}
	if node.kind == .selector && node.children_count > 0 {
		if !t.selector_const_base_is_value(node) {
			base := t.a.child_node(&node, 0)
			return t.const_expr_for_name_in_context('${base.value}.${node.value}', t.cur_module,
				t.cur_file)
		}
	}
	return none
}

fn (t &Transformer) selector_const_base_is_value(node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || isnil(t.tc) {
		return false
	}
	base_id := t.a.child(&node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := t.a.nodes[int(base_id)]
	if base.kind != .ident {
		return false
	}
	if t.var_type(base.value).len > 0 {
		return true
	}
	if typ := t.tc.expr_type(base_id) {
		name := typ.name()
		return name.len > 0 && name !in ['void', 'unknown']
	}
	resolved := t.tc.resolve_type(base_id)
	name := resolved.name()
	return name.len > 0 && name !in ['void', 'unknown']
}

// const_expr_for_name supports const expr for name handling for Transformer.
fn (t &Transformer) const_expr_for_name(name string) ?flat.NodeId {
	return t.const_expr_for_name_in_context(name, t.cur_module, t.cur_file)
}

fn (t &Transformer) const_expr_for_name_in_context(name string, module_name string, file string) ?flat.NodeId {
	if isnil(t.tc) || name.len == 0 {
		return none
	}
	if expr_id := t.tc.const_exprs[name] {
		return expr_id
	}
	key := t.const_type_key_in_context(name, module_name, file) or { return none }
	if expr_id := t.tc.const_exprs[key] {
		return expr_id
	}
	return none
}

// pack_variadic_args supports pack variadic args handling for Transformer.
fn (mut t Transformer) pack_variadic_args(node flat.Node, first_arg int, elem_type types.Type) flat.NodeId {
	expected_enum := elem_type.name()
	array_type := '[]${expected_enum}'
	if named_arg := t.transform_variadic_struct_fields(node, first_arg, elem_type) {
		if t.in_const_init {
			return t.make_array_literal_typed([named_arg], array_type)
		}
		tmp_name := t.new_temp('varargs')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum,
			t.make_int_literal(0), t.make_int_literal(1)), array_type)
		value_name := t.new_temp('vararg')
		t.pending_stmts << t.make_decl_assign_typed(value_name, named_arg, expected_enum)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', arr2(t.make_prefix(.amp,
			t.make_ident(tmp_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void'))
		t.set_var_type(tmp_name, array_type)
		return t.make_ident(tmp_name)
	}
	if t.in_const_init {
		mut values := []flat.NodeId{cap: int(node.children_count) - first_arg}
		for i in first_arg .. node.children_count {
			arg_id := t.a.child(&node, i)
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .enum_val && expected_enum in t.enum_types {
				values << t.transform_enum_shorthand(arg_id, arg, expected_enum)
			} else if t.is_sum_type_name(expected_enum) {
				values << t.wrap_sum_value(arg_id, expected_enum)
			} else {
				values << t.transform_expr(arg_id)
			}
		}
		return t.make_array_literal_typed(values, array_type)
	}
	tmp_name := t.new_temp('varargs')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum,
		t.make_int_literal(0), t.make_int_literal(int(node.children_count) - first_arg)),
		array_type)
	for i in first_arg .. node.children_count {
		arg_id := t.a.child(&node, i)
		arg := t.a.nodes[int(arg_id)]
		value := if arg.kind == .enum_val && expected_enum in t.enum_types {
			t.transform_enum_shorthand(arg_id, arg, expected_enum)
		} else if t.is_sum_type_name(expected_enum) {
			t.wrap_sum_value(arg_id, expected_enum)
		} else {
			t.transform_expr(arg_id)
		}
		value_name := t.new_temp('vararg')
		if variadic_elem_is_voidptr(elem_type) {
			value_arg := if t.voidptr_variadic_arg_passes_direct(arg_id) {
				if t.node_type(arg_id) == 'voidptr' {
					value
				} else {
					t.make_cast('voidptr', value, 'voidptr')
				}
			} else {
				storage_type := t.voidptr_variadic_storage_type(arg_id)
				storage_value := if storage_type != t.node_type(arg_id) {
					t.make_cast(storage_type, value, storage_type)
				} else {
					value
				}
				storage_name := t.new_temp('vararg_storage')
				t.pending_stmts << t.make_decl_assign_typed(storage_name, storage_value,
					storage_type)
				t.make_cast('voidptr', t.make_prefix(.amp, t.make_ident(storage_name)), 'voidptr')
			}
			t.pending_stmts << t.make_decl_assign_typed(value_name, value_arg, expected_enum)
		} else {
			// Keep the explicit interface storage type visible to cgen. A direct interface
			// literal is a struct init, whose concrete payload name would otherwise override
			// the declaration annotation.
			storage_value := if elem_type is types.Interface { t.make_paren(value) } else { value }
			t.pending_stmts << t.make_decl_assign_typed(value_name, storage_value, expected_enum)
		}
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', arr2(t.make_prefix(.amp,
			t.make_ident(tmp_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void'))
	}
	t.set_var_type(tmp_name, array_type)
	return t.make_ident(tmp_name)
}

fn variadic_elem_is_voidptr(typ types.Type) bool {
	if typ is types.Pointer {
		return typ.base_type is types.Void
	}
	return false
}

fn (t &Transformer) voidptr_variadic_arg_passes_direct(arg_id flat.NodeId) bool {
	if !isnil(t.tc) {
		return voidptr_variadic_type_passes_direct(t.tc.resolve_type(arg_id))
	}
	typ := t.node_type(arg_id)
	return typ == 'voidptr' || typ == 'nil' || typ == 'charptr' || typ == 'byteptr'
		|| typ.starts_with('&')
}

fn voidptr_variadic_type_passes_direct(typ types.Type) bool {
	if typ is types.Alias {
		return voidptr_variadic_type_passes_direct(typ.base_type)
	}
	return typ is types.Pointer || typ is types.Nil
}

fn (t &Transformer) voidptr_variadic_storage_type(arg_id flat.NodeId) string {
	typ := t.normalize_type_alias(t.node_type(arg_id))
	match typ {
		'char', 'i8', 'u8', 'i16', 'u16' {
			return 'int'
		}
		'f32' {
			return 'f64'
		}
		else {
			return typ
		}
	}
}

fn (mut t Transformer) transform_variadic_struct_fields(node flat.Node, field_start int, elem_type types.Type) ?flat.NodeId {
	if field_start >= node.children_count {
		return none
	}
	if elem_type !is types.Struct {
		return none
	}
	first := t.a.child_node(&node, field_start)
	if first.kind != .field_init {
		return none
	}
	mut field_ids := []flat.NodeId{}
	for i in field_start .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init {
			break
		}
		field_ids << field_id
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          elem_type.name()
		typ:            elem_type.name()
	})
	return t.transform_struct_fields(struct_id, t.a.nodes[int(struct_id)])
}

// make_array_literal_typed builds make array literal typed data for transform.
fn (mut t Transformer) make_array_literal_typed(values []flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	for value in values {
		t.a.children << value
	}
	return t.a.add_node(flat.Node{
		kind:           .array_literal
		children_start: start
		children_count: flat.child_count(values.len)
		typ:            typ
	})
}

// stringify_expr supports stringify expr handling for Transformer.
fn (mut t Transformer) stringify_expr(expr_id flat.NodeId) flat.NodeId {
	expr := t.transform_expr(expr_id)
	mut typ := t.node_type(expr)
	if typ.len == 0 {
		typ = t.node_type(expr_id)
	}
	if typ.len == 0 {
		// Structural fallback for compound arguments (infix, prefix, cast,
		// paren, ...) so e.g. `println(a + b)` for ints is stringified via
		// strconv__format_int instead of being passed to println as a raw
		// number. Mirrors the fallback already used by string interpolation.
		typ = t.reliable_stringify_type(expr)
		if typ.len == 0 {
			typ = t.reliable_stringify_type(expr_id)
		}
	}
	return t.wrap_string_conversion(expr, typ)
}

// reliable_stringify_type supports reliable stringify type handling for Transformer.
fn (t &Transformer) reliable_stringify_type(id flat.NodeId) string {
	mut typ := t.node_type(id)
	if typ.len > 0 {
		return typ
	}
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if node.typ.len > 0 {
			return node.typ
		}
		match node.kind {
			.int_literal {
				return 'int'
			}
			.float_literal {
				return 'f64'
			}
			.bool_literal {
				return 'bool'
			}
			.char_literal {
				return 'rune'
			}
			.string_literal, .string_interp {
				return 'string'
			}
			.infix {
				return t.reliable_infix_stringify_type(node)
			}
			.prefix {
				if node.op == .not {
					return 'bool'
				}
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			.paren {
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			.cast_expr {
				if node.value.len > 0 {
					return t.normalize_type_alias(node.value)
				}
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			else {}
		}
	}
	return ''
}

// reliable_infix_stringify_type supports reliable infix stringify type handling for Transformer.
fn (t &Transformer) reliable_infix_stringify_type(node flat.Node) string {
	if node.children_count < 2 {
		return ''
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type := t.reliable_stringify_type(lhs_id)
	rhs_type := t.reliable_stringify_type(rhs_id)
	if lhs_type == 'string' || rhs_type == 'string' {
		return 'string'
	}
	match node.op {
		.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or {
			return 'bool'
		}
		.right_shift_unsigned {
			if lhs_type.len > 0 {
				return t.unsigned_shift_type_text(lhs_type)
			}
		}
		.left_shift, .right_shift {
			// shifts keep the left operand's type/width
			if lhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type) {
				return lhs_type
			}
		}
		.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor {
			if lhs_type.len > 0 && rhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type)
				&& t.is_numeric_stringify_type(rhs_type) {
				if promoted := promote_numeric_literal_infix_type(t.a.nodes[int(lhs_id)], lhs_type,
					t.a.nodes[int(rhs_id)], rhs_type)
				{
					return promoted
				}
				// Use the promoted result type, not the lhs, so e.g.
				// `1 + u64(x)` formats as unsigned rather than signed int.
				return promote_numeric_stringify_type(lhs_type, rhs_type)
			}
		}
		else {}
	}

	return ''
}

fn (t &Transformer) unsigned_shift_type_text(typ string) string {
	clean := typ.trim_space()
	if !isnil(t.tc) {
		resolved := types.unsigned_shift_result_type(t.tc.parse_type(clean)).name()
		if resolved in ['u8', 'u16', 'u32', 'u64', 'usize'] {
			return resolved
		}
	}
	if types.is_builtin_type_name(clean) {
		return types.unsigned_shift_result_type(types.builtin_type_value(clean)).name()
	}
	return typ
}

fn promote_numeric_literal_infix_type(lhs flat.Node, lhs_type string, rhs flat.Node, rhs_type string) ?string {
	if promoted := promote_int_literal_infix_type(lhs, rhs, rhs_type) {
		return promoted
	}
	if promoted := promote_int_literal_infix_type(rhs, lhs, lhs_type) {
		return promoted
	}
	if lhs_type == 'f32' && rhs.kind == .float_literal {
		return 'f32'
	}
	if rhs_type == 'f32' && lhs.kind == .float_literal {
		return 'f32'
	}
	return none
}

fn promote_int_literal_infix_type(lit flat.Node, other flat.Node, other_type string) ?string {
	if lit.kind != .int_literal || other.kind == .int_literal {
		return none
	}
	value := decimal_int_literal_value(lit.value) or { return none }
	if unsigned_type_text_accepts_int_literal(other_type, value) {
		return other_type
	}
	return none
}

fn decimal_int_literal_value(text string) ?int {
	if text.len == 0 {
		return none
	}
	clean := text.replace('_', '')
	if clean.len == 0 {
		return none
	}
	for ch in clean {
		if ch < `0` || ch > `9` {
			return none
		}
	}
	return clean.int()
}

fn unsigned_type_text_accepts_int_literal(typ string, value int) bool {
	if value < 0 {
		return false
	}
	max := match typ {
		'u8', 'byte' { 255 }
		'u16' { 65535 }
		'u32', 'u64', 'usize' { return true }
		else { return false }
	}

	return value <= max
}

// promote_numeric_stringify_type returns the result type of a binary numeric
// operation for stringify purposes: floats dominate, the wider integer wins,
// and on equal width an explicit type beats the untyped-literal default `int`.
fn promote_numeric_stringify_type(a string, b string) string {
	if a == b {
		return a
	}
	if a == 'f64' || b == 'f64' {
		return 'f64'
	}
	if a == 'f32' || b == 'f32' {
		return 'f32'
	}
	ra := int_stringify_rank(a)
	rb := int_stringify_rank(b)
	if ra > rb {
		return a
	}
	if rb > ra {
		return b
	}
	if a == 'int' {
		return b
	}
	if b == 'int' {
		return a
	}
	return a
}

fn int_stringify_rank(typ string) int {
	return match typ {
		'i8', 'u8', 'byte' { 8 }
		'i16', 'u16' { 16 }
		'i32', 'u32', 'int', 'rune' { 32 }
		'i64', 'u64', 'isize', 'usize' { 64 }
		else { 32 }
	}
}

// is_numeric_stringify_type reports whether is numeric stringify type applies in transform.
fn (t &Transformer) is_numeric_stringify_type(typ string) bool {
	is_number := typ in ['int', 'int literal', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8',
		'byte', 'u16', 'u32', 'u64', 'f32', 'f64', 'float literal', 'rune']
	return is_number || typ in t.enum_types
}

// is_enum_stringify_type reports whether is enum stringify type applies in transform.
fn (t &Transformer) is_enum_stringify_type(typ string) bool {
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	if clean_typ.starts_with('mut ') {
		clean_typ = clean_typ[4..]
	}
	if clean_typ in t.enum_types {
		return true
	}
	mut qtyp := clean_typ
	if !qtyp.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp = '${t.cur_module}.${clean_typ}'
		if qtyp in t.enum_types {
			return true
		}
	}
	if isnil(t.tc) {
		return false
	}
	if alias := t.tc.type_aliases[clean_typ] {
		return t.is_enum_stringify_type(alias)
	}
	if qtyp != clean_typ {
		if alias := t.tc.type_aliases[qtyp] {
			return t.is_enum_stringify_type(alias)
		}
	}
	parsed := t.tc.parse_type(clean_typ)
	if parsed is types.Enum {
		return true
	}
	if qtyp != clean_typ {
		qparsed := t.tc.parse_type(qtyp)
		if qparsed is types.Enum {
			return true
		}
	}
	return false
}

// enum_str_method_name supports enum str method name handling for Transformer.
fn (t &Transformer) enum_str_method_name(typ string) ?string {
	mut candidates := []string{cap: 3}
	candidates << typ
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${typ}'
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(typ)
		if parsed is types.Enum {
			candidates << parsed.name
		}
	}
	for candidate in candidates {
		method := '${candidate}.str'
		if t.is_known_fn_name(method) {
			return method
		}
	}
	return none
}

// enum_autostr_call builds a call to the compiler-synthesized `<Enum>__autostr` helper
// (emitted by cgen's enum_str_defs) which returns the enum field NAME. Used as the default
// `${enum}` stringification when the user has not defined a custom `.str()` — V auto-derives
// one. Mirrors the struct-str qualification so the C name matches cgen's enum_decls naming.
fn (mut t Transformer) enum_autostr_call(expr flat.NodeId, typ string) flat.NodeId {
	mut qualified := typ
	if qualified.starts_with('main.') {
		qualified = qualified[5..]
	} else if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		q := '${t.cur_module}.${typ}'
		if q in t.enum_types {
			qualified = q
		}
	}
	return t.make_call_typed('${c_name(qualified)}__autostr', arr1(expr), 'string')
}

// wrap_string_conversion transforms wrap string conversion data for transform.
fn (mut t Transformer) wrap_string_conversion(expr flat.NodeId, typ string) flat.NodeId {
	mut clean_typ := typ
	is_ref := clean_typ.starts_with('&')
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	for clean_typ.starts_with('shared ') {
		clean_typ = clean_typ[7..].trim_space()
	}
	if clean_typ.starts_with('builtin.') {
		clean_typ = clean_typ.all_after_last('.')
	}
	if source_typ := t.source_type_name_from_c_name(clean_typ) {
		return t.wrap_string_conversion(expr, source_typ)
	}
	if source_typ := t.generic_specialized_source_type_name(clean_typ) {
		return t.wrap_string_conversion(expr, source_typ)
	}
	normalized_stringify_type := t.normalize_runtime_array_stringify_type(clean_typ)
	if normalized_stringify_type != clean_typ {
		return t.wrap_string_conversion(expr, normalized_stringify_type)
	}
	if t.is_fixed_array_type(clean_typ) {
		elem_type := fixed_array_elem_type(clean_typ)
		arr := t.fixed_array_value_to_array(expr, clean_typ, '[]${elem_type}')
		return t.wrap_string_conversion(arr, '[]${elem_type}')
	}
	if t.is_optional_type_name(clean_typ) {
		return t.wrap_optional_string_conversion(expr, clean_typ)
	}
	if clean_typ.len == 0 || clean_typ == 'unknown' {
		inferred := t.resolve_expr_type(expr)
		if inferred.len > 0 && inferred != clean_typ {
			return t.wrap_string_conversion(expr, inferred)
		}
	}
	if !isnil(t.tc) {
		if alias := t.tc.type_aliases[clean_typ] {
			return t.alias_str_wrap(expr, clean_typ, alias, is_ref)
		}
		mut qtyp := clean_typ
		if !qtyp.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qtyp = '${t.cur_module}.${clean_typ}'
		}
		if alias := t.tc.type_aliases[qtyp] {
			return t.alias_str_wrap(expr, clean_typ, alias, is_ref)
		}
		if !clean_typ.contains('.') {
			for aname, target in t.tc.type_aliases {
				if aname.all_after_last('.') == clean_typ {
					return t.alias_str_wrap(expr, clean_typ, target, is_ref)
				}
			}
		}
		parsed := t.tc.parse_type(clean_typ)
		if parsed is types.MultiReturn {
			return t.lower_multi_return_str(expr, parsed, clean_typ)
		}
		if parsed is types.Enum {
			if method := t.enum_str_method_name(clean_typ) {
				return t.make_call_typed(method, arr1(expr), 'string')
			}
			return t.enum_autostr_call(expr, clean_typ)
		}
		if qtyp != clean_typ {
			qparsed := t.tc.parse_type(qtyp)
			if qparsed is types.Enum {
				if method := t.enum_str_method_name(qtyp) {
					return t.make_call_typed(method, arr1(expr), 'string')
				}
				return t.enum_autostr_call(expr, qtyp)
			}
		}
	}
	if clean_typ == 'string' {
		return expr
	}
	if clean_typ == 'thread' || clean_typ.starts_with('thread ') {
		payload := if clean_typ.starts_with('thread ') {
			clean_typ[7..].trim_space()
		} else {
			'void'
		}
		return t.make_string_literal('thread(${payload})')
	}
	if clean_typ.starts_with('chan ') {
		channel_value := if is_ref { t.make_prefix(.mul, expr) } else { expr }
		return t.make_call_typed('v3_chan_str', arr2(channel_value,
			t.make_string_literal(clean_typ[5..].trim_space())), 'string')
	}
	if is_ref {
		expr_node := t.a.nodes[int(expr)]
		if expr_node.kind == .ident && t.string_interp_needs_value_read(expr_node.value, typ) {
			return t.wrap_string_conversion(t.make_prefix(.mul, expr), clean_typ)
		}
		// A `&Struct`/`&SumType` stringifies to the pointee's `.str()` value: the custom
		// method when one exists (no `&` prefix, e.g. map/array elements), otherwise
		// `&nil` for a null pointer or `&` + the value's auto str. Other pointer kinds
		// (voidptr, `&int`, `&string`, `&Interface`, ...) keep the raw ptr_str below.
		if aggregate := t.stringify_aggregate_type_name(clean_typ) {
			return t.lower_ref_str(expr, aggregate)
		}
	}
	iface_name := t.resolve_interface_type_name(clean_typ)
	if !is_ref && iface_name.len > 0 {
		if 'str' in t.tc.interface_abstract_method_names(iface_name) {
			value := t.stable_transformed_expr_for_reuse(expr, clean_typ, 'iface_str')
			method_name := '${iface_name}.str'
			t.mark_fn_used_name(method_name)
			t.mark_interface_method_implementers_used(iface_name, 'str')
			return t.make_call_typed(method_name, arr1(t.make_prefix(.amp, value)), 'string')
		}
	}
	if is_ref || clean_typ in ['voidptr', 'byteptr', 'charptr'] {
		return t.make_call_typed('ptr_str', arr1(expr), 'string')
	}
	if clean_typ == 'IError' || clean_typ == 'builtin.IError' {
		return t.make_call_typed('IError.str', arr1(expr), 'string')
	}
	if iface_name.len > 0 {
		str_key := '${iface_name}.str'
		known_str := str_key in t.fn_ret_types || (!isnil(t.tc) && str_key in t.tc.fn_ret_types)
		if known_str {
			return t.make_call_typed(str_key, arr1(expr), 'string')
		}
		return t.lower_interface_auto_str(expr, iface_name)
	}
	if clean_typ.starts_with('[]') || clean_typ.starts_with('map[') {
		if method_name := t.resolve_receiver_method_for_type(clean_typ, 'str') {
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, arr1(expr), 'string')
		}
	}
	match clean_typ {
		'bool' {
			return t.make_call_typed('bool.str', arr1(expr), 'string')
		}
		'u8', 'byte', 'u16', 'u32', 'usize' {
			// C integer promotion would pass an untruncated `int` into the u64
			// param (`u8(255) + u8(1)` is 256 in C); cast back to the value
			// type first so the arithmetic wraps at the V type's width.
			truncated := t.make_cast(clean_typ, expr, clean_typ)
			return t.make_call_typed('strconv__format_uint',
				arr2(truncated, t.make_int_literal(10)), 'string')
		}
		'u64' {
			return t.make_call_typed('u64.str', arr1(expr), 'string')
		}
		'int', 'int literal' {
			return t.make_call_typed('int.str', arr1(expr), 'string')
		}
		'i8' {
			return t.make_call_typed('i8.str', arr1(expr), 'string')
		}
		'i16' {
			return t.make_call_typed('i16.str', arr1(expr), 'string')
		}
		'i32' {
			return t.make_call_typed('i32.str', arr1(expr), 'string')
		}
		'i64' {
			return t.make_call_typed('i64.str', arr1(expr), 'string')
		}
		'isize' {
			return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
				'string')
		}
		'f32' {
			return t.make_call_typed('f32.str', arr1(expr), 'string')
		}
		'f64', 'float literal' {
			return t.make_call_typed('f64.str', arr1(expr), 'string')
		}
		else {
			if clean_typ in t.enum_types {
				if method := t.enum_str_method_name(clean_typ) {
					return t.make_call_typed(method, arr1(expr), 'string')
				}
				return t.enum_autostr_call(expr, clean_typ)
			}
			mut qenum := clean_typ
			if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
				&& t.cur_module != 'builtin' {
				qenum = '${t.cur_module}.${clean_typ}'
			}
			if qenum in t.enum_types {
				if method := t.enum_str_method_name(qenum) {
					return t.make_call_typed(method, arr1(expr), 'string')
				}
				return t.enum_autostr_call(expr, qenum)
			}
			if generic_str := t.generic_receiver_str_call(expr, clean_typ) {
				return generic_str
			}
			if aggregate_type := t.stringify_aggregate_type_name(clean_typ) {
				qualified := aggregate_type
				str_fn := '${c_name(qualified)}__str'
				v_str_fn := '${qualified}.str'
				known_str := str_fn in t.fn_ret_types || v_str_fn in t.fn_ret_types
					|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types
					|| v_str_fn in t.tc.fn_ret_types))
				if known_str {
					t.mark_fn_used_name(v_str_fn)
					t.mark_fn_used_name(str_fn)
					return t.make_call_typed(str_fn, arr1(expr), 'string')
				}
				if qualified in t.sum_types {
					return t.lower_sum_str(expr, qualified)
				}
				if struct_str := t.lower_struct_str(expr, qualified) {
					return struct_str
				}
				return t.make_string_literal('${qualified}{}')
			} else if clean_typ.len > 0 && clean_typ.starts_with('[]') {
				return t.lower_array_str(expr, clean_typ)
			} else if clean_typ.len > 0 && clean_typ.starts_with('map[') {
				return t.lower_map_str(expr, clean_typ)
			} else if clean_typ == 'rune' {
				return t.make_call_typed('rune.str', arr1(expr), 'string')
			} else {
				return expr
			}
		}
	}
}

fn (mut t Transformer) lower_interface_auto_str(expr flat.NodeId, iface_name string) flat.NodeId {
	display_name := iface_name.all_after_last('.')
	if isnil(t.tc) {
		return t.make_string_literal('${display_name}{}')
	}
	value := t.stable_transformed_expr_for_reuse(expr, iface_name, 'iface_str')
	result_name := t.new_temp('iface_str')
	t.pending_stmts << t.make_decl_assign_typed(result_name,
		t.make_string_literal('${display_name}{}'), 'string')
	tag := t.make_selector(value, '_typ', 'int')
	impl_names := if t.is_builtin_ierror_interface_name(iface_name) {
		t.tc.ierror_impl_names()
	} else {
		t.tc.interface_impl_names(iface_name)
	}
	for impl_name in impl_names {
		if !t.interface_boxed_type_marked(iface_name, impl_name) {
			continue
		}
		type_id := t.interface_impl_type_id(iface_name, impl_name) or { continue }
		object := t.make_cast('&${impl_name}', t.make_selector(value, '_object', 'voidptr'),
			'&${impl_name}')
		concrete := t.make_prefix(.mul, object)
		t.set_node_typ(int(concrete), impl_name)
		saved := t.pending_stmts.clone()
		t.pending_stmts.clear()
		inner := t.wrap_string_conversion(concrete, impl_name)
		wrapped := t.string_plus(t.string_plus(t.make_string_literal('${display_name}('), inner),
			t.make_string_literal(')'))
		mut then_body := []flat.NodeId{}
		t.drain_pending(mut then_body)
		t.pending_stmts = saved
		assign := t.make_assign(t.make_ident(result_name), wrapped)
		then_body << assign
		cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
		t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	}
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

// lower_ref_str stringifies a `&Struct`/`&SumType` pointer with the same semantics V uses
// for `.str()` and for map/array elements: when the pointee type defines a custom `str()`,
// call it (no `&` prefix); otherwise emit `&nil` for a null pointer and `&` + the value's
// auto str for a live one. `aggregate` is the resolved (non-reference) struct/sum type name.
fn (mut t Transformer) lower_ref_str(expr flat.NodeId, aggregate string) flat.NodeId {
	str_fn := '${c_name(aggregate)}__str'
	has_custom := str_fn in t.fn_ret_types || (!isnil(t.tc) && str_fn in t.tc.fn_ret_types)
	if has_custom {
		return t.lower_ref_str_guarded(expr, aggregate, false)
	}
	return t.lower_ref_str_prefixed(expr, aggregate)
}

fn (t &Transformer) str_method_has_pointer_receiver(str_fn string) bool {
	if isnil(t.tc) {
		return false
	}
	params := t.tc.fn_param_types[str_fn] or { return false }
	if params.len == 0 {
		return false
	}
	receiver := params[0]
	return receiver is types.Pointer || receiver.name().starts_with('&')
}

fn (mut t Transformer) lower_ref_str_prefixed(expr flat.NodeId, aggregate string) flat.NodeId {
	return t.lower_ref_str_guarded(expr, aggregate, true)
}

fn (mut t Transformer) lower_ref_str_guarded(expr flat.NodeId, aggregate string, prefix_non_nil bool) flat.NodeId {
	ptr_type := '&${aggregate}'
	ptr_name := t.new_temp('ref_str_ptr')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), 'string')
	// Stringifying the pointee may hoist its own prelude (nested arrays/maps dereference the
	// pointer). Capture that prelude and keep it inside the non-nil branch so a null pointer
	// is never dereferenced.
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	str_fn := '${c_name(aggregate)}__str'
	value_str := if t.str_method_has_pointer_receiver(str_fn) {
		t.make_call_typed(str_fn, arr1(t.make_ident(ptr_name)), 'string')
	} else {
		t.wrap_string_conversion(t.make_prefix(.mul, t.make_ident(ptr_name)), aggregate)
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	non_nil := if prefix_non_nil {
		t.string_plus(t.make_string_literal('&'), value_str)
	} else {
		value_str
	}
	then_body << t.make_assign(t.make_ident(res_name), non_nil)
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

// alias_str_wrap stringifies an alias value. V wraps the base str with `AliasName(...)` for
// aliases of non-primitive types (arrays, maps, structs, sum types, enums), e.g.
// `Block([1, 2])` or `AEnum(red)`, but stringifies primitive aliases (int, string, bool, ...)
// as the bare value.
fn (mut t Transformer) alias_str_wrap(expr flat.NodeId, alias_name string, base_type string, is_ref bool) flat.NodeId {
	if custom := t.alias_custom_str_call(expr, alias_name) {
		return custom
	}
	resolved_base := t.alias_str_resolved_base_type(base_type)
	inner_type := if is_ref { '&${resolved_base}' } else { resolved_base }
	inner := t.wrap_string_conversion(expr, inner_type)
	if t.alias_str_suppress_wrapper_for_mut_param_deref(expr) {
		return inner
	}
	if !t.alias_str_needs_name_wrapper(base_type) {
		return inner
	}
	display := struct_string_display_name(alias_name)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner),
		t.make_string_literal(')'))
}

// alias_custom_str_call builds a call to the alias's own str() method when one exists;
// an alias-level str() overrides the base type's stringification entirely (no name wrapper).
fn (mut t Transformer) alias_custom_str_call(expr flat.NodeId, alias_name string) ?flat.NodeId {
	mut candidates := [alias_name]
	if !alias_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${alias_name}'
	}
	for qname in candidates {
		str_fn := '${c_name(qname)}__str'
		v_str_fn := '${qname}.str'
		known := str_fn in t.fn_ret_types || v_str_fn in t.fn_ret_types
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types))
		if known {
			t.mark_fn_used_name(v_str_fn)
			t.mark_fn_used_name(str_fn)
			mut receiver := expr
			if t.str_method_has_pointer_receiver(str_fn)
				|| t.str_method_has_pointer_receiver(v_str_fn) {
				mut expr_type := t.node_type(expr)
				if expr_type.len == 0 {
					expr_type = t.resolve_expr_type(expr)
				}
				if !expr_type.starts_with('&') && !t.expr_can_take_address(expr) {
					tmp_name := t.new_temp('alias_str')
					t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, qname)
					receiver = t.make_ident(tmp_name)
				}
			}
			return t.make_call_typed(str_fn, arr1(receiver), 'string')
		}
	}
	return none
}

// lookup_str_alias resolves a type name to `(alias_name, base_type)` when it names a type
// alias, mirroring the direct/module-qualified/suffix lookups of wrap_string_conversion.
fn (t &Transformer) lookup_str_alias(clean_typ string) ?(string, string) {
	if isnil(t.tc) || clean_typ.len == 0 {
		return none
	}
	if alias := t.tc.type_aliases[clean_typ] {
		return clean_typ, alias
	}
	if !clean_typ.contains('.') {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qtyp := '${t.cur_module}.${clean_typ}'
			if alias := t.tc.type_aliases[qtyp] {
				return clean_typ, alias
			}
		}
		for aname, target in t.tc.type_aliases {
			if aname.all_after_last('.') == clean_typ {
				return clean_typ, target
			}
		}
	}
	return none
}

fn (t &Transformer) alias_str_suppress_wrapper_for_mut_param_deref(expr flat.NodeId) bool {
	if int(expr) < 0 || int(expr) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(expr)]
	if node.kind != .prefix || node.op != .mul || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(&node, 0)
	return base.kind == .ident && t.mut_param_values[base.value]
}

fn (t &Transformer) alias_str_resolved_base_type(base_type string) string {
	mut clean := base_type.trim_space()
	mut seen := []string{}
	for clean.len > 0 && clean !in seen {
		seen << clean
		next := t.normalize_type_alias(clean).trim_space()
		if next == clean {
			break
		}
		clean = next
	}
	return clean
}

fn (t &Transformer) alias_str_needs_name_wrapper(base_type string) bool {
	mut clean := t.alias_str_resolved_base_type(base_type)
	if clean.starts_with('&') {
		return false
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	return clean !in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal',
		'voidptr', 'byteptr', 'charptr', 'nil', 'void']
}

fn (mut t Transformer) lower_struct_str(expr flat.NodeId, struct_type string) ?flat.NodeId {
	info := t.lookup_struct_info(struct_type) or {
		t.generic_struct_info_for_stringify(struct_type) or { return none }
	}
	if struct_type.contains('.') && struct_type.all_after_last('.') == 'Array_string' {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	if struct_type in t.stringify_stack {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	t.stringify_stack << struct_type
	defer {
		t.stringify_stack.delete_last()
	}
	if info.fields.len == 0 {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	base := t.stable_transformed_expr_for_reuse(expr, struct_type, 'struct_str')
	display := struct_string_display_name(struct_type)
	mut result := t.make_string_literal('${display}{\n')
	for field in info.fields {
		field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
		if field_type.len == 0 {
			continue
		}
		raw_field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field_type }
		mut field_str := if field_type == struct_type {
			t.make_string_literal('${struct_string_display_name(field_type)}{}')
		} else {
			t.struct_field_str_value(t.make_selector(base, field.name, field_type), raw_field_type,
				field_type)
		}
		if raw_field_type == 'string' || raw_field_type == 'builtin.string' {
			field_str = t.string_plus(t.string_plus(t.make_string_literal("'"), field_str),
				t.make_string_literal("'"))
		}
		if t.struct_str_field_needs_indent(raw_field_type) {
			t.mark_fn_used_name('string.replace')
			t.mark_fn_used_name('string__replace')
			field_str = t.make_call_typed('string.replace', arr3(field_str,
				t.make_string_literal('\n'), t.make_string_literal('\n    ')), 'string')
		}
		result = t.string_plus(result, t.make_string_literal('    ${field.name}: '))
		result = t.string_plus(result, field_str)
		result = t.string_plus(result, t.make_string_literal('\n'))
	}
	return t.string_plus(result, t.make_string_literal('}'))
}

fn struct_string_display_name(typ string) string {
	if typ.all_after_last('.').starts_with('AnonStruct_') {
		return 'struct '
	}
	if typ.starts_with('main.') {
		return typ.all_after_last('.')
	}
	return typ
}

// struct_field_str_value stringifies one struct field for the auto-generated struct str.
// Unlike top-level stringification, V wraps an alias-typed field as `AliasName(value)` even
// when the alias base is primitive (`d: Duration(42)`), unless the alias defines its own
// str() method, which is used bare.
fn (mut t Transformer) struct_field_str_value(expr flat.NodeId, raw_field_type string, field_type string) flat.NodeId {
	mut clean := raw_field_type.trim_space()
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!')
		|| clean.starts_with('shared ') {
		return t.wrap_string_conversion(expr, field_type)
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	alias_name, base_type := t.lookup_str_alias(clean) or {
		return t.wrap_string_conversion(expr, field_type)
	}
	if custom := t.alias_custom_str_call(expr, alias_name) {
		return custom
	}
	resolved_base := t.alias_str_resolved_base_type(base_type)
	inner := t.wrap_string_conversion(expr, resolved_base)
	display := struct_string_display_name(alias_name)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner),
		t.make_string_literal(')'))
}

// struct_str_field_needs_indent reports whether a struct auto-str field value can span
// multiple lines (nested structs, arrays of structs, options, sum types), so its stringified
// text must be re-indented one level deeper, matching V's indent_count handling. Values that
// are always single-line (primitives, plain strings, enums, custom str() output) keep their
// text untouched — V does not re-indent multi-line string content or custom str() results.
fn (mut t Transformer) struct_str_field_needs_indent(field_type string) bool {
	mut clean := field_type.trim_space()
	for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		clean = clean[1..].trim_space()
	}
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	if _, _ := t.lookup_str_alias(clean) {
		return false
	}
	resolved := t.alias_str_resolved_base_type(clean)
	if !t.alias_str_needs_name_wrapper(resolved) {
		return false
	}
	if resolved in t.enum_types {
		return false
	}
	if !resolved.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && '${t.cur_module}.${resolved}' in t.enum_types {
		return false
	}
	if aggregate := t.stringify_aggregate_type_name(resolved) {
		str_fn := '${c_name(aggregate)}__str'
		v_str_fn := '${aggregate}.str'
		if str_fn in t.fn_ret_types || v_str_fn in t.fn_ret_types
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types)) {
			return false
		}
	}
	return true
}

// lower_multi_return_str formats a multi-return value as `(a, b, ...)`.
fn (mut t Transformer) lower_multi_return_str(expr flat.NodeId, multi types.MultiReturn, typ string) flat.NodeId {
	base := t.stable_transformed_expr_for_reuse(expr, typ, 'multi_ret_str')
	mut result := t.make_string_literal('(')
	for i, item in multi.types {
		if i > 0 {
			result = t.string_plus(result, t.make_string_literal(', '))
		}
		item_type := item.name()
		mut item_str := t.wrap_string_conversion(t.make_selector(base, 'arg${i}', item_type),
			item_type)
		if item is types.String {
			item_str = t.string_plus(t.string_plus(t.make_string_literal("'"), item_str),
				t.make_string_literal("'"))
		} else if item is types.Rune {
			item_str = t.string_plus(t.string_plus(t.make_string_literal('`'), item_str),
				t.make_string_literal('`'))
		}
		result = t.string_plus(result, item_str)
	}
	return t.string_plus(result, t.make_string_literal(')'))
}

fn (t &Transformer) stringify_aggregate_type_name(typ string) ?string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return none
	}
	base, args, is_generic := generic_app_parts(clean)
	if is_generic && args.len > 0 && t.generic_aggregate_base_exists(base, args.len) {
		return clean
	}
	if clean.contains('.') {
		if !isnil(t.tc) && (clean in t.tc.structs || clean in t.tc.sum_types) {
			return clean
		}
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.structs || qname in t.sum_types {
			return qname
		}
		if !isnil(t.tc) && (qname in t.tc.structs || qname in t.tc.sum_types) {
			return qname
		}
	}
	if qualified := t.qualified_types[clean] {
		if qualified in t.structs || qualified in t.sum_types {
			return qualified
		}
		if !isnil(t.tc) && (qualified in t.tc.structs || qualified in t.tc.sum_types) {
			return qualified
		}
	}
	if clean in t.structs || clean in t.sum_types {
		return clean
	}
	if !isnil(t.tc) && (clean in t.tc.structs || clean in t.tc.sum_types) {
		return clean
	}
	return none
}

fn (t &Transformer) generic_specialized_source_type_name(typ string) ?string {
	clean := typ.trim_space().trim_left('&')
	args := t.recorded_generic_specialization_args(clean) or { return none }
	if args.len == 0 {
		return none
	}
	suffix := generic_type_suffixes(args)
	if !isnil(t.tc) {
		for base, params in t.tc.struct_generic_params {
			if params.len == args.len
				&& generic_specialized_type_matches_flat_name(clean, base, suffix) {
				return generic_specialized_source_type_name_for_base(base, args)
			}
		}
		for base, params in t.tc.sum_generic_params {
			if params.len == args.len
				&& generic_specialized_type_matches_flat_name(clean, base, suffix) {
				return generic_specialized_source_type_name_for_base(base, args)
			}
		}
	}
	return none
}

fn (t &Transformer) generic_aggregate_base_exists(base string, arg_count int) bool {
	if isnil(t.tc) {
		return false
	}
	if params := t.tc.struct_generic_params[base] {
		return params.len == arg_count
	}
	if params := t.tc.sum_generic_params[base] {
		return params.len == arg_count
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${base}'
		if params := t.tc.struct_generic_params[qname] {
			return params.len == arg_count
		}
		if params := t.tc.sum_generic_params[qname] {
			return params.len == arg_count
		}
	}
	return false
}

fn generic_specialized_type_matches_flat_name(flat_name string, base string, suffix string) bool {
	if flat_name.len == 0 || base.len == 0 || suffix.len == 0 {
		return false
	}
	mut candidates := []string{}
	add_generic_specialized_type_candidate(mut candidates, '${base}_${suffix}')
	add_generic_specialized_type_candidate(mut candidates, c_name('${base}_${suffix}'))
	if base.contains('.') {
		module_name := base.all_before_last('.')
		short_base := base.all_after_last('.')
		if module_name == 'main' {
			add_generic_specialized_type_candidate(mut candidates, '${short_base}_${suffix}')
		}
		add_generic_specialized_type_candidate(mut candidates,
			'${module_name}.${short_base}_${suffix}')
		add_generic_specialized_type_candidate(mut candidates,
			c_name('${module_name}.${short_base}_${suffix}'))
	}
	return flat_name in candidates
}

fn add_generic_specialized_type_candidate(mut candidates []string, candidate string) {
	clean := candidate.trim_space()
	if clean.len > 0 && clean !in candidates {
		candidates << clean
	}
}

fn generic_specialized_source_type_name_for_base(base string, args []string) string {
	display_base := if base.starts_with('main.') { base.all_after_last('.') } else { base }
	return '${display_base}[${args.join(', ')}]'
}

fn (t &Transformer) source_type_name_from_c_name(typ string) ?string {
	clean := typ.trim_space()
	if clean.len == 0 || !clean.contains('__') || t.type_name_is_declared(clean) {
		return none
	}
	for name, _ in t.structs {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.sum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.enum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	if isnil(t.tc) {
		return none
	}
	for name, _ in t.tc.structs {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.sum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.enum_names {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.type_aliases {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	return none
}

fn source_type_name_matches_c_name(name string, cname string) bool {
	return name.contains('.') && c_name(name) == cname
}

fn (t &Transformer) normalize_runtime_array_stringify_type(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('Array_') && !clean.starts_with('Array_fixed_') {
		if clean in t.structs || clean in t.sum_types || clean in t.enum_types {
			return typ
		}
		if !isnil(t.tc) && clean in t.tc.type_aliases {
			return typ
		}
		decoded := t.generic_type_arg_from_suffix(clean)
		if decoded.len > 0 {
			return decoded
		}
	}
	return typ
}

fn (mut t Transformer) mark_interface_method_implementers_used(iface_name string, method string) {
	if isnil(t.tc) {
		return
	}
	for concrete, _ in t.tc.structs {
		if t.tc.named_type_implements_interface(concrete, iface_name) {
			t.mark_fn_used_name('${concrete}.${method}')
		}
	}
	for concrete, _ in t.tc.type_aliases {
		if t.tc.named_type_implements_interface(concrete, iface_name) {
			t.mark_fn_used_name('${concrete}.${method}')
		}
	}
}

fn (mut t Transformer) lower_sum_str(expr flat.NodeId, sum_name string) flat.NodeId {
	resolved_sum := t.resolve_sum_name(sum_name)
	variants := t.sum_types[resolved_sum] or { return t.make_string_literal('${sum_name}{}') }
	sum_display := if resolved_sum.contains('.') {
		resolved_sum.all_after_last('.')
	} else {
		resolved_sum
	}
	if resolved_sum in t.stringify_stack {
		return t.make_string_literal('${sum_display}{}')
	}
	t.stringify_stack << resolved_sum
	defer {
		t.stringify_stack.delete_last()
	}
	base := t.stable_transformed_expr_for_reuse(expr, resolved_sum, 'sum_str')
	tag := t.make_selector_op(base, 'typ', 'int', if sum_name.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.build_sum_str_chain(base, tag, resolved_sum, sum_display, variants, 0)
}

fn (mut t Transformer) build_sum_str_chain(base flat.NodeId, tag flat.NodeId, sum_name string, sum_display string, variants []string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_string_literal('${sum_name}{}')
	}
	variant := variants[idx]
	field := t.sum_field_name(variant)
	field_sel := t.make_selector_op(base, field, '&${variant}', .dot)
	variant_base := t.normalize_type_alias(variant)
	// Only statements created for THIS branch's payload conversion belong inside
	// the branch; earlier pending statements (e.g. the base temp decl from
	// lower_sum_str) must stay with the caller so they precede the whole if-chain.
	pending_start := t.pending_stmts.len
	mut value_text := if t.is_fixed_array_type(variant_base) {
		elem_type := fixed_array_elem_type(variant_base)
		arr := t.make_call_typed('new_array_from_c_array', [
			t.make_fixed_array_len_expr(variant_base),
			t.make_fixed_array_len_expr(variant_base),
			t.make_sizeof_type(elem_type),
			field_sel,
		], '[]${elem_type}')
		t.wrap_string_conversion(arr, '[]${elem_type}')
	} else {
		value := t.make_prefix(.mul, field_sel)
		t.set_node_typ(int(value), variant)
		t.wrap_string_conversion(value, variant)
	}
	// V prints a sum value as `SumName(payload_str)` — the payload's own str
	// already carries its type name for structs; string/rune payloads are quoted.
	// An alias variant keeps its alias-name wrapper (`Res(Ints([1, 2]))`).
	if variant_base == 'string' {
		value_text = t.string_plus(t.string_plus(t.make_string_literal("'"), value_text),
			t.make_string_literal("'"))
	} else if variant_base == 'rune' {
		value_text = t.string_plus(t.string_plus(t.make_string_literal('`'), value_text),
			t.make_string_literal('`'))
	} else if variant_base != variant {
		display := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		value_text = t.string_plus(t.string_plus(t.make_string_literal('${display}('), value_text),
			t.make_string_literal(')'))
	}
	value_text = t.string_plus(t.string_plus(t.make_string_literal('${sum_display}('), value_text),
		t.make_string_literal(')'))
	mut then_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	then_stmts << t.make_expr_stmt(value_text)
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name, variant)))
	then_block := t.make_block(then_stmts)
	else_expr := t.build_sum_str_chain(base, tag, sum_name, sum_display, variants, idx + 1)
	else_block := t.make_block(arr1(t.make_expr_stmt(else_expr)))
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            'string'
	})
}

fn (mut t Transformer) wrap_formatted_string_conversion(expr flat.NodeId, typ string, format string) flat.NodeId {
	if format.len == 0 {
		return t.wrap_string_conversion(expr, typ)
	}
	if format.contains('X') {
		// Uppercase hex behaves like lowercase `x` but with A-F. Format the
		// lowercase form first so width/zero-padding flags are honored, then
		// upper-case the whole result.
		lowered := t.wrap_formatted_string_conversion(expr, typ, format.replace('X', 'x'))
		return t.make_call_typed('v3_string_upper_ascii', arr1(lowered), 'string')
	}
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	if clean_typ.starts_with('builtin.') {
		clean_typ = clean_typ.all_after_last('.')
	}
	// An enum with a base (`x`/`b`/`o`) or decimal (`d`) verb prints its integer
	// value, not its name; format the underlying integer. Unsigned-backed enums use
	// u64 so values above i64.max are not rendered as negative.
	if format[format.len - 1] in [`x`, `b`, `o`, `d`] && t.is_formatted_enum_type(clean_typ) {
		int_type := if t.enum_backing_is_unsigned(clean_typ) { 'u64' } else { 'i64' }
		return t.wrap_formatted_string_conversion(t.make_cast(int_type, expr, int_type), int_type,
			format)
	}
	if decimal_format := fixed_decimal_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast('f64', expr, 'f64')
			}
			mut formatted := t.make_call_typed('v3_f64_fixed', arr2(arg,
				t.make_int_literal(decimal_format.precision)), 'string')
			if decimal_format.width > 0 || decimal_format.left {
				left := if decimal_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', arr3(formatted,
					t.make_int_literal(decimal_format.width), t.make_int_literal(left)), 'string')
			}
			return formatted
		}
	}
	if format == 'c' {
		if clean_typ in ['u8', 'byte', 'char', 'rune', 'int'] {
			arg := if clean_typ == 'int' {
				expr
			} else {
				t.make_cast('int', expr, 'int')
			}
			return t.make_call_typed('v3_char_string', arr1(arg), 'string')
		}
	}
	if base := integer_format_base(format) {
		if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
			formatted := t.make_call_typed('strconv__format_uint', arr2(expr,
				t.make_int_literal(base)), 'string')
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', arr1(formatted), 'string')
			} else {
				formatted
			}
		}
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			formatted := t.make_call_typed('strconv__format_int', arr2(expr,
				t.make_int_literal(base)), 'string')
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', arr1(formatted), 'string')
			} else {
				formatted
			}
		}
	}
	if base_format := zero_padded_integer_base_format(format) {
		converted := if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
			arg := if clean_typ == 'u64' {
				expr
			} else {
				t.make_cast('u64', expr, 'u64')
			}
			t.make_call_typed('strconv__format_uint', arr2(arg,
				t.make_int_literal(base_format.base)), 'string')
		} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			arg := if clean_typ == 'i64' {
				expr
			} else {
				t.make_cast('i64', expr, 'i64')
			}
			t.make_call_typed('strconv__format_int',
				arr2(arg, t.make_int_literal(base_format.base)), 'string')
		} else {
			t.wrap_string_conversion(expr, typ)
		}
		return t.make_call_typed('v3_string_zpad', arr2(converted,
			t.make_int_literal(base_format.width)), 'string')
	}
	if width := zero_padded_decimal_width(format) {
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune', 'usize', 'u8', 'byte',
			'u16', 'u32', 'u64'] {
			if clean_typ in ['u64', 'usize', 'u32', 'u16', 'u8', 'byte'] {
				arg := if clean_typ == 'u64' {
					expr
				} else {
					t.make_cast('u64', expr, 'u64')
				}
				return t.make_call_typed('v3_u64_zpad', arr2(arg, t.make_int_literal(width)),
					'string')
			}
			if clean_typ in ['i64', 'isize', 'i32', 'i16', 'i8', 'rune'] {
				arg := if clean_typ == 'i64' {
					expr
				} else {
					t.make_cast('i64', expr, 'i64')
				}
				return t.make_call_typed('v3_i64_zpad', arr2(arg, t.make_int_literal(width)),
					'string')
			}
			return t.make_call_typed('v3_int_zpad', arr2(expr, t.make_int_literal(width)), 'string')
		}
	}
	if width := static_format_width(format) {
		mut converted := if base := integer_format_base_suffix(format) {
			if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
				t.make_call_typed('strconv__format_uint', arr2(expr, t.make_int_literal(base)),
					'string')
			} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
				t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(base)),
					'string')
			} else {
				t.wrap_string_conversion(expr, typ)
			}
		} else {
			t.wrap_string_conversion(expr, typ)
		}
		return t.make_call_typed('v3_string_pad', arr3(converted, t.make_int_literal(width),
			t.make_int_literal(0)), 'string')
	}
	return t.wrap_string_conversion(expr, typ)
}

struct FixedDecimalFormat {
	width     int
	precision int
	left      bool
}

fn fixed_decimal_format(format string) ?FixedDecimalFormat {
	if format.len < 2 {
		return none
	}
	mut i := 0
	mut left := false
	if i < format.len && format[i] == `-` {
		left = true
		i++
	}
	if i < format.len && format[i] == `0` {
		i++
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if i >= format.len || format[i] != `.` {
		return none
	}
	i++
	mut precision := 0
	mut has_precision := false
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		has_precision = true
		precision = precision * 10 + int(format[i] - `0`)
		i++
	}
	if !has_precision {
		return none
	}
	if i < format.len {
		if format[i] != `f` {
			return none
		}
		i++
	} else if precision > 0 {
		precision--
	}
	if i != format.len {
		return none
	}
	return FixedDecimalFormat{
		width:     width
		precision: precision
		left:      left
	}
}

fn (t &Transformer) enum_backing_is_unsigned(clean_typ string) bool {
	mut backing := t.enum_backing_types[clean_typ] or { '' }
	if backing.len == 0 && !clean_typ.contains('.') && t.cur_module.len > 0
		&& t.cur_module !in ['main', 'builtin'] {
		backing = t.enum_backing_types['${t.cur_module}.${clean_typ}'] or { '' }
	}
	return backing in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize']
}

fn (t &Transformer) is_formatted_enum_type(clean_typ string) bool {
	if clean_typ in t.enum_types {
		return true
	}
	if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		return '${t.cur_module}.${clean_typ}' in t.enum_types
	}
	return false
}

fn integer_format_base(format string) ?int {
	match format {
		'b' {
			return 2
		}
		'x', 'X' {
			return 16
		}
		'o' {
			return 8
		}
		else {}
	}

	return none
}

fn integer_format_base_suffix(format string) ?int {
	if format.len == 0 {
		return none
	}
	match format[format.len - 1] {
		`b` { return 2 }
		`x` { return 16 }
		`o` { return 8 }
		else {}
	}

	return none
}

fn static_format_width(format string) ?int {
	if format.len == 0 || format[0] == `0` {
		return none
	}
	mut i := 0
	mut sign := 1
	if format[i] == `-` {
		sign = -1
		i++
	}
	if i >= format.len || format[i] < `0` || format[i] > `9` {
		return none
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if width <= 0 {
		return none
	}
	if i == format.len {
		return sign * width
	}
	if i == format.len - 1 && format[i] in [`s`, `d`, `b`, `x`, `o`, `p`] {
		return sign * width
	}
	return none
}

struct ZeroPaddedIntegerBaseFormat {
	width int
	base  int
}

fn zero_padded_integer_base_format(format string) ?ZeroPaddedIntegerBaseFormat {
	if format.len < 3 || format[0] != `0` {
		return none
	}
	end := format.len - 1
	base := match format[end] {
		`b` { 2 }
		`x` { 16 }
		`o` { 8 }
		else { return none }
	}

	mut width := 0
	for i in 1 .. end {
		ch := format[i]
		if ch < `0` || ch > `9` {
			return none
		}
		width = width * 10 + int(ch - `0`)
	}
	if width <= 0 {
		return none
	}
	return ZeroPaddedIntegerBaseFormat{
		width: width
		base:  base
	}
}

fn zero_padded_decimal_width(format string) ?int {
	if format.len < 2 || format[0] != `0` {
		return none
	}
	mut end := format.len
	if format[end - 1] == `d` {
		end--
	}
	if end >= 3 && format[end - 2] == `.` && format[end - 1] == `0` {
		end -= 2
	}
	if end <= 1 {
		return none
	}
	mut width := 0
	for i in 1 .. end {
		ch := format[i]
		if ch < `0` || ch > `9` {
			return none
		}
		width = width * 10 + int(ch - `0`)
	}
	if width <= 0 {
		return none
	}
	return width
}

fn (mut t Transformer) generic_receiver_str_call(expr flat.NodeId, typ string) ?flat.NodeId {
	if isnil(t.tc) || !typ.contains('[') {
		return none
	}
	clean_typ := if typ.starts_with('&') { typ[1..] } else { typ }
	if !clean_typ.ends_with(']') {
		return none
	}
	info := t.tc.resolve_generic_struct_method(clean_typ, 'str') or {
		t.tc.resolve_generic_sum_method(clean_typ, 'str') or { return none }
	}
	if info.return_type.name() != 'string' {
		return none
	}
	method_name := '${clean_typ}.str'
	t.mark_fn_used_name(method_name)
	return t.make_call_typed(method_name, arr1(expr), 'string')
}

// append_string builds `result = result + piece` using the runtime string concat helper.
// Using string__plus directly (instead of `+=`) keeps the synthesized node independent of
// type resolution for the freshly-introduced temp.
fn (mut t Transformer) append_string(result_name string, piece flat.NodeId) flat.NodeId {
	concat := t.make_call_typed('string__plus', arr2(t.make_ident(result_name), piece), 'string')
	return t.make_assign(t.make_ident(result_name), concat)
}

// lower_array_str expands `${arr}` for a `[]T` into a runtime loop that formats each element
// via wrap_string_conversion, so nested arrays, structs with `str`, enums, etc. all recurse
// correctly. Produces `[e0, e1, ...]`; string elements are wrapped in single quotes to match V.
fn (mut t Transformer) lower_array_str(arr_expr flat.NodeId, base_type string) flat.NodeId {
	src := t.a.nodes[int(arr_expr)]
	mut elem_type := base_type[2..]
	if recorded_elem_type := t.recorded_array_call_elem_type(arr_expr) {
		elem_type = recorded_elem_type
	}
	if elem_type.len == 0 || elem_type == 'void' {
		if selector := t.selector_expr_node(arr_expr) {
			mut selector_type := t.resolve_selector_type(selector)
			if selector_type.len == 0 || selector_type == '[]' || selector_type == '[]void'
				|| selector_type == 'shared []' || selector_type == 'shared []void' {
				if selector.children_count > 0 {
					base_id := t.a.child(&selector, 0)
					mut field_base_type := t.node_type(base_id)
					if field_base_type.starts_with('&') {
						field_base_type = field_base_type[1..]
					}
					for field_base_type.starts_with('shared ') {
						field_base_type = field_base_type[7..].trim_space()
					}
					if raw_type := t.lookup_struct_field_raw_type(field_base_type, selector.value) {
						selector_type = raw_type
					} else if field_type := t.lookup_struct_field_type(field_base_type,
						selector.value)
					{
						selector_type = field_type
					}
				}
			}
			for selector_type.starts_with('shared ') {
				selector_type = selector_type[7..].trim_space()
			}
			if selector_type.starts_with('[]') && selector_type.len > 2 {
				elem_type = selector_type[2..]
			}
		}
	}
	base := t.stable_expr_for_reuse(arr_expr)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('arr_str')
	idx_name := t.new_temp('arr_str_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('['), 'string')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_name := t.new_temp('arr_str_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	// `if idx > 0 { result = result + ', ' }`
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block(arr1(sep_stmt)), t.make_empty())
	// element text (recurses; may push its own statements for nested arrays/optionals)
	t.set_var_type(elem_name, elem_type)
	elem_str := t.wrap_string_conversion(t.make_ident(elem_name), elem_type)
	t.unset_var_type(elem_name)
	t.drain_pending(mut loop_body)
	// Quote string/rune elements, including aliases of them (`type Literal = string`).
	quote_elem := t.normalize_type_alias(elem_type)
	if quote_elem == 'string' {
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
	} else if quote_elem == 'rune' {
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
	} else {
		loop_body << t.append_string(result_name, elem_str)
	}
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal(']'))
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (t &Transformer) recorded_array_call_elem_type(call_id flat.NodeId) ?string {
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value.contains('__') {
		if !callee.value.ends_with('__array') {
			return none
		}
		receiver := callee.value.all_before_last('__')
		if args := t.recorded_generic_specialization_args(receiver) {
			if args.len == 1 {
				return args[0]
			}
		}
	}
	if callee.kind == .selector && callee.children_count > 0 {
		if callee.value != 'array' {
			return none
		}
		base_id := t.a.child(callee, 0)
		mut receiver_type := t.node_type(base_id)
		if receiver_type.starts_with('&') {
			receiver_type = receiver_type[1..]
		}
		if args := t.current_specialized_receiver_args(receiver_type) {
			if args.len == 1 {
				return args[0]
			}
		}
		if args := t.recorded_generic_specialization_args(receiver_type) {
			if args.len == 1 {
				return args[0]
			}
		}
		_, generic_args, is_generic_receiver := generic_app_parts(receiver_type)
		if is_generic_receiver && generic_args.len == 1 {
			if stringify_type_has_generic_placeholder(generic_args[0]) {
				return none
			}
			return generic_args[0]
		}
	}
	return none
}

fn (t &Transformer) current_specialized_receiver_args(receiver_type string) ?[]string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 || !t.generic_args_have_placeholders(args) {
		return none
	}
	current_receiver := t.current_fn_receiver_type()
	if current_receiver.len == 0
		|| !current_receiver_matches_open_generic_base(current_receiver, base) {
		return none
	}
	if recorded := t.recorded_generic_specialization_args(current_receiver) {
		if t.generic_args_have_placeholders(recorded) {
			return none
		}
		return recorded
	}
	_, current_args, current_ok := generic_app_parts(current_receiver)
	if current_ok && current_args.len == args.len {
		canonical_args := t.canonical_generic_specialization_args(current_args)
		if t.generic_args_have_placeholders(canonical_args) {
			return none
		}
		return canonical_args
	}
	return none
}

fn (t &Transformer) current_fn_receiver_type() string {
	name := t.cur_fn_name.trim_space()
	if name.len == 0 {
		return ''
	}
	if name.contains('.') {
		return name.all_before_last('.')
	}
	if name.contains('__') {
		return name.all_before_last('__')
	}
	return ''
}

fn current_receiver_matches_open_generic_base(receiver string, base string) bool {
	receiver_base, _, receiver_is_generic := generic_app_parts(receiver)
	clean_receiver := if receiver_is_generic { receiver_base } else { receiver }
	short_base := base.all_after_last('.')
	short_receiver := clean_receiver.all_after_last('.')
	return short_receiver == short_base || short_receiver.starts_with('${short_base}_')
		|| c_name(short_receiver).starts_with('${c_name(short_base)}_')
		|| short_receiver.contains('__${short_base}_')
}

fn (mut t Transformer) lower_map_str(map_expr flat.NodeId, map_type string) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || value_type.len == 0 {
		return map_expr
	}
	key_kind := t.map_str_kind_for_type(key_type)
	value_kind := t.map_str_kind_for_type(value_type)
	if key_kind == 0 || value_kind == 0 {
		key_has_conversion := key_kind != 0 || t.map_str_type_has_transform_conversion(key_type)
		value_has_conversion := value_kind != 0
			|| t.map_str_type_has_transform_conversion(value_type)
		if key_has_conversion && value_has_conversion {
			return t.lower_typed_map_str(map_expr, map_type, key_type, value_type)
		}
	}
	lowered := t.transform_expr_for_type(map_expr, map_type)
	return t.make_call_typed('v3_map_str', arr4(lowered, t.make_int_literal(key_kind),
		t.make_int_literal(value_kind),
		t.make_int_literal(t.map_str_fixed_len_for_type(value_type))), 'string')
}

// lower_typed_map_str handles map interpolation when v3_map_str cannot stringify a typed
// key/value directly. It mirrors lower_array_str by recursing through wrap_string_conversion.
fn (mut t Transformer) lower_typed_map_str(map_expr flat.NodeId, map_type string, key_type string, value_type string) flat.NodeId {
	src := t.a.nodes[int(map_expr)]
	base := t.stable_expr_for_reuse(map_expr)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('map_str')
	keys_name := t.new_temp('map_str_keys')
	idx_name := t.new_temp('map_str_idx')
	key_name := t.new_temp('map_str_key')
	zero_name := t.new_temp('map_str_zero')
	value_name := t.new_temp('map_str_value')
	key_kind := t.map_str_kind_for_type(key_type)
	value_kind := t.map_str_kind_for_type(value_type)
	key_storage_type := t.map_key_storage_type(key_type)
	keys_type := '[]${key_storage_type}'
	keys_call := t.make_call_typed('map__keys', arr1(t.runtime_addr(base, map_type)), keys_type)
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('{'), 'string')
	prefix << t.make_decl_assign_typed(keys_name, keys_call, keys_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(keys_name),
		'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	key_expr := t.array_get_value(t.make_ident(keys_name), t.make_ident(idx_name), key_storage_type)
	key_decl := t.make_decl_assign_typed(key_name, key_expr, key_storage_type)
	zero_decl := t.make_decl_assign_typed(zero_name, t.zero_value_for_type(value_type), value_type)
	value_expr := t.make_map_get_expr(base, map_type, key_name, zero_name, value_type)
	value_decl := t.make_decl_assign_typed(value_name, value_expr, value_type)
	mut loop_body := []flat.NodeId{}
	loop_body << key_decl
	loop_body << zero_decl
	loop_body << value_decl
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block(arr1(sep_stmt)), t.make_empty())
	key_str := t.map_str_loop_piece(key_name, key_type, key_kind, 0)
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, key_str)
	loop_body << t.append_string(result_name, t.make_string_literal(': '))
	value_str := t.map_str_loop_piece(value_name, value_type, value_kind,
		t.map_str_fixed_len_for_type(value_type))
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, value_str)
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal('}'))
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) map_str_loop_piece(name string, typ string, kind int, fixed_len int) flat.NodeId {
	if kind != 0 {
		return t.make_call_typed('v3_map_str_piece', arr4(t.make_prefix(.amp, t.make_ident(name)),
			t.make_int_literal(kind), t.make_sizeof_type(typ), t.make_int_literal(fixed_len)),
			'string')
	}
	t.set_var_type(name, typ)
	piece := t.wrap_string_conversion(t.make_ident(name), typ)
	t.unset_var_type(name)
	return piece
}

fn (t &Transformer) map_str_type_has_transform_conversion(typ string) bool {
	mut clean := t.normalize_type_alias(typ).trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		return true
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	if t.is_optional_type_name(clean) {
		return true
	}
	if clean in ['string', 'rune', 'bool', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'u8', 'byte',
		'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal', 'voidptr',
		'byteptr', 'charptr', 'IError'] {
		return true
	}
	if clean in t.enum_types || clean in t.structs || clean in t.sum_types {
		return true
	}
	if _ := t.generic_struct_info_for_stringify(clean) {
		return true
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.enum_types || qname in t.structs || qname in t.sum_types {
			return true
		}
	}
	return t.is_fixed_array_type(clean) || clean.starts_with('[]') || clean.starts_with('map[')
}

fn (t &Transformer) map_str_kind_for_type(typ string) int {
	mut clean := t.normalize_type_alias(typ).trim_space()
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	match clean {
		'string' {
			return 1
		}
		'rune' {
			return 4
		}
		'isize', 'char', 'i8', 'i16', 'i32', 'i64', 'int' {
			return 2
		}
		'usize', 'u8', 'byte', 'u16', 'u32', 'u64' {
			return 3
		}
		'f32', 'f64' {
			return 5
		}
		'bool' {
			return 7
		}
		else {
			if clean.starts_with('[]') && clean[2..] in ['f32', 'f64'] {
				return 6
			}
			if transform_type_text_is_fixed_array(clean) {
				elem := t.normalize_type_alias(fixed_array_elem_type(clean))
				if elem == 'f32' {
					return 9
				}
				if elem == 'f64' {
					return 6
				}
			}
			return 0
		}
	}
}

fn (t &Transformer) map_str_fixed_len_for_type(typ string) int {
	clean := t.normalize_type_alias(typ).trim_space()
	if transform_type_text_is_fixed_array(clean) {
		return fixed_array_len(clean)
	}
	return 0
}

// wrap_optional_string_conversion transforms wrap optional string conversion data for transform.
fn (mut t Transformer) wrap_optional_string_conversion(expr flat.NodeId, typ string) flat.NodeId {
	opt_type := t.qualify_optional_type(typ)
	mut value_type := t.optional_base_type(opt_type)
	if value_type.len == 0 || value_type == 'void' {
		value_type = 'int'
	}
	opt_name := t.new_temp('opt_str')
	res_name := t.new_temp('opt_str_text')
	t.pending_stmts << t.make_decl_assign_typed(opt_name, expr, opt_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('Option(none)'),
		'string')
	value := t.make_selector(t.make_ident(opt_name), 'value', value_type)
	mut value_str := t.wrap_string_conversion(value, value_type)
	if value_type == 'string' {
		value_str = t.string_plus(t.string_plus(t.make_string_literal("'"), value_str),
			t.make_string_literal("'"))
	}
	some_str := t.string_plus(t.string_plus(t.make_string_literal('Option('), value_str),
		t.make_string_literal(')'))
	assign_some := t.make_assign(t.make_ident(res_name), some_str)
	t.pending_stmts << t.make_if(t.make_selector(t.make_ident(opt_name), 'ok', 'bool'),
		t.make_block(arr1(assign_some)), t.make_empty())
	return t.make_ident(res_name)
}

// string_plus supports string plus handling for Transformer.
fn (mut t Transformer) string_plus(left flat.NodeId, right flat.NodeId) flat.NodeId {
	return t.make_call_typed('string__plus', arr2(left, right), 'string')
}

// is_flag_enum_type reports whether is flag enum type applies in transform.
fn (t &Transformer) is_flag_enum_type(typ string) bool {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.len == 0 {
		return false
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if parsed is types.Enum {
			return parsed.is_flag
		}
	}
	return false
}

fn (t &Transformer) flag_enum_mask_for_type(typ string) int {
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	members := t.comptime_enum_members(clean)
	mut mask := 0
	for member in members {
		mask |= int(member.value)
	}
	return mask
}

fn (t &Transformer) flag_enum_has_backing_type(typ string) bool {
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	normalized := t.normalize_type_alias(clean).trim_space()
	for candidate in [normalized, clean] {
		if candidate.len == 0 {
			continue
		}
		if _ := t.enum_backing_types[candidate] {
			return true
		}
		if !candidate.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			if _ := t.enum_backing_types['${t.cur_module}.${candidate}'] {
				return true
			}
		}
	}
	return false
}

// is_runtime_array_flags_selector reports is_runtime_array_flags_selector logic in transform.
fn (t &Transformer) is_runtime_array_flags_selector(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.value != 'flags' || node.children_count == 0 {
		return false
	}
	owner_id := t.a.child(&node, 0)
	owner_type := t.node_type(owner_id).trim_left('&')
	return owner_type.starts_with('[]') || owner_type == 'strings.Builder'
}

// try_lower_flag_enum_stmt supports try lower flag enum stmt handling for Transformer.
fn (mut t Transformer) try_lower_flag_enum_stmt(call_id flat.NodeId) ?flat.NodeId {
	if int(call_id) < 0 {
		return none
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 1 {
		return none
	}
	fn_id := t.a.children[call.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['set', 'clear', 'toggle', 'set_all', 'clear_all'] {
		return none
	}
	if fn_node.value in ['set', 'clear', 'toggle'] && call.children_count < 2 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if t.is_runtime_array_flags_selector(base_id) {
		return none
	}
	base_type := t.node_type(base_id)
	if !t.is_flag_enum_type(base_type) {
		return none
	}
	if fn_node.value == 'set_all' && t.flag_enum_has_backing_type(base_type) {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	match fn_node.value {
		'set' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, arg, .pipe_assign)
		}
		'clear' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, t.make_prefix(.bit_not, arg), .amp_assign)
		}
		'toggle' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, arg, .xor_assign)
		}
		'set_all' {
			mask := t.flag_enum_mask_for_type(base_type)
			return t.make_assign(base, t.make_int_literal_typed(mask.str(), base_type))
		}
		'clear_all' {
			return t.make_assign(base, t.make_int_literal_typed('0', base_type))
		}
		else {}
	}

	return none
}

// try_lower_flag_enum_call supports try lower flag enum call handling for Transformer.
fn (mut t Transformer) try_lower_flag_enum_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 1 {
		fn_id := t.a.children[node.children_start]
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value != 'zero' {
			return none
		}
		base_id := t.a.children[fn_node.children_start]
		base_node := t.a.nodes[int(base_id)]
		if base_node.kind != .ident || !t.is_flag_enum_type(base_node.value) {
			return none
		}
		return t.make_cast(base_node.value, t.make_int_literal(0), base_node.value)
	}
	if node.children_count < 2 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value !in ['has', 'all'] {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if t.is_runtime_array_flags_selector(base_id) {
		return none
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if !t.is_flag_enum_type(base_type) {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	arg_id := t.a.children[node.children_start + 1]
	arg := t.transform_expr(arg_id)
	masked := t.make_infix(.amp, base, arg)
	if fn_node.value == 'has' {
		return t.make_infix(.ne, masked, t.make_int_literal(0))
	}
	arg_copy := t.transform_expr(arg_id)
	return t.make_infix(.eq, masked, arg_copy)
}

// try_lower_struct_clone_method_call lowers `x.clone()` on a struct / sum-type value to a
// plain copy of the receiver. Rust's `#[derive(Clone)]` maps to `implements IClone` in the
// ownership translation, whose `clone()` is compiler-provided; V aggregates are value types,
// so the copy is produced simply by evaluating the receiver (it is copied when assigned or
// passed by value). Collection/string clones are lowered earlier, and a user-defined clone()
// method is handled by try_lower_receiver_method_call before this runs.
fn (mut t Transformer) try_lower_struct_clone_method_call(_call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'clone' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	mut raw_base_type := t.node_type(base_id)
	if raw_base_type.len == 0 {
		raw_base_type = t.lvalue_type(base_id)
	}
	mut base_type := raw_base_type
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') || base_type == 'string'
		|| t.is_fixed_array_type(base_type) {
		return none
	}
	if isnil(t.tc) || !t.tc.named_type_implements_marker(base_type, 'IClone') {
		return none
	}
	// A user-defined clone() would have been lowered already; only supply the default copy.
	if t.resolve_receiver_method_name(base_id, 'clone').len > 0 {
		return none
	}
	mut receiver := t.transform_expr(base_id)
	if raw_base_type.starts_with('&') {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), base_type)
	}
	return receiver
}

// try_lower_array_method_call supports try lower array method call handling for Transformer.
fn (mut t Transformer) try_lower_array_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if fn_node.value == 'str' {
		mut raw_base_types := []string{}
		for candidate in [t.raw_var_type_for_expr(base_id) or { '' },
			t.node_type(base_id), t.lvalue_type(base_id)] {
			clean := candidate.trim_left('&')
			if clean.len > 0 && clean !in raw_base_types {
				raw_base_types << clean
			}
			if !isnil(t.tc) {
				resolved := t.tc.resolve_imported_type_text_in_file(clean, t.cur_file)
				if resolved.len > 0 && resolved !in raw_base_types {
					raw_base_types << resolved
				}
			}
		}
		for raw_base_type in raw_base_types {
			if method_name := t.resolve_receiver_method_for_type(raw_base_type, 'str') {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used_name(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
			method_name := '${raw_base_type}.str'
			if t.is_known_fn_name(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used_name(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
		}
	}
	array_builtin_method := t.array_builtin_method_name(fn_node.value) or { '' }
	if fn_node.value !in ['clone', 'reverse', 'contains', 'index', 'last_index', 'join', 'any',
		'all', 'count', 'equals', 'prepend', 'insert', 'push_many', 'str', 'to_fixed_size', 'wait'] {
		if fn_node.value !in ['filter', 'map', 'sort', 'sorted', 'sort_with_compare', 'sorted_with_compare']
			&& array_builtin_method.len == 0 {
			mut early_base_type := t.node_type(base_id)
			if early_base_type.len == 0 {
				early_base_type = t.lvalue_type(base_id)
			}
			early_clean_type := t.normalize_type_alias(if early_base_type.starts_with('&') {
				early_base_type[1..]
			} else {
				early_base_type
			})
			if early_clean_type.starts_with('[]') || t.is_fixed_array_type(early_clean_type) {
				if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id,
					'')
				{
					return exact_call
				}
			}
			return none
		}
	}
	mut base_type := t.node_type(base_id)
	mut smartcast_container := false
	if sc := t.find_smartcast(t.expr_key(base_id)) {
		variant_type := t.resolve_variant(sc.sum_type_name, sc.variant_name)
		if variant_type.starts_with('[]') || t.is_fixed_array_type(variant_type) {
			base_type = variant_type
			smartcast_container = true
		}
	}
	base_node := t.a.nodes[int(base_id)]
	if (array_type_has_generic_placeholder(base_type) || base_type.contains('unknown'))
		&& base_node.kind == .call {
		concrete_base_type := t.concrete_generic_call_return_type(base_id, base_node)
		if concrete_base_type.starts_with('[]') {
			base_type = concrete_base_type
		}
	}
	if (!base_type.starts_with('[]') && !t.is_fixed_array_type(base_type)) || base_type == 'array' {
		lvalue_base_type := t.lvalue_type(base_id)
		if lvalue_base_type.starts_with('[]') || t.is_fixed_array_type(lvalue_base_type) {
			base_type = lvalue_base_type
		}
	}
	base_type = t.normalize_type_alias(base_type)
	if !base_type.starts_with('[]') && !t.is_fixed_array_type(base_type) {
		if base_node.kind in [.call, .selector, .as_expr] {
			new_base := t.transform_expr(base_id)
			new_base_type := t.node_type(new_base)
			if new_base_type.starts_with('[]') || t.is_fixed_array_type(new_base_type) {
				selector := t.make_selector(new_base, fn_node.value, '')
				mut children := []flat.NodeId{cap: int(node.children_count)}
				children << selector
				for i in 1 .. node.children_count {
					children << t.a.child(&node, i)
				}
				start := t.a.children.len
				for child in children {
					t.a.children << child
				}
				new_node := flat.Node{
					kind:           .call
					children_start: start
					children_count: node.children_count
					pos:            node.pos
					typ:            node.typ
				}
				return t.try_lower_array_method_call(call_id, new_node)
			}
		}
	}
	if fn_node.value == 'str' && base_node.kind == .call {
		new_base := t.transform_expr(base_id)
		mut new_base_type := t.node_type(new_base)
		if recorded_elem_type := t.recorded_array_call_elem_type(base_id) {
			new_base_type = '[]${recorded_elem_type}'
			t.set_node_typ(int(new_base), new_base_type)
		}
		if new_base_type.starts_with('[]') {
			if stringify_type_has_generic_placeholder(new_base_type) {
				return none
			}
			return t.lower_array_str(new_base, new_base_type)
		}
		if new_base_type.starts_with('map[') {
			if stringify_type_has_generic_placeholder(new_base_type) {
				return none
			}
			return t.lower_map_str(new_base, new_base_type)
		}
	}
	clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if fn_node.value == 'str' && t.is_builder_receiver(base_id, base_type) {
		return none
	}
	if t.is_fixed_array_type(clean_base_type) && fn_node.value == 'pointers'
		&& array_builtin_method.len > 0 {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && method_name != array_builtin_method
			&& t.call_resolved_to_method(call_id, method_name)
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type := t.receiver_method_return_type(method_name, node.typ)
			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type)
		}
		if dynamic_method := t.resolve_fixed_array_dynamic_receiver_method(clean_base_type,
			fn_node.value)
		{
			return t.lower_fixed_array_dynamic_receiver_method_call(node, base_id, clean_base_type,
				dynamic_method)
		}
		args := t.transform_receiver_method_args(node, base_id, array_builtin_method)
		ret_type := t.receiver_method_return_type(array_builtin_method, node.typ)
		return t.make_call_typed(array_builtin_method, args, ret_type)
	}
	if t.is_fixed_array_type(clean_base_type) {
		elem_type := fixed_array_elem_type(clean_base_type)
		array_type := '[]${elem_type}'
		tmp_name := t.new_temp('fixed_arr')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.fixed_array_value_to_array(base_id,
			clean_base_type, array_type), array_type)
		selector := t.make_selector(t.make_ident(tmp_name), fn_node.value, '')
		if fn_node.value == 'wait' {
			wait_type := fixed_thread_array_wait_return_type(elem_type)
			if wait_type.len > 0 {
				return t.make_call_expr_typed(selector, []flat.NodeId{}, wait_type)
			}
		}
		mut children := []flat.NodeId{cap: int(node.children_count)}
		children << selector
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
		start := t.a.children.len
		for child in children {
			t.a.children << child
		}
		new_node := flat.Node{
			kind:           .call
			children_start: start
			children_count: node.children_count
			pos:            node.pos
			typ:            node.typ
		}
		return t.try_lower_array_method_call(call_id, new_node)
	}
	if !clean_base_type.starts_with('[]') {
		return none
	}
	if !(smartcast_container && fn_node.value == 'str') {
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id,
			array_builtin_method)
		{
			return exact_call
		}
	}
	if fn_node.value == 'str' && stringify_type_has_generic_placeholder(clean_base_type) {
		return none
	}
	elem_type := clean_base_type[2..]
	if fn_node.value == 'prepend' {
		return t.lower_array_prepend_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'insert' {
		return t.lower_array_insert_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'push_many' {
		return t.lower_array_push_many_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'contains' {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && (t.call_resolved_to_method(call_id, method_name)
			|| transform_is_exact_array_receiver_method(method_name))
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type := t.receiver_method_return_type(method_name, node.typ)
			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type)
		}
	}
	match fn_node.value {
		'filter' {
			return t.lower_array_filter_call(node, fn_node, clean_base_type)
		}
		'map' {
			return t.lower_array_map_call(node, fn_node, clean_base_type)
		}
		'sort' {
			return t.lower_array_sort_call(node, fn_node, base_type)
		}
		'sorted' {
			return t.lower_array_sorted_call(node, fn_node, clean_base_type)
		}
		'sort_with_compare' {
			return t.lower_array_sort_with_compare_call(node, fn_node, base_type)
		}
		'sorted_with_compare' {
			return t.lower_array_sorted_with_compare_call(node, fn_node, clean_base_type)
		}
		'any', 'all' {
			return t.lower_array_any_all_call(node, fn_node, clean_base_type, fn_node.value)
		}
		'count' {
			return t.lower_array_count_call(node, fn_node, clean_base_type)
		}
		'str' {
			receiver := if smartcast_container {
				t.make_plain_expr_for_smartcast(base_id)
			} else {
				t.transform_expr(base_id)
			}
			return t.wrap_string_conversion(receiver, clean_base_type)
		}
		'to_fixed_size' {
			if base_node.kind != .array_literal {
				return none
			}
			fixed_type := if t.is_fixed_array_type(node.typ) {
				node.typ
			} else {
				'[${base_node.children_count}]${elem_type}'
			}
			return t.transform_fixed_array_literal_for_type(base_id, base_node, fixed_type)
		}
		'equals' {
			if node.children_count < 2 {
				return none
			}
			method_name := t.resolve_receiver_method_name(base_id, 'equals')
			if method_name.len > 0 && t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, 'bool')
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			if t.array_elem_needs_element_eq(elem_type) {
				return t.make_array_elementwise_eq_call(receiver, arg, elem_type, clean_base_type,
					clean_base_type, node)
			}
			if elem_type.starts_with('[]') {
				return t.make_call_typed('array_eq_array', arr3(receiver, arg,
					t.make_int_literal(array_nested_eq_depth(clean_base_type))), 'bool')
			}
			if elem_type == 'string' {
				return t.make_call_typed('array_eq_string', arr2(receiver, arg), 'bool')
			}
			return t.make_call_typed('array_eq_raw', arr3(receiver, arg,
				t.make_sizeof_type(elem_type)), 'bool')
		}
		else {}
	}

	match fn_node.value {
		'clone' {
			method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value,
				clean_base_type)
			if method_name.len > 0 && t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
			return t.make_array_clone_call(base_id, base_type)
		}
		'reverse' {
			mut receiver := t.transform_expr(base_id)
			if base_type.starts_with('&') {
				receiver = t.make_prefix(.mul, receiver)
				t.set_node_typ(int(receiver), clean_base_type)
			}
			return t.make_call_typed('array__reverse', arr1(receiver), clean_base_type)
		}
		'contains' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_membership_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(arg_id)
			fn_name := if elem_type == 'string' {
				'array_contains_string'
			} else {
				'array_contains_int'
			}
			return t.make_call_typed(fn_name, arr2(receiver, arg), 'bool')
		}
		'index' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_index_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(arg_id)
			fn_name := if elem_type == 'string' { 'array_index_string' } else { 'array_index_int' }
			return t.make_call_typed(fn_name, arr2(receiver, arg), 'int')
		}
		'last_index' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_last_index_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			return none
		}
		'join' {
			if node.children_count < 2 {
				return none
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			return t.make_call_typed('Array_string__join', arr2(receiver, arg), 'string')
		}
		else {
			if array_method_stays_in_cgen(fn_node.value) {
				method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value,
					clean_base_type)
				if method_name.len > 0 && method_name != array_builtin_method
					&& t.call_resolved_to_method(call_id, method_name)
					&& !t.receiver_method_name_is_open_generic(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type := t.receiver_method_return_type(method_name, node.typ)
					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type)
				}
				if t.validating_generic_spec
					&& !t.validate_cgen_array_method_args(node, base_id, clean_base_type, fn_node.value) {
					return t.make_empty()
				}
				if array_method_stays_in_cgen_needs_runtime_mark(fn_node.value) {
					t.mark_fn_used('array__${fn_node.value}')
				}
				return none
			}
			if array_builtin_method.len > 0 {
				method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value,
					clean_base_type)
				if method_name.len > 0 && method_name != array_builtin_method
					&& !t.receiver_method_name_is_open_generic(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type := t.receiver_method_return_type(method_name, node.typ)
					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type)
				}
				args := t.transform_receiver_method_args(node, base_id, array_builtin_method)
				ret_type := t.receiver_method_return_type(array_builtin_method, node.typ)
				return t.make_call_typed(array_builtin_method, args, ret_type)
			}
			return none
		}
	}
}

fn (t &Transformer) array_builtin_method_name(method string) ?string {
	method_name := 'array.${method}'
	if t.is_known_fn_name(method_name) {
		return method_name
	}
	return none
}

fn array_method_stays_in_cgen(method string) bool {
	return match method.len {
		3 { method == 'pop' || method == 'str' }
		4 { method == 'last' || method == 'trim' || method == 'free' || method == 'wait' }
		5 { method == 'first' || method == 'clear' }
		6 { method == 'repeat' || method == 'delete' }
		7 { method == 'bytestr' }
		8 { method == 'pop_left' }
		10 { method == 'ensure_cap' }
		11 { method == 'delete_last' }
		15 { method == 'repeat_to_depth' }
		else { false }
	}
}

fn array_method_stays_in_cgen_needs_runtime_mark(method string) bool {
	return match method.len {
		3 { method == 'pop' }
		4 { method == 'trim' }
		5 { method == 'clear' }
		else { false }
	}
}

fn (mut t Transformer) validate_cgen_array_method_args(node flat.Node, base_id flat.NodeId, base_type string, method string) bool {
	base_node := t.a.nodes[int(base_id)]
	base_name := if base_node.kind == .ident && base_node.value.len > 0 {
		base_node.value
	} else {
		base_type
	}
	display_name := '${base_name}.${method}'
	if method == 'bytestr' && base_type !in ['[]u8', '[]byte'] {
		t.record_monomorph_error('unknown function `${display_name}`')
		return false
	}
	if method == 'wait' {
		elem_type := if base_type.starts_with('[]') { base_type[2..].trim_space() } else { '' }
		if elem_type != 'thread' && !elem_type.starts_with('thread ') {
			t.record_monomorph_error('unknown function `${display_name}`')
			return false
		}
	}
	mut expected_types := []string{}
	mut has_signature := false
	if builtin_method := t.array_builtin_method_name(method) {
		params := t.call_param_types(builtin_method)
		if params.len > 0 {
			param_offset := t.receiver_method_param_offset(base_id, node, params, builtin_method)
			for i in param_offset .. params.len {
				expected_types << t.normalize_type_alias(params[i].name())
			}
			has_signature = true
		}
	}
	if !has_signature {
		expected_types = match method {
			'trim', 'repeat', 'delete', 'ensure_cap' { ['int'] }
			'repeat_to_depth' { ['int', 'int'] }
			else { []string{} }
		}
	}
	actual_count := int(node.children_count) - 1
	if actual_count != expected_types.len {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${expected_types.len}, got ${actual_count}')
		return false
	}
	mut valid := true
	for i, expected in expected_types {
		arg_id := t.a.child(&node, i + 1)
		mut actual := t.resolve_expr_type(arg_id)
		if actual.len == 0 {
			actual = t.node_type(arg_id)
		}
		if actual.len == 0 {
			actual = t.reliable_stringify_type(arg_id)
		}
		actual = t.normalize_type_alias(actual)
		if actual != expected {
			t.record_monomorph_error('cannot use `${actual}` as argument ${i + 1} to `${display_name}`; expected `${expected}`')
			valid = false
		}
	}
	return valid
}

fn thread_array_wait_return_type(elem_type string) ?string {
	name := elem_type.trim_space()
	if name == 'thread' {
		return 'void'
	}
	if !name.starts_with('thread ') {
		return none
	}
	payload := name[7..].trim_space()
	if payload == '?' || payload == '!' {
		return '${payload}void'
	}
	if payload.starts_with('?') || payload.starts_with('!') {
		value_type := payload[1..].trim_space()
		if value_type.len == 0 || value_type == 'void' {
			return '${payload[0].ascii_str()}void'
		}
		return '${payload[0].ascii_str()}[]${value_type}'
	}
	return '[]${payload}'
}

fn fixed_thread_array_wait_return_type(elem_type string) string {
	return thread_array_wait_return_type(elem_type) or { '' }
}

fn transform_is_exact_array_receiver_method(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver := name.all_before_last('.')
	return receiver.starts_with('[]') || receiver.contains('.[]')
}

// try_lower_map_method_call supports try lower map method call handling for Transformer.
fn (mut t Transformer) try_lower_map_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	is_lowered_map_method := map_method_is_lowered_by_transform(fn_node.value)
	if fn_node.kind != .selector || fn_node.children_count == 0 || !is_lowered_map_method {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.checker_node_type(base_id)
	}
	clean_type := t.clean_map_type(base_type)
	if !clean_type.starts_with('map[') {
		return none
	}
	builtin_method := 'map.${fn_node.value}'
	method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
	if method_name.len > 0 && method_name != builtin_method
		&& t.call_resolved_to_method(call_id, method_name)
		&& !t.receiver_method_name_is_open_generic(method_name) {
		args := t.transform_receiver_method_args(node, base_id, method_name)
		ret_type := t.receiver_method_return_type(method_name, node.typ)
		t.mark_fn_used(method_name)
		return t.make_call_typed(method_name, args, ret_type)
	}
	base := t.stable_expr_for_reuse(base_id)
	if map_method_needs_runtime_addr_only(fn_node.value) {
		t.mark_fn_used('map__${fn_node.value}')
		return t.make_call_typed('map__${fn_node.value}', arr1(t.runtime_addr(base, base_type)),
			'void')
	}
	if fn_node.value == 'move' {
		t.mark_fn_used('map__move')
		return t.make_call_typed('map__move', arr1(t.runtime_addr(base, base_type)), clean_type)
	}
	if fn_node.value == 'reserve' {
		if node.children_count < 2 {
			return none
		}
		t.mark_fn_used('map__reserve')
		capacity := t.transform_expr_for_type(t.a.child(&node, 1), 'u32')
		return t.make_call_typed('map__reserve', arr2(t.runtime_addr(base, base_type), capacity),
			'void')
	}
	if fn_node.value == 'delete' {
		if node.children_count < 2 {
			return none
		}
		key_type := t.map_key_type(clean_type)
		if key_type.len == 0 {
			return none
		}
		t.mark_fn_used('map__delete')
		key_name := t.new_temp('map_key')
		key_storage_type := t.map_key_storage_type(key_type)
		t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(t.a.child(&node, 1),
			key_type), key_storage_type)
		return t.make_call_typed('map__delete', arr2(t.runtime_addr(base, base_type), t.make_prefix(.amp,
			t.make_ident(key_name))), 'void')
	}
	elem_type := if fn_node.value == 'keys' {
		t.map_key_type(clean_type)
	} else {
		t.map_value_type(clean_type)
	}
	if elem_type.len == 0 {
		return none
	}
	t.mark_fn_used('map__${fn_node.value}')
	return t.make_call_typed('map__${fn_node.value}', arr1(t.runtime_addr(base, base_type)),
		'[]${elem_type}')
}

fn map_method_is_lowered_by_transform(method string) bool {
	return match method.len {
		4 { method == 'keys' || method == 'free' || method == 'move' }
		5 { method == 'clear' }
		6 { method == 'values' || method == 'delete' }
		7 { method == 'reserve' }
		else { false }
	}
}

fn map_method_needs_runtime_addr_only(method string) bool {
	return match method.len {
		4 { method == 'free' }
		5 { method == 'clear' }
		else { false }
	}
}

// try_lower_channel_method_call lowers channel source methods to runtime calls before
// backend selection.
fn (mut t Transformer) try_lower_channel_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['close', 'try_push', 'try_pop'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(call_id) {
			channel_method := 'chan.${fn_node.value}'
			runtime_method := 'sync__Channel__${fn_node.value}'
			if resolved_method != channel_method && resolved_method != runtime_method {
				if fn_node.value == 'close' {
					if exact_call := t.lower_checker_selected_receiver_method(call_id, node,
						base_id, 'chan.close')
					{
						return exact_call
					}
				}
				return none
			}
		}
	}
	mut clean_type := base_type
	mut ptr_depth := 0
	for clean_type.starts_with('&') {
		ptr_depth++
		clean_type = clean_type[1..].trim_space()
	}
	if clean_type != 'chan' && !clean_type.starts_with('chan ') {
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id,
			'chan.close')
		{
			return exact_call
		}
		return none
	}
	if fn_node.value in ['try_push', 'try_pop'] {
		t.mark_fn_used('sync__Channel__${fn_node.value}')
		return none
	}
	return t.lower_runtime_channel_close(base_id, node, ptr_depth)
}

fn (mut t Transformer) lower_runtime_channel_close(base_id flat.NodeId, node flat.Node, ptr_depth int) flat.NodeId {
	t.mark_fn_used('sync__Channel__close')
	mut err_values := []flat.NodeId{}
	if node.children_count > 1 {
		for i in 1 .. node.children_count {
			err_values << t.a.child(&node, i)
		}
	}
	errs := if err_values.len > 0 {
		lit := t.make_array_literal_typed(err_values, '[]IError')
		t.transform_array_literal(lit, t.a.nodes[int(lit)])
	} else {
		t.make_array_new_call('IError', t.make_int_literal(0), t.make_int_literal(0))
	}
	fn_expr := t.make_selector(t.make_ident('C'), 'sync__Channel__close', '')
	mut receiver := t.transform_expr(base_id)
	for _ in 0 .. ptr_depth {
		receiver = t.make_prefix(.mul, receiver)
	}
	if ptr_depth == 0 {
		receiver = t.make_cast('&sync.Channel', receiver, '&sync.Channel')
	} else {
		t.set_node_typ(int(receiver), '&sync.Channel')
	}
	return t.make_call_expr_typed(fn_expr, arr2(receiver, errs), 'void')
}

// try_lower_move_method_call supports try lower move method call handling for Transformer.
fn (mut t Transformer) try_lower_move_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 1 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value != 'move' {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	clean_array_type := t.membership_container_type(base_type)
	if clean_array_type.starts_with('[]') {
		if !isnil(t.tc) {
			if resolved := t.tc.resolved_call_name(call_id) {
				if !is_builtin_collection_resolved_call(resolved) && t.is_known_fn_name(resolved) {
					return none
				}
			}
		}
		return t.transform_expr(base_id)
	}
	return none
}

// lift_fn_literal supports lift fn literal handling for Transformer.
fn (mut t Transformer) lift_fn_literal(_id flat.NodeId, node flat.Node) flat.NodeId {
	name := t.new_fn_literal_name()
	mut param_types := []types.Type{}
	mut param_ids := []flat.NodeId{}
	mut param_names := []string{}
	mut capture_names := []string{}
	mut capture_types := map[string]string{}
	mut capture_globals := map[string]string{}
	mut capture_mut := map[string]bool{}
	mut capture_by_ref := map[string]bool{}
	mut body_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .param {
			param_ids << child_id
			if child.value.len > 0 {
				param_names << child.value
			}
			if !isnil(t.tc) {
				param_types << t.tc.parse_type(child.typ)
			}
		} else if child.kind == .ident {
			if child.value.len > 0 && child.value !in capture_names {
				mut capture_type := t.var_type(child.value)
				if capture_type.len == 0 {
					capture_type = t.node_type(child_id)
				}
				if capture_type.len == 0 && !isnil(t.tc) {
					if typ := t.tc.expr_type(child_id) {
						capture_type = typ.name()
					}
				}
				if capture_type.len == 0 || capture_type == 'unknown' {
					capture_type = 'int'
				}
				capture_names << child.value
				is_mut_capture := child.is_mut
				is_ref_capture := is_mut_capture && !capture_type.starts_with('&')
				capture_types[child.value] = if is_ref_capture {
					'&${capture_type}'
				} else {
					capture_type
				}
				capture_globals[child.value] = '${name}_${child.value}'
				capture_mut[child.value] = is_mut_capture
				capture_by_ref[child.value] = is_ref_capture
			}
		} else {
			body_ids << child_id
		}
	}
	ret_type := if node.typ.len > 0 { node.typ } else { 'void' }
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	saved_vars := t.var_types.clone()
	saved_mut_param_values := t.mut_param_values.clone()
	t.cur_fn_name = name
	t.cur_fn_ret_type = ret_type
	t.reset_var_types()
	for param_id in param_ids {
		param := t.a.nodes[int(param_id)]
		if param.value.len > 0 && param.typ.len > 0 {
			t.set_var_type(param.value, param.typ)
			if param.is_mut || param.op == .amp || param.typ.starts_with('mut ') {
				t.mut_param_values[param.value] = true
			}
		}
	}
	mut lifted_body := []flat.NodeId{cap: capture_names.len + body_ids.len}
	mut saved_capture_pointer_flags := map[string]bool{}
	mut saved_capture_pointer_rvalue_flags := map[string]bool{}
	for capture_name in capture_names {
		if capture_name in param_names {
			continue
		}
		capture_type := capture_types[capture_name] or { continue }
		capture_global := capture_globals[capture_name] or { continue }
		t.add_fn_literal_capture_global(capture_global, capture_type)
		capture_rhs := if capture_by_ref[capture_name] or { false } {
			t.make_prefix(.amp, t.make_ident(capture_name))
		} else {
			t.make_ident(capture_name)
		}
		t.pending_stmts << t.make_assign(t.make_ident(capture_global), capture_rhs)
		t.set_var_type(capture_name, capture_type)
		// Only captures that were rewritten into a synthetic `&T` pointer-value local
		// (`capture_by_ref`) need pointer-value lvalue/rvalue lowering. A mut capture
		// whose original type is already a pointer (`&S`) stays a genuine `&S` local:
		// dereferencing its rvalue uses would corrupt `&S` -> `S` and break calls that
		// expect the pointer (e.g. `takes_ptr(p)`), and its assignments must not become
		// `*p = ...`.
		if capture_by_ref[capture_name] or { false } {
			saved_capture_pointer_flags[capture_name] = t.pointer_value_lvalues[capture_name] or {
				false
			}
			saved_capture_pointer_rvalue_flags[capture_name] = t.pointer_value_rvalues[capture_name] or {
				false
			}
			t.pointer_value_lvalues[capture_name] = true
			t.pointer_value_rvalues[capture_name] = true
		}
		lifted_body << t.make_decl_assign_typed(capture_name, t.make_ident(capture_global),
			capture_type)
	}
	for body_id in body_ids {
		lifted_body << body_id
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_body := t.transform_stmts(lifted_body)
	t.pending_stmts = outer_pending
	for capture_name in capture_names {
		if capture_by_ref[capture_name] or { false } {
			if saved_capture_pointer_flags[capture_name] or { false } {
				t.pointer_value_lvalues[capture_name] = true
			} else {
				t.pointer_value_lvalues.delete(capture_name)
			}
			if saved_capture_pointer_rvalue_flags[capture_name] or { false } {
				t.pointer_value_rvalues[capture_name] = true
			} else {
				t.pointer_value_rvalues.delete(capture_name)
			}
		}
	}
	t.restore_var_types(saved_vars)
	t.mut_param_values = saved_mut_param_values.clone()
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	mut all_ids := []flat.NodeId{cap: param_ids.len + new_body.len}
	for param_id in param_ids {
		all_ids << param_id
	}
	for body_id in new_body {
		all_ids << body_id
	}
	// Generated declarations are appended after all parsed files. Emit an explicit
	// main context too, otherwise a main-module literal can inherit the preceding
	// cached module's context during the later flat AST scan.
	file_module := t.current_source_module()
	generated_module := if file_module.len > 0 { file_module } else { 'main' }
	t.a.add_node(flat.Node{
		kind:  .module_decl
		value: generated_module
	})
	start := t.a.children.len
	for child_id in all_ids {
		t.a.children << child_id
	}
	t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          name
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	if !isnil(t.tc) {
		ret := t.tc.parse_type(ret_type)
		t.tc.fn_ret_types[name] = ret
		t.tc.fn_param_types[name] = param_types.clone()
		t.tc.fn_variadic[name] = false
		t.add_receiver_method_suffix_index(name)
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			t.tc.fn_ret_types[qname] = ret
			t.tc.fn_param_types[qname] = param_types.clone()
			t.tc.fn_variadic[qname] = false
			t.add_receiver_method_suffix_index(qname)
		}
	}
	if file_module.len > 0 && file_module != 'main' && file_module != 'builtin' {
		return t.make_ident('${file_module}.${name}')
	}
	return t.make_ident(name)
}

fn (t &Transformer) current_source_module() string {
	if !isnil(t.tc) {
		if entry_file := t.tc.fn_type_files['main'] {
			if entry_file == t.cur_file {
				return 'main'
			}
		}
	}
	return if t.cur_module.len > 0 { t.cur_module } else { 'main' }
}

fn (mut t Transformer) new_fn_literal_name() string {
	for {
		name := t.new_temp('anon_fn')
		if !t.fn_literal_name_exists(name) {
			return name
		}
	}
	return t.new_temp('anon_fn')
}

fn (t &Transformer) fn_literal_name_exists(name string) bool {
	for node in t.a.nodes {
		if node.kind == .fn_decl && (node.value == name || node.value.all_after_last('.') == name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) add_fn_literal_capture_global(name string, typ string) {
	if typ.len == 0 {
		return
	}
	if t.cur_module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: t.cur_module
		})
	}
	field := t.a.add_node(flat.Node{
		kind:  .field_decl
		value: name
		typ:   typ
	})
	start := t.a.children.len
	t.a.children << field
	t.a.add_node(flat.Node{
		kind:           .global_decl
		children_start: start
		children_count: 1
	})
	t.globals[name] = typ
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		t.globals['${t.cur_module}.${name}'] = typ
	}
}

// try_lower_builtin_call checks if a call is to a builtin that needs special lowering.
// Returns none for most calls so the caller falls through to generic call transform.
fn (mut t Transformer) try_lower_builtin_call(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident && base.value == 'C' {
			return none
		}
	}
	if cast_call := t.try_lower_primitive_cast_call(node) {
		return cast_call
	}
	if flag_call := t.try_lower_flag_enum_call(node) {
		return flag_call
	}
	if flag_default := t.try_lower_flag_default_value_call(node) {
		return flag_default
	}
	if pool_call := t.try_lower_pool_generic_method_call(node) {
		return pool_call
	}
	if static_call := t.try_lower_static_assoc_call(_id, node) {
		return static_call
	}
	if move_call := t.try_lower_move_method_call(_id, node) {
		return move_call
	}
	if channel_call := t.try_lower_channel_method_call(_id, node) {
		return channel_call
	}
	if map_call := t.try_lower_map_method_call(_id, node) {
		return map_call
	}
	if array_call := t.try_lower_array_method_call(_id, node) {
		return array_call
	}
	if type_name_call := t.try_lower_sum_type_name_method_call(node) {
		return type_name_call
	}
	if enum_call := t.try_lower_enum_from_string_call(_id, node) {
		return enum_call
	}
	if receiver_call := t.try_lower_receiver_method_call(_id, node) {
		return receiver_call
	}
	if clone_call := t.try_lower_struct_clone_method_call(_id, node) {
		return clone_call
	}
	if string_call := t.try_lower_string_method_call(node) {
		return string_call
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .ident {
		return none
	}
	name := fn_node.value
	if name in ['maxof', 'minof'] && node.value.len > 0 && node.children_count == 1
		&& t.is_std_minmaxof_call(_id, name) {
		if value := t.try_lower_minmaxof_call(name, node.value) {
			return value
		}
	}
	match name {
		'copy' {
			return t.try_lower_copy_call(node)
		}
		'println', 'eprintln', 'print', 'eprint' {
			if node.children_count < 2 {
				return t.transform_call_args(_id, node)
			}
			arg := t.stringify_expr(t.a.child(&node, 1))
			return t.make_call(name, arr1(arg))
		}
		'panic' {
			if node.children_count == 2 {
				arg_id := t.a.child(&node, 1)
				arg_type := t.node_type(arg_id)
				if arg_type == 'IError' {
					arg := t.transform_expr(arg_id)
					return t.make_call('panic',
						arr1(t.make_method_call(arg, 'str', []flat.NodeId{})))
				}
			}
			return none
		}
		'sizeof' {
			return none
		}
		'typeof' {
			return none
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) is_std_minmaxof_call(id flat.NodeId, name string) bool {
	if isnil(t.tc) {
		return false
	}
	resolved := t.tc.resolved_call_name(id) or { return false }
	return resolved == 'math.${name}'
}

fn (mut t Transformer) try_lower_minmaxof_call(name string, raw_type string) ?flat.NodeId {
	mut typ := raw_type
	if !isnil(t.tc) {
		typ = t.normalize_type_in_module(raw_type, t.cur_module)
	}
	if typ.starts_with('builtin.') {
		typ = typ.all_after_last('.')
	}
	value := if name == 'maxof' {
		match typ {
			'i8' { '127' }
			'i16' { '32767' }
			'int', 'i32' { '2147483647' }
			'i64' { '9223372036854775807' }
			'u8' { '255' }
			'u16' { '65535' }
			'u32' { '4294967295' }
			'u64' { '18446744073709551615' }
			'f32' { '3.40282346638528859811704183484516925440e+38' }
			'f64' { '1.797693134862315708145274237317043567981e+308' }
			else { return none }
		}
	} else {
		match typ {
			'i8' { '-128' }
			'i16' { '-32768' }
			'int', 'i32' { '(-2147483647 - 1)' }
			'i64' { '(-9223372036854775807 - 1)' }
			'u8', 'u16', 'u32', 'u64' { '0' }
			'f32' { '-3.40282346638528859811704183484516925440e+38' }
			'f64' { '-1.797693134862315708145274237317043567981e+308' }
			else { return none }
		}
	}
	if typ in ['f32', 'f64'] {
		return t.make_float_literal_typed(value, typ)
	}
	return t.make_int_literal_typed(value, typ)
}

fn (mut t Transformer) try_lower_copy_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 3 {
		return none
	}
	dst_arg_id := t.a.child(&node, 1)
	src_arg_id := t.a.child(&node, 2)
	dst_id := t.copy_mut_arg_value(dst_arg_id)
	src := t.transform_expr_for_type(src_arg_id, '[]u8')
	if t.copy_destination_is_range(dst_id) {
		slice := t.transform_expr(dst_id)
		tmp_name := t.new_temp('copy_dst')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, slice, '[]u8')
		return t.make_call_typed('copy', arr2(t.make_prefix(.amp, t.make_ident(tmp_name)), src),
			'int')
	}
	dst_expr := t.transform_expr(dst_id)
	// a `mut` param destination is already a pointer; taking its address again
	// would hand v_copy an Array** and corrupt the caller's frame
	dst := if t.node_type(dst_id).starts_with('&') {
		dst_expr
	} else {
		t.make_prefix(.amp, dst_expr)
	}
	return t.make_call_typed('copy', arr2(dst, src), 'int')
}

fn (t &Transformer) copy_mut_arg_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.copy_mut_arg_value(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		return t.copy_mut_arg_value(t.a.child(&node, 0))
	}
	return id
}

fn (t &Transformer) copy_destination_is_range(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .index {
		return false
	}
	if node.value == 'range' {
		return true
	}
	if node.children_count > 1 {
		index := t.a.child_node(&node, 1)
		return index.kind == .range
	}
	return false
}

const primitive_cast_type_names = ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'byte',
	'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'rune', 'char']

// try_lower_primitive_cast_call supports try lower primitive cast call handling for Transformer.
fn (mut t Transformer) try_lower_primitive_cast_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .ident || fn_node.value !in primitive_cast_type_names {
		return none
	}
	arg_id := t.a.child(&node, 1)
	return t.make_cast(fn_node.value, t.transform_expr(arg_id), fn_node.value)
}

// try_lower_flag_default_value_call
// supports helper handling in transform.
fn (mut t Transformer) try_lower_flag_default_value_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	mut name := ''
	if fn_node.kind == .ident {
		name = fn_node.value
	} else if fn_node.kind == .selector {
		name = fn_node.value
	}
	if name != 'flag_default_value' {
		return none
	}
	arg_id := t.a.child(&node, 1)
	arg := t.transform_expr(arg_id)
	mut arg_type := t.resolve_expr_type(arg_id)
	if arg_type.len == 0 {
		arg_type = t.reliable_stringify_type(arg_id)
	}
	if arg_type.len == 0 {
		arg_type = t.node_type(arg)
	}
	if t.normalize_type_alias(arg_type) == 'string' {
		escaped := t.make_call_typed('escape_default_string', arr1(arg), 'string')
		return t.string_plus(t.string_plus(t.make_string_literal('"'), escaped),
			t.make_string_literal('"'))
	}
	return t.wrap_string_conversion(arg, arg_type)
}

// try_lower_sum_type_name_method_call supports try_lower_sum_type_name_method_call handling.
fn (mut t Transformer) try_lower_sum_type_name_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 1 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'type_name' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	resolved_sum := t.resolve_sum_name(clean_type)
	variants := t.sum_types[resolved_sum] or { return none }
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, 'sum_type')
	tag := t.make_sum_tag_selector(base, if base_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.build_sum_type_name_chain(tag, resolved_sum, variants, 0)
}

// build_sum_type_name_chain builds sum type name chain data for transform.
fn (mut t Transformer) build_sum_type_name_chain(tag flat.NodeId, sum_name string, variants []string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_string_literal('')
	}
	variant := variants[idx]
	display := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name, variant)))
	then_block := t.make_block(arr1(t.make_expr_stmt(t.make_string_literal(display))))
	else_expr := t.build_sum_type_name_chain(tag, sum_name, variants, idx + 1)
	else_block := t.make_block(arr1(t.make_expr_stmt(else_expr)))
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            'string'
	})
}

// try_lower_pool_generic_method_call
// supports helper handling in transform.
fn (mut t Transformer) try_lower_pool_generic_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 || node.value.len == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	if method !in ['get_item', 'get_results', 'get_results_ref'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.lvalue_type(base_id)
	if !is_pool_processor_type(base_type) {
		return none
	}
	elem_type := node.value
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if method == 'get_item' {
		if node.children_count < 2 {
			return none
		}
		idx := t.transform_expr(t.a.child(&node, 1))
		t.drain_pending(mut prefix)
		items := t.pool_processor_field(base, base_type, 'items', '[]voidptr')
		item := t.make_index(items, idx, 'voidptr')
		cast := t.make_cast('&${elem_type}', item, '&${elem_type}')
		value := t.make_prefix(.mul, cast)
		t.set_node_typ(int(value), elem_type)
		for stmt in prefix {
			t.pending_stmts << stmt
		}
		return value
	}
	result_name := t.new_temp('pool_results')
	idx_name := t.new_temp('pool_idx')
	results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	results_len := t.make_selector(results, 'len', 'int')
	is_ref_results := method == 'get_results_ref'
	out_elem_type := if is_ref_results { '&${elem_type}' } else { elem_type }
	out_type := '[]${out_elem_type}'
	prefix << t.make_decl_assign_typed(result_name, t.make_array_new_call(out_elem_type,
		t.make_int_literal(0), results_len), out_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond_results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(cond_results, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	body_results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	item := t.make_index(body_results, t.make_ident(idx_name), 'voidptr')
	value := if is_ref_results {
		t.make_cast('&${elem_type}', item, '&${elem_type}')
	} else {
		t.make_prefix(.mul, t.make_cast('&${elem_type}', item, '&${elem_type}'))
	}
	t.set_node_typ(int(value), out_elem_type)
	value_name := t.new_temp('pool_result')
	value_decl := t.make_decl_assign_typed(value_name, value, out_elem_type)
	push_call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(result_name)), t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
	loop_body := [value_decl, t.make_expr_stmt(push_call)]
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// is_pool_processor_type reports whether is pool processor type applies in transform.
fn is_pool_processor_type(typ string) bool {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.starts_with('mut ') {
		clean = clean[4..]
	}
	return clean == 'PoolProcessor' || clean.ends_with('.PoolProcessor')
}

// pool_processor_field supports pool processor field handling for Transformer.
fn (mut t Transformer) pool_processor_field(base flat.NodeId, base_type string, field string, typ string) flat.NodeId {
	op := if base_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	return t.make_selector_op(base, field, typ, op)
}

// try_lower_receiver_method_call supports try lower receiver method call handling for Transformer.
fn (mut t Transformer) try_lower_receiver_method_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	base_id := t.a.child(&fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	// `C.fn(...)` is a namespaced C call, not a receiver method. In particular,
	// generic specialization must not diagnose the `C` namespace as a value
	// whose method is missing.
	if base_node.kind == .ident && base_node.value == 'C' {
		return none
	}
	if t.is_import_alias_ident(base_id) {
		return none
	}
	// `Type.fn(...)` / `module.Type.fn(...)` is a static associated function, not a method:
	// the base names a type, so it must not be lowered into `fn(receiver, ...)`.
	if _ := t.static_assoc_fn_name(base_id, method) {
		return none
	}
	mut base_type := if base_node.kind in [.selector, .index] {
		t.lvalue_type(base_id)
	} else {
		t.node_type(base_id)
	}
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	base_is_pointer := base_type.starts_with('&')
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.len == 0 {
		return none
	}
	if method == 'close' && !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if resolved_method != 'chan.close' && resolved_method != 'sync__Channel__close' {
				if exact_call := t.lower_checker_selected_receiver_method(id, node, base_id,
					'chan.close')
				{
					return exact_call
				}
			}
		}
	}
	if method == 'close' && (base_type == 'chan' || base_type.starts_with('chan ')) {
		return t.lower_runtime_channel_close(base_id, node, if base_is_pointer { 1 } else { 0 })
	}
	if method == 'close' && !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if resolved_method == 'chan.close' || resolved_method == 'sync__Channel__close' {
				return t.lower_runtime_channel_close(base_id, node, if base_is_pointer {
					1
				} else {
					0
				})
			}
		}
	}
	if t.cur_fn_is_generic && base_type.contains('[')
		&& t.type_text_has_generic_placeholder(base_type, t.cur_module) {
		return none
	}
	mut builtin_base_type := t.normalize_type_alias(base_type)
	if builtin_base_type == 'byte' {
		builtin_base_type = 'u8'
	}
	if base_type == '[]rune' && method == 'string' {
		return t.make_call_typed('Array_rune__string', arr1(t.transform_expr(base_id)), 'string')
	}
	if method == 'str' && t.is_array_transform_call(base_id) {
		base := t.transform_expr(base_id)
		transformed_type := t.node_type(base)
		if transformed_type.starts_with('[]') {
			return t.wrap_string_conversion(base, transformed_type)
		}
	}
	if method == 'str' && t.is_builder_receiver(base_id, base_type) {
		method_name := 'strings.Builder.str'
		args := t.transform_receiver_method_args(node, base_id, method_name)
		return t.make_call_typed(method_name, args, 'string')
	}
	if method == 'str' {
		if exact_call := t.lower_checker_selected_receiver_method(id, node, base_id, 'str') {
			return exact_call
		}
		// Some calls cloned during comptime/generic lowering no longer have the
		// checker's original call-id annotation. Resolve their concrete receiver
		// method before falling back to generated auto-stringification.
		mut method_names := ['${base_type}.${method}']
		mut collection_method := ''
		resolved_method := t.resolve_receiver_method_name(base_id, method)
		if resolved_method.len > 0 && resolved_method !in method_names {
			method_names << resolved_method
		}
		if base_type.starts_with('[]') || base_type.starts_with('map[') {
			collection_method = t.resolve_collection_receiver_method_name(base_id, method,
				base_type)
			if collection_method.len > 0 && collection_method !in method_names {
				method_names << collection_method
			}
		}
		for method_name in method_names {
			if t.is_known_fn_name(method_name)
				&& (t.receiver_method_matches_base_type(method_name, base_id)
				|| (collection_method.len > 0 && method_name == collection_method)) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used_name(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
		}
	}
	if method == 'str' && stringify_type_has_generic_placeholder(base_type) {
		return none
	}
	if base_type.starts_with('[]') && method == 'str' {
		return t.wrap_string_conversion(t.transform_expr(base_id), base_type)
	}
	if method == 'str' {
		if t.is_enum_stringify_type(base_type) {
			return none
		}
		// `(&Struct).str()` keeps the reference so the pointee is stringified with V's `&`
		// prefix (or `&nil`); primitive/alias pointers keep their existing ptr_str behavior.
		if base_is_pointer {
			if aggregate := t.stringify_aggregate_type_name(base_type) {
				return t.lower_ref_str_prefixed(t.transform_expr(base_id), aggregate)
			}
		}
		return t.wrap_string_conversion(t.transform_expr(base_id), base_type)
	}
	if builtin_base_type == 'string' && method == 'hex' && !base_is_pointer {
		return t.make_call_typed('string.hex', arr1(t.transform_expr(base_id)), 'string')
	}
	if base_type == '[]u8' || base_type == '[]byte' {
		if method == 'bytestr' {
			return t.make_call_typed('Array_u8__bytestr', arr1(t.transform_expr(base_id)), 'string')
		}
		if method == 'hex' && !base_is_pointer {
			return t.make_call_typed('Array_u8__hex', arr1(t.transform_expr(base_id)), 'string')
		}
	}
	if pointer_method := t.pointer_builtin_vbytes_method(base_is_pointer, builtin_base_type, method) {
		args := t.transform_receiver_method_args(node, base_id, pointer_method)
		ret_type := t.receiver_method_return_type(pointer_method, node.typ)
		t.mark_fn_used(pointer_method)
		return t.make_call_typed(pointer_method, args, ret_type)
	}
	if t.is_builder_receiver(base_id, base_type) {
		for method_name in ['strings.Builder.${method}', 'Builder.${method}'] {
			if t.is_known_fn_name(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				return t.make_call_typed(method_name, args, ret_type)
			}
		}
	}
	if builtin_base_type == 'u8' && method in ['is_space', 'is_digit', 'is_hex_digit', 'is_letter'] {
		return t.make_call_typed('u8__${method}', arr1(t.transform_expr(base_id)), 'bool')
	}
	if !base_is_pointer
		&& builtin_base_type in ['u8', 'i8', 'u16', 'i16', 'u32', 'int', 'u64', 'i64', 'rune']
		&& method in ['hex', 'hex_full'] {
		return t.make_call_typed('${builtin_base_type}__${method}',
			arr1(t.transform_expr(base_id)), 'string')
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') {
		if base_type.starts_with('[]') {
			if t.validating_generic_spec {
				if array_method_stays_in_cgen(method) {
					if !t.validate_cgen_array_method_args(node, base_id, base_type, method) {
						return t.make_empty()
					}
				} else if
					t.resolve_collection_receiver_method_name(base_id, method, base_type).len == 0
					&& !t.receiver_selector_is_fn_field(base_type, method) {
					base_name := if base_node.kind == .ident && base_node.value.len > 0 {
						base_node.value
					} else {
						base_type
					}
					t.record_monomorph_error('unknown function `${base_name}.${method}`')
				}
			}
			return none
		}
	}
	// An active smartcast rebinds the receiver: inside `match v { A {...} }`,
	// `v.m()` must dispatch to `A.m` even when the sum type itself also
	// declares `m` (both the checker resolution and the static resolution
	// below would pick the sum's method).
	smartcast_method := t.resolve_smartcast_sum_receiver_method(base_id, method) or { '' }
	if !isnil(t.tc) && smartcast_method.len == 0 {
		if resolved_method := t.tc.resolved_call_name(id) {
			if t.receiver_method_name_is_open_generic(resolved_method) {
				return none
			}
			if t.is_known_fn_name(resolved_method) {
				params := t.call_param_types(resolved_method)
				if t.resolved_call_uses_receiver_type(base_id, base_type, params) {
					if !t.validate_resolved_receiver_method_args(node, base_id, resolved_method) {
						return t.make_empty()
					}
					args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id,
						resolved_method), resolved_method)
					ret_type := t.receiver_method_return_type(resolved_method, node.typ)
					if !t.validate_specialized_call_result(id, ret_type) {
						return t.make_empty()
					}
					return t.make_call_typed(resolved_method, args, ret_type)
				}
			}
		}
	}
	mut method_name := smartcast_method
	if method_name.len == 0 {
		method_name = t.resolve_receiver_method_name(base_id, method)
	}
	if method_name.len > 0 {
		if t.receiver_method_name_is_open_generic(method_name) {
			return none
		}
		if !t.validate_resolved_receiver_method_args(node, base_id, method_name) {
			return t.make_empty()
		}
		args := t.transform_receiver_method_args(node, base_id, method_name)
		ret_type := t.receiver_method_return_type(method_name, node.typ)
		if !t.validate_specialized_call_result(id, ret_type) {
			return t.make_empty()
		}
		return t.make_call_typed(method_name, args, ret_type)
	}
	if !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if t.receiver_method_name_is_open_generic(resolved_method) {
				return none
			}
			if t.is_known_fn_name(resolved_method) {
				params := t.call_param_types(resolved_method)
				if !t.resolved_call_uses_receiver_type(base_id, base_type, params) {
					return none
				}
				if !t.validate_resolved_receiver_method_args(node, base_id, resolved_method) {
					return t.make_empty()
				}
				args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id,
					resolved_method), resolved_method)
				ret_type := t.receiver_method_return_type(resolved_method, node.typ)
				if !t.validate_specialized_call_result(id, ret_type) {
					return t.make_empty()
				}
				return t.make_call_typed(resolved_method, args, ret_type)
			}
		}
	}
	if sum_method := t.resolve_smartcast_sum_receiver_method(base_id, method) {
		if !t.validate_resolved_receiver_method_args(node, base_id, sum_method) {
			return t.make_empty()
		}
		args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id,
			sum_method), sum_method)
		ret_type := t.receiver_method_return_type(sum_method, node.typ)
		if !t.validate_specialized_call_result(id, ret_type) {
			return t.make_empty()
		}
		return t.make_call_typed(sum_method, args, ret_type)
	}
	if t.validating_generic_spec {
		if base_node.kind == .ident && base_node.value in ['C', 'JS'] {
			// `C.fn(...)` is an extern call, not a method on an ident.
			return none
		}
		if field_type := t.lookup_struct_field_type(base_type, method) {
			if !t.validate_specialized_fn_field_call(id, node, base_id, field_type) {
				return t.make_empty()
			}
			return none
		}
		base_name := if base_node.kind == .ident && base_node.value.len > 0 {
			base_node.value
		} else {
			base_type
		}
		t.record_monomorph_error('unknown function `${base_name}.${method}`')
	}
	return none
}

fn (mut t Transformer) validate_resolved_receiver_method_args(node flat.Node, base_id flat.NodeId, method_name string) bool {
	if !t.validating_generic_spec {
		return true
	}
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return true
	}
	param_offset := t.receiver_method_param_offset(base_id, node, params, method_name)
	if param_offset == 0 {
		return true
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if t.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed_fields := if field_init_args > 0 { 1 } else { 0 }
	actual_count := int(node.children_count) - 1 - field_init_args + collapsed_fields
	expected_count := params.len - param_offset
	is_variadic := t.call_is_variadic(method_name) && params[params.len - 1] is types.Array
	mut min_count := expected_count
	mut trailing_idx := params.len - 1
	for trailing_idx >= param_offset {
		if is_variadic && trailing_idx == params.len - 1 {
			min_count--
			trailing_idx--
			continue
		}
		if _ := t.params_struct_type_name(params[trailing_idx].name()) {
			min_count--
			trailing_idx--
			continue
		}
		break
	}
	display_name := t.resolved_receiver_call_display_name(node, base_id, method_name)
	if actual_count < min_count || (!is_variadic && actual_count > expected_count) {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${expected_count}, got ${actual_count}')
		return false
	}
	mut valid := true
	mut arg_idx := 0
	mut child_idx := 1
	for child_idx < node.children_count {
		arg_id := t.a.child(&node, child_idx)
		arg_node := t.a.nodes[int(arg_id)]
		param_idx := param_offset + arg_idx
		mut expected := params[if param_idx < params.len { param_idx } else { params.len - 1 }]
		if arg_node.kind == .field_init {
			struct_type := t.params_struct_type_name(expected.name()) or {
				t.struct_arg_type_name(expected.name()) or { '' }
			}
			if struct_type.len == 0
				|| !t.validate_specialized_struct_field_args(node, child_idx, struct_type) {
				valid = false
			}
			child_idx = t.next_non_field_init_arg(node, child_idx)
			arg_idx++
			continue
		}
		if is_variadic && param_idx >= params.len - 1 {
			variadic_type := params[params.len - 1]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...' {
					if arg_node.children_count > 0 {
						spread_id := t.a.child(&arg_node, 0)
						actual_name := t.specialized_expr_type_name(spread_id)
						expected_name := params[params.len - 1].name()
						if !t.resolved_receiver_arg_compatible(spread_id, actual_name,
							expected_name) {
							t.record_monomorph_error('cannot use `${actual_name}` as argument ${
								arg_idx + 1} to `${display_name}`; expected `${expected_name}`')
							valid = false
						}
					}
					child_idx++
					arg_idx++
					continue
				}
				expected = variadic_type.elem_type
			}
		}
		actual_name := t.specialized_expr_type_name(arg_id)
		expected_name := expected.name()
		if t.resolved_receiver_arg_compatible(arg_id, actual_name, expected_name) {
			child_idx++
			arg_idx++
			continue
		}
		t.record_monomorph_error('cannot use `${actual_name}` as argument ${arg_idx + 1} to `${display_name}`; expected `${expected_name}`')
		valid = false
		child_idx++
		arg_idx++
	}
	return valid
}

fn (mut t Transformer) validate_specialized_struct_field_args(node flat.Node, field_start int, struct_type string) bool {
	mut valid := true
	mut i := field_start
	for i < node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		field_type := t.lookup_struct_field_type(struct_type, field.value) or {
			t.record_monomorph_error('unknown field `${field.value}` in `${struct_type.all_after_last('.')}`')
			valid = false
			i++
			continue
		}
		if field.children_count == 0 {
			i++
			continue
		}
		value_id := t.a.child(field, 0)
		actual_type := t.specialized_expr_type_name(value_id)
		if !t.resolved_receiver_arg_compatible(value_id, actual_type, field_type) {
			t.record_monomorph_error('cannot initialize field `${field.value}` with `${actual_type}`; expected `${field_type}`')
			valid = false
		}
		i++
	}
	return valid
}

fn (mut t Transformer) validate_specialized_fn_field_call(id flat.NodeId, node flat.Node, base_id flat.NodeId, field_type string) bool {
	display_name := t.resolved_receiver_call_display_name(node, base_id, '')
	if isnil(t.tc) {
		return true
	}
	fn_type := transform_fn_type(t.tc.parse_type(field_type)) or {
		t.record_monomorph_error('unknown function `${display_name}`')
		return false
	}
	actual_count := int(node.children_count) - 1
	if actual_count != fn_type.params.len {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${fn_type.params.len}, got ${actual_count}')
		return false
	}
	mut valid := true
	for i in 0 .. actual_count {
		arg_id := t.a.child(&node, i + 1)
		actual_type := t.specialized_expr_type_name(arg_id)
		expected_type := fn_type.params[i].name()
		if t.resolved_receiver_arg_compatible(arg_id, actual_type, expected_type) {
			continue
		}
		t.record_monomorph_error('cannot use `${actual_type}` as argument ${i + 1} to `${display_name}`; expected `${expected_type}`')
		valid = false
	}
	if !t.validate_specialized_call_result(id, fn_type.return_type.name()) {
		valid = false
	}
	return valid
}

fn transform_fn_type(typ types.Type) ?types.FnType {
	if typ is types.FnType {
		return typ
	}
	if typ is types.Alias {
		return transform_fn_type(typ.base_type)
	}
	return none
}

fn (mut t Transformer) validate_specialized_call_result(id flat.NodeId, actual_type string) bool {
	if !t.validating_generic_spec || t.expected_expr_node != int(id)
		|| t.expected_expr_type.len == 0 || t.expected_expr_type in ['unknown', 'void'] {
		return true
	}
	expected_type := t.expected_expr_type
	if t.resolved_receiver_arg_compatible(id, actual_type, expected_type) {
		return true
	}
	if t.in_return_expr {
		t.record_monomorph_error('cannot return `${actual_type}` as `${expected_type}`')
	} else {
		t.record_monomorph_error('cannot use `${actual_type}` as `${expected_type}`')
	}
	return false
}

fn (mut t Transformer) validate_specialized_comparison_operands(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, transformed_lhs flat.NodeId, transformed_rhs flat.NodeId) bool {
	if !t.validating_generic_spec || node.op !in [.eq, .ne, .lt, .gt, .le, .ge] {
		return true
	}
	lhs_is_call := t.specialized_comparison_operand_is_receiver_call(lhs_id)
	rhs_is_call := t.specialized_comparison_operand_is_receiver_call(rhs_id)
	if !lhs_is_call && !rhs_is_call {
		return true
	}
	lhs_type := t.specialized_expr_type_name(transformed_lhs)
	rhs_type := t.specialized_expr_type_name(transformed_rhs)
	if t.specialized_comparison_types_compatible(transformed_lhs, lhs_type, transformed_rhs,
		rhs_type)
	{
		return true
	}
	if lhs_is_call {
		t.record_monomorph_error('cannot use `${lhs_type}` as `${rhs_type}`')
	} else {
		t.record_monomorph_error('cannot use `${rhs_type}` as `${lhs_type}`')
	}
	return false
}

fn (t &Transformer) specialized_comparison_operand_is_receiver_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_comparison_operand_is_receiver_call(t.a.child(&node, 0))
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .selector && callee.children_count > 0
}

fn (mut t Transformer) specialized_comparison_types_compatible(lhs_id flat.NodeId, lhs_type string, rhs_id flat.NodeId, rhs_type string) bool {
	if lhs_type.len == 0 || rhs_type.len == 0 || lhs_type == 'unknown' || rhs_type == 'unknown' {
		return true
	}
	lhs := t.normalize_type_alias(lhs_type)
	rhs := t.normalize_type_alias(rhs_type)
	if lhs == rhs {
		return true
	}
	lhs_is_number := t.is_integer_type_name(lhs) || lhs in ['f32', 'f64']
	rhs_is_number := t.is_integer_type_name(rhs) || rhs in ['f32', 'f64']
	if lhs_is_number && rhs_is_number {
		return true
	}
	return t.resolved_receiver_arg_compatible(lhs_id, lhs_type, rhs_type)
		|| t.resolved_receiver_arg_compatible(rhs_id, rhs_type, lhs_type)
}

fn (mut t Transformer) specialized_expr_type_name(id flat.NodeId) string {
	if t.specialized_expr_is_none(id) {
		return 'none'
	}
	mut typ := t.resolve_expr_type(id)
	if typ.len == 0 {
		typ = t.node_type(id)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(id)
	}
	return typ
}

fn (t &Transformer) resolved_receiver_call_display_name(node flat.Node, base_id flat.NodeId, method_name string) string {
	base := t.a.nodes[int(base_id)]
	base_name := if base.kind == .ident && base.value.len > 0 {
		base.value
	} else {
		t.node_type(base_id)
	}
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value.len > 0 {
			return '${base_name}.${callee.value}'
		}
	}
	method := if method_name.contains('.') { method_name.all_after_last('.') } else { method_name }
	return '${base_name}.${method}'
}

fn (mut t Transformer) resolved_receiver_arg_compatible(arg_id flat.NodeId, actual_type string, expected_type string) bool {
	if actual_type.len == 0 || actual_type == 'unknown' || expected_type.len == 0 {
		return true
	}
	// An `unknown` expected type means the callee signature still carries an
	// unresolved generic parameter here - nothing can be validated against it
	// (including container forms such as `[]unknown`).
	if expected_type.contains('unknown') {
		return true
	}
	actual := t.normalize_type_alias(actual_type)
	expected := t.normalize_type_alias(expected_type)
	if t.is_integer_type_name(expected) {
		if literal := t.specialized_int_literal(arg_id) {
			return specialized_int_literal_fits_type(literal, expected)
		}
	}
	if actual == expected {
		return true
	}
	if t.expr_is_nil_like(arg_id)
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	// A `mut`/pointer parameter is called with the value form (`r.read(mut
	// buf)` with `buf []u8` against `&[]u8`); cgen auto-refs such args.
	if expected.starts_with('&') && actual == expected[1..] {
		return true
	}
	// Any pointer converts to voidptr.
	if expected in ['voidptr', 'byteptr', 'charptr']
		&& (actual.starts_with('&') || actual in ['voidptr', 'byteptr', 'charptr', 'nil']) {
		return true
	}
	// An empty array literal (`[]`) types as `[]void` and adopts the
	// parameter's element type.
	if actual == '[]void' && expected.starts_with('[]') {
		return true
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .float_literal && expected in ['f32', 'f64'] {
		return true
	}
	// A char literal (`\`x\``) types as rune but coerces to u8 params.
	if arg.kind == .char_literal && expected in ['u8', 'rune', 'char', 'int', 'u32'] {
		return true
	}
	if actual == 'nil'
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	if expected.starts_with('?') {
		if actual == expected[1..] || t.specialized_expr_is_none(arg_id) {
			return true
		}
	}
	if expected.starts_with('!') {
		if actual == expected[1..] || t.is_ierror_type(actual) {
			return true
		}
	}
	if t.is_sum_type_name(expected) && t.sum_target_accepts_variant_type(expected, actual) {
		return true
	}
	// The reverse also passes validation: a sum-typed value flows into a
	// variant-typed parameter under an `is`-guard (smartcast) that a not-yet
	// unrolled `$for v in T.variants` body cannot expose to this check.
	if t.is_sum_type_name(actual) && t.sum_target_accepts_variant_type(actual, expected) {
		return true
	}
	if !isnil(t.tc) && expected in t.tc.interface_names
		&& t.tc.type_text_implements_interface(actual, expected) {
		return true
	}
	// Generic applications may differ only in module qualification of their
	// type arguments (`json2.Node[ValueInfo]` vs `json2.Node[json2.ValueInfo]`).
	mut a_app := actual
	mut e_app := expected
	for a_app.starts_with('&') && e_app.starts_with('&') {
		a_app = a_app[1..]
		e_app = e_app[1..]
	}
	if !isnil(t.tc) && a_app.contains('[') && e_app.contains('[')
		&& t.tc.generic_type_name_matches(a_app, e_app) {
		return true
	}
	return false
}

struct SpecializedIntLiteral {
	negative  bool
	magnitude u64
}

fn (t &Transformer) specialized_int_literal(id flat.NodeId) ?SpecializedIntLiteral {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_int_literal(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.children_count > 0 && node.op in [.plus, .minus] {
		mut literal := t.specialized_int_literal(t.a.child(&node, 0)) or { return none }
		if node.op == .minus && literal.magnitude != 0 {
			literal = SpecializedIntLiteral{
				negative:  !literal.negative
				magnitude: literal.magnitude
			}
		}
		return literal
	}
	if node.kind != .int_literal {
		return none
	}
	magnitude := specialized_uint_literal_value(node.value) or { return none }
	return SpecializedIntLiteral{
		magnitude: magnitude
	}
}

fn specialized_uint_literal_value(text string) ?u64 {
	clean := text.replace('_', '')
	if clean.len == 0 {
		return none
	}
	mut base := u64(10)
	mut start := 0
	if clean.len >= 2 && clean[0] == `0` {
		match clean[1] {
			`x`, `X` {
				base = 16
				start = 2
			}
			`o`, `O` {
				base = 8
				start = 2
			}
			`b`, `B` {
				base = 2
				start = 2
			}
			else {}
		}
	}
	if start >= clean.len {
		return none
	}
	mut value := u64(0)
	for i in start .. clean.len {
		ch := clean[i]
		digit := if ch >= `0` && ch <= `9` {
			u64(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u64(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			u64(ch - `A`) + 10
		} else {
			return none
		}
		if digit >= base || value > (max_u64 - digit) / base {
			return none
		}
		value = value * base + digit
	}
	return value
}

fn specialized_int_literal_fits_type(literal SpecializedIntLiteral, typ string) bool {
	return match typ {
		'u8', 'byte' { !literal.negative && literal.magnitude <= 255 }
		'u16' { !literal.negative && literal.magnitude <= 65535 }
		'u32' { !literal.negative && literal.magnitude <= u64(4294967295) }
		'u64', 'usize' { !literal.negative }
		'i8' { specialized_signed_literal_fits(literal, 127) }
		'i16' { specialized_signed_literal_fits(literal, 32767) }
		'int', 'i32', 'rune' { specialized_signed_literal_fits(literal, 2147483647) }
		'i64', 'isize' { specialized_signed_literal_fits(literal, u64(max_i64)) }
		else { true }
	}
}

fn specialized_signed_literal_fits(literal SpecializedIntLiteral, max u64) bool {
	if literal.negative {
		return literal.magnitude <= max + 1
	}
	return literal.magnitude <= max
}

fn (t &Transformer) specialized_expr_is_none(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_expr_is_none(t.a.child(&node, 0))
	}
	return false
}

fn (t &Transformer) receiver_selector_is_fn_field(base_type string, field string) bool {
	field_type := t.lookup_struct_field_type(base_type, field) or { return false }
	clean := t.normalize_type_alias(field_type)
	return clean.starts_with('fn(') || clean.starts_with('fn (')
}

fn (mut t Transformer) record_monomorph_error(message string) {
	key := '${t.cur_file}:${t.cur_fn_name}:${message}'
	if t.monomorph_error_seen[key] {
		return
	}
	t.monomorph_error_seen[key] = true
	t.monomorph_errors << message
}

fn (t &Transformer) pointer_builtin_vbytes_method(base_is_pointer bool, builtin_base_type string, method string) ?string {
	if method != 'vbytes' {
		return none
	}
	if base_is_pointer && builtin_base_type == 'u8' {
		return 'byteptr.vbytes'
	}
	if builtin_base_type in ['byteptr', 'voidptr'] {
		return '${builtin_base_type}.vbytes'
	}
	return none
}

fn (t &Transformer) receiver_method_name_is_open_generic(method_name string) bool {
	if method_name.contains('.') {
		receiver := method_name.all_before_last('.')
		if collection_receiver := receiver_collection_method_type(receiver) {
			return t.generic_arg_is_unresolved_collection_type(collection_receiver)
		}
		_, args, ok := generic_app_parts(receiver)
		return ok && args.len > 0 && t.generic_args_have_placeholders(args)
	}
	return method_name_contains_mangled_open_generic_placeholder(method_name)
}

fn receiver_collection_method_type(receiver string) ?string {
	if receiver.starts_with('[]') || receiver.starts_with('map[') {
		return receiver
	}
	if receiver.contains('.[]') {
		return '[]${receiver.all_after('.[]')}'
	}
	if receiver.contains('.map[') {
		return 'map[${receiver.all_after('.map[')}'
	}
	return none
}

fn (t &Transformer) generic_arg_is_unresolved_collection_type(receiver string) bool {
	if receiver.starts_with('[]') {
		return t.generic_arg_is_unresolved(receiver[2..])
	}
	if receiver.starts_with('map[') {
		return t.generic_arg_is_unresolved(t.map_key_type(receiver))
			|| t.generic_arg_is_unresolved(t.map_value_type(receiver))
	}
	return false
}

fn method_name_contains_mangled_open_generic_placeholder(method_name string) bool {
	if method_name.len < 4 {
		return false
	}
	for i in 0 .. method_name.len - 3 {
		letter := method_name[i + 1]
		if method_name[i] == `_` && letter >= `A` && letter <= `Z` && method_name[i + 2] == `_`
			&& method_name[i + 3] == `_` {
			return true
		}
	}
	return false
}

// is_builder_receiver reports whether is builder receiver applies in transform.
fn (t &Transformer) is_builder_receiver(base_id flat.NodeId, base_type string) bool {
	if is_builder_type_name(base_type) {
		return true
	}
	if raw_type := t.raw_var_type_for_expr(base_id) {
		return is_builder_type_name(raw_type)
	}
	if raw_field_type := t.raw_selector_field_type(base_id) {
		return is_builder_type_name(raw_field_type)
	}
	return false
}

// raw_selector_field_type supports raw selector field type handling for Transformer.
fn (t &Transformer) raw_selector_field_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.original_expr_type(base_id)
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	return t.lookup_struct_field_raw_type(base_type, node.value)
}

// is_builder_type_name reports whether is builder type name applies in transform.
fn is_builder_type_name(typ string) bool {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	return clean == 'strings.Builder' || clean == 'Builder'
}

// resolved_call_uses_receiver_type
// supports helper handling in transform.
fn (t &Transformer) resolved_call_uses_receiver_type(base_id flat.NodeId, receiver_type string, params []types.Type) bool {
	if params.len == 0 {
		return false
	}
	mut param_type := params[0].name()
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	if sc := t.find_smartcast(t.expr_key(base_id)) {
		sc_type := t.trim_pointer_type(t.smartcast_target_type(sc))
		if sc_type.len > 0 && t.normalize_type_alias(sc_type) == t.normalize_type_alias(param_type) {
			return true
		}
		sc_sum := t.trim_pointer_type(t.resolve_sum_name(sc.sum_type_name))
		if sc_sum.len > 0 && t.normalize_type_alias(sc_sum) == t.normalize_type_alias(param_type) {
			return true
		}
		if t.is_sum_type_name(param_type) {
			for parent in t.sum_type_parents_for_variant(sc.variant_name) {
				if t.normalize_type_alias(parent) == t.normalize_type_alias(param_type) {
					return true
				}
			}
		}
	}
	mut base_type := receiver_type
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.len == 0 {
		return true
	}
	if t.normalize_type_alias(base_type) == t.normalize_type_alias(param_type) {
		return true
	}
	if _ := t.embedded_receiver_path(base_type, param_type) {
		return true
	}
	return false
}

// receiver_base_for_resolved_method
// supports helper handling in transform.
fn (mut t Transformer) receiver_base_for_resolved_method(base_id flat.NodeId, method_name string) flat.NodeId {
	if embedded_base := t.embedded_receiver_base(base_id, method_name) {
		return embedded_base
	}
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return t.transform_expr(base_id) }
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return t.transform_expr(base_id)
	}
	mut param_type := params[0].name()
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	target_type := t.trim_pointer_type(t.smartcast_target_type(sc))
	if target_type.len > 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(target_type) {
		return t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id),
			t.original_expr_type(base_id), t.smartcasts_for(key))
	}
	method_receiver := method_name.all_before_last('.')
	if method_receiver.len > 0 && t.is_sum_type_name(method_receiver)
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(method_receiver) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	mut sum_type := t.resolve_sum_name(sc.sum_type_name)
	if sum_type.starts_with('&') {
		sum_type = sum_type[1..]
	}
	if sum_type.len > 0 && t.normalize_type_alias(param_type) == t.normalize_type_alias(sum_type) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	original_type := t.trim_pointer_type(t.original_expr_type(base_id))
	if original_type.len > 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(original_type) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	return t.transform_expr(base_id)
}

fn (mut t Transformer) embedded_receiver_base(base_id flat.NodeId, method_name string) ?flat.NodeId {
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return none
	}
	mut param_type := params[0].name()
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	mut is_ptr := false
	if base_type.starts_with('&') {
		is_ptr = true
		base_type = base_type[1..]
	}
	path := t.embedded_receiver_path(base_type, param_type) or { return none }
	mut base := t.transform_expr(base_id)
	mut current_is_ptr := is_ptr
	for field in path {
		op := if current_is_ptr { flat.Op.arrow } else { flat.Op.dot }
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		base = t.make_selector_op(base, field.name, field_type, op)
		current_is_ptr = field_type.starts_with('&')
	}
	return base
}

fn (t &Transformer) embedded_receiver_field(base_type string, receiver_type string) ?FieldInfo {
	path := t.embedded_receiver_path(base_type, receiver_type) or { return none }
	if path.len == 0 {
		return none
	}
	return path[0]
}

fn (t &Transformer) embedded_receiver_path(base_type string, receiver_type string) ?[]FieldInfo {
	if base_type.len == 0 || receiver_type.len == 0 {
		return none
	}
	mut lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	info := t.structs[lookup_type] or { return none }
	clean_receiver := t.normalize_type_alias(receiver_type)
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		clean_field := t.normalize_type_alias(field_type.trim_left('&'))
		short_field := if clean_field.contains('.') {
			clean_field.all_after_last('.')
		} else {
			clean_field
		}
		if (field.name == clean_field || field.name == short_field) && clean_field == clean_receiver {
			return [field]
		}
		if sub_path := t.embedded_receiver_path(clean_field, receiver_type) {
			mut path := []FieldInfo{}
			path << field
			path << sub_path
			return path
		}
	}
	return none
}

// receiver_method_return_type supports receiver method return type handling for Transformer.
fn (t &Transformer) receiver_method_return_type(method_name string, fallback string) string {
	if !isnil(t.tc) {
		if typ := t.tc.fn_ret_types[method_name] {
			return t.normalize_type_alias(typ.name())
		}
	}
	if ret := t.fn_ret_types[method_name] {
		return ret
	}
	return fallback
}

fn (t &Transformer) call_resolved_to_method(call_id flat.NodeId, method_name string) bool {
	if isnil(t.tc) {
		return true
	}
	if resolved := t.tc.resolved_call_name(call_id) {
		return resolved == method_name
	}
	return false
}

fn (mut t Transformer) lower_checker_selected_receiver_method(call_id flat.NodeId, node flat.Node, base_id flat.NodeId, builtin_name string) ?flat.NodeId {
	resolved := t.checker_selected_receiver_method_name(call_id, builtin_name) or { return none }
	if !t.receiver_method_matches_base_type(resolved, base_id) {
		return none
	}
	args := t.transform_receiver_method_args(node, base_id, resolved)
	ret_type := t.receiver_method_return_type(resolved, node.typ)
	t.mark_fn_used_name(resolved)
	return t.make_call_typed(resolved, args, ret_type)
}

fn (t &Transformer) receiver_method_matches_base_type(method_name string, base_id flat.NodeId) bool {
	receiver_name := method_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 && !isnil(t.tc) {
		base_type = t.tc.resolve_type(base_id).name()
	}
	for base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	base_type = t.normalize_type_alias(base_type)
	if base_type.len == 0 {
		return true
	}
	if receiver_name == base_type {
		return true
	}
	if !base_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name == '${t.cur_module}.${base_type}' {
		return true
	}
	if receiver_name.all_after_last('.') != base_type.all_after_last('.') {
		return true
	}
	return false
}

fn (t &Transformer) checker_selected_receiver_method_name(call_id flat.NodeId, builtin_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	resolved := t.tc.resolved_call_name(call_id) or { return none }
	if t.receiver_method_name_is_open_generic(resolved) {
		return none
	}
	if resolved == builtin_name {
		return none
	}
	if is_builtin_collection_resolved_call(resolved) || !t.is_known_fn_name(resolved) {
		if known := t.known_collection_receiver_method_alias(resolved) {
			return known
		}
		return none
	}
	return resolved
}

fn (t &Transformer) known_collection_receiver_method_alias(name string) ?string {
	if name.len == 0 || is_builtin_collection_resolved_call(name) {
		return none
	}
	lowered := c_name(name)
	if lowered != name && t.is_known_fn_name(lowered) {
		return lowered
	}
	if !transform_can_prefix_collection_receiver(t.cur_module) {
		return none
	}
	if !(name.starts_with('[]') || name.starts_with('map[')) {
		return none
	}
	qname := '${t.cur_module}.${name}'
	if t.is_known_fn_name(qname) {
		return qname
	}
	qlowered := c_name(qname)
	if qlowered != qname && t.is_known_fn_name(qlowered) {
		return qlowered
	}
	return none
}

fn is_builtin_collection_resolved_call(name string) bool {
	return name.len == 0 || is_raw_collection_method_name(name, 'array.') || name == 'array_clone'
		|| is_runtime_collection_helper_name(name) || is_raw_collection_method_name(name, 'map.')
}

fn receiver_method_name_has_generic_placeholder(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver := name.all_before_last('.')
	if receiver.contains('[') {
		for marker in ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z'] {
			if receiver.contains('[${marker}]') || receiver.contains('[${marker},')
				|| receiver.contains(', ${marker}]') || receiver.contains(',${marker}]') {
				return true
			}
		}
	}
	for part in receiver.split('_') {
		if part in ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z'] {
			return true
		}
	}
	return false
}

fn is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

fn is_runtime_collection_helper_name(name string) bool {
	if name.len > 'array__'.len && has_array_runtime_prefix(name) {
		return is_array_runtime_helper_method_name(name, 'array__'.len, name.len - 'array__'.len)
	}
	if name.len > 'map__'.len && has_map_runtime_prefix(name) {
		return is_map_runtime_helper_method_name(name, 'map__'.len, name.len - 'map__'.len)
	}
	return false
}

fn has_array_runtime_prefix(name string) bool {
	return name[0] == `a` && name[1] == `r` && name[2] == `r` && name[3] == `a` && name[4] == `y`
		&& name[5] == `_` && name[6] == `_`
}

fn has_map_runtime_prefix(name string) bool {
	return name[0] == `m` && name[1] == `a` && name[2] == `p` && name[3] == `_` && name[4] == `_`
}

fn is_array_runtime_helper_method_name(name string, start int, len int) bool {
	return match len {
		5 { name_part_eq(name, start, 'clone') }
		6 { name_part_eq(name, start, 'insert') }
		7 { name_part_eq(name, start, 'reverse') || name_part_eq(name, start, 'prepend') }
		9 { name_part_eq(name, start, 'push_many') }
		18 { name_part_eq(name, start, 'needs_unique_shift') }
		else { false }
	}
}

fn is_map_runtime_helper_method_name(name string, start int, len int) bool {
	return match len {
		3 {
			name_part_eq(name, start, 'get') || name_part_eq(name, start, 'set')
		}
		4 {
			name_part_eq(name, start, 'keys') || name_part_eq(name, start, 'move')
				|| name_part_eq(name, start, 'free')
		}
		5 {
			name_part_eq(name, start, 'clear')
		}
		6 {
			name_part_eq(name, start, 'delete') || name_part_eq(name, start, 'values')
				|| name_part_eq(name, start, 'exists')
		}
		7 {
			name_part_eq(name, start, 'reserve')
		}
		9 {
			name_part_eq(name, start, 'get_check')
		}
		else {
			false
		}
	}
}

fn name_part_eq(name string, start int, expected string) bool {
	if name.len - start != expected.len {
		return false
	}
	for i in 0 .. expected.len {
		if name[start + i] != expected[i] {
			return false
		}
	}
	return true
}

// resolve_smartcast_sum_receiver_method supports resolve_smartcast_sum_receiver_method handling.
fn (t &Transformer) resolve_smartcast_sum_receiver_method(base_id flat.NodeId, method string) ?string {
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	variant := t.resolve_variant(sc.sum_type_name, sc.variant_name)
	mut receiver_types := []string{}
	receiver_types << variant
	original_type := t.trim_pointer_type(t.original_expr_type(base_id))
	if original_type.len > 0 && original_type !in receiver_types {
		receiver_types << original_type
	}
	if sc.sum_type_name.len > 0 && sc.sum_type_name !in receiver_types {
		receiver_types << sc.sum_type_name
	}
	sum_type := t.resolve_sum_name(sc.sum_type_name)
	if sum_type.len > 0 && sum_type !in receiver_types {
		receiver_types << sum_type
	}
	for parent in t.sum_type_parents_for_variant(sc.variant_name) {
		if parent !in receiver_types {
			receiver_types << parent
		}
	}
	for receiver_type in receiver_types {
		for candidate in t.receiver_method_candidates(receiver_type, method) {
			if t.is_known_fn_name(candidate) {
				return candidate
			}
		}
	}
	return none
}

// sum_type_parents_for_variant supports sum type parents for variant handling for Transformer.
fn (t &Transformer) sum_type_parents_for_variant(variant string) []string {
	if parents := t.sum_variant_parents[variant] {
		return parents.clone()
	}
	short := t.variant_short_name(variant)
	if short != variant {
		if parents := t.sum_variant_parents[short] {
			return parents.clone()
		}
	}
	mut result := []string{}
	for sum_name, variants in t.sum_types {
		for v in variants {
			short_v := t.variant_short_name(v)
			if v == variant || short_v == short {
				result << sum_name
				break
			}
		}
	}
	if !isnil(t.tc) {
		for sum_name, variants in t.tc.sum_types {
			if sum_name in result {
				continue
			}
			for v in variants {
				short_v := t.variant_short_name(v)
				if v == variant || short_v == short {
					result << sum_name
					break
				}
			}
		}
	}
	return result
}

// receiver_method_candidates supports receiver method candidates handling for Transformer.
fn (t &Transformer) receiver_method_candidates(receiver_type string, method string) []string {
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		return t.map_receiver_method_candidates(clean_type, method)
	}
	mut candidates := []string{}
	candidates << '${clean_type}.${method}'
	for receiver in generic_receiver_flat_type_variants(clean_type) {
		candidates << '${receiver}.${method}'
	}
	for receiver in flattened_generic_receiver_short_variants(clean_type) {
		candidates << '${receiver}.${method}'
	}
	if clean_type.starts_with('[]') {
		elem_type := clean_type[2..]
		short_elem := if elem_type.contains('.') { elem_type.all_after_last('.') } else { elem_type }
		candidates << '[]${short_elem}.${method}'
		if elem_type.contains('.') {
			candidates << '${elem_type.all_before_last('.')}.[]${short_elem}.${method}'
		} else if transform_can_prefix_collection_receiver(t.cur_module) {
			candidates << '${t.cur_module}.[]${short_elem}.${method}'
		}
	} else if clean_type.contains('.') {
		short_type := clean_type.all_after_last('.')
		candidates << '${short_type}.${method}'
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${clean_type}.${method}'
	}
	return candidates
}

fn generic_receiver_flat_type_variants(receiver_type string) []string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 {
		return []string{}
	}
	suffix := generic_type_suffixes(args)
	if suffix.len == 0 {
		return []string{}
	}
	mut receivers := []string{}
	transform_push_receiver_candidate(mut receivers, '${base}_${suffix}')
	if base.contains('.') {
		short_base := base.all_after_last('.')
		transform_push_receiver_candidate(mut receivers, '${short_base}_${suffix}')
		transform_push_receiver_candidate(mut receivers,
			'${base.all_before_last('.')}.${short_base}_${suffix}')
	}
	return receivers
}

fn flattened_generic_receiver_short_variants(receiver_type string) []string {
	clean := receiver_type.trim_space()
	if clean.len == 0 || !clean.contains('__') || !clean.contains('_') {
		return []string{}
	}
	module_name := if clean.contains('.') { clean.all_before_last('.') } else { '' }
	leaf := if clean.contains('.') { clean.all_after_last('.') } else { clean }
	parts := flattened_generic_receiver_leaf_parts(leaf)
	mut changed := false
	mut short_parts := []string{cap: parts.len}
	for part in parts {
		if part.contains('__') {
			short_parts << part.all_after_last('__')
			changed = true
		} else {
			short_parts << part
		}
	}
	if !changed {
		return []string{}
	}
	short_leaf := short_parts.join('_')
	mut variants := [short_leaf]
	if module_name.len > 0 {
		variants << '${module_name}.${short_leaf}'
	}
	return variants
}

fn flattened_generic_receiver_leaf_parts(leaf string) []string {
	mut parts := []string{}
	mut start := 0
	mut i := 0
	for i < leaf.len {
		if leaf[i] == `_` {
			if i + 1 < leaf.len && leaf[i + 1] == `_` {
				i += 2
				continue
			}
			parts << leaf[start..i]
			i++
			start = i
			continue
		}
		i++
	}
	parts << leaf[start..]
	return parts
}

fn (t &Transformer) resolve_fixed_array_dynamic_receiver_method(fixed_type string, method string) ?string {
	elem_type := fixed_array_outer_elem_type(fixed_type)
	if elem_type.len == 0 {
		return none
	}
	return t.resolve_receiver_method_for_type('[]${elem_type}', method)
}

fn (mut t Transformer) lower_fixed_array_dynamic_receiver_method_call(node flat.Node, base_id flat.NodeId, fixed_type string, method_name string) flat.NodeId {
	elem_type := fixed_array_outer_elem_type(fixed_type)
	array_type := '[]${elem_type}'
	tmp_name := t.new_temp('fixed_arr')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.fixed_array_value_to_array(base_id,
		fixed_type, array_type), array_type)
	args := t.transform_receiver_method_args_with_base(node, t.make_ident(tmp_name), method_name)
	ret_type := t.receiver_method_return_type(method_name, node.typ)
	t.mark_fn_used(method_name)
	return t.make_call_typed(method_name, args, ret_type)
}

fn fixed_array_outer_elem_type(type_text string) string {
	clean := type_text.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[bracket_end + 1..]
		}
		return ''
	}
	elem, dims := transform_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return fixed_array_elem_type(clean)
	}
	mut out := elem
	for i := dims.len - 1; i > 0; i-- {
		out += '[${dims[i]}]'
	}
	return out
}

fn transform_push_receiver_candidate(mut candidates []string, candidate string) {
	if candidate.len > 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn transform_can_prefix_collection_receiver(module_name string) bool {
	return module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
}

fn (t &Transformer) receiver_type_text_variants(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	transform_push_receiver_candidate(mut names, clean)
	transform_push_receiver_candidate(mut names, receiver_type_text_short_spelling(clean))
	if t.is_fixed_array_type(clean) {
		source := t.receiver_type_text_source_fixed_spelling(clean)
		transform_push_receiver_candidate(mut names, source)
		transform_push_receiver_candidate(mut names, receiver_type_text_short_spelling(source))
	}
	return names
}

fn (t &Transformer) receiver_type_text_source_fixed_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.len == 0 || clean.starts_with('[') || !t.is_fixed_array_type(clean) {
		return clean
	}
	elem, dims := transform_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return clean
	}
	mut source := elem
	for i := dims.len; i > 0; i-- {
		source = '[${dims[i - 1]}]${source}'
	}
	return source
}

fn transform_postfix_fixed_array_parts(type_text string) (string, []string) {
	clean := type_text.trim_space()
	mut end := clean.len
	mut dims := []string{}
	for end > 0 && clean[end - 1] == `]` {
		start := transform_trailing_matching_bracket_start(clean, end)
		if start < 0 {
			break
		}
		dims << clean[start + 1..end - 1].trim_space()
		end = start
	}
	return clean[..end], dims
}

fn transform_trailing_matching_bracket_start(s string, end int) int {
	mut depth := 0
	for i := end - 1; i >= 0; i-- {
		if s[i] == `]` {
			depth++
		} else if s[i] == `[` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn receiver_type_text_short_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.starts_with('[]') {
		return '[]' + receiver_type_text_short_spelling(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + receiver_type_text_short_spelling(clean[bracket_end +
				1..])
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := receiver_type_text_short_spelling(clean[4..bracket_end])
			value := receiver_type_text_short_spelling(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.contains('.') {
		return clean.all_after_last('.')
	}
	return clean
}

fn (t &Transformer) receiver_type_text_module_names(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	if clean.starts_with('[]') {
		return t.receiver_type_text_module_names(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.receiver_type_text_module_names(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		key_type := t.map_key_type(clean)
		value_type := t.map_value_type(clean)
		for name in t.receiver_type_text_module_names(key_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		for name in t.receiver_type_text_module_names(value_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		return names
	}
	if t.is_fixed_array_type(clean) {
		return t.receiver_type_text_module_names(fixed_array_elem_type(clean))
	}
	if clean.contains('.') {
		transform_push_receiver_candidate(mut names, clean.all_before_last('.'))
	}
	return names
}

// map_receiver_method_candidates supports map receiver method candidates handling for Transformer.
fn (t &Transformer) map_receiver_method_candidates(receiver_type string, method string) []string {
	clean_type := t.clean_map_type(receiver_type)
	key_type := t.map_key_type(clean_type)
	value_type := t.map_value_type(clean_type)
	mut candidates := []string{}
	if key_type.len == 0 || value_type.len == 0 {
		transform_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
		return candidates
	}
	key_types := t.receiver_type_text_variants(key_type)
	value_types := t.receiver_type_text_variants(value_type)
	mut map_types := []string{}
	for key in key_types {
		for value in value_types {
			transform_push_receiver_candidate(mut map_types, 'map[${key}]${value}')
		}
	}
	for map_type in map_types {
		transform_push_receiver_candidate(mut candidates, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if transform_can_prefix_collection_receiver(t.cur_module) {
		transform_push_receiver_candidate(mut module_names, t.cur_module)
	}
	for mod_name in t.receiver_type_text_module_names(key_type) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in t.receiver_type_text_module_names(value_type) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			transform_push_receiver_candidate(mut candidates, '${mod_name}.${map_type}.${method}')
		}
	}
	return candidates
}

// transform_receiver_method_args transforms transform receiver method args data for transform.
fn (mut t Transformer) transform_receiver_method_args(node flat.Node, base_id flat.NodeId, method_name string) []flat.NodeId {
	return t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id,
		method_name), method_name)
}

// transform_receiver_method_args_with_base
// transforms helper data for transform.
fn (mut t Transformer) transform_receiver_method_args_with_base(node flat.Node, base flat.NodeId, method_name string) []flat.NodeId {
	mut args := []flat.NodeId{cap: int(node.children_count)}
	args << base
	params := t.call_param_types(method_name)
	param_offset := t.receiver_method_param_offset(base, node, params, method_name)
	explicit_args := int(node.children_count) - 1
	expected_explicit := params.len - param_offset
	is_variadic := t.call_is_variadic(method_name) || (params.len > 0
		&& params[params.len - 1] is types.Array && explicit_args > expected_explicit)
	variadic_idx := if is_variadic && params.len > 0 && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	mut i := 1
	for i < node.children_count {
		param_idx := (args.len - 1) + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := if param_idx < params.len { params[param_idx].name() } else { '' }
		if arg_node.kind == .field_init {
			struct_param_type := if variadic_idx >= 0 && param_idx == variadic_idx
				&& param_type.starts_with('[]') {
				param_type[2..]
			} else {
				param_type
			}
			if packed_arg := t.transform_params_struct_call_arg(node, i, struct_param_type) {
				args << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
			if packed_arg := t.transform_struct_call_arg(node, i, struct_param_type) {
				args << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
		}
		if variadic_idx >= 0 && param_idx == variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					args << t.transform_call_arg_for_param(spread_id, param_type)
					i++
					break
				}
				remaining := int(node.children_count) - i
				if remaining == 1 {
					arg_type := t.node_type(arg_id)
					if arg_type.starts_with('[]') {
						args << t.transform_call_arg_for_param(arg_id, param_type)
					} else {
						args << t.pack_variadic_args(node, i, variadic_type.elem_type)
					}
				} else {
					args << t.pack_variadic_args(node, i, variadic_type.elem_type)
				}
				break
			}
		}
		args << t.transform_call_arg_for_param(arg_id, param_type)
		i++
	}
	if variadic_idx >= 0 && explicit_args == variadic_idx - param_offset {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			args << t.pack_variadic_args(node, int(node.children_count), variadic_type.elem_type)
		}
	}
	t.append_missing_params_struct_args(mut args, params, param_offset)
	return args
}

// receiver_method_param_offset supports receiver method param offset handling for Transformer.
fn (t &Transformer) receiver_method_param_offset(base_id flat.NodeId, node flat.Node, params []types.Type, method_name string) int {
	if params.len == 0 {
		return 0
	}
	base_type := t.node_type(base_id)
	first_type := t.normalize_type_alias(params[0].name())
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if receiver_param_types_match(clean_first, clean_base)
		|| receiver_param_matches_method_name(clean_first, method_name)
		|| t.normalize_type_alias(clean_first) == t.normalize_type_alias(clean_base)
		|| clean_first.all_after_last('.') == clean_base.all_after_last('.') {
		return 1
	}
	if params.len >= int(node.children_count) {
		return 1
	}
	return 0
}

fn receiver_param_matches_method_name(first string, method_name string) bool {
	if first.len == 0 || method_name.len == 0 || !method_name.contains('.') {
		return false
	}
	mut receiver := method_name.all_before_last('.')
	if receiver.len == 0 {
		return false
	}
	if receiver.starts_with('&') {
		receiver = receiver[1..]
	}
	mut first_base := first
	first_generic_base, _, first_is_generic := generic_app_parts(first)
	if first_is_generic {
		first_base = first_generic_base
	}
	mut receiver_base := receiver
	receiver_generic_base, _, receiver_is_generic := generic_app_parts(receiver)
	if receiver_is_generic {
		receiver_base = receiver_generic_base
	}
	first_short := first_base.all_after_last('.')
	receiver_short := receiver_base.all_after_last('.')
	return first_base == receiver_base || first_short == receiver_short
		|| receiver_short.starts_with('${first_short}_')
		|| c_name(receiver_base).starts_with('${c_name(first_base)}_')
}

fn receiver_param_types_match(first string, base string) bool {
	if first == base {
		return true
	}
	first_base, first_args, first_ok := generic_app_parts(first)
	base_base, base_args, base_ok := generic_app_parts(base)
	if !first_ok || !base_ok || first_args.len != base_args.len {
		return false
	}
	if first_base != base_base && first_base.all_after_last('.') != base_base.all_after_last('.') {
		return false
	}
	for i, first_arg in first_args {
		base_arg := base_args[i]
		if first_arg == base_arg {
			continue
		}
		if generic_type_arg_short(first_arg) == generic_type_arg_short(base_arg) {
			continue
		}
		if c_name(first_arg) == c_name(base_arg) {
			continue
		}
		return false
	}
	return true
}

// try_lower_string_method_call supports try lower string method call handling for Transformer.
fn (mut t Transformer) try_lower_string_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	if method == 'count' && node.children_count == 2 && t.expr_uses_ident(t.a.child(&node, 1), 'it') {
		return t.lower_string_count_call(node, fn_node)
	}
	if method !in ['replace', 'replace_once', 'trim', 'trim_left', 'trim_right', 'all_before',
		'all_after', 'all_before_last', 'all_after_last', 'contains', 'starts_with', 'ends_with',
		'bytes', 'substr', 'substr_unsafe', 'repeat', 'plus_two'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	if base_type != 'string' {
		return none
	}
	mut args := []flat.NodeId{cap: int(node.children_count)}
	args << t.transform_expr(base_id)
	for i in 1 .. node.children_count {
		args << t.transform_expr(t.a.child(&node, i))
	}
	ret_type := match method {
		'contains', 'starts_with', 'ends_with' { 'bool' }
		'bytes' { '[]u8' }
		else { 'string' }
	}

	return t.make_call_typed('string__${method}', args, ret_type)
}

// lower_string_count_call builds lower string count call data for transform.
fn (mut t Transformer) lower_string_count_call(node flat.Node, fn_node flat.Node) ?flat.NodeId {
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	if base_type != 'string' {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), 'int')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.make_index(base, t.make_ident(idx_name), 'u8')
	elem_decl := t.make_decl_assign_typed('it', elem_expr, 'u8')
	predicate_id := t.a.child(&node, 1)
	old_it := t.var_type('it')
	t.set_var_type('it', 'u8')
	predicate := t.transform_expr(predicate_id)
	if old_it.len > 0 {
		t.set_var_type('it', old_it)
	} else {
		t.unset_var_type('it')
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
	inc := t.make_assign_op(t.make_ident(result_name), t.make_int_literal(1), .plus_assign)
	loop_body << t.make_if(predicate, t.make_block(arr1(inc)), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// expr_uses_ident supports expr uses ident handling for Transformer.
fn (t &Transformer) expr_uses_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.expr_uses_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

// is_method_call checks if a .call node is a method call (child[0] is .selector).
// Returns true for `obj.method(args)` patterns.
fn (mut t Transformer) is_method_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	return fn_node.kind == .selector
}

// get_call_return_type looks up the return type for a resolved call.
// Handles both checker-resolved calls and transform-time name resolution.
fn (t &Transformer) get_call_return_type(id flat.NodeId, node flat.Node) string {
	if t.is_local_fn_value_call(node) {
		return ''
	}
	if ret := t.current_generic_receiver_call_return_type(node) {
		return ret
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			receiver_type := t.node_type(t.a.child(fn_node, 0)).trim_string_left('&')
			if args := t.current_specialized_receiver_args(receiver_type) {
				receiver_base, _, is_generic_receiver := generic_app_parts(receiver_type)
				if is_generic_receiver {
					mut names := ['${receiver_base}.${fn_node.value}']
					if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin']
						&& !receiver_base.contains('.') {
						names << '${t.cur_module}.${receiver_base}.${fn_node.value}'
					}
					for name in names {
						if ret := t.fn_ret_types[name] {
							return substitute_generic_type_text(ret, args)
						}
						if !isnil(t.tc) {
							if ret := t.tc.fn_ret_types[name] {
								return substitute_generic_type_text(ret.name(), args)
							}
						}
					}
				}
			}
		}
		if fn_node.kind == .ident && t.var_type(fn_node.value).len == 0 {
			qualified_name := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin']
				&& !fn_node.value.contains('.') {
				'${t.cur_module}.${fn_node.value}'
			} else {
				fn_node.value
			}
			for name in [fn_node.value, qualified_name] {
				if !isnil(t.tc) && t.tc.specialized_generic_fns[name] {
					if ret := t.fn_ret_types[name] {
						return t.call_return_type_name(ret, node)
					}
					if ret := t.tc.fn_ret_types[name] {
						return t.call_return_type_name(ret.name(), node)
					}
				}
			}
		}
	}
	if ret := t.checker_resolved_non_builtin_return_type(id, node) {
		return ret
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident {
			local_type := t.var_type(fn_node.value)
			if local_type.len > 0 {
				if ret := t.local_fn_value_return_type_from_type(local_type) {
					return t.call_return_type_name(ret, node)
				}
			} else {
				if ret := t.local_fn_decl_return_type(fn_node.value) {
					return t.call_return_type_name(ret, node)
				}
			}
		}
		if fn_node.kind == .selector
			&& fn_node.value in ['clone', 'reverse', 'repeat', 'repeat_to_depth']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type := t.node_type(base_id)
			clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if fn_node.value == 'clone' && clean_type.len > 0 {
				return clean_type
			}
			if clean_type.starts_with('[]') {
				return clean_type
			}
		}
		if fn_node.kind == .selector && fn_node.value == 'wait' && fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type := t.node_type(base_id)
			clean_type := t.membership_container_type(base_type)
			if clean_type.starts_with('[]') {
				elem_type := clean_type[2..]
				if wait_type := thread_array_wait_return_type(elem_type) {
					return wait_type
				}
			}
			if t.is_fixed_array_type(clean_type) {
				elem_type := fixed_array_elem_type(clean_type)
				if wait_type := thread_array_wait_return_type(elem_type) {
					return wait_type
				}
			}
			if clean_type == 'thread' {
				return 'void'
			}
			if clean_type.starts_with('thread ') {
				return clean_type[7..]
			}
		}
		if fn_node.kind == .selector && fn_node.value in ['keys', 'values']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			mut base_type := t.node_type(base_id)
			if base_type.len == 0 {
				base_type = t.checker_node_type(base_id)
			}
			clean_type := t.clean_map_type(base_type)
			if clean_type.starts_with('map[') {
				elem_type := if fn_node.value == 'keys' {
					t.map_key_type(clean_type)
				} else {
					t.map_value_type(clean_type)
				}
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
		}
		// A method on a concrete generic instance (`Box[int].clone`) is registered under
		// the open form (`Box[T].clone`), whose stored return type collapsed `Box[T]` to
		// the bare base. Resolve it through the checker, which re-substitutes the concrete
		// arguments from the signature text, so the inferred decl type is `Box[int]`.
		if !isnil(t.tc) && fn_node.kind == .selector && fn_node.children_count > 0 {
			base_type := t.node_type(t.a.child(fn_node, 0))
			clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if clean_base.contains('[') && clean_base.ends_with(']') {
				if ci := t.tc.resolve_generic_struct_method(clean_base, fn_node.value) {
					rn := ci.return_type.name()
					if rn.len > 0 && rn != 'void' && rn != 'unknown' {
						return t.normalize_type_alias(rn)
					}
				}
			}
		}
	}
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			if ret := t.tc.fn_ret_types[name] {
				return t.call_return_type_name(ret.name(), node)
			}
		}
	}
	name := t.resolve_call_name(node)
	if name.len == 0 {
		return ''
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[name] {
			return t.call_return_type_name(ret.name(), node)
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			if ret := t.tc.fn_ret_types[qname] {
				return t.call_return_type_name(ret.name(), node)
			}
		}
	}
	// Try exact name first
	if ret := t.fn_ret_types[name] {
		return t.call_return_type_name(ret, node)
	}
	// Try qualified with current module
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if ret := t.fn_ret_types[qname] {
			return t.call_return_type_name(ret, node)
		}
	}
	return ''
}

fn (t &Transformer) current_generic_receiver_call_return_type(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	current_receiver := t.current_fn_receiver_type()
	current_generic_base, generic_args, current_ok := generic_app_parts(current_receiver)
	current_args := if current_ok {
		generic_args
	} else {
		t.recorded_generic_specialization_args(current_receiver) or { return none }
	}
	if current_args.len == 0 || t.generic_args_have_placeholders(current_args) {
		return none
	}
	current_base := if current_ok { current_generic_base } else { current_receiver }
	receiver_type := t.node_type(t.a.child(callee, 0)).trim_string_left('&')
	if receiver_type.len > 0 {
		receiver_base, _, receiver_ok := generic_app_parts(receiver_type)
		if !receiver_ok || !current_receiver_matches_open_generic_base(current_base, receiver_base) {
			return none
		}
	}
	for name, ret in t.fn_ret_types {
		if !name.ends_with('.${callee.value}') {
			continue
		}
		decl_receiver := name.all_before_last('.')
		generic_decl_base, _, decl_ok := generic_app_parts(decl_receiver)
		decl_base := if decl_ok { generic_decl_base } else { decl_receiver }
		if !current_receiver_matches_open_generic_base(current_base, decl_base) {
			continue
		}
		resolved := substitute_generic_type_text(ret, current_args)
		if resolved.len > 0 && !t.generic_args_have_placeholders([resolved]) {
			return resolved
		}
	}
	return none
}

fn (t &Transformer) checker_resolved_non_builtin_return_type(id flat.NodeId, node flat.Node) ?string {
	if isnil(t.tc) {
		return none
	}
	name := t.tc.resolved_call_name(id) or { return none }
	if is_builtin_collection_resolved_call(name) {
		return none
	}
	if typ := t.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			return t.call_return_type_name(typ.name(), node)
		}
	}
	ret := t.tc.fn_ret_types[name] or { return none }
	return t.call_return_type_name(ret.name(), node)
}

// call_return_type_name updates call return type name state for Transformer.
fn (t &Transformer) call_return_type_name(ret_name string, node flat.Node) string {
	mut typ := ret_name
	if node.value.len > 0 {
		generic_arg := t.normalize_type_in_module(node.value, t.cur_module)
		if generic_arg.len > 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg)
		}
	}
	if t.is_optional_type_name(typ) {
		return typ
	}
	return t.normalize_type_alias(typ)
}

// specialize_generic_type_name supports specialize generic type name handling for Transformer.
fn (t &Transformer) specialize_generic_type_name(typ string, generic_arg string) string {
	clean := typ.trim_space()
	if clean.len == 0 || generic_arg.len == 0 {
		return typ
	}
	if clean == 'T' {
		return generic_arg
	}
	if clean.starts_with('&') {
		return '&' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('[]') {
		return '[]' + t.specialize_generic_type_name(clean[2..], generic_arg)
	}
	if clean.starts_with('?') {
		return '?' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('!') {
		return '!' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('...') {
		return '...' + t.specialize_generic_type_name(clean[3..], generic_arg)
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return typ }
		key_type := t.specialize_generic_type_name(clean[4..bracket_end], generic_arg)
		value_type := t.specialize_generic_type_name(clean[bracket_end + 1..], generic_arg)
		return 'map[${key_type}]${value_type}'
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return typ }
		return clean[..bracket_end + 1] + t.specialize_generic_type_name(clean[bracket_end +
			1..], generic_arg)
	}
	return typ
}
