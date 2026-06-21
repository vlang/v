module transform

import v3.flat

fn (mut t Transformer) transform_infix_string_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	match node.op {
		.plus, .eq, .ne, .lt, .gt, .le, .ge {}
		else {
			return none
		}
	}

	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]

	is_string := t.is_string_type(lhs_id) || t.is_string_type(rhs_id)

	if !is_string {
		return none
	}

	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(rhs_id)

	match node.op {
		.plus {
			lhs := t.a.nodes[int(new_lhs)]
			rhs := t.a.nodes[int(new_rhs)]
			if lhs.kind == .string_literal && rhs.kind == .string_literal {
				return t.make_string_literal(lhs.value + rhs.value)
			}
			return t.make_call('string__plus', arr2(new_lhs, new_rhs))
		}
		.eq {
			return t.make_call('string__eq', arr2(new_lhs, new_rhs))
		}
		.ne {
			eq_call := t.make_call('string__eq', arr2(new_lhs, new_rhs))
			start := t.a.children.len
			t.a.children << eq_call
			return t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		.lt {
			return t.make_call('string__lt', arr2(new_lhs, new_rhs))
		}
		.gt {
			// a > b  ->  string__lt(b, a)
			return t.make_call('string__lt', arr2(new_rhs, new_lhs))
		}
		.le {
			// a <= b  ->  !(b < a)  ->  !string__lt(rhs, lhs)
			lt_call := t.make_call('string__lt', arr2(new_rhs, new_lhs))
			start := t.a.children.len
			t.a.children << lt_call
			return t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		.ge {
			// a >= b  ->  !(a < b)  ->  !string__lt(lhs, rhs)
			lt_call := t.make_call('string__lt', arr2(new_lhs, new_rhs))
			start := t.a.children.len
			t.a.children << lt_call
			return t.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: start
				children_count: 1
			})
		}
		else {
			return none
		}
	}
}

fn (mut t Transformer) transform_infix_array_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_type := t.membership_container_type(t.node_type(lhs_id))
	rhs_type := t.membership_container_type(t.node_type(rhs_id))
	if !(lhs_type.starts_with('[]') || lhs_type == 'array') || !(rhs_type.starts_with('[]')
		|| rhs_type == 'array') {
		return none
	}
	mut elem_type := ''
	if lhs_type.starts_with('[]') {
		elem_type = lhs_type[2..]
	} else if rhs_type.starts_with('[]') {
		elem_type = rhs_type[2..]
	}
	if elem_type.len == 0 {
		elem_type = 'int'
	}
	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(rhs_id)
	eq_call := if elem_type == 'string' {
		t.make_call_typed('array_eq_string', arr2(new_lhs, new_rhs), 'bool')
	} else {
		t.make_call_typed('array_eq_raw', arr3(new_lhs, new_rhs, t.make_sizeof_type(elem_type)),
			'bool')
	}
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (mut t Transformer) transform_infix_map_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 || node.op !in [.eq, .ne] {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_type := t.node_type(lhs_id)
	rhs_type := t.node_type(rhs_id)
	lhs_map_type := t.clean_map_type(lhs_type)
	rhs_map_type := t.clean_map_type(rhs_type)
	if !lhs_map_type.starts_with('map[') || !rhs_map_type.starts_with('map[') {
		return none
	}
	mut new_lhs := t.transform_expr(lhs_id)
	mut new_rhs := t.transform_expr(rhs_id)
	if lhs_type.starts_with('&') {
		new_lhs = t.make_prefix(.mul, new_lhs)
	}
	if rhs_type.starts_with('&') {
		new_rhs = t.make_prefix(.mul, new_rhs)
	}
	eq_call := t.make_call_typed('map_map_eq', arr2(new_lhs, new_rhs), 'bool')
	if node.op == .ne {
		return t.make_prefix(.not, eq_call)
	}
	return eq_call
}

fn (mut t Transformer) transform_infix_struct_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	mut lhs_type := t.node_type(lhs_id)
	if lhs_type.starts_with('&') {
		return none
	}
	struct_type := t.struct_lookup_name(lhs_type)
	if struct_type.len == 0 {
		return none
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		lhs := t.transform_expr(lhs_id)
		rhs := t.transform_expr(t.a.children[node.children_start + 1])
		mut call_lhs := lhs
		mut call_rhs := rhs
		if call_info.reverse {
			call_lhs = t.stable_transformed_expr_for_reuse(lhs, lhs_type, 'op_lhs')
			call_rhs = t.stable_transformed_expr_for_reuse(rhs, t.node_type(t.a.children[
				node.children_start + 1]), 'op_rhs')
		}
		args := if call_info.reverse {
			arr2(call_rhs, call_lhs)
		} else {
			arr2(call_lhs, call_rhs)
		}
		call := t.make_call_typed(call_info.name, args, node.typ)
		if call_info.negate {
			return t.make_prefix(.not, call)
		}
		return call
	}
	if node.op != .eq && node.op != .ne {
		return none
	}
	if !t.has_struct_operator_fn(struct_type, '==') {
		lhs := t.stable_expr_for_reuse(lhs_id)
		rhs := t.stable_expr_for_reuse(t.a.children[node.children_start + 1])
		cmp := t.make_call_typed('memcmp', arr3(t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
			t.make_sizeof_type(struct_type)), 'int')
		return t.make_infix(node.op, cmp, t.make_int_literal(0))
	}
	if eq_fn := t.struct_operator_fn_name(struct_type, '==') {
		lhs := t.transform_expr(lhs_id)
		rhs := t.transform_expr(t.a.children[node.children_start + 1])
		eq_call := t.make_call_typed(eq_fn, arr2(lhs, rhs), 'bool')
		if node.op == .ne {
			return t.make_prefix(.not, eq_call)
		}
		return eq_call
	}
	return none
}

struct StructOperatorCallInfo {
	name    string
	reverse bool
	negate  bool
}

fn (t &Transformer) struct_operator_call_info(struct_type string, op flat.Op) ?StructOperatorCallInfo {
	if op_name := struct_operator_symbol(op) {
		if method_name := t.struct_operator_fn_name(struct_type, op_name) {
			return StructOperatorCallInfo{
				name: method_name
			}
		}
	}
	match op {
		.gt {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
				}
			}
		}
		.ge {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		.le {
			if method_name := t.struct_operator_fn_name(struct_type, '<') {
				return StructOperatorCallInfo{
					name:    method_name
					reverse: true
					negate:  true
				}
			}
		}
		.ne {
			if method_name := t.struct_operator_fn_name(struct_type, '==') {
				return StructOperatorCallInfo{
					name:   method_name
					negate: true
				}
			}
		}
		else {}
	}

	return none
}

fn struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.div { return '/' }
		.mod { return '%' }
		.eq { return '==' }
		.ne { return '!=' }
		.lt { return '<' }
		.gt { return '>' }
		.le { return '<=' }
		.ge { return '>=' }
		else {}
	}

	return none
}

fn (t &Transformer) struct_operator_fn_name(struct_type string, op_name string) ?string {
	method_name := '${struct_type}.${op_name}'
	if t.is_known_fn_name(method_name) {
		return method_name
	}
	cmethod_name := c_name(method_name)
	if t.is_known_fn_name(cmethod_name) {
		return cmethod_name
	}
	return none
}

fn (t &Transformer) has_struct_operator_fn(struct_type string, op_name string) bool {
	if _ := t.struct_operator_fn_name(struct_type, op_name) {
		return true
	}
	return false
}

fn (t &Transformer) struct_operator_return_type(fn_name string) string {
	if ret := t.fn_ret_types[fn_name] {
		return t.normalize_type_alias(ret)
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[fn_name] {
			return t.normalize_type_alias(ret.name())
		}
	}
	return ''
}

fn (t &Transformer) infix_struct_operator_result_type(node flat.Node) string {
	if node.children_count < 2 {
		return ''
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.node_type(lhs_id)
	if lhs_type.starts_with('&') {
		lhs_type = lhs_type[1..]
	}
	struct_type := t.struct_lookup_name(lhs_type)
	if struct_type.len == 0 {
		return ''
	}
	if call_info := t.struct_operator_call_info(struct_type, node.op) {
		ret_type := t.struct_operator_return_type(call_info.name)
		if ret_type.len > 0 {
			return ret_type
		}
	}
	return ''
}

fn (mut t Transformer) transform_infix_sum_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs_raw_type := t.node_type(lhs_id)
	rhs_raw_type := t.node_type(rhs_id)
	mut lhs_type := lhs_raw_type
	mut rhs_type := rhs_raw_type
	lhs_is_ptr := lhs_type.starts_with('&')
	rhs_is_ptr := rhs_type.starts_with('&')
	if lhs_is_ptr {
		lhs_type = lhs_type[1..]
	}
	if rhs_is_ptr {
		rhs_type = rhs_type[1..]
	}
	if !t.is_sum_type_name(lhs_type) || !t.is_sum_type_name(rhs_type) {
		return none
	}
	sum_type := if t.is_sum_type_name(lhs_type) {
		t.resolve_sum_name(lhs_type)
	} else if t.is_sum_type_name(rhs_type) {
		t.resolve_sum_name(rhs_type)
	} else {
		''
	}
	if sum_type.len == 0 {
		return none
	}
	mut lhs := t.stable_expr_for_reuse(lhs_id)
	mut rhs := t.stable_expr_for_reuse(rhs_id)
	if lhs_is_ptr {
		lhs = t.make_prefix(.mul, lhs)
		t.a.nodes[int(lhs)].typ = lhs_type
	}
	if rhs_is_ptr {
		rhs = t.make_prefix(.mul, rhs)
		t.a.nodes[int(rhs)].typ = rhs_type
	}
	tag_eq := t.make_infix(.eq, t.make_selector(lhs, 'typ', 'int'), t.make_selector(rhs, 'typ',
		'int'))
	cmp := t.make_call_typed('memcmp', arr3(t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
		t.make_sizeof_type(sum_type)), 'int')
	value_eq := t.make_infix(.eq, cmp, t.make_int_literal(0))
	eq := t.make_infix(.logical_and, tag_eq, value_eq)
	if node.op == .ne {
		return t.make_prefix(.not, t.make_paren(eq))
	}
	return eq
}

fn (mut t Transformer) transform_infix_optional_none_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	lhs := t.a.nodes[int(lhs_id)]
	rhs := t.a.nodes[int(rhs_id)]
	mut opt_id := flat.empty_node
	if lhs.kind == .none_expr {
		opt_id = rhs_id
	} else if rhs.kind == .none_expr {
		opt_id = lhs_id
	} else {
		return none
	}
	opt_type := t.node_type(opt_id)
	if !t.is_optional_type_name(opt_type) {
		return none
	}
	ok := t.make_selector(t.transform_expr(opt_id), 'ok', 'bool')
	if node.op == .eq {
		return t.make_prefix(.not, ok)
	}
	return ok
}

fn (t &Transformer) struct_lookup_name(type_name string) string {
	if type_name.len == 0 {
		return ''
	}
	if type_name.contains('.') {
		if type_name in t.structs {
			return type_name
		}
		short_type := type_name.all_after_last('.')
		type_mod := type_name.all_before_last('.')
		if info := t.structs[short_type] {
			if info.module == type_mod {
				return short_type
			}
		}
		if info := t.structs[type_name] {
			if info.module == type_mod {
				return type_name
			}
		}
		return ''
	}
	if type_name in t.structs {
		if info := t.structs[type_name] {
			if info.module.len == 0 || info.module == t.cur_module {
				return type_name
			}
		}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${type_name}'
		if qtype in t.structs {
			return qtype
		}
	}
	return ''
}

fn (mut t Transformer) transform_in_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	rhs := t.a.nodes[int(rhs_id)]

	is_not_in := node.value == '!in'

	mut result := id
	if rhs.kind == .range {
		// x in low..high  ->  x >= low && x < high
		if rhs.children_count >= 2 {
			new_lhs := t.stable_expr_for_reuse(lhs_id)
			low_id := t.a.children[rhs.children_start]
			high_id := t.a.children[rhs.children_start + 1]
			new_low := t.transform_expr(low_id)
			new_high := t.transform_expr(high_id)

			ge_cmp := t.make_infix(.ge, new_lhs, new_low)
			lt_cmp := t.make_infix(.lt, new_lhs, new_high)
			result = t.make_infix(.logical_and, ge_cmp, lt_cmp)
		}
	} else if rhs.kind == .array_literal {
		if type_checks := t.lower_type_pattern_membership(lhs_id, rhs, is_not_in) {
			return type_checks
		}
		// x in [a, b, c]  ->  x == a || x == b || x == c
		if rhs.children_count == 0 {
			result = t.make_bool_literal(false)
		} else {
			new_lhs := t.stable_expr_for_reuse(lhs_id)
			is_str := t.is_string_type(lhs_id)
			mut or_chain := flat.empty_node
			for i in 0 .. rhs.children_count {
				elem_id := t.a.children[rhs.children_start + i]
				new_elem := t.transform_expr(elem_id)
				eq_cmp := if is_str {
					t.make_call('string__eq', arr2(new_lhs, new_elem))
				} else {
					t.make_infix(.eq, new_lhs, new_elem)
				}
				if int(or_chain) < 0 {
					or_chain = eq_cmp
				} else {
					or_chain = t.make_infix(.logical_or, or_chain, eq_cmp)
				}
			}
			result = or_chain
		}
	} else {
		rhs_type := t.node_type(rhs_id)
		clean_rhs_type := t.membership_container_type(rhs_type)
		rhs_is_ptr_array := t.membership_container_is_pointer_array(rhs_type)
		if clean_rhs_type.starts_with('[]') || clean_rhs_type == 'array' {
			if lowered := t.lower_array_membership_expr(rhs_id, lhs_id, rhs_type, false, node) {
				result = lowered
			} else {
				// dynamic array membership -> array_contains_int/string(arr, val)
				new_lhs := t.transform_expr(lhs_id)
				mut new_rhs := t.transform_expr(rhs_id)
				if rhs_is_ptr_array {
					new_rhs = t.make_prefix(.mul, new_rhs)
				}
				mut elem := if clean_rhs_type.starts_with('[]') { clean_rhs_type[2..] } else { '' }
				if elem.len == 0 {
					elem = t.node_type(lhs_id)
				}
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
			}
		} else if rhs.kind in [.ident, .selector] && (rhs_type.len == 0 || rhs_type == 'unknown') {
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			mut elem := t.node_type(lhs_id)
			lhs := t.a.nodes[int(lhs_id)]
			if elem.len == 0 && lhs.kind == .selector {
				elem = t.selector_field_type(lhs)
			}
			if elem.len == 0 && rhs.kind == .selector {
				elem = t.selector_array_elem_type(rhs)
			}
			if elem.len > 0 {
				fn_name := array_contains_fn_name(elem)
				result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
			}
		} else if is_fixed_array_type(clean_rhs_type) {
			// fixed array membership -> fixed_array_contains_int/string(arr, len, val)
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			elem := fixed_array_elem_type(clean_rhs_type)
			fn_name := fixed_array_contains_fn_name(elem)
			len_expr := t.make_fixed_array_len_expr(clean_rhs_type)
			result = t.make_call_typed(fn_name, arr3(new_rhs, len_expr, new_lhs), 'bool')
		} else if clean_rhs_type == 'string' {
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			fn_name := if t.node_type(lhs_id) in ['u8', 'byte'] {
				'string__contains_u8'
			} else {
				'string__contains'
			}
			result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
		} else if clean_rhs_type.starts_with('map[') || clean_rhs_type == 'map' {
			if lowered := t.lower_map_membership_expr(rhs_id, lhs_id, rhs_type) {
				result = lowered
			}
		} else {
			// Unknown containment is kept as in_expr so the backend can reject or
			// handle genuinely unresolved cases.
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			in_start := t.a.children.len
			t.a.children << new_lhs
			t.a.children << new_rhs
			result = t.a.add_node(flat.Node{
				kind:           .in_expr
				op:             node.op
				children_start: in_start
				children_count: 2
				pos:            node.pos
				value:          'in'
				typ:            node.typ
			})
		}
	}

	if is_not_in && result != id {
		start := t.a.children.len
		t.a.children << t.make_paren(result)
		return t.a.add_node(flat.Node{
			kind:           .prefix
			op:             .not
			children_start: start
			children_count: 1
		})
	}
	return result
}

fn (mut t Transformer) lower_type_pattern_membership(lhs_id flat.NodeId, rhs flat.Node, is_not_in bool) ?flat.NodeId {
	if rhs.children_count == 0 {
		return none
	}
	mut patterns := []string{cap: int(rhs.children_count)}
	mut sum_name := t.trim_pointer_type(t.original_expr_type(lhs_id))
	if !t.is_sum_type_name(sum_name) {
		sum_name = t.trim_pointer_type(t.node_type(lhs_id))
	}
	for i in 0 .. rhs.children_count {
		elem_id := t.a.child(&rhs, i)
		pattern := t.type_pattern_name(elem_id)
		if pattern.len == 0 {
			return none
		}
		if !t.is_sum_type_name(sum_name) {
			pattern_sum := t.find_sum_type_for_variant(pattern)
			if pattern_sum.len == 0 {
				return none
			}
			sum_name = pattern_sum
		} else if t.sum_variant_path(sum_name, pattern).len == 0 {
			return none
		}
		patterns << pattern
	}
	if !t.is_sum_type_name(sum_name) {
		return none
	}
	lhs_type := t.original_expr_type(lhs_id)
	base := t.stable_expr_for_reuse(lhs_id)
	mut chain := flat.empty_node
	for pattern in patterns {
		cmp := t.make_sum_type_pattern_check(base, lhs_type, sum_name, pattern) or { return none }
		if int(chain) < 0 {
			chain = cmp
		} else {
			chain = t.make_infix(.logical_or, chain, cmp)
		}
	}
	if is_not_in {
		return t.make_prefix(.not, t.make_paren(chain))
	}
	return chain
}

fn (t &Transformer) type_pattern_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.type_pattern_name(t.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.array_init {
			if node.value.len == 0 {
				return ''
			}
			return '[]${node.value}'
		}
		.array_literal {
			if node.value.len == 0 {
				return ''
			}
			return '[${node.children_count}]${node.value}'
		}
		else {
			return ''
		}
	}
}

fn (mut t Transformer) lower_array_membership_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type string, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type := t.membership_container_type(base_type)
	if !clean_base_type.starts_with('[]') && clean_base_type != 'array' {
		return none
	}
	mut elem_type := if clean_base_type.starts_with('[]') { clean_base_type[2..] } else { '' }
	if elem_type.len == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type.len == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
		needle = t.stable_expr_for_reuse(needle_id)
	} else {
		needle = t.stable_expr_for_reuse(needle_id)
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('contains')
	idx_name := t.new_temp('contains_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_bool_literal(false), 'bool')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	eq_expr := t.make_membership_eq_expr(elem_expr, needle, elem_type)
	assign_true := t.make_assign(t.make_ident(result_name), t.make_bool_literal(true))
	then_block := t.make_block(arr1(assign_true))
	loop_body := arr1(t.make_if(eq_expr, then_block, t.make_empty()))
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) lower_array_index_expr(base_id flat.NodeId, needle_id flat.NodeId, base_type string, receiver_first bool, src flat.Node) ?flat.NodeId {
	clean_base_type := t.membership_container_type(base_type)
	if !clean_base_type.starts_with('[]') && clean_base_type != 'array' {
		return none
	}
	mut elem_type := if clean_base_type.starts_with('[]') { clean_base_type[2..] } else { '' }
	if elem_type.len == 0 {
		elem_type = t.membership_container_type(t.node_type(needle_id))
	}
	if elem_type.len == 0 {
		return none
	}
	mut base := flat.empty_node
	mut needle := flat.empty_node
	if receiver_first {
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
		needle = t.stable_expr_for_reuse(needle_id)
	} else {
		needle = t.stable_expr_for_reuse(needle_id)
		base = t.stable_array_expr_for_membership(base_id, base_type, clean_base_type)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('index')
	idx_name := t.new_temp('index_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(-1), 'int')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	eq_expr := t.make_membership_eq_expr(elem_expr, needle, elem_type)
	not_found := t.make_infix(.lt, t.make_ident(result_name), t.make_int_literal(0))
	found_cond := t.make_infix(.logical_and, not_found, eq_expr)
	assign_idx := t.make_assign(t.make_ident(result_name), t.make_ident(idx_name))
	loop_body := arr1(t.make_if(found_cond, t.make_block(arr1(assign_idx)), t.make_empty()))
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) stable_array_expr_for_membership(id flat.NodeId, raw_type string, clean_type string) flat.NodeId {
	mut expr := t.transform_expr(id)
	if t.membership_container_is_pointer_array(raw_type) {
		expr = t.make_prefix(.mul, expr)
	}
	return t.stable_transformed_expr_for_reuse(expr, clean_type, 'in_arr')
}

fn (mut t Transformer) make_membership_eq_expr(lhs flat.NodeId, rhs flat.NodeId, elem_type string) flat.NodeId {
	mut clean := t.membership_container_type(elem_type)
	if clean == 'string' {
		return t.make_call_typed('string__eq', arr2(lhs, rhs), 'bool')
	}
	if clean.starts_with('[]') {
		inner := clean[2..]
		if inner == 'string' {
			return t.make_call_typed('array_eq_string', arr2(lhs, rhs), 'bool')
		}
		return t.make_call_typed('array_eq_raw', arr3(lhs, rhs, t.make_sizeof_type(inner)), 'bool')
	}
	struct_type := t.struct_lookup_name(clean)
	if struct_type.len > 0 {
		method_name := '${struct_type}.=='
		if t.is_known_fn_name(method_name) {
			return t.make_call(method_name, arr2(lhs, rhs))
		}
		cmp := t.make_call_typed('memcmp', arr3(t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
			t.make_sizeof_type(struct_type)), 'int')
		return t.make_infix(.eq, cmp, t.make_int_literal(0))
	}
	return t.make_infix(.eq, lhs, rhs)
}

fn (t &Transformer) selector_field_type(node flat.Node) string {
	if node.value.len == 0 {
		return ''
	}
	for _, info in t.structs {
		for field in info.fields {
			if field.name == node.value {
				return t.normalize_type_alias(field.typ)
			}
		}
	}
	return ''
}

fn (t &Transformer) selector_array_elem_type(node flat.Node) string {
	if node.value.len == 0 {
		return ''
	}
	for _, info in t.structs {
		for field in info.fields {
			clean_typ := t.membership_container_type(field.typ)
			if field.name == node.value && clean_typ.starts_with('[]') {
				return t.normalize_type_alias(clean_typ[2..])
			}
		}
	}
	return ''
}

fn (t &Transformer) membership_container_type(typ string) string {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('...') {
			clean = '[]' + clean[3..].trim_space()
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

fn (t &Transformer) membership_container_is_pointer_array(typ string) bool {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if !clean.starts_with('&') {
		return false
	}
	clean = t.membership_container_type(clean)
	return clean.starts_with('[]') || clean == 'array'
}

fn array_contains_fn_name(elem string) string {
	return match elem {
		'string' { 'array_contains_string' }
		'u8', 'byte' { 'array_contains_u8' }
		else { 'array_contains_int' }
	}
}

fn fixed_array_contains_fn_name(elem string) string {
	return match elem {
		'string' { 'fixed_array_contains_string' }
		'u8', 'byte' { 'fixed_array_contains_u8' }
		else { 'fixed_array_contains_int' }
	}
}

fn (mut t Transformer) stable_expr_for_reuse(id flat.NodeId) flat.NodeId {
	expr := t.transform_expr(id)
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp('in_lhs')
	tmp_typ := t.node_type(id)
	decl := t.make_decl_assign(tmp_name, expr)
	if tmp_typ.len > 0 {
		t.a.nodes[int(decl)].typ = tmp_typ
		t.set_var_type(tmp_name, tmp_typ)
	}
	t.pending_stmts << decl
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) stable_transformed_expr_for_reuse(expr flat.NodeId, typ string, prefix string) flat.NodeId {
	if t.is_stable_expr_for_reuse(expr) {
		return expr
	}
	tmp_name := t.new_temp(prefix)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, typ)
	return t.make_ident(tmp_name)
}

fn (t &Transformer) is_stable_expr_for_reuse(id flat.NodeId) bool {
	if int(id) < 0 {
		return true
	}
	node := t.a.nodes[int(id)]
	return match node.kind {
		.ident, .int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.nil_literal, .none_expr, .enum_val, .sizeof_expr, .typeof_expr {
			true
		}
		.selector {
			node.children_count > 0 && t.is_stable_expr_for_reuse(t.a.children[node.children_start])
		}
		.index {
			node.children_count >= 2
				&& t.is_stable_expr_for_reuse(t.a.children[node.children_start])
				&& t.is_stable_expr_for_reuse(t.a.children[node.children_start + 1])
		}
		else {
			false
		}
	}
}

fn (mut t Transformer) transform_fixed_array_len(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.value != 'len' || node.children_count == 0 {
		return none
	}
	base_id := t.a.children[node.children_start]
	base_type := t.node_type(base_id)
	if !is_fixed_array_type(base_type) {
		return none
	}
	return t.make_fixed_array_len_expr(base_type)
}

fn (t &Transformer) clean_map_type(typ string) string {
	mut clean := t.normalize_type_alias(typ).trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		break
	}
	return t.normalize_type_alias(clean)
}

fn (mut t Transformer) runtime_addr(expr flat.NodeId, typ string) flat.NodeId {
	if typ.starts_with('&') {
		return expr
	}
	if !t.expr_can_take_address(expr) {
		stable := t.stable_transformed_expr_for_reuse(expr, typ, 'addr')
		return t.make_prefix(.amp, stable)
	}
	return t.make_prefix(.amp, expr)
}

fn (mut t Transformer) transform_enum_shorthand(id flat.NodeId, node flat.Node, expected_enum string) flat.NodeId {
	if expected_enum.len == 0 {
		return id
	}
	short_name := node.value.trim_left('.')
	if fields := t.enum_types[expected_enum] {
		for f in fields {
			if f == short_name {
				return t.a.add_node(flat.Node{
					kind:  .enum_val
					value: '${expected_enum}.${short_name}'
					typ:   expected_enum
				})
			}
		}
	}
	return id
}

pub fn (mut t Transformer) make_call(fn_name string, args []flat.NodeId) flat.NodeId {
	return t.make_call_typed(fn_name, args, '')
}

pub fn (mut t Transformer) make_call_typed(fn_name string, args []flat.NodeId, typ string) flat.NodeId {
	fn_ident := t.make_ident(fn_name)
	return t.make_call_expr_typed(fn_ident, args, typ)
}

pub fn (mut t Transformer) make_call_expr_typed(fn_expr flat.NodeId, args []flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << fn_expr
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
		typ:            typ
	})
}

pub fn (mut t Transformer) make_empty() flat.NodeId {
	return t.a.add(.empty)
}

pub fn (mut t Transformer) make_method_call(receiver flat.NodeId, method_name string, args []flat.NodeId) flat.NodeId {
	// Build selector: receiver.method_name
	sel_start := t.a.children.len
	t.a.children << receiver
	selector := t.a.add_node(flat.Node{
		kind:           .selector
		value:          method_name
		children_start: sel_start
		children_count: 1
	})

	start := t.a.children.len
	t.a.children << selector
	for arg in args {
		t.a.children << arg
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		children_start: start
		children_count: flat.child_count(1 + args.len)
	})
}

pub fn (mut t Transformer) make_selector(base flat.NodeId, field string, typ string) flat.NodeId {
	return t.make_selector_op(base, field, typ, .dot)
}

pub fn (mut t Transformer) make_selector_op(base flat.NodeId, field string, typ string, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             op
		children_start: start
		children_count: 1
		value:          field
		typ:            typ
	})
}

pub fn (mut t Transformer) make_index(base flat.NodeId, index flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << base
	t.a.children << index
	return t.a.add_node(flat.Node{
		kind:           .index
		children_start: start
		children_count: 2
		typ:            typ
	})
}

pub fn (mut t Transformer) make_cast(target_type string, expr flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		children_start: start
		children_count: 1
		value:          target_type
		typ:            typ
	})
}

pub fn (mut t Transformer) make_postfix(expr flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .postfix
		op:             op
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_struct_init(name string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .struct_init
		value: name
		typ:   name
	})
}

pub fn (mut t Transformer) make_array_init(elem_type string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .array_init
		value: elem_type
		typ:   '[]${elem_type}'
	})
}

pub fn (mut t Transformer) make_map_init(map_type string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .map_init
		value: map_type
		typ:   map_type
	})
}

pub fn (mut t Transformer) make_string_literal(value string) flat.NodeId {
	return t.a.add_val(.string_literal, value)
}

pub fn (mut t Transformer) make_int_literal(value int) flat.NodeId {
	return t.a.add_val(.int_literal, '${value}')
}

pub fn (mut t Transformer) make_float_literal(value string) flat.NodeId {
	return t.a.add_val(.float_literal, value)
}

pub fn (mut t Transformer) make_bool_literal(value bool) flat.NodeId {
	return t.a.add_val(.bool_literal, if value { 'true' } else { 'false' })
}

pub fn (mut t Transformer) make_sizeof_type(type_name string) flat.NodeId {
	return t.a.add_node(flat.Node{
		kind:  .sizeof_expr
		value: type_name
		typ:   'usize'
	})
}

// is_fixed_array_type reports whether a v-type string denotes a fixed array
// like `int[5]` (as opposed to a dynamic array `[]int` or a map `map[...]...`).
fn is_fixed_array_type(s string) bool {
	if s.starts_with('[]') || s.starts_with('map[') {
		return false
	}
	return s.contains('[') && (s.ends_with(']') || s.starts_with('['))
}

fn fixed_array_len(s string) int {
	return fixed_array_len_text(s).int()
}

fn fixed_array_len_text(s string) string {
	return s.all_after('[').all_before(']').trim_space()
}

fn fixed_array_elem_type(s string) string {
	if s.starts_with('[') {
		return s.all_after(']')
	}
	return s.all_before('[')
}

fn is_decimal_text(s string) bool {
	if s.len == 0 {
		return false
	}
	for ch in s {
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return true
}

fn (mut t Transformer) make_fixed_array_len_expr(s string) flat.NodeId {
	len_text := fixed_array_len_text(s)
	if is_decimal_text(len_text) {
		return t.make_int_literal(len_text.int())
	}
	if len_text.contains('.') {
		base := len_text.all_before_last('.')
		field := len_text.all_after_last('.')
		return t.make_selector(t.make_ident(base), field, 'int')
	}
	return t.make_ident(len_text)
}

const c_reserved_words = ['auto', 'break', 'case', 'char', 'const', 'continue', 'copy', 'default',
	'do', 'double', 'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long',
	'register', 'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch',
	'typedef', 'union', 'unsigned', 'void', 'volatile', 'while']

fn c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	n := name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',', '_').replace(' ', '_').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}
