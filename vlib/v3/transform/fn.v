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
			// Try unqualified name first
			if t.is_known_fn_name(name) {
				return name
			}
			// Try qualified with current module
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qname := '${t.cur_module}.${name}'
				if t.is_known_fn_name(qname) {
					return qname
				}
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

fn (t &Transformer) is_known_fn_name(name string) bool {
	if name in t.fn_ret_types {
		return true
	}
	if !isnil(t.tc) {
		return name in t.tc.fn_ret_types || name in t.tc.fn_param_types
	}
	return false
}

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
	mut candidates := []string{}
	candidates << '${base_type}.${method}'
	if base_type.starts_with('[]') {
		elem_type := base_type[2..]
		short_elem := if elem_type.contains('.') { elem_type.all_after_last('.') } else { elem_type }
		candidates << '[]${short_elem}.${method}'
		if elem_type.contains('.') {
			candidates << '${elem_type.all_before_last('.')}.[]${short_elem}.${method}'
		}
	}
	if base_type.contains('.') {
		short_type := base_type.all_after_last('.')
		candidates << '${short_type}.${method}'
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${base_type}.${method}'
	}
	for candidate in candidates {
		if t.is_known_fn_name(candidate) {
			return candidate
		}
	}
	return ''
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

// transform_call_args transforms all children of a call expression.
// child[0] is the function expression, children[1..n] are arguments.
fn (mut t Transformer) transform_call_args(node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.a.add_node(flat.Node{
			kind:  .call
			op:    node.op
			pos:   node.pos
			value: node.value
			typ:   node.typ
		})
	}
	call_name := t.resolve_call_name(node)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.children[node.children_start + i]
		if i > 0 {
			mut child := &t.a.nodes[int(child_id)]
			if child.kind == .array_literal && child.typ.len == 0 {
				param_type := t.call_param_type_name(call_name, i - 1)
				if param_type.starts_with('[]') {
					child.typ = param_type
				}
			}
		}
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

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

fn (mut t Transformer) stringify_expr(expr_id flat.NodeId) flat.NodeId {
	typ := t.node_type(expr_id)
	expr := t.transform_expr(expr_id)
	return t.wrap_string_conversion(expr, typ)
}

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
			.paren, .cast_expr {
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			else {}
		}
	}
	return ''
}

fn (t &Transformer) reliable_infix_stringify_type(node flat.Node) string {
	if node.children_count < 2 {
		return ''
	}
	lhs_type := t.reliable_stringify_type(t.a.child(&node, 0))
	rhs_type := t.reliable_stringify_type(t.a.child(&node, 1))
	if lhs_type == 'string' || rhs_type == 'string' {
		return 'string'
	}
	match node.op {
		.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or {
			return 'bool'
		}
		.plus, .minus, .mul, .div, .mod, .left_shift, .right_shift, .amp, .pipe, .xor {
			if lhs_type.len > 0 && rhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type)
				&& t.is_numeric_stringify_type(rhs_type) {
				return lhs_type
			}
		}
		else {}
	}

	return ''
}

fn (t &Transformer) is_numeric_stringify_type(typ string) bool {
	is_number := typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16',
		'u32', 'u64', 'f32', 'f64', 'rune']
	return is_number || typ in t.enum_types
}

fn (mut t Transformer) wrap_string_conversion(expr flat.NodeId, typ string) flat.NodeId {
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	if !isnil(t.tc) {
		if alias := t.tc.type_aliases[clean_typ] {
			return t.wrap_string_conversion(expr, alias)
		}
		mut qtyp := clean_typ
		if !qtyp.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qtyp = '${t.cur_module}.${clean_typ}'
		}
		if alias := t.tc.type_aliases[qtyp] {
			return t.wrap_string_conversion(expr, alias)
		}
		if !clean_typ.contains('.') {
			for aname, target in t.tc.type_aliases {
				if aname.all_after_last('.') == clean_typ {
					return t.wrap_string_conversion(expr, target)
				}
			}
		}
		parsed := t.tc.parse_type(clean_typ)
		if parsed is types.Enum {
			return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
				'string')
		}
		if qtyp != clean_typ {
			qparsed := t.tc.parse_type(qtyp)
			if qparsed is types.Enum {
				return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
					'string')
			}
		}
	}
	if clean_typ == 'string' {
		return expr
	}
	if clean_typ == 'IError' || clean_typ.ends_with('.IError') {
		start := t.a.children.len
		t.a.children << expr
		return t.a.add_node(flat.Node{
			kind:           .selector
			op:             .dot
			children_start: start
			children_count: 1
			value:          'message'
			typ:            'string'
		})
	}
	match clean_typ {
		'bool' {
			return t.make_call_typed('bool.str', arr1(expr), 'string')
		}
		'u8', 'byte', 'u16', 'u32', 'u64' {
			return t.make_call_typed('strconv__format_uint', arr2(expr, t.make_int_literal(10)),
				'string')
		}
		'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize' {
			return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
				'string')
		}
		'f32' {
			return t.make_call_typed('strconv__f32_to_str_l', arr1(expr), 'string')
		}
		'f64' {
			return t.make_call_typed('strconv__f64_to_str_l', arr1(expr), 'string')
		}
		else {
			if clean_typ in t.enum_types {
				return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
					'string')
			}
			mut qenum := clean_typ
			if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
				&& t.cur_module != 'builtin' {
				qenum = '${t.cur_module}.${clean_typ}'
			}
			if qenum in t.enum_types {
				return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
					'string')
			}
			if clean_typ in t.structs || clean_typ in t.sum_types {
				mut qualified := clean_typ
				if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
					&& t.cur_module != 'builtin' {
					q := '${t.cur_module}.${clean_typ}'
					if q in t.structs || q in t.sum_types {
						qualified = q
					}
				}
				return t.make_call_typed('${c_name(qualified)}__str', arr1(expr), 'string')
			} else if clean_typ.len > 0 && clean_typ.starts_with('[]') {
				return t.make_call_typed('Array_str', arr1(expr), 'string')
			} else if clean_typ == 'rune' {
				return t.make_call_typed('strconv__format_int', arr2(expr, t.make_int_literal(10)),
					'string')
			} else {
				return expr
			}
		}
	}
}

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

fn (mut t Transformer) try_lower_flag_enum_stmt(call_id flat.NodeId) ?flat.NodeId {
	if int(call_id) < 0 {
		return none
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return none
	}
	fn_id := t.a.children[call.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['set', 'clear'] {
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
	base := t.transform_expr(base_id)
	arg := t.transform_expr(t.a.children[call.children_start + 1])
	if fn_node.value == 'set' {
		return t.make_assign_op(base, arg, .pipe_assign)
	}
	return t.make_assign_op(base, t.make_prefix(.bit_not, arg), .amp_assign)
}

fn (mut t Transformer) try_lower_flag_enum_call(node flat.Node) ?flat.NodeId {
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
	base_type := t.node_type(base_id)
	if !t.is_flag_enum_type(base_type) {
		return none
	}
	base := t.transform_expr(base_id)
	arg_id := t.a.children[node.children_start + 1]
	arg := t.transform_expr(arg_id)
	masked := t.make_infix(.amp, base, arg)
	if fn_node.value == 'has' {
		return t.make_infix(.ne, masked, t.make_int_literal(0))
	}
	arg_copy := t.transform_expr(arg_id)
	return t.make_infix(.eq, masked, arg_copy)
}

fn (mut t Transformer) try_lower_array_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	if fn_node.value !in ['clone', 'contains', 'index', 'join'] {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	base_type := t.node_type(base_id)
	if !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	receiver := t.transform_expr(base_id)
	match fn_node.value {
		'clone' {
			return t.make_call_typed('array_clone', arr1(receiver), base_type)
		}
		'contains' {
			if node.children_count < 2 {
				return none
			}
			arg := t.transform_expr(t.a.children[node.children_start + 1])
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
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			fn_name := if elem_type == 'string' { 'array_index_string' } else { 'array_index_int' }
			return t.make_call_typed(fn_name, arr2(receiver, arg), 'int')
		}
		'join' {
			if node.children_count < 2 {
				return none
			}
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			return t.make_call_typed('array_string_join', arr2(receiver, arg), 'string')
		}
		else {
			return none
		}
	}
}

// try_lower_builtin_call checks if a call is to a builtin that needs special lowering.
// Returns none for most calls so the caller falls through to generic call transform.
fn (mut t Transformer) try_lower_builtin_call(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	if flag_call := t.try_lower_flag_enum_call(node) {
		return flag_call
	}
	if array_call := t.try_lower_array_method_call(node) {
		return array_call
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
	match name {
		'println', 'eprintln', 'print' {
			if node.children_count < 2 {
				return t.transform_call_args(node)
			}
			arg := t.stringify_expr(t.a.child(&node, 1))
			return t.make_call(name, arr1(arg))
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
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			if ret := t.tc.fn_ret_types[name] {
				return t.normalize_type_alias(ret.name())
			}
		}
	}
	name := t.resolve_call_name(node)
	if name.len == 0 {
		return ''
	}
	// Try exact name first
	if ret := t.fn_ret_types[name] {
		return t.normalize_type_alias(ret)
	}
	// Try qualified with current module
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if ret := t.fn_ret_types[qname] {
			return t.normalize_type_alias(ret)
		}
	}
	return ''
}
