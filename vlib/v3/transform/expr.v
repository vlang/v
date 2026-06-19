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

fn (mut t Transformer) transform_infix_struct_ops(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	op_name := match node.op {
		.plus { '+' }
		.minus { '-' }
		.eq { '==' }
		.ne { '!=' }
		.lt { '<' }
		.gt { '>' }
		.le { '<=' }
		.ge { '>=' }
		else { '' }
	}

	if op_name.len == 0 || node.children_count < 2 {
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
	method_name := '${struct_type}.${op_name}'
	if !t.is_known_fn_name(method_name) {
		if node.op != .eq && node.op != .ne {
			return none
		}
		lhs := t.stable_expr_for_reuse(lhs_id)
		rhs := t.stable_expr_for_reuse(t.a.children[node.children_start + 1])
		cmp := t.make_call_typed('memcmp', arr3(t.make_prefix(.amp, lhs), t.make_prefix(.amp, rhs),
			t.make_sizeof_type(struct_type)), 'int')
		return t.make_infix(node.op, cmp, t.make_int_literal(0))
	}
	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(t.a.children[node.children_start + 1])
	return t.make_call(method_name, arr2(new_lhs, new_rhs))
}

fn (t &Transformer) struct_lookup_name(type_name string) string {
	if type_name.len == 0 {
		return ''
	}
	if type_name in t.structs {
		return type_name
	}
	if type_name.contains('.') {
		short_type := type_name.all_after_last('.')
		if short_type in t.structs {
			return short_type
		}
		return ''
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
		if rhs_type.starts_with('[]') {
			// dynamic array membership -> array_contains_int/string(arr, val)
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			elem := rhs_type[2..]
			fn_name := array_contains_fn_name(elem)
			result = t.make_call_typed(fn_name, arr2(new_rhs, new_lhs), 'bool')
		} else if is_fixed_array_type(rhs_type) {
			// fixed array membership -> fixed_array_contains_int/string(arr, len, val)
			new_lhs := t.transform_expr(lhs_id)
			new_rhs := t.transform_expr(rhs_id)
			elem := fixed_array_elem_type(rhs_type)
			fn_name := fixed_array_contains_fn_name(elem)
			len_expr := t.make_fixed_array_len_expr(rhs_type)
			result = t.make_call_typed(fn_name, arr3(new_rhs, len_expr, new_lhs), 'bool')
		} else if t.clean_map_type(rhs_type).starts_with('map[') {
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
	if typ.starts_with('&') {
		return typ[1..]
	}
	return typ
}

fn (mut t Transformer) runtime_addr(expr flat.NodeId, typ string) flat.NodeId {
	if typ.starts_with('&') {
		return expr
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
				return t.a.add_val(.enum_val, '${expected_enum}.${short_name}')
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
	start := t.a.children.len
	t.a.children << fn_ident
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
		'__lt').replace('.>', '__gt').replace('.', '__')
	if n in c_reserved_words {
		return 'v_${n}'
	}
	return n
}
