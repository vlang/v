module types

import v.flat

// independent_numeric_const_initializer excludes every expression whose type
// can change when another declaration is resolved during constant collection.
@[direct_array_access]
fn independent_numeric_const_initializer(a &flat.FlatAst, id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return false
	}
	node := unsafe { &a.nodes[int(id)] }
	if node.typ.len > 0 && node.typ !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32',
		'u64', 'f32', 'f64', 'usize', 'isize'] {
		return false
	}
	match node.kind {
		.int_literal, .float_literal {
			return node.children_count == 0
		}
		.cast_expr {
			if node.value !in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
				'f64', 'usize', 'isize'] || node.children_count != 1 {
				return false
			}
		}
		.prefix {
			if node.op !in [.plus, .minus, .bit_not] || node.children_count != 1 {
				return false
			}
		}
		.postfix {
			if node.op != .not || node.children_count != 1
				|| a.child_node(node, 0).kind != .array_literal {
				return false
			}
		}
		.paren {
			if node.children_count != 1 {
				return false
			}
		}
		.array_literal {
			// Explicit array type text can contain a const dimension or alias.
			if node.typ.len != 0 || node.children_count == 0 {
				return false
			}
		}
		else {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if !independent_numeric_const_initializer(a, a.child(node, i)) {
			return false
		}
	}
	return true
}
