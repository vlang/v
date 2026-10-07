module ssa

import v.flat

fn test_native_alignof_preserves_nested_fixed_array_element_alignment() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	u64_type := m.type_store.get_uint(64)
	aligned_type := m.type_store.register(Type{
		kind:      .struct_t
		fields:    [u64_type]
		alignment: 64
	})
	packed_type := m.type_store.register(Type{
		kind:      .struct_t
		fields:    [u64_type]
		is_packed: true
	})
	mut b := Builder{
		a:            &a
		m:            m
		u64_type:     u64_type
		struct_types: {
			'Aligned': aligned_type
			'Packed':  packed_type
		}
	}
	callee := a.add_node(flat.Node{ kind: .ident, value: '__alignof__' })
	for type_name, alignment in {
		'Aligned':       64
		'[3][2]Aligned': 64
		'[3]Packed':     1
	} {
		arg := a.add_node(flat.Node{ kind: .sizeof_expr, value: type_name })
		start := a.children.len
		a.children << [callee, arg]
		call := a.add_node(flat.Node{
			kind:           .call
			children_start: start
			children_count: 2
		})
		value := b.build_expr(call)
		assert m.values[value].name == alignment.str()
	}
	assert m.instrs.len == 0
}
