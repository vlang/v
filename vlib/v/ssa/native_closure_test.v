module ssa

import v.flat

fn test_native_void_and_function_pointers_have_pointer_storage() {
	mut m := Module.new()
	void_pointer := m.type_store.get_ptr(TypeID(0))
	function_pointer := m.type_store.register(Type{ kind: .func_t })
	for pointer_type in [void_pointer, function_pointer] {
		assert m.type_size(pointer_type) == 8
		assert m.type_align(pointer_type) == 8
		array_type := m.type_store.get_array(pointer_type, 2)
		assert m.type_size(array_type) == 16
	}
}

fn test_native_closure_context_uses_registered_runtime_getter() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	void_type := TypeID(0)
	i8_type := m.type_store.get_int(8)
	i64_type := m.type_store.get_int(64)
	ptr_i8 := m.type_store.get_ptr(i8_type)
	closure_type := m.type_store.register(Type{
		kind:        .struct_t
		fields:      [i64_type, ptr_i8]
		field_names: ['reserved', 'closure_get_data']
	})
	global := m.add_global('closure.g_closure', closure_type)
	function := m.add_function('lifted_closure', ptr_i8)
	entry := m.add_block(function, 'entry')
	mut b := Builder{
		a:           &a
		m:           m
		i8_type:     i8_type
		i64_type:    i64_type
		void_type:   void_type
		cur_block:   entry
		global_vars: {
			'closure.g_closure': global
		}
	}
	callee := a.add_node(flat.Node{ kind: .ident, value: '__v3_closure_current_data' })
	start := a.children.len
	a.children << callee
	call := a.add_node(flat.Node{ kind: .call, children_start: start, children_count: 1 })
	value := b.build_expr(call)
	assert m.values[value].typ == ptr_i8
	call_instruction := m.instrs[m.values[value].index]
	assert call_instruction.op == .call_indirect
	getter_load := m.instrs[m.values[call_instruction.operands[0]].index]
	assert getter_load.op == .load
	getter_addr := m.instrs[m.values[getter_load.operands[0]].index]
	assert getter_addr.op == .get_element_ptr
	assert getter_addr.operands[0] == global
	assert m.values[getter_addr.operands[1]].name == '8'
}

fn test_native_darwin_memory_mapping_constants() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	i8_type := m.type_store.get_int(8)
	i64_type := m.type_store.get_int(64)
	mut b := Builder{ a: &a, m: m, i8_type: i8_type, i64_type: i64_type }
	base := a.add_node(flat.Node{ kind: .ident, value: 'C' })
	for name, expected in {
		'PROT_READ':     '1'
		'PROT_WRITE':    '2'
		'PROT_EXEC':     '4'
		'MAP_PRIVATE':   '2'
		'MAP_ANON':      '4096'
		'MAP_ANONYMOUS': '4096'
		'MAP_FAILED':    '-1'
		'_SC_PAGESIZE':  '29'
		'_SC_PAGE_SIZE': '29'
	} {
		start := a.children.len
		a.children << base
		selector := a.add_node(flat.Node{ kind: .selector, value: name, children_start: start, children_count: 1 })
		value := b.build_expr(selector)
		assert m.values[value].name == expected
		if name == 'MAP_FAILED' {
			assert m.values[value].typ == m.type_store.get_ptr(i8_type)
		}
	}
}
