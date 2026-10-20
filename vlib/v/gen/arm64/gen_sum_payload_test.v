module arm64

import v.ssa

fn test_inline_sum_member_copy_uses_the_selected_union_member_size() {
	for words in [1, 2, 3] {
		mut m := ssa.Module.new()
		i8_type := m.type_store.get_int(8)
		i32_type := m.type_store.get_int(32)
		i64_type := m.type_store.get_int(64)
		small := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i8_type], field_names: ['dummy'] })
		pair := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i64_type, i64_type], field_names: [
			'name',
			'reference',
		] })
		triple := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i64_type, i64_type,
			i64_type], field_names: ['x', 'y', 'z'] })
		members := [small, pair, triple]
		member := members[words - 1]
		payload := m.type_store.register(ssa.Type{ kind: .struct_t, is_union: true, fields: members, field_names: [
			'Small',
			'Pair',
			'Triple',
		] })
		choice := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i32_type, payload], field_names: [
			'typ',
			'_payload',
		] })
		function := m.new_function('copy_payload', ssa.TypeID(0))
		block := m.add_block(function, 'entry')
		storage := m.add_instr(.alloca, block, m.type_store.get_ptr(choice), [])
		payload_offset := m.get_or_add_const(i64_type, '8')
		zero := m.get_or_add_const(i64_type, '0')
		payload_ptr := m.add_instr(.get_element_ptr, block, m.type_store.get_ptr(payload), [
			storage,
			payload_offset,
		])
		member_ptr := m.add_instr(.get_element_ptr, block, m.type_store.get_ptr(member), [
			payload_ptr,
			zero,
		])
		value := m.add_instr(.struct_init, block, member, [])
		store := m.add_instr(.store, block, ssa.TypeID(0), [value, member_ptr])
		mut g := Gen.new(m)
		g.reset_value_slots(&m.funcs[function])
		g.stack_size = 128
		g.set_alloca_slot(int(storage), -128, m.type_size(choice), 8)
		g.set_stack_slot(int(member_ptr), -80)
		g.set_stack_slot(int(value), -64)
		assert g.aggregate_store_size(int(member_ptr), member) == m.type_size(member)
		assert g.aggregate_load_size(int(member_ptr), member) == m.type_size(member)
		g.gen_instr(int(store))
		if words == 1 {
			assert asm_str_b(Reg(8), Reg(9)) in g.text_words
		} else {
			// The pointer word in Pair and every word in Triple must reach the payload.
			for word in 0 .. words {
				assert asm_str_imm(Reg(8), Reg(9), u32(word)) in g.text_words
			}
		}
	}
}

fn test_ordinary_struct_member_copy_keeps_its_storage_bound() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	large := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i64_type, i64_type, i64_type] })
	pair := m.type_store.register(ssa.Type{ kind: .struct_t, fields: [i64_type, i64_type], field_names: [
		'left',
		'right',
	] })
	function := m.new_function('bounded_copy', ssa.TypeID(0))
	block := m.add_block(function, 'entry')
	storage := m.add_instr(.alloca, block, m.type_store.get_ptr(pair), [])
	zero := m.get_or_add_const(i64_type, '0')
	field := m.add_instr(.get_element_ptr, block, m.type_store.get_ptr(i64_type), [
		storage,
		zero,
	])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[function])
	g.set_alloca_slot(int(storage), -32, m.type_size(pair), 8)
	assert g.aggregate_store_size(int(field), large) == 8
	assert g.aggregate_load_size(int(field), large) == 8
}
