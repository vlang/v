module arm64

import os
import v.ssa

fn select_scalar_failures(mut m ssa.Module, block ssa.BlockID, first ssa.ValueID, second ssa.ValueID) ssa.ValueID {
	boolean := m.type_store.get_int(1)
	typ := m.values[first].typ
	mut failed := m.get_or_add_const(boolean, '0')
	for condition in [0, 1, 7] {
		cond := m.get_or_add_const(boolean, condition.str())
		selected := m.add_instr(.select, block, typ, [cond, first, second])
		expected := if condition == 0 { second } else { first }
		wrong := m.add_instr(.ne, block, boolean, [selected, expected])
		failed = m.add_instr(.or_, block, boolean, [failed, wrong])
	}
	return failed
}

fn test_native_select_preserves_scalar_bits_and_every_aggregate_word() {
	$if macos && arm64 {
		mut m := ssa.Module.new()
		boolean := m.type_store.get_int(1)
		i32_type := m.type_store.get_int(32)
		i64_type := m.type_store.get_int(64)
		main := m.new_function('main', i32_type)
		entry := m.add_block(main, 'entry')
		mut failed := m.get_or_add_const(boolean, '0')
		for width in [8, 16, 32, 64] {
			typ := m.type_store.get_int(width)
			first := m.get_or_add_const(typ, '-7')
			second := m.get_or_add_const(typ, '19')
			wrong := select_scalar_failures(mut m, entry, first, second)
			failed = m.add_instr(.or_, entry, boolean, [failed, wrong])
		}
		for width in [32, 64] {
			typ := m.type_store.get_float(width)
			first := m.get_or_add_const(typ, '-1.25')
			second := m.get_or_add_const(typ, '2.5')
			wrong := select_scalar_failures(mut m, entry, first, second)
			failed = m.add_instr(.or_, entry, boolean, [failed, wrong])
		}
		ptr_type := m.type_store.get_ptr(i64_type)
		first_ptr := m.add_instr(.alloca, entry, ptr_type, [])
		second_ptr := m.add_instr(.alloca, entry, ptr_type, [])
		pointer_wrong := select_scalar_failures(mut m, entry, first_ptr, second_ptr)
		failed = m.add_instr(.or_, entry, boolean, [failed, pointer_wrong])
		triple := m.type_store.get_tuple([i64_type, i64_type, i64_type])
		triple_ptr := m.type_store.get_ptr(triple)
		mut aggregates := []ssa.ValueID{}
		for base in [11, 101] {
			storage := m.add_instr(.alloca, entry, triple_ptr, [])
			for word in 0 .. 3 {
				offset := m.get_or_add_const(i64_type, (word * 8).str())
				address := m.add_instr(.get_element_ptr, entry, ptr_type, [storage, offset])
				value := m.get_or_add_const(i64_type, (base + word).str())
				m.add_instr(.store, entry, ssa.TypeID(0), [value, address])
			}
			aggregates << m.add_instr(.load, entry, triple, [storage])
		}
		for condition in [0, 1, 7] {
			cond := m.get_or_add_const(boolean, condition.str())
			selected := m.add_instr(.select, entry, triple, [cond, aggregates[0], aggregates[1]])
			storage := m.add_instr(.alloca, entry, triple_ptr, [])
			m.add_instr(.store, entry, ssa.TypeID(0), [selected, storage])
			base := if condition == 0 { 101 } else { 11 }
			for word in 0 .. 3 {
				offset := m.get_or_add_const(i64_type, (word * 8).str())
				address := m.add_instr(.get_element_ptr, entry, ptr_type, [storage, offset])
				actual := m.add_instr(.load, entry, i64_type, [address])
				expected := m.get_or_add_const(i64_type, (base + word).str())
				wrong := m.add_instr(.ne, entry, boolean, [actual, expected])
				failed = m.add_instr(.or_, entry, boolean, [failed, wrong])
			}
		}
		m.add_instr(.ret, entry, ssa.TypeID(0), [failed])
		output := os.join_path(os.vtmp_dir(), 'arm64_select_${os.getpid()}')
		defer { os.rm(output) or {} }
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, 'select: exit ${result.exit_code}: ${result.output}'
	}
}
