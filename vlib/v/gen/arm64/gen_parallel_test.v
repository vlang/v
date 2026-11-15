module arm64

import v.ssa
import v.workers

// Generating functions on several threads must produce the object sequential
// generation does. Each function calls one defined earlier and one defined later,
// so the calls fall on both sides of the range boundaries, and uses a literal
// shared with every other range, which must end up stored once.
fn parallel_test_module() &ssa.Module {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	u8_type := m.type_store.get_int(8)
	string_type := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [m.type_store.get_ptr(u8_type), i64_type, i64_type]
		field_names: ['str', 'len', 'is_lit']
	})
	external_id := m.new_function('consume', ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	external.is_variadic = true
	external.variadic_start = 0
	m.funcs[external_id] = external
	consume := m.add_value(.func_ref, ssa.TypeID(0), 'consume', external_id)
	count := 300
	for i in 0 .. count {
		m.new_function('f${i}', ssa.TypeID(0))
	}
	for i in 0 .. count {
		func_id := m.new_function('f${i}', ssa.TypeID(0))
		entry := m.add_block(func_id, 'entry')
		shared_literal := m.add_value(.string_literal, string_type, 'shared', 0)
		own_literal := m.add_value(.string_literal, string_type, 'only ${i}', 0)
		m.add_instr(.call, entry, ssa.TypeID(0), [consume, shared_literal, own_literal])
		earlier := m.add_value(.func_ref, ssa.TypeID(0), 'f${(i + count - 7) % count}', 0)
		later := m.add_value(.func_ref, ssa.TypeID(0), 'f${(i + 11) % count}', 0)
		m.add_instr(.call, entry, ssa.TypeID(0), [earlier])
		m.add_instr(.call, entry, ssa.TypeID(0), [later])
		m.add_instr(.ret, entry, ssa.TypeID(0), []ssa.ValueID{})
	}
	return m
}

// parallel_test_linked_text resolves calls between the generated functions:
// the sequential path emits a direct `bl` where the callee is already known,
// while a range only knows its own functions and leaves a relocation.
fn parallel_test_linked_text(g &Gen) []u8 {
	mut text := g.macho.text_data.clone()
	for relocation in g.macho.relocs {
		symbol := g.macho.symbols[relocation.sym_idx]
		if relocation.type_ == arm64_reloc_branch26 && symbol.sect == 1 {
			offset := i32(i64(symbol.value) - i64(relocation.addr)) / 4
			write_u32_le_at_arr(mut text, relocation.addr, asm_bl(offset))
		}
	}
	return text
}

fn test_parallel_generation_matches_sequential_generation() {
	mut sequential := Gen.new(parallel_test_module())
	sequential.gen()
	mut pool := workers.new(3)
	defer {
		pool.close()
	}
	mut parallel := Gen.new(parallel_test_module())
	parallel.gen_parallel(pool)
	assert parallel_test_linked_text(parallel) == parallel_test_linked_text(sequential)
	assert parallel.macho.str_data == sequential.macho.str_data
	assert parallel.macho.symbols.len == sequential.macho.symbols.len
	for index, symbol in sequential.macho.symbols {
		other := parallel.macho.symbols[index]
		assert other.name == symbol.name
		assert other.value == symbol.value
		assert other.sect == symbol.sect
		assert other.type_ == symbol.type_
	}
}

// A use laid out before the loop that reaches it: the value is live through every
// block of the loop even though the last mention of it comes first in the layout.
fn test_value_stays_live_through_a_loop_after_its_last_use_in_layout() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	bool_type := m.type_store.get_int(1)
	func_id := m.new_function('looping', i64_type)
	entry := m.add_block(func_id, 'entry')
	exit_block := m.add_block(func_id, 'exit')
	header := m.add_block(func_id, 'header')
	body := m.add_block(func_id, 'body')
	one := m.add_value(.constant, i64_type, '1', 0)
	value := m.add_instr(.add, entry, i64_type, [one, one])
	m.add_instr(.jmp, entry, ssa.TypeID(0), [ssa.ValueID(header)])
	used := m.add_instr(.add, exit_block, i64_type, [value, value])
	m.add_instr(.ret, exit_block, ssa.TypeID(0), [used])
	cond := m.add_instr(.lt, header, bool_type, [one, one])
	m.add_instr(.br, header, ssa.TypeID(0), [cond, ssa.ValueID(body), ssa.ValueID(exit_block)])
	counted := m.add_instr(.add, body, i64_type, [one, one])
	m.add_instr(.jmp, body, ssa.TypeID(0), [ssa.ValueID(header)])
	mut g := Gen.new(m)
	func := m.funcs[func_id]
	g.reset_value_slots(&func)
	g.reset_block_offsets(&func)
	g.regalloc_function(&func)
	mut interval := LiveInterval{
		value: 0
	}
	for candidate in g.ra_list {
		if candidate.value == int(value) {
			interval = candidate
		}
	}
	assert interval.value == int(value)
	assert interval.start == 0
	// Live through the back edge at the end of `body`.
	assert interval.end == 7
	_ = counted
}
