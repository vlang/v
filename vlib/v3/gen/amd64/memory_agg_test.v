module amd64

import v3.ssa

struct MemoryAggTestAllocaFixture {
mut:
	m              &ssa.Module
	profile        TargetProfile
	function_index int
	block_id       ssa.BlockID
	element_type   ssa.TypeID
	i64_type       ssa.TypeID
	alloca_id      ssa.ValueID
	count_id       ssa.ValueID
	ret_id         ssa.ValueID
}

struct MemoryAggTestAggregateFixture {
mut:
	m                &ssa.Module
	profile          TargetProfile
	function_index   int
	block_id         ssa.BlockID
	i1_type          ssa.TypeID
	i8_type          ssa.TypeID
	i16_type         ssa.TypeID
	i32_type         ssa.TypeID
	i64_type         ssa.TypeID
	aggregate_type   ssa.TypeID
	aggregate_pointer ssa.TypeID
	alloca_id        ssa.ValueID
	field_values     []ssa.ValueID
	delta_values     []ssa.ValueID
	field_pointers   []ssa.ValueID
	field_stores     []ssa.ValueID
	construct_id     ssa.ValueID
	first_store_id   ssa.ValueID
	load_id          ssa.ValueID
	extract_id       ssa.ValueID
	insert_value_id  ssa.ValueID
	insert_id         ssa.ValueID
	second_store_id  ssa.ValueID
	ret_id            ssa.ValueID
}

fn memory_agg_test_int_type(mut m ssa.Module, width int, is_unsigned bool) ssa.TypeID {
	mut type_store := m.type_store
	type_id := if is_unsigned {
		type_store.get_uint(width)
	} else {
		type_store.get_int(width)
	}
	m.type_store = type_store
	return type_id
}

fn memory_agg_test_pointer_type(mut m ssa.Module, element_type ssa.TypeID) ssa.TypeID {
	mut type_store := m.type_store
	type_id := type_store.get_ptr(element_type)
	m.type_store = type_store
	return type_id
}

fn memory_agg_test_anchor(m &ssa.Module, function_index int, value_id ssa.ValueID) MemoryAggInstructionAnchor {
	value := m.values[int(value_id)]
	instruction := m.instrs[value.index]
	function := m.funcs[function_index]
	mut block_ordinal := -1
	mut instruction_ordinal := -1
	for candidate_block_ordinal, block_id in function.blocks {
		if block_id != instruction.block {
			continue
		}
		block_ordinal = candidate_block_ordinal
		for candidate_instruction_ordinal, candidate_value_id in m.blocks[int(block_id)].instrs {
			if candidate_value_id == value_id {
				instruction_ordinal = candidate_instruction_ordinal
				break
			}
		}
		break
	}
	assert block_ordinal >= 0
	assert instruction_ordinal >= 0
	return MemoryAggInstructionAnchor{
		function_index:       function_index
		block_id:             instruction.block
		block_ordinal:        u32(block_ordinal)
		instruction_value_id: value_id
		instruction_index:    value.index
		instruction_ordinal:  u32(instruction_ordinal)
	}
}

fn memory_agg_test_layout(profile TargetProfile, m &ssa.Module, type_id ssa.TypeID) MemoryAggScalarLayoutBinding {
	typ := m.type_store.types[int(type_id)]
	storage := match typ.width {
		1, 8 { u8(1) }
		16 { u8(2) }
		32 { u8(4) }
		64 { u8(8) }
		else { u8(0) }
	}
	return MemoryAggScalarLayoutBinding{
		profile:              profile
		type_id:              type_id
		authority:            .native_plain
		semantic_width_bits:  typ.width
		semantic_is_unsigned: typ.is_unsigned
		storage_width_bytes:  storage
		alignment_bytes:      storage
	}
}

fn memory_agg_test_layouts(profile TargetProfile, m &ssa.Module, type_ids []ssa.TypeID) []MemoryAggScalarLayoutBinding {
	mut seen := map[int]bool{}
	mut result := []MemoryAggScalarLayoutBinding{}
	for type_id in type_ids {
		if int(type_id) in seen {
			continue
		}
		seen[int(type_id)] = true
		result << memory_agg_test_layout(profile, m, type_id)
	}
	return result
}

fn memory_agg_test_local(profile TargetProfile, m &ssa.Module, function_index int, alloca_id ssa.ValueID, element_type ssa.TypeID, form MemoryAggAllocaForm, count_id ssa.ValueID) MemoryAggStaticLocalBinding {
	return MemoryAggStaticLocalBinding{
		profile:        profile
		anchor:         memory_agg_test_anchor(m, function_index, alloca_id)
		authority:      .native_plain
		element_type:   element_type
		form:           form
		count_value_id: count_id
	}
}

fn memory_agg_test_access(profile TargetProfile, m &ssa.Module, function_index int, access_id ssa.ValueID) MemoryAggAccessBinding {
	value := m.values[int(access_id)]
	instruction := m.instrs[value.index]
	if instruction.op == .load {
		return MemoryAggAccessBinding{
			profile:          profile
			anchor:           memory_agg_test_anchor(m, function_index, access_id)
			semantics:        .nonvolatile
			kind:             .load
			pointer_value_id: instruction.operands[0]
			scalar_value_id:  access_id
			scalar_type:      instruction.typ
		}
	}
	assert instruction.op == .store
	scalar_id := instruction.operands[0]
	return MemoryAggAccessBinding{
		profile:          profile
		anchor:           memory_agg_test_anchor(m, function_index, access_id)
		semantics:        .nonvolatile
		kind:             .store
		pointer_value_id: instruction.operands[1]
		scalar_value_id:  scalar_id
		scalar_type:      m.values[int(scalar_id)].typ
	}
}

fn memory_agg_test_constant(value_id ssa.ValueID, type_id ssa.TypeID, raw_bits u64) ScalarConstantBinding {
	return ScalarConstantBinding{
		value_id: value_id
		type_id:  type_id
		raw_bits: raw_bits
	}
}

fn memory_agg_test_facts(profile TargetProfile, function_index int, layouts []MemoryAggScalarLayoutBinding, locals []MemoryAggStaticLocalBinding, constants []ScalarConstantBinding, accesses []MemoryAggAccessBinding) MemoryAggFunctionFacts {
	return MemoryAggFunctionFacts{
		profile:         profile
		function_index:  function_index
		ssa_form:        .final_static
		scalar_layouts:  layouts
		static_locals:   locals
		scalar_constants: constants
		accesses:         accesses
	}
}

fn memory_agg_test_expect_error(m &ssa.Module, facts &MemoryAggFunctionFacts, expected string) {
	if _ := plan_scalar_static_memory(m, facts) {
		assert false, 'expected error containing `${expected}`'
	} else {
		assert err.msg().contains(expected), '`${err.msg()}` does not contain `${expected}`'
	}
}

fn memory_agg_test_count_cap(label string, limit int) {
	memory_agg_validate_count(label, limit, limit) or { panic(err) }
	if _ := memory_agg_validate_count(label, limit + 1, limit) {
		assert false, 'expected ${label} limit+1 to fail'
	} else {
		assert err.msg().contains('${label} ${limit + 1} exceeds ${limit}')
	}
}

fn memory_agg_test_empty_module() (&ssa.Module, int) {
	mut m := ssa.Module.new()
	function_index := m.new_function('memory_empty', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	return m, function_index
}

fn memory_agg_test_alloca_fixture(profile TargetProfile, width int, is_unsigned bool, counted bool, count_bits u64) MemoryAggTestAllocaFixture {
	mut m := ssa.Module.new()
	element_type := memory_agg_test_int_type(mut m, width, is_unsigned)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	pointer_type := memory_agg_test_pointer_type(mut m, element_type)
	function_index := m.new_function('memory_alloca', ssa.TypeID(0))
	block_id := m.add_block(function_index, 'entry')
	mut count_id := ssa.ValueID(0)
	mut operands := []ssa.ValueID{}
	if counted {
		count_id = m.add_value(.constant, i64_type, 'count-sidecar-only', 0)
		operands << count_id
	}
	alloca_id := m.add_instr(.alloca, block_id, pointer_type, operands)
	ret_id := m.add_instr(.ret, block_id, ssa.TypeID(0), [])
	return MemoryAggTestAllocaFixture{
		m:              m
		profile:        profile
		function_index: function_index
		block_id:       block_id
		element_type:   element_type
		i64_type:       i64_type
		alloca_id:      alloca_id
		count_id:       count_id
		ret_id:         ret_id
	}
}

fn memory_agg_test_alloca_facts(fixture &MemoryAggTestAllocaFixture, count_bits u64) MemoryAggFunctionFacts {
	mut type_ids := [fixture.element_type]
	mut constants := []ScalarConstantBinding{}
	mut form := MemoryAggAllocaForm.one
	if fixture.count_id != ssa.ValueID(0) {
		type_ids << fixture.i64_type
		constants << memory_agg_test_constant(fixture.count_id, fixture.i64_type, count_bits)
		form = .constant_count
	}
	return memory_agg_test_facts(fixture.profile, fixture.function_index,
		memory_agg_test_layouts(fixture.profile, fixture.m, type_ids),
		[memory_agg_test_local(fixture.profile, fixture.m, fixture.function_index,
			fixture.alloca_id, fixture.element_type, form, fixture.count_id)], constants, [])
}

fn memory_agg_test_replace_instruction(mut m ssa.Module, value_id ssa.ValueID, instruction ssa.Instruction) {
	value := m.values[int(value_id)]
	m.instrs[value.index] = instruction
}

fn memory_agg_test_struct_type(mut m ssa.Module, fields []ssa.TypeID) ssa.TypeID {
	mut type_store := m.type_store
	type_id := type_store.get_tuple(fields)
	m.type_store = type_store
	return type_id
}

fn memory_agg_test_align_up(value u64, alignment u64) u64 {
	return (value + alignment - 1) & ~(alignment - 1)
}

fn memory_agg_test_aggregate_layout(profile TargetProfile, m &ssa.Module, type_id ssa.TypeID) MemoryAggAggregateLayoutBinding {
	typ := m.type_store.types[int(type_id)]
	mut fields := []MemoryAggAggregateFieldLayout{cap: typ.fields.len}
	mut padding := []MemoryAggByteRange{}
	mut cursor := u64(0)
	mut maximum_alignment := u64(1)
	for field_index, field_type in typ.fields {
		scalar := memory_agg_test_layout(profile, m, field_type)
		alignment := u64(scalar.alignment_bytes)
		offset := memory_agg_test_align_up(cursor, alignment)
		if offset > cursor {
			padding << MemoryAggByteRange{
				offset_bytes: cursor
				size_bytes:   offset - cursor
			}
		}
		fields << MemoryAggAggregateFieldLayout{
			index:           u16(field_index)
			type_id:         field_type
			offset_bytes:    offset
			size_bytes:      u64(scalar.storage_width_bytes)
			alignment_bytes: alignment
		}
		cursor = offset + u64(scalar.storage_width_bytes)
		if alignment > maximum_alignment {
			maximum_alignment = alignment
		}
	}
	size := memory_agg_test_align_up(cursor, maximum_alignment)
	if size > cursor {
		padding << MemoryAggByteRange{
			offset_bytes: cursor
			size_bytes:   size - cursor
		}
	}
	return MemoryAggAggregateLayoutBinding{
		profile:         profile
		authority:       .native_plain
		type_id:         type_id
		size_bytes:      size
		alignment_bytes: maximum_alignment
		fields:          fields
		padding:         padding
	}
}

fn memory_agg_test_nominal_aggregate_fixture(profile TargetProfile, padding MemoryAggConstructPadding) (MemoryAggTestAggregateFixture, MemoryAggFunctionFacts) {
	mut m := ssa.Module.new()
	i1_type := memory_agg_test_int_type(mut m, 1, false)
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	i16_type := memory_agg_test_int_type(mut m, 16, false)
	i32_type := memory_agg_test_int_type(mut m, 32, false)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	aggregate_type := memory_agg_test_struct_type(mut m, [i8_type, i32_type, i16_type])
	aggregate_pointer := memory_agg_test_pointer_type(mut m, aggregate_type)
	function_index := m.new_function('memory_aggregate_nominal', ssa.TypeID(0))
	block_id := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block_id, aggregate_pointer, [])
	field_values := [
		m.add_value(.constant, i8_type, 'field-0', 0),
		m.add_value(.constant, i32_type, 'field-1', 0),
		m.add_value(.constant, i16_type, 'field-2', 0),
	]
	construct_id := m.add_instr(.struct_init, block_id, aggregate_type, field_values)
	first_store_id := m.add_instr(.store, block_id, ssa.TypeID(0),
		[construct_id, alloca_id])
	load_id := m.add_instr(.load, block_id, aggregate_type, [alloca_id])
	extract_id := m.add_instr(.extractvalue, block_id, i32_type, [load_id])
	insert_value_id := m.add_value(.constant, i32_type, 'insert-field', 0)
	insert_id := m.add_instr(.insertvalue, block_id, aggregate_type,
		[load_id, insert_value_id])
	second_store_id := m.add_instr(.store, block_id, ssa.TypeID(0),
		[insert_id, alloca_id])
	ret_id := m.add_instr(.ret, block_id, ssa.TypeID(0), [])
	fixture := MemoryAggTestAggregateFixture{
		m:                 m
		profile:           profile
		function_index:    function_index
		block_id:          block_id
		i1_type:           i1_type
		i8_type:           i8_type
		i16_type:          i16_type
		i32_type:          i32_type
		i64_type:          i64_type
		aggregate_type:    aggregate_type
		aggregate_pointer: aggregate_pointer
		alloca_id:         alloca_id
		field_values:      field_values
		construct_id:      construct_id
		first_store_id:    first_store_id
		load_id:           load_id
		extract_id:        extract_id
		insert_value_id:   insert_value_id
		insert_id:          insert_id
		second_store_id:   second_store_id
		ret_id:            ret_id
	}
	facts := MemoryAggFunctionFacts{
		profile:         profile
		function_index:  function_index
		ssa_form:        .final_static
		scalar_layouts:  memory_agg_test_layouts(profile, m,
			[i8_type, i32_type, i16_type])
		scalar_constants: [
			memory_agg_test_constant(field_values[0], i8_type, 0x12),
			memory_agg_test_constant(field_values[1], i32_type, 0x34567890),
			memory_agg_test_constant(field_values[2], i16_type, 0x5678),
			memory_agg_test_constant(insert_value_id, i32_type, 0x10203040),
		]
		aggregate_layouts: [memory_agg_test_aggregate_layout(profile, m, aggregate_type)]
		aggregate_allocas: [
			MemoryAggAggregateAllocaBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, alloca_id)
				authority:         .native_plain
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
				role:              .fixed_alloca
			},
		]
		aggregate_constructs: [
			MemoryAggAggregateConstructBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, construct_id)
				result_value_id:   construct_id
				aggregate_type_id: aggregate_type
				padding_policy:    padding
			},
		]
		aggregate_loads: [
			MemoryAggAggregateLoadBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, load_id)
				semantics:         .nonvolatile
				pointer_value_id:  alloca_id
				result_value_id:   load_id
				aggregate_type_id: aggregate_type
			},
		]
		aggregate_stores: [
			MemoryAggAggregateStoreBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index,
					first_store_id)
				semantics:         .nonvolatile
				source_value_id:   construct_id
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
			},
			MemoryAggAggregateStoreBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index,
					second_store_id)
				semantics:         .nonvolatile
				source_value_id:   insert_id
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
			},
		]
		aggregate_extracts: [
			MemoryAggAggregateExtractBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, extract_id)
				source_value_id:   load_id
				result_value_id:   extract_id
				aggregate_type_id: aggregate_type
				field_index:       1
			},
		]
		aggregate_inserts: [
			MemoryAggAggregateInsertBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, insert_id)
				source_value_id:   load_id
				field_value_id:    insert_value_id
				result_value_id:   insert_id
				aggregate_type_id: aggregate_type
				field_index:       1
			},
		]
	}
	return fixture, facts
}

fn memory_agg_test_builder_aggregate_fixture(profile TargetProfile) (MemoryAggTestAggregateFixture, MemoryAggFunctionFacts) {
	mut m := ssa.Module.new()
	i1_type := memory_agg_test_int_type(mut m, 1, false)
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	i16_type := memory_agg_test_int_type(mut m, 16, false)
	i32_type := memory_agg_test_int_type(mut m, 32, false)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	field_types := [i8_type, i32_type, i16_type]
	aggregate_type := memory_agg_test_struct_type(mut m, field_types)
	aggregate_pointer := memory_agg_test_pointer_type(mut m, aggregate_type)
	layout := memory_agg_test_aggregate_layout(profile, m, aggregate_type)
	function_index := m.new_function('memory_aggregate_builder', ssa.TypeID(0))
	block_id := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block_id, aggregate_pointer, [])
	mut field_values := []ssa.ValueID{}
	mut delta_values := []ssa.ValueID{}
	mut field_pointers := []ssa.ValueID{}
	mut field_stores := []ssa.ValueID{}
	mut constants := []ScalarConstantBinding{}
	mut field_pointer_facts := []MemoryAggAggregateFieldPointerBinding{}
	mut accesses := []MemoryAggAccessBinding{}
	for field_index, field in layout.fields {
		value_id := m.add_value(.constant, field.type_id, 'builder-field-${field_index}',
			0)
		delta_id := m.add_value(.constant, i64_type, 'builder-delta-${field_index}', 0)
		pointer_type := memory_agg_test_pointer_type(mut m, field.type_id)
		field_pointer := m.add_instr(.get_element_ptr, block_id, pointer_type,
			[alloca_id, delta_id])
		store_id := m.add_instr(.store, block_id, ssa.TypeID(0),
			[value_id, field_pointer])
		field_values << value_id
		delta_values << delta_id
		field_pointers << field_pointer
		field_stores << store_id
		constants << memory_agg_test_constant(value_id, field.type_id,
			u64(0x11 + field_index))
		constants << memory_agg_test_constant(delta_id, i64_type, field.offset_bytes)
		field_pointer_facts << MemoryAggAggregateFieldPointerBinding{
			profile:                 profile
			anchor:                  memory_agg_test_anchor(m, function_index,
				field_pointer)
			source_pointer_value_id: alloca_id
			result_pointer_value_id: field_pointer
			aggregate_type_id:       aggregate_type
			field_index:             u16(field_index)
		}
		accesses << memory_agg_test_access(profile, m, function_index, store_id)
	}
	extract_id := m.add_instr(.load, block_id, i32_type, [field_pointers[1]])
	accesses << memory_agg_test_access(profile, m, function_index, extract_id)
	load_id := m.add_instr(.load, block_id, aggregate_type, [alloca_id])
	aggregate_store_id := m.add_instr(.store, block_id, ssa.TypeID(0),
		[load_id, alloca_id])
	ret_id := m.add_instr(.ret, block_id, ssa.TypeID(0), [])
	fixture := MemoryAggTestAggregateFixture{
		m:                 m
		profile:           profile
		function_index:    function_index
		block_id:          block_id
		i1_type:           i1_type
		i8_type:           i8_type
		i16_type:          i16_type
		i32_type:          i32_type
		i64_type:          i64_type
		aggregate_type:    aggregate_type
		aggregate_pointer: aggregate_pointer
		alloca_id:         alloca_id
		field_values:      field_values
		delta_values:      delta_values
		field_pointers:    field_pointers
		field_stores:      field_stores
		extract_id:        extract_id
		load_id:           load_id
		first_store_id:    aggregate_store_id
		ret_id:            ret_id
	}
	facts := MemoryAggFunctionFacts{
		profile:                  profile
		function_index:           function_index
		ssa_form:                  .final_static
		scalar_layouts:           memory_agg_test_layouts(profile, m,
			[i8_type, i32_type, i16_type, i64_type])
		scalar_constants:         constants
		accesses:                 accesses
		aggregate_layouts:        [layout]
		aggregate_allocas:        [
			MemoryAggAggregateAllocaBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, alloca_id)
				authority:         .native_plain
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
				role:              .aggregate_temp
			},
		]
		aggregate_field_pointers: field_pointer_facts
		aggregate_loads:          [
			MemoryAggAggregateLoadBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index, load_id)
				semantics:         .nonvolatile
				pointer_value_id:  alloca_id
				result_value_id:   load_id
				aggregate_type_id: aggregate_type
			},
		]
		aggregate_stores:         [
			MemoryAggAggregateStoreBinding{
				profile:           profile
				anchor:            memory_agg_test_anchor(m, function_index,
					aggregate_store_id)
				semantics:         .nonvolatile
				source_value_id:   load_id
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
			},
		]
	}
	return fixture, facts
}

fn memory_agg_test_reanchor_aggregate_facts(m &ssa.Module, facts MemoryAggFunctionFacts) MemoryAggFunctionFacts {
	mut allocas := []MemoryAggAggregateAllocaBinding{cap: facts.aggregate_allocas.len}
	for fact in facts.aggregate_allocas {
		allocas << MemoryAggAggregateAllocaBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index, fact.pointer_value_id)
		}
	}
	mut field_pointers := []MemoryAggAggregateFieldPointerBinding{
		cap: facts.aggregate_field_pointers.len
	}
	for fact in facts.aggregate_field_pointers {
		field_pointers << MemoryAggAggregateFieldPointerBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index,
				fact.result_pointer_value_id)
		}
	}
	mut constructs := []MemoryAggAggregateConstructBinding{cap: facts.aggregate_constructs.len}
	for fact in facts.aggregate_constructs {
		constructs << MemoryAggAggregateConstructBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index, fact.result_value_id)
		}
	}
	mut loads := []MemoryAggAggregateLoadBinding{cap: facts.aggregate_loads.len}
	for fact in facts.aggregate_loads {
		loads << MemoryAggAggregateLoadBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index, fact.result_value_id)
		}
	}
	mut stores := []MemoryAggAggregateStoreBinding{cap: facts.aggregate_stores.len}
	for fact in facts.aggregate_stores {
		stores << MemoryAggAggregateStoreBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index,
				fact.anchor.instruction_value_id)
		}
	}
	mut extracts := []MemoryAggAggregateExtractBinding{cap: facts.aggregate_extracts.len}
	for fact in facts.aggregate_extracts {
		extracts << MemoryAggAggregateExtractBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index, fact.result_value_id)
		}
	}
	mut inserts := []MemoryAggAggregateInsertBinding{cap: facts.aggregate_inserts.len}
	for fact in facts.aggregate_inserts {
		inserts << MemoryAggAggregateInsertBinding{
			...fact
			anchor: memory_agg_test_anchor(m, facts.function_index, fact.result_value_id)
		}
	}
	return MemoryAggFunctionFacts{
		...facts
		aggregate_allocas:        allocas
		aggregate_field_pointers: field_pointers
		aggregate_constructs:     constructs
		aggregate_loads:          loads
		aggregate_stores:         stores
		aggregate_extracts:       extracts
		aggregate_inserts:        inserts
	}
}

fn test_memory_agg_m101_empty_plan_for_all_profiles() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		m, function_index := memory_agg_test_empty_module()
		facts := memory_agg_test_facts(profile, function_index, [], [], [], [])
		plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
		assert plan.profile == profile
		assert plan.function_index == function_index
		assert plan.function_id == u32(function_index)
		assert plan.scalar_layouts.len == 0
		assert plan.slot_requests.len == 0
		assert plan.pointers.len == 0
		assert plan.accesses.len == 0
		assert plan.total_requested_bytes == 0
	}
}

fn test_memory_agg_m102_exhaustive_native_scalar_layouts_and_signed_i1() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	mut type_ids := []ssa.TypeID{}
	type_ids << memory_agg_test_int_type(mut m, 1, false)
	for width in [8, 16, 32, 64] {
		type_ids << memory_agg_test_int_type(mut m, width, false)
		type_ids << memory_agg_test_int_type(mut m, width, true)
	}
	function_index := m.new_function('all_scalar_layouts', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	mut alloca_ids := []ssa.ValueID{}
	for type_id in type_ids {
		pointer_type := memory_agg_test_pointer_type(mut m, type_id)
		alloca_ids << m.add_instr(.alloca, block, pointer_type, [])
	}
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	mut locals := []MemoryAggStaticLocalBinding{}
	for index, alloca_id in alloca_ids {
		locals << memory_agg_test_local(profile, m, function_index, alloca_id,
			type_ids[index], .one, ssa.ValueID(0))
	}
	facts := memory_agg_test_facts(profile, function_index,
		memory_agg_test_layouts(profile, m, type_ids), locals, [], [])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.scalar_layouts.len == 9
	assert plan.slot_requests.len == 9
	assert plan.total_requested_bytes == 31
	assert plan.scalar_layouts[0].semantic_width_bits == 1
	assert !plan.scalar_layouts[0].semantic_is_unsigned
	assert plan.scalar_layouts[0].storage_width_bytes == 1

	mut unsigned_i1 := memory_agg_test_alloca_fixture(profile, 1, true, false, 0)
	unsigned_facts := memory_agg_test_alloca_facts(&unsigned_i1, 0)
	memory_agg_test_expect_error(unsigned_i1.m, &unsigned_facts, 'unsigned i1')
}

fn test_memory_agg_m103_layout_missing_duplicate_stale_and_orphan_facts() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut fixture := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	local := memory_agg_test_local(profile, fixture.m, fixture.function_index,
		fixture.alloca_id, fixture.element_type, .one, ssa.ValueID(0))
	missing := memory_agg_test_facts(profile, fixture.function_index, [], [local], [], [])
	memory_agg_test_expect_error(fixture.m, &missing, 'incomplete_fact')

	layout := memory_agg_test_layout(profile, fixture.m, fixture.element_type)
	duplicate := memory_agg_test_facts(profile, fixture.function_index, [layout, layout],
		[local], [], [])
	memory_agg_test_expect_error(fixture.m, &duplicate, 'duplicate_fact')

	stale_layout := MemoryAggScalarLayoutBinding{
		profile:              layout.profile
		type_id:              layout.type_id
		authority:            layout.authority
		semantic_width_bits:  layout.semantic_width_bits
		semantic_is_unsigned: layout.semantic_is_unsigned
		storage_width_bytes:  8
		alignment_bytes:      layout.alignment_bytes
	}
	stale := memory_agg_test_facts(profile, fixture.function_index, [stale_layout], [local],
		[], [])
	memory_agg_test_expect_error(fixture.m, &stale, 'stale_fact')

	extra_type := memory_agg_test_int_type(mut fixture.m, 8, false)
	orphan := memory_agg_test_facts(profile, fixture.function_index,
		[layout, memory_agg_test_layout(profile, fixture.m, extra_type)], [local], [], [])
	memory_agg_test_expect_error(fixture.m, &orphan, 'orphan_fact')
}

fn test_memory_agg_m104_zero_operand_fixed_alloca_contract() {
	profile := TargetProfile.macos_x86_64_sysv_macho
	fixture := memory_agg_test_alloca_fixture(profile, 16, false, false, 0)
	facts := memory_agg_test_alloca_facts(&fixture, 0)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.slot_requests.len == 1
	assert plan.slot_requests[0].alloca_value_id == fixture.alloca_id
	assert plan.slot_requests[0].element_count == 1
	assert plan.slot_requests[0].request.kind == .fixed_alloca
	assert plan.slot_requests[0].request.size_bytes == 2
	assert plan.slot_requests[0].request.alignment_bytes == 2
	assert plan.pointers[0].origin == .fixed_alloca

	bad_local := memory_agg_test_local(profile, fixture.m, fixture.function_index,
		fixture.alloca_id, fixture.element_type, .constant_count, ssa.ValueID(0))
	bad := memory_agg_test_facts(profile, fixture.function_index, facts.scalar_layouts,
		[bad_local], [], [])
	memory_agg_test_expect_error(fixture.m, &bad, 'requires form one')
}

fn test_memory_agg_m105_positive_constant_count_alloca() {
	profile := TargetProfile.windows_x86_64_microsoft_abi_coff
	fixture := memory_agg_test_alloca_fixture(profile, 16, true, true, 3)
	facts := memory_agg_test_alloca_facts(&fixture, 3)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.scalar_layouts.len == 2
	assert plan.slot_requests.len == 1
	assert plan.slot_requests[0].element_type == fixture.element_type
	assert plan.slot_requests[0].element_count == 3
	assert plan.slot_requests[0].request.size_bytes == 6
	assert plan.total_requested_bytes == 6
}

fn test_memory_agg_m106_count_zero_negative_dynamic_cap_and_overflow_refuse() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	zero := memory_agg_test_alloca_fixture(profile, 8, false, true, 0)
	zero_facts := memory_agg_test_alloca_facts(&zero, 0)
	memory_agg_test_expect_error(zero.m, &zero_facts, 'count must be positive')

	negative := memory_agg_test_alloca_fixture(profile, 8, false, true, max_u64)
	negative_facts := memory_agg_test_alloca_facts(&negative, max_u64)
	memory_agg_test_expect_error(negative.m, &negative_facts, 'count must be positive')

	mut dynamic := ssa.Module.new()
	element_type := memory_agg_test_int_type(mut dynamic, 8, false)
	i64_type := memory_agg_test_int_type(mut dynamic, 64, false)
	pointer_type := memory_agg_test_pointer_type(mut dynamic, element_type)
	function_index := dynamic.new_function('dynamic_alloca', ssa.TypeID(0))
	count_argument := dynamic.add_value(.argument, i64_type, 'count', 0)
	dynamic.func_add_param(function_index, count_argument)
	block := dynamic.add_block(function_index, 'entry')
	alloca_id := dynamic.add_instr(.alloca, block, pointer_type, [count_argument])
	dynamic.add_instr(.ret, block, ssa.TypeID(0), [])
	dynamic_facts := memory_agg_test_facts(profile, function_index,
		memory_agg_test_layouts(profile, dynamic, [element_type, i64_type]),
		[memory_agg_test_local(profile, dynamic, function_index, alloca_id, element_type,
			.constant_count, count_argument)], [], [])
	memory_agg_test_expect_error(dynamic, &dynamic_facts, 'count is dynamic')

	cap_exceeded := memory_agg_test_alloca_fixture(profile, 64, false, true, 0x10000000)
	cap_facts := memory_agg_test_alloca_facts(&cap_exceeded, 0x10000000)
	memory_agg_test_expect_error(cap_exceeded.m, &cap_facts, 'total slot bytes')

	overflow := memory_agg_test_alloca_fixture(profile, 64, false, true,
		0x7fffffffffffffff)
	overflow_facts := memory_agg_test_alloca_facts(&overflow, 0x7fffffffffffffff)
	memory_agg_test_expect_error(overflow.m, &overflow_facts, 'arithmetic overflow')
}

fn test_memory_agg_m107_stable_m0_ids_order_collision_and_slot_cap() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
	function_index := m.new_function('slot_order', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	first_id := m.add_instr(.alloca, block, pointer_type, [])
	second_id := m.add_instr(.alloca, block, pointer_type, [])
	ret_id := m.add_instr(.ret, block, ssa.TypeID(0), [])
	mut rewritten_block := m.blocks[int(block)]
	rewritten_block.instrs = [second_id, first_id, ret_id]
	m.blocks[int(block)] = rewritten_block
	facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, m, i8_type)],
		[
			memory_agg_test_local(profile, m, function_index, second_id, i8_type, .one,
				ssa.ValueID(0)),
			memory_agg_test_local(profile, m, function_index, first_id, i8_type, .one,
				ssa.ValueID(0)),
		], [], [])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.slot_requests.len == 2
	assert plan.slot_requests[0].request.id == u32(first_id)
	assert plan.slot_requests[1].request.id == u32(second_id)

	mut collision := memory_agg_test_alloca_fixture(profile, 8, false, false, 0)
	mut collision_value := collision.m.values[int(collision.alloca_id)]
	collision_value.id = collision.ret_id
	collision.m.values[int(collision.alloca_id)] = collision_value
	collision_facts := memory_agg_test_alloca_facts(&collision, 0)
	memory_agg_test_expect_error(collision.m, &collision_facts, 'canonical instruction value')

	empty, empty_function := memory_agg_test_empty_module()
	dummy := MemoryAggStaticLocalBinding{
		profile:   profile
		authority: .native_plain
		form:      .one
	}
	too_many := []MemoryAggStaticLocalBinding{len: memory_agg_max_static_locals + 1, init: dummy}
	cap_facts := memory_agg_test_facts(profile, empty_function, [], too_many, [], [])
	memory_agg_test_expect_error(empty, &cap_facts, 'static local count')
}

struct MemoryAggTestPointerFixture {
mut:
	m              &ssa.Module
	profile        TargetProfile
	function_index int
	element_type   ssa.TypeID
	i64_type       ssa.TypeID
	alloca_id      ssa.ValueID
	count_id       ssa.ValueID
	delta_id       ssa.ValueID
	pointer_id     ssa.ValueID
}

fn memory_agg_test_pointer_fixture(profile TargetProfile, opcode ssa.OpCode, width int, count_bits u64, delta_bits u64) MemoryAggTestPointerFixture {
	mut m := ssa.Module.new()
	element_type := memory_agg_test_int_type(mut m, width, false)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	pointer_type := memory_agg_test_pointer_type(mut m, element_type)
	function_index := m.new_function('byte_delta_pointer', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	count_id := m.add_value(.constant, i64_type, 'count', 0)
	delta_id := m.add_value(.constant, i64_type, 'already-scaled-byte-delta', 0)
	alloca_id := m.add_instr(.alloca, block, pointer_type, [count_id])
	pointer_id := m.add_instr(opcode, block, pointer_type, [alloca_id, delta_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	return MemoryAggTestPointerFixture{
		m:              m
		profile:        profile
		function_index: function_index
		element_type:   element_type
		i64_type:       i64_type
		alloca_id:      alloca_id
		count_id:       count_id
		delta_id:       delta_id
		pointer_id:     pointer_id
	}
}

fn memory_agg_test_pointer_facts(fixture &MemoryAggTestPointerFixture, count_bits u64, delta_bits u64) MemoryAggFunctionFacts {
	return memory_agg_test_facts(fixture.profile, fixture.function_index,
		memory_agg_test_layouts(fixture.profile, fixture.m,
			[fixture.element_type, fixture.i64_type]),
		[memory_agg_test_local(fixture.profile, fixture.m, fixture.function_index,
			fixture.alloca_id, fixture.element_type, .constant_count, fixture.count_id)],
		[
			memory_agg_test_constant(fixture.count_id, fixture.i64_type, count_bits),
			memory_agg_test_constant(fixture.delta_id, fixture.i64_type, delta_bits),
		], [])
}

struct MemoryAggTestLoadFixture {
mut:
	m              &ssa.Module
	profile        TargetProfile
	function_index int
	element_type   ssa.TypeID
	i64_type       ssa.TypeID
	alloca_id      ssa.ValueID
	count_id       ssa.ValueID
	delta_id       ssa.ValueID
	pointer_id     ssa.ValueID
	load_id        ssa.ValueID
}

fn memory_agg_test_load_fixture(profile TargetProfile, width int, count_bits u64, has_delta bool, delta_bits u64) MemoryAggTestLoadFixture {
	mut m := ssa.Module.new()
	element_type := memory_agg_test_int_type(mut m, width, false)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	pointer_type := memory_agg_test_pointer_type(mut m, element_type)
	function_index := m.new_function('scalar_load', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	count_id := m.add_value(.constant, i64_type, 'count', 0)
	alloca_id := m.add_instr(.alloca, block, pointer_type, [count_id])
	mut delta_id := ssa.ValueID(0)
	mut pointer_id := alloca_id
	if has_delta {
		delta_id = m.add_value(.constant, i64_type, 'already-scaled-byte-delta', 0)
		pointer_id = m.add_instr(.get_element_ptr, block, pointer_type,
			[alloca_id, delta_id])
	}
	load_id := m.add_instr(.load, block, element_type, [pointer_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	return MemoryAggTestLoadFixture{
		m:              m
		profile:        profile
		function_index: function_index
		element_type:   element_type
		i64_type:       i64_type
		alloca_id:      alloca_id
		count_id:       count_id
		delta_id:       delta_id
		pointer_id:     pointer_id
		load_id:        load_id
	}
}

fn memory_agg_test_load_facts(fixture &MemoryAggTestLoadFixture, count_bits u64, delta_bits u64) MemoryAggFunctionFacts {
	mut constants := [
		memory_agg_test_constant(fixture.count_id, fixture.i64_type, count_bits),
	]
	if fixture.delta_id != ssa.ValueID(0) {
		constants << memory_agg_test_constant(fixture.delta_id, fixture.i64_type, delta_bits)
	}
	return memory_agg_test_facts(fixture.profile, fixture.function_index,
		memory_agg_test_layouts(fixture.profile, fixture.m,
			[fixture.element_type, fixture.i64_type]),
		[memory_agg_test_local(fixture.profile, fixture.m, fixture.function_index,
			fixture.alloca_id, fixture.element_type, .constant_count, fixture.count_id)],
		constants,
		[memory_agg_test_access(fixture.profile, fixture.m, fixture.function_index,
			fixture.load_id)])
}

fn test_memory_agg_m108_positive_gep_is_one_already_scaled_byte_delta() {
	fixture := memory_agg_test_pointer_fixture(.linux_x86_64_sysv_elf,
		.get_element_ptr, 8, 8, 3)
	facts := memory_agg_test_pointer_facts(&fixture, 8, 3)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.pointers.len == 2
	assert plan.pointers[0].value_id == fixture.alloca_id
	assert plan.pointers[1].value_id == fixture.pointer_id
	assert plan.pointers[1].origin == .byte_delta
	assert plan.pointers[1].byte_offset == 3
	assert plan.pointers[1].remaining_bytes == 5
	assert !plan.pointers[1].is_one_past
}

fn test_memory_agg_m109_pointer_add_has_exact_operand_order_and_byte_meaning() {
	mut fixture := memory_agg_test_pointer_fixture(.macos_x86_64_sysv_macho, .add,
		8, 8, 5)
	facts := memory_agg_test_pointer_facts(&fixture, 8, 5)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.pointers[1].byte_offset == 5

	value := fixture.m.values[int(fixture.pointer_id)]
	mut reversed := fixture.m.instrs[value.index]
	reversed.operands = [fixture.delta_id, fixture.alloca_id]
	fixture.m.instrs[value.index] = reversed
	reversed_facts := memory_agg_test_pointer_facts(&fixture, 8, 5)
	memory_agg_test_expect_error(fixture.m, &reversed_facts, 'operand 0 requires pointer')
}

fn test_memory_agg_m110_negative_delta_min_i64_before_root_and_large_positive() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
	function_index := m.new_function('negative_delta', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	count_id := m.add_value(.constant, i64_type, 'count', 0)
	positive_id := m.add_value(.constant, i64_type, 'positive-four', 0)
	negative_id := m.add_value(.constant, i64_type, 'negative-two', 0)
	alloca_id := m.add_instr(.alloca, block, pointer_type, [count_id])
	at_four := m.add_instr(.get_element_ptr, block, pointer_type, [alloca_id, positive_id])
	at_two := m.add_instr(.add, block, pointer_type, [at_four, negative_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(profile, function_index,
		memory_agg_test_layouts(profile, m, [i8_type, i64_type]),
		[memory_agg_test_local(profile, m, function_index, alloca_id, i8_type,
			.constant_count, count_id)],
		[
			memory_agg_test_constant(count_id, i64_type, 8),
			memory_agg_test_constant(positive_id, i64_type, 4),
			memory_agg_test_constant(negative_id, i64_type, 0xfffffffffffffffe),
		], [])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.pointers[2].value_id == at_two
	assert plan.pointers[2].byte_offset == 2

	minimum := memory_agg_test_pointer_fixture(profile, .get_element_ptr, 8, 8,
		0x8000000000000000)
	minimum_facts := memory_agg_test_pointer_facts(&minimum, 8, 0x8000000000000000)
	memory_agg_test_expect_error(minimum.m, &minimum_facts, 'before its local root')

	before := memory_agg_test_pointer_fixture(profile, .get_element_ptr, 8, 8, max_u64)
	before_facts := memory_agg_test_pointer_facts(&before, 8, max_u64)
	memory_agg_test_expect_error(before.m, &before_facts, 'before its local root')

	large := memory_agg_test_pointer_fixture(profile, .get_element_ptr, 8, 8,
		0x7fffffffffffffff)
	large_facts := memory_agg_test_pointer_facts(&large, 8, 0x7fffffffffffffff)
	memory_agg_test_expect_error(large.m, &large_facts, 'beyond one-past')
}

fn test_memory_agg_m111_one_past_creation_is_valid_but_dereference_refuses() {
	one_past := memory_agg_test_pointer_fixture(.linux_x86_64_sysv_elf,
		.get_element_ptr, 8, 8, 8)
	one_past_facts := memory_agg_test_pointer_facts(&one_past, 8, 8)
	plan := plan_scalar_static_memory(one_past.m, &one_past_facts) or { panic(err) }
	assert plan.pointers[1].byte_offset == 8
	assert plan.pointers[1].remaining_bytes == 0
	assert plan.pointers[1].is_one_past

	at_end := memory_agg_test_load_fixture(.linux_x86_64_sysv_elf, 8, 8, true, 7)
	at_end_facts := memory_agg_test_load_facts(&at_end, 8, 7)
	end_plan := plan_scalar_static_memory(at_end.m, &at_end_facts) or { panic(err) }
	assert end_plan.accesses[0].byte_offset == 7

	dereference := memory_agg_test_load_fixture(.linux_x86_64_sysv_elf, 8, 8,
		true, 8)
	dereference_facts := memory_agg_test_load_facts(&dereference, 8, 8)
	memory_agg_test_expect_error(dereference.m, &dereference_facts, 'one-past')
}

fn test_memory_agg_m112_pointer_bitcast_preserves_provenance_and_checks_alignment() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i64_type := memory_agg_test_int_type(mut m, 64, false)
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	i64_pointer := memory_agg_test_pointer_type(mut m, i64_type)
	i8_pointer := memory_agg_test_pointer_type(mut m, i8_type)
	function_index := m.new_function('aligned_bitcast', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block, i64_pointer, [])
	bitcast_id := m.add_instr(.bitcast, block, i8_pointer, [alloca_id])
	load_id := m.add_instr(.load, block, i8_type, [bitcast_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(profile, function_index,
		memory_agg_test_layouts(profile, m, [i64_type, i8_type]),
		[memory_agg_test_local(profile, m, function_index, alloca_id, i64_type, .one,
			ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, m, function_index, load_id)])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.pointers[1].origin == .bitcast
	assert plan.pointers[1].root_slot_id == plan.pointers[0].root_slot_id
	assert plan.pointers[1].root_size_bytes == 8
	assert plan.pointers[1].pointee_type == i8_type

	mut unaligned := ssa.Module.new()
	unaligned_i8 := memory_agg_test_int_type(mut unaligned, 8, false)
	unaligned_i64 := memory_agg_test_int_type(mut unaligned, 64, false)
	count_type := memory_agg_test_int_type(mut unaligned, 64, false)
	root_pointer := memory_agg_test_pointer_type(mut unaligned, unaligned_i8)
	wide_pointer := memory_agg_test_pointer_type(mut unaligned, unaligned_i64)
	unaligned_function := unaligned.new_function('unaligned_bitcast', ssa.TypeID(0))
	unaligned_block := unaligned.add_block(unaligned_function, 'entry')
	count_id := unaligned.add_value(.constant, count_type, 'count', 0)
	root_id := unaligned.add_instr(.alloca, unaligned_block, root_pointer, [count_id])
	wide_id := unaligned.add_instr(.bitcast, unaligned_block, wide_pointer, [root_id])
	wide_load := unaligned.add_instr(.load, unaligned_block, unaligned_i64, [wide_id])
	unaligned.add_instr(.ret, unaligned_block, ssa.TypeID(0), [])
	unaligned_facts := memory_agg_test_facts(profile, unaligned_function,
		memory_agg_test_layouts(profile, unaligned, [unaligned_i8, unaligned_i64]),
		[memory_agg_test_local(profile, unaligned, unaligned_function, root_id,
			unaligned_i8, .constant_count, count_id)],
		[memory_agg_test_constant(count_id, count_type, 8)],
		[memory_agg_test_access(profile, unaligned, unaligned_function, wide_load)])
	memory_agg_test_expect_error(unaligned, &unaligned_facts, 'naturally aligned')
}

fn test_memory_agg_m113_zero_operand_bitcast_tombstone_always_refuses() {
	mut m := ssa.Module.new()
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
	function_index := m.new_function('bitcast_tombstone', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	m.add_instr(.bitcast, block, pointer_type, [])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(.linux_x86_64_sysv_elf, function_index, [], [], [],
		[])
	memory_agg_test_expect_error(m, &facts, 'zero-operand BITCAST tombstone')
}

fn test_memory_agg_m114_scalar_load_and_i1_byte_canonicalization() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	fixture := memory_agg_test_load_fixture(profile, 1, 1, false, 0)
	facts := memory_agg_test_load_facts(&fixture, 1, 0)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.accesses.len == 1
	assert plan.accesses[0].kind == .load
	assert plan.accesses[0].semantic_width_bits == 1
	assert plan.accesses[0].storage_width_bytes == 1
	assert plan.accesses[0].alignment_bytes == 1
	assert plan.accesses[0].canonicalize_i1
	assert !plan.accesses[0].semantic_is_unsigned

	narrow := memory_agg_test_load_fixture(profile, 8, 1, false, 0)
	narrow_facts := memory_agg_test_load_facts(&narrow, 1, 0)
	narrow_plan := plan_scalar_static_memory(narrow.m, &narrow_facts) or { panic(err) }
	assert !narrow_plan.accesses[0].canonicalize_i1
	assert narrow_plan.accesses[0].storage_width_bytes == 1
}

fn test_memory_agg_m115_scalar_store_operand_type_and_void_contract() {
	profile := TargetProfile.windows_x86_64_microsoft_abi_coff
	mut m := ssa.Module.new()
	i32_type := memory_agg_test_int_type(mut m, 32, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i32_type)
	function_index := m.new_function('scalar_store', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block, pointer_type, [])
	scalar_id := m.add_value(.constant, i32_type, 'store-value-without-bits-sidecar', 0)
	store_id := m.add_instr(.store, block, ssa.TypeID(0), [scalar_id, alloca_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, m, i32_type)],
		[memory_agg_test_local(profile, m, function_index, alloca_id, i32_type, .one,
			ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, m, function_index, store_id)])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.accesses[0].kind == .store
	assert plan.accesses[0].scalar_value_id == scalar_id
	assert plan.accesses[0].pointer_value_id == alloca_id
	assert plan.accesses[0].storage_width_bytes == 4

	mut wrong_order := m
	store_value := wrong_order.values[int(store_id)]
	mut reversed := wrong_order.instrs[store_value.index]
	reversed.operands = [alloca_id, scalar_id]
	wrong_order.instrs[store_value.index] = reversed
	wrong_order_facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, wrong_order, i32_type)],
		[memory_agg_test_local(profile, wrong_order, function_index, alloca_id, i32_type,
			.one, ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, wrong_order, function_index, store_id)])
	memory_agg_test_expect_error(wrong_order, &wrong_order_facts, 'requires scalar integer type')

	mut wrong_void := m
	mut wrong_instruction := wrong_void.instrs[store_value.index]
	wrong_instruction.typ = i32_type
	wrong_void.instrs[store_value.index] = wrong_instruction
	mut wrong_value := wrong_void.values[int(store_id)]
	wrong_value.typ = i32_type
	wrong_void.values[int(store_id)] = wrong_value
	wrong_void_facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, wrong_void, i32_type)],
		[memory_agg_test_local(profile, wrong_void, function_index, alloca_id, i32_type,
			.one, ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, wrong_void, function_index, store_id)])
	memory_agg_test_expect_error(wrong_void, &wrong_void_facts, 'result must be canonical void')
}

fn test_memory_agg_m116_access_facts_are_exhaustive_exact_and_nonvolatile() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut fixture := memory_agg_test_load_fixture(profile, 32, 1, false, 0)
	valid := memory_agg_test_load_facts(&fixture, 1, 0)
	missing := memory_agg_test_facts(profile, fixture.function_index, valid.scalar_layouts,
		valid.static_locals, valid.scalar_constants, [])
	memory_agg_test_expect_error(fixture.m, &missing, 'incomplete_fact')

	access := valid.accesses[0]
	volatile_access := MemoryAggAccessBinding{
		profile:          access.profile
		anchor:           access.anchor
		semantics:        .volatile
		kind:             access.kind
		pointer_value_id: access.pointer_value_id
		scalar_value_id:  access.scalar_value_id
		scalar_type:      access.scalar_type
	}
	volatile_facts := memory_agg_test_facts(profile, fixture.function_index,
		valid.scalar_layouts, valid.static_locals, valid.scalar_constants, [volatile_access])
	memory_agg_test_expect_error(fixture.m, &volatile_facts, 'not nonvolatile')

	duplicate := memory_agg_test_facts(profile, fixture.function_index, valid.scalar_layouts,
		valid.static_locals, valid.scalar_constants, [access, access])
	memory_agg_test_expect_error(fixture.m, &duplicate, 'duplicate_fact')

	stale_access := MemoryAggAccessBinding{
		profile:          access.profile
		anchor:           access.anchor
		semantics:        access.semantics
		kind:             .store
		pointer_value_id: access.pointer_value_id
		scalar_value_id:  access.scalar_value_id
		scalar_type:      access.scalar_type
	}
	stale := memory_agg_test_facts(profile, fixture.function_index, valid.scalar_layouts,
		valid.static_locals, valid.scalar_constants, [stale_access])
	memory_agg_test_expect_error(fixture.m, &stale, 'stale_fact')

	load_value := fixture.m.values[int(fixture.load_id)]
	mut atomic_instruction := fixture.m.instrs[load_value.index]
	atomic_instruction.atomic_ord = .acquire
	fixture.m.instrs[load_value.index] = atomic_instruction
	atomic_facts := memory_agg_test_load_facts(&fixture, 1, 0)
	memory_agg_test_expect_error(fixture.m, &atomic_facts, 'is atomic')
}

fn test_memory_agg_m117_structural_owner_index_block_and_anchor_corruption_refuses() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut bad_index := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	bad_index_facts := memory_agg_test_alloca_facts(&bad_index, 0)
	mut indexed_value := bad_index.m.values[int(bad_index.alloca_id)]
	indexed_value.index = bad_index.m.instrs.len + 9
	bad_index.m.values[int(bad_index.alloca_id)] = indexed_value
	memory_agg_test_expect_error(bad_index.m, &bad_index_facts,
		'invalid instruction index')

	mut bad_block := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	bad_block_facts := memory_agg_test_alloca_facts(&bad_block, 0)
	alloca_value := bad_block.m.values[int(bad_block.alloca_id)]
	mut displaced := bad_block.m.instrs[alloca_value.index]
	displaced.block = ssa.BlockID(999)
	bad_block.m.instrs[alloca_value.index] = displaced
	memory_agg_test_expect_error(bad_block.m, &bad_block_facts,
		'block/type ownership is inconsistent')

	mut bad_parent := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	bad_parent_facts := memory_agg_test_alloca_facts(&bad_parent, 0)
	mut orphan_block := bad_parent.m.blocks[int(bad_parent.block_id)]
	orphan_block.parent = 7
	bad_parent.m.blocks[int(bad_parent.block_id)] = orphan_block
	memory_agg_test_expect_error(bad_parent.m, &bad_parent_facts,
		'block 0 ownership is inconsistent')

	stale_anchor := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	valid := memory_agg_test_alloca_facts(&stale_anchor, 0)
	local := valid.static_locals[0]
	bad_anchor := MemoryAggInstructionAnchor{
		function_index:       local.anchor.function_index
		block_id:             local.anchor.block_id
		block_ordinal:        local.anchor.block_ordinal
		instruction_value_id: local.anchor.instruction_value_id
		instruction_index:    local.anchor.instruction_index + 1
		instruction_ordinal:  local.anchor.instruction_ordinal
	}
	stale_local := MemoryAggStaticLocalBinding{
		profile:        local.profile
		anchor:         bad_anchor
		authority:      local.authority
		element_type:   local.element_type
		form:           local.form
		count_value_id: local.count_value_id
	}
	stale_facts := memory_agg_test_facts(profile, stale_anchor.function_index,
		valid.scalar_layouts, [stale_local], valid.scalar_constants, [])
	memory_agg_test_expect_error(stale_anchor.m, &stale_facts, 'stale_fact')
}

fn test_memory_agg_m118_mutable_value_uses_are_ignored_and_not_modified() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut fixture := memory_agg_test_load_fixture(profile, 16, 1, false, 0)
	facts := memory_agg_test_load_facts(&fixture, 1, 0)
	mut alloca_value := fixture.m.values[int(fixture.alloca_id)]
	alloca_value.uses = [ssa.ValueID(999), fixture.load_id, fixture.load_id]
	fixture.m.values[int(fixture.alloca_id)] = alloca_value
	mut load_value := fixture.m.values[int(fixture.load_id)]
	load_value.uses = [ssa.ValueID(-77)]
	fixture.m.values[int(fixture.load_id)] = load_value
	before_alloca_uses := fixture.m.values[int(fixture.alloca_id)].uses.clone()
	before_load_uses := fixture.m.values[int(fixture.load_id)].uses.clone()
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.accesses.len == 1
	assert fixture.m.values[int(fixture.alloca_id)].uses == before_alloca_uses
	assert fixture.m.values[int(fixture.load_id)].uses == before_load_uses
}

fn test_memory_agg_m119_cross_block_definition_dominates_access() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i32_type := memory_agg_test_int_type(mut m, 32, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i32_type)
	function_index := m.new_function('dominating_local', ssa.TypeID(0))
	entry := m.add_block(function_index, 'entry')
	body := m.add_block(function_index, 'body')
	alloca_id := m.add_instr(.alloca, entry, pointer_type, [])
	m.add_instr(.jmp, entry, ssa.TypeID(0), [ssa.ValueID(body)])
	load_id := m.add_instr(.load, body, i32_type, [alloca_id])
	m.add_instr(.ret, body, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, m, i32_type)],
		[memory_agg_test_local(profile, m, function_index, alloca_id, i32_type, .one,
			ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, m, function_index, load_id)])
	plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
	assert plan.accesses.len == 1
	assert plan.accesses[0].root_slot_id == u32(alloca_id)
}

fn test_memory_agg_m120_nondominance_cycle_depth_and_unreachable_refuse() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut nondom := ssa.Module.new()
	i1_type := memory_agg_test_int_type(mut nondom, 1, false)
	i32_type := memory_agg_test_int_type(mut nondom, 32, false)
	pointer_type := memory_agg_test_pointer_type(mut nondom, i32_type)
	function_index := nondom.new_function('nondominating_local', ssa.TypeID(0))
	entry := nondom.add_block(function_index, 'entry')
	left := nondom.add_block(function_index, 'left')
	right := nondom.add_block(function_index, 'right')
	condition := nondom.add_value(.constant, i1_type, 'branch-condition', 0)
	nondom.add_instr(.br, entry, ssa.TypeID(0),
		[condition, ssa.ValueID(left), ssa.ValueID(right)])
	alloca_id := nondom.add_instr(.alloca, left, pointer_type, [])
	nondom.add_instr(.ret, left, ssa.TypeID(0), [])
	load_id := nondom.add_instr(.load, right, i32_type, [alloca_id])
	nondom.add_instr(.ret, right, ssa.TypeID(0), [])
	nondom_facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, nondom, i32_type)],
		[memory_agg_test_local(profile, nondom, function_index, alloca_id, i32_type,
			.one, ssa.ValueID(0))], [],
		[memory_agg_test_access(profile, nondom, function_index, load_id)])
	memory_agg_test_expect_error(nondom, &nondom_facts, 'does not dominate')

	mut cycle := ssa.Module.new()
	cycle_i8 := memory_agg_test_int_type(mut cycle, 8, false)
	cycle_pointer := memory_agg_test_pointer_type(mut cycle, cycle_i8)
	cycle_function := cycle.new_function('provenance_cycle', ssa.TypeID(0))
	cycle_argument := cycle.add_value(.argument, cycle_pointer, 'unknown-root', 0)
	cycle.func_add_param(cycle_function, cycle_argument)
	cycle_block := cycle.add_block(cycle_function, 'entry')
	first := cycle.add_instr(.bitcast, cycle_block, cycle_pointer, [cycle_argument])
	second := cycle.add_instr(.bitcast, cycle_block, cycle_pointer, [first])
	first_value := cycle.values[int(first)]
	mut first_instruction := cycle.instrs[first_value.index]
	first_instruction.operands = [second]
	cycle.instrs[first_value.index] = first_instruction
	cycle.add_instr(.ret, cycle_block, ssa.TypeID(0), [])
	cycle_facts := memory_agg_test_facts(profile, cycle_function, [], [], [], [])
	memory_agg_test_expect_error(cycle, &cycle_facts, 'provenance cycle')

	mut deep := ssa.Module.new()
	deep_i8 := memory_agg_test_int_type(mut deep, 8, false)
	deep_pointer := memory_agg_test_pointer_type(mut deep, deep_i8)
	deep_function := deep.new_function('deep_provenance', ssa.TypeID(0))
	deep_block := deep.add_block(deep_function, 'entry')
	deep_alloca := deep.add_instr(.alloca, deep_block, deep_pointer, [])
	mut pointer_ids := [deep_alloca]
	mut source := deep_alloca
	for _ in 0 .. memory_agg_max_provenance_depth + 1 {
		source = deep.add_instr(.bitcast, deep_block, deep_pointer, [source])
		pointer_ids << source
	}
	deep_ret := deep.add_instr(.ret, deep_block, ssa.TypeID(0), [])
	mut reversed_ids := []ssa.ValueID{cap: pointer_ids.len + 1}
	for offset in 0 .. pointer_ids.len {
		reversed_ids << pointer_ids[pointer_ids.len - 1 - offset]
	}
	reversed_ids << deep_ret
	mut deep_block_value := deep.blocks[int(deep_block)]
	deep_block_value.instrs = reversed_ids
	deep.blocks[int(deep_block)] = deep_block_value
	deep_facts := memory_agg_test_facts(profile, deep_function,
		[memory_agg_test_layout(profile, deep, deep_i8)],
		[memory_agg_test_local(profile, deep, deep_function, deep_alloca, deep_i8, .one,
			ssa.ValueID(0))], [], [])
	memory_agg_test_expect_error(deep, &deep_facts, 'provenance depth exceeds')

	mut unreachable := ssa.Module.new()
	unreachable_function := unreachable.new_function('unreachable_block', ssa.TypeID(0))
	first_block := unreachable.add_block(unreachable_function, 'entry')
	second_block := unreachable.add_block(unreachable_function, 'dead')
	unreachable.add_instr(.ret, first_block, ssa.TypeID(0), [])
	unreachable.add_instr(.ret, second_block, ssa.TypeID(0), [])
	unreachable_facts := memory_agg_test_facts(profile, unreachable_function, [], [], [],
		[])
	memory_agg_test_expect_error(unreachable, &unreachable_facts, 'is unreachable')
}

fn test_memory_agg_m121_pointer_escape_and_unknown_alias_matrix_refuses() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	for opcode in [ssa.OpCode.assign, .phi, .select] {
		mut m := ssa.Module.new()
		i1_type := memory_agg_test_int_type(mut m, 1, false)
		i8_type := memory_agg_test_int_type(mut m, 8, false)
		pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
		function_index := m.new_function('pointer_${opcode}', ssa.TypeID(0))
		block := m.add_block(function_index, 'entry')
		alloca_id := m.add_instr(.alloca, block, pointer_type, [])
		mut operands := [alloca_id]
		if opcode == .phi {
			operands = [alloca_id, ssa.ValueID(block)]
		} else if opcode == .select {
			condition := m.add_value(.constant, i1_type, 'condition', 0)
			operands = [condition, alloca_id, alloca_id]
		}
		m.add_instr(opcode, block, pointer_type, operands)
		m.add_instr(.ret, block, ssa.TypeID(0), [])
		facts := memory_agg_test_facts(profile, function_index, [], [], [], [])
		memory_agg_test_expect_error(m, &facts, 'outside static provenance')
	}

	mut returned := ssa.Module.new()
	return_i8 := memory_agg_test_int_type(mut returned, 8, false)
	return_pointer := memory_agg_test_pointer_type(mut returned, return_i8)
	return_function := returned.new_function('return_escape', return_pointer)
	return_block := returned.add_block(return_function, 'entry')
	return_alloca := returned.add_instr(.alloca, return_block, return_pointer, [])
	returned.add_instr(.ret, return_block, ssa.TypeID(0), [return_alloca])
	return_facts := memory_agg_test_facts(profile, return_function, [], [], [], [])
	memory_agg_test_expect_error(returned, &return_facts, 'pointer escape through ret')

	mut called := ssa.Module.new()
	call_i8 := memory_agg_test_int_type(mut called, 8, false)
	call_pointer := memory_agg_test_pointer_type(mut called, call_i8)
	call_function := called.new_function('call_escape', ssa.TypeID(0))
	call_block := called.add_block(call_function, 'entry')
	call_alloca := called.add_instr(.alloca, call_block, call_pointer, [])
	function_ref := called.add_value(.func_ref, ssa.TypeID(0), 'sink', 9)
	called.add_instr(.call, call_block, ssa.TypeID(0), [function_ref, call_alloca])
	called.add_instr(.ret, call_block, ssa.TypeID(0), [])
	call_facts := memory_agg_test_facts(profile, call_function, [], [], [], [])
	memory_agg_test_expect_error(called, &call_facts, 'pointer escape through call')

	mut stored := ssa.Module.new()
	store_i8 := memory_agg_test_int_type(mut stored, 8, false)
	store_pointer := memory_agg_test_pointer_type(mut stored, store_i8)
	store_function := stored.new_function('store_pointer_escape', ssa.TypeID(0))
	store_block := stored.add_block(store_function, 'entry')
	source_pointer := stored.add_instr(.alloca, store_block, store_pointer, [])
	destination_pointer := stored.add_instr(.alloca, store_block, store_pointer, [])
	stored.add_instr(.store, store_block, ssa.TypeID(0),
		[source_pointer, destination_pointer])
	stored.add_instr(.ret, store_block, ssa.TypeID(0), [])
	store_facts := memory_agg_test_facts(profile, store_function, [], [], [], [])
	memory_agg_test_expect_error(stored, &store_facts, 'requires scalar integer type')

	mut unknown := ssa.Module.new()
	unknown_i32 := memory_agg_test_int_type(mut unknown, 32, false)
	unknown_pointer := memory_agg_test_pointer_type(mut unknown, unknown_i32)
	unknown_function := unknown.new_function('unknown_alias', ssa.TypeID(0))
	argument := unknown.add_value(.argument, unknown_pointer, 'external-pointer', 0)
	unknown.func_add_param(unknown_function, argument)
	unknown_block := unknown.add_block(unknown_function, 'entry')
	unknown_load := unknown.add_instr(.load, unknown_block, unknown_i32, [argument])
	unknown.add_instr(.ret, unknown_block, ssa.TypeID(0), [])
	unknown_facts := memory_agg_test_facts(profile, unknown_function,
		[memory_agg_test_layout(profile, unknown, unknown_i32)], [], [],
		[memory_agg_test_access(profile, unknown, unknown_function, unknown_load)])
	memory_agg_test_expect_error(unknown, &unknown_facts, 'unknown non-local provenance')
}

fn test_memory_agg_m122_aggregate_dynamic_and_runtime_memory_forms_refuse() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut aggregate := ssa.Module.new()
	aggregate_i8 := memory_agg_test_int_type(mut aggregate, 8, false)
	mut aggregate_store := aggregate.type_store
	aggregate_type := aggregate_store.get_tuple([aggregate_i8, aggregate_i8])
	aggregate.type_store = aggregate_store
	aggregate_pointer := memory_agg_test_pointer_type(mut aggregate, aggregate_type)
	aggregate_function := aggregate.new_function('aggregate_alloca', ssa.TypeID(0))
	aggregate_block := aggregate.add_block(aggregate_function, 'entry')
	aggregate.add_instr(.alloca, aggregate_block, aggregate_pointer, [])
	aggregate.add_instr(.ret, aggregate_block, ssa.TypeID(0), [])
	aggregate_facts := memory_agg_test_facts(profile, aggregate_function, [], [], [], [])
	memory_agg_test_expect_error(aggregate, &aggregate_facts, 'requires scalar integer type')

	mut runtime := ssa.Module.new()
	runtime_i8 := memory_agg_test_int_type(mut runtime, 8, false)
	runtime_i64 := memory_agg_test_int_type(mut runtime, 64, false)
	runtime_pointer := memory_agg_test_pointer_type(mut runtime, runtime_i8)
	runtime_function := runtime.new_function('runtime_gep', ssa.TypeID(0))
	delta_argument := runtime.add_value(.argument, runtime_i64, 'runtime-delta', 0)
	runtime.func_add_param(runtime_function, delta_argument)
	runtime_block := runtime.add_block(runtime_function, 'entry')
	runtime_alloca := runtime.add_instr(.alloca, runtime_block, runtime_pointer, [])
	runtime.add_instr(.get_element_ptr, runtime_block, runtime_pointer,
		[runtime_alloca, delta_argument])
	runtime.add_instr(.ret, runtime_block, ssa.TypeID(0), [])
	runtime_facts := memory_agg_test_facts(profile, runtime_function,
		memory_agg_test_layouts(profile, runtime, [runtime_i8, runtime_i64]),
		[memory_agg_test_local(profile, runtime, runtime_function, runtime_alloca,
			runtime_i8, .one, ssa.ValueID(0))], [], [])
	memory_agg_test_expect_error(runtime, &runtime_facts, 'byte delta is not constant')

	mut heap := ssa.Module.new()
	heap_i8 := memory_agg_test_int_type(mut heap, 8, false)
	heap_pointer := memory_agg_test_pointer_type(mut heap, heap_i8)
	heap_function := heap.new_function('heap_memory', ssa.TypeID(0))
	heap_block := heap.add_block(heap_function, 'entry')
	heap.add_instr(.heap_alloc, heap_block, heap_pointer, [])
	heap.add_instr(.ret, heap_block, ssa.TypeID(0), [])
	heap_facts := memory_agg_test_facts(profile, heap_function, [], [], [], [])
	memory_agg_test_expect_error(heap, &heap_facts, 'runtime or atomic memory opcode')

	mut construction := ssa.Module.new()
	construction_i8 := memory_agg_test_int_type(mut construction, 8, false)
	mut construction_store := construction.type_store
	construction_type := construction_store.get_tuple([construction_i8])
	construction.type_store = construction_store
	construction_function := construction.new_function('aggregate_construction',
		ssa.TypeID(0))
	construction_block := construction.add_block(construction_function, 'entry')
	construction.add_instr(.struct_init, construction_block, construction_type, [])
	construction.add_instr(.ret, construction_block, ssa.TypeID(0), [])
	construction_facts := memory_agg_test_facts(profile, construction_function, [], [], [],
		[])
	memory_agg_test_expect_error(construction, &construction_facts, 'requires M1b')

	mut aggregate_assign := ssa.Module.new()
	assign_i8 := memory_agg_test_int_type(mut aggregate_assign, 8, false)
	mut assign_store := aggregate_assign.type_store
	assign_type := assign_store.get_tuple([assign_i8])
	aggregate_assign.type_store = assign_store
	assign_function := aggregate_assign.new_function('aggregate_assign', ssa.TypeID(0))
	assign_block := aggregate_assign.add_block(assign_function, 'entry')
	assign_source := aggregate_assign.add_value(.constant, assign_type, 'aggregate-source',
		0)
	aggregate_assign.add_instr(.assign, assign_block, assign_type, [assign_source])
	aggregate_assign.add_instr(.ret, assign_block, ssa.TypeID(0), [])
	assign_facts := memory_agg_test_facts(profile, assign_function, [], [], [], [])
	memory_agg_test_expect_error(aggregate_assign, &assign_facts, 'outside scalar M1a')
}

fn test_memory_agg_m123_transactional_deterministic_and_deep_input_isolation() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut first := memory_agg_test_load_fixture(profile, 16, 4, true, 2)
	mut second := memory_agg_test_load_fixture(profile, 16, 4, true, 2)
	mut first_layouts := memory_agg_test_layouts(profile, first.m,
		[first.element_type, first.i64_type])
	mut first_locals := [
		memory_agg_test_local(profile, first.m, first.function_index, first.alloca_id,
			first.element_type, .constant_count, first.count_id),
	]
	mut first_constants := [
		memory_agg_test_constant(first.count_id, first.i64_type, 4),
		memory_agg_test_constant(first.delta_id, first.i64_type, 2),
	]
	mut first_accesses := [
		memory_agg_test_access(profile, first.m, first.function_index, first.load_id),
	]
	first_facts := memory_agg_test_facts(profile, first.function_index, first_layouts,
		first_locals, first_constants, first_accesses)
	second_facts := memory_agg_test_load_facts(&second, 4, 2)
	first_plan := plan_scalar_static_memory(first.m, &first_facts) or { panic(err) }
	second_plan := plan_scalar_static_memory(second.m, &second_facts) or { panic(err) }
	assert first_plan == second_plan
	first_snapshot := first_plan
	second_snapshot := second_plan

	first_layouts[0] = MemoryAggScalarLayoutBinding{}
	first_locals[0] = MemoryAggStaticLocalBinding{}
	first_constants[0] = ScalarConstantBinding{}
	first_accesses[0] = MemoryAggAccessBinding{}
	mut first_value := first.m.values[int(first.alloca_id)]
	first_value.uses = [ssa.ValueID(9876)]
	first.m.values[int(first.alloca_id)] = first_value
	_ = memory_agg_test_int_type(mut second.m, 32, true)
	assert first_plan == first_snapshot
	assert second_plan == second_snapshot
	assert first_plan == second_plan

	values_before := first.m.values.clone()
	bad_facts := memory_agg_test_facts(profile, first.function_index, [], [], [], [])
	memory_agg_test_expect_error(first.m, &bad_facts, 'incomplete_fact')
	assert first.m.values == values_before
}

fn test_memory_agg_m124_frozen_numeric_caps_raw_precedence_and_size_endpoints() {
	assert memory_agg_max_scalar_layouts == 64
	assert memory_agg_max_static_locals == 1024
	assert memory_agg_max_scalar_constants == 4096
	assert memory_agg_max_accesses == 4096
	assert memory_agg_max_blocks == 4096
	assert memory_agg_max_active_instructions == 65536
	assert memory_agg_max_pointer_definitions == 8192
	assert memory_agg_max_use_edges == 262144
	assert memory_agg_max_module_values == 131072
	assert memory_agg_max_module_functions == 4096
	assert memory_agg_max_module_instructions == 65536
	assert memory_agg_max_provenance_depth == 64
	assert memory_agg_max_requested_bytes == u64(0x7ffffff8)
	memory_agg_test_count_cap('scalar layout count', memory_agg_max_scalar_layouts)
	memory_agg_test_count_cap('static local count', memory_agg_max_static_locals)
	memory_agg_test_count_cap('slot count', memory_agg_max_static_locals)
	memory_agg_test_count_cap('scalar constant count', memory_agg_max_scalar_constants)
	memory_agg_test_count_cap('access count', memory_agg_max_accesses)
	memory_agg_test_count_cap('active block count', memory_agg_max_blocks)
	memory_agg_test_count_cap('active instruction count',
		memory_agg_max_active_instructions)
	memory_agg_test_count_cap('pointer definition count',
		memory_agg_max_pointer_definitions)
	memory_agg_test_count_cap('use edge count', memory_agg_max_use_edges)
	memory_agg_test_count_cap('module value count', memory_agg_max_module_values)
	memory_agg_test_count_cap('module function count', memory_agg_max_module_functions)
	memory_agg_test_count_cap('module instruction count',
		memory_agg_max_module_instructions)
	memory_agg_test_count_cap('pointer provenance depth',
		memory_agg_max_provenance_depth)

	exact_input_caps := MemoryAggFunctionFacts{
		scalar_layouts:   []MemoryAggScalarLayoutBinding{len: memory_agg_max_scalar_layouts}
		static_locals:    []MemoryAggStaticLocalBinding{len: memory_agg_max_static_locals}
		scalar_constants: []ScalarConstantBinding{len: memory_agg_max_scalar_constants}
		accesses:         []MemoryAggAccessBinding{len: memory_agg_max_accesses}
	}
	memory_agg_validate_input_caps(&exact_input_caps) or { panic(err) }
	mut exact_value_module := ssa.Module.new()
	exact_value_module.values = []ssa.Value{len: memory_agg_max_module_values}
	memory_agg_validate_module_preallocation(exact_value_module) or { panic(err) }
	mut exceeded_value_module := ssa.Module.new()
	exceeded_value_module.values = []ssa.Value{len: memory_agg_max_module_values + 1}
	if _ := memory_agg_validate_module_preallocation(exceeded_value_module) {
		assert false, 'expected module value cap+1 to fail'
	} else {
		assert err.msg().contains('module value count')
	}

	exact := memory_agg_test_alloca_fixture(.linux_x86_64_sysv_elf, 8, false, true,
		0x7ffffff8)
	exact_facts := memory_agg_test_alloca_facts(&exact, 0x7ffffff8)
	exact_plan := plan_scalar_static_memory(exact.m, &exact_facts) or { panic(err) }
	assert exact_plan.total_requested_bytes == u64(0x7ffffff8)

	exceeded := memory_agg_test_alloca_fixture(.linux_x86_64_sysv_elf, 8, false,
		true, 0x7ffffff9)
	exceeded_facts := memory_agg_test_alloca_facts(&exceeded, 0x7ffffff9)
	memory_agg_test_expect_error(exceeded.m, &exceeded_facts, 'total slot bytes')

	empty, function_index := memory_agg_test_empty_module()
	bad_profile := unsafe { TargetProfile(255) }
	dummy_layout := MemoryAggScalarLayoutBinding{
		profile:   bad_profile
		authority: .native_plain
	}
	too_many_layouts := []MemoryAggScalarLayoutBinding{
		len:  memory_agg_max_scalar_layouts + 1,
		init: dummy_layout
	}
	raw_precedence := memory_agg_test_facts(bad_profile, function_index, too_many_layouts,
		[], [], [])
	memory_agg_test_expect_error(empty, &raw_precedence, 'unsupported target profile')

	too_many_constants := []ScalarConstantBinding{
		len: memory_agg_max_scalar_constants + 1
	}
	constant_cap := memory_agg_test_facts(.linux_x86_64_sysv_elf, function_index, [], [],
		too_many_constants, [])
	memory_agg_test_expect_error(empty, &constant_cap, 'scalar constant count')

	dummy_access := MemoryAggAccessBinding{
		profile:   .linux_x86_64_sysv_elf
		semantics: .nonvolatile
		kind:      .load
	}
	too_many_accesses := []MemoryAggAccessBinding{
		len:  memory_agg_max_accesses + 1,
		init: dummy_access
	}
	access_cap := memory_agg_test_facts(.linux_x86_64_sysv_elf, function_index, [], [],
		[], too_many_accesses)
	memory_agg_test_expect_error(empty, &access_cap, 'access count')
}

fn test_memory_agg_canary_cached_absolute_provenance_depth_64_65() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	for chain_length in [memory_agg_max_provenance_depth,
		memory_agg_max_provenance_depth + 1] {
		mut m := ssa.Module.new()
		i8_type := memory_agg_test_int_type(mut m, 8, false)
		pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
		function_index := m.new_function('cached_depth_${chain_length}', ssa.TypeID(0))
		block := m.add_block(function_index, 'entry')
		alloca_id := m.add_instr(.alloca, block, pointer_type, [])
		mut pointer_id := alloca_id
		for _ in 0 .. chain_length {
			pointer_id = m.add_instr(.bitcast, block, pointer_type, [pointer_id])
		}
		m.add_instr(.ret, block, ssa.TypeID(0), [])
		facts := memory_agg_test_facts(profile, function_index,
			[memory_agg_test_layout(profile, m, i8_type)],
			[memory_agg_test_local(profile, m, function_index, alloca_id, i8_type, .one,
				ssa.ValueID(0))], [], [])
		if chain_length == memory_agg_max_provenance_depth {
			plan := plan_scalar_static_memory(m, &facts) or { panic(err) }
			assert plan.pointers.len == chain_length + 1
			assert plan.pointers.last().value_id == pointer_id
		} else {
			memory_agg_test_expect_error(m, &facts, 'provenance depth exceeds')
		}
	}
}

fn test_memory_agg_canary_module_wide_cross_function_pointer_escape_refuses() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	pointer_type := memory_agg_test_pointer_type(mut m, i8_type)
	function_index := m.new_function('pointer_owner', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block, pointer_type, [])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	foreign_function := m.new_function('foreign_pointer_user', ssa.TypeID(0))
	foreign_block := m.add_block(foreign_function, 'entry')
	m.add_instr(.bitcast, foreign_block, pointer_type, [alloca_id])
	m.add_instr(.ret, foreign_block, ssa.TypeID(0), [])
	facts := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, m, i8_type)],
		[memory_agg_test_local(profile, m, function_index, alloca_id, i8_type, .one,
			ssa.ValueID(0))], [], [])
	memory_agg_test_expect_error(m, &facts, 'cross-function reference')
}

fn test_memory_agg_canary_canonical_void_int_pointer_payloads_and_metadata() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut bad_void, bad_void_function := memory_agg_test_empty_module()
	bad_void.type_store.types[0] = ssa.Type{
		kind:  .void_t
		width: 8
	}
	bad_void_facts := memory_agg_test_facts(profile, bad_void_function, [], [], [], [])
	memory_agg_test_expect_error(bad_void, &bad_void_facts, 'noncanonical void payload')

	mut bad_int := memory_agg_test_alloca_fixture(profile, 32, false, false, 0)
	bad_int_facts := memory_agg_test_alloca_facts(&bad_int, 0)
	bad_int.m.type_store.types[int(bad_int.element_type)] = ssa.Type{
		kind:      .int_t
		width:     32
		elem_type: bad_int.element_type
	}
	memory_agg_test_expect_error(bad_int.m, &bad_int_facts,
		'noncanonical integer payload')

	mut bad_metadata := memory_agg_test_alloca_fixture(profile, 16, false, false, 0)
	bad_metadata_facts := memory_agg_test_alloca_facts(&bad_metadata, 0)
	bad_metadata.m.type_store.types[int(bad_metadata.element_type)] = ssa.Type{
		kind:        .int_t
		width:       16
		is_c_struct: true
		is_union:    true
	}
	memory_agg_test_expect_error(bad_metadata.m, &bad_metadata_facts,
		'contradictory C struct/union metadata')

	mut bad_pointer := memory_agg_test_alloca_fixture(profile, 8, false, false, 0)
	bad_pointer_facts := memory_agg_test_alloca_facts(&bad_pointer, 0)
	pointer_type := bad_pointer.m.values[int(bad_pointer.alloca_id)].typ
	bad_pointer.m.type_store.types[int(pointer_type)] = ssa.Type{
		kind:      .ptr_t
		width:     64
		elem_type: bad_pointer.element_type
	}
	memory_agg_test_expect_error(bad_pointer.m, &bad_pointer_facts,
		'noncanonical pointer payload')
}

fn test_memory_agg_canary_unused_bitcast_requires_authoritative_pointee_layout() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut m := ssa.Module.new()
	i8_type := memory_agg_test_int_type(mut m, 8, false)
	i32_type := memory_agg_test_int_type(mut m, 32, false)
	i8_pointer := memory_agg_test_pointer_type(mut m, i8_type)
	i32_pointer := memory_agg_test_pointer_type(mut m, i32_type)
	function_index := m.new_function('unused_bitcast_layout', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	alloca_id := m.add_instr(.alloca, block, i8_pointer, [])
	bitcast_id := m.add_instr(.bitcast, block, i32_pointer, [alloca_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	local := memory_agg_test_local(profile, m, function_index, alloca_id, i8_type,
		.one, ssa.ValueID(0))
	missing := memory_agg_test_facts(profile, function_index,
		[memory_agg_test_layout(profile, m, i8_type)], [local], [], [])
	memory_agg_test_expect_error(m, &missing, 'pointer ${bitcast_id} pointee is missing scalar layout')
	complete := memory_agg_test_facts(profile, function_index,
		memory_agg_test_layouts(profile, m, [i8_type, i32_type]), [local], [], [])
	plan := plan_scalar_static_memory(m, &complete) or { panic(err) }
	assert plan.pointers.len == 2
	assert plan.pointers[1].value_id == bitcast_id
	assert plan.pointers[1].pointee_type == i32_type
}

fn test_memory_agg_canary_module_function_table_cap_exact_and_first_exceed() {
	mut m := ssa.Module.new()
	m.funcs = []ssa.Function{len: memory_agg_max_module_functions}
	memory_agg_validate_module_preallocation(m) or { panic(err) }
	m.funcs << ssa.Function{}
	if _ := memory_agg_validate_module_preallocation(m) {
		assert false, 'expected module function table limit+1 to fail'
	} else {
		assert err.msg().contains('module function count ${memory_agg_max_module_functions + 1} exceeds ${memory_agg_max_module_functions}')
	}
}

fn test_memory_agg_canary_module_instruction_table_cap_exact_and_first_exceed() {
	mut m := ssa.Module.new()
	m.instrs = []ssa.Instruction{len: memory_agg_max_module_instructions}
	memory_agg_validate_module_preallocation(m) or { panic(err) }
	m.instrs << ssa.Instruction{}
	if _ := memory_agg_validate_module_preallocation(m) {
		assert false, 'expected module instruction table limit+1 to fail'
	} else {
		assert err.msg().contains('module instruction count ${memory_agg_max_module_instructions + 1} exceeds ${memory_agg_max_module_instructions}')
	}
}

fn test_memory_agg_m201_current_builder_form_is_exhaustively_attested() {
	fixture, facts := memory_agg_test_builder_aggregate_fixture(.linux_x86_64_sysv_elf)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.aggregate_layouts.len == 1
	assert plan.aggregate_slots.len == 2
	assert plan.aggregate_slots[0].request.id == u32(0x80000000)
	assert plan.aggregate_slots[0].request.kind == .aggregate_temp
	assert plan.aggregate_slots[1].request.id == u32(0x80000001)
	assert plan.aggregate_snapshots.len == 1
	assert plan.aggregate_snapshots[0].value_id == fixture.load_id
	assert plan.pointers.len == 4
	assert plan.pointers[0].origin == .aggregate_storage
	assert plan.pointers[1].origin == .aggregate_field
	assert plan.accesses.len == 4
	assert plan.accesses.last().kind == .load
	assert plan.accesses.last().scalar_value_id == fixture.extract_id
	assert plan.aggregate_actions.len == 6
	for action in plan.aggregate_actions {
		assert action.kind == .copy
		assert action.phase == 2
		assert action.width_bytes == 4
	}
}

fn test_memory_agg_m202_nominal_construct_load_store_extract_insert_phases() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.windows_x86_64_microsoft_abi_coff, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.aggregate_slots.len == 4
	assert plan.aggregate_slots[0].request.id == u32(fixture.alloca_id)
	assert plan.aggregate_slots[0].request.kind == .fixed_alloca
	assert plan.aggregate_slots[1].request.id == u32(0x80000000)
	assert plan.aggregate_slots[2].request.id == u32(0x80000001)
	assert plan.aggregate_slots[3].request.id == u32(0x80000002)
	assert plan.aggregate_snapshots.len == 3
	assert plan.total_requested_bytes == 48
	assert plan.aggregate_actions.len == 20
	mut construct_phases := []u8{}
	mut insert_phases := []u8{}
	for action in plan.aggregate_actions {
		if action.anchor.instruction_value_id == fixture.construct_id {
			construct_phases << action.phase
		}
		if action.anchor.instruction_value_id == fixture.insert_id {
			insert_phases << action.phase
		}
	}
	assert construct_phases == [u8(1), 1, 1, 3, 3, 3]
	assert insert_phases == [u8(2), 2, 2, 3]
}

fn test_memory_agg_m203_profile_keyed_flat_layout_and_padding_partition() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		fixture, facts := memory_agg_test_nominal_aggregate_fixture(profile, .zero)
		plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
		layout := plan.aggregate_layouts[0]
		assert layout.profile == profile
		assert layout.size_bytes == 12
		assert layout.alignment_bytes == 4
		assert layout.fields.map(it.offset_bytes) == [u64(0), 4, 8]
		assert layout.fields.map(it.size_bytes) == [u64(1), 4, 2]
		assert layout.padding == [
			MemoryAggByteRange{
				offset_bytes: 1
				size_bytes:   3
			},
			MemoryAggByteRange{
				offset_bytes: 10
				size_bytes:   2
			},
		]
	}
}

fn test_memory_agg_m204_layout_missing_duplicate_stale_and_orphan_refuse() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	missing := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: []MemoryAggAggregateLayoutBinding{}
	}
	memory_agg_test_expect_error(fixture.m, &missing, 'incomplete_fact')

	layout := facts.aggregate_layouts[0]
	duplicate := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [layout, layout]
	}
	memory_agg_test_expect_error(fixture.m, &duplicate, 'duplicate_fact')

	stale_layout := MemoryAggAggregateLayoutBinding{
		...layout
		profile: .macos_x86_64_sysv_macho
	}
	stale := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [stale_layout]
	}
	memory_agg_test_expect_error(fixture.m, &stale, 'wrong profile')

	mut extra_m := fixture.m
	extra_type := memory_agg_test_struct_type(mut extra_m, [fixture.i8_type])
	orphan_layout := memory_agg_test_aggregate_layout(fixture.profile, extra_m, extra_type)
	orphan := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [layout, orphan_layout]
	}
	memory_agg_test_expect_error(extra_m, &orphan, 'orphan_fact')
}

fn test_memory_agg_m205_layout_size_alignment_field_and_padding_formulas() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	layout := facts.aggregate_layouts[0]
	for bad_layout in [
		MemoryAggAggregateLayoutBinding{
			...layout
			size_bytes: 0
		},
		MemoryAggAggregateLayoutBinding{
			...layout
			alignment_bytes: 3
		},
		MemoryAggAggregateLayoutBinding{
			...layout
			size_bytes: 10
		},
	] {
		bad := MemoryAggFunctionFacts{
			...facts
			aggregate_layouts: [bad_layout]
		}
		memory_agg_test_expect_error(fixture.m, &bad, 'aggregate layout')
	}
	mut overlapping_fields := layout.fields.clone()
	overlapping_fields[1] = MemoryAggAggregateFieldLayout{
		...overlapping_fields[1]
		offset_bytes: 0
	}
	overlap := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				...layout
				fields: overlapping_fields
			},
		]
	}
	memory_agg_test_expect_error(fixture.m, &overlap, 'overlap')

	wrong_padding := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				...layout
				padding: [
					MemoryAggByteRange{
						offset_bytes: 1
						size_bytes:   2
					},
				]
			},
		]
	}
	memory_agg_test_expect_error(fixture.m, &wrong_padding, 'maximal complement')
}

fn test_memory_agg_m206_nonplain_nested_pointer_c_union_and_empty_refuse() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	mut c_fixture, c_facts := memory_agg_test_nominal_aggregate_fixture(profile, .zero)
	c_type := c_fixture.m.type_store.types[int(c_fixture.aggregate_type)]
	c_fixture.m.type_store.types[int(c_fixture.aggregate_type)] = ssa.Type{
		...c_type
		is_c_struct: true
	}
	memory_agg_test_expect_error(c_fixture.m, &c_facts, 'C or union storage')

	mut union_fixture, union_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	union_type := union_fixture.m.type_store.types[int(union_fixture.aggregate_type)]
	union_fixture.m.type_store.types[int(union_fixture.aggregate_type)] = ssa.Type{
		...union_type
		is_union: true
	}
	memory_agg_test_expect_error(union_fixture.m, &union_facts, 'C or union storage')

	mut pointer_fixture, mut pointer_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	pointer_field := memory_agg_test_pointer_type(mut pointer_fixture.m,
		pointer_fixture.i8_type)
	pointer_type := pointer_fixture.m.type_store.types[int(pointer_fixture.aggregate_type)]
	mut pointer_fields := pointer_type.fields.clone()
	pointer_fields[0] = pointer_field
	pointer_fixture.m.type_store.types[int(pointer_fixture.aggregate_type)] = ssa.Type{
		...pointer_type
		fields: pointer_fields
	}
	mut pointer_layout_fields := pointer_facts.aggregate_layouts[0].fields.clone()
	pointer_layout_fields[0] = MemoryAggAggregateFieldLayout{
		...pointer_layout_fields[0]
		type_id: pointer_field
	}
	pointer_facts = MemoryAggFunctionFacts{
		...pointer_facts
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				...pointer_facts.aggregate_layouts[0]
				fields: pointer_layout_fields
			},
		]
	}
	memory_agg_test_expect_error(pointer_fixture.m, &pointer_facts,
		'not a scalar integer')
}

fn test_memory_agg_m207_shared_fixed_and_temp_ids_are_injective_and_stable() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	assert plan.aggregate_slots.map(it.request.id) == [
		u32(fixture.alloca_id),
		u32(0x80000000),
		u32(0x80000001),
		u32(0x80000002),
	]
	anchor := memory_agg_test_anchor(fixture.m, fixture.function_index,
		fixture.construct_id)
	candidates := [
		MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: 30
			role:           .aggregate_temp
			purpose:        .load_result
		},
		MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: 10
			role:           .aggregate_temp
			purpose:        .construct_result
		},
		MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: 20
			role:           .aggregate_temp
			purpose:        .load_result
		},
	]
	ids, ordinals := memory_agg_assign_temp_ids(candidates) or { panic(err) }
	assert ids[10] == u32(0x80000000)
	assert ids[20] == u32(0x80000001)
	assert ids[30] == u32(0x80000002)
	assert ordinals[10] == 0
	assert ordinals[20] == 1
	assert ordinals[30] == 2
}

fn test_memory_agg_m208_duplicate_slot_owner_precedes_layout_and_id_arithmetic() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	bad_layout := MemoryAggAggregateLayoutBinding{
		...facts.aggregate_layouts[0]
		size_bytes: 0
	}
	duplicate := MemoryAggFunctionFacts{
		...facts
		aggregate_layouts: [bad_layout]
		aggregate_allocas: [facts.aggregate_allocas[0], facts.aggregate_allocas[0]]
	}
	memory_agg_test_expect_error(fixture.m, &duplicate, 'duplicate_fact')

	anchor := memory_agg_test_anchor(fixture.m, fixture.function_index,
		fixture.construct_id)
	duplicate_candidates := [
		MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: 9
			role:           .aggregate_temp
			purpose:        .construct_result
		},
		MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: 9
			role:           .aggregate_temp
			purpose:        .load_result
		},
	]
	if _, _ := memory_agg_assign_temp_ids(duplicate_candidates) {
		assert false
	} else {
		assert err.msg().contains('duplicate_fact')
	}
}

fn test_memory_agg_m209_builder_sidecars_are_exhaustive() {
	fixture, facts := memory_agg_test_builder_aggregate_fixture(
		.linux_x86_64_sysv_elf)
	missing_field := MemoryAggFunctionFacts{
		...facts
		aggregate_field_pointers: facts.aggregate_field_pointers[1..].clone()
	}
	memory_agg_test_expect_error(fixture.m, &missing_field,
		'requires authoritative aggregate field-pointer fact')

	missing_load := MemoryAggFunctionFacts{
		...facts
		aggregate_loads: []MemoryAggAggregateLoadBinding{}
	}
	memory_agg_test_expect_error(fixture.m, &missing_load, 'requires M1b sidecar')

	missing_store := MemoryAggFunctionFacts{
		...facts
		accesses: facts.accesses[1..].clone()
	}
	memory_agg_test_expect_error(fixture.m, &missing_store, 'incomplete_fact')
}

fn test_memory_agg_m210_nominal_sidecars_are_exhaustive_and_exact() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	for missing in [
		MemoryAggFunctionFacts{
			...facts
			aggregate_constructs: []MemoryAggAggregateConstructBinding{}
		},
		MemoryAggFunctionFacts{
			...facts
			aggregate_extracts: []MemoryAggAggregateExtractBinding{}
		},
		MemoryAggFunctionFacts{
			...facts
			aggregate_inserts: []MemoryAggAggregateInsertBinding{}
		},
	] {
		memory_agg_test_expect_error(fixture.m, &missing, 'requires M1b sidecar')
	}
	stale_extract := MemoryAggAggregateExtractBinding{
		...facts.aggregate_extracts[0]
		field_index: 2
	}
	stale := MemoryAggFunctionFacts{
		...facts
		aggregate_extracts: [stale_extract]
	}
	memory_agg_test_expect_error(fixture.m, &stale, 'shape disagrees')
}

fn test_memory_agg_m211_load_creates_an_immediate_immutable_snapshot() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	load_snapshot := plan.aggregate_snapshots.filter(it.value_id == fixture.load_id)[0]
	assert load_snapshot.publish_phase == 4
	assert load_snapshot.root_slot_id == u32(0x80000001)
	assert load_snapshot.root_slot_id != u32(fixture.alloca_id)
	mut load_writes := 0
	mut later_load_slot_writes := 0
	for action in plan.aggregate_actions {
		if action.anchor.instruction_value_id == fixture.load_id
			&& action.destination_slot_id == load_snapshot.root_slot_id {
			load_writes++
			assert action.phase == 2
		}
		if action.anchor.instruction_ordinal
			> load_snapshot.definition.instruction_ordinal
			&& action.destination_slot_id == load_snapshot.root_slot_id {
			later_load_slot_writes++
		}
	}
	assert load_writes == 3
	assert later_load_slot_writes == 0
}

fn test_memory_agg_m212_zero_construct_covers_padding_before_field_writes() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	actions := plan.aggregate_actions.filter(it.anchor.instruction_value_id == fixture.construct_id)
	assert actions.len == 6
	assert actions[..3].map(it.kind) == [
		MemoryAggAggregateActionKind.zero,
		.zero,
		.zero,
	]
	assert actions[..3].map(it.destination_offset_bytes) == [u64(0), 4, 8]
	assert actions[..3].map(it.width_bytes) == [u8(4), 4, 4]
	assert actions[3..].map(it.kind) == [
		MemoryAggAggregateActionKind.scalar_write,
		.scalar_write,
		.scalar_write,
	]
	assert actions[3..].map(it.destination_offset_bytes) == [u64(0), 4, 8]
}

fn test_memory_agg_m213_construct_padding_is_fail_closed_and_zeroed() {
	refused_fixture, refused_facts := memory_agg_test_nominal_aggregate_fixture(
		.macos_x86_64_sysv_macho, .preserve_unwritten)
	memory_agg_test_expect_error(refused_fixture.m, &refused_facts,
		'unsupported padding policy')

	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.macos_x86_64_sysv_macho, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	actions := plan.aggregate_actions.filter(it.anchor.instruction_value_id == fixture.construct_id)
	assert actions.len == 6
	assert actions[..3].all(it.kind == .zero && it.phase == 1)
	assert actions[..3].map(it.destination_offset_bytes) == [u64(0), 4, 8]
	assert actions[..3].map(it.width_bytes) == [u8(4), 4, 4]
	assert actions[3..].all(it.kind == .scalar_write && it.phase == 3)
	assert actions[3..].map(it.destination_offset_bytes) == [u64(0), 4, 8]
	assert plan.aggregate_actions.len == 20
}

fn test_memory_agg_m214_store_copies_complete_object_representation_including_padding() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	actions := plan.aggregate_actions.filter(it.anchor.instruction_value_id == fixture.first_store_id)
	assert actions.len == 3
	assert actions.all(it.kind == .copy)
	assert actions.map(it.source_offset_bytes) == [u64(0), 4, 8]
	assert actions.map(it.destination_offset_bytes) == [u64(0), 4, 8]
	assert actions.map(it.width_bytes) == [u8(4), 4, 4]
}

fn test_memory_agg_m215_extract_preserves_i1_one_byte_semantics() {
	mut fixture, mut facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	aggregate_type := fixture.m.type_store.types[int(fixture.aggregate_type)]
	mut aggregate_fields := aggregate_type.fields.clone()
	aggregate_fields[0] = fixture.i1_type
	fixture.m.type_store.types[int(fixture.aggregate_type)] = ssa.Type{
		...aggregate_type
		fields: aggregate_fields
	}
	mut first_value := fixture.m.values[int(fixture.field_values[0])]
	first_value.typ = fixture.i1_type
	fixture.m.values[int(fixture.field_values[0])] = first_value
	extract_value := fixture.m.values[int(fixture.extract_id)]
	mut extract_instruction := fixture.m.instrs[extract_value.index]
	extract_instruction.typ = fixture.i1_type
	fixture.m.instrs[extract_value.index] = extract_instruction
	mut changed_extract_value := extract_value
	changed_extract_value.typ = fixture.i1_type
	fixture.m.values[int(fixture.extract_id)] = changed_extract_value
	mut constants := facts.scalar_constants.clone()
	constants[0] = memory_agg_test_constant(fixture.field_values[0], fixture.i1_type,
		1)
	mut extracts := facts.aggregate_extracts.clone()
	extracts[0] = MemoryAggAggregateExtractBinding{
		...extracts[0]
		field_index: 0
	}
	facts = MemoryAggFunctionFacts{
		...facts
		scalar_layouts: memory_agg_test_layouts(fixture.profile, fixture.m,
			[fixture.i1_type, fixture.i32_type, fixture.i16_type])
		scalar_constants: constants
		aggregate_layouts: [
			memory_agg_test_aggregate_layout(fixture.profile, fixture.m,
				fixture.aggregate_type),
		]
		aggregate_extracts: extracts
	}
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	action := plan.aggregate_actions.filter(it.anchor.instruction_value_id == fixture.extract_id)[0]
	assert action.kind == .scalar_read
	assert action.width_bytes == 1
	assert action.scalar_type == fixture.i1_type
	assert action.canonicalize_i1
}

fn test_memory_agg_m216_insert_copies_source_before_overwrite_and_keeps_source_immutable() {
	fixture, facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	plan := plan_scalar_static_memory(fixture.m, &facts) or { panic(err) }
	load_snapshot := plan.aggregate_snapshots.filter(it.value_id == fixture.load_id)[0]
	insert_snapshot := plan.aggregate_snapshots.filter(it.value_id == fixture.insert_id)[0]
	assert load_snapshot.root_slot_id != insert_snapshot.root_slot_id
	actions := plan.aggregate_actions.filter(it.anchor.instruction_value_id == fixture.insert_id)
	assert actions.len == 4
	assert actions[..3].all(it.kind == .copy && it.phase == 2)
	assert actions[..3].all(it.source_slot_id == load_snapshot.root_slot_id)
	assert actions[..3].all(it.destination_slot_id == insert_snapshot.root_slot_id)
	assert actions[3].kind == .scalar_write
	assert actions[3].phase == 3
	assert actions[3].destination_offset_bytes == 4
}

fn test_memory_agg_m217_nonoverlap_copy_uses_deterministic_aligned_8_4_2_1_chunks() {
	source := MemoryAggLogicalRegion{
		slot_id:         1
		offset_bytes:    0
		size_bytes:      15
		alignment_bytes: 8
	}
	destination := MemoryAggLogicalRegion{
		slot_id:         2
		offset_bytes:    0
		size_bytes:      15
		alignment_bytes: 8
	}
	count, direction := memory_agg_count_copy_chunks(source, destination) or {
		panic(err)
	}
	assert count == 4
	assert direction == .low_to_high
	mut actions := []MemoryAggAggregateAction{cap: count}
	final_ordinal := memory_agg_append_copy_actions(mut actions,
		MemoryAggInstructionAnchor{}, source, destination, 0) or { panic(err) }
	assert final_ordinal == 4
	assert actions.map(it.width_bytes) == [u8(8), 4, 2, 1]
	assert actions.map(it.source_offset_bytes) == [u64(0), 8, 12, 14]
	assert actions.map(it.destination_offset_bytes) == [u64(0), 8, 12, 14]
}

fn test_memory_agg_m218_overlap_copy_direction_is_memmove_safe() {
	forward_source := MemoryAggLogicalRegion{
		slot_id:         7
		offset_bytes:    8
		size_bytes:      16
		alignment_bytes: 8
	}
	forward_destination := MemoryAggLogicalRegion{
		slot_id:         7
		offset_bytes:    0
		size_bytes:      16
		alignment_bytes: 8
	}
	_, forward := memory_agg_count_copy_chunks(forward_source, forward_destination) or {
		panic(err)
	}
	assert forward == .low_to_high

	backward_source := MemoryAggLogicalRegion{
		slot_id:         7
		offset_bytes:    0
		size_bytes:      16
		alignment_bytes: 8
	}
	backward_destination := MemoryAggLogicalRegion{
		slot_id:         7
		offset_bytes:    8
		size_bytes:      16
		alignment_bytes: 8
	}
	count, backward := memory_agg_count_copy_chunks(backward_source,
		backward_destination) or { panic(err) }
	assert count == 2
	assert backward == .high_to_low
	mut actions := []MemoryAggAggregateAction{cap: count}
	_ = memory_agg_append_copy_actions(mut actions, MemoryAggInstructionAnchor{},
		backward_source, backward_destination, 0) or { panic(err) }
	assert actions.map(it.source_offset_bytes) == [u64(8), 0]
	assert actions.map(it.destination_offset_bytes) == [u64(16), 8]
	assert actions.all(it.direction == .high_to_low)
}

fn test_memory_agg_m219_identical_copy_is_noop_and_checked_half_open_bounds() {
	region := MemoryAggLogicalRegion{
		slot_id:         9
		offset_bytes:    3
		size_bytes:      13
		alignment_bytes: 8
	}
	count, direction := memory_agg_count_copy_chunks(region, region) or { panic(err) }
	assert count == 0
	assert direction == .low_to_high
	mut actions := []MemoryAggAggregateAction{}
	final_ordinal := memory_agg_append_copy_actions(mut actions,
		MemoryAggInstructionAnchor{}, region, region, 7) or { panic(err) }
	assert final_ordinal == 7
	assert actions.len == 0

	overflow := MemoryAggLogicalRegion{
		slot_id:         9
		offset_bytes:    max_u64
		size_bytes:      1
		alignment_bytes: 1
	}
	if _, _ := memory_agg_count_copy_chunks(overflow, overflow) {
		assert false
	} else {
		assert err.msg().contains('arithmetic overflow')
	}
}

fn test_memory_agg_m220_snapshot_dominance_and_escape_are_fail_closed() {
	mut reordered_fixture, mut reordered_facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	mut instructions := reordered_fixture.m.blocks[int(reordered_fixture.block_id)].instrs.clone()
	construct_position := instructions.index(reordered_fixture.construct_id)
	store_position := instructions.index(reordered_fixture.first_store_id)
	instructions[construct_position] = reordered_fixture.first_store_id
	instructions[store_position] = reordered_fixture.construct_id
	reordered_fixture.m.blocks[int(reordered_fixture.block_id)].instrs = instructions
	reordered_facts = memory_agg_test_reanchor_aggregate_facts(reordered_fixture.m,
		reordered_facts)
	memory_agg_test_expect_error(reordered_fixture.m, &reordered_facts,
		'does not dominate')

	mut escape_fixture, escape_facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	ret_value := escape_fixture.m.values[int(escape_fixture.ret_id)]
	mut ret_instruction := escape_fixture.m.instrs[ret_value.index]
	ret_instruction.operands = [escape_fixture.load_id]
	escape_fixture.m.instrs[ret_value.index] = ret_instruction
	memory_agg_test_expect_error(escape_fixture.m, &escape_facts, 'upstream_standby')
}

fn test_memory_agg_m221_volatile_atomic_dynamic_and_unknown_alias_refuse() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	volatile_fixture, volatile_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	volatile_load := MemoryAggAggregateLoadBinding{
		...volatile_facts.aggregate_loads[0]
		semantics: .volatile
	}
	volatile_case := MemoryAggFunctionFacts{
		...volatile_facts
		aggregate_loads: [volatile_load]
	}
	memory_agg_test_expect_error(volatile_fixture.m, &volatile_case, 'not nonvolatile')

	mut atomic_fixture, atomic_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	load_value := atomic_fixture.m.values[int(atomic_fixture.load_id)]
	mut load_instruction := atomic_fixture.m.instrs[load_value.index]
	load_instruction.atomic_ord = .acquire
	atomic_fixture.m.instrs[load_value.index] = load_instruction
	memory_agg_test_expect_error(atomic_fixture.m, &atomic_facts, 'is atomic')

	mut counted_fixture, counted_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	count_id := counted_fixture.m.add_value(.constant, counted_fixture.i64_type,
		'aggregate-count', 0)
	alloca_value := counted_fixture.m.values[int(counted_fixture.alloca_id)]
	mut alloca_instruction := counted_fixture.m.instrs[alloca_value.index]
	alloca_instruction.operands = [count_id]
	counted_fixture.m.instrs[alloca_value.index] = alloca_instruction
	memory_agg_test_expect_error(counted_fixture.m, &counted_facts,
		'count form is outside M1b')

	mut alias_fixture, mut alias_facts := memory_agg_test_nominal_aggregate_fixture(profile,
		.zero)
	argument_id := alias_fixture.m.add_value(.argument, alias_fixture.aggregate_pointer,
		'external-aggregate-pointer', 0)
	alias_fixture.m.func_add_param(alias_fixture.function_index, argument_id)
	alias_load_value := alias_fixture.m.values[int(alias_fixture.load_id)]
	mut alias_load_instruction := alias_fixture.m.instrs[alias_load_value.index]
	alias_load_instruction.operands = [argument_id]
	alias_fixture.m.instrs[alias_load_value.index] = alias_load_instruction
	alias_facts = MemoryAggFunctionFacts{
		...alias_facts
		aggregate_loads: [
			MemoryAggAggregateLoadBinding{
				...alias_facts.aggregate_loads[0]
				pointer_value_id: argument_id
			},
		]
	}
	memory_agg_test_expect_error(alias_fixture.m, &alias_facts,
		'unknown local pointer provenance')
}

fn test_memory_agg_m222_frozen_aggregate_caps_and_temp_rank_endpoints() {
	assert memory_agg_max_aggregate_layouts == 64
	assert memory_agg_max_aggregate_fields_per_layout == 64
	assert memory_agg_max_aggregate_fields == 1024
	assert memory_agg_max_padding_ranges_per_layout == 65
	assert memory_agg_max_padding_ranges == 1088
	assert memory_agg_max_aggregate_object_bytes == u64(65536)
	assert memory_agg_max_aggregate_facts == 4096
	assert memory_agg_max_aggregate_temps == 1024
	assert memory_agg_max_aggregate_actions == 65536
	assert memory_agg_temp_id_base == u64(0x80000000)
	memory_agg_test_count_cap('aggregate layout count', memory_agg_max_aggregate_layouts)
	memory_agg_test_count_cap('aggregate alloca count', memory_agg_max_static_locals)
	memory_agg_test_count_cap('aggregate operation count', memory_agg_max_aggregate_facts)
	memory_agg_test_count_cap('aggregate temp count', memory_agg_max_aggregate_temps)
	memory_agg_test_count_cap('aggregate action count', memory_agg_max_aggregate_actions)
	bad_role := unsafe { MemoryAggAggregateSlotRole(255) }
	bad_role_facts := MemoryAggFunctionFacts{
		profile:   .linux_x86_64_sysv_elf
		ssa_form:  .final_static
		aggregate_allocas: [
			MemoryAggAggregateAllocaBinding{
				profile:   .linux_x86_64_sysv_elf
				authority: .native_plain
				role:      bad_role
			},
		]
	}
	if _ := memory_agg_validate_raw_domains(&bad_role_facts) {
		assert false
	} else {
		assert err.msg().contains('unsupported slot role')
	}
	bad_padding := unsafe { MemoryAggConstructPadding(255) }
	bad_padding_facts := MemoryAggFunctionFacts{
		profile:   .linux_x86_64_sysv_elf
		ssa_form:  .final_static
		aggregate_constructs: [
			MemoryAggAggregateConstructBinding{
				profile:        .linux_x86_64_sysv_elf
				padding_policy: bad_padding
			},
		]
	}
	if _ := memory_agg_validate_raw_domains(&bad_padding_facts) {
		assert false
	} else {
		assert err.msg().contains('unsupported padding policy')
	}

	anchor := MemoryAggInstructionAnchor{
		function_index: 1
		block_id:       1
	}
	mut candidates := []MemoryAggAggregateSlotCandidate{
		cap: memory_agg_max_aggregate_temps
	}
	for index in 0 .. memory_agg_max_aggregate_temps {
		candidates << MemoryAggAggregateSlotCandidate{
			definition:     anchor
			owner_value_id: ssa.ValueID(index + 1)
			role:           .aggregate_temp
			purpose:        .load_result
		}
	}
	ids, ordinals := memory_agg_assign_temp_ids(candidates) or { panic(err) }
	assert ids[memory_agg_max_aggregate_temps] == u32(0x800003ff)
	assert ordinals[memory_agg_max_aggregate_temps] == u16(1023)
	too_many := []MemoryAggAggregateSlotCandidate{
		len:  memory_agg_max_aggregate_temps + 1,
		init: MemoryAggAggregateSlotCandidate{
			role: .aggregate_temp
		}
	}
	if _, _ := memory_agg_assign_temp_ids(too_many) {
		assert false
	} else {
		assert err.msg().contains('aggregate temp count')
	}
	exact_action_count := memory_agg_checked_action_count(0,
		memory_agg_max_aggregate_actions) or { panic(err) }
	assert exact_action_count == memory_agg_max_aggregate_actions
	if _ := memory_agg_checked_action_count(0, memory_agg_max_aggregate_actions + 1) {
		assert false
	} else {
		assert err.msg().contains('aggregate action count')
	}
	byte_source := MemoryAggLogicalRegion{
		slot_id:         1
		size_bytes:      memory_agg_max_aggregate_object_bytes
		alignment_bytes: 1
	}
	byte_destination := MemoryAggLogicalRegion{
		slot_id:         2
		size_bytes:      memory_agg_max_aggregate_object_bytes
		alignment_bytes: 1
	}
	byte_chunks, _ := memory_agg_count_copy_chunks(byte_source, byte_destination) or {
		panic(err)
	}
	assert byte_chunks == memory_agg_max_aggregate_actions
	if _ := memory_agg_checked_action_count(byte_chunks, 1) {
		assert false
	} else {
		assert err.msg().contains('aggregate action count')
	}
}

fn test_memory_agg_m223_preflight_totals_object_and_first_exceed_endpoints() {
	exact_layout_count := MemoryAggFunctionFacts{
		aggregate_layouts: []MemoryAggAggregateLayoutBinding{
			len: memory_agg_max_aggregate_layouts
		}
	}
	memory_agg_validate_input_caps(&exact_layout_count) or { panic(err) }
	exceeded_layout_count := MemoryAggFunctionFacts{
		aggregate_layouts: []MemoryAggAggregateLayoutBinding{
			len: memory_agg_max_aggregate_layouts + 1
		}
	}
	if _ := memory_agg_validate_input_caps(&exceeded_layout_count) {
		assert false
	} else {
		assert err.msg().contains('aggregate layout count')
	}
	exact_alloca_count := MemoryAggFunctionFacts{
		aggregate_allocas: []MemoryAggAggregateAllocaBinding{
			len: memory_agg_max_static_locals
		}
	}
	memory_agg_validate_input_caps(&exact_alloca_count) or { panic(err) }
	exceeded_alloca_count := MemoryAggFunctionFacts{
		aggregate_allocas: []MemoryAggAggregateAllocaBinding{
			len: memory_agg_max_static_locals + 1
		}
	}
	if _ := memory_agg_validate_input_caps(&exceeded_alloca_count) {
		assert false
	} else {
		assert err.msg().contains('aggregate alloca count')
	}
	per_layout_fields := MemoryAggFunctionFacts{
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				fields: []MemoryAggAggregateFieldLayout{
					len: memory_agg_max_aggregate_fields_per_layout + 1
				}
			},
		]
	}
	if _ := memory_agg_validate_input_caps(&per_layout_fields) {
		assert false
	} else {
		assert err.msg().contains('layout 0 field count')
	}
	per_layout_padding := MemoryAggFunctionFacts{
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				padding: []MemoryAggByteRange{
					len: memory_agg_max_padding_ranges_per_layout + 1
				}
			},
		]
	}
	if _ := memory_agg_validate_input_caps(&per_layout_padding) {
		assert false
	} else {
		assert err.msg().contains('layout 0 padding count')
	}

	mut exact_field_layouts := []MemoryAggAggregateLayoutBinding{}
	for _ in 0 .. 16 {
		exact_field_layouts << MemoryAggAggregateLayoutBinding{
			fields: []MemoryAggAggregateFieldLayout{
				len: memory_agg_max_aggregate_fields_per_layout
			}
		}
	}
	exact_fields := MemoryAggFunctionFacts{
		aggregate_layouts: exact_field_layouts
	}
	memory_agg_validate_input_caps(&exact_fields) or { panic(err) }
	mut exceeded_field_layouts := exact_field_layouts.clone()
	exceeded_field_layouts << MemoryAggAggregateLayoutBinding{
		fields: [
			MemoryAggAggregateFieldLayout{},
		]
	}
	exceeded_fields := MemoryAggFunctionFacts{
		aggregate_layouts: exceeded_field_layouts
	}
	if _ := memory_agg_validate_input_caps(&exceeded_fields) {
		assert false
	} else {
		assert err.msg().contains('aggregate field count')
	}

	mut exact_padding_layouts := []MemoryAggAggregateLayoutBinding{}
	for _ in 0 .. 16 {
		exact_padding_layouts << MemoryAggAggregateLayoutBinding{
			padding: []MemoryAggByteRange{len: memory_agg_max_padding_ranges_per_layout}
		}
	}
	exact_padding_layouts << MemoryAggAggregateLayoutBinding{
		padding: []MemoryAggByteRange{len: 48}
	}
	exact_padding := MemoryAggFunctionFacts{
		aggregate_layouts: exact_padding_layouts
	}
	memory_agg_validate_input_caps(&exact_padding) or { panic(err) }
	mut exceeded_padding_layouts := exact_padding_layouts.clone()
	exceeded_padding_layouts[16] = MemoryAggAggregateLayoutBinding{
		padding: []MemoryAggByteRange{len: 49}
	}
	exceeded_padding := MemoryAggFunctionFacts{
		aggregate_layouts: exceeded_padding_layouts
	}
	if _ := memory_agg_validate_input_caps(&exceeded_padding) {
		assert false
	} else {
		assert err.msg().contains('aggregate padding count')
	}

	exact_operations := MemoryAggFunctionFacts{
		aggregate_field_pointers: []MemoryAggAggregateFieldPointerBinding{
			len: memory_agg_max_aggregate_facts
		}
	}
	memory_agg_validate_input_caps(&exact_operations) or { panic(err) }
	exceeded_operations := MemoryAggFunctionFacts{
		aggregate_field_pointers: []MemoryAggAggregateFieldPointerBinding{
			len: memory_agg_max_aggregate_facts + 1
		}
	}
	if _ := memory_agg_validate_input_caps(&exceeded_operations) {
		assert false
	} else {
		assert err.msg().contains('aggregate operation count')
	}

	mut large_module := ssa.Module.new()
	large_i8 := memory_agg_test_int_type(mut large_module, 8, false)
	large_type := memory_agg_test_struct_type(mut large_module, [large_i8])
	large_pointer := memory_agg_test_pointer_type(mut large_module, large_type)
	large_function := large_module.new_function('large_aggregate_layout', ssa.TypeID(0))
	large_block := large_module.add_block(large_function, 'entry')
	large_alloca := large_module.add_instr(.alloca, large_block, large_pointer, [])
	large_module.add_instr(.ret, large_block, ssa.TypeID(0), [])
	large_layout := MemoryAggAggregateLayoutBinding{
		profile:         .linux_x86_64_sysv_elf
		authority:       .native_plain
		type_id:         large_type
		size_bytes:      memory_agg_max_aggregate_object_bytes
		alignment_bytes: 1
		fields:          [
			MemoryAggAggregateFieldLayout{
				index:           0
				type_id:         large_i8
				offset_bytes:    0
				size_bytes:      1
				alignment_bytes: 1
			},
		]
		padding:         [
			MemoryAggByteRange{
				offset_bytes: 1
				size_bytes:   memory_agg_max_aggregate_object_bytes - 1
			},
		]
	}
	large_facts := MemoryAggFunctionFacts{
		profile:         .linux_x86_64_sysv_elf
		function_index:  large_function
		ssa_form:        .final_static
		scalar_layouts:  [memory_agg_test_layout(.linux_x86_64_sysv_elf,
			large_module, large_i8)]
		aggregate_layouts: [large_layout]
		aggregate_allocas: [
			MemoryAggAggregateAllocaBinding{
				profile:           .linux_x86_64_sysv_elf
				anchor:            memory_agg_test_anchor(large_module, large_function,
					large_alloca)
				authority:         .native_plain
				pointer_value_id:  large_alloca
				aggregate_type_id: large_type
				role:              .fixed_alloca
			},
		]
	}
	large_plan := plan_scalar_static_memory(large_module, &large_facts) or { panic(err) }
	assert large_plan.aggregate_layouts[0].size_bytes == 65536
	assert large_plan.total_requested_bytes == u64(65536)
	assert large_plan.aggregate_actions.len == 0
	too_large := MemoryAggFunctionFacts{
		...large_facts
		aggregate_layouts: [
			MemoryAggAggregateLayoutBinding{
				...large_layout
				size_bytes: memory_agg_max_aggregate_object_bytes + 1
			},
		]
	}
	memory_agg_test_expect_error(large_module, &too_large, 'outside 1..65536')

	mut merged_module := ssa.Module.new()
	merged_i8 := memory_agg_test_int_type(mut merged_module, 8, false)
	merged_i64 := memory_agg_test_int_type(mut merged_module, 64, false)
	merged_aggregate := memory_agg_test_struct_type(mut merged_module, [merged_i8])
	merged_scalar_pointer := memory_agg_test_pointer_type(mut merged_module, merged_i8)
	merged_aggregate_pointer := memory_agg_test_pointer_type(mut merged_module,
		merged_aggregate)
	merged_function := merged_module.new_function('merged_slot_bytes', ssa.TypeID(0))
	merged_block := merged_module.add_block(merged_function, 'entry')
	merged_count := merged_module.add_value(.constant, merged_i64, 'merged-count', 0)
	merged_scalar_alloca := merged_module.add_instr(.alloca, merged_block,
		merged_scalar_pointer, [merged_count])
	merged_aggregate_alloca := merged_module.add_instr(.alloca, merged_block,
		merged_aggregate_pointer, [])
	merged_module.add_instr(.ret, merged_block, ssa.TypeID(0), [])
	merged_layout := memory_agg_test_aggregate_layout(.linux_x86_64_sysv_elf,
		merged_module, merged_aggregate)
	merged_local := memory_agg_test_local(.linux_x86_64_sysv_elf, merged_module,
		merged_function, merged_scalar_alloca, merged_i8, .constant_count, merged_count)
	merged_aggregate_fact := MemoryAggAggregateAllocaBinding{
		profile:           .linux_x86_64_sysv_elf
		anchor:            memory_agg_test_anchor(merged_module, merged_function,
			merged_aggregate_alloca)
		authority:         .native_plain
		pointer_value_id:  merged_aggregate_alloca
		aggregate_type_id: merged_aggregate
		role:              .fixed_alloca
	}
	merged_exact := MemoryAggFunctionFacts{
		profile:          .linux_x86_64_sysv_elf
		function_index:   merged_function
		ssa_form:         .final_static
		scalar_layouts:   memory_agg_test_layouts(.linux_x86_64_sysv_elf,
			merged_module, [merged_i8, merged_i64])
		static_locals:    [merged_local]
		scalar_constants: [
			memory_agg_test_constant(merged_count, merged_i64,
				memory_agg_max_requested_bytes - 1),
		]
		aggregate_layouts: [merged_layout]
		aggregate_allocas: [merged_aggregate_fact]
	}
	merged_plan := plan_scalar_static_memory(merged_module, &merged_exact) or {
		panic(err)
	}
	assert merged_plan.total_requested_bytes == memory_agg_max_requested_bytes
	merged_exceeded := MemoryAggFunctionFacts{
		...merged_exact
		scalar_constants: [
			memory_agg_test_constant(merged_count, merged_i64,
				memory_agg_max_requested_bytes),
		]
	}
	memory_agg_test_expect_error(merged_module, &merged_exceeded, 'total slot bytes')
}

fn test_memory_agg_m224_transactional_deterministic_deep_nonaliasing() {
	first_fixture, first_base := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	second_fixture, second_facts := memory_agg_test_nominal_aggregate_fixture(
		.linux_x86_64_sysv_elf, .zero)
	mut input_fields := first_base.aggregate_layouts[0].fields.clone()
	mut input_padding := first_base.aggregate_layouts[0].padding.clone()
	mut input_layouts := [
		MemoryAggAggregateLayoutBinding{
			...first_base.aggregate_layouts[0]
			fields:  input_fields
			padding: input_padding
		},
	]
	first_facts := MemoryAggFunctionFacts{
		...first_base
		aggregate_layouts: input_layouts
	}
	first_plan := plan_scalar_static_memory(first_fixture.m, &first_facts) or {
		panic(err)
	}
	second_plan := plan_scalar_static_memory(second_fixture.m, &second_facts) or {
		panic(err)
	}
	assert first_plan == second_plan
	snapshot := first_plan
	input_fields[0] = MemoryAggAggregateFieldLayout{}
	input_padding[0] = MemoryAggByteRange{}
	input_layouts[0] = MemoryAggAggregateLayoutBinding{}
	assert first_plan == snapshot
	assert first_plan == second_plan

	values_before := first_fixture.m.values.clone()
	instructions_before := first_fixture.m.instrs.clone()
	bad := MemoryAggFunctionFacts{
		...first_base
		aggregate_loads: []MemoryAggAggregateLoadBinding{}
	}
	memory_agg_test_expect_error(first_fixture.m, &bad, 'requires M1b sidecar')
	assert first_fixture.m.values == values_before
	assert first_fixture.m.instrs == instructions_before
}

fn test_memory_agg_canary_aggregate_field_pointer_rejects_nonaggregate_gep_add_anchor() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	for opcode in [ssa.OpCode.get_element_ptr, .add] {
		fixture := memory_agg_test_pointer_fixture(profile, opcode, 8, 8, 0)
		base := memory_agg_test_pointer_facts(&fixture, 8, 0)
		facts := MemoryAggFunctionFacts{
			...base
			aggregate_field_pointers: [
				MemoryAggAggregateFieldPointerBinding{
					profile:                 profile
					anchor:                  memory_agg_test_anchor(fixture.m,
						fixture.function_index, fixture.pointer_id)
					source_pointer_value_id: fixture.alloca_id
					result_pointer_value_id: fixture.pointer_id
					aggregate_type_id:       fixture.element_type
					field_index:             0
				},
			]
		}
		memory_agg_test_expect_error(fixture.m, &facts, 'attests non-aggregate')
	}
}

fn test_memory_agg_canary_aggregate_field_pointer_refuses_further_derivation() {
	profile := TargetProfile.linux_x86_64_sysv_elf
	for opcode in [ssa.OpCode.get_element_ptr, .add, .bitcast] {
		mut fixture, facts := memory_agg_test_builder_aggregate_fixture(profile)
		source_id := fixture.field_pointers[1]
		source_type := fixture.m.values[int(source_id)].typ
		mut operands := [source_id]
		mut constants := facts.scalar_constants.clone()
		if opcode != .bitcast {
			delta_id := fixture.m.add_value(.constant, fixture.i64_type,
				'cross-field-byte-delta', 0)
			operands << delta_id
			constants << memory_agg_test_constant(delta_id, fixture.i64_type, 4)
		}
		derived_id := fixture.m.add_instr(opcode, fixture.block_id, source_type,
			operands)
		mut instructions := fixture.m.blocks[int(fixture.block_id)].instrs.clone()
		ret_position := instructions.index(fixture.ret_id)
		derived_position := instructions.index(derived_id)
		assert ret_position >= 0
		assert derived_position >= 0
		instructions[ret_position] = derived_id
		instructions[derived_position] = fixture.ret_id
		fixture.m.blocks[int(fixture.block_id)].instrs = instructions
		revised := MemoryAggFunctionFacts{
			...facts
			scalar_constants: constants
		}
		memory_agg_test_expect_error(fixture.m, &revised,
			'aggregate field pointer ${source_id} further')
	}
}
