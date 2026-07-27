module amd64

import v3.ssa

struct MemoryFrameComposeTestScalarFixture {
mut:
	m              &ssa.Module
	profile        TargetProfile
	function_index int
	element_type   ssa.TypeID
	count_type     ssa.TypeID
	count_id       ssa.ValueID
	alloca_id      ssa.ValueID
}

struct MemoryFrameComposeTestAggregateFixture {
mut:
	m              &ssa.Module
	profile        TargetProfile
	function_index int
	aggregate_type ssa.TypeID
	scalar_alloca_id ssa.ValueID
	alloca_id      ssa.ValueID
	construct_id   ssa.ValueID
	load_id        ssa.ValueID
	insert_id      ssa.ValueID
}

fn memory_frame_compose_test_int_type(mut m ssa.Module, width int, is_unsigned bool) ssa.TypeID {
	mut type_store := m.type_store
	type_id := if is_unsigned {
		type_store.get_uint(width)
	} else {
		type_store.get_int(width)
	}
	m.type_store = type_store
	return type_id
}

fn memory_frame_compose_test_pointer_type(mut m ssa.Module, element_type ssa.TypeID) ssa.TypeID {
	mut type_store := m.type_store
	type_id := type_store.get_ptr(element_type)
	m.type_store = type_store
	return type_id
}

fn memory_frame_compose_test_struct_type(mut m ssa.Module, fields []ssa.TypeID) ssa.TypeID {
	mut type_store := m.type_store
	type_id := type_store.get_tuple(fields)
	m.type_store = type_store
	return type_id
}

fn memory_frame_compose_test_anchor(m &ssa.Module, function_index int, value_id ssa.ValueID) MemoryAggInstructionAnchor {
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

fn memory_frame_compose_test_scalar_layout(profile TargetProfile, m &ssa.Module, type_id ssa.TypeID) MemoryAggScalarLayoutBinding {
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

fn memory_frame_compose_test_scalar_layouts(profile TargetProfile, m &ssa.Module, type_ids []ssa.TypeID) []MemoryAggScalarLayoutBinding {
	mut seen := map[int]bool{}
	mut result := []MemoryAggScalarLayoutBinding{}
	for type_id in type_ids {
		if int(type_id) in seen {
			continue
		}
		seen[int(type_id)] = true
		result << memory_frame_compose_test_scalar_layout(profile, m, type_id)
	}
	return result
}

fn memory_frame_compose_test_empty(profile TargetProfile) (&ssa.Module, MemoryAggFunctionFacts) {
	mut m := ssa.Module.new()
	function_index := m.new_function('memory_frame_compose_empty', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	return m, MemoryAggFunctionFacts{
		profile:        profile
		function_index: function_index
		ssa_form:       .final_static
	}
}

fn memory_frame_compose_test_caller(profile TargetProfile) (&ssa.Module, MemoryAggFunctionFacts, ssa.ValueID) {
	mut m := ssa.Module.new()
	callee_index := m.new_function('memory_frame_compose_callee', ssa.TypeID(0))
	callee_block := m.add_block(callee_index, 'entry')
	m.add_instr(.ret, callee_block, ssa.TypeID(0), [])
	caller_index := m.new_function('memory_frame_compose_caller', ssa.TypeID(0))
	caller_block := m.add_block(caller_index, 'entry')
	function_ref := m.add_value(.func_ref, ssa.TypeID(0), 'memory_frame_compose_callee',
		callee_index)
	call_id := m.add_instr(.call, caller_block, ssa.TypeID(0), [function_ref])
	m.add_instr(.ret, caller_block, ssa.TypeID(0), [])
	return m, MemoryAggFunctionFacts{
		profile:        profile
		function_index: caller_index
		ssa_form:       .final_static
	}, call_id
}

fn memory_frame_compose_test_scalar(profile TargetProfile, count u64) (MemoryFrameComposeTestScalarFixture, MemoryAggFunctionFacts) {
	mut m := ssa.Module.new()
	element_type := memory_frame_compose_test_int_type(mut m, 8, true)
	count_type := memory_frame_compose_test_int_type(mut m, 64, false)
	pointer_type := memory_frame_compose_test_pointer_type(mut m, element_type)
	function_index := m.new_function('memory_frame_compose_scalar', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	count_id := m.add_value(.constant, count_type, 'count-sidecar-only', 0)
	alloca_id := m.add_instr(.alloca, block, pointer_type, [count_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	fixture := MemoryFrameComposeTestScalarFixture{
		m:              m
		profile:        profile
		function_index: function_index
		element_type:   element_type
		count_type:     count_type
		count_id:       count_id
		alloca_id:      alloca_id
	}
	facts := MemoryAggFunctionFacts{
		profile:         profile
		function_index:  function_index
		ssa_form:        .final_static
		scalar_layouts:  memory_frame_compose_test_scalar_layouts(profile, m,
			[element_type, count_type])
		static_locals:   [
			MemoryAggStaticLocalBinding{
				profile:        profile
				anchor:         memory_frame_compose_test_anchor(m, function_index,
					alloca_id)
				authority:      .native_plain
				element_type:   element_type
				form:           .constant_count
				count_value_id: count_id
			},
		]
		scalar_constants: [
			ScalarConstantBinding{
				value_id: count_id
				type_id:  count_type
				raw_bits: count
			},
		]
	}
	return fixture, facts
}

fn memory_frame_compose_test_align_up(value u64, alignment u64) u64 {
	return (value + alignment - 1) & ~(alignment - 1)
}

fn memory_frame_compose_test_aggregate_layout(profile TargetProfile, m &ssa.Module, type_id ssa.TypeID) MemoryAggAggregateLayoutBinding {
	typ := m.type_store.types[int(type_id)]
	mut fields := []MemoryAggAggregateFieldLayout{cap: typ.fields.len}
	mut padding := []MemoryAggByteRange{}
	mut cursor := u64(0)
	mut maximum_alignment := u64(1)
	for field_index, field_type in typ.fields {
		scalar := memory_frame_compose_test_scalar_layout(profile, m, field_type)
		alignment := u64(scalar.alignment_bytes)
		offset := memory_frame_compose_test_align_up(cursor, alignment)
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
	size := memory_frame_compose_test_align_up(cursor, maximum_alignment)
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

fn memory_frame_compose_test_aggregate(profile TargetProfile) (MemoryFrameComposeTestAggregateFixture, MemoryAggFunctionFacts) {
	mut m := ssa.Module.new()
	i8_type := memory_frame_compose_test_int_type(mut m, 8, false)
	i16_type := memory_frame_compose_test_int_type(mut m, 16, false)
	i32_type := memory_frame_compose_test_int_type(mut m, 32, false)
	i64_type := memory_frame_compose_test_int_type(mut m, 64, false)
	aggregate_type := memory_frame_compose_test_struct_type(mut m,
		[i8_type, i32_type, i16_type])
	scalar_pointer := memory_frame_compose_test_pointer_type(mut m, i64_type)
	aggregate_pointer := memory_frame_compose_test_pointer_type(mut m, aggregate_type)
	function_index := m.new_function('memory_frame_compose_aggregate', ssa.TypeID(0))
	block := m.add_block(function_index, 'entry')
	scalar_alloca_id := m.add_instr(.alloca, block, scalar_pointer, [])
	alloca_id := m.add_instr(.alloca, block, aggregate_pointer, [])
	field_values := [
		m.add_value(.constant, i8_type, 'field-0', 0),
		m.add_value(.constant, i32_type, 'field-1', 0),
		m.add_value(.constant, i16_type, 'field-2', 0),
	]
	construct_id := m.add_instr(.struct_init, block, aggregate_type, field_values)
	first_store_id := m.add_instr(.store, block, ssa.TypeID(0),
		[construct_id, alloca_id])
	load_id := m.add_instr(.load, block, aggregate_type, [alloca_id])
	extract_id := m.add_instr(.extractvalue, block, i32_type, [load_id])
	insert_value_id := m.add_value(.constant, i32_type, 'insert-field', 0)
	insert_id := m.add_instr(.insertvalue, block, aggregate_type,
		[load_id, insert_value_id])
	second_store_id := m.add_instr(.store, block, ssa.TypeID(0),
		[insert_id, alloca_id])
	m.add_instr(.ret, block, ssa.TypeID(0), [])
	fixture := MemoryFrameComposeTestAggregateFixture{
		m:                m
		profile:          profile
		function_index:   function_index
		aggregate_type:   aggregate_type
		scalar_alloca_id: scalar_alloca_id
		alloca_id:        alloca_id
		construct_id:     construct_id
		load_id:          load_id
		insert_id:        insert_id
	}
	facts := MemoryAggFunctionFacts{
		profile:         profile
		function_index:  function_index
		ssa_form:        .final_static
		scalar_layouts:  memory_frame_compose_test_scalar_layouts(profile, m,
			[i8_type, i16_type, i32_type, i64_type])
		static_locals:   [
			MemoryAggStaticLocalBinding{
				profile:        profile
				anchor:         memory_frame_compose_test_anchor(m, function_index,
					scalar_alloca_id)
				authority:      .native_plain
				element_type:   i64_type
				form:           .one
				count_value_id: 0
			},
		]
		scalar_constants: [
			ScalarConstantBinding{
				value_id: field_values[0]
				type_id:  i8_type
				raw_bits: 0x12
			},
			ScalarConstantBinding{
				value_id: field_values[1]
				type_id:  i32_type
				raw_bits: 0x34567890
			},
			ScalarConstantBinding{
				value_id: field_values[2]
				type_id:  i16_type
				raw_bits: 0x5678
			},
			ScalarConstantBinding{
				value_id: insert_value_id
				type_id:  i32_type
				raw_bits: 0x10203040
			},
		]
		aggregate_layouts: [
			memory_frame_compose_test_aggregate_layout(profile, m, aggregate_type),
		]
		aggregate_allocas: [
			MemoryAggAggregateAllocaBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					alloca_id)
				authority:         .native_plain
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
				role:              .fixed_alloca
			},
		]
		aggregate_constructs: [
			MemoryAggAggregateConstructBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					construct_id)
				result_value_id:   construct_id
				aggregate_type_id: aggregate_type
				padding_policy:    .zero
			},
		]
		aggregate_loads: [
			MemoryAggAggregateLoadBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					load_id)
				semantics:         .nonvolatile
				pointer_value_id:  alloca_id
				result_value_id:   load_id
				aggregate_type_id: aggregate_type
			},
		]
		aggregate_stores: [
			MemoryAggAggregateStoreBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					first_store_id)
				semantics:         .nonvolatile
				source_value_id:   construct_id
				pointer_value_id:  alloca_id
				aggregate_type_id: aggregate_type
			},
			MemoryAggAggregateStoreBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
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
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					extract_id)
				source_value_id:   load_id
				result_value_id:   extract_id
				aggregate_type_id: aggregate_type
				field_index:       1
			},
		]
		aggregate_inserts: [
			MemoryAggAggregateInsertBinding{
				profile:           profile
				anchor:            memory_frame_compose_test_anchor(m, function_index,
					insert_id)
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

fn memory_frame_compose_test_call(profile TargetProfile, function_id u32, has_call bool, call_extent u64) MemoryFrameCallExtentFacts {
	return MemoryFrameCallExtentFacts{
		present:           true
		function_id:       function_id
		profile:           profile
		has_call:          has_call
		call_extent_bytes: call_extent
	}
}

fn memory_frame_compose_test_saves(function_id u32, registers []MemorySavedGpr) MemoryCalleeSaveFacts {
	return MemoryCalleeSaveFacts{
		present:     true
		function_id: function_id
		registers:   registers.clone()
	}
}

fn memory_frame_compose_test_plan(m &ssa.Module, facts &MemoryAggFunctionFacts, call MemoryFrameCallExtentFacts, registers []MemorySavedGpr) !MemoryFrameCompositionPlan {
	saves := memory_frame_compose_test_saves(call.function_id, registers)
	return plan_scalar_static_memory_frame(m, facts, &call, &saves)
}

fn memory_frame_compose_test_expect_error(m &ssa.Module, facts &MemoryAggFunctionFacts, call MemoryFrameCallExtentFacts, saves MemoryCalleeSaveFacts, expected string) {
	if _ := plan_scalar_static_memory_frame(m, facts, &call, &saves) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected, '`${err.msg()}` != `${expected}`'
	}
}

fn memory_frame_compose_test_expect_private_error(run fn () !, expected string) {
	run() or {
		assert err.msg() == expected, '`${err.msg()}` != `${expected}`'
		return
	}
	assert false, 'expected `${expected}`'
}

fn memory_frame_compose_test_memory_plan(requests []MemorySlotRequest, aggregate_requests []MemorySlotRequest, total u64) MemoryAggPlan {
	mut scalar_slots := []MemoryAggSlotBinding{cap: requests.len}
	for request in requests {
		scalar_slots << MemoryAggSlotBinding{
			alloca_value_id: ssa.ValueID(request.id)
			request:         request
		}
	}
	mut aggregate_slots := []MemoryAggAggregateSlotBinding{cap: aggregate_requests.len}
	for request in aggregate_requests {
		role := if request.kind == .aggregate_temp {
			MemoryAggAggregateSlotRole.aggregate_temp
		} else {
			.fixed_alloca
		}
		aggregate_slots << MemoryAggAggregateSlotBinding{
			owner_value_id: ssa.ValueID(request.id)
			role:           role
			request:        request
		}
	}
	return MemoryAggPlan{
		slot_requests:         scalar_slots
		aggregate_slots:       aggregate_slots
		total_requested_bytes: total
	}
}

fn test_memory_frame_compose_m601_schema_and_call_fact_domains() {
	assert int(MemoryFrameComposedSlotOrigin.scalar_fixed_alloca) == 0
	assert int(MemoryFrameComposedSlotOrigin.aggregate_fixed_alloca) == 1
	assert int(MemoryFrameComposedSlotOrigin.aggregate_temp) == 2
	m, facts := memory_frame_compose_test_empty(.linux_x86_64_sysv_elf)
	missing := MemoryFrameCallExtentFacts{}
	saves := memory_frame_compose_test_saves(0, [])
	memory_frame_compose_test_expect_error(m, &facts, missing, saves,
		'amd64 memory frame compose: call-extent facts are required')
	forged := MemoryFrameCallExtentFacts{
		present:     true
		function_id: 0
		profile:     unsafe { TargetProfile(255) }
	}
	memory_frame_compose_test_expect_error(m, &facts, forged, saves,
		'amd64 memory frame compose: unsupported call-extent target profile')
}

fn test_memory_frame_compose_m602_empty_sysv_dwarf_zero_delta() {
	m, facts := memory_frame_compose_test_empty(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(m, &facts, call, []) or { panic(err) }
	assert plan.memory.slot_requests.len == 0
	assert plan.frame.cfi.disposition == .dwarf_zero_delta
	assert plan.frame.cfi.frame.layout_frame.stack_adjustment_bytes == 0
	assert plan.frame.cie_initial_instruction_bytes == [u8(0x0c), 0x07, 0x08, 0x90,
		0x01]
	assert plan.slot_bindings.len == 0
}

fn test_memory_frame_compose_m603_empty_apple_dwarf_zero_delta() {
	m, facts := memory_frame_compose_test_empty(.macos_x86_64_sysv_macho)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(m, &facts, call, []) or { panic(err) }
	assert plan.memory.profile == .macos_x86_64_sysv_macho
	assert plan.frame.cfi.disposition == .dwarf_zero_delta
	assert plan.frame.total_instruction_fragment_bytes == 5
	assert plan.slot_bindings.len == 0
}

fn test_memory_frame_compose_m604_empty_windows_none() {
	m, facts := memory_frame_compose_test_empty(.windows_x86_64_microsoft_abi_coff)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(m, &facts, call, []) or { panic(err) }
	assert plan.frame.cfi.disposition == .windows_none
	assert plan.frame.cie_initial_instruction_bytes.len == 0
	assert plan.frame.total_instruction_fragment_bytes == 0
	assert !plan.frame.cfi.frame.windows_unwind.present
	assert plan.slot_bindings.len == 0
}

fn test_memory_frame_compose_m605_scalar_fixed_alloca_binding() {
	fixture, facts := memory_frame_compose_test_scalar(.linux_x86_64_sysv_elf, 8)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	assert plan.memory.slot_requests.len == 1
	assert plan.memory.slot_requests[0].alloca_value_id == fixture.alloca_id
	assert plan.slot_bindings == [
		MemoryFrameComposedSlotBinding{
			origin:           .scalar_fixed_alloca
			source_index:     0
			frame_slot_index: 0
		},
	]
	assert plan.frame.cfi.frame.layout_frame.slots[0].id == u32(fixture.alloca_id)
	assert plan.frame.cfi.frame.slots[0].placement ==
		plan.frame.cfi.frame.layout_frame.slots[0]
}

fn test_memory_frame_compose_m606_aggregate_fixed_alloca_binding() {
	fixture, facts := memory_frame_compose_test_aggregate(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	assert plan.memory.slot_requests.len == 1
	assert plan.memory.aggregate_slots.len == 4
	assert plan.memory.aggregate_slots[0].owner_value_id == fixture.alloca_id
	assert plan.memory.aggregate_slots[0].role == .fixed_alloca
	assert plan.slot_bindings[1].origin == .aggregate_fixed_alloca
	assert plan.slot_bindings[1].source_index == 0
}

fn test_memory_frame_compose_m607_all_aggregate_temp_purposes_bind() {
	fixture, facts := memory_frame_compose_test_aggregate(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	assert plan.memory.aggregate_slots[1..].map(it.purpose) == [
		MemoryAggTempPurpose.construct_result,
		.load_result,
		.insert_result,
	]
	assert plan.memory.aggregate_slots[1..].map(it.owner_value_id) == [
		fixture.construct_id,
		fixture.load_id,
		fixture.insert_id,
	]
	for binding in plan.slot_bindings[2..] {
		assert binding.origin == .aggregate_temp
		assert plan.frame.cfi.frame.slots[int(binding.frame_slot_index)].placement.id >=
			u32(0x80000000)
	}
}

fn test_memory_frame_compose_m608_mixed_order_and_exact_bijection() {
	fixture, facts := memory_frame_compose_test_aggregate(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	assert plan.slot_bindings.len == 5
	assert plan.slot_bindings.map(it.frame_slot_index) == [u32(0), 1, 2, 3, 4]
	assert plan.slot_bindings.map(it.origin) == [
		MemoryFrameComposedSlotOrigin.scalar_fixed_alloca,
		.aggregate_fixed_alloca,
		.aggregate_temp,
		.aggregate_temp,
		.aggregate_temp,
	]
	geometry := plan.frame.cfi.frame.layout_frame.slots
	assert geometry.map(it.id) == [
		u32(fixture.scalar_alloca_id),
		u32(fixture.alloca_id),
		u32(0x80000000),
		u32(0x80000001),
		u32(0x80000002),
	]
	for index, encoded in plan.frame.cfi.frame.slots {
		assert encoded.source_placement_index == u32(index)
		assert encoded.placement == geometry[index]
	}
}

fn test_memory_frame_compose_m609_sysv_red_zone_endpoint_128() {
	fixture, facts := memory_frame_compose_test_scalar(.linux_x86_64_sysv_elf, 128)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	frame := plan.frame.cfi.frame.layout_frame
	assert frame.uses_red_zone
	assert frame.red_zone_extent_bytes == 128
	assert frame.stack_adjustment_bytes == 0
	assert frame.slots[0].basis == .entry_rsp
	assert frame.slots[0].displacement_bytes == -128
}

fn test_memory_frame_compose_m610_apple_red_zone_fallback_129() {
	fixture, facts := memory_frame_compose_test_scalar(.macos_x86_64_sysv_macho, 129)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call, []) or {
		panic(err)
	}
	frame := plan.frame.cfi.frame.layout_frame
	assert !frame.uses_red_zone
	assert frame.non_red_zone_extent_bytes == 129
	assert frame.stack_adjustment_bytes == 136
	assert frame.slots[0].basis == .body_rsp
	assert frame.slots[0].displacement_bytes == 0
}

fn test_memory_frame_compose_m611_nonempty_saves_force_forbidden_policy() {
	fixture, facts := memory_frame_compose_test_scalar(.linux_x86_64_sysv_elf, 8)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	plan := memory_frame_compose_test_plan(fixture.m, &facts, call,
		[MemorySavedGpr.rbx]) or { panic(err) }
	frame := plan.frame.cfi.frame
	assert frame.layout_frame.red_zone_policy == .forbidden
	assert !frame.layout_frame.uses_red_zone
	assert frame.save_push_count == 1
	assert frame.base_allocation_bytes == 8
	assert frame.padding_bytes == 8
	assert frame.allocation_bytes == 16
	assert frame.total_stack_extent_bytes == 24
	assert plan.frame.cfi.disposition == .dwarf_transitions
}

fn test_memory_frame_compose_m612_sysv_call_high_water_zero_derives_d8() {
	m, facts, call_id := memory_frame_compose_test_caller(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		true, 0)
	plan := memory_frame_compose_test_plan(m, &facts, call, []) or { panic(err) }
	frame := plan.frame.cfi.frame.layout_frame
	assert m.instrs[m.values[int(call_id)].index].op == .call
	assert frame.has_call
	assert frame.call_extent_bytes == 0
	assert !frame.uses_red_zone
	assert frame.non_red_zone_extent_bytes == 0
	assert frame.stack_adjustment_bytes == 8
	assert plan.frame.cfi.frame.prologue_bytes == [u8(0x48), 0x83, 0xec, 0x08]
	assert plan.frame.cfi.frame.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x08]
}

fn test_memory_frame_compose_m613_microsoft_call_high_water_32_derives_d40() {
	m, facts, call_id := memory_frame_compose_test_caller(.windows_x86_64_microsoft_abi_coff)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		true, 32)
	plan := memory_frame_compose_test_plan(m, &facts, call, []) or { panic(err) }
	frame := plan.frame.cfi.frame
	assert m.instrs[m.values[int(call_id)].index].op == .call
	assert frame.layout_frame.has_call
	assert frame.layout_frame.call_extent_bytes == 32
	assert frame.layout_frame.non_red_zone_extent_bytes == 32
	assert frame.layout_frame.stack_adjustment_bytes == 40
	assert frame.prologue_bytes == [u8(0x48), 0x83, 0xec, 0x28]
	assert frame.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x28]
	assert frame.windows_unwind.present
	assert frame.windows_unwind.xdata_bytes.len == 8
	assert plan.frame.cfi.disposition == .windows_none
	assert plan.frame.cie_initial_instruction_bytes.len == 0
}

fn test_memory_frame_compose_m614_merged_slot_cap_endpoints() {
	mut requests := []MemorySlotRequest{cap: memory_frame_compose_max_slots + 1}
	for id in 1 .. memory_frame_compose_max_slots + 1 {
		requests << MemorySlotRequest{
			id:              u32(id)
			kind:            .fixed_alloca
			size_bytes:      1
			alignment_bytes: 1
		}
	}
	exact := memory_frame_compose_test_memory_plan(requests, [],
		u64(memory_frame_compose_max_slots))
	preflight := memory_frame_compose_preflight(&exact) or { panic(err) }
	assert preflight.scalar_slot_count == 1024
	assert preflight.aggregate_slot_count == 0
	assert preflight.total_slot_count == 1024
	requests << MemorySlotRequest{
		id:              1025
		kind:            .fixed_alloca
		size_bytes:      1
		alignment_bytes: 1
	}
	exceeded := memory_frame_compose_test_memory_plan(requests, [], 1025)
	memory_frame_compose_test_expect_private_error(fn [exceeded] () ! {
		memory_frame_compose_preflight(&exceeded)!
	}, 'amd64 memory frame compose: merged slot count 1025 exceeds 1024')
}

fn test_memory_frame_compose_m615_requested_byte_and_overflow_endpoints() {
	exact_request := MemorySlotRequest{
		id:              1
		kind:            .fixed_alloca
		size_bytes:      u64(0x7ffffff8)
		alignment_bytes: 8
	}
	exact := memory_frame_compose_test_memory_plan([exact_request], [],
		u64(0x7ffffff8))
	preflight := memory_frame_compose_preflight(&exact) or { panic(err) }
	assert preflight.total_requested_bytes == u64(0x7ffffff8)
	exceeded_request := MemorySlotRequest{
		...exact_request
		size_bytes: u64(0x80000000)
	}
	exceeded := memory_frame_compose_test_memory_plan([exceeded_request], [],
		u64(0x80000000))
	memory_frame_compose_test_expect_private_error(fn [exceeded] () ! {
		memory_frame_compose_preflight(&exceeded)!
	}, 'amd64 memory frame compose: requested bytes 2147483648 exceed 2147483640')
	overflow := memory_frame_compose_test_memory_plan([
		MemorySlotRequest{
			id:              1
			kind:            .fixed_alloca
			size_bytes:      max_u64
			alignment_bytes: 8
		},
		MemorySlotRequest{
			id:              2
			kind:            .fixed_alloca
			size_bytes:      1
			alignment_bytes: 1
		},
	], [], 0)
	memory_frame_compose_test_expect_private_error(fn [overflow] () ! {
		memory_frame_compose_preflight(&overflow)!
	}, 'amd64 memory frame compose: arithmetic overflow')
}

fn test_memory_frame_compose_m616_m1_error_precedes_frame_errors() {
	m, base := memory_frame_compose_test_empty(.linux_x86_64_sysv_elf)
	facts := MemoryAggFunctionFacts{
		...base
		ssa_form: .unknown
	}
	call := memory_frame_compose_test_call(base.profile, u32(base.function_index),
		false, 8)
	missing_saves := MemoryCalleeSaveFacts{}
	memory_frame_compose_test_expect_error(m, &facts, call, missing_saves,
		'amd64 memory agg m1a: upstream_standby:final_static SSA attestation is required')
}

fn test_memory_frame_compose_m617_stale_call_attestations_refuse() {
	m, facts := memory_frame_compose_test_empty(.linux_x86_64_sysv_elf)
	saves := memory_frame_compose_test_saves(u32(facts.function_index), [])
	wrong_function := memory_frame_compose_test_call(facts.profile,
		u32(facts.function_index + 1), false, 0)
	memory_frame_compose_test_expect_error(m, &facts, wrong_function, saves,
		'amd64 memory frame compose: call-extent function 1 does not match memory function 0')
	wrong_profile := memory_frame_compose_test_call(.macos_x86_64_sysv_macho,
		u32(facts.function_index), false, 0)
	memory_frame_compose_test_expect_error(m, &facts, wrong_profile, saves,
		'amd64 memory frame compose: call-extent profile does not match memory profile')
}

fn test_memory_frame_compose_m618_predecessor_errors_propagate_exactly() {
	m, facts := memory_frame_compose_test_empty(.linux_x86_64_sysv_elf)
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	memory_frame_compose_test_expect_error(m, &facts, call, MemoryCalleeSaveFacts{},
		'amd64 memory frame save: callee-save facts are required')
	noncaller_extent := MemoryFrameCallExtentFacts{
		...call
		call_extent_bytes: 8
	}
	saves := memory_frame_compose_test_saves(u32(facts.function_index), [])
	memory_frame_compose_test_expect_error(m, &facts, noncaller_extent, saves,
		'amd64 memory frame: noncaller call extent must be zero')
	windows_m, windows_facts := memory_frame_compose_test_empty(.windows_x86_64_microsoft_abi_coff)
	windows_call := memory_frame_compose_test_call(windows_facts.profile,
		u32(windows_facts.function_index), true, 24)
	windows_saves := memory_frame_compose_test_saves(u32(windows_facts.function_index),
		[])
	memory_frame_compose_test_expect_error(windows_m, &windows_facts, windows_call,
		windows_saves, 'amd64 memory frame: Microsoft call extent 24 is below 32')
}

fn test_memory_frame_compose_m619_internal_source_bijection_and_endpoint_canaries() {
	scalar_request := MemorySlotRequest{
		id:              7
		kind:            .fixed_alloca
		size_bytes:      8
		alignment_bytes: 8
	}
	bad_kind := memory_frame_compose_test_memory_plan([
		MemorySlotRequest{
			...scalar_request
			kind: .spill
		},
	], [], 8)
	bad_kind_preflight := memory_frame_compose_preflight(&bad_kind) or { panic(err) }
	memory_frame_compose_test_expect_private_error(fn [bad_kind, bad_kind_preflight] () ! {
		memory_frame_compose_materialize_sources(&bad_kind, bad_kind_preflight)!
	}, 'amd64 memory frame compose: scalar slot 0 is not fixed_alloca')
	duplicate := memory_frame_compose_test_memory_plan([scalar_request], [
		MemorySlotRequest{
			...scalar_request
			kind: .aggregate_temp
		},
	], 16)
	duplicate_preflight := memory_frame_compose_preflight(&duplicate) or { panic(err) }
	memory_frame_compose_test_expect_private_error(fn [duplicate, duplicate_preflight] () ! {
		memory_frame_compose_materialize_sources(&duplicate, duplicate_preflight)!
	}, 'amd64 memory frame compose: duplicate logical slot id 7')
	missing_pointer := MemoryAggPlan{
		pointers: [
			MemoryAggPointerSnapshot{
				root_slot_id: 99
			},
		]
	}
	indices := {
		7: u32(0)
	}
	memory_frame_compose_test_expect_private_error(fn [missing_pointer, indices] () ! {
		memory_frame_compose_validate_endpoints(&missing_pointer, indices)!
	}, 'amd64 memory frame compose: pointer 0 references missing logical slot 99')
}

fn test_memory_frame_compose_m620_determinism_transactionality_deep_copy_and_direct_m5_identity() {
	fixture, mut facts := memory_frame_compose_test_aggregate(.linux_x86_64_sysv_elf)
	mut registers := [MemorySavedGpr.r12, .rbx]
	call := memory_frame_compose_test_call(facts.profile, u32(facts.function_index),
		false, 0)
	first_saves := memory_frame_compose_test_saves(call.function_id, registers)
	second_saves := memory_frame_compose_test_saves(call.function_id, registers)
	values_before := fixture.m.values.clone()
	instructions_before := fixture.m.instrs.clone()
	first := plan_scalar_static_memory_frame(fixture.m, &facts, &call,
		&first_saves) or { panic(err) }
	second := plan_scalar_static_memory_frame(fixture.m, &facts, &call,
		&second_saves) or { panic(err) }
	assert first == second
	assert fixture.m.values == values_before
	assert fixture.m.instrs == instructions_before
	unsafe {
		assert first.slot_bindings.data != second.slot_bindings.data
		assert first.memory.slot_requests.data != second.memory.slot_requests.data
		assert first.memory.aggregate_slots.data != second.memory.aggregate_slots.data
		assert first.memory.aggregate_actions.data != second.memory.aggregate_actions.data
		assert first.frame.cfi.frame.layout_frame.slots.data !=
			second.frame.cfi.frame.layout_frame.slots.data
		assert first.frame.cfi.frame.slots.data != second.frame.cfi.frame.slots.data
		assert first.frame.cie_initial_instruction_bytes.data !=
			second.frame.cie_initial_instruction_bytes.data
		mut binding := &MemoryFrameComposedSlotBinding(first.slot_bindings.data)
		mut cie := &u8(first.frame.cie_initial_instruction_bytes.data)
		binding[0] = MemoryFrameComposedSlotBinding{}
		cie[0] = 0
	}
	assert second.slot_bindings[0].origin == .scalar_fixed_alloca
	assert second.frame.cie_initial_instruction_bytes[0] == 0x0c
	registers[0] = .r15
	mut layouts := facts.scalar_layouts.clone()
	layouts[0] = MemoryAggScalarLayoutBinding{}
	facts = MemoryAggFunctionFacts{
		...facts
		scalar_layouts: layouts
	}
	assert second.frame.cfi.frame.save_facts.registers == [MemorySavedGpr.r12, .rbx]
	assert second.memory.scalar_layouts[0] != MemoryAggScalarLayoutBinding{}

	mut requests := []MemorySlotRequest{
		cap: second.memory.slot_requests.len + second.memory.aggregate_slots.len
	}
	for slot in second.memory.slot_requests {
		requests << slot.request
	}
	for slot in second.memory.aggregate_slots {
		requests << slot.request
	}
	direct_facts := MemoryFunctionFrameFacts{
		function_id:       second.memory.function_id
		profile:           second.memory.profile
		extent_kind:       .fixed
		call_extent_bytes: call.call_extent_bytes
		has_call:          call.has_call
		slots:             requests
	}
	direct := plan_memory_saved_frame_cfi_instruction_fragments(&direct_facts,
		&second_saves) or { panic(err) }
	assert direct == second.frame

	bad_call := MemoryFrameCallExtentFacts{
		...call
		call_extent_bytes: 8
	}
	if _ := plan_scalar_static_memory_frame(fixture.m, &facts, &bad_call,
		&second_saves) {
		assert false
	} else {
		assert err.msg() == 'amd64 memory agg m1a: upstream_standby:scalar layout 0 is not native_plain'
	}
	assert fixture.m.values == values_before
	assert fixture.m.instrs == instructions_before
}
