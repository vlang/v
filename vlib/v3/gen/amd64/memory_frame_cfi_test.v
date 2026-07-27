module amd64

fn memory_frame_cfi_test_slot(id u32, size u64, alignment u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              id
		kind:            .local
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn memory_frame_cfi_test_facts(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       77
		profile:           profile
		extent_kind:       .fixed
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_cfi_test_empty(profile TargetProfile) MemoryFunctionFrameFacts {
	return memory_frame_cfi_test_facts(profile, false, 0, [])
}

fn memory_frame_cfi_test_adjustment(profile TargetProfile, adjustment u64) MemoryFunctionFrameFacts {
	if adjustment == 0 {
		return memory_frame_cfi_test_empty(profile)
	}
	return memory_frame_cfi_test_facts(profile, false, 0, [
		memory_frame_cfi_test_slot(1, adjustment, 1),
	])
}

fn memory_frame_cfi_test_saves(registers []MemorySavedGpr) MemoryCalleeSaveFacts {
	return MemoryCalleeSaveFacts{
		present:     true
		function_id: 77
		registers:   registers.clone()
	}
}

fn memory_frame_cfi_test_plan(facts &MemoryFunctionFrameFacts, registers []MemorySavedGpr) MemoryFrameCfiPlan {
	saves := memory_frame_cfi_test_saves(registers)
	return plan_memory_saved_frame_cfi(facts, &saves) or { panic(err.msg()) }
}

fn memory_frame_cfi_test_expect_error(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts, expected string) {
	if _ := plan_memory_saved_frame_cfi(facts, saves) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected
	}
}

fn memory_frame_cfi_test_baseline() MemoryFrameCfiBaseline {
	return MemoryFrameCfiBaseline{
		present:                               true
		code_alignment_factor:                 1
		data_alignment_factor:                 -8
		cfa_register_number:                   7
		cfa_offset_bytes:                      8
		return_address_register_number:        16
		return_address_cfa_displacement_bytes: -8
	}
}

fn memory_frame_cfi_test_def(region MemoryFrameCfiRegion, phase MemoryFrameCfiPhase, instruction_end u8, cfa u64) MemoryFrameCfiOp {
	return MemoryFrameCfiOp{
		region:                       region
		phase:                        phase
		instruction_end_offset_bytes: instruction_end
		same_pc_ordinal:              0
		kind:                         .def_cfa_offset
		cfa_offset_bytes:             cfa
		register:                     .rbx
	}
}

fn memory_frame_cfi_test_offset(instruction_end u8, cfa u64, register MemorySavedGpr, dwarf_register u8) MemoryFrameCfiOp {
	return MemoryFrameCfiOp{
		region:                       .prologue
		phase:                        .after_push
		instruction_end_offset_bytes: instruction_end
		same_pc_ordinal:              1
		kind:                         .offset_register
		cfa_offset_bytes:             cfa
		register_present:             true
		register:                     register
		dwarf_register_number:        dwarf_register
		saved_cfa_displacement_bytes: -i64(cfa)
	}
}

fn memory_frame_cfi_test_assert_active_baseline(plan &MemoryFrameCfiPlan) {
	assert plan.baseline == memory_frame_cfi_test_baseline()
}

fn memory_frame_cfi_test_assert_windows_none(plan &MemoryFrameCfiPlan) {
	assert plan.disposition == .windows_none
	assert plan.baseline == MemoryFrameCfiBaseline{}
	assert plan.prologue_ops.len == 0
	assert plan.epilogue_template_ops.len == 0
}

fn memory_frame_cfi_test_replay(plan &MemoryFrameCfiPlan) {
	assert plan.disposition == .dwarf_transitions
	mut cfa := plan.baseline.cfa_offset_bytes
	mut retained_rules := map[int]i64{}
	for op in plan.prologue_ops {
		assert op.region == .prologue
		match op.kind {
			.def_cfa_offset {
				assert !op.register_present
				cfa = op.cfa_offset_bytes
			}
			.offset_register {
				assert op.register_present
				assert op.cfa_offset_bytes == cfa
				assert op.saved_cfa_displacement_bytes == -i64(cfa)
				retained_rules[int(op.register)] = op.saved_cfa_displacement_bytes
			}
		}
	}
	assert cfa == plan.frame.body_cfa_offset_bytes
	assert retained_rules.len == plan.frame.saves.len
	for op in plan.epilogue_template_ops {
		assert op.region == .epilogue_template
		assert op.kind == .def_cfa_offset
		assert !op.register_present
		cfa = op.cfa_offset_bytes
	}
	assert cfa == plan.baseline.cfa_offset_bytes
	for save in plan.frame.saves {
		assert int(save.register) in retained_rules
		assert retained_rules[int(save.register)] < 0
	}
}

fn test_memory_frame_cfi_c401_schema_ordinals_and_linux_zero_delta_are_exact() {
	assert int(MemoryFrameCfiDisposition.windows_none) == 0
	assert int(MemoryFrameCfiDisposition.dwarf_zero_delta) == 1
	assert int(MemoryFrameCfiDisposition.dwarf_transitions) == 2
	assert int(MemoryFrameCfiRegion.prologue) == 0
	assert int(MemoryFrameCfiRegion.epilogue_template) == 1
	assert int(MemoryFrameCfiPhase.after_push) == 0
	assert int(MemoryFrameCfiPhase.after_allocation) == 1
	assert int(MemoryFrameCfiPhase.after_add) == 2
	assert int(MemoryFrameCfiPhase.after_pop) == 3
	assert int(MemoryFrameCfiOpKind.def_cfa_offset) == 0
	assert int(MemoryFrameCfiOpKind.offset_register) == 1

	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [])
	assert plan.disposition == .dwarf_zero_delta
	memory_frame_cfi_test_assert_active_baseline(&plan)
	assert plan.prologue_ops.len == 0
	assert plan.epilogue_template_ops.len == 0
	assert plan.frame.total_stack_extent_bytes == 0
}

fn test_memory_frame_cfi_c402_linux_red_zone_is_dwarf_active_zero_delta() {
	facts := memory_frame_cfi_test_facts(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_cfi_test_slot(1, 128, 8),
	])
	plan := memory_frame_cfi_test_plan(&facts, [])
	assert plan.disposition == .dwarf_zero_delta
	memory_frame_cfi_test_assert_active_baseline(&plan)
	assert plan.frame.layout_frame.uses_red_zone
	assert plan.frame.layout_frame.red_zone_extent_bytes == 128
	assert plan.frame.allocation_bytes == 0
	assert plan.prologue_ops.len == 0
	assert plan.epilogue_template_ops.len == 0
}

fn test_memory_frame_cfi_c403_apple_empty_and_red_zone_are_dwarf_active() {
	empty := memory_frame_cfi_test_empty(.macos_x86_64_sysv_macho)
	empty_plan := memory_frame_cfi_test_plan(&empty, [])
	assert empty_plan.disposition == .dwarf_zero_delta
	memory_frame_cfi_test_assert_active_baseline(&empty_plan)

	red_zone := memory_frame_cfi_test_facts(.macos_x86_64_sysv_macho, false, 0, [
		memory_frame_cfi_test_slot(2, 128, 8),
	])
	red_zone_plan := memory_frame_cfi_test_plan(&red_zone, [])
	assert red_zone_plan.disposition == .dwarf_zero_delta
	assert red_zone_plan.frame.layout_frame.uses_red_zone
	memory_frame_cfi_test_assert_active_baseline(&red_zone_plan)
}

fn test_memory_frame_cfi_c404_windows_empty_is_truly_none() {
	facts := memory_frame_cfi_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_cfi_test_plan(&facts, [])
	memory_frame_cfi_test_assert_windows_none(&plan)
	assert plan.frame.prologue_bytes.len == 0
	assert plan.frame.epilogue_bytes.len == 0
	assert !plan.frame.windows_unwind.present
}

fn test_memory_frame_cfi_c405_windows_delegates_all_m3_errors_before_none() {
	bad_facts := MemoryFunctionFrameFacts{
		function_id:       77
		profile:           .windows_x86_64_microsoft_abi_coff
		extent_kind:       .dynamic
		call_extent_bytes: 40
		has_call:          true
	}
	missing := MemoryCalleeSaveFacts{}
	memory_frame_cfi_test_expect_error(&bad_facts, &missing,
		'amd64 memory frame save: callee-save facts are required')
	valid_saves := memory_frame_cfi_test_saves([])
	memory_frame_cfi_test_expect_error(&bad_facts, &valid_saves,
		'amd64 memory frame: dynamic frame extent is unsupported')

	valid_facts := memory_frame_cfi_test_empty(.windows_x86_64_microsoft_abi_coff)
	mismatch := MemoryCalleeSaveFacts{
		present:     true
		function_id: 78
		registers:   [.rbx]
	}
	memory_frame_cfi_test_expect_error(&valid_facts, &mismatch,
		'amd64 memory frame save: callee-save function 78 does not match frame function 77')
}

fn test_memory_frame_cfi_c406_allocation_only_has_one_row_per_region() {
	facts := memory_frame_cfi_test_facts(.linux_x86_64_sysv_elf, true, 8, [])
	plan := memory_frame_cfi_test_plan(&facts, [])
	assert plan.disposition == .dwarf_transitions
	memory_frame_cfi_test_assert_active_baseline(&plan)
	assert plan.frame.allocation_bytes == 8
	assert plan.frame.total_stack_extent_bytes == 8
	assert plan.prologue_ops == [
		memory_frame_cfi_test_def(.prologue, .after_allocation, 4, 16),
	]
	assert plan.epilogue_template_ops == [
		memory_frame_cfi_test_def(.epilogue_template, .after_add, 4, 8),
	]
}

fn test_memory_frame_cfi_c407_rbx_push_and_pop_rows_are_exact() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [.rbx])
	assert plan.frame.allocation_bytes == 0
	assert plan.prologue_ops == [
		memory_frame_cfi_test_def(.prologue, .after_push, 1, 16),
		memory_frame_cfi_test_offset(1, 16, .rbx, 3),
	]
	assert plan.epilogue_template_ops == [
		memory_frame_cfi_test_def(.epilogue_template, .after_pop, 1, 8),
	]
}

fn test_memory_frame_cfi_c408_rbp_dwarf_six_is_independent_of_hardware_five() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [.rbp])
	assert plan.frame.saves[0].register_encoding == 5
	assert plan.prologue_ops[1] == memory_frame_cfi_test_offset(1, 16, .rbp, 6)
	assert plan.prologue_ops[1].dwarf_register_number !=
		plan.frame.saves[0].register_encoding
}

fn test_memory_frame_cfi_c409_apple_r12_uses_two_byte_pc_and_dwarf_twelve() {
	facts := memory_frame_cfi_test_empty(.macos_x86_64_sysv_macho)
	plan := memory_frame_cfi_test_plan(&facts, [.r12])
	assert plan.frame.saves[0].push_width_bytes == 2
	assert plan.frame.saves[0].pop_width_bytes == 2
	assert plan.prologue_ops == [
		memory_frame_cfi_test_def(.prologue, .after_push, 2, 16),
		memory_frame_cfi_test_offset(2, 16, .r12, 12),
	]
	assert plan.epilogue_template_ops == [
		memory_frame_cfi_test_def(.epilogue_template, .after_pop, 2, 8),
	]
}

fn test_memory_frame_cfi_c410_all_six_sysv_registers_reach_exact_active_caps() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [.r15, .r13, .rbx, .r14, .rbp, .r12])
	assert plan.frame.saves.map(it.register) == [MemorySavedGpr.rbx, .rbp, .r12, .r13,
		.r14, .r15]
	assert plan.frame.saves.map(it.register_encoding) == [u8(3), 5, 12, 13, 14, 15]
	assert plan.prologue_ops.filter(it.kind == .offset_register).map(it.dwarf_register_number) == [
		u8(3),
		6,
		12,
		13,
		14,
		15,
	]
	assert plan.frame.allocation_bytes == 8
	assert plan.prologue_ops.len == 13
	assert plan.epilogue_template_ops.len == 7
	assert plan.prologue_ops.len + plan.epilogue_template_ops.len == 20
}

fn test_memory_frame_cfi_c411_regions_phases_coordinates_and_same_pc_order_are_exact() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [.r12, .rbx])
	assert plan.prologue_ops == [
		memory_frame_cfi_test_def(.prologue, .after_push, 1, 16),
		memory_frame_cfi_test_offset(1, 16, .rbx, 3),
		memory_frame_cfi_test_def(.prologue, .after_push, 3, 24),
		memory_frame_cfi_test_offset(3, 24, .r12, 12),
		memory_frame_cfi_test_def(.prologue, .after_allocation, 7, 32),
	]
	assert plan.epilogue_template_ops == [
		memory_frame_cfi_test_def(.epilogue_template, .after_add, 4, 24),
		memory_frame_cfi_test_def(.epilogue_template, .after_pop, 6, 16),
		memory_frame_cfi_test_def(.epilogue_template, .after_pop, 7, 8),
	]
	for index := 1; index < plan.prologue_ops.len; index++ {
		left := plan.prologue_ops[index - 1]
		right := plan.prologue_ops[index]
		assert left.instruction_end_offset_bytes < right.instruction_end_offset_bytes
			|| (left.instruction_end_offset_bytes == right.instruction_end_offset_bytes
			&& left.same_pc_ordinal < right.same_pc_ordinal)
	}
}

fn test_memory_frame_cfi_c412_a128_boundary_uses_final_m3_coordinates() {
	facts := memory_frame_cfi_test_adjustment(.linux_x86_64_sysv_elf, 120)
	plan := memory_frame_cfi_test_plan(&facts, [.rbx])
	assert plan.frame.allocation_bytes == 128
	assert plan.frame.total_stack_extent_bytes == 136
	assert plan.frame.body_offset_bytes == 8
	assert plan.frame.body_cfa_offset_bytes == 144
	assert plan.prologue_ops.last() ==
		memory_frame_cfi_test_def(.prologue, .after_allocation, 8, 144)
	assert plan.epilogue_template_ops == [
		memory_frame_cfi_test_def(.epilogue_template, .after_add, 7, 16),
		memory_frame_cfi_test_def(.epilogue_template, .after_pop, 8, 8),
	]
}

fn test_memory_frame_cfi_c413_a4096_boundary_has_semantic_rows_without_reprobe() {
	facts := memory_frame_cfi_test_adjustment(.linux_x86_64_sysv_elf, 4088)
	plan := memory_frame_cfi_test_plan(&facts, [.rbx])
	assert plan.frame.allocation_bytes == 4096
	assert !plan.frame.probe_required
	assert plan.frame.body_offset_bytes == 8
	assert plan.frame.body_cfa_offset_bytes == 4112
	assert plan.prologue_ops.last().cfa_offset_bytes == 4112
	assert plan.epilogue_template_ops[0] ==
		memory_frame_cfi_test_def(.epilogue_template, .after_add, 7, 16)
}

fn test_memory_frame_cfi_c414_a524288_boundary_remains_checked_and_exact() {
	facts := memory_frame_cfi_test_adjustment(.macos_x86_64_sysv_macho, 524280)
	plan := memory_frame_cfi_test_plan(&facts, [.rbx])
	assert plan.frame.allocation_bytes == 524288
	assert plan.frame.total_stack_extent_bytes == 524296
	assert plan.frame.body_cfa_offset_bytes == 524304
	assert plan.prologue_ops.last() ==
		memory_frame_cfi_test_def(.prologue, .after_allocation, 8, 524304)
	assert plan.epilogue_template_ops[0].instruction_end_offset_bytes == 7
}

fn test_memory_frame_cfi_c415_every_reverse_pop_has_one_row_and_no_post_ret_row() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_test_plan(&facts, [.r15, .r14, .r13, .r12, .rbp, .rbx])
	assert plan.epilogue_template_ops.map(it.instruction_end_offset_bytes) == [
		u8(4),
		6,
		8,
		10,
		12,
		13,
		14,
	]
	assert plan.epilogue_template_ops.map(it.cfa_offset_bytes) == [u64(56), 48, 40,
		32, 24, 16, 8]
	assert plan.epilogue_template_ops.map(it.phase) == [
		MemoryFrameCfiPhase.after_add,
		.after_pop,
		.after_pop,
		.after_pop,
		.after_pop,
		.after_pop,
		.after_pop,
	]
	assert plan.epilogue_template_ops.filter(it.kind == .offset_register).len == 0
	assert plan.epilogue_template_ops.last().instruction_end_offset_bytes ==
		u8(plan.frame.epilogue_bytes.len)
}

fn test_memory_frame_cfi_c416_row_simulation_reaches_body_and_returns_to_baseline() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho] {
		facts := memory_frame_cfi_test_adjustment(profile, 120)
		plan := memory_frame_cfi_test_plan(&facts, [.r15, .r12, .rbp, .rbx])
		memory_frame_cfi_test_replay(&plan)
	}
}

fn test_memory_frame_cfi_c417_m0_slots_cannot_overwrite_private_save_words() {
	facts := memory_frame_cfi_test_facts(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_cfi_test_slot(2, 16, 8),
		memory_frame_cfi_test_slot(1, 8, 8),
	])
	plan := memory_frame_cfi_test_plan(&facts, [.r12, .rbx])
	assert plan.frame.layout_frame.red_zone_policy == .forbidden
	assert !plan.frame.layout_frame.uses_red_zone
	save_floor := -i64(plan.frame.saves.len * 8)
	for slot in plan.frame.slots {
		assert slot.placement.basis == .body_rsp
		start := i64(slot.address.displacement_bytes) -
			i64(plan.frame.total_stack_extent_bytes)
		end := start + i64(slot.placement.size_bytes)
		assert end <= save_floor
	}
	assert int(MemoryFrameCfiOpKind.offset_register) == 1
}

fn test_memory_frame_cfi_c418_m3_validation_errors_and_inputs_are_preserved() {
	facts := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	missing := MemoryCalleeSaveFacts{}
	memory_frame_cfi_test_expect_error(&facts, &missing,
		'amd64 memory frame save: callee-save facts are required')

	mut duplicate_registers := [MemorySavedGpr.rbx, .rbx]
	duplicate := memory_frame_cfi_test_saves(duplicate_registers)
	memory_frame_cfi_test_expect_error(&facts, &duplicate,
		'amd64 memory frame save: duplicate saved register rbx')
	rsi := memory_frame_cfi_test_saves([.rsi])
	memory_frame_cfi_test_expect_error(&facts, &rsi,
		'amd64 memory frame save: save 0 register rsi is not nonvolatile for target profile')

	windows := memory_frame_cfi_test_empty(.windows_x86_64_microsoft_abi_coff)
	too_many := memory_frame_cfi_test_saves([.rbx, .rbp, .rsi, .rdi, .r12, .r13,
		.r14, .r15, .rbx])
	memory_frame_cfi_test_expect_error(&windows, &too_many,
		'amd64 memory frame save: save count 9 exceeds 8')
	assert duplicate_registers == [MemorySavedGpr.rbx, .rbx]
	assert duplicate.registers == duplicate_registers
}

fn test_memory_frame_cfi_c419_exact_caps_and_windows_eight_save_none_are_pinned() {
	assert memory_frame_cfi_max_prologue_ops == 13
	assert memory_frame_cfi_max_epilogue_ops == 7
	assert memory_frame_cfi_max_total_ops == 20
	assert memory_frame_cfi_dwarf_code_alignment == 1
	assert memory_frame_cfi_dwarf_data_alignment == -8
	assert memory_frame_cfi_dwarf_rsp_register == 7
	assert memory_frame_cfi_dwarf_rip_register == 16

	sysv := memory_frame_cfi_test_empty(.linux_x86_64_sysv_elf)
	active := memory_frame_cfi_test_plan(&sysv, [.rbx, .rbp, .r12, .r13, .r14, .r15])
	assert active.prologue_ops.len == memory_frame_cfi_max_prologue_ops
	assert active.epilogue_template_ops.len == memory_frame_cfi_max_epilogue_ops
	assert active.prologue_ops.len + active.epilogue_template_ops.len ==
		memory_frame_cfi_max_total_ops

	windows := memory_frame_cfi_test_empty(.windows_x86_64_microsoft_abi_coff)
	windows_none_plan := memory_frame_cfi_test_plan(&windows, [.rbx, .rbp, .rsi,
		.rdi, .r12, .r13, .r14, .r15])
	memory_frame_cfi_test_assert_windows_none(&windows_none_plan)
	assert windows_none_plan.frame.saves.len == 8
	assert windows_none_plan.frame.windows_unwind.present
}

fn test_memory_frame_cfi_c420_determinism_transactionality_and_deep_nonaliasing() {
	mut input_slots := [
		memory_frame_cfi_test_slot(2, 16, 8),
		memory_frame_cfi_test_slot(1, 8, 8),
	]
	mut input_registers := [MemorySavedGpr.r12, .rbx]
	first_facts := memory_frame_cfi_test_facts(.linux_x86_64_sysv_elf, false, 0,
		input_slots)
	second_facts := memory_frame_cfi_test_facts(.linux_x86_64_sysv_elf, false, 0,
		input_slots)
	first_saves := memory_frame_cfi_test_saves(input_registers)
	second_saves := memory_frame_cfi_test_saves(input_registers)
	first := plan_memory_saved_frame_cfi(&first_facts, &first_saves) or { panic(err.msg()) }
	second := plan_memory_saved_frame_cfi(&second_facts, &second_saves) or {
		panic(err.msg())
	}
	assert first == second
	unsafe {
		assert first.prologue_ops.data != second.prologue_ops.data
		assert first.epilogue_template_ops.data != second.epilogue_template_ops.data
		assert first.frame.prologue_bytes.data != second.frame.prologue_bytes.data
		assert first.frame.epilogue_bytes.data != second.frame.epilogue_bytes.data
		assert first.frame.save_facts.registers.data != second.frame.save_facts.registers.data
		assert first.frame.saves.data != second.frame.saves.data
		assert first.frame.layout_frame.slots.data != second.frame.layout_frame.slots.data
		assert first.frame.slots.data != second.frame.slots.data
	}

	input_slots[0] = memory_frame_cfi_test_slot(999, 1, 1)
	input_registers[0] = .r15
	assert second.frame.layout_frame.slots.map(it.id) == [u32(1), 2]
	assert second.frame.save_facts.registers == [MemorySavedGpr.r12, .rbx]
	unsafe {
		mut prologue_op := &MemoryFrameCfiOp(first.prologue_ops.data)
		mut epilogue_op := &MemoryFrameCfiOp(first.epilogue_template_ops.data)
		mut prologue_byte := &u8(first.frame.prologue_bytes.data)
		prologue_op[0] = MemoryFrameCfiOp{}
		epilogue_op[0] = MemoryFrameCfiOp{}
		prologue_byte[0] = 0
	}
	assert second.prologue_ops[0] ==
		memory_frame_cfi_test_def(.prologue, .after_push, 1, 16)
	assert second.epilogue_template_ops[0] ==
		memory_frame_cfi_test_def(.epilogue_template, .after_add, 4, 24)
	assert second.frame.prologue_bytes[0] == 0x53
}
