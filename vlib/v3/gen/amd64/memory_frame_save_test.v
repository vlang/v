module amd64

fn memory_frame_save_test_slot(id u32, size u64, alignment u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              id
		kind:            .local
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn memory_frame_save_test_facts(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       77
		profile:           profile
		extent_kind:       .fixed
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_save_test_empty(profile TargetProfile) MemoryFunctionFrameFacts {
	return memory_frame_save_test_facts(profile, false, 0, [])
}

fn memory_frame_save_test_adjustment(profile TargetProfile, adjustment u64) MemoryFunctionFrameFacts {
	return memory_frame_save_test_facts(profile, false, 0, [
		memory_frame_save_test_slot(1, adjustment, 1),
	])
}

fn memory_frame_save_test_facts_for_d(profile TargetProfile, adjustment u64) MemoryFunctionFrameFacts {
	if adjustment == 0 {
		return memory_frame_save_test_empty(profile)
	}
	return memory_frame_save_test_adjustment(profile, adjustment)
}

fn memory_frame_save_test_saves(registers []MemorySavedGpr) MemoryCalleeSaveFacts {
	return MemoryCalleeSaveFacts{
		present:     true
		function_id: 77
		registers:   registers.clone()
	}
}

fn memory_frame_save_test_plan(facts &MemoryFunctionFrameFacts, registers []MemorySavedGpr) MemorySavedFramePlan {
	saves := memory_frame_save_test_saves(registers)
	return plan_memory_saved_frame(facts, &saves) or { panic(err.msg()) }
}

fn memory_frame_save_test_expect_error(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts, expected string) {
	if _ := plan_memory_saved_frame(facts, saves) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected
	}
}

fn memory_frame_save_test_assert_absent_probe(plan &MemorySavedFramePlan) {
	assert !plan.probe_required
	assert !plan.probe_fixup.present
	assert plan.probe_fixup.kind == .none
	assert plan.probe_fixup.target_name == ''
	assert plan.probe_fixup.opcode_offset_bytes == 0
	assert plan.probe_fixup.field_offset_bytes == 0
	assert plan.probe_fixup.width_bytes == 0
	assert plan.probe_fixup.addend == 0
	assert !plan.chkstk.present
	assert plan.chkstk.allocation_bytes == 0
	assert plan.chkstk.save_push_count == 0
}

fn memory_frame_save_test_assert_probe(plan &MemorySavedFramePlan, allocation u64, push_count u8, opcode_offset u8, field_offset u8) {
	assert plan.probe_required
	assert plan.allocation_prologue_kind == .windows_chkstk
	assert plan.probe_fixup == MemoryFrameRel32Fixup{
		present:             true
		kind:                .call_rel32_next_instruction
		target_name:         '__chkstk'
		opcode_offset_bytes: opcode_offset
		field_offset_bytes:  field_offset
		width_bytes:         4
		addend:              0
	}
	even_push_count := push_count % 2 == 0
	assert plan.chkstk == MemoryChkstkContract{
		present:                      true
		allocation_bytes:             allocation
		save_push_count:              push_count
		shadow_space_bytes:           0
		pre_call_rsp_mod_16:          if even_push_count { u8(8) } else { u8(0) }
		helper_entry_rsp_mod_16:      if even_push_count { u8(0) } else { u8(8) }
		eax_zero_extends_rax:         true
		helper_preserves_rax:         true
		rax_after_prologue:           allocation
		clobbers_r10:                 true
		clobbers_r11:                 true
		clobbers_eflags:              true
		preserves_other_integer_gprs: true
	}
	assert plan.prologue_bytes[opcode_offset] == 0xe8
	assert plan.prologue_bytes[field_offset..field_offset + 4] == [u8(0), 0, 0, 0]
}

fn memory_frame_save_test_assert_unwind(plan &MemorySavedFramePlan, kind MemoryWindowsUnwindKind, allocation u64, count u8, expected []u8) {
	assert plan.windows_unwind.present
	assert plan.windows_unwind.allocation_kind == kind
	assert plan.windows_unwind.allocation_code_present == (allocation != 0)
	assert plan.windows_unwind.allocation_bytes == allocation
	assert plan.windows_unwind.size_of_prolog_bytes == u8(plan.prologue_bytes.len)
	assert plan.windows_unwind.allocation_code_offset_bytes == if allocation == 0 {
		u8(0)
	} else {
		u8(plan.prologue_bytes.len)
	}
	assert plan.windows_unwind.count_of_codes == count
	assert plan.windows_unwind.xdata_bytes == expected
}

fn memory_frame_save_test_assert_empty_legacy(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) {
	facts := memory_frame_save_test_facts(profile, has_call, call_extent, slots)
	legacy := plan_memory_frame_encoding(&facts) or { panic(err.msg()) }
	plan := memory_frame_save_test_plan(&facts, [])
	assert plan.layout_frame == legacy.frame
	assert plan.save_facts == memory_frame_save_test_saves([])
	assert plan.saves.len == 0
	assert plan.save_push_count == 0
	assert plan.base_allocation_bytes == legacy.frame.stack_adjustment_bytes
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == legacy.frame.stack_adjustment_bytes
	assert plan.total_stack_extent_bytes == legacy.frame.stack_adjustment_bytes
	assert plan.translations == legacy.frame.translations
	assert plan.allocation_prologue_kind == legacy.prologue_kind
	assert plan.prologue_bytes == legacy.prologue_bytes
	assert plan.epilogue_bytes == legacy.epilogue_bytes
	assert plan.body_offset_bytes == legacy.body_offset_bytes
	assert plan.entry_cfa_offset_bytes == legacy.entry_cfa_offset_bytes
	assert plan.body_cfa_offset_bytes == legacy.body_cfa_offset_bytes
	assert plan.probe_fixup == legacy.probe_fixup
	assert plan.chkstk == legacy.chkstk
	assert plan.slots == legacy.slots
	assert plan.windows_unwind.present == legacy.windows_unwind.present
	assert plan.windows_unwind.allocation_kind == legacy.windows_unwind.kind
	assert plan.windows_unwind.allocation_bytes == legacy.windows_unwind.allocation_bytes
	assert plan.windows_unwind.size_of_prolog_bytes ==
		legacy.windows_unwind.size_of_prolog_bytes
	assert plan.windows_unwind.allocation_code_offset_bytes ==
		legacy.windows_unwind.allocation_code_offset_bytes
	assert plan.windows_unwind.count_of_codes == legacy.windows_unwind.count_of_codes
	assert plan.windows_unwind.xdata_bytes == legacy.windows_unwind.xdata_bytes
}

fn test_memory_frame_save_m301_empty_save_sidecar_is_exact_legacy_identity() {
	memory_frame_save_test_assert_empty_legacy(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_save_test_slot(9, 8, 8),
	])
	memory_frame_save_test_assert_empty_legacy(.macos_x86_64_sysv_macho, true, 8,
		[])
	memory_frame_save_test_assert_empty_legacy(.windows_x86_64_microsoft_abi_coff,
		true, 40, [])
}

fn test_memory_frame_save_m302_sidecar_presence_and_function_binding_are_mandatory() {
	facts := memory_frame_save_test_empty(.linux_x86_64_sysv_elf)
	missing := MemoryCalleeSaveFacts{}
	memory_frame_save_test_expect_error(&facts, &missing,
		'amd64 memory frame save: callee-save facts are required')
	mismatch := MemoryCalleeSaveFacts{
		present:     true
		function_id: 78
		registers:   [.rbx]
	}
	memory_frame_save_test_expect_error(&facts, &mismatch,
		'amd64 memory frame save: callee-save function 78 does not match frame function 77')
}

fn test_memory_frame_save_m303_register_ordinals_encodings_and_profile_sets_are_exact() {
	assert int(MemorySavedGpr.rbx) == 0
	assert int(MemorySavedGpr.rbp) == 1
	assert int(MemorySavedGpr.rsi) == 2
	assert int(MemorySavedGpr.rdi) == 3
	assert int(MemorySavedGpr.r12) == 4
	assert int(MemorySavedGpr.r13) == 5
	assert int(MemorySavedGpr.r14) == 6
	assert int(MemorySavedGpr.r15) == 7
	facts := memory_frame_save_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_save_test_plan(&facts, [.r15, .r14, .r13, .r12, .rdi, .rsi,
		.rbp, .rbx])
	assert plan.saves.map(it.register_encoding) == [u8(3), 5, 6, 7, 12, 13, 14, 15]
	assert plan.saves.map(it.register) == [MemorySavedGpr.rbx, .rbp, .rsi, .rdi, .r12,
		.r13, .r14, .r15]
}

fn test_memory_frame_save_m304_canonical_order_source_indices_duplicates_and_save_cap() {
	facts := memory_frame_save_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_save_test_plan(&facts, [.r15, .rbx, .r12, .rbp])
	assert plan.saves.map(it.register) == [MemorySavedGpr.rbx, .rbp, .r12, .r15]
	assert plan.saves.map(it.source_request_index) == [u32(1), 3, 2, 0]

	duplicate := memory_frame_save_test_saves([.rbx, .rbx])
	memory_frame_save_test_expect_error(&facts, &duplicate,
		'amd64 memory frame save: duplicate saved register rbx')
	too_many := memory_frame_save_test_saves([.rbx, .rbp, .rsi, .rdi, .r12, .r13,
		.r14, .r15, .rbx])
	memory_frame_save_test_expect_error(&facts, &too_many,
		'amd64 memory frame save: save count 9 exceeds 8')
}

fn test_memory_frame_save_m305_sysv_one_push_has_q0_a0_t8() {
	facts := memory_frame_save_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.layout_frame.red_zone_policy == .forbidden
	assert !plan.layout_frame.uses_red_zone
	assert plan.save_push_count == 1
	assert plan.base_allocation_bytes == 0
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 0
	assert plan.total_stack_extent_bytes == 8
	assert plan.translations == MemoryStackTranslations{
		entry_to_body_subtract_bytes: 8
		incoming_from_body_add_bytes: 8
		outgoing_from_body_add_bytes: 0
	}
	assert plan.prologue_bytes == [u8(0x53)]
	assert plan.epilogue_bytes == [u8(0x5b)]
	assert plan.body_offset_bytes == 1
	assert plan.body_cfa_offset_bytes == 16
	assert plan.allocation_prologue_kind == .none
	memory_frame_save_test_assert_absent_probe(&plan)
	assert !plan.windows_unwind.present
}

fn test_memory_frame_save_m306_sysv_two_pushes_fold_q8_into_single_a8() {
	facts := memory_frame_save_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_save_test_plan(&facts, [.r12, .rbx])
	assert plan.save_push_count == 2
	assert plan.base_allocation_bytes == 0
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 8
	assert plan.total_stack_extent_bytes == 24
	assert plan.prologue_bytes == [u8(0x53), 0x41, 0x54, 0x48, 0x83, 0xec, 0x08]
	assert plan.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x08, 0x41, 0x5c, 0x5b]
	assert plan.allocation_prologue_kind == .sub_imm8
	assert plan.body_cfa_offset_bytes == 32
	assert !plan.windows_unwind.present
}

fn test_memory_frame_save_m307_nonempty_save_forbids_red_zone_without_forging_call_facts() {
	facts := memory_frame_save_test_facts(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_save_test_slot(4, 8, 8),
	])
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert !plan.layout_frame.has_call
	assert plan.layout_frame.call_extent_bytes == 0
	assert plan.layout_frame.red_zone_policy == .forbidden
	assert !plan.layout_frame.uses_red_zone
	assert plan.layout_frame.stack_adjustment_bytes == 8
	assert plan.layout_frame.slots == [
		MemorySlotPlacement{
			id:                 4
			kind:               .local
			basis:              .body_rsp
			displacement_bytes: 0
			size_bytes:         8
			alignment_bytes:    8
		},
	]
	assert plan.base_allocation_bytes == 8
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 16
	assert plan.total_stack_extent_bytes == 24
	assert plan.prologue_bytes == [u8(0x53), 0x48, 0x83, 0xec, 0x10]
	assert plan.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x10, 0x5b]
}

fn test_memory_frame_save_m308_apple_three_pushes_have_q0_a0_t24() {
	facts := memory_frame_save_test_empty(.macos_x86_64_sysv_macho)
	plan := memory_frame_save_test_plan(&facts, [.r15, .rbx, .r12])
	assert plan.saves.map(it.register) == [MemorySavedGpr.rbx, .r12, .r15]
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 0
	assert plan.total_stack_extent_bytes == 24
	assert plan.prologue_bytes == [u8(0x53), 0x41, 0x54, 0x41, 0x57]
	assert plan.epilogue_bytes == [u8(0x41), 0x5f, 0x41, 0x5c, 0x5b]
	assert !plan.windows_unwind.present
}

fn test_memory_frame_save_m309_windows_one_push_has_push_only_unwind() {
	facts := memory_frame_save_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.base_allocation_bytes == 0
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 0
	assert plan.total_stack_extent_bytes == 8
	assert plan.prologue_bytes == [u8(0x53)]
	assert plan.epilogue_bytes == [u8(0x5b)]
	memory_frame_save_test_assert_absent_probe(&plan)
	memory_frame_save_test_assert_unwind(&plan, .none, 0, 1, [u8(0x01), 0x01,
		0x01, 0x00, 0x01, 0x30, 0x00, 0x00])
	assert plan.saves[0].windows_unwind_present
	assert plan.saves[0].windows_unwind_code_offset_bytes == 1
}

fn test_memory_frame_save_m310_windows_two_pushes_fold_one_a8_and_full_unwind() {
	facts := memory_frame_save_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_save_test_plan(&facts, [.r12, .rbx])
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 8
	assert plan.total_stack_extent_bytes == 24
	assert plan.prologue_bytes == [u8(0x53), 0x41, 0x54, 0x48, 0x83, 0xec, 0x08]
	assert plan.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x08, 0x41, 0x5c, 0x5b]
	memory_frame_save_test_assert_unwind(&plan, .alloc_small, 8, 3, [u8(0x01),
		0x07, 0x03, 0x00, 0x07, 0x02, 0x03, 0xc0, 0x01, 0x30, 0x00, 0x00])
}

fn test_memory_frame_save_m311_windows_d40_p1_has_q8_a48_t56() {
	facts := memory_frame_save_test_facts(.windows_x86_64_microsoft_abi_coff,
		true, 40, [])
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.base_allocation_bytes == 40
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 48
	assert plan.total_stack_extent_bytes == 56
	assert plan.prologue_bytes == [u8(0x53), 0x48, 0x83, 0xec, 0x30]
	assert plan.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x30, 0x5b]
	memory_frame_save_test_assert_unwind(&plan, .alloc_small, 48, 2, [u8(0x01),
		0x05, 0x02, 0x00, 0x05, 0x52, 0x01, 0x30])
}

fn test_memory_frame_save_m312_windows_d40_p2_has_q0_one_a40_and_one_add() {
	facts := memory_frame_save_test_facts(.windows_x86_64_microsoft_abi_coff,
		true, 40, [])
	plan := memory_frame_save_test_plan(&facts, [.rbp, .rbx])
	assert plan.base_allocation_bytes == 40
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 40
	assert plan.total_stack_extent_bytes == 56
	assert plan.prologue_bytes == [u8(0x53), 0x55, 0x48, 0x83, 0xec, 0x28]
	assert plan.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x28, 0x5d, 0x5b]
	assert plan.epilogue_bytes.filter(it == 0xc4).len == 1
	memory_frame_save_test_assert_unwind(&plan, .alloc_small, 40, 3, [u8(0x01),
		0x06, 0x03, 0x00, 0x06, 0x42, 0x02, 0x50, 0x01, 0x30, 0x00, 0x00])
}

fn test_memory_frame_save_m313_a128_is_last_small_allocation() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		120)
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.base_allocation_bytes == 120
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 128
	assert plan.total_stack_extent_bytes == 136
	assert plan.allocation_prologue_kind == .sub_imm32
	assert plan.prologue_bytes == [u8(0x53), 0x48, 0x81, 0xec, 0x80, 0x00, 0x00,
		0x00]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x80, 0x00, 0x00, 0x00,
		0x5b]
	memory_frame_save_test_assert_absent_probe(&plan)
	memory_frame_save_test_assert_unwind(&plan, .alloc_small, 128, 2, [u8(0x01),
		0x08, 0x02, 0x00, 0x08, 0xf2, 0x01, 0x30])
}

fn test_memory_frame_save_m314_a4096_is_first_probe_with_odd_push_residue() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		4088)
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.base_allocation_bytes == 4088
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 4096
	assert plan.total_stack_extent_bytes == 4104
	assert plan.prologue_bytes == [u8(0x53), 0xb8, 0x00, 0x10, 0x00, 0x00, 0xe8,
		0x00, 0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x00, 0x10, 0x00, 0x00,
		0x5b]
	memory_frame_save_test_assert_probe(&plan, 4096, 1, 6, 7)
	assert plan.chkstk.pre_call_rsp_mod_16 == 0
	assert plan.chkstk.helper_entry_rsp_mod_16 == 8
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info0, 4096, 3,
		[u8(0x01), 0x0e, 0x03, 0x00, 0x0e, 0x01, 0x00, 0x02, 0x01, 0x30, 0x00,
			0x00])
}

fn test_memory_frame_save_m315_even_push_probe_offsets_and_one_add_are_exact() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		4104)
	plan := memory_frame_save_test_plan(&facts, [.rbp, .rbx])
	assert plan.base_allocation_bytes == 4104
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 4104
	assert plan.total_stack_extent_bytes == 4120
	assert plan.prologue_bytes == [u8(0x53), 0x55, 0xb8, 0x08, 0x10, 0x00, 0x00,
		0xe8, 0x00, 0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x08, 0x10, 0x00, 0x00,
		0x5d, 0x5b]
	assert plan.epilogue_bytes.filter(it == 0xc4).len == 1
	memory_frame_save_test_assert_probe(&plan, 4104, 2, 7, 8)
	assert plan.chkstk.pre_call_rsp_mod_16 == 8
	assert plan.chkstk.helper_entry_rsp_mod_16 == 0
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info0, 4104, 4,
		[u8(0x01), 0x0f, 0x04, 0x00, 0x0f, 0x01, 0x01, 0x02, 0x02, 0x50, 0x01,
			0x30])
}

fn test_memory_frame_save_m316_a524280_is_last_large_info0() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		524280)
	plan := memory_frame_save_test_plan(&facts, [.rbp, .rbx])
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 524280
	assert plan.total_stack_extent_bytes == 524296
	assert plan.prologue_bytes[2..7] == [u8(0xb8), 0xf8, 0xff, 0x07, 0x00]
	memory_frame_save_test_assert_probe(&plan, 524280, 2, 7, 8)
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info0, 524280, 4,
		[u8(0x01), 0x0f, 0x04, 0x00, 0x0f, 0x01, 0xff, 0xff, 0x02, 0x50, 0x01,
			0x30])
}

fn test_memory_frame_save_m317_a524288_is_first_large_info1() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		524280)
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 524288
	assert plan.total_stack_extent_bytes == 524296
	assert plan.prologue_bytes[1..6] == [u8(0xb8), 0x00, 0x00, 0x08, 0x00]
	memory_frame_save_test_assert_probe(&plan, 524288, 1, 6, 7)
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info1, 524288, 4,
		[u8(0x01), 0x0e, 0x04, 0x00, 0x0e, 0x11, 0x00, 0x00, 0x08, 0x00, 0x01,
			0x30])
}

fn test_memory_frame_save_m318_maximum_allocation_is_exact_without_overflow() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		u64(0x7ffffff8))
	plan := memory_frame_save_test_plan(&facts, [.rbp, .rbx])
	assert plan.base_allocation_bytes == u64(0x7ffffff8)
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == u64(0x7ffffff8)
	assert plan.total_stack_extent_bytes == u64(0x80000008)
	assert plan.body_cfa_offset_bytes == u64(0x80000010)
	assert plan.prologue_bytes[2..7] == [u8(0xb8), 0xf8, 0xff, 0xff, 0x7f]
	assert plan.epilogue_bytes[0..7] == [u8(0x48), 0x81, 0xc4, 0xf8, 0xff, 0xff,
		0x7f]
	memory_frame_save_test_assert_probe(&plan, u64(0x7ffffff8), 2, 7, 8)
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info1, u64(0x7ffffff8),
		5, [u8(0x01), 0x0f, 0x05, 0x00, 0x0f, 0x11, 0xf8, 0xff, 0xff, 0x7f, 0x02,
		0x50, 0x01, 0x30, 0x00, 0x00])
}

fn test_memory_frame_save_m319_maximum_plus_required_padding_refuses_transactionally() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		u64(0x7ffffff8))
	mut registers := [MemorySavedGpr.rbx]
	saves := memory_frame_save_test_saves(registers)
	memory_frame_save_test_expect_error(&facts, &saves,
		'amd64 memory frame save: allocation 2147483648 exceeds 2147483640')
	assert registers == [MemorySavedGpr.rbx]
	assert facts.slots[0].size_bytes == u64(0x7ffffff8)
}

fn test_memory_frame_save_m320_all_windows_saves_reach_exact_output_caps() {
	facts := memory_frame_save_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		524296)
	registers := [MemorySavedGpr.r15, .rdi, .r13, .rbx, .rsi, .r14, .rbp, .r12]
	plan := memory_frame_save_test_plan(&facts, registers)
	assert plan.saves.map(it.register) == [MemorySavedGpr.rbx, .rbp, .rsi, .rdi, .r12,
		.r13, .r14, .r15]
	assert plan.save_push_count == 8
	assert plan.padding_bytes == 0
	assert plan.allocation_bytes == 524296
	assert plan.total_stack_extent_bytes == 524360
	assert plan.prologue_bytes == [u8(0x53), 0x55, 0x56, 0x57, 0x41, 0x54, 0x41,
		0x55, 0x41, 0x56, 0x41, 0x57, 0xb8, 0x08, 0x00, 0x08, 0x00, 0xe8, 0x00,
		0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x08, 0x00, 0x08, 0x00,
		0x41, 0x5f, 0x41, 0x5e, 0x41, 0x5d, 0x41, 0x5c, 0x5f, 0x5e, 0x5d, 0x5b]
	assert plan.prologue_bytes.len == memory_saved_frame_max_prologue_bytes
	assert plan.epilogue_bytes.len == memory_saved_frame_max_epilogue_bytes
	memory_frame_save_test_assert_probe(&plan, 524296, 8, 17, 18)
	memory_frame_save_test_assert_unwind(&plan, .alloc_large_info1, 524296, 11,
		[u8(0x01), 0x19, 0x0b, 0x00, 0x19, 0x11, 0x08, 0x00, 0x08, 0x00, 0x0c,
			0xf0, 0x0a, 0xe0, 0x08, 0xd0, 0x06, 0xc0, 0x04, 0x70, 0x03, 0x60, 0x02,
			0x50, 0x01, 0x30, 0x00, 0x00])
	assert plan.windows_unwind.xdata_bytes.len == memory_saved_frame_max_xdata_bytes
}

fn test_memory_frame_save_m321_sysv_and_apple_accept_exact_six_register_set() {
	registers := [MemorySavedGpr.r15, .r13, .rbx, .r14, .rbp, .r12]
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho] {
		facts := memory_frame_save_test_empty(profile)
		plan := memory_frame_save_test_plan(&facts, registers)
		assert plan.saves.map(it.register) == [MemorySavedGpr.rbx, .rbp, .r12, .r13,
			.r14, .r15]
		assert plan.save_push_count == 6
		assert plan.padding_bytes == 8
		assert plan.allocation_bytes == 8
		assert plan.total_stack_extent_bytes == 56
		assert plan.prologue_bytes.len == 14
		assert plan.epilogue_bytes.len == 14
		assert !plan.windows_unwind.present
	}
	sysv := memory_frame_save_test_empty(.linux_x86_64_sysv_elf)
	bad_sysv := memory_frame_save_test_saves([.rsi])
	memory_frame_save_test_expect_error(&sysv, &bad_sysv,
		'amd64 memory frame save: save 0 register rsi is not nonvolatile for target profile')
	apple := memory_frame_save_test_empty(.macos_x86_64_sysv_macho)
	bad_apple := memory_frame_save_test_saves([.rdi])
	memory_frame_save_test_expect_error(&apple, &bad_apple,
		'amd64 memory frame save: save 0 register rdi is not nonvolatile for target profile')
}

fn test_memory_frame_save_m322_m0_slots_are_consumed_once_with_final_translation() {
	facts := memory_frame_save_test_facts(.windows_x86_64_microsoft_abi_coff,
		true, 40, [
		memory_frame_save_test_slot(2, 8, 8),
		memory_frame_save_test_slot(1, 8, 8),
	])
	plan := memory_frame_save_test_plan(&facts, [.rbx])
	assert plan.layout_frame.stack_adjustment_bytes == 56
	assert plan.base_allocation_bytes == 56
	assert plan.padding_bytes == 8
	assert plan.allocation_bytes == 64
	assert plan.total_stack_extent_bytes == 72
	assert plan.translations == MemoryStackTranslations{
		entry_to_body_subtract_bytes: 72
		incoming_from_body_add_bytes: 72
		outgoing_from_body_add_bytes: 0
	}
	assert plan.slots.len == plan.layout_frame.slots.len
	for index, encoded in plan.slots {
		assert encoded.source_placement_index == u32(index)
		assert encoded.placement == plan.layout_frame.slots[index]
		assert encoded.address.basis == .body_rsp
		assert encoded.address.displacement_bytes == i32(40 + index * 8)
	}
	assert plan.slots.map(it.placement.id) == [u32(1), 2]
}

fn test_memory_frame_save_m323_validation_precedence_and_inputs_are_stable() {
	bad_register := unsafe { MemorySavedGpr(255) }
	bad_facts := MemoryFunctionFrameFacts{
		function_id:       77
		profile:           .linux_x86_64_sysv_elf
		extent_kind:       .dynamic
		call_extent_bytes: 8
		has_call:          false
	}
	missing := MemoryCalleeSaveFacts{}
	memory_frame_save_test_expect_error(&bad_facts, &missing,
		'amd64 memory frame save: callee-save facts are required')

	too_many := memory_frame_save_test_saves([.rbx, .rbp, .rsi, .rdi, .r12, .r13,
		.r14, .r15, .rbx])
	memory_frame_save_test_expect_error(&bad_facts, &too_many,
		'amd64 memory frame save: save count 9 exceeds 8')

	valid_saves := memory_frame_save_test_saves([.rbx])
	memory_frame_save_test_expect_error(&bad_facts, &valid_saves,
		'amd64 memory frame: dynamic frame extent is unsupported')

	valid_facts := memory_frame_save_test_empty(.windows_x86_64_microsoft_abi_coff)
	mismatch_and_bad := MemoryCalleeSaveFacts{
		present:     true
		function_id: 78
		registers:   [bad_register]
	}
	memory_frame_save_test_expect_error(&valid_facts, &mismatch_and_bad,
		'amd64 memory frame save: callee-save function 78 does not match frame function 77')
	bad_only := memory_frame_save_test_saves([bad_register])
	memory_frame_save_test_expect_error(&valid_facts, &bad_only,
		'amd64 memory frame save: save 0 has unsupported register')

	sysv := memory_frame_save_test_empty(.linux_x86_64_sysv_elf)
	profile_before_duplicate := memory_frame_save_test_saves([.rsi, .rsi])
	memory_frame_save_test_expect_error(&sysv, &profile_before_duplicate,
		'amd64 memory frame save: save 0 register rsi is not nonvolatile for target profile')
	duplicate := memory_frame_save_test_saves([.rbx, .rbx])
	memory_frame_save_test_expect_error(&sysv, &duplicate,
		'amd64 memory frame save: duplicate saved register rbx')
	assert duplicate.registers == [MemorySavedGpr.rbx, .rbx]
	assert valid_facts.slots.len == 0
}

fn test_memory_frame_save_m324_caps_determinism_and_deep_nonaliasing() {
	assert memory_saved_frame_max_saves == 8
	assert memory_saved_frame_max_prologue_bytes == 25
	assert memory_saved_frame_max_epilogue_bytes == 19
	assert memory_saved_frame_max_unwind_slots == 11
	assert memory_saved_frame_max_xdata_bytes == 28
	assert memory_saved_frame_max_fixups == 1
	assert memory_saved_frame_max_displacement_bytes == 4096
	assert memory_saved_frame_max_byte_slice_payload == 4168
	assert memory_saved_frame_max_allocation_bytes == u64(0x7ffffff8)
	assert memory_saved_frame_probe_threshold_bytes == 4096

	mut input_slots := []MemorySlotRequest{cap: 1024}
	for index in 0 .. 1024 {
		input_slots << memory_frame_save_test_slot(u32(index), 1, 1)
	}
	mut input_registers := [MemorySavedGpr.r15, .rdi, .r13, .rbx, .rsi, .r14, .rbp,
		.r12]
	first_facts := memory_frame_save_test_facts(.windows_x86_64_microsoft_abi_coff,
		true, 524296, input_slots)
	second_facts := memory_frame_save_test_facts(.windows_x86_64_microsoft_abi_coff,
		true, 524296, input_slots)
	first_saves := memory_frame_save_test_saves(input_registers)
	second_saves := memory_frame_save_test_saves(input_registers)
	first := plan_memory_saved_frame(&first_facts, &first_saves) or { panic(err.msg()) }
	second := plan_memory_saved_frame(&second_facts, &second_saves) or { panic(err.msg()) }
	assert first == second
	assert first.slots.len == 1024
	mut displacement_bytes := 0
	for encoded in first.slots {
		assert encoded.address.kind == .disp32
		displacement_bytes += encoded.address.displacement_le.len
	}
	assert displacement_bytes == 4096
	assert displacement_bytes + first.prologue_bytes.len + first.epilogue_bytes.len +
		first.windows_unwind.xdata_bytes.len == 4168

	input_slots[0] = memory_frame_save_test_slot(9999, 8, 8)
	input_registers[0] = .rbx
	assert first == second
	assert first.layout_frame.slots[0].id == 0
	assert first.save_facts.registers[0] == .r15
	unsafe {
		assert first.prologue_bytes.data != second.prologue_bytes.data
		assert first.epilogue_bytes.data != second.epilogue_bytes.data
		assert first.windows_unwind.xdata_bytes.data != second.windows_unwind.xdata_bytes.data
		assert first.save_facts.registers.data != second.save_facts.registers.data
		assert first.saves.data != second.saves.data
		assert first.layout_frame.slots.data != second.layout_frame.slots.data
		assert first.slots.data != second.slots.data
		assert first.slots[0].address.displacement_le.data !=
			second.slots[0].address.displacement_le.data
	}
	unsafe {
		mut prologue := &u8(first.prologue_bytes.data)
		mut epilogue := &u8(first.epilogue_bytes.data)
		mut xdata := &u8(first.windows_unwind.xdata_bytes.data)
		mut displacement := &u8(first.slots[0].address.displacement_le.data)
		prologue[0] = 0
		epilogue[0] = 0
		xdata[0] = 0
		displacement[0] = 0
	}
	assert second.prologue_bytes[0] == 0x53
	assert second.epilogue_bytes[0] == 0x48
	assert second.windows_unwind.xdata_bytes[0] == 0x01
	assert second.slots[0].address.displacement_le[0] == 0x08
}
