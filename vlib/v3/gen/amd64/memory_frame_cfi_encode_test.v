module amd64

fn memory_frame_cfi_encode_test_slot(id u32, size u64, alignment u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              id
		kind:            .local
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn memory_frame_cfi_encode_test_facts(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       77
		profile:           profile
		extent_kind:       .fixed
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_cfi_encode_test_empty(profile TargetProfile) MemoryFunctionFrameFacts {
	return memory_frame_cfi_encode_test_facts(profile, false, 0, [])
}

fn memory_frame_cfi_encode_test_adjustment(profile TargetProfile, adjustment u64) MemoryFunctionFrameFacts {
	if adjustment == 0 {
		return memory_frame_cfi_encode_test_empty(profile)
	}
	return memory_frame_cfi_encode_test_facts(profile, false, 0, [
		memory_frame_cfi_encode_test_slot(1, adjustment, 1),
	])
}

fn memory_frame_cfi_encode_test_saves(registers []MemorySavedGpr) MemoryCalleeSaveFacts {
	return MemoryCalleeSaveFacts{
		present:     true
		function_id: 77
		registers:   registers.clone()
	}
}

fn memory_frame_cfi_encode_test_plan(facts &MemoryFunctionFrameFacts, registers []MemorySavedGpr) MemoryFrameCfiInstructionFragmentPlan {
	saves := memory_frame_cfi_encode_test_saves(registers)
	return plan_memory_saved_frame_cfi_instruction_fragments(facts, &saves) or {
		panic(err.msg())
	}
}

fn memory_frame_cfi_encode_test_expect_error(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts, expected string) {
	if _ := plan_memory_saved_frame_cfi_instruction_fragments(facts, saves) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected
	}
}

fn memory_frame_cfi_encode_test_expect_private_error(run fn () !, expected string) {
	run() or {
		assert err.msg() == expected
		return
	}
	assert false, 'expected `${expected}`'
}

fn memory_frame_cfi_encode_test_prologue_bytes(plan &MemoryFrameCfiInstructionFragmentPlan) []u8 {
	return plan.prologue_fde_instruction_fragment.region_relative_fde_instruction_fragment_bytes
}

fn memory_frame_cfi_encode_test_epilogue_bytes(plan &MemoryFrameCfiInstructionFragmentPlan) []u8 {
	return plan.epilogue_fde_instruction_template.region_relative_fde_instruction_fragment_bytes
}

fn memory_frame_cfi_encode_test_uleb128(value u64) []u8 {
	mut bytes := []u8{}
	memory_frame_cfi_encode_append_uleb128(mut bytes, value)
	return bytes
}

fn memory_frame_cfi_encode_test_assert_active_fragments(plan &MemoryFrameCfiInstructionFragmentPlan) {
	assert plan.cfi.disposition in [.dwarf_zero_delta, .dwarf_transitions]
	assert plan.cie_initial_instruction_bytes == [u8(0x0c), 0x07, 0x08, 0x90, 0x01]
	assert plan.prologue_fde_instruction_fragment.present
	assert plan.prologue_fde_instruction_fragment.region == .prologue
	assert plan.prologue_fde_instruction_fragment.region_relative_initial_code_offset_bytes == 0
	assert plan.epilogue_fde_instruction_template.present
	assert plan.epilogue_fde_instruction_template.region == .epilogue_template
	assert plan.epilogue_fde_instruction_template.region_relative_initial_code_offset_bytes == 0
	assert int(plan.total_instruction_fragment_bytes) == plan.cie_initial_instruction_bytes.len +
		memory_frame_cfi_encode_test_prologue_bytes(plan).len +
		memory_frame_cfi_encode_test_epilogue_bytes(plan).len
}

fn test_memory_frame_cfi_encode_m501_windows_empty_is_canonical_none() {
	facts := memory_frame_cfi_encode_test_empty(.windows_x86_64_microsoft_abi_coff)
	plan := memory_frame_cfi_encode_test_plan(&facts, [])
	assert plan.cfi.disposition == .windows_none
	assert plan.cie_initial_instruction_bytes == []u8{}
	assert plan.prologue_fde_instruction_fragment ==
		MemoryFrameDwarfFdeInstructionFragment{
		region:                                         .prologue
		region_relative_fde_instruction_fragment_bytes: []u8{}
	}
	assert plan.epilogue_fde_instruction_template ==
		MemoryFrameDwarfFdeInstructionFragment{
		region:                                         .epilogue_template
		region_relative_fde_instruction_fragment_bytes: []u8{}
	}
	assert plan.total_instruction_fragment_bytes == 0
}

fn test_memory_frame_cfi_encode_m502_windows_p8_max_bypasses_every_dwarf_cap() {
	facts := memory_frame_cfi_encode_test_adjustment(.windows_x86_64_microsoft_abi_coff,
		u64(0x7ffffff8))
	registers := [MemorySavedGpr.rbx, .rbp, .rsi, .rdi, .r12, .r13, .r14, .r15]
	plan := memory_frame_cfi_encode_test_plan(&facts, registers)
	assert plan.cfi.disposition == .windows_none
	assert plan.cfi.frame.save_push_count == 8
	assert plan.cfi.frame.allocation_bytes == u64(0x7ffffff8)
	assert plan.cfi.frame.total_stack_extent_bytes == u64(0x80000038)
	assert plan.cfi.frame.body_cfa_offset_bytes == u64(0x80000040)
	assert plan.cfi.frame.prologue_bytes.len == 25
	assert plan.cfi.frame.epilogue_bytes.len == 19
	assert plan.cfi.frame.body_cfa_offset_bytes >
		memory_frame_cfi_encode_max_cfa_offset_bytes
	assert plan.cfi.frame.prologue_bytes.len >
		int(memory_frame_cfi_encode_max_region_code_offset_bytes)
	assert plan.cie_initial_instruction_bytes.len == 0
	assert !plan.prologue_fde_instruction_fragment.present
	assert !plan.epilogue_fde_instruction_template.present
	assert plan.total_instruction_fragment_bytes == 0
}

fn test_memory_frame_cfi_encode_m503_linux_empty_is_active_zero_delta() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [])
	memory_frame_cfi_encode_test_assert_active_fragments(&plan)
	assert plan.cfi.disposition == .dwarf_zero_delta
	assert plan.prologue_fde_instruction_fragment ==
		MemoryFrameDwarfFdeInstructionFragment{
		present:                                        true
		region:                                         .prologue
		initial_cfa_offset_bytes:                       8
		final_cfa_offset_bytes:                         8
		region_relative_fde_instruction_fragment_bytes: []u8{}
	}
	assert plan.epilogue_fde_instruction_template ==
		MemoryFrameDwarfFdeInstructionFragment{
		present:                                        true
		region:                                         .epilogue_template
		initial_cfa_offset_bytes:                       8
		final_cfa_offset_bytes:                         8
		region_relative_fde_instruction_fragment_bytes: []u8{}
	}
	assert plan.total_instruction_fragment_bytes == 5
}

fn test_memory_frame_cfi_encode_m504_linux_red_zone_is_active_zero_delta() {
	facts := memory_frame_cfi_encode_test_facts(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_cfi_encode_test_slot(1, 128, 8),
	])
	plan := memory_frame_cfi_encode_test_plan(&facts, [])
	memory_frame_cfi_encode_test_assert_active_fragments(&plan)
	assert plan.cfi.disposition == .dwarf_zero_delta
	assert plan.cfi.frame.layout_frame.uses_red_zone
	assert plan.cfi.frame.layout_frame.red_zone_extent_bytes == 128
	assert plan.cfi.frame.allocation_bytes == 0
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan).len == 0
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan).len == 0
	assert plan.total_instruction_fragment_bytes == 5
}

fn test_memory_frame_cfi_encode_m505_apple_zero_delta_is_dwarf_active() {
	for slots in [
		[]MemorySlotRequest{},
		[memory_frame_cfi_encode_test_slot(2, 128, 8)],
	] {
		facts := memory_frame_cfi_encode_test_facts(.macos_x86_64_sysv_macho, false,
			0, slots)
		plan := memory_frame_cfi_encode_test_plan(&facts, [])
		memory_frame_cfi_encode_test_assert_active_fragments(&plan)
		assert plan.cfi.disposition == .dwarf_zero_delta
		assert plan.total_instruction_fragment_bytes == 5
	}
}

fn test_memory_frame_cfi_encode_m506_m4_and_m3_errors_propagate_unchanged() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	missing := MemoryCalleeSaveFacts{}
	memory_frame_cfi_encode_test_expect_error(&facts, &missing,
		'amd64 memory frame save: callee-save facts are required')

	bad_facts := MemoryFunctionFrameFacts{
		function_id:       77
		profile:           .windows_x86_64_microsoft_abi_coff
		extent_kind:       .dynamic
		call_extent_bytes: 40
		has_call:          true
	}
	valid_saves := memory_frame_cfi_encode_test_saves([])
	memory_frame_cfi_encode_test_expect_error(&bad_facts, &valid_saves,
		'amd64 memory frame: dynamic frame extent is unsupported')

	rsi := memory_frame_cfi_encode_test_saves([.rsi])
	memory_frame_cfi_encode_test_expect_error(&facts, &rsi,
		'amd64 memory frame save: save 0 register rsi is not nonvolatile for target profile')
}

fn test_memory_frame_cfi_encode_m507_cie_initial_instruction_subsequence_is_exact() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [])
	assert memory_frame_cfi_encode_cie_initial_instruction_bytes == [u8(0x0c), 0x07,
		0x08, 0x90, 0x01]
	assert plan.cie_initial_instruction_bytes == [u8(0x0c), 0x07, 0x08, 0x90, 0x01]
	assert plan.cie_initial_instruction_bytes.len == 5
	assert plan.total_instruction_fragment_bytes == 5
}

fn test_memory_frame_cfi_encode_m508_allocation_only_fragments_are_exact() {
	facts := memory_frame_cfi_encode_test_facts(.linux_x86_64_sysv_elf, true, 8,
		[])
	plan := memory_frame_cfi_encode_test_plan(&facts, [])
	memory_frame_cfi_encode_test_assert_active_fragments(&plan)
	assert plan.cfi.frame.allocation_bytes == 8
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x44), 0x0e,
		0x10]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x44), 0x0e,
		0x08]
	assert plan.prologue_fde_instruction_fragment.region_relative_final_code_offset_bytes == 4
	assert plan.prologue_fde_instruction_fragment.initial_cfa_offset_bytes == 8
	assert plan.prologue_fde_instruction_fragment.final_cfa_offset_bytes == 16
	assert plan.prologue_fde_instruction_fragment.semantic_operation_count == 1
	assert plan.epilogue_fde_instruction_template.region_relative_final_code_offset_bytes == 4
	assert plan.epilogue_fde_instruction_template.initial_cfa_offset_bytes == 16
	assert plan.epilogue_fde_instruction_template.final_cfa_offset_bytes == 8
	assert plan.epilogue_fde_instruction_template.semantic_operation_count == 1
	assert plan.total_instruction_fragment_bytes == 11
}

fn test_memory_frame_cfi_encode_m509_rbx_push_and_pop_fragments_are_exact() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.rbx])
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x41), 0x0e,
		0x08]
	assert plan.prologue_fde_instruction_fragment.semantic_operation_count == 2
	assert plan.epilogue_fde_instruction_template.semantic_operation_count == 1
	assert plan.total_instruction_fragment_bytes == 13
}

fn test_memory_frame_cfi_encode_m510_rbp_uses_dwarf_register_six() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.rbp])
	assert plan.cfi.frame.saves[0].register_encoding == 5
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x86, 0x02]
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan)[3] !=
		u8(0x80 | plan.cfi.frame.saves[0].register_encoding)
}

fn test_memory_frame_cfi_encode_m511_apple_r12_uses_two_byte_coordinate() {
	facts := memory_frame_cfi_encode_test_empty(.macos_x86_64_sysv_macho)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.r12])
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x42), 0x0e,
		0x10, 0x8c, 0x02]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x42), 0x0e,
		0x08]
	assert plan.prologue_fde_instruction_fragment.region_relative_final_code_offset_bytes == 2
	assert plan.epilogue_fde_instruction_template.region_relative_final_code_offset_bytes == 2
}

fn test_memory_frame_cfi_encode_m512_same_pc_order_and_two_save_streams_are_exact() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.r12, .rbx])
	assert plan.cfi.prologue_ops.map(it.same_pc_ordinal) == [u8(0), 1, 0, 1, 0]
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02, 0x42, 0x0e, 0x18, 0x8c, 0x03, 0x44, 0x0e, 0x20]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x44), 0x0e,
		0x18, 0x42, 0x0e, 0x10, 0x41, 0x0e, 0x08]
	assert plan.total_instruction_fragment_bytes == 27
}

fn test_memory_frame_cfi_encode_m513_full_six_save_streams_are_exact() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	registers := [MemorySavedGpr.r15, .r13, .rbx, .r14, .rbp, .r12]
	plan := memory_frame_cfi_encode_test_plan(&facts, registers)
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02, 0x41, 0x0e, 0x18, 0x86, 0x03, 0x42, 0x0e, 0x20, 0x8c,
		0x04, 0x42, 0x0e, 0x28, 0x8d, 0x05, 0x42, 0x0e, 0x30, 0x8e, 0x06, 0x42,
		0x0e, 0x38, 0x8f, 0x07, 0x44, 0x0e, 0x40]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x44), 0x0e,
		0x38, 0x42, 0x0e, 0x30, 0x42, 0x0e, 0x28, 0x42, 0x0e, 0x20, 0x42, 0x0e,
		0x18, 0x41, 0x0e, 0x10, 0x41, 0x0e, 0x08]
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan).len == 33
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan).len == 21
	assert plan.total_instruction_fragment_bytes == 59
}

fn test_memory_frame_cfi_encode_m514_shortest_uleb128_boundaries_are_exact() {
	assert memory_frame_cfi_encode_uleb128_size(0) == 1
	assert memory_frame_cfi_encode_test_uleb128(0) == [u8(0x00)]
	assert memory_frame_cfi_encode_uleb128_size(127) == 1
	assert memory_frame_cfi_encode_test_uleb128(127) == [u8(0x7f)]
	assert memory_frame_cfi_encode_uleb128_size(128) == 2
	assert memory_frame_cfi_encode_test_uleb128(128) == [u8(0x80), 0x01]
	assert memory_frame_cfi_encode_uleb128_size(16383) == 2
	assert memory_frame_cfi_encode_test_uleb128(16383) == [u8(0xff), 0x7f]
	assert memory_frame_cfi_encode_uleb128_size(16384) == 3
	assert memory_frame_cfi_encode_test_uleb128(16384) == [u8(0x80), 0x80, 0x01]
	assert memory_frame_cfi_encode_uleb128_size(2097151) == 3
	assert memory_frame_cfi_encode_test_uleb128(2097151) == [u8(0xff), 0xff, 0x7f]
	assert memory_frame_cfi_encode_uleb128_size(2097152) == 4
	assert memory_frame_cfi_encode_test_uleb128(2097152) == [u8(0x80), 0x80, 0x80,
		0x01]
	assert memory_frame_cfi_encode_uleb128_size(268435455) == 4
	assert memory_frame_cfi_encode_test_uleb128(268435455) == [u8(0xff), 0xff, 0xff,
		0x7f]
	assert memory_frame_cfi_encode_uleb128_size(268435456) == 5
	assert memory_frame_cfi_encode_test_uleb128(268435456) == [u8(0x80), 0x80, 0x80,
		0x80, 0x01]
	assert memory_frame_cfi_encode_uleb128_size(u64(0x80000030)) == 5
	assert memory_frame_cfi_encode_test_uleb128(u64(0x80000030)) == [u8(0xb0), 0x80,
		0x80, 0x80, 0x08]
	assert memory_frame_cfi_encode_uleb128_size(u64(1) << 35) == 6
}

fn test_memory_frame_cfi_encode_m515_a128_uses_two_byte_cfa_uleb() {
	facts := memory_frame_cfi_encode_test_adjustment(.linux_x86_64_sysv_elf, 120)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.rbx])
	assert plan.cfi.frame.allocation_bytes == 128
	assert plan.cfi.frame.body_cfa_offset_bytes == 144
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02, 0x47, 0x0e, 0x90, 0x01]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x47), 0x0e,
		0x10, 0x41, 0x0e, 0x08]
}

fn test_memory_frame_cfi_encode_m516_a4096_uses_exact_region_fragments() {
	facts := memory_frame_cfi_encode_test_adjustment(.linux_x86_64_sysv_elf, 4088)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.rbx])
	assert plan.cfi.frame.allocation_bytes == 4096
	assert plan.cfi.frame.body_cfa_offset_bytes == 4112
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02, 0x47, 0x0e, 0x90, 0x20]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x47), 0x0e,
		0x10, 0x41, 0x0e, 0x08]
}

fn test_memory_frame_cfi_encode_m517_a524288_uses_three_byte_cfa_uleb() {
	facts := memory_frame_cfi_encode_test_adjustment(.macos_x86_64_sysv_macho,
		524280)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.rbx])
	assert plan.cfi.frame.allocation_bytes == 524288
	assert plan.cfi.frame.body_cfa_offset_bytes == 524304
	assert memory_frame_cfi_encode_test_prologue_bytes(&plan) == [u8(0x41), 0x0e,
		0x10, 0x83, 0x02, 0x47, 0x0e, 0x90, 0x80, 0x20]
	assert memory_frame_cfi_encode_test_epilogue_bytes(&plan) == [u8(0x47), 0x0e,
		0x10, 0x41, 0x0e, 0x08]
}

fn test_memory_frame_cfi_encode_m518_reachable_dwarf_max_hits_37_21_63() {
	facts := memory_frame_cfi_encode_test_adjustment(.linux_x86_64_sysv_elf,
		u64(0x7ffffff8))
	registers := [MemorySavedGpr.rbx, .rbp, .r12, .r13, .r14, .r15]
	plan := memory_frame_cfi_encode_test_plan(&facts, registers)
	prologue := memory_frame_cfi_encode_test_prologue_bytes(&plan)
	epilogue := memory_frame_cfi_encode_test_epilogue_bytes(&plan)
	assert plan.cfi.frame.allocation_bytes == u64(0x7ffffff8)
	assert plan.cfi.frame.body_cfa_offset_bytes == u64(0x80000030)
	assert plan.cfi.frame.body_offset_bytes == 17
	assert plan.cfi.frame.epilogue_bytes.len == 17
	assert prologue.len == 37
	assert epilogue.len == 21
	assert plan.total_instruction_fragment_bytes == 63
	assert prologue[30..] == [u8(0x47), 0x0e, 0xb0, 0x80, 0x80, 0x80, 0x08]
	assert epilogue == [u8(0x47), 0x0e, 0x38, 0x42, 0x0e, 0x30, 0x42, 0x0e,
		0x28, 0x42, 0x0e, 0x20, 0x42, 0x0e, 0x18, 0x41, 0x0e, 0x10, 0x41, 0x0e,
		0x08]
	assert plan.prologue_fde_instruction_fragment.region_relative_final_code_offset_bytes == 17
	assert plan.epilogue_fde_instruction_template.region_relative_final_code_offset_bytes == 17
}

fn test_memory_frame_cfi_encode_m519_private_decoder_and_cap_refusals_are_exact() {
	facts := memory_frame_cfi_encode_test_empty(.linux_x86_64_sysv_elf)
	plan := memory_frame_cfi_encode_test_plan(&facts, [.r12, .rbx])
	prologue := memory_frame_cfi_encode_test_prologue_bytes(&plan)
	decoded := memory_frame_cfi_encode_decode_fragment(prologue, .prologue, 8,
		plan.cfi.prologue_ops.len) or { panic(err.msg()) }
	assert decoded.final_code_offset_bytes == 7
	assert decoded.final_cfa_offset_bytes == 32
	assert decoded.ops.len == 5
	assert decoded.ops.map(it.same_pc_ordinal) == [u8(0), 1, 0, 1, 0]

	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		_ := memory_frame_cfi_encode_decode_fragment([u8(0x40)], .prologue, 8,
			0)!
	}, 'amd64 memory frame cfi encode: non-shortest code advance')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		_ := memory_frame_cfi_encode_decode_fragment([u8(0x41)], .prologue, 8,
			0)!
	}, 'amd64 memory frame cfi encode: trailing code advance')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		_ := memory_frame_cfi_encode_decode_fragment([u8(0x41), 0x41, 0x0e,
			0x08], .prologue, 8, 1)!
	}, 'amd64 memory frame cfi encode: non-shortest code advance')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		_ := memory_frame_cfi_encode_decode_fragment([u8(0x0e), 0x88, 0x00],
			.prologue, 8, 1)!
	}, 'amd64 memory frame cfi encode: non-shortest ULEB128')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		_ := memory_frame_cfi_encode_decode_fragment([u8(0x01)], .prologue, 8,
			1)!
	}, 'amd64 memory frame cfi encode: unsupported CFI opcode 0x01')

	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000031), 17, 5, 37,
			21, 63)!
	}, 'amd64 memory frame cfi encode: CFA offset cap exceeded')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000030), 18, 5, 37,
			21, 63)!
	}, 'amd64 memory frame cfi encode: region code offset cap exceeded')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000030), 17, 6, 37,
			21, 63)!
	}, 'amd64 memory frame cfi encode: ULEB128 byte cap exceeded')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000030), 17, 5, 38,
			21, 63)!
	}, 'amd64 memory frame cfi encode: prologue fragment byte cap exceeded')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000030), 17, 5, 37,
			22, 63)!
	}, 'amd64 memory frame cfi encode: epilogue fragment byte cap exceeded')
	memory_frame_cfi_encode_test_expect_private_error(fn () ! {
		memory_frame_cfi_encode_validate_active_caps(u64(0x80000030), 17, 5, 37,
			21, 64)!
	}, 'amd64 memory frame cfi encode: total instruction fragment byte cap exceeded')
}

fn test_memory_frame_cfi_encode_m520_determinism_transactionality_and_deep_nonaliasing() {
	mut input_slots := [
		memory_frame_cfi_encode_test_slot(2, 16, 8),
		memory_frame_cfi_encode_test_slot(1, 8, 8),
	]
	mut input_registers := [MemorySavedGpr.r12, .rbx]
	first_facts := memory_frame_cfi_encode_test_facts(.linux_x86_64_sysv_elf, false,
		0, input_slots)
	second_facts := memory_frame_cfi_encode_test_facts(.linux_x86_64_sysv_elf,
		false, 0, input_slots)
	first_saves := memory_frame_cfi_encode_test_saves(input_registers)
	second_saves := memory_frame_cfi_encode_test_saves(input_registers)
	first := plan_memory_saved_frame_cfi_instruction_fragments(&first_facts,
		&first_saves) or { panic(err.msg()) }
	second := plan_memory_saved_frame_cfi_instruction_fragments(&second_facts,
		&second_saves) or { panic(err.msg()) }
	assert first == second
	unsafe {
		assert first.cie_initial_instruction_bytes.data !=
			second.cie_initial_instruction_bytes.data
		assert first.prologue_fde_instruction_fragment.region_relative_fde_instruction_fragment_bytes.data !=
			second.prologue_fde_instruction_fragment.region_relative_fde_instruction_fragment_bytes.data
		assert first.epilogue_fde_instruction_template.region_relative_fde_instruction_fragment_bytes.data !=
			second.epilogue_fde_instruction_template.region_relative_fde_instruction_fragment_bytes.data
		assert first.cfi.prologue_ops.data != second.cfi.prologue_ops.data
		assert first.cfi.epilogue_template_ops.data != second.cfi.epilogue_template_ops.data
		assert first.cfi.frame.prologue_bytes.data != second.cfi.frame.prologue_bytes.data
		assert first.cfi.frame.epilogue_bytes.data != second.cfi.frame.epilogue_bytes.data
		assert first.cfi.frame.layout_frame.slots.data !=
			second.cfi.frame.layout_frame.slots.data
	}
	input_slots[0] = memory_frame_cfi_encode_test_slot(999, 1, 1)
	input_registers[0] = .r15
	assert second.cfi.frame.layout_frame.slots.map(it.id) == [u32(1), 2]
	assert second.cfi.frame.save_facts.registers == [MemorySavedGpr.r12, .rbx]
	unsafe {
		mut cie := &u8(first.cie_initial_instruction_bytes.data)
		mut prologue := &u8(first.prologue_fde_instruction_fragment.region_relative_fde_instruction_fragment_bytes.data)
		mut epilogue := &u8(first.epilogue_fde_instruction_template.region_relative_fde_instruction_fragment_bytes.data)
		mut op := &MemoryFrameCfiOp(first.cfi.prologue_ops.data)
		cie[0] = 0
		prologue[0] = 0
		epilogue[0] = 0
		op[0] = MemoryFrameCfiOp{}
	}
	assert second.cie_initial_instruction_bytes[0] == 0x0c
	assert memory_frame_cfi_encode_test_prologue_bytes(&second)[0] == 0x41
	assert memory_frame_cfi_encode_test_epilogue_bytes(&second)[0] == 0x44
	assert second.cfi.prologue_ops[0].instruction_end_offset_bytes == 1
}
