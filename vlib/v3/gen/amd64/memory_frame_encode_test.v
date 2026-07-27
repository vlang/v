module amd64

fn memory_frame_encode_test_slot(id u32, kind MemorySlotKind, size u64, alignment u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              id
		kind:            kind
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn memory_frame_encode_test_facts(profile TargetProfile, function_id u32, has_call bool, call_extent u64, extent_kind MemoryFrameExtentKind, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       function_id
		profile:           profile
		extent_kind:       extent_kind
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_encode_test_fixed(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return memory_frame_encode_test_facts(profile, 29, has_call, call_extent, .fixed,
		slots)
}

fn memory_frame_encode_test_plan(facts &MemoryFunctionFrameFacts) MemoryFrameEncodingPlan {
	return plan_memory_frame_encoding(facts) or { panic(err.msg()) }
}

fn memory_frame_encode_test_windows_adjustment(adjustment u64) MemoryFrameEncodingPlan {
	facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, [memory_frame_encode_test_slot(1, .local, adjustment, 1)])
	plan := memory_frame_encode_test_plan(&facts)
	assert plan.frame.stack_adjustment_bytes == adjustment
	return plan
}

fn memory_frame_encode_test_expect_error(facts &MemoryFunctionFrameFacts, expected string) {
	if _ := plan_memory_frame_encoding(facts) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == expected
	}
}

fn memory_frame_encode_test_assert_stack(plan &MemoryFrameEncodingPlan, kind MemoryFramePrologueKind, adjustment u64, prologue []u8, epilogue []u8) {
	assert plan.frame.stack_adjustment_bytes == adjustment
	assert plan.prologue_kind == kind
	assert plan.prologue_bytes == prologue
	assert plan.epilogue_bytes == epilogue
	assert plan.body_offset_bytes == u8(prologue.len)
	assert plan.entry_cfa_offset_bytes == 8
	assert plan.body_cfa_offset_bytes == adjustment + 8
}

fn memory_frame_encode_test_assert_absent_probe(plan &MemoryFrameEncodingPlan) {
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
	assert plan.chkstk.shadow_space_bytes == 0
	assert plan.chkstk.pre_call_rsp_mod_16 == 0
	assert plan.chkstk.helper_entry_rsp_mod_16 == 0
	assert !plan.chkstk.eax_zero_extends_rax
	assert !plan.chkstk.helper_preserves_rax
	assert plan.chkstk.rax_after_prologue == 0
	assert !plan.chkstk.clobbers_r10
	assert !plan.chkstk.clobbers_r11
	assert !plan.chkstk.clobbers_eflags
	assert !plan.chkstk.preserves_other_integer_gprs
}

fn memory_frame_encode_test_assert_probe(plan &MemoryFrameEncodingPlan, adjustment u64) {
	assert plan.probe_fixup == MemoryFrameRel32Fixup{
		present:             true
		kind:                .call_rel32_next_instruction
		target_name:         '__chkstk'
		opcode_offset_bytes: 5
		field_offset_bytes:  6
		width_bytes:         4
		addend:              0
	}
	assert plan.chkstk == MemoryChkstkContract{
		present:                      true
		allocation_bytes:             adjustment
		save_push_count:              0
		shadow_space_bytes:           0
		pre_call_rsp_mod_16:          8
		helper_entry_rsp_mod_16:      0
		eax_zero_extends_rax:         true
		helper_preserves_rax:         true
		rax_after_prologue:           adjustment
		clobbers_r10:                 true
		clobbers_r11:                 true
		clobbers_eflags:              true
		preserves_other_integer_gprs: true
	}
	assert plan.prologue_bytes[plan.probe_fixup.opcode_offset_bytes] == 0xe8
	assert plan.prologue_bytes[plan.probe_fixup.field_offset_bytes..plan.probe_fixup.field_offset_bytes +
		plan.probe_fixup.width_bytes] == [u8(0), 0, 0, 0]
}

fn memory_frame_encode_test_assert_no_unwind(plan &MemoryFrameEncodingPlan) {
	assert !plan.windows_unwind.present
	assert plan.windows_unwind.kind == .none
	assert plan.windows_unwind.allocation_bytes == 0
	assert plan.windows_unwind.size_of_prolog_bytes == 0
	assert plan.windows_unwind.allocation_code_offset_bytes == 0
	assert plan.windows_unwind.count_of_codes == 0
	assert plan.windows_unwind.xdata_bytes.len == 0
}

fn memory_frame_encode_test_assert_unwind(plan &MemoryFrameEncodingPlan, kind MemoryWindowsUnwindKind, count u8, expected []u8) {
	assert plan.windows_unwind.present
	assert plan.windows_unwind.kind == kind
	assert plan.windows_unwind.allocation_bytes == plan.frame.stack_adjustment_bytes
	assert plan.windows_unwind.size_of_prolog_bytes == u8(plan.prologue_bytes.len)
	assert plan.windows_unwind.allocation_code_offset_bytes == u8(plan.prologue_bytes.len)
	assert plan.windows_unwind.count_of_codes == count
	assert plan.windows_unwind.xdata_bytes == expected
}

fn memory_frame_encode_test_assert_rsp(displacement i64, kind MemoryRspDisplacementKind, mod_bits u8, expected []u8) {
	basis := if displacement < 0 { MemorySlotBasis.entry_rsp } else { MemorySlotBasis.body_rsp }
	address := memory_frame_encoding_encode_rsp_address(basis, displacement) or {
		panic(err.msg())
	}
	assert address.basis == basis
	assert i64(address.displacement_bytes) == displacement
	assert address.kind == kind
	assert address.mod_bits == mod_bits
	assert address.rm_bits == 4
	assert address.sib_scale_bits == 0
	assert address.sib_index_bits == 4
	assert address.sib_base_bits == 4
	assert (address.sib_scale_bits << 6) | (address.sib_index_bits << 3) |
		address.sib_base_bits == 0x24
	assert (address.mod_bits << 6) | (u8(5) << 3) | address.rm_bits ==
		(mod_bits << 6) | u8(0x2c)
	assert address.displacement_le == expected
}

fn test_memory_frame_encode_e01_empty_sysv_is_normalized() {
	facts := memory_frame_encode_test_fixed(.linux_x86_64_sysv_elf, false, 0, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .none, 0, [], [])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_no_unwind(&plan)
	assert plan.frame.profile == .linux_x86_64_sysv_elf
	assert plan.frame.slots.len == 0
	assert plan.slots.len == 0
}

fn test_memory_frame_encode_e02_empty_apple_is_normalized() {
	facts := memory_frame_encode_test_fixed(.macos_x86_64_sysv_macho, false, 0, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .none, 0, [], [])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_no_unwind(&plan)
	assert plan.frame.profile == .macos_x86_64_sysv_macho
	assert plan.frame.slots.len == 0
	assert plan.slots.len == 0
}

fn test_memory_frame_encode_e03_empty_windows_is_normalized() {
	facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .none, 0, [], [])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_no_unwind(&plan)
	assert plan.frame.profile == .windows_x86_64_microsoft_abi_coff
	assert plan.frame.slots.len == 0
	assert plan.slots.len == 0
}

fn test_memory_frame_encode_e04_sysv_call_only_d8_exact_bytes() {
	facts := memory_frame_encode_test_fixed(.linux_x86_64_sysv_elf, true, 0, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm8, 8,
		[u8(0x48), 0x83, 0xec, 0x08], [u8(0x48), 0x83, 0xc4, 0x08])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_no_unwind(&plan)
}

fn test_memory_frame_encode_e05_apple_call_only_d8_exact_bytes() {
	facts := memory_frame_encode_test_fixed(.macos_x86_64_sysv_macho, true, 0, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm8, 8,
		[u8(0x48), 0x83, 0xec, 0x08], [u8(0x48), 0x83, 0xc4, 0x08])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_no_unwind(&plan)
}

fn test_memory_frame_encode_e06_windows_d8_small_unwind_and_zero_displacement() {
	plan := memory_frame_encode_test_windows_adjustment(8)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm8, 8,
		[u8(0x48), 0x83, 0xec, 0x08], [u8(0x48), 0x83, 0xc4, 0x08])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_small, 1,
		[u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x02, 0x00, 0x00])
	assert plan.slots.len == 1
	assert plan.slots[0].source_placement_index == 0
	assert plan.slots[0].address.kind == .zero
	assert plan.slots[0].address.displacement_le.len == 0
}

fn test_memory_frame_encode_e07_windows_call_only_d40_preserves_legacy_record() {
	facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 32, [])
	plan := memory_frame_encode_test_plan(&facts)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm8, 40,
		[u8(0x48), 0x83, 0xec, 0x28], [u8(0x48), 0x83, 0xc4, 0x28])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_small, 1,
		[u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00])
}

fn test_memory_frame_encode_e08_windows_d120_last_small_unwind() {
	plan := memory_frame_encode_test_windows_adjustment(120)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm8, 120,
		[u8(0x48), 0x83, 0xec, 0x78], [u8(0x48), 0x83, 0xc4, 0x78])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_small, 1,
		[u8(0x01), 0x04, 0x01, 0x00, 0x04, 0xe2, 0x00, 0x00])
}

fn test_memory_frame_encode_e09_windows_d136_first_large_info0() {
	plan := memory_frame_encode_test_windows_adjustment(136)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm32, 136,
		[u8(0x48), 0x81, 0xec, 0x88, 0x00, 0x00, 0x00],
		[u8(0x48), 0x81, 0xc4, 0x88, 0x00, 0x00, 0x00])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info0, 2,
		[u8(0x01), 0x07, 0x02, 0x00, 0x07, 0x01, 0x11, 0x00])
}

fn test_memory_frame_encode_e10_windows_d4088_last_nonprobe() {
	plan := memory_frame_encode_test_windows_adjustment(4088)
	memory_frame_encode_test_assert_stack(&plan, .sub_imm32, 4088,
		[u8(0x48), 0x81, 0xec, 0xf8, 0x0f, 0x00, 0x00],
		[u8(0x48), 0x81, 0xc4, 0xf8, 0x0f, 0x00, 0x00])
	memory_frame_encode_test_assert_absent_probe(&plan)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info0, 2,
		[u8(0x01), 0x07, 0x02, 0x00, 0x07, 0x01, 0xff, 0x01])
}

fn test_memory_frame_encode_e11_windows_d4104_first_probe_exact_fixup_and_unwind() {
	plan := memory_frame_encode_test_windows_adjustment(4104)
	memory_frame_encode_test_assert_stack(&plan, .windows_chkstk, 4104,
		[u8(0xb8), 0x08, 0x10, 0x00, 0x00, 0xe8, 0x00, 0x00, 0x00, 0x00,
			0x48, 0x29, 0xc4],
		[u8(0x48), 0x81, 0xc4, 0x08, 0x10, 0x00, 0x00])
	memory_frame_encode_test_assert_probe(&plan, 4104)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info0, 2,
		[u8(0x01), 0x0d, 0x02, 0x00, 0x0d, 0x01, 0x01, 0x02])
}

fn test_memory_frame_encode_e12_windows_d524280_last_large_info0() {
	plan := memory_frame_encode_test_windows_adjustment(524280)
	memory_frame_encode_test_assert_stack(&plan, .windows_chkstk, 524280,
		[u8(0xb8), 0xf8, 0xff, 0x07, 0x00, 0xe8, 0x00, 0x00, 0x00, 0x00,
			0x48, 0x29, 0xc4],
		[u8(0x48), 0x81, 0xc4, 0xf8, 0xff, 0x07, 0x00])
	memory_frame_encode_test_assert_probe(&plan, 524280)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info0, 2,
		[u8(0x01), 0x0d, 0x02, 0x00, 0x0d, 0x01, 0xff, 0xff])
	assert plan.windows_unwind.xdata_bytes.len == 8
}

fn test_memory_frame_encode_e13_windows_d524296_first_large_info1_with_padding() {
	plan := memory_frame_encode_test_windows_adjustment(524296)
	memory_frame_encode_test_assert_stack(&plan, .windows_chkstk, 524296,
		[u8(0xb8), 0x08, 0x00, 0x08, 0x00, 0xe8, 0x00, 0x00, 0x00, 0x00,
			0x48, 0x29, 0xc4],
		[u8(0x48), 0x81, 0xc4, 0x08, 0x00, 0x08, 0x00])
	memory_frame_encode_test_assert_probe(&plan, 524296)
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info1, 3,
		[u8(0x01), 0x0d, 0x03, 0x00, 0x0d, 0x11, 0x08, 0x00, 0x08, 0x00,
			0x00, 0x00])
	assert plan.windows_unwind.xdata_bytes.len == 12
}

fn test_memory_frame_encode_e14_windows_maximum_adjustment_is_exact() {
	plan := memory_frame_encode_test_windows_adjustment(u64(0x7ffffff8))
	memory_frame_encode_test_assert_stack(&plan, .windows_chkstk, u64(0x7ffffff8),
		[u8(0xb8), 0xf8, 0xff, 0xff, 0x7f, 0xe8, 0x00, 0x00, 0x00, 0x00,
			0x48, 0x29, 0xc4],
		[u8(0x48), 0x81, 0xc4, 0xf8, 0xff, 0xff, 0x7f])
	memory_frame_encode_test_assert_probe(&plan, u64(0x7ffffff8))
	memory_frame_encode_test_assert_unwind(&plan, .alloc_large_info1, 3,
		[u8(0x01), 0x0d, 0x03, 0x00, 0x0d, 0x11, 0xf8, 0xff, 0xff, 0x7f,
			0x00, 0x00])
	assert plan.body_cfa_offset_bytes == u64(0x80000000)
}

fn test_memory_frame_encode_e15_signed_rsp_displacement_and_i32_endpoints() {
	memory_frame_encode_test_assert_rsp(-129, .disp32, 2, [u8(0x7f), 0xff, 0xff, 0xff])
	memory_frame_encode_test_assert_rsp(-128, .disp8, 1, [u8(0x80)])
	memory_frame_encode_test_assert_rsp(-1, .disp8, 1, [u8(0xff)])
	memory_frame_encode_test_assert_rsp(0, .zero, 0, [])
	memory_frame_encode_test_assert_rsp(1, .disp8, 1, [u8(0x01)])
	memory_frame_encode_test_assert_rsp(127, .disp8, 1, [u8(0x7f)])
	memory_frame_encode_test_assert_rsp(128, .disp32, 2, [u8(0x80), 0x00, 0x00, 0x00])
	memory_frame_encode_test_assert_rsp(i64(min_i32), .disp32, 2,
		[u8(0x00), 0x00, 0x00, 0x80])
	memory_frame_encode_test_assert_rsp(i64(max_i32), .disp32, 2,
		[u8(0xff), 0xff, 0xff, 0x7f])
	for value in [i64(min_i32) - 1, i64(max_i32) + 1] {
		if _ := memory_frame_encoding_encode_rsp_address(.body_rsp, value) {
			assert false, 'expected signed i32 refusal for ${value}'
		} else {
			assert err.msg() == 'amd64 memory frame encoding: RSP displacement ${value} is outside signed i32'
		}
	}
}

fn test_memory_frame_encode_e16_m0_basis_and_every_placement_are_consumed_once() {
	red_facts := memory_frame_encode_test_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_encode_test_slot(7, .spill, 8, 8),
		memory_frame_encode_test_slot(3, .local, 1, 1),
	])
	red := memory_frame_encode_test_plan(&red_facts)
	assert red.frame.uses_red_zone
	assert red.frame.stack_adjustment_bytes == 0
	assert red.frame.slots.map(it.id) == [u32(3), 7]
	assert red.frame.slots.map(it.displacement_bytes) == [i64(-1), -16]
	assert red.slots.len == red.frame.slots.len
	for index, encoded in red.slots {
		assert encoded.source_placement_index == u32(index)
		assert encoded.placement == red.frame.slots[index]
		assert encoded.address.basis == .entry_rsp
		assert i64(encoded.address.displacement_bytes) ==
			red.frame.slots[index].displacement_bytes
	}

	body_facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 32, [
		memory_frame_encode_test_slot(4, .local, 8, 8),
		memory_frame_encode_test_slot(5, .spill, 8, 8),
	])
	body := memory_frame_encode_test_plan(&body_facts)
	assert !body.frame.uses_red_zone
	assert body.frame.slots.map(it.displacement_bytes) == [i64(32), 40]
	assert body.slots.map(it.source_placement_index) == [u32(0), 1]
	assert body.slots.map(it.address.basis) == [MemorySlotBasis.body_rsp, .body_rsp]
	assert body.slots.map(it.address.displacement_le) == [[u8(32)], [u8(40)]]
}

fn test_memory_frame_encode_e17_m1_composition_and_all_output_cap_endpoints() {
	m1 := MemoryAggPlan{
		profile:         .windows_x86_64_microsoft_abi_coff
		function_index: 4
		function_id:    4
		slot_requests:  [
			MemoryAggSlotBinding{
				request: memory_frame_encode_test_slot(17, .fixed_alloca, 8, 8)
			},
		]
		aggregate_slots: [
			MemoryAggAggregateSlotBinding{
				request: memory_frame_encode_test_slot(0x80000001, .aggregate_temp,
					16, 16)
			},
		]
	}
	mut merged := []MemorySlotRequest{}
	for binding in m1.slot_requests {
		merged << binding.request
	}
	for binding in m1.aggregate_slots {
		merged << binding.request
	}
	merged_facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, merged)
	merged_plan := memory_frame_encode_test_plan(&merged_facts)
	assert merged_plan.frame.slots.map(it.id) == [u32(17), 0x80000001]
	assert merged_plan.frame.slots.map(it.kind) == [MemorySlotKind.fixed_alloca,
		.aggregate_temp]
	assert merged_plan.slots.map(it.source_placement_index) == [u32(0), 1]

	assert memory_frame_encoding_max_slots == 1024
	assert memory_frame_encoding_max_fixups == 1
	assert memory_frame_encoding_max_prologue_bytes == 13
	assert memory_frame_encoding_max_epilogue_bytes == 7
	assert memory_frame_encoding_max_xdata_bytes == 12
	assert memory_frame_encoding_max_total_displacement_bytes == 4096
	assert memory_frame_encoding_max_byte_slice_payload == 4128
	mut cap_slots := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1024 {
		cap_slots << memory_frame_encode_test_slot(u32(index), .local, 1, 1)
	}
	at_cap_facts := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 524296, cap_slots)
	at_cap := memory_frame_encode_test_plan(&at_cap_facts)
	assert at_cap.slots.len == 1024
	mut displacement_bytes := 0
	for encoded in at_cap.slots {
		assert encoded.address.kind == .disp32
		displacement_bytes += encoded.address.displacement_le.len
	}
	assert displacement_bytes == 4096
	assert displacement_bytes + at_cap.prologue_bytes.len + at_cap.epilogue_bytes.len +
		at_cap.windows_unwind.xdata_bytes.len == 4128

	cap_slots << memory_frame_encode_test_slot(1024, .local, 1, 1)
	over_cap := memory_frame_encode_test_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 524296, cap_slots)
	memory_frame_encode_test_expect_error(&over_cap,
		'amd64 memory frame: slot count 1025 exceeds 1024')
}

fn test_memory_frame_encode_e18_precedence_transactionality_and_deep_copy() {
	bad_profile := unsafe { TargetProfile(255) }
	bad_extent := unsafe { MemoryFrameExtentKind(255) }
	bad := memory_frame_encode_test_facts(bad_profile, 1, false, 8, bad_extent, [
		memory_frame_encode_test_slot(1, .local, 0, 0),
	])
	memory_frame_encode_test_expect_error(&bad,
		'amd64 memory frame: unsupported target profile')

	mut too_many := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1025 {
		too_many << memory_frame_encode_test_slot(u32(index), .local, 0, 0)
	}
	cap_before_dynamic := memory_frame_encode_test_facts(.linux_x86_64_sysv_elf,
		1, false, 8, .dynamic, too_many)
	memory_frame_encode_test_expect_error(&cap_before_dynamic,
		'amd64 memory frame: slot count 1025 exceeds 1024')
	assert too_many.len == 1025
	assert too_many[0].size_bytes == 0

	mut left_input := [
		memory_frame_encode_test_slot(9, .local, 4064, 1),
	]
	right_input := left_input.clone()
	left_facts := memory_frame_encode_test_facts(.windows_x86_64_microsoft_abi_coff,
		8, true, 32, .fixed, left_input)
	right_facts := memory_frame_encode_test_facts(.windows_x86_64_microsoft_abi_coff,
		8, true, 32, .fixed, right_input)
	left := memory_frame_encode_test_plan(&left_facts)
	right := memory_frame_encode_test_plan(&right_facts)
	left_input[0] = memory_frame_encode_test_slot(99, .spill, 1, 1)
	assert left == right
	assert left.frame.slots[0].id == 9
	unsafe {
		assert left.prologue_bytes.data != right.prologue_bytes.data
		assert left.epilogue_bytes.data != right.epilogue_bytes.data
		assert left.windows_unwind.xdata_bytes.data != right.windows_unwind.xdata_bytes.data
		assert left.frame.slots.data != right.frame.slots.data
		assert left.slots.data != right.slots.data
		assert left.slots[0].address.displacement_le.data !=
			right.slots[0].address.displacement_le.data
	}
	unsafe {
		mut prologue_pointer := &u8(left.prologue_bytes.data)
		mut epilogue_pointer := &u8(left.epilogue_bytes.data)
		mut xdata_pointer := &u8(left.windows_unwind.xdata_bytes.data)
		mut displacement_pointer := &u8(left.slots[0].address.displacement_le.data)
		prologue_pointer[0] = 0
		epilogue_pointer[0] = 0
		xdata_pointer[0] = 0
		displacement_pointer[0] = 0
	}
	assert right.prologue_bytes[0] == 0xb8
	assert right.epilogue_bytes[0] == 0x48
	assert right.windows_unwind.xdata_bytes[0] == 0x01
	assert right.slots[0].address.displacement_le[0] == 32
}
