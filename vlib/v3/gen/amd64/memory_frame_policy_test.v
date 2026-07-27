module amd64

fn memory_frame_policy_slot(size u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              9
		kind:            .local
		size_bytes:      size
		alignment_bytes: 8
	}
}

fn memory_frame_policy_facts(profile TargetProfile, has_call bool, call_extent u64, extent_kind MemoryFrameExtentKind, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       77
		profile:           profile
		extent_kind:       extent_kind
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_policy_fixed(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return memory_frame_policy_facts(profile, has_call, call_extent, .fixed, slots)
}

fn memory_frame_policy_plan(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) MemoryFramePlan {
	return plan_memory_frame_with_red_zone_policy(facts, policy) or { panic(err.msg()) }
}

fn memory_frame_policy_encoding(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) MemoryFrameEncodingPlan {
	return plan_memory_frame_encoding_with_red_zone_policy(facts, policy) or {
		panic(err.msg())
	}
}

fn memory_frame_policy_legacy_error(facts &MemoryFunctionFrameFacts) string {
	plan_memory_frame(facts) or { return err.msg() }
	return ''
}

fn memory_frame_policy_explicit_error(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) string {
	plan_memory_frame_with_red_zone_policy(facts, policy) or { return err.msg() }
	return ''
}

fn memory_frame_policy_legacy_encoding_error(facts &MemoryFunctionFrameFacts) string {
	plan_memory_frame_encoding(facts) or { return err.msg() }
	return ''
}

fn memory_frame_policy_explicit_encoding_error(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) string {
	plan_memory_frame_encoding_with_red_zone_policy(facts, policy) or {
		return err.msg()
	}
	return ''
}

fn memory_frame_policy_translations(adjustment u64) MemoryStackTranslations {
	return MemoryStackTranslations{
		entry_to_body_subtract_bytes: adjustment
		incoming_from_body_add_bytes: adjustment
		outgoing_from_body_add_bytes: 0
	}
}

fn test_memory_frame_policy_p01_ordinals_and_appended_field_default() {
	assert int(MemoryRedZonePolicy.abi_default) == 0
	assert int(MemoryRedZonePolicy.forbidden) == 1
	source_compatible_literal := MemoryFramePlan{}
	assert source_compatible_literal.red_zone_policy == .abi_default
}

fn test_memory_frame_policy_p02_sysv_default_whole_plan_and_legacy_identity() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_policy_slot(8),
	])
	legacy := plan_memory_frame(&facts) or { panic(err.msg()) }
	explicit := memory_frame_policy_plan(&facts, .abi_default)
	assert legacy == explicit
	assert explicit == MemoryFramePlan{
		function_id:               77
		profile:                   .linux_x86_64_sysv_elf
		extent_kind:               .fixed
		call_extent_bytes:         0
		has_call:                  false
		uses_red_zone:             true
		red_zone_extent_bytes:     8
		non_red_zone_extent_bytes: 8
		stack_adjustment_bytes:    0
		probe_required:            false
		translations:              memory_frame_policy_translations(0)
		slots:                     [
			MemorySlotPlacement{
				id:                 9
				kind:               .local
				basis:              .entry_rsp
				displacement_bytes: -8
				size_bytes:         8
				alignment_bytes:    8
			},
		]
		red_zone_policy:           .abi_default
	}
	assert plan_memory_frame_encoding(&facts) or { panic(err.msg()) } ==
		memory_frame_policy_encoding(&facts, .abi_default)
}

fn test_memory_frame_policy_p03_apple_default_whole_plan_and_legacy_identity() {
	facts := memory_frame_policy_fixed(.macos_x86_64_sysv_macho, false, 0, [
		memory_frame_policy_slot(8),
	])
	legacy := plan_memory_frame(&facts) or { panic(err.msg()) }
	explicit := memory_frame_policy_plan(&facts, .abi_default)
	assert legacy == explicit
	assert explicit.profile == .macos_x86_64_sysv_macho
	assert explicit.uses_red_zone
	assert explicit.red_zone_extent_bytes == 8
	assert explicit.non_red_zone_extent_bytes == 8
	assert explicit.stack_adjustment_bytes == 0
	assert explicit.slots == [
		MemorySlotPlacement{
			id:                 9
			kind:               .local
			basis:              .entry_rsp
			displacement_bytes: -8
			size_bytes:         8
			alignment_bytes:    8
		},
	]
	assert explicit.red_zone_policy == .abi_default
	assert plan_memory_frame_encoding(&facts) or { panic(err.msg()) } ==
		memory_frame_policy_encoding(&facts, .abi_default)
}

fn test_memory_frame_policy_p04_microsoft_default_whole_plan_bytes_and_legacy_identity() {
	facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true,
		40, [])
	legacy := plan_memory_frame(&facts) or { panic(err.msg()) }
	explicit := memory_frame_policy_plan(&facts, .abi_default)
	assert legacy == explicit
	assert explicit == MemoryFramePlan{
		function_id:               77
		profile:                   .windows_x86_64_microsoft_abi_coff
		extent_kind:               .fixed
		call_extent_bytes:         40
		has_call:                  true
		uses_red_zone:             false
		red_zone_extent_bytes:     0
		non_red_zone_extent_bytes: 40
		stack_adjustment_bytes:    40
		probe_required:            false
		translations:              memory_frame_policy_translations(40)
		slots:                     []
		red_zone_policy:           .abi_default
	}
	legacy_encoding := plan_memory_frame_encoding(&facts) or { panic(err.msg()) }
	encoding := memory_frame_policy_encoding(&facts, .abi_default)
	assert legacy_encoding == encoding
	assert encoding.prologue_bytes == [u8(0x48), 0x83, 0xec, 0x28]
	assert encoding.epilogue_bytes == [u8(0x48), 0x83, 0xc4, 0x28]
	assert encoding.windows_unwind.xdata_bytes == [u8(0x01), 0x04, 0x01, 0x00,
		0x04, 0x42, 0x00, 0x00]
}

fn test_memory_frame_policy_p05_legacy_error_strings_are_exactly_preserved() {
	mut too_many := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1025 {
		too_many << MemorySlotRequest{
			id:              u32(index)
			kind:            .local
			size_bytes:      1
			alignment_bytes: 1
		}
	}
	cases := [
		memory_frame_policy_facts(.linux_x86_64_sysv_elf, false, 0, .dynamic, []),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 8, []),
		memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true, 24, []),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, true, 1, []),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
			MemorySlotRequest{
				id:              1
				kind:            .local
				size_bytes:      0
				alignment_bytes: 1
			},
		]),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
			MemorySlotRequest{
				id:              1
				kind:            .local
				size_bytes:      1
				alignment_bytes: 3
			},
		]),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
			MemorySlotRequest{
				id:              1
				kind:            .local
				size_bytes:      1
				alignment_bytes: 1
			},
			MemorySlotRequest{
				id:              1
				kind:            .spill
				size_bytes:      1
				alignment_bytes: 1
			},
		]),
		memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, too_many),
	]
	expected := [
		'amd64 memory frame: dynamic frame extent is unsupported',
		'amd64 memory frame: noncaller call extent must be zero',
		'amd64 memory frame: Microsoft call extent 24 is below 32',
		'amd64 memory frame: call extent 1 is not a multiple of 8',
		'amd64 memory frame: slot 0 id 1 size must be positive',
		'amd64 memory frame: slot 0 id 1 alignment 3 is invalid',
		'amd64 memory frame: duplicate slot id 1',
		'amd64 memory frame: slot count 1025 exceeds 1024',
	]
	for index, facts in cases {
		assert memory_frame_policy_legacy_error(&facts) == expected[index]
		assert memory_frame_policy_explicit_error(&facts, .abi_default) == expected[index]
		assert memory_frame_policy_legacy_encoding_error(&facts) == expected[index]
		assert memory_frame_policy_explicit_encoding_error(&facts, .abi_default) ==
			expected[index]
	}
}

fn test_memory_frame_policy_p06_forbidden_sysv_128_uses_ordinary_geometry() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_policy_slot(128),
	])
	plan := memory_frame_policy_plan(&facts, .forbidden)
	assert plan.red_zone_policy == .forbidden
	assert !plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 0
	assert plan.non_red_zone_extent_bytes == 128
	assert plan.stack_adjustment_bytes == 136
	assert plan.translations == memory_frame_policy_translations(136)
	assert plan.slots == [
		MemorySlotPlacement{
			id:                 9
			kind:               .local
			basis:              .body_rsp
			displacement_bytes: 0
			size_bytes:         128
			alignment_bytes:    8
		},
	]
}

fn test_memory_frame_policy_p07_forbidden_apple_128_uses_ordinary_geometry() {
	facts := memory_frame_policy_fixed(.macos_x86_64_sysv_macho, false, 0, [
		memory_frame_policy_slot(128),
	])
	plan := memory_frame_policy_plan(&facts, .forbidden)
	assert plan.profile == .macos_x86_64_sysv_macho
	assert plan.red_zone_policy == .forbidden
	assert !plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 0
	assert plan.non_red_zone_extent_bytes == 128
	assert plan.stack_adjustment_bytes == 136
	assert plan.translations == memory_frame_policy_translations(136)
	assert plan.slots[0].basis == .body_rsp
	assert plan.slots[0].displacement_bytes == 0
}

fn test_memory_frame_policy_p08_default_sysv_128_is_exact_red_zone_endpoint() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_policy_slot(128),
	])
	plan := memory_frame_policy_plan(&facts, .abi_default)
	assert plan.red_zone_policy == .abi_default
	assert plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 128
	assert plan.non_red_zone_extent_bytes == 128
	assert plan.stack_adjustment_bytes == 0
	assert plan.translations == memory_frame_policy_translations(0)
	assert plan.slots[0].basis == .entry_rsp
	assert plan.slots[0].displacement_bytes == -128
}

fn test_memory_frame_policy_p09_129_falls_back_to_same_ordinary_geometry() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_policy_slot(129),
	])
	default_plan := memory_frame_policy_plan(&facts, .abi_default)
	forbidden_plan := memory_frame_policy_plan(&facts, .forbidden)
	assert !default_plan.uses_red_zone
	assert !forbidden_plan.uses_red_zone
	assert default_plan.non_red_zone_extent_bytes == 129
	assert default_plan.stack_adjustment_bytes == 136
	assert default_plan.slots[0].basis == .body_rsp
	assert default_plan.slots[0].displacement_bytes == 0
	assert forbidden_plan.function_id == default_plan.function_id
	assert forbidden_plan.profile == default_plan.profile
	assert forbidden_plan.extent_kind == default_plan.extent_kind
	assert forbidden_plan.call_extent_bytes == default_plan.call_extent_bytes
	assert forbidden_plan.has_call == default_plan.has_call
	assert forbidden_plan.uses_red_zone == default_plan.uses_red_zone
	assert forbidden_plan.red_zone_extent_bytes == default_plan.red_zone_extent_bytes
	assert forbidden_plan.non_red_zone_extent_bytes == default_plan.non_red_zone_extent_bytes
	assert forbidden_plan.stack_adjustment_bytes == default_plan.stack_adjustment_bytes
	assert forbidden_plan.probe_required == default_plan.probe_required
	assert forbidden_plan.translations == default_plan.translations
	assert forbidden_plan.slots == default_plan.slots
	assert default_plan.red_zone_policy == .abi_default
	assert forbidden_plan.red_zone_policy == .forbidden
}

fn test_memory_frame_policy_p10_empty_noncaller_remains_d0_without_forged_call() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [])
	default_plan := memory_frame_policy_plan(&facts, .abi_default)
	forbidden_plan := memory_frame_policy_plan(&facts, .forbidden)
	for plan in [default_plan, forbidden_plan] {
		assert !plan.has_call
		assert plan.call_extent_bytes == 0
		assert !plan.uses_red_zone
		assert plan.red_zone_extent_bytes == 0
		assert plan.non_red_zone_extent_bytes == 0
		assert plan.stack_adjustment_bytes == 0
		assert plan.translations == memory_frame_policy_translations(0)
		assert plan.slots.len == 0
	}
}

fn test_memory_frame_policy_p11_sysv_caller_preserves_call_facts_and_d8() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, true, 8, [])
	default_plan := memory_frame_policy_plan(&facts, .abi_default)
	forbidden_plan := memory_frame_policy_plan(&facts, .forbidden)
	for plan in [default_plan, forbidden_plan] {
		assert plan.has_call
		assert plan.call_extent_bytes == 8
		assert !plan.uses_red_zone
		assert plan.non_red_zone_extent_bytes == 8
		assert plan.stack_adjustment_bytes == 8
		assert plan.translations == memory_frame_policy_translations(8)
	}
}

fn test_memory_frame_policy_p12_windows_4088_policy_changes_no_geometry() {
	facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true,
		4088, [])
	default_plan := memory_frame_policy_plan(&facts, .abi_default)
	forbidden_plan := memory_frame_policy_plan(&facts, .forbidden)
	for plan in [default_plan, forbidden_plan] {
		assert plan.has_call
		assert plan.call_extent_bytes == 4088
		assert !plan.uses_red_zone
		assert plan.non_red_zone_extent_bytes == 4088
		assert plan.stack_adjustment_bytes == 4088
		assert !plan.probe_required
		assert plan.translations == memory_frame_policy_translations(4088)
	}
}

fn test_memory_frame_policy_p13_windows_4104_policy_changes_no_geometry() {
	facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true,
		4104, [])
	default_plan := memory_frame_policy_plan(&facts, .abi_default)
	forbidden_plan := memory_frame_policy_plan(&facts, .forbidden)
	for plan in [default_plan, forbidden_plan] {
		assert plan.has_call
		assert plan.call_extent_bytes == 4104
		assert !plan.uses_red_zone
		assert plan.non_red_zone_extent_bytes == 4104
		assert plan.stack_adjustment_bytes == 4104
		assert plan.probe_required
		assert plan.translations == memory_frame_policy_translations(4104)
	}
}

fn test_memory_frame_policy_p14_forbidden_sysv_m2_d136_bytes_and_address() {
	facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_policy_slot(128),
	])
	plan := memory_frame_policy_encoding(&facts, .forbidden)
	assert plan.frame.red_zone_policy == .forbidden
	assert plan.frame.stack_adjustment_bytes == 136
	assert plan.prologue_kind == .sub_imm32
	assert plan.prologue_bytes == [u8(0x48), 0x81, 0xec, 0x88, 0x00, 0x00, 0x00]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x88, 0x00, 0x00, 0x00]
	assert plan.body_offset_bytes == 7
	assert plan.entry_cfa_offset_bytes == 8
	assert plan.body_cfa_offset_bytes == 144
	assert !plan.probe_fixup.present
	assert !plan.chkstk.present
	assert !plan.windows_unwind.present
	assert plan.slots.len == 1
	assert plan.slots[0].source_placement_index == 0
	assert plan.slots[0].placement == plan.frame.slots[0]
	assert plan.slots[0].address == MemoryRspAddressEncoding{
		basis:              .body_rsp
		displacement_bytes: 0
		kind:               .zero
		mod_bits:           0
		rm_bits:            4
		sib_scale_bits:     0
		sib_index_bits:     4
		sib_base_bits:      4
		displacement_le:    []
	}
}

fn test_memory_frame_policy_p15_windows_4088_m2_bytes_and_unwind() {
	facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true,
		4088, [])
	plan := memory_frame_policy_encoding(&facts, .forbidden)
	assert plan.frame.red_zone_policy == .forbidden
	assert plan.prologue_kind == .sub_imm32
	assert plan.prologue_bytes == [u8(0x48), 0x81, 0xec, 0xf8, 0x0f, 0x00, 0x00]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0xf8, 0x0f, 0x00, 0x00]
	assert plan.body_offset_bytes == 7
	assert plan.body_cfa_offset_bytes == 4096
	assert !plan.probe_fixup.present
	assert !plan.chkstk.present
	assert plan.windows_unwind.kind == .alloc_large_info0
	assert plan.windows_unwind.count_of_codes == 2
	assert plan.windows_unwind.xdata_bytes == [u8(0x01), 0x07, 0x02, 0x00, 0x07,
		0x01, 0xff, 0x01]
}

fn test_memory_frame_policy_p16_windows_4104_m2_bytes_fixup_contract_and_unwind() {
	facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff, true,
		4104, [])
	plan := memory_frame_policy_encoding(&facts, .forbidden)
	assert plan.frame.red_zone_policy == .forbidden
	assert plan.prologue_kind == .windows_chkstk
	assert plan.prologue_bytes == [u8(0xb8), 0x08, 0x10, 0x00, 0x00, 0xe8, 0x00,
		0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
	assert plan.epilogue_bytes == [u8(0x48), 0x81, 0xc4, 0x08, 0x10, 0x00, 0x00]
	assert plan.body_offset_bytes == 13
	assert plan.body_cfa_offset_bytes == 4112
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
		allocation_bytes:             4104
		save_push_count:              0
		shadow_space_bytes:           0
		pre_call_rsp_mod_16:          8
		helper_entry_rsp_mod_16:      0
		eax_zero_extends_rax:         true
		helper_preserves_rax:         true
		rax_after_prologue:           4104
		clobbers_r10:                 true
		clobbers_r11:                 true
		clobbers_eflags:              true
		preserves_other_integer_gprs: true
	}
	assert plan.windows_unwind.kind == .alloc_large_info0
	assert plan.windows_unwind.count_of_codes == 2
	assert plan.windows_unwind.xdata_bytes == [u8(0x01), 0x0d, 0x02, 0x00, 0x0d,
		0x01, 0x01, 0x02]
}

fn test_memory_frame_policy_p17_forged_policy_mixed_invalid_precedence_and_m2_forwarding() {
	bad_profile := unsafe { TargetProfile(255) }
	bad_extent := unsafe { MemoryFrameExtentKind(255) }
	bad_slot_kind := unsafe { MemorySlotKind(255) }
	bad_policy := unsafe { MemoryRedZonePolicy(255) }
	bad_slot := MemorySlotRequest{
		id:              1
		kind:            bad_slot_kind
		size_bytes:      0
		alignment_bytes: 0
	}
	profile_first := memory_frame_policy_facts(bad_profile, false, 8, bad_extent,
		[bad_slot])
	assert memory_frame_policy_explicit_error(&profile_first, bad_policy) ==
		'amd64 memory frame: unsupported target profile'
	extent_second := memory_frame_policy_facts(.linux_x86_64_sysv_elf, false, 8,
		bad_extent, [bad_slot])
	assert memory_frame_policy_explicit_error(&extent_second, bad_policy) ==
		'amd64 memory frame: unsupported frame extent kind'
	slot_third := memory_frame_policy_facts(.linux_x86_64_sysv_elf, false, 8, .fixed,
		[bad_slot])
	assert memory_frame_policy_explicit_error(&slot_third, bad_policy) ==
		'amd64 memory frame: slot 0 has unsupported kind'

	mut too_many := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1025 {
		too_many << MemorySlotRequest{
			id:              u32(index)
			kind:            .local
			size_bytes:      0
			alignment_bytes: 0
		}
	}
	policy_before_cap := memory_frame_policy_facts(.linux_x86_64_sysv_elf, false,
		8, .dynamic, too_many)
	expected := 'amd64 memory frame: unsupported red-zone policy'
	assert memory_frame_policy_explicit_error(&policy_before_cap, bad_policy) == expected
	assert memory_frame_policy_explicit_encoding_error(&policy_before_cap, bad_policy) ==
		expected
}

fn test_memory_frame_policy_p18_caps_overflow_transactionality_and_fresh_storage() {
	mut slots := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1024 {
		slots << MemorySlotRequest{
			id:              u32(index)
			kind:            .local
			size_bytes:      1
			alignment_bytes: 1
		}
	}
	at_cap_facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0,
		slots)
	at_cap := memory_frame_policy_plan(&at_cap_facts, .forbidden)
	assert at_cap.slots.len == 1024
	assert at_cap.non_red_zone_extent_bytes == 1024
	assert at_cap.stack_adjustment_bytes == 1032
	slots << MemorySlotRequest{
		id:              1024
		kind:            .local
		size_bytes:      1
		alignment_bytes: 1
	}
	over_cap_facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, false, 0,
		slots)
	assert memory_frame_policy_explicit_error(&over_cap_facts, .forbidden) ==
		'amd64 memory frame: slot count 1025 exceeds 1024'
	assert slots.len == 1025

	overflow_facts := memory_frame_policy_fixed(.linux_x86_64_sysv_elf, true,
		max_u64 - 7, [])
	assert memory_frame_policy_explicit_error(&overflow_facts, .forbidden) ==
		'amd64 memory frame: arithmetic overflow'
	assert overflow_facts.call_extent_bytes == max_u64 - 7
	assert overflow_facts.has_call

	mut source_slots := [
		MemorySlotRequest{
			id:              9
			kind:            .local
			size_bytes:      4064
			alignment_bytes: 1
		},
	]
	left_facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 32, source_slots)
	right_facts := memory_frame_policy_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 32, source_slots)
	mut left := memory_frame_policy_encoding(&left_facts, .abi_default)
	right := memory_frame_policy_encoding(&right_facts, .abi_default)
	source_slots[0] = memory_frame_policy_slot(1)
	assert left == right
	unsafe {
		assert left.frame.slots.data != right.frame.slots.data
		assert left.slots.data != right.slots.data
		assert left.prologue_bytes.data != right.prologue_bytes.data
		assert left.epilogue_bytes.data != right.epilogue_bytes.data
		assert left.windows_unwind.xdata_bytes.data != right.windows_unwind.xdata_bytes.data
		assert left.slots[0].address.displacement_le.data !=
			right.slots[0].address.displacement_le.data
		mut prologue := &u8(left.prologue_bytes.data)
		mut epilogue := &u8(left.epilogue_bytes.data)
		mut xdata := &u8(left.windows_unwind.xdata_bytes.data)
		mut displacement := &u8(left.slots[0].address.displacement_le.data)
		prologue[0] = 0
		epilogue[0] = 0
		xdata[0] = 0
		displacement[0] = 0
	}
	assert right.prologue_bytes[0] == 0xb8
	assert right.epilogue_bytes[0] == 0x48
	assert right.windows_unwind.xdata_bytes[0] == 0x01
	assert right.slots[0].address.displacement_le[0] == 32
}
