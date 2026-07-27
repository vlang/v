module amd64

fn memory_frame_test_facts(profile TargetProfile, function_id u32, has_call bool, call_extent u64, extent_kind MemoryFrameExtentKind, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return MemoryFunctionFrameFacts{
		function_id:       function_id
		profile:           profile
		extent_kind:       extent_kind
		call_extent_bytes: call_extent
		has_call:          has_call
		slots:             slots.clone()
	}
}

fn memory_frame_test_fixed(profile TargetProfile, has_call bool, call_extent u64, slots []MemorySlotRequest) MemoryFunctionFrameFacts {
	return memory_frame_test_facts(profile, 17, has_call, call_extent, .fixed, slots)
}

fn memory_frame_test_slot(id u32, kind MemorySlotKind, size u64, alignment u64) MemorySlotRequest {
	return MemorySlotRequest{
		id:              id
		kind:            kind
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn memory_frame_test_plan(facts &MemoryFunctionFrameFacts) MemoryFramePlan {
	return plan_memory_frame(facts) or { panic(err.msg()) }
}

fn memory_frame_test_expect_error(facts &MemoryFunctionFrameFacts, expected string) {
	if _ := plan_memory_frame(facts) {
		assert false, 'expected `${expected}`'
	} else {
		assert err.msg() == 'amd64 memory frame: ${expected}'
	}
}

fn memory_frame_test_assert_translations(plan &MemoryFramePlan, adjustment u64) {
	assert plan.translations == MemoryStackTranslations{
		entry_to_body_subtract_bytes: adjustment
		incoming_from_body_add_bytes: adjustment
		outgoing_from_body_add_bytes: 0
	}
}

fn test_memory_frame_m01_empty_fixed_noncaller_all_profiles() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		facts := memory_frame_test_fixed(profile, false, 0, [])
		plan := memory_frame_test_plan(&facts)
		assert plan.function_id == 17
		assert plan.profile == profile
		assert plan.extent_kind == .fixed
		assert plan.call_extent_bytes == 0
		assert !plan.has_call
		assert !plan.uses_red_zone
		assert plan.red_zone_extent_bytes == 0
		assert plan.non_red_zone_extent_bytes == 0
		assert plan.stack_adjustment_bytes == 0
		assert !plan.probe_required
		assert plan.slots.len == 0
		memory_frame_test_assert_translations(&plan, 0)
	}
}

fn test_memory_frame_m02_sysv_apple_call_only_uses_exact_eight_byte_adjustment() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho] {
		facts := memory_frame_test_fixed(profile, true, 0, [])
		plan := memory_frame_test_plan(&facts)
		assert plan.non_red_zone_extent_bytes == 0
		assert plan.stack_adjustment_bytes == 8
		assert !plan.uses_red_zone
		assert !plan.probe_required
		memory_frame_test_assert_translations(&plan, 8)
	}
}

fn test_memory_frame_m03_microsoft_call_only_uses_exact_forty_byte_adjustment() {
	facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, true, 32,
		[])
	plan := memory_frame_test_plan(&facts)
	assert plan.call_extent_bytes == 32
	assert plan.non_red_zone_extent_bytes == 32
	assert plan.stack_adjustment_bytes == 40
	assert !plan.uses_red_zone
	assert !plan.probe_required
	memory_frame_test_assert_translations(&plan, 40)
}

fn test_memory_frame_m04_microsoft_five_six_seven_argument_high_waters_are_unrounded() {
	for case in [
		[u64(40), u64(40)],
		[u64(48), u64(56)],
		[u64(56), u64(56)],
	] {
		facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, true,
			case[0], [])
		plan := memory_frame_test_plan(&facts)
		assert plan.call_extent_bytes == case[0]
		assert plan.non_red_zone_extent_bytes == case[0]
		assert plan.stack_adjustment_bytes == case[1]
	}
}

fn test_memory_frame_m05_aggregate_temp_is_a_separate_slot_not_part_of_c() {
	slots := [memory_frame_test_slot(9, .aggregate_temp, 16, 16)]
	facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, true, 40,
		slots)
	plan := memory_frame_test_plan(&facts)
	assert plan.call_extent_bytes == 40
	assert plan.slots == [
		MemorySlotPlacement{
			id:                 9
			kind:               .aggregate_temp
			basis:              .body_rsp
			displacement_bytes: 48
			size_bytes:         16
			alignment_bytes:    16
		},
	]
	assert plan.non_red_zone_extent_bytes == 64
	assert plan.stack_adjustment_bytes == 72
}

fn test_memory_frame_m06_forged_raw_enums_precede_every_semantic_check() {
	bad_profile := unsafe { TargetProfile(255) }
	bad_extent := unsafe { MemoryFrameExtentKind(255) }
	bad_slot_kind := unsafe { MemorySlotKind(255) }
	bad_slot := memory_frame_test_slot(1, bad_slot_kind, 0, 0)

	profile_first := memory_frame_test_facts(bad_profile, 1, false, 8, bad_extent,
		[bad_slot])
	memory_frame_test_expect_error(&profile_first, 'unsupported target profile')

	extent_second := memory_frame_test_facts(.linux_x86_64_sysv_elf, 1, false, 8,
		bad_extent, [bad_slot])
	memory_frame_test_expect_error(&extent_second, 'unsupported frame extent kind')

	slot_third := memory_frame_test_facts(.linux_x86_64_sysv_elf, 1, false, 8, .fixed,
		[bad_slot])
	memory_frame_test_expect_error(&slot_third, 'slot 0 has unsupported kind')
}

fn test_memory_frame_m07_slot_cap_boundary_precedes_planner_allocation() {
	mut slots := []MemorySlotRequest{cap: 1025}
	for index in 0 .. 1024 {
		slots << memory_frame_test_slot(u32(index), .local, 1, 1)
	}
	at_cap := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0, slots)
	plan := memory_frame_test_plan(&at_cap)
	assert plan.slots.len == 1024
	assert plan.non_red_zone_extent_bytes == 1024
	assert plan.stack_adjustment_bytes == 1032

	slots << memory_frame_test_slot(1024, .local, 1, 1)
	over_cap := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0, slots)
	memory_frame_test_expect_error(&over_cap, 'slot count 1025 exceeds 1024')
}

fn test_memory_frame_m08_fixed_dynamic_and_call_fact_precedence_is_stable() {
	dynamic_bad_call := memory_frame_test_facts(.linux_x86_64_sysv_elf, 1, false,
		8, .dynamic, [])
	memory_frame_test_expect_error(&dynamic_bad_call, 'dynamic frame extent is unsupported')

	noncaller_extent := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 8,
		[])
	memory_frame_test_expect_error(&noncaller_extent, 'noncaller call extent must be zero')

	microsoft_floor := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff,
		true, 24, [])
	memory_frame_test_expect_error(&microsoft_floor, 'Microsoft call extent 24 is below 32')

	sysv_alignment := memory_frame_test_fixed(.linux_x86_64_sysv_elf, true, 1, [])
	memory_frame_test_expect_error(&sysv_alignment,
		'call extent 1 is not a multiple of 8')
}

fn test_memory_frame_m09_positive_size_and_alignment_validation_is_in_caller_order() {
	zero_first := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_test_slot(7, .local, 0, 8),
		memory_frame_test_slot(8, .local, 1, 0),
	])
	memory_frame_test_expect_error(&zero_first, 'slot 0 id 7 size must be positive')

	for alignment in [u64(0), 3, 32] {
		invalid_alignment := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false,
			0, [memory_frame_test_slot(8, .local, 1, alignment)])
		memory_frame_test_expect_error(&invalid_alignment,
			'slot 0 id 8 alignment ${alignment} is invalid')
	}
}

fn test_memory_frame_m10_duplicate_id_is_global_across_different_kinds() {
	facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, false, 0, [
		memory_frame_test_slot(44, .local, 8, 8),
		memory_frame_test_slot(44, .aggregate_temp, 16, 16),
	])
	memory_frame_test_expect_error(&facts, 'duplicate slot id 44')
}

fn test_memory_frame_m11_kind_id_sorting_and_deep_snapshot_ignore_input_order() {
	mut left_input := [
		memory_frame_test_slot(9, .aggregate_temp, 1, 1),
		memory_frame_test_slot(5, .local, 1, 1),
		memory_frame_test_slot(2, .spill, 1, 1),
		memory_frame_test_slot(1, .local, 1, 1),
		memory_frame_test_slot(3, .fixed_alloca, 1, 1),
	]
	right_input := left_input.clone()
	left_facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, false,
		0, left_input)
	left_plan := memory_frame_test_plan(&left_facts)
	left_input[0] = memory_frame_test_slot(99, .local, 8, 8)
	right_facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, false,
		0, right_input)
	right_plan := memory_frame_test_plan(&right_facts)

	assert left_plan.slots == right_plan.slots
	assert left_plan.slots.map(it.id) == [u32(1), 5, 2, 3, 9]
	assert left_plan.slots.map(it.kind) == [
		MemorySlotKind.local,
		.local,
		.spill,
		.fixed_alloca,
		.aggregate_temp,
	]
	unsafe {
		assert left_plan.slots.data != right_plan.slots.data
	}
}

fn test_memory_frame_m12_ordinary_mixed_alignment_starts_at_unrounded_c() {
	facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff, true, 40, [
		memory_frame_test_slot(1, .local, 1, 1),
		memory_frame_test_slot(2, .spill, 8, 8),
		memory_frame_test_slot(3, .fixed_alloca, 16, 16),
		memory_frame_test_slot(4, .aggregate_temp, 3, 4),
	])
	plan := memory_frame_test_plan(&facts)
	assert plan.slots.map(it.displacement_bytes) == [i64(40), 48, 64, 80]
	assert plan.slots.map(it.basis) == [
		MemorySlotBasis.body_rsp,
		.body_rsp,
		.body_rsp,
		.body_rsp,
	]
	assert plan.non_red_zone_extent_bytes == 83
	assert plan.stack_adjustment_bytes == 88
}

fn test_memory_frame_m13_sysv_red_recurrence_proves_alignment_containment_and_separation() {
	facts := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_test_slot(1, .local, 1, 1),
		memory_frame_test_slot(2, .spill, 8, 8),
		memory_frame_test_slot(3, .fixed_alloca, 16, 16),
	])
	plan := memory_frame_test_plan(&facts)
	assert plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 40
	assert plan.non_red_zone_extent_bytes == 32
	assert plan.stack_adjustment_bytes == 0
	assert plan.slots.map(it.displacement_bytes) == [i64(-1), -16, -40]
	for slot in plan.slots {
		depth := u64(-slot.displacement_bytes)
		assert depth <= 128
		assert depth % slot.alignment_bytes == u64(8) % slot.alignment_bytes
		assert slot.displacement_bytes + i64(slot.size_bytes) <= 0
	}
	assert plan.slots[1].displacement_bytes + i64(plan.slots[1].size_bytes) <=
		plan.slots[0].displacement_bytes
	assert plan.slots[2].displacement_bytes + i64(plan.slots[2].size_bytes) <=
		plan.slots[1].displacement_bytes
}

fn test_memory_frame_m14_apple_exact_red_zone_depth_128_is_accepted() {
	facts := memory_frame_test_fixed(.macos_x86_64_sysv_macho, false, 0, [
		memory_frame_test_slot(1, .local, 128, 8),
	])
	plan := memory_frame_test_plan(&facts)
	assert plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 128
	assert plan.slots[0].basis == .entry_rsp
	assert plan.slots[0].displacement_bytes == -128
	assert plan.slots[0].displacement_bytes + i64(plan.slots[0].size_bytes) == 0
	assert plan.stack_adjustment_bytes == 0
}

fn test_memory_frame_m15_red_failure_discards_partial_work_and_restarts_ordinary() {
	facts := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0, [
		memory_frame_test_slot(1, .local, 8, 8),
		memory_frame_test_slot(2, .spill, 121, 1),
	])
	plan := memory_frame_test_plan(&facts)
	assert !plan.uses_red_zone
	assert plan.red_zone_extent_bytes == 0
	assert plan.slots.map(it.basis) == [MemorySlotBasis.body_rsp, .body_rsp]
	assert plan.slots.map(it.displacement_bytes) == [i64(0), 8]
	assert plan.non_red_zone_extent_bytes == 129
	assert plan.stack_adjustment_bytes == 136
}

fn test_memory_frame_m16_calls_disable_red_zone_and_microsoft_never_uses_it() {
	sysv_facts := memory_frame_test_fixed(.linux_x86_64_sysv_elf, true, 8, [
		memory_frame_test_slot(1, .local, 8, 8),
	])
	sysv := memory_frame_test_plan(&sysv_facts)
	assert !sysv.uses_red_zone
	assert sysv.slots[0].basis == .body_rsp
	assert sysv.slots[0].displacement_bytes == 8
	assert sysv.stack_adjustment_bytes == 24

	windows_facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, [memory_frame_test_slot(1, .local, 8, 8)])
	windows := memory_frame_test_plan(&windows_facts)
	assert !windows.uses_red_zone
	assert windows.slots[0].basis == .body_rsp
	assert windows.slots[0].displacement_bytes == 0
	assert windows.stack_adjustment_bytes == 8
}

fn test_memory_frame_m17_stack_adjustment_endpoint_first_exceed_and_overflow_are_exact() {
	at_limit_slots := [
		memory_frame_test_slot(1, .local, u64(0x7ffffff8), 1),
	]
	at_limit := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0,
		at_limit_slots)
	limit_plan := memory_frame_test_plan(&at_limit)
	assert limit_plan.non_red_zone_extent_bytes == u64(0x7ffffff8)
	assert limit_plan.stack_adjustment_bytes == u64(0x7ffffff8)
	assert limit_plan.slots[0].displacement_bytes == 0

	first_exceed_slots := [
		memory_frame_test_slot(1, .local, u64(0x7ffffff9), 1),
	]
	first_exceed := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0,
		first_exceed_slots)
	memory_frame_test_expect_error(&first_exceed,
		'stack adjustment 2147483656 exceeds 2147483640')

	overflow_slots := [memory_frame_test_slot(1, .local, max_u64, 1)]
	overflow := memory_frame_test_fixed(.linux_x86_64_sysv_elf, false, 0,
		overflow_slots)
	memory_frame_test_expect_error(&overflow, 'arithmetic overflow')
	assert at_limit_slots[0].size_bytes == u64(0x7ffffff8)
	assert first_exceed_slots[0].size_bytes == u64(0x7ffffff9)
	assert overflow_slots[0].size_bytes == max_u64
}

fn test_memory_frame_m18_probe_threshold_is_windows_only_and_translations_are_derived() {
	windows_small_facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, [memory_frame_test_slot(1, .local, 4088, 1)])
	windows_small := memory_frame_test_plan(&windows_small_facts)
	assert windows_small.stack_adjustment_bytes == 4088
	assert !windows_small.probe_required
	memory_frame_test_assert_translations(&windows_small, 4088)

	windows_large_facts := memory_frame_test_fixed(.windows_x86_64_microsoft_abi_coff,
		false, 0, [memory_frame_test_slot(1, .local, 4089, 1)])
	windows_large := memory_frame_test_plan(&windows_large_facts)
	assert windows_large.stack_adjustment_bytes == 4104
	assert windows_large.probe_required
	memory_frame_test_assert_translations(&windows_large, 4104)

	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho] {
		facts := memory_frame_test_fixed(profile, false, 0, [
			memory_frame_test_slot(1, .local, 4089, 1),
		])
		plan := memory_frame_test_plan(&facts)
		assert plan.stack_adjustment_bytes == 4104
		assert !plan.probe_required
		memory_frame_test_assert_translations(&plan, 4104)
	}
}
