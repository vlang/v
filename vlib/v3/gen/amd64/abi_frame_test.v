module amd64

import v3.ssa

fn abi_frame_test_add_function(mut type_store ssa.TypeStore, parameters []ssa.TypeID, return_type ssa.TypeID) ssa.TypeID {
	return type_store.register(ssa.Type{
		kind:     .func_t
		params:   parameters.clone()
		ret_type: return_type
	})
}

fn abi_frame_test_add_struct(mut type_store ssa.TypeStore, fields []ssa.TypeID, is_c_struct bool) ssa.TypeID {
	return type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      fields.clone()
		is_c_struct: is_c_struct
	})
}

fn abi_frame_test_plan(profile TargetProfile, type_store &ssa.TypeStore, layouts &AbiLayoutSnapshot, proofs &AbiMicrosoftUdtEvidence, function_type ssa.TypeID) !AbiPlannedCall {
	return classify_and_plan_abi_call(profile, .prototyped, type_store, layouts, proofs,
		function_type)!
}

fn abi_frame_test_default_plan(profile TargetProfile, type_store &ssa.TypeStore, function_type ssa.TypeID) !AbiPlannedCall {
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	return abi_frame_test_plan(profile, type_store, &layouts, &proofs, function_type)!
}

fn abi_frame_test_assert_plan(planned &AbiPlannedCall, profile TargetProfile, function_type ssa.TypeID, outgoing_size int, temporary_offset int, temporary_size int, inter_padding int, tail_padding int, total int) {
	expected_abi := if profile == .windows_x86_64_microsoft_abi_coff {
		AbiKind.microsoft_x64
	} else {
		AbiKind.sysv_amd64
	}
	assert planned.frame == AbiCallFramePlan{
		profile:                              profile
		abi:                                  expected_abi
		function_type:                        function_type
		outgoing_area_offset_bytes:           0
		outgoing_area_size_bytes:             outgoing_size
		indirect_temporary_area_offset_bytes: temporary_offset
		indirect_temporary_area_size_bytes:   temporary_size
		inter_area_padding_bytes:             inter_padding
		tail_alignment_padding_bytes:         tail_padding
		total_allocation_bytes:               total
		required_pre_call_alignment_bytes:    16
	}
}

fn abi_frame_test_deep_clone_types(types []ssa.Type) []ssa.Type {
	mut snapshot := []ssa.Type{cap: types.len}
	for typ in types {
		snapshot << ssa.Type{
			kind:        typ.kind
			width:       typ.width
			is_unsigned: typ.is_unsigned
			elem_type:   typ.elem_type
			len:         typ.len
			fields:      typ.fields.clone()
			field_names: typ.field_names.clone()
			params:      typ.params.clone()
			ret_type:    typ.ret_type
			is_c_struct: typ.is_c_struct
			is_union:    typ.is_union
		}
	}
	return snapshot
}

fn abi_frame_test_expect_public_error(profile TargetProfile, call_kind AbiCallKind, type_store &ssa.TypeStore, layouts &AbiLayoutSnapshot, proofs &AbiMicrosoftUdtEvidence, function_type ssa.TypeID, code string) {
	if _ := classify_and_plan_abi_call(profile, call_kind, type_store, layouts, proofs,
		function_type) {
		assert false, 'expected amd64 ABI error `${code}`'
	} else {
		assert err.msg() == 'amd64 ABI: ${code}'
	}
}

fn abi_frame_test_none_location() AbiLocation {
	return AbiLocation{
		kind:     .none
		register: .none
		class:    .no_class
	}
}

fn abi_frame_test_gpr(register AbiRegister, home bool, home_offset int) AbiLocation {
	return AbiLocation{
		kind:                     .gpr
		register:                 register
		class:                    .integer
		width_bytes:              8
		has_home_address:         home
		caller_home_offset_bytes: home_offset
		callee_home_offset_bytes: if home { home_offset + 8 } else { 0 }
	}
}

fn abi_frame_test_no_value() AbiValueDecision {
	return AbiValueDecision{
		type_id:         0
		mode:            .no_value
		alignment_bytes: 1
	}
}

fn abi_frame_test_absent_sret() AbiHiddenSretDecision {
	return AbiHiddenSretDecision{
		input: abi_frame_test_none_location()
		echo:  abi_frame_test_none_location()
	}
}

fn abi_frame_test_synthetic_decision(profile TargetProfile, function_type ssa.TypeID, outgoing_size int, temporary_size int, parameters []AbiValueDecision) AbiFunctionDecision {
	is_microsoft := profile == .windows_x86_64_microsoft_abi_coff
	return AbiFunctionDecision{
		profile:                               profile
		abi:                                   if is_microsoft { .microsoft_x64 } else { .sysv_amd64 }
		call_kind:                             .prototyped
		function_type:                         function_type
		return_value:                          abi_frame_test_no_value()
		parameters:                            parameters.clone()
		hidden_sret:                           abi_frame_test_absent_sret()
		shadow_space_bytes:                    if is_microsoft { 32 } else { 0 }
		red_zone_bytes:                        if is_microsoft { 0 } else { 128 }
		minimum_outgoing_area_bytes:           outgoing_size
		minimum_indirect_temporary_area_bytes: temporary_size
		pre_call_stack_alignment_bytes:        16
	}
}

fn abi_frame_test_synthetic_indirect_parameter(offset int, size int, recorded_size int, alignment int) AbiValueDecision {
	return AbiValueDecision{
		type_id:                            2
		mode:                               .indirect
		size_bytes:                         size
		alignment_bytes:                    8
		classes:                            [.memory]
		locations:                          [abi_frame_test_gpr(.rcx, true, 0)]
		has_indirect_temporary:             true
		indirect_temporary_offset_bytes:    offset
		indirect_temporary_size_bytes:      recorded_size
		indirect_temporary_alignment_bytes: alignment
	}
}

fn abi_frame_test_expect_private_error(profile TargetProfile, call_kind AbiCallKind, function_type ssa.TypeID, decision &AbiFunctionDecision, code string) {
	if _ := abi_plan_call_frame(profile, call_kind, function_type, decision) {
		assert false, 'expected private frame error `${code}`'
	} else {
		assert err.msg() == 'amd64 ABI: ${code}'
	}
}

fn abi_frame_test_assert_location_geometry(planned &AbiPlannedCall) {
	first_stack := if planned.frame.abi == .microsoft_x64 { 32 } else { 0 }
	for parameter in planned.decision.parameters {
		for location in parameter.locations {
			if location.has_stack_address {
				assert location.caller_stack_offset_bytes >= first_stack
				assert location.callee_stack_offset_bytes == location.caller_stack_offset_bytes + 8
				assert location.caller_stack_offset_bytes + location.width_bytes <= planned.frame.outgoing_area_size_bytes
			}
			if location.has_home_address {
				assert planned.frame.abi == .microsoft_x64
				assert location.caller_home_offset_bytes in [0, 8, 16, 24]
				assert location.callee_home_offset_bytes == location.caller_home_offset_bytes + 8
			}
		}
	}
	for location in planned.decision.return_value.locations {
		assert !location.has_home_address
	}
	assert !planned.decision.hidden_sret.echo.has_home_address
}

fn test_abi_frame_f01_linux_empty() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_frame_test_add_function(mut type_store, [], 0)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 0, 0,
		0, 0, 0, 0)
	assert planned.decision.shadow_space_bytes == 0
	assert planned.decision.red_zone_bytes == 128
}

fn test_abi_frame_f02_macos_empty() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_frame_test_add_function(mut type_store, [], 0)
	planned := abi_frame_test_default_plan(.macos_x86_64_sysv_macho, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .macos_x86_64_sysv_macho, function_type, 0,
		0, 0, 0, 0, 0)
	assert planned.decision.shadow_space_bytes == 0
	assert planned.decision.red_zone_bytes == 128
}

fn test_abi_frame_f03_windows_empty() {
	mut type_store := ssa.TypeStore.new()
	function_type := abi_frame_test_add_function(mut type_store, [], 0)
	planned := abi_frame_test_default_plan(.windows_x86_64_microsoft_abi_coff,
		&type_store, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 32, 0, 0, 0, 0, 32)
	assert planned.decision.shadow_space_bytes == 32
	assert planned.decision.red_zone_bytes == 0
}

fn test_abi_frame_f04_linux_seventh_i64() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 7, init: i64_type}, 0)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 8, 0,
		0, 0, 8, 16)
	location := planned.decision.parameters[6].locations[0]
	assert location.has_stack_address
	assert location.caller_stack_offset_bytes == 0
	assert location.callee_stack_offset_bytes == 8
}

fn test_abi_frame_f05_linux_eighth_i64() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 8, init: i64_type}, 0)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 16,
		0, 0, 0, 0, 16)
	assert planned.decision.parameters[6].locations[0].caller_stack_offset_bytes == 0
	assert planned.decision.parameters[7].locations[0].caller_stack_offset_bytes == 8
}

fn test_abi_frame_f06_linux_ninth_f64() {
	mut type_store := ssa.TypeStore.new()
	f64_type := type_store.get_float(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 9, init: f64_type}, 0)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 8, 0,
		0, 0, 8, 16)
	location := planned.decision.parameters[8].locations[0]
	assert location.kind == .stack
	assert location.class == .sse
	assert location.caller_stack_offset_bytes == 0
}

fn test_abi_frame_f07_linux_memory_by_value() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	aggregate_type := abi_frame_test_add_struct(mut type_store,
		[i64_type, i64_type, i64_type], false)
	function_type := abi_frame_test_add_function(mut type_store, [aggregate_type], 0)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 24,
		0, 0, 0, 8, 32)
	parameter := planned.decision.parameters[0]
	assert parameter.mode == .memory_by_value
	assert !parameter.has_indirect_temporary
	assert parameter.locations[0].width_bytes == 24
}

fn test_abi_frame_f08_sysv_hidden_sret() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	return_type := abi_frame_test_add_struct(mut type_store, [i64_type, i64_type, i64_type],
		false)
	function_type := abi_frame_test_add_function(mut type_store, [i64_type], return_type)
	planned := abi_frame_test_default_plan(.linux_x86_64_sysv_elf, &type_store,
		function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .linux_x86_64_sysv_elf, function_type, 0, 0,
		0, 0, 0, 0)
	assert planned.decision.hidden_sret.present
	assert planned.decision.hidden_sret.input.register == .rdi
	assert planned.decision.hidden_sret.echo.register == .rax
	assert planned.decision.parameters[0].locations[0].register == .rsi
}

fn test_abi_frame_f09_windows_fifth_scalar() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 5, init: i64_type}, 0)
	planned := abi_frame_test_default_plan(.windows_x86_64_microsoft_abi_coff,
		&type_store, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 40, 0, 0, 0, 8, 48)
	assert planned.decision.parameters[4].locations[0].caller_stack_offset_bytes == 32
}

fn test_abi_frame_f10_windows_sixth_scalar() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 6, init: i64_type}, 0)
	planned := abi_frame_test_default_plan(.windows_x86_64_microsoft_abi_coff,
		&type_store, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 48, 0, 0, 0, 0, 48)
	assert planned.decision.parameters[4].locations[0].caller_stack_offset_bytes == 32
	assert planned.decision.parameters[5].locations[0].caller_stack_offset_bytes == 40
}

fn test_abi_frame_f11_windows_one_indirect_udt() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	udt_type := abi_frame_test_add_struct(mut type_store, [i8_type, i8_type, i8_type],
		false)
	function_type := abi_frame_test_add_function(mut type_store, [udt_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     udt_type
			eligibility: .eligible_plain_trivial
		}]
	}
	planned := abi_frame_test_plan(.windows_x86_64_microsoft_abi_coff, &type_store,
		&layouts, &proofs, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 32, 32, 16, 0, 0, 48)
	parameter := planned.decision.parameters[0]
	assert parameter.has_indirect_temporary
	assert parameter.indirect_temporary_offset_bytes == 0
	assert parameter.indirect_temporary_size_bytes == 3
}

fn test_abi_frame_f12_windows_fifth_indirect_udt() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	i8_type := type_store.get_int(8)
	udt_type := abi_frame_test_add_struct(mut type_store, [i8_type, i8_type, i8_type],
		false)
	function_type := abi_frame_test_add_function(mut type_store,
		[i64_type, i64_type, i64_type, i64_type, udt_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     udt_type
			eligibility: .eligible_plain_trivial
		}]
	}
	planned := abi_frame_test_plan(.windows_x86_64_microsoft_abi_coff, &type_store,
		&layouts, &proofs, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 40, 48, 16, 8, 0, 64)
	assert planned.decision.parameters[4].locations[0].caller_stack_offset_bytes == 32
}

fn test_abi_frame_f13_windows_two_indirect_udts() {
	mut type_store := ssa.TypeStore.new()
	i8_type := type_store.get_int(8)
	udt_type := abi_frame_test_add_struct(mut type_store, [i8_type, i8_type, i8_type],
		false)
	function_type := abi_frame_test_add_function(mut type_store, [udt_type, udt_type], 0)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     udt_type
			eligibility: .eligible_plain_trivial
		}]
	}
	planned := abi_frame_test_plan(.windows_x86_64_microsoft_abi_coff, &type_store,
		&layouts, &proofs, function_type) or { panic(err) }
	abi_frame_test_assert_plan(&planned, .windows_x86_64_microsoft_abi_coff,
		function_type, 32, 32, 32, 0, 0, 64)
	assert planned.decision.parameters[0].indirect_temporary_offset_bytes == 0
	assert planned.decision.parameters[1].indirect_temporary_offset_bytes == 16
}

fn test_abi_frame_f14_stack_home_interval_matrix() {
	mut type_store := ssa.TypeStore.new()
	i64_type := type_store.get_int(64)
	function_type := abi_frame_test_add_function(mut type_store,
		[]ssa.TypeID{len: 8, init: i64_type}, 0)
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		planned := abi_frame_test_default_plan(profile, &type_store, function_type) or {
			panic(err)
		}
		abi_frame_test_assert_location_geometry(&planned)
	}
	mut sret_store := ssa.TypeStore.new()
	sret_i64 := sret_store.get_int(64)
	sret_type := abi_frame_test_add_struct(mut sret_store, [sret_i64, sret_i64], false)
	sret_function := abi_frame_test_add_function(mut sret_store, [], sret_type)
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     sret_type
			eligibility: .eligible_plain_trivial
		}]
	}
	sret := abi_frame_test_plan(.windows_x86_64_microsoft_abi_coff, &sret_store,
		&layouts, &proofs, sret_function) or { panic(err) }
	assert sret.decision.hidden_sret.input.has_home_address
	assert sret.decision.hidden_sret.input.caller_home_offset_bytes == 0
	assert !sret.decision.hidden_sret.echo.has_home_address
}

fn test_abi_frame_f15_no_temporary_normalization() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		mut type_store := ssa.TypeStore.new()
		i64_type := type_store.get_int(64)
		function_type := abi_frame_test_add_function(mut type_store, [i64_type], 0)
		layouts := AbiLayoutSnapshot{}
		proofs := AbiMicrosoftUdtEvidence{}
		planned := abi_frame_test_plan(profile, &type_store, &layouts, &proofs,
			function_type) or { panic(err) }
		classified := classify_abi_function(profile, .prototyped, &type_store, &layouts,
			&proofs, function_type) or { panic(err) }
		expected_abi := if profile == .windows_x86_64_microsoft_abi_coff {
			AbiKind.microsoft_x64
		} else {
			AbiKind.sysv_amd64
		}
		expected_outgoing := if profile == .windows_x86_64_microsoft_abi_coff {
			32
		} else {
			0
		}
		assert planned == AbiPlannedCall{
			decision: classified
			frame:    AbiCallFramePlan{
				profile:                           profile
				abi:                               expected_abi
				function_type:                     function_type
				outgoing_area_size_bytes:          expected_outgoing
				total_allocation_bytes:            expected_outgoing
				required_pre_call_alignment_bytes: 16
			}
		}
		assert !planned.decision.return_value.has_indirect_temporary
		assert planned.decision.return_value.indirect_temporary_offset_bytes == 0
		assert planned.decision.return_value.indirect_temporary_size_bytes == 0
		assert planned.decision.return_value.indirect_temporary_alignment_bytes == 0
		for parameter in planned.decision.parameters {
			assert !parameter.has_indirect_temporary
			assert parameter.indirect_temporary_offset_bytes == 0
			assert parameter.indirect_temporary_size_bytes == 0
			assert parameter.indirect_temporary_alignment_bytes == 0
		}
	}
}

fn test_abi_frame_f16_deterministic_nonaliased_snapshot() {
	for profile in [TargetProfile.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho,
		.windows_x86_64_microsoft_abi_coff] {
		mut first_store := ssa.TypeStore.new()
		first_i8 := first_store.get_int(8)
		first_udt := abi_frame_test_add_struct(mut first_store,
			[first_i8, first_i8, first_i8], false)
		first_function := abi_frame_test_add_function(mut first_store, [first_udt, first_udt],
			0)
		mut second_store := ssa.TypeStore.new()
		second_i8 := second_store.get_int(8)
		second_udt := abi_frame_test_add_struct(mut second_store,
			[second_i8, second_i8, second_i8], false)
		second_function := abi_frame_test_add_function(mut second_store,
			[second_udt, second_udt], 0)
		assert first_function == second_function
		first_layouts := AbiLayoutSnapshot{}
		second_layouts := AbiLayoutSnapshot{}
		first_proofs := AbiMicrosoftUdtEvidence{
			proofs: [AbiMicrosoftUdtProof{
				type_id:     first_udt
				eligibility: .eligible_plain_trivial
			}]
		}
		second_proofs := AbiMicrosoftUdtEvidence{
			proofs: [AbiMicrosoftUdtProof{
				type_id:     second_udt
				eligibility: .eligible_plain_trivial
			}]
		}
		first_types_before := abi_frame_test_deep_clone_types(first_store.types)
		first_cache_before := first_store.cache.clone()
		second_types_before := abi_frame_test_deep_clone_types(second_store.types)
		second_cache_before := second_store.cache.clone()
		first := abi_frame_test_plan(profile, &first_store, &first_layouts, &first_proofs,
			first_function) or {
			panic(err)
		}
		second := abi_frame_test_plan(profile, &second_store, &second_layouts, &second_proofs,
			second_function) or { panic(err) }
		assert first == second
		assert first.decision.parameters[0].locations.len == 1
		assert first_store.types == first_types_before
		assert first_store.cache == first_cache_before
		assert second_store.types == second_types_before
		assert second_store.cache == second_cache_before
		first_decision_snapshot := abi_frame_clone_function_decision(&first.decision)
		second_decision_snapshot := abi_frame_clone_function_decision(&second.decision)
		first_frame_snapshot := first.frame
		second_frame_snapshot := second.frame
		_ = first_store.get_int(32)
		_ = second_store.get_float(32)
		assert first_store.types != first_types_before
		assert second_store.types != second_types_before
		assert first.decision == first_decision_snapshot
		assert first.frame == first_frame_snapshot
		assert second.decision == second_decision_snapshot
		assert second.frame == second_frame_snapshot
	}
}

fn test_abi_frame_f17_abi0_errors_propagate() {
	mut valid_store := ssa.TypeStore.new()
	valid_function := abi_frame_test_add_function(mut valid_store, [], 0)
	empty_layouts := AbiLayoutSnapshot{}
	empty_proofs := AbiMicrosoftUdtEvidence{}
	invalid_profile := unsafe { TargetProfile(255) }
	abi_frame_test_expect_public_error(invalid_profile, .prototyped, &valid_store,
		&empty_layouts, &empty_proofs, valid_function, 'invalid_target_profile')
	abi_frame_test_expect_public_error(.linux_x86_64_sysv_elf, .variadic, &valid_store,
		&empty_layouts, &empty_proofs, valid_function, 'unsupported_call_kind')
	abi_frame_test_expect_public_error(.linux_x86_64_sysv_elf, .unprototyped,
		&valid_store, &empty_layouts, &empty_proofs, valid_function, 'unsupported_call_kind')
	mut c_store := ssa.TypeStore.new()
	c_i8 := c_store.get_int(8)
	c_type := abi_frame_test_add_struct(mut c_store, [c_i8], true)
	c_function := abi_frame_test_add_function(mut c_store, [c_type], 0)
	abi_frame_test_expect_public_error(.linux_x86_64_sysv_elf, .prototyped, &c_store,
		&empty_layouts, &empty_proofs, c_function, 'missing_external_c_layout')
	mut udt_store := ssa.TypeStore.new()
	udt_i8 := udt_store.get_int(8)
	udt_type := abi_frame_test_add_struct(mut udt_store, [udt_i8, udt_i8, udt_i8],
		false)
	udt_function := abi_frame_test_add_function(mut udt_store, [udt_type], 0)
	unknown_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     udt_type
			eligibility: .unknown
		}]
	}
	abi_frame_test_expect_public_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&udt_store, &empty_layouts, &unknown_proofs, udt_function,
		'unknown_microsoft_udt_eligibility')
	ineligible_proofs := AbiMicrosoftUdtEvidence{
		proofs: [AbiMicrosoftUdtProof{
			type_id:     udt_type
			eligibility: .ineligible
		}]
	}
	abi_frame_test_expect_public_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		&udt_store, &empty_layouts, &ineligible_proofs, udt_function,
		'unsupported_microsoft_udt')
	invalid_store := ssa.TypeStore{}
	abi_frame_test_expect_public_error(.linux_x86_64_sysv_elf, .prototyped,
		&invalid_store, &empty_layouts, &empty_proofs, 0, 'invalid_type_graph')
}

fn test_abi_frame_f18_private_invariants_and_checked_arithmetic() {
	function_type := ssa.TypeID(1)
	base := abi_frame_test_synthetic_decision(.linux_x86_64_sysv_elf, function_type,
		0, 0, [])
	bad_call_kind := AbiFunctionDecision{
		...base
		call_kind: .variadic
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &bad_call_kind, 'invalid_call_frame_geometry')
	bad_profile := AbiFunctionDecision{
		...base
		profile: .macos_x86_64_sysv_macho
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &bad_profile, 'invalid_call_frame_geometry')
	bad_abi := AbiFunctionDecision{
		...base
		abi: .microsoft_x64
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &bad_abi, 'invalid_call_frame_geometry')
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped, 2, &base,
		'invalid_call_frame_geometry')
	bad_alignment := AbiFunctionDecision{
		...base
		pre_call_stack_alignment_bytes: 8
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &bad_alignment, 'invalid_call_frame_geometry')
	bad_sysv_metadata := AbiFunctionDecision{
		...base
		red_zone_bytes: 0
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &bad_sysv_metadata, 'invalid_call_frame_geometry')
	bad_windows := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, 31, 0, [])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &bad_windows, 'invalid_call_frame_geometry')
	bad_windows_metadata := AbiFunctionDecision{
		...abi_frame_test_synthetic_decision(.windows_x86_64_microsoft_abi_coff,
			function_type, 32, 0, [])
		shadow_space_bytes: 0
		red_zone_bytes:     128
	}
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &bad_windows_metadata, 'invalid_call_frame_geometry')
	output_home := AbiFunctionDecision{
		...base
		return_value: AbiValueDecision{
			type_id:         2
			mode:            .direct
			size_bytes:      8
			alignment_bytes: 8
			classes:         [.integer]
			locations:       [abi_frame_test_gpr(.rax, true, 0)]
		}
	}
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &output_home, 'invalid_call_frame_geometry')
	valid_sret := AbiFunctionDecision{
		...abi_frame_test_synthetic_decision(.windows_x86_64_microsoft_abi_coff,
			function_type, 32, 0, [])
		return_value: AbiValueDecision{
			type_id:         2
			mode:            .indirect
			size_bytes:      16
			alignment_bytes: 8
			classes:         [.memory]
		}
		hidden_sret: AbiHiddenSretDecision{
			present: true
			input:   abi_frame_test_gpr(.rcx, true, 0)
			echo:    abi_frame_test_gpr(.rax, false, 0)
		}
	}
	_ := abi_plan_call_frame(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &valid_sret) or { panic(err) }
	echo_home := AbiFunctionDecision{
		...valid_sret
		hidden_sret: AbiHiddenSretDecision{
			present: true
			input:   abi_frame_test_gpr(.rcx, true, 0)
			echo:    abi_frame_test_gpr(.rax, true, 0)
		}
	}
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &echo_home, 'invalid_call_frame_geometry')
	stale_inactive := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, 32, 0, [AbiValueDecision{
			type_id:                            2
			mode:                               .direct
			size_bytes:                         8
			alignment_bytes:                    8
			classes:                            [.integer]
			locations:                          [abi_frame_test_gpr(.rcx, true, 0)]
			indirect_temporary_offset_bytes:    1
			indirect_temporary_size_bytes:      0
			indirect_temporary_alignment_bytes: 0
		}])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &stale_inactive, 'invalid_call_frame_geometry')
	sysv_active := abi_frame_test_synthetic_decision(.linux_x86_64_sysv_elf,
		function_type, 0, 16, [AbiValueDecision{
			...abi_frame_test_synthetic_indirect_parameter(0, 3, 3, 16)
			locations: [abi_frame_test_gpr(.rdi, false, 0)]
		}])
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &sysv_active, 'invalid_call_frame_geometry')
	wrong_size := abi_frame_test_synthetic_decision(.windows_x86_64_microsoft_abi_coff,
		function_type, 32, 16, [abi_frame_test_synthetic_indirect_parameter(0, 3, 4,
		16)])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &wrong_size, 'invalid_call_frame_geometry')
	wrong_alignment := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, 32, 16,
		[abi_frame_test_synthetic_indirect_parameter(0, 3, 3, 8)])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &wrong_alignment, 'invalid_call_frame_geometry')
	second_location := AbiLocation{
		...abi_frame_test_gpr(.rdx, true, 8)
	}
	second_parameter := AbiValueDecision{
		...abi_frame_test_synthetic_indirect_parameter(0, 3, 3, 16)
		locations: [second_location]
	}
	overlapping := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, 32, 32,
		[abi_frame_test_synthetic_indirect_parameter(0, 3, 3, 16), second_parameter])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &overlapping, 'invalid_call_frame_geometry')
	out_of_range := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, 32, 16,
		[abi_frame_test_synthetic_indirect_parameter(0, 17, 17, 16)])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &out_of_range, 'invalid_call_frame_geometry')
	overflow_outgoing := abi_frame_test_synthetic_decision(.linux_x86_64_sysv_elf,
		function_type, max_int, 0, [])
	abi_frame_test_expect_private_error(.linux_x86_64_sysv_elf, .prototyped,
		function_type, &overflow_outgoing, 'arithmetic_overflow')
	overflow_temporary_align := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, max_int, 16,
		[abi_frame_test_synthetic_indirect_parameter(0, 1, 1, 16)])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &overflow_temporary_align, 'arithmetic_overflow')
	overflow_temporary_add := abi_frame_test_synthetic_decision(
		.windows_x86_64_microsoft_abi_coff, function_type, max_int - 15, 16,
		[abi_frame_test_synthetic_indirect_parameter(0, 1, 1, 16)])
	abi_frame_test_expect_private_error(.windows_x86_64_microsoft_abi_coff, .prototyped,
		function_type, &overflow_temporary_add, 'arithmetic_overflow')
}
