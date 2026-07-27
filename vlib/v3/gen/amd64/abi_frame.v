module amd64

import v3.ssa

pub struct AbiCallFramePlan {
pub:
	profile                              TargetProfile
	abi                                  AbiKind
	function_type                        ssa.TypeID
	outgoing_area_offset_bytes           int
	outgoing_area_size_bytes             int
	indirect_temporary_area_offset_bytes int
	indirect_temporary_area_size_bytes   int
	inter_area_padding_bytes             int
	tail_alignment_padding_bytes         int
	total_allocation_bytes               int
	required_pre_call_alignment_bytes    int
}

pub struct AbiPlannedCall {
pub:
	decision AbiFunctionDecision
	frame    AbiCallFramePlan
}

pub fn classify_and_plan_abi_call(profile TargetProfile, call_kind AbiCallKind, type_store &ssa.TypeStore, external_layouts &AbiLayoutSnapshot, udt_evidence &AbiMicrosoftUdtEvidence, function_type ssa.TypeID) !AbiPlannedCall {
	decision := classify_abi_function(profile, call_kind, type_store, external_layouts,
		udt_evidence, function_type)!
	frame := abi_plan_call_frame(profile, call_kind, function_type, &decision)!
	return AbiPlannedCall{
		decision: abi_frame_clone_function_decision(&decision)
		frame:    frame
	}
}

fn abi_frame_error() IError {
	return abi_error('invalid_call_frame_geometry')
}

fn abi_frame_checked_sub(left int, right int) !int {
	if left < 0 || right < 0 || left < right {
		return abi_error('arithmetic_overflow')
	}
	return left - right
}

fn abi_frame_is_gpr(register AbiRegister) bool {
	return register in [.rax, .rdx, .rcx, .rdi, .rsi, .r8, .r9]
}

fn abi_frame_is_xmm(register AbiRegister) bool {
	return register in [.xmm0, .xmm1, .xmm2, .xmm3, .xmm4, .xmm5, .xmm6, .xmm7]
}

fn abi_frame_none_location_is_canonical(location &AbiLocation) bool {
	return location.kind == .none && location.register == .none && location.class == .no_class
		&& location.value_offset_bytes == 0 && location.width_bytes == 0
		&& !location.has_stack_address && location.caller_stack_offset_bytes == 0
		&& location.callee_stack_offset_bytes == 0 && !location.has_home_address
		&& location.caller_home_offset_bytes == 0 && location.callee_home_offset_bytes == 0
}

fn abi_frame_validate_location(location &AbiLocation, abi AbiKind, outgoing_size int, allow_none bool, allow_stack bool, allow_home bool) ! {
	if location.kind == .none {
		if !allow_none || !abi_frame_none_location_is_canonical(location) {
			return abi_frame_error()
		}
		return
	}
	if location.value_offset_bytes < 0 || location.width_bytes <= 0 {
		return abi_frame_error()
	}
	match location.kind {
		.gpr {
			if !abi_frame_is_gpr(location.register) || location.class != .integer
				|| location.has_stack_address {
				return abi_frame_error()
			}
		}
		.xmm {
			if !abi_frame_is_xmm(location.register) || location.class != .sse
				|| location.has_stack_address {
				return abi_frame_error()
			}
		}
		.stack {
			if !allow_stack || location.register != .none
				|| location.class !in [.integer, .sse, .memory] || !location.has_stack_address
				|| location.has_home_address {
				return abi_frame_error()
			}
		}
		else {
			return abi_frame_error()
		}
	}
	if location.has_stack_address {
		first_stack_offset := if abi == .microsoft_x64 { 32 } else { 0 }
		if location.caller_stack_offset_bytes < first_stack_offset {
			return abi_frame_error()
		}
		expected_callee := abi_checked_add(location.caller_stack_offset_bytes, 8)!
		stack_end := abi_checked_add(location.caller_stack_offset_bytes,
			location.width_bytes)!
		if location.callee_stack_offset_bytes != expected_callee || stack_end > outgoing_size {
			return abi_frame_error()
		}
	} else if location.caller_stack_offset_bytes != 0
		|| location.callee_stack_offset_bytes != 0 {
		return abi_frame_error()
	}
	if location.has_home_address {
		if !allow_home || abi != .microsoft_x64 || location.kind !in [.gpr, .xmm]
			|| location.caller_home_offset_bytes !in [0, 8, 16, 24] {
			return abi_frame_error()
		}
		expected_callee := abi_checked_add(location.caller_home_offset_bytes, 8)!
		home_end := abi_checked_add(location.caller_home_offset_bytes, 8)!
		if location.callee_home_offset_bytes != expected_callee || home_end > 32 {
			return abi_frame_error()
		}
	} else if location.caller_home_offset_bytes != 0 || location.callee_home_offset_bytes != 0 {
		return abi_frame_error()
	}
}

fn abi_frame_validate_inactive_temporary(value &AbiValueDecision) ! {
	if value.has_indirect_temporary || value.indirect_temporary_offset_bytes != 0
		|| value.indirect_temporary_size_bytes != 0
		|| value.indirect_temporary_alignment_bytes != 0 {
		return abi_frame_error()
	}
}

fn abi_frame_validate_return(decision &AbiFunctionDecision) ! {
	value := &decision.return_value
	abi_frame_validate_inactive_temporary(value)!
	match value.mode {
		.no_value, .indirect {
			if value.locations.len != 0 {
				return abi_frame_error()
			}
		}
		.direct, .mixed {
			if value.locations.len == 0 {
				return abi_frame_error()
			}
			for location in value.locations {
				abi_frame_validate_location(&location, decision.abi,
					decision.minimum_outgoing_area_bytes, false, false, false)!
			}
		}
		else {
			return abi_frame_error()
		}
	}
}

fn abi_frame_validate_hidden_sret(decision &AbiFunctionDecision) ! {
	hidden := &decision.hidden_sret
	if hidden.present != (decision.return_value.mode == .indirect) {
		return abi_frame_error()
	}
	if !hidden.present {
		if !abi_frame_none_location_is_canonical(&hidden.input)
			|| !abi_frame_none_location_is_canonical(&hidden.echo) {
			return abi_frame_error()
		}
		return
	}
	abi_frame_validate_location(&hidden.input, decision.abi,
		decision.minimum_outgoing_area_bytes, false, false, decision.abi == .microsoft_x64)!
	abi_frame_validate_location(&hidden.echo, decision.abi,
		decision.minimum_outgoing_area_bytes, false, false, false)!
	if hidden.input.kind != .gpr || hidden.input.class != .integer
		|| hidden.input.value_offset_bytes != 0 || hidden.input.width_bytes != 8
		|| hidden.echo.kind != .gpr || hidden.echo.register != .rax
		|| hidden.echo.class != .integer || hidden.echo.value_offset_bytes != 0
		|| hidden.echo.width_bytes != 8 {
		return abi_frame_error()
	}
	if decision.abi == .sysv_amd64 {
		if hidden.input.register != .rdi || hidden.input.has_home_address {
			return abi_frame_error()
		}
	} else if hidden.input.register != .rcx || !hidden.input.has_home_address
		|| hidden.input.caller_home_offset_bytes != 0
		|| hidden.input.callee_home_offset_bytes != 8 {
		return abi_frame_error()
	}
}

fn abi_frame_validate_microsoft_parameter_position(parameter &AbiValueDecision, parameter_index int, has_sret bool) ! {
	if parameter.locations.len != 1 {
		return abi_frame_error()
	}
	position := abi_checked_add(parameter_index, if has_sret { 1 } else { 0 })!
	position_offset := abi_checked_mul(position, 8)!
	location := &parameter.locations[0]
	if position < 4 {
		if location.kind !in [.gpr, .xmm] || location.has_stack_address
			|| !location.has_home_address
			|| location.caller_home_offset_bytes != position_offset {
			return abi_frame_error()
		}
	} else if location.kind != .stack || !location.has_stack_address
		|| location.caller_stack_offset_bytes != position_offset || location.has_home_address {
		return abi_frame_error()
	}
}

fn abi_frame_validate_parameters(decision &AbiFunctionDecision) ! {
	mut temporary_cursor := 0
	mut active_temporary_count := 0
	for parameter_index, parameter in decision.parameters {
		if parameter.size_bytes <= 0 || parameter.alignment_bytes <= 0
			|| parameter.locations.len == 0 {
			return abi_frame_error()
		}
		if decision.abi == .microsoft_x64 {
			if parameter.mode !in [.direct, .indirect] {
				return abi_frame_error()
			}
			abi_frame_validate_microsoft_parameter_position(&parameter, parameter_index,
				decision.hidden_sret.present)!
		} else if parameter.mode !in [.direct, .mixed, .memory_by_value] {
			return abi_frame_error()
		}
		for location in parameter.locations {
			abi_frame_validate_location(&location, decision.abi,
				decision.minimum_outgoing_area_bytes, false, true,
				decision.abi == .microsoft_x64)!
		}
		if parameter.mode == .indirect {
			if decision.abi != .microsoft_x64 || !parameter.has_indirect_temporary
				|| parameter.indirect_temporary_size_bytes != parameter.size_bytes
				|| parameter.indirect_temporary_alignment_bytes != 16 {
				return abi_frame_error()
			}
			location := parameter.locations[0]
			if location.class != .integer || location.value_offset_bytes != 0
				|| location.width_bytes != 8 {
				return abi_frame_error()
			}
			expected_offset := abi_checked_align_up(temporary_cursor, 16)!
			if parameter.indirect_temporary_offset_bytes != expected_offset {
				return abi_frame_error()
			}
			temporary_end := abi_checked_add(expected_offset,
				parameter.indirect_temporary_size_bytes)!
			if temporary_end > decision.minimum_indirect_temporary_area_bytes {
				return abi_frame_error()
			}
			temporary_cursor = temporary_end
			active_temporary_count++
		} else {
			abi_frame_validate_inactive_temporary(&parameter)!
		}
	}
	expected_temporary_area := if active_temporary_count == 0 {
		0
	} else {
		abi_checked_align_up(temporary_cursor, 16)!
	}
	if decision.minimum_indirect_temporary_area_bytes != expected_temporary_area {
		return abi_frame_error()
	}
}

fn abi_plan_call_frame(profile TargetProfile, call_kind AbiCallKind, function_type ssa.TypeID, decision &AbiFunctionDecision) !AbiCallFramePlan {
	profile_value := int(profile)
	is_sysv := profile_value == int(TargetProfile.linux_x86_64_sysv_elf)
		|| profile_value == int(TargetProfile.macos_x86_64_sysv_macho)
	is_microsoft := profile_value == int(TargetProfile.windows_x86_64_microsoft_abi_coff)
	if !is_sysv && !is_microsoft {
		return abi_frame_error()
	}
	expected_abi := if is_microsoft { AbiKind.microsoft_x64 } else { AbiKind.sysv_amd64 }
	if call_kind != .prototyped || decision.profile != profile || decision.abi != expected_abi
		|| decision.call_kind != call_kind || decision.function_type != function_type
		|| int(function_type) < 0 || decision.pre_call_stack_alignment_bytes != 16
		|| decision.minimum_outgoing_area_bytes < 0
		|| decision.minimum_indirect_temporary_area_bytes < 0 {
		return abi_frame_error()
	}
	if is_sysv {
		if decision.shadow_space_bytes != 0 || decision.red_zone_bytes != 128 {
			return abi_frame_error()
		}
	} else if decision.shadow_space_bytes != 32 || decision.red_zone_bytes != 0
		|| decision.minimum_outgoing_area_bytes < 32 {
		return abi_frame_error()
	}
	abi_frame_validate_return(decision)!
	abi_frame_validate_hidden_sret(decision)!
	abi_frame_validate_parameters(decision)!
	outgoing_size := decision.minimum_outgoing_area_bytes
	temporary_size := decision.minimum_indirect_temporary_area_bytes
	mut temporary_offset := 0
	mut inter_padding := 0
	mut tail_padding := 0
	mut total := 0
	if temporary_size == 0 {
		total = abi_checked_align_up(outgoing_size, 16)!
		tail_padding = abi_frame_checked_sub(total, outgoing_size)!
	} else {
		temporary_offset = abi_checked_align_up(outgoing_size, 16)!
		inter_padding = abi_frame_checked_sub(temporary_offset, outgoing_size)!
		temporary_end := abi_checked_add(temporary_offset, temporary_size)!
		total = abi_checked_align_up(temporary_end, 16)!
		tail_padding = abi_frame_checked_sub(total, temporary_end)!
	}
	if total % 16 != 0 || (temporary_size == 0
		&& (temporary_offset != 0 || inter_padding != 0)) {
		return abi_frame_error()
	}
	return AbiCallFramePlan{
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

fn abi_frame_clone_value_decision(value &AbiValueDecision) AbiValueDecision {
	return AbiValueDecision{
		type_id:                            value.type_id
		mode:                               value.mode
		size_bytes:                         value.size_bytes
		alignment_bytes:                    value.alignment_bytes
		semantic_width_bits:                value.semantic_width_bits
		abi_transfer_width_bits:            value.abi_transfer_width_bits
		semantic_is_unsigned:               value.semantic_is_unsigned
		integral_extension:                 value.integral_extension
		classes:                            value.classes.clone()
		locations:                          value.locations.clone()
		has_indirect_temporary:             value.has_indirect_temporary
		indirect_temporary_offset_bytes:    value.indirect_temporary_offset_bytes
		indirect_temporary_size_bytes:      value.indirect_temporary_size_bytes
		indirect_temporary_alignment_bytes: value.indirect_temporary_alignment_bytes
	}
}

fn abi_frame_clone_function_decision(decision &AbiFunctionDecision) AbiFunctionDecision {
	mut parameters := []AbiValueDecision{cap: decision.parameters.len}
	for parameter in decision.parameters {
		parameters << abi_frame_clone_value_decision(&parameter)
	}
	return AbiFunctionDecision{
		profile:                               decision.profile
		abi:                                   decision.abi
		call_kind:                             decision.call_kind
		function_type:                         decision.function_type
		return_value:                          abi_frame_clone_value_decision(&decision.return_value)
		parameters:                            parameters
		hidden_sret:                           decision.hidden_sret
		shadow_space_bytes:                    decision.shadow_space_bytes
		red_zone_bytes:                        decision.red_zone_bytes
		minimum_outgoing_area_bytes:           decision.minimum_outgoing_area_bytes
		minimum_indirect_temporary_area_bytes: decision.minimum_indirect_temporary_area_bytes
		pre_call_stack_alignment_bytes:        decision.pre_call_stack_alignment_bytes
	}
}
