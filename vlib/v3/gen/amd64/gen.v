module amd64

import v3.ssa

// TargetProfile identifies the complete ABI and object-format contract.
pub enum TargetProfile {
	linux_x86_64_sysv_elf
	macos_x86_64_sysv_macho
	windows_x86_64_microsoft_abi_coff
}

// Gen stores an immutable, backend-local lowering snapshot.
@[noinit]
pub struct Gen {
	plan          LoweringPlan
	memory_frames []GenMemoryFrame
}

struct GenMemoryFrame {
	present                bool
	source_function_index  int = -1
	function_id            u32
	call_extent_bytes      u64
	stack_adjustment_bytes u64
	prologue_bytes         []u8
	epilogue_bytes         []u8
	windows_unwind_bytes   []u8
}

struct GenFunctionLayout {
	offset int
	size   int
	end    int
	blocks []GenBlockLayout
}

struct GenBlockLayout {
	offset int
	size   int
	end    int
}

struct GenPreflight {
	functions       []GenFunctionLayout
	total_text_size int
}

// new validates and snapshots the supported final SSA subset.
pub fn Gen.new(profile TargetProfile, m &ssa.Module) !Gen {
	plan := validate_and_snapshot(profile, m)!
	_ = gen_preflight(&plan)!
	return Gen{
		plan:          plan
		memory_frames: []GenMemoryFrame{}
	}
}

// new_with_scalar_constants validates and snapshots typed backend-owned scalar constants.
pub fn Gen.new_with_scalar_constants(profile TargetProfile, m &ssa.Module, bindings []ScalarConstantBinding) !Gen {
	plan := validate_and_snapshot_with_scalar_constants(profile, m, bindings)!
	_ = gen_preflight(&plan)!
	return Gen{
		plan:          plan
		memory_frames: []GenMemoryFrame{}
	}
}

// new_with_scalar_abi validates canonical direct signatures and snapshots the
// bounded scalar-integer ABI subset.
pub fn Gen.new_with_scalar_abi(profile TargetProfile, m &ssa.Module, bindings []ScalarConstantBinding, signatures []AbiDirectSignatureBinding) !Gen {
	plan := validate_and_snapshot_with_scalar_abi(profile, m, signatures, bindings)!
	_ = gen_preflight(&plan)!
	return Gen{
		plan:          plan
		memory_frames: []GenMemoryFrame{}
	}
}

fn gen_memory_frame_activation_error(function_index int, detail string) IError {
	context := if function_index < 0 {
		'memory frame activation'
	} else {
		'memory frame activation function ${function_index}'
	}
	return lowering_error(context, detail)
}

fn gen_memory_frame_validate_empty_memory(memory &MemoryAggPlan) ! {
	if memory.scalar_layouts.len != 0 || memory.slot_requests.len != 0 || memory.pointers.len != 0
		|| memory.accesses.len != 0 || memory.aggregate_layouts.len != 0
		|| memory.aggregate_slots.len != 0 || memory.aggregate_snapshots.len != 0
		|| memory.aggregate_actions.len != 0 || memory.total_requested_bytes != 0 {
		return gen_memory_frame_activation_error(memory.function_index,
			'M7 requires an empty M1 memory plan')
	}
}

fn gen_memory_frame_validate_composition(source_function_index int, composition &MemoryFrameCompositionPlan) !GenMemoryFrame {
	memory := composition.memory
	if memory.function_index != source_function_index
		|| memory.function_id != u32(source_function_index) {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 composition function identity mismatch')
	}
	if memory.profile != .windows_x86_64_microsoft_abi_coff {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 composition is not Microsoft x64 COFF')
	}
	gen_memory_frame_validate_empty_memory(&memory)!
	if composition.slot_bindings.len != 0 {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 does not accept composed frame slots')
	}

	encoded := composition.frame
	cfi := encoded.cfi
	frame := cfi.frame
	layout := frame.layout_frame
	expected_prologue := [u8(0x48), 0x83, 0xec, 0x28]
	expected_epilogue := [u8(0x48), 0x83, 0xc4, 0x28]
	expected_unwind := [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
	expected_translations := MemoryStackTranslations{
		entry_to_body_subtract_bytes: 40
		incoming_from_body_add_bytes: 40
	}
	if layout.function_id != memory.function_id || layout.profile != memory.profile
		|| layout.extent_kind != .fixed || !layout.has_call || layout.call_extent_bytes != 32
		|| layout.uses_red_zone || layout.red_zone_extent_bytes != 0
		|| layout.non_red_zone_extent_bytes != 32 || layout.stack_adjustment_bytes != 40
		|| layout.probe_required || layout.translations != expected_translations
		|| layout.slots.len != 0 {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 composition is outside the C=32 D=40 frame contract')
	}
	if !frame.save_facts.present || frame.save_facts.function_id != memory.function_id
		|| layout.red_zone_policy != .abi_default || frame.save_facts.registers.len != 0
		|| frame.saves.len != 0 || frame.save_push_count != 0
		|| frame.base_allocation_bytes != 40 || frame.padding_bytes != 0
		|| frame.allocation_bytes != 40 || frame.total_stack_extent_bytes != 40
		|| frame.allocation_prologue_kind != .sub_imm8 || frame.prologue_bytes != expected_prologue
		|| frame.epilogue_bytes != expected_epilogue || frame.body_offset_bytes != 4
		|| frame.entry_cfa_offset_bytes != 8 || frame.body_cfa_offset_bytes != 48
		|| frame.probe_required || frame.translations != expected_translations
		|| frame.probe_fixup != MemoryFrameRel32Fixup{} || frame.chkstk != MemoryChkstkContract{}
		|| frame.slots.len != 0 {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 composition has noncanonical M7 frame encoding')
	}
	unwind := frame.windows_unwind
	if !unwind.present || unwind.allocation_kind != .alloc_small || !unwind.allocation_code_present
		|| unwind.allocation_bytes != 40 || unwind.size_of_prolog_bytes != 4
		|| unwind.allocation_code_offset_bytes != 4 || unwind.count_of_codes != 1
		|| unwind.xdata_bytes != expected_unwind {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 composition has noncanonical M7 Windows unwind')
	}
	if cfi.disposition != .windows_none || cfi.baseline != MemoryFrameCfiBaseline{}
		|| cfi.prologue_ops.len != 0 || cfi.epilogue_template_ops.len != 0
		|| encoded.cie_initial_instruction_bytes.len != 0
		|| encoded.prologue_fde_instruction_fragment != memory_frame_cfi_encode_absent_fragment(.prologue)
		|| encoded.epilogue_fde_instruction_template != memory_frame_cfi_encode_absent_fragment(.epilogue_template)
		|| encoded.total_instruction_fragment_bytes != 0 {
		return gen_memory_frame_activation_error(source_function_index,
			'M6 Windows composition must not publish DWARF fragments')
	}
	return GenMemoryFrame{
		present:                true
		source_function_index:  source_function_index
		function_id:            memory.function_id
		call_extent_bytes:      layout.call_extent_bytes
		stack_adjustment_bytes: layout.stack_adjustment_bytes
		prologue_bytes:         frame.prologue_bytes.clone()
		epilogue_bytes:         frame.epilogue_bytes.clone()
		windows_unwind_bytes:   unwind.xdata_bytes.clone()
	}
}

fn gen_memory_frame_validate_source_shape(m &ssa.Module, source_function_index int, lowered_function &LoweredFunction) ! {
	function := m.funcs[source_function_index]
	if function.id != source_function_index || function.typ != ssa.TypeID(0)
		|| function.params.len != 0 || function.blocks.len != 1 || lowered_function.blocks.len != 0
		|| !lowered_function.uses_scalar_abi || lowered_function.abi_parameters.len != 0
		|| lowered_function.abi_result.present || lowered_function.return_value.kind != .void_t
		|| lowered_function.calls.len != 1 {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 requires one scalar-ABI void CALL wrapper block')
	}
	block_index := int(function.blocks[0])
	block := m.blocks[block_index]
	if block.instrs.len != 2 {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 requires exactly CALL then RET')
	}
	call_value := m.values[int(block.instrs[0])]
	ret_value := m.values[int(block.instrs[1])]
	call_instruction := m.instrs[call_value.index]
	ret_instruction := m.instrs[ret_value.index]
	if call_instruction.op != .call || call_instruction.operands.len != 1
		|| call_instruction.typ != ssa.TypeID(0) || ret_instruction.op != .ret
		|| ret_instruction.operands.len != 0 || ret_instruction.typ != ssa.TypeID(0) {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 requires one direct no-argument void CALL followed by void RET')
	}
	call_operand := m.values[int(call_instruction.operands[0])]
	if call_operand.kind != .func_ref {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 CALL operand is not a direct function reference')
	}
	target := lowered_function.calls[0]
	if !target.uses_scalar_abi || target.argument_mode != .none || target.argument_bits != 0
		|| target.abi_arguments.len != 0 || target.abi_result.present
		|| target.abi_stack_decrement_bytes != 40 {
		return gen_memory_frame_activation_error(source_function_index,
			'M7 requires canonical no-argument Microsoft scalar CALL facts')
	}
}

fn gen_memory_frame_snapshot(m &ssa.Module, plan &LoweringPlan, compositions []MemoryFrameCompositionPlan) ![]GenMemoryFrame {
	mut source_indices := []int{cap: plan.functions.len}
	for source_function_index, function in m.funcs {
		if !function.is_c_extern {
			source_indices << source_function_index
		}
	}
	if source_indices.len != plan.functions.len {
		return gen_memory_frame_activation_error(-1, 'lowered/source function count mismatch')
	}
	mut frames := []GenMemoryFrame{len: plan.functions.len}
	mut composition_index := 0
	mut caller_count := 0
	for function_index, lowered_function in plan.functions {
		if lowered_function.calls.len == 0 {
			continue
		}
		source_function_index := source_indices[function_index]
		gen_memory_frame_validate_source_shape(m, source_function_index, &lowered_function)!
		if composition_index >= compositions.len {
			return gen_memory_frame_activation_error(source_function_index,
				'M6 composition is missing')
		}
		frames[function_index] = gen_memory_frame_validate_composition(source_function_index,
			&compositions[composition_index])!
		composition_index++
		caller_count++
	}
	if caller_count == 0 {
		return gen_memory_frame_activation_error(-1, 'M7 requires at least one direct CALL wrapper')
	}
	if composition_index != compositions.len {
		return gen_memory_frame_activation_error(-1,
			'M6 composition does not belong to an emitted caller')
	}
	return frames
}

// new_with_scalar_abi_memory_frames activates exact M6 Microsoft CALL32 frames.
pub fn Gen.new_with_scalar_abi_memory_frames(profile TargetProfile, m &ssa.Module, bindings []ScalarConstantBinding, signatures []AbiDirectSignatureBinding, compositions []MemoryFrameCompositionPlan) !Gen {
	if profile != .windows_x86_64_microsoft_abi_coff {
		return gen_memory_frame_activation_error(-1, 'M7 supports only Microsoft x64 COFF')
	}
	plan := validate_and_snapshot_with_scalar_abi(profile, m, signatures, bindings)!
	_ = gen_preflight(&plan)!
	memory_frames := gen_memory_frame_snapshot(m, &plan, compositions)!
	return Gen{
		plan:          plan
		memory_frames: memory_frames
	}
}

fn gen_abi_transfer_semantics_equal(left AbiScalarTransfer, right AbiScalarTransfer) bool {
	return left.present == right.present && left.type_id == right.type_id
		&& left.semantic_width_bits == right.semantic_width_bits
		&& left.abi_transfer_width_bits == right.abi_transfer_width_bits
		&& left.semantic_is_unsigned == right.semantic_is_unsigned
		&& left.integral_extension == right.integral_extension
}

fn gen_validate_abi_transfer(context string, transfer AbiScalarTransfer, result bool) !int {
	if !transfer.present || transfer.semantic_width_bits !in [1, 8, 16, 32, 64]
		|| transfer.abi_transfer_width_bits !in [8, 16, 32, 64]
		|| transfer.location.width_bytes * 8 != transfer.abi_transfer_width_bits
		|| transfer.location.class != .integer || transfer.location.value_offset_bytes != 0 {
		return lowering_error(context, 'invalid scalar ABI transfer')
	}
	if result {
		if transfer.location.kind != .gpr || transfer.location.register != .rax
			|| transfer.location.has_stack_address || transfer.location.has_home_address {
			return lowering_error(context, 'scalar ABI result is not in RAX')
		}
	} else {
		match transfer.location.kind {
			.gpr {
				if transfer.location.register !in [.rcx, .rdx, .rdi, .rsi, .r8, .r9]
					|| transfer.location.has_stack_address {
					return lowering_error(context, 'invalid scalar ABI parameter GPR')
				}
			}
			.stack {
				if transfer.location.register != .none || !transfer.location.has_stack_address
					|| transfer.location.has_home_address {
					return lowering_error(context, 'invalid scalar ABI parameter stack slot')
				}
			}
			else {
				return lowering_error(context, 'scalar ABI parameter has no integer location')
			}
		}
	}
	return transfer.location.width_bytes
}

fn gen_validate_scalar_abi_call(profile TargetProfile, context string, target LoweredCallTarget) ! {
	if !target.uses_scalar_abi || target.argument_mode != .none || target.argument_bits != 0 {
		return lowering_error(context, 'invalid scalar ABI call marker')
	}
	match profile {
		.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
			if target.abi_arguments.len > 7 || target.abi_stack_decrement_bytes != 8 {
				return abi_consume_requires_memory_agg()
			}
		}
		.windows_x86_64_microsoft_abi_coff {
			if target.abi_arguments.len > 5 || target.abi_stack_decrement_bytes != 40 {
				return abi_consume_requires_memory_agg()
			}
		}
	}

	for argument_index, argument in target.abi_arguments {
		width := gen_validate_abi_transfer('${context} argument ${argument_index}',
			argument.transfer, false)!
		if argument.bits & ~abi_consume_transfer_mask(argument.transfer.abi_transfer_width_bits) != 0 {
			return lowering_error(context, 'scalar ABI argument bits exceed transfer width')
		}
		if argument.transfer.location.kind == .stack {
			offset := argument.transfer.location.caller_stack_offset_bytes
			if offset < 0 || offset + width > target.abi_stack_decrement_bytes {
				return lowering_error(context, 'scalar ABI stack argument exceeds live decrement')
			}
		}
	}
	if target.abi_result.present {
		_ = gen_validate_abi_transfer('${context} result', target.abi_result, true)!
	}
}

fn gen_validate_scalar_abi_function(profile TargetProfile, context string, lowered_function LoweredFunction, functions []LoweredFunction) ! {
	if !lowered_function.uses_scalar_abi || lowered_function.blocks.len != 0 {
		return lowering_error(context, 'invalid scalar ABI function marker')
	}
	for parameter_index, parameter in lowered_function.abi_parameters {
		_ = gen_validate_abi_transfer('${context} parameter ${parameter_index}', parameter, false)!
	}
	if lowered_function.abi_result.present {
		_ = gen_validate_abi_transfer('${context} result', lowered_function.abi_result, true)!
	}
	return_value := lowered_function.return_value
	if return_value.abi_result != lowered_function.abi_result {
		return lowering_error(context, 'lowered return result does not match function signature')
	}
	match return_value.kind {
		.void_t {
			if return_value.bits != 0 || return_value.parameter_index != -1
				|| return_value.abi_parameter.present || return_value.abi_result.present
				|| lowered_function.calls.len > 1 {
				return lowering_error(context, 'invalid void scalar ABI return state')
			}
		}
		.scalar_constant {
			if lowered_function.calls.len != 0 || return_value.parameter_index != -1
				|| return_value.abi_parameter.present || !return_value.abi_result.present
				|| return_value.bits & ~abi_consume_transfer_mask(return_value.abi_result.abi_transfer_width_bits) != 0 {
				return lowering_error(context, 'invalid scalar ABI constant return state')
			}
		}
		.scalar_parameter {
			if lowered_function.calls.len != 0 || return_value.parameter_index < 0
				|| return_value.parameter_index >= lowered_function.abi_parameters.len
				|| return_value.abi_parameter != lowered_function.abi_parameters[return_value.parameter_index]
				|| !gen_abi_transfer_semantics_equal(return_value.abi_parameter, return_value.abi_result) {
				return lowering_error(context, 'invalid scalar ABI parameter return state')
			}
		}
		.scalar_call_result {
			if lowered_function.calls.len != 1 || return_value.parameter_index != -1
				|| return_value.abi_parameter.present || !return_value.abi_result.present {
				return lowering_error(context, 'invalid scalar ABI CALL-result return state')
			}
		}
	}

	for call_index, target in lowered_function.calls {
		call_context := '${context} call ${call_index}'
		gen_validate_scalar_abi_call(profile, call_context, target)!
		if return_value.kind == .void_t {
			if target.abi_result.present {
				return lowering_error(call_context, 'void wrapper calls a scalar-result signature')
			}
		} else if return_value.kind == .scalar_call_result {
			if target.abi_result != return_value.abi_result {
				return lowering_error(call_context, 'CALL result does not match wrapper result')
			}
		} else {
			return lowering_error(call_context, 'leaf scalar ABI function contains a call')
		}
		if target.kind == .definition {
			target_function := functions[int(target.index)]
			if !target_function.uses_scalar_abi
				|| target.abi_arguments.len != target_function.abi_parameters.len
				|| target.abi_result != target_function.abi_result {
				return lowering_error(call_context, 'internal target signature snapshot mismatch')
			}
			for argument_index, argument in target.abi_arguments {
				if argument.transfer != target_function.abi_parameters[argument_index] {
					return lowering_error(call_context,
						'internal target parameter ${argument_index} snapshot mismatch')
				}
			}
		}
	}
}

fn gen_emit_scalar_abi_stack_adjust(mut text []u8, decrement int, subtract bool) ! {
	match decrement {
		8 {
			if subtract {
				emit_sub_rsp_8(mut text)
			} else {
				emit_add_rsp_8(mut text)
			}
		}
		40 {
			if subtract {
				emit_sub_rsp_40(mut text)
			} else {
				emit_add_rsp_40(mut text)
			}
		}
		else {
			return abi_consume_requires_memory_agg()
		}
	}
}

fn gen_emit_scalar_abi_argument(mut text []u8, argument AbiScalarConstantArgument) ! {
	width := argument.transfer.location.width_bytes
	match argument.transfer.location.kind {
		.gpr {
			emit_mov_gpr_imm(mut text, argument.transfer.location.register, width, argument.bits)!
		}
		.stack {
			emit_mov_gpr_imm(mut text, .rax, width, argument.bits)!
			emit_mov_rsp_offset_rax(mut text, argument.transfer.location.caller_stack_offset_bytes,
				width)!
		}
		else {
			return lowering_error('scalar ABI emission', 'argument has no integer location')
		}
	}
}

fn gen_emit_scalar_abi_function(profile TargetProfile, lowered_function LoweredFunction, mut text []u8, mut call_sites []CallRel32Site) ! {
	match lowered_function.return_value.kind {
		.void_t {
			if lowered_function.calls.len == 1 {
				target := lowered_function.calls[0]
				gen_emit_scalar_abi_stack_adjust(mut text, target.abi_stack_decrement_bytes, true)!
				for argument in target.abi_arguments {
					gen_emit_scalar_abi_argument(mut text, argument)!
				}
				call_sites << emit_call_rel32_placeholder(mut text)
				emit_xor_eax_eax(mut text)
				gen_emit_scalar_abi_stack_adjust(mut text, target.abi_stack_decrement_bytes, false)!
			} else {
				emit_xor_eax_eax(mut text)
			}
			emit_ret(mut text)
		}
		.scalar_constant {
			emit_mov_gpr_imm(mut text, .rax,
				lowered_function.return_value.abi_result.location.width_bytes,
				lowered_function.return_value.bits)!
			emit_ret(mut text)
		}
		.scalar_parameter {
			source := lowered_function.return_value.abi_parameter.location
			match source.kind {
				.gpr {
					emit_mov_rax_gpr(mut text, source.register, source.width_bytes)!
				}
				.stack {
					emit_mov_rax_rsp_offset(mut text, source.callee_stack_offset_bytes,
						source.width_bytes)!
				}
				else {
					return lowering_error('scalar ABI emission',
						'parameter has no integer location')
				}
			}

			emit_ret(mut text)
		}
		.scalar_call_result {
			target := lowered_function.calls[0]
			gen_emit_scalar_abi_stack_adjust(mut text, target.abi_stack_decrement_bytes, true)!
			for argument in target.abi_arguments {
				gen_emit_scalar_abi_argument(mut text, argument)!
			}
			call_sites << emit_call_rel32_placeholder(mut text)
			gen_emit_scalar_abi_stack_adjust(mut text, target.abi_stack_decrement_bytes, false)!
			emit_ret(mut text)
		}
	}

	_ = profile
}

fn gen_emit_scalar_abi_memory_frame_function(lowered_function LoweredFunction, frame &GenMemoryFrame, mut text []u8, mut call_sites []CallRel32Site) ! {
	if !frame.present || lowered_function.calls.len != 1
		|| lowered_function.return_value.kind != .void_t {
		return gen_memory_frame_activation_error(frame.source_function_index,
			'invalid snapshotted M7 emission state')
	}
	text << frame.prologue_bytes
	call_sites << emit_call_rel32_placeholder(mut text)
	emit_xor_eax_eax(mut text)
	text << frame.epilogue_bytes
	emit_ret(mut text)
}

fn gen_scalar_abi_function_text_size(profile TargetProfile, lowered_function LoweredFunction) !int {
	mut text := []u8{cap: 128}
	mut call_sites := []CallRel32Site{cap: 1}
	gen_emit_scalar_abi_function(profile, lowered_function, mut text, mut call_sites)!
	return text.len
}

fn gen_function_text_size(call_count int) !int {
	if call_count < 0 {
		return lowering_error('generation', 'call count must not be negative')
	}
	if call_count == 0 {
		return 3
	}
	if call_count > (max_int - 11) / 5 {
		return lowering_error('generation',
			'call count ${call_count} exceeds representable function text size')
	}
	return 11 + 5 * call_count
}

fn gen_flat_function_text_size(context string, calls []LoweredCallTarget, return_value LoweredReturnValue) !int {
	kind_value := int(return_value.kind)
	if kind_value != int(LoweredReturnKind.void_t)
		&& kind_value != int(LoweredReturnKind.scalar_constant)
		&& kind_value != int(LoweredReturnKind.scalar_parameter)
		&& kind_value != int(LoweredReturnKind.scalar_call_result) {
		return lowering_error(context, 'unsupported return kind ${kind_value}')
	}
	match return_value.kind {
		.void_t {
			if return_value.bits != 0 {
				return lowering_error(context,
					'void return bits must be zero, got 0x${return_value.bits:016x}')
			}
			return gen_function_text_size(calls.len)
		}
		.scalar_constant {
			if calls.len != 0 {
				return lowering_error(context,
					'scalar-returning definition must not contain calls, got ${calls.len}')
			}
			return 11
		}
		.scalar_parameter {
			if return_value.bits != 0 {
				return lowering_error(context,
					'scalar parameter bits must be zero, got 0x${return_value.bits:016x}')
			}
			if calls.len != 0 {
				return lowering_error(context,
					'scalar parameter definition must not contain calls, got ${calls.len}')
			}
			return 4
		}
		.scalar_call_result {
			if return_value.bits != 0 {
				return lowering_error(context,
					'scalar CALL result bits must be zero, got 0x${return_value.bits:016x}')
			}
			if calls.len != 1 {
				return lowering_error(context,
					'scalar CALL result definition must contain exactly one call, got ${calls.len}')
			}
			gen_validate_call_argument('${context} call 0', calls[0])!
			return match calls[0].argument_mode {
				.none { 14 }
				.scalar_imm64 { 24 }
			}
		}
	}
}

fn gen_validate_multiblock_return_value(context string, return_value LoweredReturnValue) ! {
	kind_value := int(return_value.kind)
	if kind_value != int(LoweredReturnKind.void_t)
		&& kind_value != int(LoweredReturnKind.scalar_constant)
		&& kind_value != int(LoweredReturnKind.scalar_parameter)
		&& kind_value != int(LoweredReturnKind.scalar_call_result) {
		return lowering_error(context, 'unsupported return kind ${kind_value}')
	}
	if return_value.kind != .void_t {
		return lowering_error(context, 'scalar returns are unsupported in multiblock definitions')
	}
	if return_value.bits != 0 {
		return lowering_error(context,
			'void return bits must be zero, got 0x${return_value.bits:016x}')
	}
}

fn gen_checked_total_text_size(total int, function_size int) !int {
	if total < 0 || function_size < 0 {
		return lowering_error('generation', 'text sizes must not be negative')
	}
	if function_size > max_int - total {
		return lowering_error('generation', 'aggregate text size exceeds max_int')
	}
	return total + function_size
}

fn gen_checked_public_symbol_count(function_count u64, external_count u64) !u32 {
	if external_count > max_u64 - function_count {
		return lowering_error('generation', 'public symbol count overflows u64')
	}
	total := function_count + external_count
	if total > u64(max_u32) {
		return lowering_error('generation', 'public symbol count exceeds u32')
	}
	return u32(total)
}

fn gen_checked_call_text_size(call_count int) !int {
	if call_count < 0 {
		return lowering_error('generation', 'call count must not be negative')
	}
	if call_count > max_int / 5 {
		return lowering_error('generation',
			'call count ${call_count} exceeds representable block text size')
	}
	return call_count * 5
}

fn gen_validate_call_argument(context string, target LoweredCallTarget) ! {
	if target.uses_scalar_abi {
		if target.argument_mode != .none || target.argument_bits != 0 {
			return lowering_error(context, 'scalar ABI call has legacy argument state')
		}
		return
	}
	mode_value := int(target.argument_mode)
	if mode_value != int(LoweredCallArgumentMode.none)
		&& mode_value != int(LoweredCallArgumentMode.scalar_imm64) {
		return lowering_error(context, 'unsupported call argument mode ${mode_value}')
	}
	if target.argument_mode == .none && target.argument_bits != 0 {
		return lowering_error(context,
			'no-argument call bits must be zero, got 0x${target.argument_bits:016x}')
	}
	if target.abi_arguments.len != 0 || target.abi_result.present
		|| target.abi_stack_decrement_bytes != 0 {
		return lowering_error(context, 'legacy call has scalar ABI state')
	}
}

fn gen_validate_call_target(context string, target LoweredCallTarget, function_count int, external_count int) ! {
	gen_validate_call_argument(context, target)!
	kind_value := int(target.kind)
	if kind_value != int(LoweredCallTargetKind.definition)
		&& kind_value != int(LoweredCallTargetKind.external) {
		return lowering_error(context, 'unsupported call target kind ${kind_value}')
	}
	match target.kind {
		.definition {
			if u64(target.index) >= u64(function_count) {
				return lowering_error(context,
					'function target ${target.index} is outside 0..${function_count - 1}')
			}
		}
		.external {
			if u64(target.index) >= u64(external_count) {
				return lowering_error(context,
					'external target ${target.index} is outside 0..${external_count - 1}')
			}
		}
	}
}

fn gen_validate_flat_call_target(context string, return_value LoweredReturnValue, target LoweredCallTarget, functions []LoweredFunction) ! {
	if target.uses_scalar_abi {
		return
	}
	if return_value.kind == .scalar_call_result {
		if target.kind == .external {
			return
		}
		target_function := functions[int(target.index)]
		match target.argument_mode {
			.none {
				if target_function.return_value.kind != .scalar_constant {
					return lowering_error(context,
						'scalar CALL result target `${target_function.name}` must be an M4-C scalar leaf')
				}
			}
			.scalar_imm64 {
				if target_function.return_value.kind != .scalar_parameter {
					return lowering_error(context,
						'scalar immediate CALL target `${target_function.name}` must be an M4-E scalar parameter leaf')
				}
			}
		}

		return
	}
	if target.argument_mode != .none {
		return lowering_error(context,
			'scalar immediate argument is only valid on a scalar CALL-result definition')
	}
	if target.kind == .definition && functions[int(target.index)].return_value.kind != .void_t {
		return lowering_error(context,
			'non-scalar-CALL-result definition cannot call scalar-returning definition `${functions[int(target.index)].name}`')
	}
}

fn gen_multiblock_function_layout(function_index int, lowered_function LoweredFunction, function_count int, external_count int) !GenFunctionLayout {
	context := 'generation function ${function_index}'
	if lowered_function.calls.len != 0 {
		return lowering_error(context,
			'multiblock plan must not contain legacy flat calls, got ${lowered_function.calls.len}')
	}
	if lowered_function.blocks.len < 2 {
		return lowering_error(context,
			'multiblock plan must contain at least 2 blocks, got ${lowered_function.blocks.len}')
	}
	gen_validate_multiblock_return_value('${context} flat return state',
		lowered_function.return_value)!

	mut has_calls := false
	mut ret_count := 0
	for block_index, block in lowered_function.blocks {
		gen_validate_multiblock_return_value('${context} block ${block_index} return state',
			block.return_value)!
		terminator_value := int(block.terminator)
		if terminator_value != int(LoweredBlockTerminator.ret)
			&& terminator_value != int(LoweredBlockTerminator.jmp) {
			return lowering_error('${context} block ${block_index}',
				'unsupported terminator ${terminator_value}')
		}
		for call_index, target in block.calls {
			gen_validate_call_target('${context} block ${block_index} call ${call_index}', target,
				function_count, external_count)!
		}
		has_calls = has_calls || block.calls.len != 0
		match block.terminator {
			.ret {
				if block.jump_target != -1 {
					return lowering_error('${context} block ${block_index}',
						'ret block jump target must be -1, got ${block.jump_target}')
				}
				ret_count++
			}
			.jmp {
				if block.jump_target < 0 || block.jump_target >= lowered_function.blocks.len {
					return lowering_error('${context} block ${block_index}',
						'jump target ${block.jump_target} is outside 0..${lowered_function.blocks.len - 1}')
				}
			}
		}
	}
	if ret_count != 1 {
		return lowering_error(context, 'exactly one ret block is required, got ${ret_count}')
	}

	mut blocks := []GenBlockLayout{cap: lowered_function.blocks.len}
	mut function_size := if has_calls { 4 } else { 0 }
	for block in lowered_function.blocks {
		block_offset := function_size
		call_text_size := gen_checked_call_text_size(block.calls.len)!
		mut block_size := call_text_size
		terminator_size := match block.terminator {
			.ret {
				if has_calls { 7 } else { 3 }
			}
			.jmp {
				5
			}
		}

		block_size = gen_checked_total_text_size(block_size, terminator_size)!
		block_end := gen_checked_total_text_size(block_offset, block_size)!
		blocks << GenBlockLayout{
			offset: block_offset
			size:   block_size
			end:    block_end
		}
		function_size = block_end
	}
	for block_index, block in lowered_function.blocks {
		if block.terminator == .jmp {
			_ = checked_jmp_rel32_displacement(i64(blocks[block_index].end),
				i64(blocks[block.jump_target].offset))!
		}
	}
	return GenFunctionLayout{
		size:   function_size
		end:    function_size
		blocks: blocks
	}
}

fn gen_preflight(plan &LoweringPlan) !GenPreflight {
	_ = gen_checked_public_symbol_count(u64(plan.functions.len), u64(plan.externals.len))!
	mut public_names := map[string]bool{}
	for function_index, lowered_function in plan.functions {
		object_validate_symbol_name(lowered_function.name) or {
			return lowering_error('generation function ${function_index}', err.msg())
		}
		if public_names[lowered_function.name] {
			return lowering_error('generation function ${function_index}',
				'duplicate public symbol `${lowered_function.name}`')
		}
		public_names[lowered_function.name] = true
	}
	for symbol in plan.private_data.symbols {
		if public_names[symbol.name] {
			return lowering_error('generation private data',
				'symbol `${symbol.name}` collides with a public symbol')
		}
		public_names[symbol.name] = true
	}
	mut external_referenced := []bool{len: plan.externals.len}
	for external_index, external in plan.externals {
		object_validate_symbol_name(external.name) or {
			return lowering_error('generation external ${external_index}', err.msg())
		}
		if public_names[external.name] {
			return lowering_error('generation external ${external_index}',
				'symbol `${external.name}` collides with an emitted symbol')
		}
		public_names[external.name] = true
	}
	mut functions := []GenFunctionLayout{cap: plan.functions.len}
	mut total_text_size := 0
	for function_index, lowered_function in plan.functions {
		mut local_layout := GenFunctionLayout{}
		if plan.uses_scalar_abi {
			if !lowered_function.uses_scalar_abi || lowered_function.blocks.len != 0 {
				return lowering_error('generation function ${function_index}',
					'invalid scalar ABI lowering plan')
			}
			for call_index, target in lowered_function.calls {
				call_context := 'generation function ${function_index} call ${call_index}'
				gen_validate_call_target(call_context, target, plan.functions.len,
					plan.externals.len)!
				if target.kind == .external {
					external_referenced[int(target.index)] = true
				}
			}
			gen_validate_scalar_abi_function(plan.profile, 'generation function ${function_index}',
				lowered_function, plan.functions)!
			local_layout = GenFunctionLayout{
				size: gen_scalar_abi_function_text_size(plan.profile, lowered_function)!
			}
		} else if lowered_function.uses_scalar_abi {
			return lowering_error('generation function ${function_index}',
				'legacy plan contains scalar ABI state')
		} else if lowered_function.blocks.len == 0 {
			flat_context := 'generation function ${function_index}'
			flat_size := gen_flat_function_text_size(flat_context, lowered_function.calls,
				lowered_function.return_value)!
			for call_index, target in lowered_function.calls {
				call_context := 'generation function ${function_index} call ${call_index}'
				gen_validate_call_target(call_context, target, plan.functions.len,
					plan.externals.len)!
				gen_validate_flat_call_target(call_context, lowered_function.return_value, target,
					plan.functions)!
				if target.kind == .external {
					external_referenced[int(target.index)] = true
				}
			}
			local_layout = GenFunctionLayout{
				size: flat_size
			}
		} else {
			for block_index, block in lowered_function.blocks {
				for call_index, target in block.calls {
					call_context := 'generation function ${function_index} block ${block_index} call ${call_index}'
					gen_validate_call_target(call_context, target, plan.functions.len,
						plan.externals.len)!
					gen_validate_flat_call_target(call_context, lowered_function.return_value,
						target, plan.functions)!
					if target.kind == .external {
						external_referenced[int(target.index)] = true
					}
				}
			}
			local_layout = gen_multiblock_function_layout(function_index, lowered_function,
				plan.functions.len, plan.externals.len)!
		}
		function_end := gen_checked_total_text_size(total_text_size, local_layout.size)!
		functions << GenFunctionLayout{
			offset: total_text_size
			size:   local_layout.size
			end:    function_end
			blocks: local_layout.blocks
		}
		total_text_size = function_end
	}
	for external_index, is_referenced in external_referenced {
		if !is_referenced {
			return lowering_error('generation external ${external_index}',
				'symbol `${plan.externals[external_index].name}` has no CALL')
		}
	}
	if plan.profile == .windows_x86_64_microsoft_abi_coff {
		coff64_validate_plan_external_names(plan)!
	}
	return GenPreflight{
		functions:       functions
		total_text_size: total_text_size
	}
}

// gen returns a fresh deterministic target-profile relocatable object.
pub fn (g &Gen) gen() ![]u8 {
	preflight := gen_preflight(&g.plan)!
	explicit_memory_frames := g.memory_frames.len != 0
	if explicit_memory_frames && g.memory_frames.len != g.plan.functions.len {
		return gen_memory_frame_activation_error(-1, 'snapshotted frame/function count mismatch')
	}
	mut object := Object.new()
	mut symbols := []SymbolID{cap: g.plan.functions.len}
	for lowered_function in g.plan.functions {
		symbols << object.intern_function_symbol(lowered_function.name)!
	}
	object.install_private_data(&g.plan.private_data)!
	mut external_symbols := []SymbolID{cap: g.plan.externals.len}
	for external in g.plan.externals {
		external_symbols << object.intern_external_function_symbol(external.name)!
	}

	mut emitted_text_size := 0
	for function_index, lowered_function in g.plan.functions {
		layout := preflight.functions[function_index]
		mut text := []u8{cap: layout.size}
		mut call_sites := []CallRel32Site{}
		mut call_targets := []LoweredCallTarget{}
		if lowered_function.blocks.len == 0 {
			call_sites = []CallRel32Site{cap: lowered_function.calls.len}
			call_targets = lowered_function.calls.clone()
			if g.plan.uses_scalar_abi {
				if explicit_memory_frames {
					frame := g.memory_frames[function_index]
					if frame.present != (lowered_function.calls.len != 0) {
						return gen_memory_frame_activation_error(frame.source_function_index,
							'snapshotted caller/frame presence mismatch')
					}
					if frame.present {
						gen_emit_scalar_abi_memory_frame_function(lowered_function, &frame, mut
							text, mut call_sites)!
					} else {
						gen_emit_scalar_abi_function(g.plan.profile, lowered_function, mut text, mut
							call_sites)!
					}
				} else {
					gen_emit_scalar_abi_function(g.plan.profile, lowered_function, mut text, mut
						call_sites)!
				}
			} else {
				match lowered_function.return_value.kind {
					.void_t {
						if lowered_function.calls.len != 0 {
							match g.plan.profile {
								.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
									emit_sub_rsp_8(mut text)
								}
								.windows_x86_64_microsoft_abi_coff {
									emit_sub_rsp_40(mut text)
								}
							}

							for _ in lowered_function.calls {
								call_sites << emit_call_rel32_placeholder(mut text)
							}
						}
						emit_xor_eax_eax(mut text)
						if lowered_function.calls.len != 0 {
							match g.plan.profile {
								.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
									emit_add_rsp_8(mut text)
								}
								.windows_x86_64_microsoft_abi_coff {
									emit_add_rsp_40(mut text)
								}
							}
						}
						emit_ret(mut text)
					}
					.scalar_constant {
						emit_mov_rax_imm64(mut text, lowered_function.return_value.bits)
						emit_ret(mut text)
					}
					.scalar_parameter {
						match g.plan.profile {
							.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
								emit_mov_rax_rdi(mut text)
							}
							.windows_x86_64_microsoft_abi_coff {
								emit_mov_rax_rcx(mut text)
							}
						}

						emit_ret(mut text)
					}
					.scalar_call_result {
						match g.plan.profile {
							.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
								emit_sub_rsp_8(mut text)
							}
							.windows_x86_64_microsoft_abi_coff {
								emit_sub_rsp_40(mut text)
							}
						}

						match lowered_function.calls[0].argument_mode {
							.none {}
							.scalar_imm64 {
								match g.plan.profile {
									.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
										emit_mov_rdi_imm64(mut text,
											lowered_function.calls[0].argument_bits)
									}
									.windows_x86_64_microsoft_abi_coff {
										emit_mov_rcx_imm64(mut text,
											lowered_function.calls[0].argument_bits)
									}
								}
							}
						}

						call_sites << emit_call_rel32_placeholder(mut text)
						match g.plan.profile {
							.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
								emit_add_rsp_8(mut text)
							}
							.windows_x86_64_microsoft_abi_coff {
								emit_add_rsp_40(mut text)
							}
						}

						emit_ret(mut text)
					}
				}
			}
		} else {
			mut has_calls := false
			for block in lowered_function.blocks {
				has_calls = has_calls || block.calls.len != 0
			}
			if has_calls {
				match g.plan.profile {
					.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
						emit_sub_rsp_8(mut text)
					}
					.windows_x86_64_microsoft_abi_coff {
						emit_sub_rsp_40(mut text)
					}
				}
			}
			for block_index, block in lowered_function.blocks {
				block_layout := layout.blocks[block_index]
				if text.len != block_layout.offset {
					return lowering_error('generation',
						'function ${function_index} block ${block_index} began at ${text.len}, expected ${block_layout.offset}')
				}
				for target in block.calls {
					call_sites << emit_call_rel32_placeholder(mut text)
					call_targets << target
				}
				match block.terminator {
					.ret {
						emit_xor_eax_eax(mut text)
						if has_calls {
							match g.plan.profile {
								.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
									emit_add_rsp_8(mut text)
								}
								.windows_x86_64_microsoft_abi_coff {
									emit_add_rsp_40(mut text)
								}
							}
						}
						emit_ret(mut text)
					}
					.jmp {
						site := emit_jmp_rel32_placeholder(mut text)
						patch_jmp_rel32(mut text, site, layout.blocks[block.jump_target].offset)!
					}
				}

				if text.len != block_layout.end {
					return lowering_error('generation',
						'function ${function_index} block ${block_index} ended at ${text.len}, expected ${block_layout.end}')
				}
			}
		}
		if text.len != layout.size {
			return lowering_error('generation',
				'function ${function_index} emitted ${text.len} bytes, expected ${layout.size}')
		}
		if call_sites.len != call_targets.len {
			return lowering_error('generation',
				'function ${function_index} emitted ${call_sites.len} call sites, expected ${call_targets.len}')
		}

		start := object.append_text(text)!
		if start != u64(layout.offset) {
			return lowering_error('generation',
				'function ${function_index} began at ${start}, expected ${layout.offset}')
		}
		object.define_text_function(symbols[function_index], start, u64(text.len))!
		for call_index, target in call_targets {
			target_symbol := match target.kind {
				.definition { symbols[int(target.index)] }
				.external { external_symbols[int(target.index)] }
			}

			object.add_text_call_relocation(start + call_sites[call_index].field_offset,
				target_symbol)!
		}
		if explicit_memory_frames && g.memory_frames[function_index].present {
			frame := g.memory_frames[function_index]
			object.add_function_frame(symbols[function_index], frame.prologue_bytes,
				frame.epilogue_bytes, frame.windows_unwind_bytes)!
		}
		emitted_text_size = layout.end
	}
	if emitted_text_size != preflight.total_text_size {
		return lowering_error('generation',
			'emitted text size ${emitted_text_size} does not match preflight ${preflight.total_text_size}')
	}
	return match g.plan.profile {
		.linux_x86_64_sysv_elf { elf64_relocatable_bytes(&object) }
		.macos_x86_64_sysv_macho { macho64_relocatable_bytes(&object) }
		.windows_x86_64_microsoft_abi_coff { coff64_relocatable_bytes(&object) }
	}
}
