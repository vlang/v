module amd64

import v3.ssa

// AbiDirectSignatureBinding is the required canonical signature sidecar for one
// direct-call-capable SSA function.
pub struct AbiDirectSignatureBinding {
pub:
	function_index int
	function_type  ssa.TypeID
	call_kind      AbiCallKind
}

struct AbiScalarTransfer {
	present                 bool
	type_id                 ssa.TypeID
	semantic_width_bits     int
	abi_transfer_width_bits int
	semantic_is_unsigned    bool
	integral_extension      AbiIntegralExtension
	location                AbiLocation
}

struct AbiScalarConstantArgument {
	bits     u64
	transfer AbiScalarTransfer
}

struct AbiConsumeSignature {
	present                      bool
	function_type                ssa.TypeID
	call_kind                    AbiCallKind
	parameter_types              []ssa.TypeID
	return_type                  ssa.TypeID
	parameters                   []AbiScalarTransfer
	return_value                 AbiScalarTransfer
	outgoing_area_size_bytes     int
	frame_total_allocation_bytes int
}

struct AbiConsumeSignatureTable {
	signatures []AbiConsumeSignature
}

fn abi_consume_requires_memory_agg() IError {
	return abi_error('requires_memory_agg')
}

fn abi_consume_type_is_scalar_integer(type_store &ssa.TypeStore, type_id ssa.TypeID) bool {
	type_index := int(type_id)
	return type_index > 0 && type_index < type_store.types.len
		&& type_store.types[type_index].kind == .int_t
}

fn abi_consume_validate_top_level_types(type_store &ssa.TypeStore, function_type ssa.TypeID) !ssa.Type {
	type_index := int(function_type)
	if type_index < 0 || type_index >= type_store.types.len {
		return abi_error('invalid_function_type')
	}
	typ := type_store.types[type_index]
	if typ.kind != .func_t {
		return abi_error('invalid_function_type')
	}
	if typ.ret_type != ssa.TypeID(0)
		&& !abi_consume_type_is_scalar_integer(type_store, typ.ret_type) {
		return abi_consume_requires_memory_agg()
	}
	for parameter_type in typ.params {
		if !abi_consume_type_is_scalar_integer(type_store, parameter_type) {
			return abi_consume_requires_memory_agg()
		}
	}
	return typ
}

fn abi_consume_scalar_transfer(context string, type_store &ssa.TypeStore, type_id ssa.TypeID, value &AbiValueDecision, allow_stack bool, expected_return bool) !AbiScalarTransfer {
	if !abi_consume_type_is_scalar_integer(type_store, type_id) {
		return abi_consume_requires_memory_agg()
	}
	typ := type_store.types[int(type_id)]
	if value.type_id != type_id || value.mode != .direct || value.size_bytes <= 0
		|| value.alignment_bytes <= 0 || value.semantic_width_bits != typ.width
		|| value.semantic_is_unsigned != typ.is_unsigned || value.classes != [.integer]
		|| value.locations.len != 1 || value.has_indirect_temporary
		|| value.indirect_temporary_offset_bytes != 0
		|| value.indirect_temporary_size_bytes != 0
		|| value.indirect_temporary_alignment_bytes != 0 {
		return lowering_error(context, 'noncanonical scalar ABI decision')
	}
	if value.abi_transfer_width_bits !in [8, 16, 32, 64] {
		return lowering_error(context,
			'unsupported ABI transfer width ${value.abi_transfer_width_bits}')
	}
	match value.integral_extension {
		.none {}
		.sign_extend_to_32 {
			if typ.is_unsigned || typ.width >= 32 || value.abi_transfer_width_bits != 32 {
				return lowering_error(context, 'invalid signed integral extension')
			}
		}
		.zero_extend_to_32 {
			if (!typ.is_unsigned && typ.width != 1) || typ.width >= 32
				|| value.abi_transfer_width_bits != 32 {
				return lowering_error(context, 'invalid unsigned integral extension')
			}
		}
	}
	location := value.locations[0]
	if location.class != .integer || location.value_offset_bytes != 0
		|| location.width_bytes * 8 != value.abi_transfer_width_bits {
		return lowering_error(context, 'noncanonical scalar ABI location width')
	}
	if expected_return {
		if location.kind != .gpr || location.register != .rax || location.has_stack_address
			|| location.has_home_address {
			return lowering_error(context, 'scalar result is not in RAX')
		}
	} else {
		match location.kind {
			.gpr {
				if location.register !in [.rcx, .rdx, .rdi, .rsi, .r8, .r9]
					|| location.has_stack_address {
					return lowering_error(context, 'invalid scalar parameter GPR')
				}
			}
			.stack {
				if !allow_stack || location.register != .none || !location.has_stack_address
					|| location.has_home_address {
					return lowering_error(context, 'invalid scalar parameter stack location')
				}
			}
			else {
				return lowering_error(context, 'scalar parameter is not in a GPR or stack slot')
			}
		}
	}
	return AbiScalarTransfer{
		present:                 true
		type_id:                 type_id
		semantic_width_bits:     value.semantic_width_bits
		abi_transfer_width_bits: value.abi_transfer_width_bits
		semantic_is_unsigned:    value.semantic_is_unsigned
		integral_extension:      value.integral_extension
		location:                location
	}
}

fn abi_consume_snapshot_signatures(profile TargetProfile, type_store &ssa.TypeStore, function_count int, bindings []AbiDirectSignatureBinding) !AbiConsumeSignatureTable {
	mut signatures := []AbiConsumeSignature{len: function_count}
	layouts := AbiLayoutSnapshot{}
	proofs := AbiMicrosoftUdtEvidence{}
	for binding_index, binding in bindings {
		context := 'scalar ABI binding ${binding_index}'
		if binding.function_index < 0 || binding.function_index >= function_count {
			return lowering_error(context,
				'function index ${binding.function_index} is outside 0..${function_count - 1}')
		}
		if signatures[binding.function_index].present {
			return lowering_error(context,
				'function ${binding.function_index} duplicates an earlier signature binding')
		}
		if binding.call_kind != .prototyped {
			return abi_error('unsupported_call_kind')
		}
		function_typ := abi_consume_validate_top_level_types(type_store,
			binding.function_type)!
		if function_typ.params.len > 7 {
			return abi_consume_requires_memory_agg()
		}
		planned := classify_and_plan_abi_call(profile, binding.call_kind, type_store, &layouts,
			&proofs, binding.function_type)!
		if planned.decision.hidden_sret.present
			|| planned.decision.minimum_indirect_temporary_area_bytes != 0
			|| planned.frame.indirect_temporary_area_size_bytes != 0
			|| planned.frame.outgoing_area_offset_bytes != 0
			|| planned.frame.outgoing_area_size_bytes != planned.decision.minimum_outgoing_area_bytes {
			return abi_consume_requires_memory_agg()
		}
		mut parameter_transfers := []AbiScalarTransfer{cap: function_typ.params.len}
		if planned.decision.parameters.len != function_typ.params.len {
			return lowering_error(context, 'ABI parameter decision count mismatch')
		}
		for parameter_index, parameter_type in function_typ.params {
			parameter_transfers << abi_consume_scalar_transfer('${context} parameter ${parameter_index}',
				type_store, parameter_type, &planned.decision.parameters[parameter_index], true,
				false)!
		}
		mut return_transfer := AbiScalarTransfer{}
		if function_typ.ret_type == ssa.TypeID(0) {
			if planned.decision.return_value.mode != .no_value
				|| planned.decision.return_value.locations.len != 0 {
				return lowering_error(context, 'void signature has a scalar ABI result')
			}
		} else {
			return_transfer = abi_consume_scalar_transfer('${context} return', type_store,
				function_typ.ret_type, &planned.decision.return_value, false, true)!
		}
		signatures[binding.function_index] = AbiConsumeSignature{
			present:                      true
			function_type:                binding.function_type
			call_kind:                    binding.call_kind
			parameter_types:              function_typ.params.clone()
			return_type:                  function_typ.ret_type
			parameters:                   parameter_transfers
			return_value:                 return_transfer
			outgoing_area_size_bytes:     planned.frame.outgoing_area_size_bytes
			frame_total_allocation_bytes: planned.frame.total_allocation_bytes
		}
	}
	for function_index, signature in signatures {
		if !signature.present {
			return lowering_error('scalar ABI signatures',
				'function ${function_index} has no signature binding')
		}
	}
	return AbiConsumeSignatureTable{
		signatures: signatures
	}
}

fn abi_consume_transfer_mask(width_bits int) u64 {
	if width_bits == 64 {
		return max_u64
	}
	return (u64(1) << width_bits) - 1
}

fn abi_consume_transfer_bits(context string, canonical_bits u64, transfer AbiScalarTransfer) !u64 {
	if !transfer.present || transfer.semantic_width_bits !in [1, 8, 16, 32, 64]
		|| transfer.abi_transfer_width_bits !in [8, 16, 32, 64] {
		return lowering_error(context, 'invalid scalar transfer snapshot')
	}
	semantic_mask := abi_consume_transfer_mask(transfer.semantic_width_bits)
	mut bits := canonical_bits & semantic_mask
	match transfer.integral_extension {
		.none {}
		.sign_extend_to_32 {
			sign_bit := u64(1) << (transfer.semantic_width_bits - 1)
			if bits & sign_bit != 0 {
				bits |= u64(0xffff_ffff) & ~semantic_mask
			}
		}
		.zero_extend_to_32 {}
	}
	return bits & abi_consume_transfer_mask(transfer.abi_transfer_width_bits)
}

fn abi_consume_live_call_decrement(outgoing_area_size_bytes int) !int {
	with_return_address := abi_checked_add(outgoing_area_size_bytes, 8)!
	aligned := abi_checked_align_up(with_return_address, 16)!
	return abi_frame_checked_sub(aligned, 8)
}

fn abi_consume_validate_call_geometry(profile TargetProfile, argument_count int, signature &AbiConsumeSignature) !int {
	if argument_count != signature.parameters.len {
		return lowering_error('scalar ABI call', 'argument count does not match signature')
	}
	if argument_count > 7 {
		return abi_consume_requires_memory_agg()
	}
	decrement := abi_consume_live_call_decrement(signature.outgoing_area_size_bytes)!
	expected_total := abi_checked_align_up(signature.outgoing_area_size_bytes, 16)!
	if signature.frame_total_allocation_bytes != expected_total {
		return lowering_error('scalar ABI call', 'normalized frame extent mismatch')
	}
	match profile {
		.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
			if argument_count > 7 || decrement != 8 {
				return abi_consume_requires_memory_agg()
			}
		}
		.windows_x86_64_microsoft_abi_coff {
			if argument_count > 5 || decrement != 40 {
				return abi_consume_requires_memory_agg()
			}
		}
	}
	return decrement
}

fn abi_consume_external_semantic_name(context string, target &ssa.Function, function_ref &ssa.Value) !string {
	if !target.is_c_extern || target.linkage != .external || target.call_conv != .c_decl
		|| !target.name.starts_with('C.') || target.name.len == 2 {
		return lowering_error(context, 'target is not a callable C external declaration')
	}
	semantic_name := target.name[2..]
	if semantic_name.index_u8(u8(0)) >= 0 {
		return lowering_error(context, 'C external semantic name contains NUL')
	}
	if function_ref.name != semantic_name {
		return lowering_error(context,
			'function reference name `${function_ref.name}` does not match C external semantic name `${semantic_name}`')
	}
	return semantic_name
}

fn abi_consume_call_target(context string, target_index int, target &ssa.Function, function_ref &ssa.Value, original_to_dense []int, mut original_to_external []int, mut externals []ReferencedExternal, mut emitted_names map[string]bool) !LoweredCallTarget {
	dense_index := original_to_dense[target_index]
	if dense_index >= 0 {
		if function_ref.name != target.name {
			return lowering_error(context,
				'function reference name `${function_ref.name}` does not match target `${target.name}`')
		}
		if u64(dense_index) >= u64(max_u32) {
			return lowering_error(context, 'function target exceeds the lowering index range')
		}
		return LoweredCallTarget{
			kind:  .definition
			index: u32(dense_index)
		}
	}
	semantic_name := abi_consume_external_semantic_name(context, target, function_ref)!
	mut external_index := original_to_external[target_index]
	if external_index < 0 {
		if emitted_names[semantic_name] {
			return lowering_error(context,
				'C external semantic name `${semantic_name}` collides with an emitted symbol')
		}
		if u64(externals.len) >= u64(max_u32) {
			return lowering_error(context, 'too many referenced C externals')
		}
		external_index = externals.len
		externals << ReferencedExternal{
			name: semantic_name.clone()
		}
		original_to_external[target_index] = external_index
		emitted_names[semantic_name] = true
	}
	return LoweredCallTarget{
		kind:  .external
		index: u32(external_index)
	}
}

fn abi_consume_snapshot_direct_call(profile TargetProfile, m &ssa.Module, context string, call_value_id int, instruction &ssa.Instruction, signatures &AbiConsumeSignatureTable, binding_table &ScalarBindingTable, mut binding_use_counts []int, original_to_dense []int, mut original_to_external []int, mut externals []ReferencedExternal, mut emitted_names map[string]bool) !LoweredCallTarget {
	if instruction.op != .call || instruction.operands.len == 0 {
		return abi_consume_requires_memory_agg()
	}
	function_ref_id := int(instruction.operands[0])
	if function_ref_id <= 0 || function_ref_id >= m.values.len {
		return lowering_error(context, 'function reference is outside the value table')
	}
	function_ref := m.values[function_ref_id]
	if function_ref.id != ssa.ValueID(function_ref_id) || function_ref.kind != .func_ref {
		return abi_consume_requires_memory_agg()
	}
	target_index := function_ref.index
	if target_index < 0 || target_index >= m.funcs.len {
		return lowering_error(context, 'function target is outside the function table')
	}
	target := m.funcs[target_index]
	signature := signatures.signatures[target_index]
	if signature.call_kind != .prototyped {
		return abi_error('unsupported_call_kind')
	}
	if instruction.operands.len != signature.parameter_types.len + 1 {
		return lowering_error(context,
			'direct call has ${instruction.operands.len - 1} arguments, signature requires ${signature.parameter_types.len}')
	}
	if function_ref.typ != signature.return_type || instruction.typ != signature.return_type {
		return lowering_error(context, 'direct call result type does not match signature')
	}
	call_value := m.values[call_value_id]
	if call_value.typ != signature.return_type {
		return lowering_error(context, 'direct call value type does not match signature')
	}
	decrement := abi_consume_validate_call_geometry(profile, signature.parameter_types.len,
		&signature)!
	mut arguments := []AbiScalarConstantArgument{cap: signature.parameter_types.len}
	for argument_index, parameter_type in signature.parameter_types {
		argument_id := int(instruction.operands[argument_index + 1])
		if argument_id <= 0 || argument_id >= m.values.len {
			return lowering_error(context, 'call argument ${argument_index} is outside the value table')
		}
		argument := m.values[argument_id]
		if argument.id != ssa.ValueID(argument_id) || argument.kind != .constant {
			return abi_consume_requires_memory_agg()
		}
		if argument.typ != parameter_type {
			return lowering_error(context,
				'call argument ${argument_index} type ${argument.typ} does not match signature type ${parameter_type}')
		}
		binding_index := binding_table.index_by_value[argument_id]
		if binding_index < 0 {
			return lowering_error(context,
				'scalar binding for call argument ${argument_id} is missing')
		}
		transfer := signature.parameters[argument_index]
		arguments << AbiScalarConstantArgument{
			bits: abi_consume_transfer_bits('${context} argument ${argument_index}',
				binding_table.canonical_bits[binding_index], transfer)!
			transfer: transfer
		}
		binding_use_counts[binding_index]++
	}
	base_target := abi_consume_call_target(context, target_index, &target, &function_ref,
		original_to_dense, mut original_to_external, mut externals, mut emitted_names)!
	return LoweredCallTarget{
		kind:                      base_target.kind
		index:                     base_target.index
		uses_scalar_abi:           true
		abi_arguments:             arguments
		abi_result:                signature.return_value
		abi_stack_decrement_bytes: decrement
	}
}

fn abi_consume_validate_parameter(m &ssa.Module, function_index int, parameter_index int, parameter_id int, expected_type ssa.TypeID, mut owners map[int]string) ! {
	context := 'function ${function_index} parameter ${parameter_index}'
	if parameter_id <= 0 || parameter_id >= m.values.len {
		return lowering_error(context, 'value is outside the value table')
	}
	if owners[parameter_id].len != 0 {
		return lowering_error(context, 'value ${parameter_id} is already owned by ${owners[parameter_id]}')
	}
	parameter := m.values[parameter_id]
	if parameter.id != ssa.ValueID(parameter_id) || parameter.kind != .argument
		|| parameter.index != parameter_index || parameter.typ != expected_type {
		return lowering_error(context, 'parameter does not match canonical signature position')
	}
	owners[parameter_id] = context
}

fn abi_consume_instruction(m &ssa.Module, function_index int, block_index int, instruction_position int, value_id int, mut value_owners map[int]string, mut instruction_owners map[int]string) !(ssa.Value, ssa.Instruction) {
	context := 'function ${function_index} block ${block_index} instruction ${instruction_position}'
	if value_id <= 0 || value_id >= m.values.len {
		return lowering_error(context, 'instruction value is outside the value table')
	}
	if value_owners[value_id].len != 0 {
		return lowering_error(context, 'instruction value ${value_id} is already owned')
	}
	value := m.values[value_id]
	if value.id != ssa.ValueID(value_id) || value.kind != .instruction || value.index < 0
		|| value.index >= m.instrs.len {
		return lowering_error(context, 'invalid instruction value')
	}
	if instruction_owners[value.index].len != 0 {
		return lowering_error(context, 'instruction ${value.index} is already owned')
	}
	instruction := m.instrs[value.index]
	if instruction.block != ssa.BlockID(block_index) {
		return lowering_error(context, 'instruction block does not match owning block')
	}
	value_owners[value_id] = context
	instruction_owners[value.index] = context
	return value, instruction
}

fn abi_consume_snapshot_leaf_return(m &ssa.Module, context string, function &ssa.Function, signature &AbiConsumeSignature, instruction &ssa.Instruction, binding_table &ScalarBindingTable, mut binding_use_counts []int) !LoweredReturnValue {
	if instruction.op != .ret || instruction.typ != ssa.TypeID(0) {
		return abi_consume_requires_memory_agg()
	}
	if signature.return_type == ssa.TypeID(0) {
		if instruction.operands.len != 0 {
			return lowering_error(context, 'void RET must be operandless')
		}
		return LoweredReturnValue{
			kind: .void_t
		}
	}
	if instruction.operands.len != 1 {
		return lowering_error(context, 'scalar RET must have exactly one operand')
	}
	return_id := int(instruction.operands[0])
	if return_id <= 0 || return_id >= m.values.len {
		return lowering_error(context, 'return value is outside the value table')
	}
	return_value := m.values[return_id]
	if return_value.id != ssa.ValueID(return_id) || return_value.typ != signature.return_type {
		return lowering_error(context, 'return value does not match signature result type')
	}
	if return_value.kind == .constant {
		binding_index := binding_table.index_by_value[return_id]
		if binding_index < 0 {
			return lowering_error(context, 'scalar binding for return value ${return_id} is missing')
		}
		binding_use_counts[binding_index]++
		return LoweredReturnValue{
			kind:       .scalar_constant
			bits:       abi_consume_transfer_bits(context,
				binding_table.canonical_bits[binding_index], signature.return_value)!
			abi_result: signature.return_value
		}
	}
	if return_value.kind != .argument {
		return abi_consume_requires_memory_agg()
	}
	mut parameter_index := -1
	for index, parameter_id in function.params {
		if parameter_id == ssa.ValueID(return_id) {
			parameter_index = index
			break
		}
	}
	if parameter_index < 0 || parameter_index >= signature.parameters.len {
		return lowering_error(context, 'RET argument is not a canonical function parameter')
	}
	return LoweredReturnValue{
		kind:            .scalar_parameter
		parameter_index: parameter_index
		abi_parameter:   signature.parameters[parameter_index]
		abi_result:      signature.return_value
	}
}

fn validate_and_snapshot_with_scalar_abi(profile TargetProfile, m &ssa.Module, signatures []AbiDirectSignatureBinding, bindings []ScalarConstantBinding) !LoweringPlan {
	_ = abi_kind_for_target_profile(profile)!
	if m.target.ptr_size != 8 {
		return lowering_error('target', 'pointer size must be 8 bytes, got ${m.target.ptr_size}')
	}
	if !m.target.endian_little {
		return lowering_error('target', 'little-endian target data is required')
	}
	if m.type_store.types.len == 0 || !abi_is_canonical_void(m.type_store.types[0]) {
		return abi_error('invalid_type_graph')
	}
	signature_table := abi_consume_snapshot_signatures(profile, &m.type_store, m.funcs.len,
		signatures)!
	binding_table := snapshot_scalar_constant_bindings(m, bindings)!
	mut binding_use_counts := []int{len: bindings.len}
	mut names := map[string]int{}
	mut function_names := []string{cap: m.funcs.len}
	mut definition_indices := []int{}
	mut original_to_dense := []int{len: m.funcs.len, init: -1}
	mut parameter_owners := map[int]string{}
	for function_index, function in m.funcs {
		context := 'function ${function_index}'
		if function.id != function_index {
			return lowering_error(context, 'id ${function.id} does not match array index')
		}
		if function.name.len == 0 || function.name.index_u8(u8(0)) >= 0 {
			return lowering_error(context, 'invalid symbol name')
		}
		if function.name in names {
			return lowering_error(context,
				'symbol `${function.name}` duplicates function ${names[function.name]}')
		}
		names[function.name] = function_index
		function_names << function.name.clone()
		if function.linkage != .external || function.call_conv != .c_decl {
			return lowering_error(context, 'scalar ABI functions require external c_decl')
		}
		signature := signature_table.signatures[function_index]
		if function.typ != signature.return_type {
			return lowering_error(context,
				'return type ${function.typ} does not match signature ${signature.return_type}')
		}
		if function.is_c_extern {
			if function.blocks.len != 0 {
				return lowering_error(context, 'C external declaration must not have body blocks')
			}
			if !function.name.starts_with('C.') || function.name.len == 2 {
				return lowering_error(context, 'C external name must have a nonempty `C.` prefix')
			}
			continue
		}
		if function.is_prototype {
			return abi_consume_requires_memory_agg()
		}
		if function.params.len != signature.parameter_types.len {
			return lowering_error(context,
				'definition has ${function.params.len} parameters, signature requires ${signature.parameter_types.len}')
		}
		for parameter_index, parameter_id in function.params {
			abi_consume_validate_parameter(m, function_index, parameter_index, int(parameter_id),
				signature.parameter_types[parameter_index], mut parameter_owners)!
		}
		if function.blocks.len != 1 {
			return abi_consume_requires_memory_agg()
		}
		original_to_dense[function_index] = definition_indices.len
		definition_indices << function_index
	}
	private_data := validate_and_snapshot_private_data(m, function_names)!
	mut emitted_names := map[string]bool{}
	for function_index in definition_indices {
		emitted_names[m.funcs[function_index].name] = true
	}
	for symbol in private_data.symbols {
		emitted_names[symbol.name] = true
	}
	mut original_to_external := []int{len: m.funcs.len, init: -1}
	mut externals := []ReferencedExternal{}
	mut block_owners := map[int]string{}
	mut value_owners := map[int]string{}
	mut instruction_owners := map[int]string{}
	mut functions := []LoweredFunction{cap: definition_indices.len}
	for function_index in definition_indices {
		function := m.funcs[function_index]
		signature := signature_table.signatures[function_index]
		block_index := int(function.blocks[0])
		context := 'function ${function_index}'
		if block_index < 0 || block_index >= m.blocks.len {
			return lowering_error(context, 'body block is outside the block table')
		}
		if block_owners[block_index].len != 0 {
			return lowering_error(context, 'body block ${block_index} is already owned')
		}
		block := m.blocks[block_index]
		if block.id != ssa.BlockID(block_index) || block.val_id != ssa.ValueID(0)
			|| block.parent != function_index {
			return lowering_error(context, 'invalid body block identity')
		}
		block_owners[block_index] = context
		if block.instrs.len == 0 || block.instrs.len > 2 {
			return abi_consume_requires_memory_agg()
		}
		if block.instrs.len == 1 {
			value_id := int(block.instrs[0])
			value, instruction := abi_consume_instruction(m, function_index, block_index, 0,
				value_id, mut value_owners, mut instruction_owners)!
			if value.typ != ssa.TypeID(0) {
				return lowering_error(context, 'RET instruction value must have void type')
			}
			return_value := abi_consume_snapshot_leaf_return(m, context, &function, &signature,
				&instruction, &binding_table, mut binding_use_counts)!
			functions << LoweredFunction{
				name:            function.name.clone()
				return_value:    return_value
				uses_scalar_abi: true
				abi_parameters:  signature.parameters.clone()
				abi_result:      signature.return_value
			}
			continue
		}
		if function.params.len != 0 {
			return abi_consume_requires_memory_agg()
		}
		call_value_id := int(block.instrs[0])
		call_value, call_instruction := abi_consume_instruction(m, function_index, block_index,
			0, call_value_id, mut value_owners, mut instruction_owners)!
		call_target := abi_consume_snapshot_direct_call(profile, m, '${context} call',
			call_value_id, &call_instruction, &signature_table, &binding_table, mut
			binding_use_counts, original_to_dense, mut original_to_external, mut externals,
			mut emitted_names)!
		ret_value_id := int(block.instrs[1])
		ret_value, ret_instruction := abi_consume_instruction(m, function_index, block_index,
			1, ret_value_id, mut value_owners, mut instruction_owners)!
		if ret_value.typ != ssa.TypeID(0) || ret_instruction.typ != ssa.TypeID(0)
			|| ret_instruction.op != .ret {
			return abi_consume_requires_memory_agg()
		}
		mut return_value := LoweredReturnValue{}
		if signature.return_type == ssa.TypeID(0) {
			if call_value.typ != ssa.TypeID(0) || call_target.abi_result.present
				|| ret_instruction.operands.len != 0 {
				return lowering_error(context, 'void call wrapper signature mismatch')
			}
			return_value = LoweredReturnValue{
				kind: .void_t
			}
		} else {
			if call_value.typ != signature.return_type || !call_target.abi_result.present
				|| call_target.abi_result.type_id != signature.return_type
				|| ret_instruction.operands != [ssa.ValueID(call_value_id)] {
				return lowering_error(context, 'CALL result and RET do not match wrapper signature')
			}
			return_value = LoweredReturnValue{
				kind:       .scalar_call_result
				abi_result: signature.return_value
			}
		}
		functions << LoweredFunction{
			name:            function.name.clone()
			calls:           [call_target]
			return_value:    return_value
			uses_scalar_abi: true
			abi_parameters:  signature.parameters.clone()
			abi_result:      signature.return_value
		}
	}
	for binding_index, use_count in binding_use_counts {
		if use_count == 0 {
			return lowering_error('scalar binding ${binding_index}',
				'value ${bindings[binding_index].value_id} is not consumed by any approved scalar return or call argument')
		}
	}
	return LoweringPlan{
		profile:         profile
		functions:       functions
		externals:       externals
		private_data:    private_data
		uses_scalar_abi: true
	}
}
