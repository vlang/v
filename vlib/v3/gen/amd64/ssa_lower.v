module amd64

import v3.ssa

// ScalarConstantBinding supplies immutable typed bits for one SSA constant value.
pub struct ScalarConstantBinding {
pub:
	value_id ssa.ValueID
	type_id  ssa.TypeID
	raw_bits u64
}

enum LoweredBlockTerminator {
	ret
	jmp
}

enum LoweredReturnKind {
	void_t
	scalar_constant
	scalar_parameter
	scalar_call_result
}

struct LoweredReturnValue {
	kind            LoweredReturnKind
	bits            u64
	parameter_index int = -1
	abi_parameter   AbiScalarTransfer
	abi_result      AbiScalarTransfer
}

type ExternalID = u32

enum LoweredCallTargetKind {
	definition
	external
}

enum LoweredCallArgumentMode {
	none
	scalar_imm64
}

struct LoweredCallTarget {
	kind                      LoweredCallTargetKind
	index                     u32
	argument_mode             LoweredCallArgumentMode
	argument_bits             u64
	uses_scalar_abi           bool
	abi_arguments             []AbiScalarConstantArgument
	abi_result                AbiScalarTransfer
	abi_stack_decrement_bytes int
}

struct LoweredScalarCallSnapshot {
	target        LoweredCallTarget
	binding_index int = -1
}

struct LoweredScalarArgumentSnapshot {
	bits          u64
	binding_index int
}

struct ReferencedExternal {
	name string
}

struct LoweredBlock {
	calls        []LoweredCallTarget
	terminator   LoweredBlockTerminator
	jump_target  int = -1
	return_value LoweredReturnValue
}

struct LoweredFunction {
	name            string
	calls           []LoweredCallTarget
	blocks          []LoweredBlock
	return_value    LoweredReturnValue
	uses_scalar_abi bool
	abi_parameters  []AbiScalarTransfer
	abi_result      AbiScalarTransfer
}

struct LoweringPlan {
	profile      TargetProfile @[required]
	functions    []LoweredFunction
	externals    []ReferencedExternal
	private_data PrivateDataPlan
	uses_scalar_abi bool
}

struct StructuralTypePathSegment {
	parent int
	text   string
}

struct StructuralTypeWorkItem {
	type_id    ssa.TypeID
	path_index int
}

struct ScalarBindingTable {
	index_by_value []int
	canonical_bits []u64
}

fn structural_type_label(root string, path_index int, paths []StructuralTypePathSegment) string {
	mut reversed := []string{}
	mut current := path_index
	for current >= 0 {
		segment := paths[current]
		reversed << segment.text
		current = segment.parent
	}
	mut parts := []string{cap: reversed.len + 1}
	parts << root
	for offset in 0 .. reversed.len {
		parts << reversed[reversed.len - 1 - offset]
	}
	return parts.join(' ')
}

fn validate_structural_type(m &ssa.Module, context string, label string, type_id ssa.TypeID, mut seen []bool) ! {
	mut paths := []StructuralTypePathSegment{}
	mut worklist := [
		StructuralTypeWorkItem{
			type_id:    type_id
			path_index: -1
		},
	]
	for worklist.len > 0 {
		item := worklist.pop()
		type_index := int(item.type_id)
		if type_index < 0 || type_index >= m.type_store.types.len {
			item_label := structural_type_label(label, item.path_index, paths)
			return lowering_error(context,
				'${item_label} ${type_index} is outside 0..${m.type_store.types.len - 1}')
		}
		if seen[type_index] {
			continue
		}
		typ := m.type_store.types[type_index]
		if typ.kind !in [ssa.TypeKind.void_t, .int_t, .float_t, .ptr_t, .array_t, .struct_t, .func_t,
			.label_t, .metadata_t] {
			item_label := structural_type_label(label, item.path_index, paths)
			return lowering_error(context,
				'${item_label} ${type_index} has invalid kind ${int(typ.kind)}')
		}
		seen[type_index] = true
		match typ.kind {
			.ptr_t, .array_t {
				paths << StructuralTypePathSegment{
					parent: item.path_index
					text:   '${type_index} element type'
				}
				worklist << StructuralTypeWorkItem{
					type_id:    typ.elem_type
					path_index: paths.len - 1
				}
			}
			.struct_t {
				for field_offset in 0 .. typ.fields.len {
					field_index := typ.fields.len - 1 - field_offset
					paths << StructuralTypePathSegment{
						parent: item.path_index
						text:   '${type_index} field ${field_index} type'
					}
					worklist << StructuralTypeWorkItem{
						type_id:    typ.fields[field_index]
						path_index: paths.len - 1
					}
				}
			}
			.func_t {
				for parameter_offset in 0 .. typ.params.len {
					parameter_index := typ.params.len - 1 - parameter_offset
					paths << StructuralTypePathSegment{
						parent: item.path_index
						text:   '${type_index} parameter ${parameter_index} type'
					}
					worklist << StructuralTypeWorkItem{
						type_id:    typ.params[parameter_index]
						path_index: paths.len - 1
					}
				}
				paths << StructuralTypePathSegment{
					parent: item.path_index
					text:   '${type_index} return type'
				}
				worklist << StructuralTypeWorkItem{
					type_id:    typ.ret_type
					path_index: paths.len - 1
				}
			}
			else {}
		}
	}
}

fn validate_scalar_integer_type(context string, typ ssa.Type) ! {
	if typ.kind != .int_t {
		return lowering_error(context, 'type must be int_t, got ${int(typ.kind)}')
	}
	match typ.width {
		1 {
			if typ.is_unsigned {
				return lowering_error(context, 'unsigned width 1 is unsupported')
			}
		}
		8, 16, 32, 64 {}
		else {
			return lowering_error(context,
				'integer width must be 1, 8, 16, 32, or 64, got ${typ.width}')
		}
	}
}

fn canonical_scalar_constant_bits(context string, typ ssa.Type, raw_bits u64) !u64 {
	validate_scalar_integer_type(context, typ)!
	if typ.width == 1 {
		if raw_bits > 1 {
			return lowering_error(context,
				'signed width 1 raw bits must be 0 or 1, got ${raw_bits}')
		}
		return raw_bits
	}
	if typ.width == 64 {
		return raw_bits
	}
	mask := (u64(1) << typ.width) - 1
	if raw_bits & ~mask != 0 {
		return lowering_error(context,
			'raw bits 0x${raw_bits:016x} exceed declared width ${typ.width}')
	}
	if typ.is_unsigned {
		return raw_bits
	}
	sign_bit := u64(1) << (typ.width - 1)
	if raw_bits & sign_bit != 0 {
		return raw_bits | ~mask
	}
	return raw_bits
}

fn snapshot_scalar_constant_bindings(m &ssa.Module, bindings []ScalarConstantBinding) !ScalarBindingTable {
	mut index_by_value := []int{len: m.values.len, init: -1}
	mut canonical_bits := []u64{len: bindings.len}
	for binding_index, binding in bindings {
		context := 'scalar binding ${binding_index}'
		value_index := int(binding.value_id)
		if value_index <= 0 || value_index >= m.values.len {
			return lowering_error(context,
				'value reference ${value_index} is outside 1..${m.values.len - 1}')
		}
		if index_by_value[value_index] >= 0 {
			return lowering_error(context,
				'value ${value_index} duplicates scalar binding ${index_by_value[value_index]}')
		}
		value := m.values[value_index]
		if value.id != binding.value_id {
			return lowering_error(context, 'value ${value_index} has id ${value.id}')
		}
		if value.kind != .constant {
			return lowering_error(context, 'value ${value_index} is not a constant')
		}
		type_index := int(binding.type_id)
		if type_index <= 0 || type_index >= m.type_store.types.len {
			return lowering_error(context,
				'type ${type_index} is outside 1..${m.type_store.types.len - 1}')
		}
		if binding.type_id != value.typ {
			return lowering_error(context,
				'type ${binding.type_id} does not match value ${value_index} type ${value.typ}')
		}
		canonical_bits[binding_index] = canonical_scalar_constant_bits(context,
			m.type_store.types[type_index], binding.raw_bits)!
		index_by_value[value_index] = binding_index
	}
	return ScalarBindingTable{
		index_by_value: index_by_value
		canonical_bits: canonical_bits
	}
}

fn validate_and_snapshot_private_data(m &ssa.Module, function_names []string) !PrivateDataPlan {
	mut all_names := map[string]bool{}
	for name in function_names {
		all_names[name] = true
	}
	mut global_names := map[string]bool{}
	for global_index, global in m.globals {
		context := 'global ${global_index}'
		if global.name.len == 0 {
			return lowering_error(context, 'symbol name is empty')
		}
		if global.name.index_u8(u8(0)) >= 0 {
			return lowering_error(context, 'symbol name contains NUL')
		}
		if global_names[global.name] {
			return lowering_error(context, 'symbol `${global.name}` duplicates an earlier global')
		}
		if all_names[global.name] {
			return lowering_error(context, 'symbol `${global.name}` collides with a function')
		}
		global_names[global.name] = true
		all_names[global.name] = true
		if global.linkage != .private {
			return lowering_error(context, 'linkage must be private, got ${int(global.linkage)}')
		}
		type_index := int(global.typ)
		if type_index < 0 || type_index >= m.type_store.types.len {
			return lowering_error(context,
				'type ${type_index} is outside 0..${m.type_store.types.len - 1}')
		}
		typ := m.type_store.types[type_index]
		if typ.kind != .int_t {
			return lowering_error(context, 'type must be int_t, got ${int(typ.kind)}')
		}
		size := private_data_width_size(typ.width) or { return lowering_error(context, err.msg()) }
		if global.is_constant {
			return lowering_error(context, 'constant private data is unsupported')
		}
		if global.initial_data.len != 0 {
			return lowering_error(context,
				'initial_data is unsupported, got ${global.initial_data.len} bytes')
		}
		if global.alignment != 0 && global.alignment != int(size) {
			return lowering_error(context,
				'alignment must be 0 or natural alignment ${size}, got ${global.alignment}')
		}
		if !private_data_value_in_range(typ.width, typ.is_unsigned, global.initial_value) {
			return lowering_error(context,
				'value ${global.initial_value} is outside ${typ.width}-bit range')
		}
	}

	mut owners := []int{len: m.globals.len, init: -1}
	for value_index, value in m.values {
		if value.kind != .global {
			continue
		}
		context := 'global value ${value_index}'
		if value_index == 0 {
			return lowering_error(context, 'reserved value zero must not be a global')
		}
		if value.id != ssa.ValueID(value_index) {
			return lowering_error(context,
				'id ${value.id} does not match array index ${value_index}')
		}
		global_index := value.index
		if global_index < 0 || global_index >= m.globals.len {
			return lowering_error(context,
				'global index ${global_index} is outside 0..${m.globals.len - 1}')
		}
		if owners[global_index] >= 0 {
			return lowering_error(context,
				'global ${global_index} already has value ${owners[global_index]}')
		}
		global := m.globals[global_index]
		if value.name != global.name {
			return lowering_error(context,
				'name `${value.name}` does not match global `${global.name}`')
		}
		value_type_index := int(value.typ)
		if value_type_index < 0 || value_type_index >= m.type_store.types.len {
			return lowering_error(context,
				'type ${value_type_index} is outside 0..${m.type_store.types.len - 1}')
		}
		value_type := m.type_store.types[value_type_index]
		if value_type.kind != .ptr_t {
			return lowering_error(context, 'type must be a pointer, got ${int(value_type.kind)}')
		}
		if value_type.elem_type != global.typ {
			return lowering_error(context,
				'pointer element type ${value_type.elem_type} does not match global type ${global.typ}')
		}
		owners[global_index] = value_index
	}
	for global_index, owner in owners {
		if owner < 0 {
			return lowering_error('global ${global_index}', 'matching global value is missing')
		}
	}

	mut definitions := []PrivateDataDefinition{cap: m.globals.len}
	for global in m.globals {
		typ := m.type_store.types[int(global.typ)]
		size := private_data_width_size(typ.width) or {
			return lowering_error('private data preflight', err.msg())
		}
		definitions << PrivateDataDefinition{
			name:        global.name.clone()
			value:       global.initial_value
			width:       typ.width
			is_unsigned: typ.is_unsigned
			alignment:   size
		}
	}
	plan := private_data_preflight(definitions, function_names) or {
		return lowering_error('private data preflight', err.msg())
	}
	return plan
}

fn validate_scalar_imm64_target_parameter(m &ssa.Module, instruction_context string, target ssa.Function, expected_type ssa.TypeID) ! {
	if target.params.len != 1 {
		return lowering_error(instruction_context,
			'scalar immediate call target `${target.name}` must have exactly one parameter, got ${target.params.len}')
	}
	expected_type_index := int(expected_type)
	expected_typ := m.type_store.types[expected_type_index]
	if expected_typ.kind != .int_t || expected_typ.width != 64 {
		return lowering_error(instruction_context,
			'scalar immediate call type must be 64-bit int_t, got kind ${int(expected_typ.kind)} width ${expected_typ.width}')
	}
	parameter_id := int(target.params[0])
	if parameter_id <= 0 || parameter_id >= m.values.len {
		return lowering_error(instruction_context,
			'target parameter ${parameter_id} is outside 1..${m.values.len - 1}')
	}
	parameter := m.values[parameter_id]
	if parameter.id != ssa.ValueID(parameter_id) {
		return lowering_error(instruction_context,
			'target parameter value ${parameter_id} has id ${parameter.id}')
	}
	if parameter.kind != .argument {
		return lowering_error(instruction_context,
			'target parameter value ${parameter_id} is not an argument')
	}
	if parameter.index != 0 {
		return lowering_error(instruction_context,
			'target parameter value ${parameter_id} index ${parameter.index} must be 0')
	}
	if parameter.typ != expected_type {
		return lowering_error(instruction_context,
			'target parameter type ${parameter.typ} does not match scalar call type ${expected_type}')
	}
}

fn snapshot_scalar_imm64_argument(m &ssa.Module, instruction_context string, instruction ssa.Instruction, expected_type ssa.TypeID, binding_table ScalarBindingTable) !LoweredScalarArgumentSnapshot {
	argument_id := int(instruction.operands[1])
	if argument_id <= 0 || argument_id >= m.values.len {
		return lowering_error(instruction_context,
			'scalar argument ${argument_id} is outside 1..${m.values.len - 1}')
	}
	argument := m.values[argument_id]
	if argument.id != ssa.ValueID(argument_id) {
		return lowering_error(instruction_context,
			'scalar argument value ${argument_id} has id ${argument.id}')
	}
	if argument.kind != .constant {
		return lowering_error(instruction_context,
			'scalar argument value ${argument_id} is not a constant')
	}
	if argument.typ != expected_type {
		return lowering_error(instruction_context,
			'scalar argument type ${argument.typ} does not match scalar call type ${expected_type}')
	}
	binding_index := binding_table.index_by_value[argument_id]
	if binding_index < 0 {
		return lowering_error(instruction_context,
			'scalar binding for call argument ${argument_id} is missing')
	}
	return LoweredScalarArgumentSnapshot{
		bits:          binding_table.canonical_bits[binding_index]
		binding_index: binding_index
	}
}

fn validate_c_external_target(m &ssa.Module, instruction_context string, function_ref ssa.Value, target_index int, expected_type ssa.TypeID, scalar_result bool, argument_mode LoweredCallArgumentMode) !string {
	target := m.funcs[target_index]
	if !target.is_c_extern {
		return lowering_error(instruction_context,
			'function target ${target_index} `${target.name}` is not a callable C external declaration')
	}
	if target.linkage != .external {
		return lowering_error(instruction_context,
			'C external target `${target.name}` linkage must be external')
	}
	if target.call_conv != .c_decl {
		return lowering_error(instruction_context,
			'C external target `${target.name}` calling convention must be c_decl')
	}
	match argument_mode {
		.none {
			if target.params.len != 0 {
				return lowering_error(instruction_context,
					'C external target `${target.name}` parameters are unsupported, got ${target.params.len}')
			}
		}
		.scalar_imm64 {
			if target.params.len == 0 {
				return lowering_error(instruction_context,
					'direct external scalar call must have one function reference operand, got 2')
			}
			validate_scalar_imm64_target_parameter(m, instruction_context, target, expected_type)!
		}
	}
	if scalar_result {
		if target.typ != expected_type {
			return lowering_error(instruction_context,
				'C external target `${target.name}` return type ${target.typ} does not match scalar call type ${expected_type}')
		}
	} else if target.typ != ssa.TypeID(0) {
		return lowering_error(instruction_context,
			'C external target `${target.name}` return type must be canonical void type 0, got ${target.typ}')
	}
	if !target.name.starts_with('C.') || target.name.len == 2 {
		return lowering_error(instruction_context,
			'C external target `${target.name}` must have a nonempty `C.`-prefixed name')
	}
	semantic_name := target.name[2..]
	if semantic_name.index_u8(u8(0)) >= 0 {
		return lowering_error(instruction_context,
			'C external target `${target.name}` semantic name contains NUL')
	}
	if function_ref.name != semantic_name {
		return lowering_error(instruction_context,
			'function reference name `${function_ref.name}` does not match C external semantic name `${semantic_name}`')
	}
	return semantic_name
}

fn intern_c_external_target(instruction_context string, target_index int, semantic_name string, argument_mode LoweredCallArgumentMode, argument_bits u64, mut original_to_external []int, mut externals []ReferencedExternal, mut emitted_names map[string]bool) !LoweredCallTarget {
	existing_external_index := original_to_external[target_index]
	if existing_external_index >= 0 {
		return LoweredCallTarget{
			kind:          .external
			index:         u32(existing_external_index)
			argument_mode: argument_mode
			argument_bits: argument_bits
		}
	}
	if emitted_names[semantic_name] {
		return lowering_error(instruction_context,
			'C external semantic name `${semantic_name}` collides with an emitted symbol')
	}
	if u64(externals.len) >= u64(max_u32) {
		return lowering_error(instruction_context, 'too many referenced C externals')
	}
	external_id := ExternalID(externals.len)
	externals << ReferencedExternal{
		name: semantic_name.clone()
	}
	original_to_external[target_index] = int(external_id)
	emitted_names[semantic_name] = true
	return LoweredCallTarget{
		kind:          .external
		index:         u32(external_id)
		argument_mode: argument_mode
		argument_bits: argument_bits
	}
}

fn snapshot_direct_void_call(m &ssa.Module, instruction_context string, instruction ssa.Instruction, original_to_dense []int, mut original_to_external []int, mut externals []ReferencedExternal, mut emitted_names map[string]bool, allow_scalar_returns bool) !LoweredCallTarget {
	if instruction.operands.len != 1 {
		return lowering_error(instruction_context,
			'direct zero-argument call must have one function reference operand, got ${instruction.operands.len}')
	}

	function_ref_id := int(instruction.operands[0])
	if function_ref_id <= 0 || function_ref_id >= m.values.len {
		return lowering_error(instruction_context,
			'function reference ${function_ref_id} is outside 1..${m.values.len - 1}')
	}
	function_ref := m.values[function_ref_id]
	if function_ref.id != ssa.ValueID(function_ref_id) {
		return lowering_error(instruction_context,
			'function reference value ${function_ref_id} has id ${function_ref.id}')
	}
	if function_ref.kind != .func_ref {
		return lowering_error(instruction_context,
			'call operand ${function_ref_id} is not a function reference')
	}
	target_index := function_ref.index
	if target_index < 0 || target_index >= m.funcs.len {
		return lowering_error(instruction_context,
			'function target ${target_index} is outside 0..${m.funcs.len - 1}')
	}
	target := m.funcs[target_index]
	dense_target_index := original_to_dense[target_index]
	if allow_scalar_returns && dense_target_index >= 0 && target.typ != ssa.TypeID(0) {
		return lowering_error(instruction_context,
			'void caller cannot call scalar-returning definition `${target.name}`')
	}
	if function_ref.typ != ssa.TypeID(0) {
		return lowering_error(instruction_context,
			'function reference type must be canonical void type 0, got ${function_ref.typ}')
	}
	if dense_target_index >= 0 {
		if function_ref.name != target.name {
			return lowering_error(instruction_context,
				'function reference name `${function_ref.name}` does not match target `${target.name}`')
		}
		if u64(dense_target_index) >= u64(max_u32) {
			return lowering_error(instruction_context,
				'function target ${dense_target_index} exceeds the lowering index range')
		}
		return LoweredCallTarget{
			kind:  .definition
			index: u32(dense_target_index)
		}
	}
	semantic_name := validate_c_external_target(m, instruction_context, function_ref, target_index,
		ssa.TypeID(0), false, .none)!
	return intern_c_external_target(instruction_context, target_index, semantic_name, .none,
		0, mut original_to_external, mut externals, mut emitted_names)
}

fn snapshot_direct_scalar_call_result(m &ssa.Module, instruction_context string, instruction ssa.Instruction, expected_type ssa.TypeID, original_to_dense []int, binding_table ScalarBindingTable, mut original_to_external []int, mut externals []ReferencedExternal, mut emitted_names map[string]bool) !LoweredScalarCallSnapshot {
	if instruction.operands.len < 1 || instruction.operands.len > 2 {
		return lowering_error(instruction_context,
			'direct scalar call must have one function reference operand, got ${instruction.operands.len}')
	}
	function_ref_id := int(instruction.operands[0])
	if function_ref_id <= 0 || function_ref_id >= m.values.len {
		return lowering_error(instruction_context,
			'function reference ${function_ref_id} is outside 1..${m.values.len - 1}')
	}
	function_ref := m.values[function_ref_id]
	if function_ref.id != ssa.ValueID(function_ref_id) {
		return lowering_error(instruction_context,
			'function reference value ${function_ref_id} has id ${function_ref.id}')
	}
	if function_ref.kind != .func_ref {
		return lowering_error(instruction_context,
			'call operand ${function_ref_id} is not a function reference')
	}
	if function_ref.typ != expected_type {
		return lowering_error(instruction_context,
			'function reference type ${function_ref.typ} does not match scalar return type ${expected_type}')
	}
	target_index := function_ref.index
	if target_index < 0 || target_index >= m.funcs.len {
		return lowering_error(instruction_context,
			'function target ${target_index} is outside 0..${m.funcs.len - 1}')
	}
	target := m.funcs[target_index]
	dense_target_index := original_to_dense[target_index]
	if dense_target_index < 0 {
		if !target.is_c_extern {
			return lowering_error(instruction_context,
				'scalar call target ${target_index} `${target.name}` is not a defined internal function')
		}
		expected_typ := m.type_store.types[int(expected_type)]
		if expected_typ.kind != .int_t || expected_typ.width != 64 {
			return lowering_error(instruction_context,
				'external scalar call type must be 64-bit int_t, got kind ${int(expected_typ.kind)} width ${expected_typ.width}')
		}
		argument_mode := if instruction.operands.len == 1 {
			LoweredCallArgumentMode.none
		} else {
			LoweredCallArgumentMode.scalar_imm64
		}
		semantic_name := validate_c_external_target(m, instruction_context, function_ref,
			target_index, expected_type, true, argument_mode)!
		if argument_mode == .none {
			return LoweredScalarCallSnapshot{
				target: intern_c_external_target(instruction_context, target_index, semantic_name,
					.none, 0, mut original_to_external, mut externals, mut emitted_names)!
			}
		}
		argument_snapshot := snapshot_scalar_imm64_argument(m, instruction_context, instruction,
			expected_type, binding_table)!
		return LoweredScalarCallSnapshot{
			target:        intern_c_external_target(instruction_context, target_index, semantic_name,
				.scalar_imm64, argument_snapshot.bits, mut original_to_external, mut externals,
				mut emitted_names)!
			binding_index: argument_snapshot.binding_index
		}
	}
	if target.typ != expected_type {
		return lowering_error(instruction_context,
			'scalar call target `${target.name}` return type ${target.typ} does not match caller type ${expected_type}')
	}
	if function_ref.name != target.name {
		return lowering_error(instruction_context,
			'function reference name `${function_ref.name}` does not match target `${target.name}`')
	}
	if u64(dense_target_index) >= u64(max_u32) {
		return lowering_error(instruction_context,
			'function target ${dense_target_index} exceeds the lowering index range')
	}
	if instruction.operands.len == 1 {
		return LoweredScalarCallSnapshot{
			target: LoweredCallTarget{
				kind:  .definition
				index: u32(dense_target_index)
			}
		}
	}

	validate_scalar_imm64_target_parameter(m, instruction_context, target, expected_type)!
	argument_snapshot := snapshot_scalar_imm64_argument(m, instruction_context, instruction,
		expected_type, binding_table)!
	return LoweredScalarCallSnapshot{
		target:        LoweredCallTarget{
			kind:          .definition
			index:         u32(dense_target_index)
			argument_mode: .scalar_imm64
			argument_bits: argument_snapshot.bits
		}
		binding_index: argument_snapshot.binding_index
	}
}

fn validate_and_snapshot(profile TargetProfile, m &ssa.Module) !LoweringPlan {
	return validate_and_snapshot_internal(profile, m, [], false)
}

fn validate_and_snapshot_with_scalar_constants(profile TargetProfile, m &ssa.Module, bindings []ScalarConstantBinding) !LoweringPlan {
	return validate_and_snapshot_internal(profile, m, bindings, true)
}

fn validate_and_snapshot_internal(profile TargetProfile, m &ssa.Module, bindings []ScalarConstantBinding, allow_scalar_returns bool) !LoweringPlan {
	if profile != .linux_x86_64_sysv_elf && profile != .macos_x86_64_sysv_macho
		&& profile != .windows_x86_64_microsoft_abi_coff {
		return lowering_error('target', 'unsupported target profile')
	}

	if m.target.ptr_size != 8 {
		return lowering_error('target', 'pointer size must be 8 bytes, got ${m.target.ptr_size}')
	}
	if !m.target.endian_little {
		return lowering_error('target', 'little-endian target data is required')
	}
	if m.type_store.types.len == 0 {
		return lowering_error('type store', 'canonical void type 0 is missing')
	}
	if m.type_store.types[0].kind != .void_t {
		return lowering_error('type store', 'type 0 is not void')
	}
	binding_table := snapshot_scalar_constant_bindings(m, bindings)!
	mut binding_use_counts := []int{len: bindings.len}
	mut pending_scalar_call_binding_uses := []int{}

	mut names := map[string]int{}
	mut active_parameter_values := map[int]string{}
	mut seen_types := []bool{len: m.type_store.types.len}
	mut original_to_dense := []int{len: m.funcs.len, init: -1}
	mut definition_indices := []int{cap: m.funcs.len}
	mut function_names := []string{cap: m.funcs.len}
	mut definition_names := []string{cap: m.funcs.len}
	for function_index, ssa_function in m.funcs {
		function_context := 'function ${function_index}'
		if ssa_function.id != function_index {
			return lowering_error(function_context,
				'id ${ssa_function.id} does not match array index ${function_index}')
		}
		if ssa_function.name.len == 0 {
			return lowering_error(function_context, 'symbol name is empty')
		}
		if ssa_function.name.index_u8(u8(0)) >= 0 {
			return lowering_error(function_context, 'symbol name contains NUL')
		}
		if previous_index := names[ssa_function.name] {
			return lowering_error(function_context,
				'symbol `${ssa_function.name}` duplicates function ${previous_index}')
		}
		names[ssa_function.name] = function_index
		function_names << ssa_function.name.clone()
		if ssa_function.linkage !in [ssa.Linkage.external, .private, .internal] {
			return lowering_error(function_context,
				'linkage has invalid value ${int(ssa_function.linkage)}')
		}
		if ssa_function.call_conv !in [ssa.CallConv.c_decl, .fast_call, .wasm_std] {
			return lowering_error(function_context,
				'calling convention has invalid value ${int(ssa_function.call_conv)}')
		}
		validate_structural_type(m, function_context, 'return type', ssa_function.typ, mut
			seen_types)!
		for parameter_index, raw_parameter_id in ssa_function.params {
			parameter_id := int(raw_parameter_id)
			parameter_context := '${function_context} parameter ${parameter_index}'
			if parameter_id <= 0 || parameter_id >= m.values.len {
				return lowering_error(parameter_context,
					'value reference ${parameter_id} is outside 1..${m.values.len - 1}')
			}
			if previous_owner := active_parameter_values[parameter_id] {
				return lowering_error(parameter_context,
					'value ${parameter_id} is already owned by ${previous_owner}')
			}
			active_parameter_values[parameter_id] = parameter_context
			parameter := m.values[parameter_id]
			if parameter.id != ssa.ValueID(parameter_id) {
				return lowering_error(parameter_context,
					'value ${parameter_id} has id ${parameter.id}')
			}
			if parameter.kind !in [ssa.ValueKind.unknown, .constant, .argument, .global, .instruction,
				.basic_block, .string_literal, .c_string_literal, .func_ref] {
				return lowering_error(parameter_context,
					'value ${parameter_id} has invalid kind ${int(parameter.kind)}')
			}
			if parameter.kind != .argument {
				return lowering_error(parameter_context, 'value ${parameter_id} is not an argument')
			}
			validate_structural_type(m, parameter_context, 'value type', parameter.typ, mut
				seen_types)!
			if parameter.index != parameter_index {
				return lowering_error(parameter_context,
					'value ${parameter_id} index ${parameter.index} does not match parameter position ${parameter_index}')
			}
		}

		is_declaration := ssa_function.is_prototype || ssa_function.is_c_extern
		if is_declaration {
			if ssa_function.blocks.len != 0 {
				return lowering_error(function_context,
					'declaration must not have body blocks, got ${ssa_function.blocks.len}')
			}
			continue
		}
		if ssa_function.blocks.len == 0 {
			return lowering_error(function_context, 'body block is missing')
		}
		original_to_dense[function_index] = definition_indices.len
		definition_indices << function_index
		definition_names << ssa_function.name.clone()
	}
	private_data := validate_and_snapshot_private_data(m, function_names)!
	mut emitted_names := map[string]bool{}
	for name in definition_names {
		emitted_names[name] = true
	}
	for symbol in private_data.symbols {
		emitted_names[symbol.name] = true
	}
	mut original_to_external := []int{len: m.funcs.len, init: -1}
	mut externals := []ReferencedExternal{}
	mut active_blocks := map[int]string{}
	mut active_instruction_values := map[int]string{}
	mut active_instructions := map[int]string{}
	mut functions := []LoweredFunction{cap: definition_indices.len}
	for function_index in definition_indices {
		ssa_function := m.funcs[function_index]
		function_context := 'function ${function_index}'
		if ssa_function.linkage != .external {
			return lowering_error(function_context, 'linkage must be external')
		}
		if ssa_function.call_conv != .c_decl {
			return lowering_error(function_context, 'calling convention must be c_decl')
		}
		is_scalar_return := ssa_function.typ != ssa.TypeID(0)
		if is_scalar_return {
			if !allow_scalar_returns {
				return lowering_error(function_context,
					'return type must be canonical void type 0, got ${ssa_function.typ}')
			}
			return_type_index := int(ssa_function.typ)
			return_type := m.type_store.types[return_type_index]
			validate_scalar_integer_type('${function_context} return type', return_type)!
			if ssa_function.params.len > 1 {
				return lowering_error(function_context,
					'scalar definitions support at most one parameter, got ${ssa_function.params.len}')
			}
			if ssa_function.params.len == 1 {
				if return_type.width != 64 {
					return lowering_error(function_context,
						'scalar parameter definition requires integer width 64, got ${return_type.width}')
				}
				parameter_id := int(ssa_function.params[0])
				parameter := m.values[parameter_id]
				if parameter.typ != ssa_function.typ {
					return lowering_error(function_context,
						'parameter type ${parameter.typ} does not match function return type ${ssa_function.typ}')
				}
			}
			if ssa_function.blocks.len != 1 {
				return lowering_error(function_context,
					'scalar-returning definition must contain exactly one block, got ${ssa_function.blocks.len}')
			}
		} else if ssa_function.params.len != 0 {
			return lowering_error(function_context,
				'parameters are unsupported, got ${ssa_function.params.len}')
		}
		mut block_to_local := map[int]int{}
		for local_block_index, raw_block_id in ssa_function.blocks {
			block_index := int(raw_block_id)
			if block_index < 0 || block_index >= m.blocks.len {
				return lowering_error(function_context,
					'block reference ${block_index} is outside 0..${m.blocks.len - 1}')
			}
			if previous_owner := active_blocks[block_index] {
				return lowering_error(function_context,
					'block ${block_index} is already owned by ${previous_owner}')
			}
			active_blocks[block_index] = function_context
			block_to_local[block_index] = local_block_index
			block := m.blocks[block_index]
			block_context := '${function_context} block ${block_index}'
			if block.id != ssa.BlockID(block_index) {
				return lowering_error(block_context,
					'id ${block.id} does not match array index ${block_index}')
			}
			if block.val_id != ssa.ValueID(0) {
				return lowering_error(block_context,
					'value id must be 0 in the raw-block-id model, got ${block.val_id}')
			}
			if block.parent != function_index {
				return lowering_error(block_context,
					'parent ${block.parent} does not match function ${function_index}')
			}
			if block.instrs.len == 0 {
				if is_scalar_return {
					return lowering_error(block_context,
						'scalar block must contain exactly one RET instruction, got 0')
				}
				return lowering_error(block_context,
					'body must end with an unconditional jmp or operandless ret')
			}
		}

		mut lowered_blocks := []LoweredBlock{cap: ssa_function.blocks.len}
		mut ret_count := 0
		for raw_block_id in ssa_function.blocks {
			block_index := int(raw_block_id)
			block := m.blocks[block_index]
			block_context := '${function_context} block ${block_index}'
			mut calls := []LoweredCallTarget{}
			mut terminator := LoweredBlockTerminator.ret
			mut jump_target := -1
			mut return_value := LoweredReturnValue{}
			for instruction_position, raw_value_id in block.instrs {
				value_id := int(raw_value_id)
				instruction_context := '${block_context} instruction ${instruction_position}'
				if value_id <= 0 || value_id >= m.values.len {
					return lowering_error(instruction_context,
						'value reference ${value_id} is outside 1..${m.values.len - 1}')
				}
				if previous_owner := active_instruction_values[value_id] {
					return lowering_error(instruction_context,
						'instruction value ${value_id} is already owned by ${previous_owner}')
				}
				active_instruction_values[value_id] = instruction_context
				value := m.values[value_id]
				if value.id != ssa.ValueID(value_id) {
					return lowering_error(instruction_context,
						'value ${value_id} has id ${value.id}')
				}
				if value.kind != .instruction {
					return lowering_error(instruction_context,
						'value ${value_id} is not an instruction')
				}
				instruction_index := value.index
				if instruction_index < 0 || instruction_index >= m.instrs.len {
					return lowering_error(instruction_context,
						'instruction index ${instruction_index} is outside 0..${m.instrs.len - 1}')
				}
				if previous_owner := active_instructions[instruction_index] {
					return lowering_error(instruction_context,
						'instruction index ${instruction_index} is already owned by ${previous_owner}')
				}
				active_instructions[instruction_index] = instruction_context
				instruction := m.instrs[instruction_index]
				if instruction.block != ssa.BlockID(block_index) {
					return lowering_error(instruction_context,
						'instruction block ${instruction.block} does not match ${block_index}')
				}
				if is_scalar_return {
					if ssa_function.params.len == 1 {
						if block.instrs.len != 1 {
							return lowering_error(block_context,
								'scalar parameter definition must contain exactly RET parameter, got ${block.instrs.len} instructions')
						}
						if value.typ != ssa.TypeID(0) {
							return lowering_error(instruction_context,
								'value ${value_id} type must be canonical void type 0, got ${value.typ}')
						}
						if instruction.typ != ssa.TypeID(0) {
							return lowering_error(instruction_context,
								'instruction type must be canonical void type 0, got ${instruction.typ}')
						}
						if instruction.op != .ret {
							return lowering_error(instruction_context,
								'scalar parameter definition must contain RET parameter, got ${instruction.op}')
						}
						if instruction.operands.len != 1 {
							return lowering_error(instruction_context,
								'scalar parameter ret must have exactly one parameter operand, got ${instruction.operands.len}')
						}
						return_id := int(instruction.operands[0])
						if return_id <= 0 || return_id >= m.values.len {
							return lowering_error(instruction_context,
								'parameter return value ${return_id} is outside 1..${m.values.len - 1}')
						}
						return_parameter := m.values[return_id]
						if return_parameter.id != ssa.ValueID(return_id) {
							return lowering_error(instruction_context,
								'parameter return value ${return_id} has id ${return_parameter.id}')
						}
						if return_parameter.kind != .argument {
							return lowering_error(instruction_context,
								'parameter return value ${return_id} is not an argument')
						}
						if return_parameter.typ != ssa_function.typ {
							return lowering_error(instruction_context,
								'parameter return type ${return_parameter.typ} does not match function return type ${ssa_function.typ}')
						}
						if instruction.operands[0] != ssa_function.params[0] {
							return lowering_error(instruction_context,
								'scalar parameter RET operand ${instruction.operands[0]} is not sole parameter ${ssa_function.params[0]}')
						}
						return_value = LoweredReturnValue{
							kind: .scalar_parameter
						}
						ret_count++
						continue
					}
					if block.instrs.len == 1 && instruction.op == .call
						&& value.typ == ssa_function.typ && instruction.typ == ssa_function.typ {
						return lowering_error(block_context,
							'scalar caller must contain exactly CALL-result then RET, got 1 instructions')
					}
					if block.instrs.len == 1 {
						if value.typ != ssa.TypeID(0) {
							return lowering_error(instruction_context,
								'value ${value_id} type must be canonical void type 0, got ${value.typ}')
						}
						if instruction.typ != ssa.TypeID(0) {
							return lowering_error(instruction_context,
								'instruction type must be canonical void type 0, got ${instruction.typ}')
						}
						if instruction.op != .ret {
							return lowering_error(instruction_context,
								'scalar leaf must contain RET constant, got ${instruction.op}')
						}
						if instruction.operands.len != 1 {
							return lowering_error(instruction_context,
								'scalar ret must have exactly one constant operand, got ${instruction.operands.len}')
						}
						constant_index := int(instruction.operands[0])
						if constant_index <= 0 || constant_index >= m.values.len {
							return lowering_error(instruction_context,
								'return value ${constant_index} is outside 1..${m.values.len - 1}')
						}
						constant := m.values[constant_index]
						if constant.id != ssa.ValueID(constant_index) {
							return lowering_error(instruction_context,
								'return value ${constant_index} has id ${constant.id}')
						}
						if constant.kind != .constant {
							return lowering_error(instruction_context,
								'return value ${constant_index} is not a constant')
						}
						if constant.typ != ssa_function.typ {
							return lowering_error(instruction_context,
								'return value type ${constant.typ} does not match function return type ${ssa_function.typ}')
						}
						binding_index := binding_table.index_by_value[constant_index]
						if binding_index < 0 {
							return lowering_error(instruction_context,
								'scalar binding for return value ${constant_index} is missing')
						}
						binding_use_counts[binding_index]++
						return_value = LoweredReturnValue{
							kind: .scalar_constant
							bits: binding_table.canonical_bits[binding_index]
						}
						ret_count++
						continue
					}
					if block.instrs.len != 2 {
						return lowering_error(block_context,
							'scalar caller must contain exactly CALL-result then RET, got ${block.instrs.len} instructions')
					}
					if instruction_position == 0 {
						if instruction.op != .call {
							return lowering_error(instruction_context,
								'scalar caller instruction 0 must be CALL, got ${instruction.op}')
						}
						if value.typ != ssa_function.typ {
							return lowering_error(instruction_context,
								'CALL result value type ${value.typ} does not match function return type ${ssa_function.typ}')
						}
						if instruction.typ != ssa_function.typ {
							return lowering_error(instruction_context,
								'CALL result instruction type ${instruction.typ} does not match function return type ${ssa_function.typ}')
						}
						call_snapshot := snapshot_direct_scalar_call_result(m, instruction_context,
							instruction, ssa_function.typ, original_to_dense, binding_table, mut
							original_to_external, mut externals, mut emitted_names)!
						calls << call_snapshot.target
						if call_snapshot.binding_index >= 0 {
							pending_scalar_call_binding_uses << call_snapshot.binding_index
						}
						continue
					}
					if value.typ != ssa.TypeID(0) {
						return lowering_error(instruction_context,
							'value ${value_id} type must be canonical void type 0, got ${value.typ}')
					}
					if instruction.typ != ssa.TypeID(0) {
						return lowering_error(instruction_context,
							'instruction type must be canonical void type 0, got ${instruction.typ}')
					}
					if instruction.op != .ret {
						return lowering_error(instruction_context,
							'scalar caller instruction 1 must be RET, got ${instruction.op}')
					}
					if instruction.operands.len != 1 {
						return lowering_error(instruction_context,
							'scalar caller RET must have exactly one CALL result operand, got ${instruction.operands.len}')
					}
					if instruction.operands[0] != block.instrs[0] {
						return lowering_error(instruction_context,
							'scalar caller RET operand ${instruction.operands[0]} is not CALL result ${block.instrs[0]}')
					}
					return_value = LoweredReturnValue{
						kind: .scalar_call_result
					}
					ret_count++
					continue
				}
				if value.typ != ssa.TypeID(0) {
					return lowering_error(instruction_context,
						'value ${value_id} type must be canonical void type 0, got ${value.typ}')
				}
				if instruction.typ != ssa.TypeID(0) {
					return lowering_error(instruction_context,
						'instruction type must be canonical void type 0, got ${instruction.typ}')
				}

				is_last := instruction_position == block.instrs.len - 1
				if is_last {
					match instruction.op {
						.ret {
							if instruction.operands.len != 0 {
								return lowering_error(instruction_context,
									'ret must be operandless, got ${instruction.operands.len} operands')
							}
							ret_count++
						}
						.jmp {
							if instruction.operands.len != 1 {
								return lowering_error(instruction_context,
									'unconditional jmp must have one block operand, got ${instruction.operands.len}')
							}
							target_block_index := int(instruction.operands[0])
							if target_block_index < 0 || target_block_index >= m.blocks.len {
								return lowering_error(instruction_context,
									'jump target block ${target_block_index} is outside 0..${m.blocks.len - 1}')
							}
							jump_target = block_to_local[target_block_index] or {
								return lowering_error(instruction_context,
									'jump target block ${target_block_index} does not belong to function ${function_index}')
							}
							terminator = .jmp
						}
						else {
							return lowering_error(instruction_context,
								'terminator must be unconditional jmp or operandless ret, got ${instruction.op}')
						}
					}

					continue
				}
				if instruction.op != .call {
					return lowering_error(instruction_context,
						'only direct zero-argument calls may precede block terminator, got ${instruction.op}')
				}
				calls << snapshot_direct_void_call(m, instruction_context, instruction,
					original_to_dense, mut original_to_external, mut externals, mut emitted_names,
					allow_scalar_returns)!
			}
			lowered_blocks << LoweredBlock{
				calls:        calls
				terminator:   terminator
				jump_target:  jump_target
				return_value: return_value
			}
		}
		if ret_count != 1 {
			if is_scalar_return {
				return lowering_error(function_context,
					'exactly one scalar ret is required, got ${ret_count}')
			}
			return lowering_error(function_context,
				'exactly one operandless ret block is required, got ${ret_count}')
		}
		if lowered_blocks.len == 1 {
			functions << LoweredFunction{
				name:         ssa_function.name.clone()
				calls:        lowered_blocks[0].calls.clone()
				return_value: lowered_blocks[0].return_value
			}
		} else {
			functions << LoweredFunction{
				name:   ssa_function.name.clone()
				blocks: lowered_blocks
			}
		}
	}
	for binding_index in pending_scalar_call_binding_uses {
		binding_use_counts[binding_index]++
	}
	for binding_index, use_count in binding_use_counts {
		if use_count == 0 {
			return lowering_error('scalar binding ${binding_index}',
				'value ${bindings[binding_index].value_id} is not consumed by any approved scalar return or call argument')
		}
	}
	return LoweringPlan{
		profile:      profile
		functions:    functions
		externals:    externals
		private_data: private_data
	}
}
