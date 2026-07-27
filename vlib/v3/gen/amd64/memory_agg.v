module amd64

import v3.ssa

const memory_agg_max_scalar_layouts = 64
const memory_agg_max_static_locals = 1024
const memory_agg_max_scalar_constants = 4096
const memory_agg_max_accesses = 4096
const memory_agg_max_blocks = 4096
const memory_agg_max_active_instructions = 65536
const memory_agg_max_pointer_definitions = 8192
const memory_agg_max_use_edges = 262144
const memory_agg_max_module_values = 131072
const memory_agg_max_module_functions = 4096
const memory_agg_max_module_instructions = 65536
const memory_agg_max_provenance_depth = 64
const memory_agg_max_aggregate_layouts = 64
const memory_agg_max_aggregate_fields_per_layout = 64
const memory_agg_max_aggregate_fields = 1024
const memory_agg_max_padding_ranges_per_layout = 65
const memory_agg_max_padding_ranges = 1088
const memory_agg_max_aggregate_object_bytes = u64(65536)
const memory_agg_max_aggregate_facts = 4096
const memory_agg_max_aggregate_temps = 1024
const memory_agg_max_aggregate_actions = 65536
const memory_agg_temp_id_base = u64(0x80000000)
const memory_agg_max_requested_bytes = u64(0x7ffffff8)

pub enum MemoryAggSsaForm {
	unknown
	final_static
}

pub enum MemoryAggNativeAuthority {
	unknown
	native_plain
	external_c
	packed
	bitfield
	union_
}

pub enum MemoryAggAllocaForm {
	one
	constant_count
}

pub enum MemoryAggAccessSemantics {
	unknown
	nonvolatile
	volatile
	atomic
}

pub enum MemoryAggScalarAccessKind {
	load
	store
}

pub enum MemoryAggPointerOrigin {
	fixed_alloca
	byte_delta
	bitcast
	aggregate_storage
	aggregate_field
}

pub enum MemoryAggAggregateSlotRole {
	fixed_alloca
	aggregate_temp
}

pub enum MemoryAggTempPurpose {
	explicit_temp
	construct_result
	load_result
	insert_result
}

pub enum MemoryAggConstructPadding {
	preserve_unwritten
	zero
}

pub enum MemoryAggCopyDirection {
	low_to_high
	high_to_low
}

pub enum MemoryAggAggregateActionKind {
	zero
	copy
	scalar_read
	scalar_write
}

pub struct MemoryAggInstructionAnchor {
pub:
	function_index       int
	block_id             ssa.BlockID
	block_ordinal        u32
	instruction_value_id ssa.ValueID
	instruction_index    int
	instruction_ordinal  u32
}

pub struct MemoryAggScalarLayoutBinding {
pub:
	profile              TargetProfile
	type_id              ssa.TypeID
	authority            MemoryAggNativeAuthority
	semantic_width_bits  int
	semantic_is_unsigned bool
	storage_width_bytes  u8
	alignment_bytes      u8
}

pub struct MemoryAggStaticLocalBinding {
pub:
	profile        TargetProfile
	anchor         MemoryAggInstructionAnchor
	authority      MemoryAggNativeAuthority
	element_type   ssa.TypeID
	form           MemoryAggAllocaForm
	count_value_id ssa.ValueID
}

pub struct MemoryAggAccessBinding {
pub:
	profile              TargetProfile
	anchor               MemoryAggInstructionAnchor
	semantics            MemoryAggAccessSemantics
	kind                 MemoryAggScalarAccessKind
	pointer_value_id     ssa.ValueID
	scalar_value_id      ssa.ValueID
	scalar_type          ssa.TypeID
}

pub struct MemoryAggByteRange {
pub:
	offset_bytes u64
	size_bytes   u64
}

pub struct MemoryAggAggregateFieldLayout {
pub:
	index           u16
	type_id         ssa.TypeID
	offset_bytes    u64
	size_bytes      u64
	alignment_bytes u64
}

pub struct MemoryAggAggregateLayoutBinding {
pub:
	profile         TargetProfile
	authority       MemoryAggNativeAuthority
	type_id         ssa.TypeID
	size_bytes      u64
	alignment_bytes u64
	fields          []MemoryAggAggregateFieldLayout
	padding         []MemoryAggByteRange
}

pub struct MemoryAggAggregateAllocaBinding {
pub:
	profile               TargetProfile
	anchor                MemoryAggInstructionAnchor
	authority             MemoryAggNativeAuthority
	pointer_value_id      ssa.ValueID
	aggregate_type_id     ssa.TypeID
	role                  MemoryAggAggregateSlotRole
}

pub struct MemoryAggAggregateFieldPointerBinding {
pub:
	profile                 TargetProfile
	anchor                  MemoryAggInstructionAnchor
	source_pointer_value_id ssa.ValueID
	result_pointer_value_id ssa.ValueID
	aggregate_type_id       ssa.TypeID
	field_index             u16
}

pub struct MemoryAggAggregateConstructBinding {
pub:
	profile           TargetProfile
	anchor            MemoryAggInstructionAnchor
	result_value_id   ssa.ValueID
	aggregate_type_id ssa.TypeID
	padding_policy    MemoryAggConstructPadding
}

pub struct MemoryAggAggregateLoadBinding {
pub:
	profile           TargetProfile
	anchor            MemoryAggInstructionAnchor
	semantics         MemoryAggAccessSemantics
	pointer_value_id  ssa.ValueID
	result_value_id   ssa.ValueID
	aggregate_type_id ssa.TypeID
}

pub struct MemoryAggAggregateStoreBinding {
pub:
	profile           TargetProfile
	anchor            MemoryAggInstructionAnchor
	semantics         MemoryAggAccessSemantics
	source_value_id   ssa.ValueID
	pointer_value_id  ssa.ValueID
	aggregate_type_id ssa.TypeID
}

pub struct MemoryAggAggregateExtractBinding {
pub:
	profile           TargetProfile
	anchor            MemoryAggInstructionAnchor
	source_value_id   ssa.ValueID
	result_value_id   ssa.ValueID
	aggregate_type_id ssa.TypeID
	field_index       u16
}

pub struct MemoryAggAggregateInsertBinding {
pub:
	profile           TargetProfile
	anchor            MemoryAggInstructionAnchor
	source_value_id   ssa.ValueID
	field_value_id    ssa.ValueID
	result_value_id   ssa.ValueID
	aggregate_type_id ssa.TypeID
	field_index       u16
}

pub struct MemoryAggFunctionFacts {
pub:
	profile                  TargetProfile
	function_index           int
	ssa_form                  MemoryAggSsaForm
	scalar_layouts            []MemoryAggScalarLayoutBinding
	static_locals             []MemoryAggStaticLocalBinding
	scalar_constants          []ScalarConstantBinding
	accesses                  []MemoryAggAccessBinding
	aggregate_layouts         []MemoryAggAggregateLayoutBinding
	aggregate_allocas         []MemoryAggAggregateAllocaBinding
	aggregate_field_pointers  []MemoryAggAggregateFieldPointerBinding
	aggregate_constructs      []MemoryAggAggregateConstructBinding
	aggregate_loads           []MemoryAggAggregateLoadBinding
	aggregate_stores          []MemoryAggAggregateStoreBinding
	aggregate_extracts        []MemoryAggAggregateExtractBinding
	aggregate_inserts         []MemoryAggAggregateInsertBinding
}

pub struct MemoryAggSlotBinding {
pub:
	definition      MemoryAggInstructionAnchor
	alloca_value_id ssa.ValueID
	element_type    ssa.TypeID
	element_count   u64
	request         MemorySlotRequest
}

pub struct MemoryAggPointerSnapshot {
pub:
	definition       MemoryAggInstructionAnchor
	value_id         ssa.ValueID
	origin           MemoryAggPointerOrigin
	pointee_type     ssa.TypeID
	root_slot_id     u32
	root_size_bytes  u64
	byte_offset      u64
	remaining_bytes  u64
	is_one_past      bool
}

pub struct MemoryAggScalarAccess {
pub:
	anchor               MemoryAggInstructionAnchor
	kind                 MemoryAggScalarAccessKind
	pointer_value_id     ssa.ValueID
	scalar_value_id      ssa.ValueID
	scalar_type          ssa.TypeID
	root_slot_id         u32
	byte_offset          u64
	semantic_width_bits  int
	semantic_is_unsigned bool
	storage_width_bytes  u8
	alignment_bytes      u8
	canonicalize_i1      bool
}

pub struct MemoryAggAggregateSlotBinding {
pub:
	definition              MemoryAggInstructionAnchor
	owner_value_id          ssa.ValueID
	aggregate_type_id       ssa.TypeID
	role                    MemoryAggAggregateSlotRole
	purpose                 MemoryAggTempPurpose
	anchor_instance_ordinal u16
	request                 MemorySlotRequest
}

pub struct MemoryAggAggregateSnapshot {
pub:
	definition        MemoryAggInstructionAnchor
	value_id          ssa.ValueID
	aggregate_type_id ssa.TypeID
	root_slot_id      u32
	publish_phase     u8
}

pub struct MemoryAggAggregateAction {
pub:
	anchor               MemoryAggInstructionAnchor
	phase                u8
	ordinal              u32
	kind                 MemoryAggAggregateActionKind
	direction            MemoryAggCopyDirection
	source_slot_id       u32
	source_offset_bytes  u64
	destination_slot_id  u32
	destination_offset_bytes u64
	width_bytes          u8
	scalar_value_id      ssa.ValueID
	scalar_type          ssa.TypeID
	canonicalize_i1      bool
}

pub struct MemoryAggPlan {
pub:
	profile               TargetProfile
	function_index        int
	function_id           u32
	scalar_layouts        []MemoryAggScalarLayoutBinding
	slot_requests         []MemoryAggSlotBinding
	pointers              []MemoryAggPointerSnapshot
	accesses              []MemoryAggScalarAccess
	aggregate_layouts     []MemoryAggAggregateLayoutBinding
	aggregate_slots       []MemoryAggAggregateSlotBinding
	aggregate_snapshots   []MemoryAggAggregateSnapshot
	aggregate_actions     []MemoryAggAggregateAction
	total_requested_bytes u64
}

struct MemoryAggUse {
	anchor        MemoryAggInstructionAnchor
	operand_index int
}

struct MemoryAggStructure {
	function_id       u32
	block_ids         []ssa.BlockID
	block_ord_by_id   map[int]int
	anchors           map[int]MemoryAggInstructionAnchor
	active_values     []ssa.ValueID
	uses              map[int][]MemoryAggUse
	successors        [][]int
	predecessors      [][]int
	dominators        []u64
	dominator_words   int
}

struct MemoryAggModuleProof {
	uses                    map[int][]MemoryAggUse
	active_block_count      int
	active_instruction_count int
	use_edge_count          int
}

struct MemoryAggConstantTable {
mut:
	index_by_value map[int]int
	canonical_bits []u64
	use_counts     []int
}

struct MemoryAggAggregateFactIndex {
mut:
	allocas        []int
	field_pointers []int
	constructs     []int
	loads          []int
	stores         []int
	extracts       []int
	inserts        []int
}

struct MemoryAggAggregateLayoutTable {
	index_by_type map[int]int
	layouts       []MemoryAggAggregateLayoutBinding
	use_counts    []int
}

fn memory_agg_error(class string, detail string) IError {
	return error('amd64 memory agg m1a: ${class}:${detail}')
}

fn memory_agg_malformed(detail string) IError {
	return memory_agg_error('malformed', detail)
}

fn memory_agg_incomplete(detail string) IError {
	return memory_agg_error('incomplete_fact', detail)
}

fn memory_agg_duplicate(detail string) IError {
	return memory_agg_error('duplicate_fact', detail)
}

fn memory_agg_stale(detail string) IError {
	return memory_agg_error('stale_fact', detail)
}

fn memory_agg_orphan(detail string) IError {
	return memory_agg_error('orphan_fact', detail)
}

fn memory_agg_upstream(detail string) IError {
	return memory_agg_error('upstream_standby', detail)
}

fn memory_agg_profile_is_valid(profile TargetProfile) bool {
	value := int(profile)
	return value == int(TargetProfile.linux_x86_64_sysv_elf)
		|| value == int(TargetProfile.macos_x86_64_sysv_macho)
		|| value == int(TargetProfile.windows_x86_64_microsoft_abi_coff)
}

fn memory_agg_ssa_form_is_valid(form MemoryAggSsaForm) bool {
	value := int(form)
	return value >= int(MemoryAggSsaForm.unknown)
		&& value <= int(MemoryAggSsaForm.final_static)
}

fn memory_agg_authority_is_valid(authority MemoryAggNativeAuthority) bool {
	value := int(authority)
	return value >= int(MemoryAggNativeAuthority.unknown)
		&& value <= int(MemoryAggNativeAuthority.union_)
}

fn memory_agg_alloca_form_is_valid(form MemoryAggAllocaForm) bool {
	value := int(form)
	return value >= int(MemoryAggAllocaForm.one)
		&& value <= int(MemoryAggAllocaForm.constant_count)
}

fn memory_agg_access_semantics_is_valid(semantics MemoryAggAccessSemantics) bool {
	value := int(semantics)
	return value >= int(MemoryAggAccessSemantics.unknown)
		&& value <= int(MemoryAggAccessSemantics.atomic)
}

fn memory_agg_access_kind_is_valid(kind MemoryAggScalarAccessKind) bool {
	value := int(kind)
	return value >= int(MemoryAggScalarAccessKind.load)
		&& value <= int(MemoryAggScalarAccessKind.store)
}

fn memory_agg_aggregate_slot_role_is_valid(role MemoryAggAggregateSlotRole) bool {
	value := int(role)
	return value >= int(MemoryAggAggregateSlotRole.fixed_alloca)
		&& value <= int(MemoryAggAggregateSlotRole.aggregate_temp)
}

fn memory_agg_construct_padding_is_valid(policy MemoryAggConstructPadding) bool {
	return policy == .zero
}

fn memory_agg_validate_raw_domains(facts &MemoryAggFunctionFacts) ! {
	if !memory_agg_profile_is_valid(facts.profile) {
		return memory_agg_malformed('unsupported target profile')
	}
	if !memory_agg_ssa_form_is_valid(facts.ssa_form) {
		return memory_agg_malformed('unsupported SSA form')
	}
	for index, layout in facts.scalar_layouts {
		if !memory_agg_profile_is_valid(layout.profile) {
			return memory_agg_malformed('scalar layout ${index} has unsupported profile')
		}
		if !memory_agg_authority_is_valid(layout.authority) {
			return memory_agg_malformed('scalar layout ${index} has unsupported authority')
		}
	}
	for index, local in facts.static_locals {
		if !memory_agg_profile_is_valid(local.profile) {
			return memory_agg_malformed('static local ${index} has unsupported profile')
		}
		if !memory_agg_authority_is_valid(local.authority) {
			return memory_agg_malformed('static local ${index} has unsupported authority')
		}
		if !memory_agg_alloca_form_is_valid(local.form) {
			return memory_agg_malformed('static local ${index} has unsupported alloca form')
		}
	}
	for index, access in facts.accesses {
		if !memory_agg_profile_is_valid(access.profile) {
			return memory_agg_malformed('access ${index} has unsupported profile')
		}
		if !memory_agg_access_semantics_is_valid(access.semantics) {
			return memory_agg_malformed('access ${index} has unsupported semantics')
		}
		if !memory_agg_access_kind_is_valid(access.kind) {
			return memory_agg_malformed('access ${index} has unsupported kind')
		}
	}
	for index, layout in facts.aggregate_layouts {
		if !memory_agg_profile_is_valid(layout.profile) {
			return memory_agg_malformed('aggregate layout ${index} has unsupported profile')
		}
		if !memory_agg_authority_is_valid(layout.authority) {
			return memory_agg_malformed('aggregate layout ${index} has unsupported authority')
		}
	}
	for index, alloca in facts.aggregate_allocas {
		if !memory_agg_profile_is_valid(alloca.profile) {
			return memory_agg_malformed('aggregate alloca ${index} has unsupported profile')
		}
		if !memory_agg_authority_is_valid(alloca.authority) {
			return memory_agg_malformed('aggregate alloca ${index} has unsupported authority')
		}
		if !memory_agg_aggregate_slot_role_is_valid(alloca.role) {
			return memory_agg_malformed('aggregate alloca ${index} has unsupported slot role')
		}
	}
	for index, pointer in facts.aggregate_field_pointers {
		if !memory_agg_profile_is_valid(pointer.profile) {
			return memory_agg_malformed('aggregate field pointer ${index} has unsupported profile')
		}
	}
	for index, construct in facts.aggregate_constructs {
		if !memory_agg_profile_is_valid(construct.profile) {
			return memory_agg_malformed('aggregate construct ${index} has unsupported profile')
		}
		if !memory_agg_construct_padding_is_valid(construct.padding_policy) {
			return memory_agg_malformed('aggregate construct ${index} has unsupported padding policy')
		}
	}
	for index, load in facts.aggregate_loads {
		if !memory_agg_profile_is_valid(load.profile) {
			return memory_agg_malformed('aggregate load ${index} has unsupported profile')
		}
		if !memory_agg_access_semantics_is_valid(load.semantics) {
			return memory_agg_malformed('aggregate load ${index} has unsupported semantics')
		}
	}
	for index, store in facts.aggregate_stores {
		if !memory_agg_profile_is_valid(store.profile) {
			return memory_agg_malformed('aggregate store ${index} has unsupported profile')
		}
		if !memory_agg_access_semantics_is_valid(store.semantics) {
			return memory_agg_malformed('aggregate store ${index} has unsupported semantics')
		}
	}
	for index, extract in facts.aggregate_extracts {
		if !memory_agg_profile_is_valid(extract.profile) {
			return memory_agg_malformed('aggregate extract ${index} has unsupported profile')
		}
	}
	for index, insert in facts.aggregate_inserts {
		if !memory_agg_profile_is_valid(insert.profile) {
			return memory_agg_malformed('aggregate insert ${index} has unsupported profile')
		}
	}
}

fn memory_agg_validate_input_caps(facts &MemoryAggFunctionFacts) ! {
	memory_agg_validate_count('scalar layout count', facts.scalar_layouts.len,
		memory_agg_max_scalar_layouts)!
	memory_agg_validate_count('static local count', facts.static_locals.len,
		memory_agg_max_static_locals)!
	memory_agg_validate_count('scalar constant count', facts.scalar_constants.len,
		memory_agg_max_scalar_constants)!
	memory_agg_validate_count('access count', facts.accesses.len, memory_agg_max_accesses)!
	memory_agg_validate_count('aggregate layout count', facts.aggregate_layouts.len,
		memory_agg_max_aggregate_layouts)!
	memory_agg_validate_count('aggregate alloca count', facts.aggregate_allocas.len,
		memory_agg_max_static_locals)!
	mut field_count := 0
	mut padding_count := 0
	for index, layout in facts.aggregate_layouts {
		memory_agg_validate_count('aggregate layout ${index} field count', layout.fields.len,
			memory_agg_max_aggregate_fields_per_layout)!
		memory_agg_validate_count('aggregate layout ${index} padding count',
			layout.padding.len, memory_agg_max_padding_ranges_per_layout)!
		if field_count > memory_agg_max_aggregate_fields - layout.fields.len {
			return memory_agg_malformed('aggregate field count exceeds ${memory_agg_max_aggregate_fields}')
		}
		field_count += layout.fields.len
		if padding_count > memory_agg_max_padding_ranges - layout.padding.len {
			return memory_agg_malformed('aggregate padding count exceeds ${memory_agg_max_padding_ranges}')
		}
		padding_count += layout.padding.len
	}
	mut aggregate_fact_count := facts.aggregate_field_pointers.len
	for count in [facts.aggregate_constructs.len, facts.aggregate_loads.len,
		facts.aggregate_stores.len, facts.aggregate_extracts.len, facts.aggregate_inserts.len] {
		if aggregate_fact_count > memory_agg_max_aggregate_facts - count {
			return memory_agg_malformed('aggregate operation count exceeds ${memory_agg_max_aggregate_facts}')
		}
		aggregate_fact_count += count
	}
	memory_agg_validate_count('aggregate operation count', aggregate_fact_count,
		memory_agg_max_aggregate_facts)!
}

fn memory_agg_validate_count(label string, count int, maximum int) ! {
	if count > maximum {
		return memory_agg_malformed('${label} ${count} exceeds ${maximum}')
	}
}

fn memory_agg_validate_module_preallocation(m &ssa.Module) ! {
	memory_agg_validate_count('module value count', m.values.len,
		memory_agg_max_module_values)!
	memory_agg_validate_count('module function count', m.funcs.len,
		memory_agg_max_module_functions)!
	memory_agg_validate_count('module instruction count', m.instrs.len,
		memory_agg_max_module_instructions)!
}

fn memory_agg_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_agg_malformed('arithmetic overflow')
	}
	return left + right
}

fn memory_agg_checked_mul(left u64, right u64) !u64 {
	if left != 0 && right > max_u64 / left {
		return memory_agg_malformed('arithmetic overflow')
	}
	return left * right
}

fn memory_agg_type_kind(m &ssa.Module, type_id ssa.TypeID) !ssa.TypeKind {
	return memory_agg_get_type(m, type_id)!.kind
}

fn memory_agg_value_type_kind(m &ssa.Module, value_id ssa.ValueID) !ssa.TypeKind {
	index := int(value_id)
	if index <= 0 || index >= m.values.len {
		return memory_agg_malformed('value ${index} is outside 1..${m.values.len - 1}')
	}
	value := m.values[index]
	if value.id != value_id {
		return memory_agg_malformed('value ${index} has id ${value.id}')
	}
	return memory_agg_type_kind(m, value.typ)
}

fn memory_agg_anchor_equal(left MemoryAggInstructionAnchor, right MemoryAggInstructionAnchor) bool {
	return left.function_index == right.function_index && left.block_id == right.block_id
		&& left.block_ordinal == right.block_ordinal
		&& left.instruction_value_id == right.instruction_value_id
		&& left.instruction_index == right.instruction_index
		&& left.instruction_ordinal == right.instruction_ordinal
}

fn memory_agg_is_terminator(op ssa.OpCode) bool {
	return op in [.ret, .br, .jmp, .switch_, .unreachable]
}

fn memory_agg_value_operand_indices(instruction &ssa.Instruction) []int {
	match instruction.op {
		.jmp {
			return []
		}
		.br {
			return if instruction.operands.len > 0 { [0] } else { [] }
		}
		.switch_ {
			mut result := []int{}
			if instruction.operands.len > 0 {
				result << 0
			}
			for index := 2; index < instruction.operands.len; index += 2 {
				result << index
			}
			return result
		}
		.phi {
			mut result := []int{}
			for index := 0; index < instruction.operands.len; index += 2 {
				result << index
			}
			return result
		}
		else {
			mut result := []int{cap: instruction.operands.len}
			for index in 0 .. instruction.operands.len {
				result << index
			}
			return result
		}
	}
}

fn memory_agg_snapshot_module_uses(m &ssa.Module) !MemoryAggModuleProof {
	mut block_owners := map[int]int{}
	mut parameter_owners := map[int]int{}
	mut active_value_owners := map[int]int{}
	mut instruction_owners := map[int]int{}
	mut uses := map[int][]MemoryAggUse{}
	mut active_block_count := 0
	mut active_instruction_count := 0
	mut use_edge_count := 0
	for function_index, function in m.funcs {
		if function.id != function_index {
			return memory_agg_malformed('function ${function_index} has id ${function.id}')
		}
		memory_agg_get_type(m, function.typ)!
		mut local_parameters := map[int]bool{}
		for parameter_index, parameter_id in function.params {
			parameter_value_index := int(parameter_id)
			if parameter_value_index in local_parameters {
				return memory_agg_malformed('function ${function_index} contains duplicate parameter ${parameter_id}')
			}
			if parameter_value_index in parameter_owners {
				return memory_agg_malformed('parameter ${parameter_id} has duplicate function ownership')
			}
			parameter := memory_agg_get_value(m, parameter_id)!
			if parameter.kind != .argument || parameter.index != parameter_index {
				return memory_agg_malformed('function ${function_index} parameter ${parameter_id} ownership is inconsistent')
			}
			local_parameters[parameter_value_index] = true
			parameter_owners[parameter_value_index] = function_index
		}
		for block_ordinal, block_id in function.blocks {
			active_block_count++
			memory_agg_validate_count('active block count', active_block_count,
				memory_agg_max_blocks)!
			block_index := int(block_id)
			if block_index < 0 || block_index >= m.blocks.len {
				return memory_agg_malformed('function ${function_index} block ${block_index} is outside the block table')
			}
			if block_index in block_owners {
				return memory_agg_malformed('block ${block_index} has duplicate function ownership')
			}
			block := m.blocks[block_index]
			if block.id != block_id || block.parent != function_index {
				return memory_agg_malformed('block ${block_index} ownership is inconsistent')
			}
			block_owners[block_index] = function_index
			for instruction_ordinal, value_id in block.instrs {
				active_instruction_count++
				memory_agg_validate_count('active instruction count',
					active_instruction_count, memory_agg_max_active_instructions)!
				value_index := int(value_id)
				if value_index <= 0 || value_index >= m.values.len {
					return memory_agg_malformed('block ${block_id} contains invalid instruction value ${value_index}')
				}
				if value_index in active_value_owners {
					return memory_agg_malformed('instruction value ${value_index} has duplicate block ownership')
				}
				value := m.values[value_index]
				if value.id != value_id || value.kind != .instruction {
					return memory_agg_malformed('active value ${value_index} is not its canonical instruction value')
				}
				if value.index < 0 || value.index >= m.instrs.len {
					return memory_agg_malformed('instruction value ${value_index} has invalid instruction index ${value.index}')
				}
				if value.index in instruction_owners {
					return memory_agg_malformed('instruction index ${value.index} has duplicate active owners')
				}
				instruction := m.instrs[value.index]
				if instruction.block != block_id || instruction.typ != value.typ {
					return memory_agg_malformed('instruction value ${value_index} block/type ownership is inconsistent')
				}
				op_value := int(instruction.op)
				if op_value < int(ssa.OpCode.ret) || op_value > int(ssa.OpCode.struct_init) {
					return memory_agg_malformed('instruction value ${value_index} has unsupported opcode ${op_value}')
				}
				memory_agg_validate_count('instruction operand count',
					instruction.operands.len, memory_agg_max_use_edges)!
				anchor := MemoryAggInstructionAnchor{
					function_index:       function_index
					block_id:             block_id
					block_ordinal:        u32(block_ordinal)
					instruction_value_id: value_id
					instruction_index:    value.index
					instruction_ordinal:  u32(instruction_ordinal)
				}
				active_value_owners[value_index] = function_index
				instruction_owners[value.index] = value_index
				for operand_index in memory_agg_value_operand_indices(&instruction) {
					if operand_index >= instruction.operands.len {
						continue
					}
					operand_id := instruction.operands[operand_index]
					operand_value_index := int(operand_id)
					if operand_value_index <= 0 || operand_value_index >= m.values.len {
						return memory_agg_malformed('instruction value ${value_index} has invalid operand ${operand_value_index}')
					}
					if m.values[operand_value_index].id != operand_id {
						return memory_agg_malformed('operand value ${operand_value_index} has stale id')
					}
					use_edge_count++
					memory_agg_validate_count('use edge count', use_edge_count,
						memory_agg_max_use_edges)!
					uses[operand_value_index] << MemoryAggUse{
						anchor:        anchor
						operand_index: operand_index
					}
				}
			}
		}
	}
	for value_index, value in m.values {
		if value.kind != .instruction || value.index < 0 || value.index >= m.instrs.len {
			continue
		}
		instruction := m.instrs[value.index]
		if int(instruction.block) in block_owners && value_index !in active_value_owners {
			return memory_agg_malformed('instruction value ${value_index} is orphaned from its owning block')
		}
	}
	for instruction_index, instruction in m.instrs {
		if int(instruction.block) in block_owners && instruction_index !in instruction_owners {
			return memory_agg_malformed('instruction index ${instruction_index} is orphaned from an active value')
		}
	}
	return MemoryAggModuleProof{
		uses:                     uses
		active_block_count:       active_block_count
		active_instruction_count: active_instruction_count
		use_edge_count:           use_edge_count
	}
}

fn memory_agg_validate_terminator(m &ssa.Module, function_index int, block_ord_by_id map[int]int, instruction &ssa.Instruction) ![]int {
	mut successors := []int{}
	match instruction.op {
		.ret {
			if instruction.operands.len > 1 {
				return memory_agg_malformed('RET has ${instruction.operands.len} operands')
			}
			if instruction.typ != ssa.TypeID(0) {
				return memory_agg_malformed('RET type must be canonical void')
			}
		}
		.jmp {
			if instruction.operands.len != 1 {
				return memory_agg_malformed('JMP has ${instruction.operands.len} operands')
			}
			target := int(instruction.operands[0])
			successors << (block_ord_by_id[target] or {
				return memory_agg_malformed('JMP target block ${target} does not belong to function ${function_index}')
			})
		}
		.br {
			if instruction.operands.len != 3 {
				return memory_agg_malformed('BR has ${instruction.operands.len} operands')
			}
			condition_id := instruction.operands[0]
			condition_index := int(condition_id)
			if condition_index <= 0 || condition_index >= m.values.len {
				return memory_agg_malformed('BR condition ${condition_index} is outside the value table')
			}
			condition_type := m.values[condition_index].typ
			condition := memory_agg_get_type(m, condition_type)!
			if condition.kind != .int_t || condition.width != 1 {
				return memory_agg_malformed('BR condition is not i1')
			}
			for operand_index in 1 .. 3 {
				target := int(instruction.operands[operand_index])
				successors << (block_ord_by_id[target] or {
					return memory_agg_malformed('BR target block ${target} does not belong to function ${function_index}')
				})
			}
		}
		.switch_ {
			if instruction.operands.len < 2 || instruction.operands.len % 2 != 0 {
				return memory_agg_malformed('SWITCH has invalid operand count ${instruction.operands.len}')
			}
			selector_id := int(instruction.operands[0])
			if selector_id <= 0 || selector_id >= m.values.len
				|| memory_agg_value_type_kind(m, instruction.operands[0])! != .int_t {
				return memory_agg_malformed('SWITCH selector is not a scalar integer')
			}
			default_target := int(instruction.operands[1])
			successors << (block_ord_by_id[default_target] or {
				return memory_agg_malformed('SWITCH default block ${default_target} does not belong to function ${function_index}')
			})
			for index := 2; index < instruction.operands.len; index += 2 {
				case_id := int(instruction.operands[index])
				if case_id <= 0 || case_id >= m.values.len
					|| m.values[case_id].kind != .constant {
					return memory_agg_malformed('SWITCH case ${case_id} is not a constant')
				}
				target := int(instruction.operands[index + 1])
				successors << (block_ord_by_id[target] or {
					return memory_agg_malformed('SWITCH target block ${target} does not belong to function ${function_index}')
				})
			}
		}
		.unreachable {
			if instruction.operands.len != 0 {
				return memory_agg_malformed('UNREACHABLE has ${instruction.operands.len} operands')
			}
		}
		else {
			return memory_agg_malformed('block does not end in a terminator')
		}
	}
	return successors
}

fn memory_agg_build_dominators(successors [][]int, predecessors [][]int) !([]u64, int) {
	block_count := successors.len
	words := (block_count + 63) / 64
	mut reachable := []bool{len: block_count}
	mut queue := []int{cap: block_count}
	reachable[0] = true
	queue << 0
	mut cursor := 0
	for cursor < queue.len {
		block := queue[cursor]
		cursor++
		for successor in successors[block] {
			if !reachable[successor] {
				reachable[successor] = true
				queue << successor
			}
		}
	}
	for block, is_reachable in reachable {
		if !is_reachable {
			return memory_agg_malformed('block ordinal ${block} is unreachable from entry')
		}
	}

	mut dominators := []u64{len: block_count * words}
	valid_last_mask := if block_count % 64 == 0 {
		max_u64
	} else {
		(u64(1) << (block_count % 64)) - 1
	}
	for block in 0 .. block_count {
		row := block * words
		if block == 0 {
			dominators[row] = 1
			continue
		}
		for word in 0 .. words {
			dominators[row + word] = if word == words - 1 { valid_last_mask } else { max_u64 }
		}
	}
	for _ in 0 .. block_count {
		mut changed := false
		for block in 1 .. block_count {
			if predecessors[block].len == 0 {
				return memory_agg_malformed('reachable block ordinal ${block} has no predecessor')
			}
			row := block * words
			first_predecessor_row := predecessors[block][0] * words
			mut next := []u64{len: words}
			for word in 0 .. words {
				next[word] = dominators[first_predecessor_row + word]
			}
			for predecessor_index in 1 .. predecessors[block].len {
				predecessor_row := predecessors[block][predecessor_index] * words
				for word in 0 .. words {
					next[word] &= dominators[predecessor_row + word]
				}
			}
			next[block / 64] |= u64(1) << (block % 64)
			for word in 0 .. words {
				if dominators[row + word] != next[word] {
					dominators[row + word] = next[word]
					changed = true
				}
			}
		}
		if !changed {
			return dominators, words
		}
	}
	return memory_agg_malformed('dominance fixed point did not converge')
}

fn memory_agg_snapshot_structure(m &ssa.Module, function_index int) !MemoryAggStructure {
	if function_index < 0 || function_index >= m.funcs.len {
		return memory_agg_malformed('function index ${function_index} is outside 0..${m.funcs.len - 1}')
	}
	function := m.funcs[function_index]
	if function.id != function_index {
		return memory_agg_malformed('function ${function_index} has id ${function.id}')
	}
	if function.is_c_extern || function.is_prototype {
		return memory_agg_upstream('function has no final body')
	}
	_ = memory_agg_get_type(m, function.typ)!
	mut parameter_ids := map[int]bool{}
	for parameter_index, parameter_id in function.params {
		if int(parameter_id) in parameter_ids {
			return memory_agg_malformed('function contains duplicate parameter ${parameter_id}')
		}
		parameter := memory_agg_get_value(m, parameter_id)!
		if parameter.kind != .argument || parameter.index != parameter_index {
			return memory_agg_malformed('function parameter ${parameter_id} ownership is inconsistent')
		}
		parameter_ids[int(parameter_id)] = true
	}
	if function.blocks.len == 0 {
		return memory_agg_malformed('function has no blocks')
	}
	if function.blocks.len > memory_agg_max_blocks {
		return memory_agg_malformed('block count ${function.blocks.len} exceeds ${memory_agg_max_blocks}')
	}
	if u64(function.id) > u64(max_u32) {
		return memory_agg_malformed('function id exceeds u32')
	}

	mut block_ord_by_id := map[int]int{}
	mut block_ids := []ssa.BlockID{cap: function.blocks.len}
	for block_ordinal, block_id in function.blocks {
		block_index := int(block_id)
		if block_index < 0 || block_index >= m.blocks.len {
			return memory_agg_malformed('function block ${block_index} is outside the block table')
		}
		if block_index in block_ord_by_id {
			return memory_agg_malformed('function contains duplicate block ${block_index}')
		}
		block := m.blocks[block_index]
		if block.id != block_id || block.parent != function_index {
			return memory_agg_malformed('block ${block_index} ownership is inconsistent')
		}
		block_ord_by_id[block_index] = block_ordinal
		block_ids << block_id
	}

	mut anchors := map[int]MemoryAggInstructionAnchor{}
	mut active_values := []ssa.ValueID{}
	mut instruction_owners := map[int]int{}
	mut uses := map[int][]MemoryAggUse{}
	mut use_edge_count := 0
	mut successors := [][]int{len: block_ids.len}
	for block_ordinal, block_id in block_ids {
		block := m.blocks[int(block_id)]
		if block.instrs.len == 0 {
			return memory_agg_malformed('block ${block_id} has no terminator')
		}
		for instruction_ordinal, value_id in block.instrs {
			if active_values.len >= memory_agg_max_active_instructions {
				return memory_agg_malformed('active instruction count exceeds ${memory_agg_max_active_instructions}')
			}
			value_index := int(value_id)
			if value_index <= 0 || value_index >= m.values.len {
				return memory_agg_malformed('block ${block_id} contains invalid instruction value ${value_index}')
			}
			if value_index in anchors {
				return memory_agg_malformed('instruction value ${value_index} has duplicate block ownership')
			}
			value := m.values[value_index]
			if value.id != value_id || value.kind != .instruction {
				return memory_agg_malformed('active value ${value_index} is not its canonical instruction value')
			}
			if value.index < 0 || value.index >= m.instrs.len {
				return memory_agg_malformed('instruction value ${value_index} has invalid instruction index ${value.index}')
			}
			if value.index in instruction_owners {
				return memory_agg_malformed('instruction index ${value.index} has duplicate active owners')
			}
			instruction := m.instrs[value.index]
			if instruction.block != block_id || instruction.typ != value.typ {
				return memory_agg_malformed('instruction value ${value_index} block/type ownership is inconsistent')
			}
			if memory_agg_is_terminator(instruction.op)
				!= (instruction_ordinal == block.instrs.len - 1) {
				return memory_agg_malformed('block ${block_id} terminator position is invalid')
			}
			anchor := MemoryAggInstructionAnchor{
				function_index:       function_index
				block_id:             block_id
				block_ordinal:        u32(block_ordinal)
				instruction_value_id: value_id
				instruction_index:    value.index
				instruction_ordinal:  u32(instruction_ordinal)
			}
			anchors[value_index] = anchor
			instruction_owners[value.index] = value_index
			active_values << value_id
			for operand_index in memory_agg_value_operand_indices(&instruction) {
				if operand_index >= instruction.operands.len {
					continue
				}
				operand_id := instruction.operands[operand_index]
				operand_value_index := int(operand_id)
				if operand_value_index <= 0 || operand_value_index >= m.values.len {
					return memory_agg_malformed('instruction value ${value_index} has invalid operand ${operand_value_index}')
				}
				if m.values[operand_value_index].id != operand_id {
					return memory_agg_malformed('operand value ${operand_value_index} has stale id')
				}
				use_edge_count++
				if use_edge_count > memory_agg_max_use_edges {
					return memory_agg_malformed('use edge count exceeds ${memory_agg_max_use_edges}')
				}
				uses[operand_value_index] << MemoryAggUse{
					anchor:        anchor
					operand_index: operand_index
				}
			}
		}
		last_value := block.instrs.last()
		last_instruction := m.instrs[m.values[int(last_value)].index]
		successors[block_ordinal] = memory_agg_validate_terminator(m, function_index,
			block_ord_by_id, &last_instruction)!
	}

	for value_index, value in m.values {
		if value.kind != .instruction || value.index < 0 || value.index >= m.instrs.len {
			continue
		}
		instruction := m.instrs[value.index]
		if int(instruction.block) in block_ord_by_id && value_index !in anchors {
			return memory_agg_malformed('instruction value ${value_index} is orphaned from its owning block')
		}
	}
	for instruction_index, instruction in m.instrs {
		if int(instruction.block) in block_ord_by_id && instruction_index !in instruction_owners {
			return memory_agg_malformed('instruction index ${instruction_index} is orphaned from an active value')
		}
	}

	mut predecessors := [][]int{len: block_ids.len}
	for block_ordinal, targets in successors {
		for target in targets {
			mut incoming := predecessors[target].clone()
			if block_ordinal !in incoming {
				incoming << block_ordinal
				predecessors[target] = incoming
			}
		}
	}
	dominators, words := memory_agg_build_dominators(successors, predecessors)!
	return MemoryAggStructure{
		function_id:     u32(function.id)
		block_ids:       block_ids
		block_ord_by_id: block_ord_by_id
		anchors:         anchors
		active_values:   active_values
		uses:            uses
		successors:      successors
		predecessors:    predecessors
		dominators:      dominators
		dominator_words: words
	}
}

fn memory_agg_get_type(m &ssa.Module, type_id ssa.TypeID) !ssa.Type {
	type_index := int(type_id)
	if type_index < 0 || type_index >= m.type_store.types.len {
		return memory_agg_malformed('type ${type_index} is outside 0..${m.type_store.types.len - 1}')
	}
	typ := m.type_store.types[type_index]
	kind := int(typ.kind)
	if kind < int(ssa.TypeKind.void_t) || kind > int(ssa.TypeKind.metadata_t) {
		return memory_agg_malformed('type ${type_index} has unsupported kind ${kind}')
	}
	if (typ.is_c_struct || typ.is_union) && typ.kind != .struct_t {
		return memory_agg_malformed('type ${type_index} has contradictory C struct/union metadata')
	}
	if typ.is_c_struct && typ.is_union {
		return memory_agg_malformed('type ${type_index} has contradictory C struct/union metadata')
	}
	match typ.kind {
		.void_t {
			if type_index != 0 || typ.width != 0 || typ.is_unsigned || typ.elem_type != 0
				|| typ.len != 0 || typ.fields.len != 0 || typ.field_names.len != 0
				|| typ.params.len != 0 || typ.ret_type != 0 {
				return memory_agg_malformed('type ${type_index} has noncanonical void payload')
			}
		}
		.int_t {
			if typ.width == 1 && typ.is_unsigned {
				return memory_agg_malformed('type ${type_index} has noncanonical unsigned i1 payload')
			}
			if type_index == 0 || typ.width !in [1, 8, 16, 32, 64]
				|| typ.elem_type != 0 || typ.len != 0
				|| typ.fields.len != 0 || typ.field_names.len != 0
				|| typ.params.len != 0 || typ.ret_type != 0 {
				return memory_agg_malformed('type ${type_index} has noncanonical integer payload')
			}
		}
		.ptr_t {
			if type_index == 0 || typ.width != 0 || typ.is_unsigned
				|| int(typ.elem_type) < 0 || int(typ.elem_type) >= m.type_store.types.len
				|| typ.len != 0 || typ.fields.len != 0 || typ.field_names.len != 0
				|| typ.params.len != 0 || typ.ret_type != 0 {
				return memory_agg_malformed('type ${type_index} has noncanonical pointer payload')
			}
		}
		else {}
	}
	return typ
}

fn memory_agg_get_value(m &ssa.Module, value_id ssa.ValueID) !ssa.Value {
	value_index := int(value_id)
	if value_index <= 0 || value_index >= m.values.len {
		return memory_agg_malformed('value ${value_index} is outside 1..${m.values.len - 1}')
	}
	value := m.values[value_index]
	if value.id != value_id {
		return memory_agg_malformed('value ${value_index} has id ${value.id}')
	}
	_ = memory_agg_get_type(m, value.typ)!
	return value
}

fn memory_agg_type_is_pointer(m &ssa.Module, type_id ssa.TypeID) !bool {
	return memory_agg_get_type(m, type_id)!.kind == .ptr_t
}

fn memory_agg_instruction_has_pointer(m &ssa.Module, instruction &ssa.Instruction) !bool {
	if memory_agg_type_is_pointer(m, instruction.typ)! {
		return true
	}
	for operand_index in memory_agg_value_operand_indices(instruction) {
		if operand_index < instruction.operands.len {
			value := memory_agg_get_value(m, instruction.operands[operand_index])!
			if memory_agg_type_is_pointer(m, value.typ)! {
				return true
			}
		}
	}
	return false
}

fn memory_agg_validate_scalar_or_void_instruction(m &ssa.Module, value_id ssa.ValueID, instruction &ssa.Instruction) ! {
	result_type := memory_agg_get_type(m, instruction.typ)!
	if result_type.kind !in [ssa.TypeKind.void_t, .int_t] {
		return memory_agg_upstream('instruction ${value_id} result type is outside scalar M1a')
	}
	for operand_index in memory_agg_value_operand_indices(instruction) {
		if operand_index >= instruction.operands.len {
			continue
		}
		operand := memory_agg_get_value(m, instruction.operands[operand_index])!
		operand_type := memory_agg_get_type(m, operand.typ)!
		if operand_type.kind !in [ssa.TypeKind.void_t, .int_t] {
			return memory_agg_upstream('instruction ${value_id} operand ${operand_index} is outside scalar M1a')
		}
	}
}

fn memory_agg_require_scalar_type(m &ssa.Module, context string, type_id ssa.TypeID) !ssa.Type {
	typ := memory_agg_get_type(m, type_id)!
	if typ.kind != .int_t {
		return memory_agg_upstream('${context} requires scalar integer type')
	}
	return typ
}

fn memory_agg_require_pointer_type(m &ssa.Module, context string, type_id ssa.TypeID) !ssa.Type {
	typ := memory_agg_get_type(m, type_id)!
	if typ.kind != .ptr_t {
		return memory_agg_malformed('${context} requires pointer type')
	}
	_ = memory_agg_get_type(m, typ.elem_type)!
	return typ
}

fn memory_agg_validate_target_and_function(m &ssa.Module, facts &MemoryAggFunctionFacts) ! {
	if m.target.ptr_size != 8 {
		return memory_agg_upstream('target pointer size must be 8 bytes')
	}
	if !m.target.endian_little {
		return memory_agg_upstream('target must be little endian')
	}
	if m.type_store.types.len == 0 {
		return memory_agg_malformed('type 0 must be canonical void')
	}
	_ = memory_agg_get_type(m, ssa.TypeID(0))!
	if facts.ssa_form != .final_static {
		return memory_agg_upstream('final_static SSA attestation is required')
	}
	if facts.function_index < 0 || facts.function_index >= m.funcs.len {
		return memory_agg_malformed('function index ${facts.function_index} is outside 0..${m.funcs.len - 1}')
	}
}

fn memory_agg_validate_relevant_op_table(m &ssa.Module, structure &MemoryAggStructure, aggregate_index &MemoryAggAggregateFactIndex) ! {
	for value_id in structure.active_values {
		value := memory_agg_get_value(m, value_id)!
		instruction := m.instrs[value.index]
		op_value := int(instruction.op)
		if op_value < int(ssa.OpCode.ret) || op_value > int(ssa.OpCode.struct_init) {
			return memory_agg_malformed('instruction value ${value_id} has unsupported opcode ${op_value}')
		}
		for operand_index in memory_agg_value_operand_indices(&instruction) {
			if operand_index < instruction.operands.len {
				_ = memory_agg_get_value(m, instruction.operands[operand_index])!
			}
		}
		atomic_value := int(instruction.atomic_ord)
		if atomic_value < int(ssa.AtomicOrdering.not_atomic)
			|| atomic_value > int(ssa.AtomicOrdering.seq_cst) {
			return memory_agg_malformed('instruction value ${value_id} has unsupported atomic ordering ${atomic_value}')
		}
		match instruction.op {
			.alloca {
				if instruction.operands.len > 1 {
					return memory_agg_malformed('ALLOCA ${value_id} has ${instruction.operands.len} operands')
				}
				pointer_type := memory_agg_require_pointer_type(m, 'ALLOCA ${value_id}',
					instruction.typ)!
				element_type := memory_agg_get_type(m, pointer_type.elem_type)!
				if element_type.kind == .struct_t
					&& aggregate_index.allocas[int(value_id)] >= 0 {
					if instruction.operands.len != 0 {
						return memory_agg_upstream('aggregate ALLOCA ${value_id} count form is outside M1b')
					}
				} else {
					_ = memory_agg_require_scalar_type(m, 'ALLOCA ${value_id} element',
						pointer_type.elem_type)!
				}
			}
			.get_element_ptr {
				if instruction.operands.len != 2 {
					return memory_agg_malformed('GEP ${value_id} has ${instruction.operands.len} operands')
				}
				_ = memory_agg_require_pointer_type(m, 'GEP ${value_id} result',
					instruction.typ)!
				source := memory_agg_get_value(m, instruction.operands[0])!
				source_pointer := memory_agg_require_pointer_type(m, 'GEP ${value_id} operand 0',
					source.typ)!
				delta := memory_agg_get_value(m, instruction.operands[1])!
				_ = memory_agg_require_scalar_type(m, 'GEP ${value_id} byte delta',
					delta.typ)!
				source_kind := memory_agg_get_type(m, source_pointer.elem_type)!.kind
				field_pointer_index := aggregate_index.field_pointers[int(value_id)]
				if field_pointer_index >= 0 && source_kind != .struct_t {
					return memory_agg_orphan('aggregate field pointer ${field_pointer_index} attests non-aggregate GEP ${value_id}')
				}
				if source_kind == .struct_t && field_pointer_index < 0 {
					return memory_agg_upstream('GEP ${value_id} requires authoritative aggregate field-pointer fact')
				}
			}
			.add {
				if memory_agg_instruction_has_pointer(m, &instruction)! {
					if instruction.operands.len != 2 {
						return memory_agg_malformed('pointer ADD ${value_id} has ${instruction.operands.len} operands')
					}
					_ = memory_agg_require_pointer_type(m, 'pointer ADD ${value_id} result',
						instruction.typ)!
					source := memory_agg_get_value(m, instruction.operands[0])!
					source_pointer := memory_agg_require_pointer_type(m,
						'pointer ADD ${value_id} operand 0',
						source.typ)!
					delta := memory_agg_get_value(m, instruction.operands[1])!
					_ = memory_agg_require_scalar_type(m, 'pointer ADD ${value_id} byte delta',
						delta.typ)!
					source_kind := memory_agg_get_type(m, source_pointer.elem_type)!.kind
					field_pointer_index := aggregate_index.field_pointers[int(value_id)]
					if field_pointer_index >= 0 && source_kind != .struct_t {
						return memory_agg_orphan('aggregate field pointer ${field_pointer_index} attests non-aggregate pointer ADD ${value_id}')
					}
					if source_kind == .struct_t && field_pointer_index < 0 {
						return memory_agg_upstream('pointer ADD ${value_id} requires authoritative aggregate field-pointer fact')
					}
				} else {
					memory_agg_validate_scalar_or_void_instruction(m, value_id,
						&instruction)!
				}
			}
			.bitcast {
				if instruction.operands.len == 0 {
					return memory_agg_upstream('zero-operand BITCAST tombstone ${value_id}')
				}
				if memory_agg_instruction_has_pointer(m, &instruction)! {
					if instruction.operands.len != 1 {
						return memory_agg_malformed('pointer BITCAST ${value_id} has ${instruction.operands.len} operands')
					}
					_ = memory_agg_require_pointer_type(m, 'pointer BITCAST ${value_id} result',
						instruction.typ)!
					source := memory_agg_get_value(m, instruction.operands[0])!
					_ = memory_agg_require_pointer_type(m, 'pointer BITCAST ${value_id} operand',
						source.typ)!
				} else {
					memory_agg_validate_scalar_or_void_instruction(m, value_id,
						&instruction)!
				}
			}
			.load {
				if instruction.operands.len != 1 {
					return memory_agg_malformed('LOAD ${value_id} has ${instruction.operands.len} operands')
				}
				result_type := memory_agg_get_type(m, instruction.typ)!
				if result_type.kind == .struct_t {
					if aggregate_index.loads[int(value_id)] < 0 {
						return memory_agg_upstream('aggregate opcode load requires M1b sidecar')
					}
				} else {
					_ = memory_agg_require_scalar_type(m, 'LOAD ${value_id} result',
						instruction.typ)!
				}
				source := memory_agg_get_value(m, instruction.operands[0])!
				_ = memory_agg_require_pointer_type(m, 'LOAD ${value_id} pointer',
					source.typ)!
			}
			.store {
				if instruction.operands.len != 2 {
					return memory_agg_malformed('STORE ${value_id} has ${instruction.operands.len} operands')
				}
				if instruction.typ != ssa.TypeID(0) {
					return memory_agg_malformed('STORE ${value_id} result must be canonical void')
				}
				stored := memory_agg_get_value(m, instruction.operands[0])!
				stored_type := memory_agg_get_type(m, stored.typ)!
				if stored_type.kind == .struct_t {
					if aggregate_index.stores[int(value_id)] < 0 {
						return memory_agg_upstream('aggregate opcode store requires M1b sidecar')
					}
				} else {
					_ = memory_agg_require_scalar_type(m, 'STORE ${value_id} scalar',
						stored.typ)!
				}
				pointer := memory_agg_get_value(m, instruction.operands[1])!
				_ = memory_agg_require_pointer_type(m, 'STORE ${value_id} pointer',
					pointer.typ)!
			}
			.heap_alloc, .fence, .cmpxchg, .atomicrmw {
				return memory_agg_upstream('runtime or atomic memory opcode ${instruction.op} is outside M1a')
			}
			.struct_init {
				if aggregate_index.constructs[int(value_id)] < 0 {
					return memory_agg_upstream('aggregate opcode ${instruction.op} requires M1b sidecar')
				}
			}
			.extractvalue {
				if aggregate_index.extracts[int(value_id)] < 0 {
					return memory_agg_upstream('aggregate opcode ${instruction.op} requires M1b sidecar')
				}
				if instruction.operands.len != 1 {
					return memory_agg_malformed('EXTRACTVALUE ${value_id} has ${instruction.operands.len} operands')
				}
			}
			.insertvalue {
				if aggregate_index.inserts[int(value_id)] < 0 {
					return memory_agg_upstream('aggregate opcode ${instruction.op} requires M1b sidecar')
				}
				if instruction.operands.len != 2 {
					return memory_agg_malformed('INSERTVALUE ${value_id} has ${instruction.operands.len} operands')
				}
			}
			.inline_string_init {
				return memory_agg_upstream('aggregate opcode ${instruction.op} requires M1b')
			}
			.call_indirect, .call_sret {
				return memory_agg_upstream('indirect or aggregate call opcode ${instruction.op} is outside M1a')
			}
			.phi, .select, .assign {
				if memory_agg_instruction_has_pointer(m, &instruction)! {
					return memory_agg_upstream('pointer ${instruction.op} ${value_id} is outside static provenance')
				}
				memory_agg_validate_scalar_or_void_instruction(m, value_id, &instruction)!
			}
			.call, .ret {
				if memory_agg_instruction_has_pointer(m, &instruction)! {
					return memory_agg_upstream('pointer escape through ${instruction.op} ${value_id}')
				}
				memory_agg_validate_scalar_or_void_instruction(m, value_id, &instruction)!
			}
			else {
				memory_agg_validate_scalar_or_void_instruction(m, value_id, &instruction)!
			}
		}
	}
}

fn memory_agg_validate_fact_anchor(structure &MemoryAggStructure, anchor MemoryAggInstructionAnchor, fact_kind string) !MemoryAggInstructionAnchor {
	value_index := int(anchor.instruction_value_id)
	expected := structure.anchors[value_index] or {
		return memory_agg_stale('${fact_kind} anchor value ${value_index} is not active')
	}
	if !memory_agg_anchor_equal(anchor, expected) {
		return memory_agg_stale('${fact_kind} anchor value ${value_index} does not match declaration coordinates')
	}
	return expected
}

fn memory_agg_index_anchored_facts(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure) !([]int, []int) {
	mut locals_by_value := []int{len: m.values.len, init: -1}
	for fact_index, local in facts.static_locals {
		anchor := memory_agg_validate_fact_anchor(structure, local.anchor, 'static local ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		if locals_by_value[value_index] >= 0 {
			return memory_agg_duplicate('static local ${fact_index} duplicates fact ${locals_by_value[value_index]}')
		}
		instruction := m.instrs[anchor.instruction_index]
		if instruction.op != .alloca {
			return memory_agg_orphan('static local ${fact_index} does not attest an ALLOCA')
		}
		locals_by_value[value_index] = fact_index
	}
	mut accesses_by_value := []int{len: m.values.len, init: -1}
	for fact_index, access in facts.accesses {
		anchor := memory_agg_validate_fact_anchor(structure, access.anchor, 'access ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		if accesses_by_value[value_index] >= 0 {
			return memory_agg_duplicate('access ${fact_index} duplicates fact ${accesses_by_value[value_index]}')
		}
		instruction := m.instrs[anchor.instruction_index]
		if instruction.op !in [ssa.OpCode.load, .store] {
			return memory_agg_orphan('access ${fact_index} does not attest a LOAD or STORE')
		}
		accesses_by_value[value_index] = fact_index
	}
	return locals_by_value, accesses_by_value
}

fn memory_agg_claim_aggregate_anchor(mut claimed map[int]string, value_index int, label string) ! {
	if previous := claimed[value_index] {
		return memory_agg_duplicate('${label} duplicates ${previous} at value ${value_index}')
	}
	claimed[value_index] = label
}

fn memory_agg_index_aggregate_facts(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure) !MemoryAggAggregateFactIndex {
	mut claimed := map[int]string{}
	for index, local in facts.static_locals {
		claimed[int(local.anchor.instruction_value_id)] = 'static local ${index}'
	}
	for index, access in facts.accesses {
		claimed[int(access.anchor.instruction_value_id)] = 'scalar access ${index}'
	}
	mut result := MemoryAggAggregateFactIndex{
		allocas:        []int{len: m.values.len, init: -1}
		field_pointers: []int{len: m.values.len, init: -1}
		constructs:     []int{len: m.values.len, init: -1}
		loads:          []int{len: m.values.len, init: -1}
		stores:         []int{len: m.values.len, init: -1}
		extracts:       []int{len: m.values.len, init: -1}
		inserts:        []int{len: m.values.len, init: -1}
	}
	for fact_index, fact in facts.aggregate_allocas {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate alloca ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate alloca ${fact_index}')!
		if fact.pointer_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate alloca ${fact_index} pointer value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op != .alloca {
			return memory_agg_orphan('aggregate alloca ${fact_index} does not attest an ALLOCA')
		}
		result.allocas[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_field_pointers {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate field pointer ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate field pointer ${fact_index}')!
		if fact.result_pointer_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate field pointer ${fact_index} result value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op !in [ssa.OpCode.get_element_ptr, .add] {
			return memory_agg_orphan('aggregate field pointer ${fact_index} does not attest a GEP or pointer ADD')
		}
		result.field_pointers[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_constructs {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate construct ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate construct ${fact_index}')!
		if fact.result_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate construct ${fact_index} result value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op != .struct_init {
			return memory_agg_orphan('aggregate construct ${fact_index} does not attest STRUCT_INIT')
		}
		result.constructs[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_loads {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate load ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate load ${fact_index}')!
		if fact.result_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate load ${fact_index} result value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op != .load {
			return memory_agg_orphan('aggregate load ${fact_index} does not attest LOAD')
		}
		result.loads[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_stores {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate store ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate store ${fact_index}')!
		if m.instrs[anchor.instruction_index].op != .store {
			return memory_agg_orphan('aggregate store ${fact_index} does not attest STORE')
		}
		result.stores[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_extracts {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate extract ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate extract ${fact_index}')!
		if fact.result_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate extract ${fact_index} result value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op != .extractvalue {
			return memory_agg_orphan('aggregate extract ${fact_index} does not attest EXTRACTVALUE')
		}
		result.extracts[value_index] = fact_index
	}
	for fact_index, fact in facts.aggregate_inserts {
		anchor := memory_agg_validate_fact_anchor(structure, fact.anchor,
			'aggregate insert ${fact_index}')!
		value_index := int(anchor.instruction_value_id)
		memory_agg_claim_aggregate_anchor(mut claimed, value_index,
			'aggregate insert ${fact_index}')!
		if fact.result_value_id != anchor.instruction_value_id {
			return memory_agg_stale('aggregate insert ${fact_index} result value disagrees with its anchor')
		}
		if m.instrs[anchor.instruction_index].op != .insertvalue {
			return memory_agg_orphan('aggregate insert ${fact_index} does not attest INSERTVALUE')
		}
		result.inserts[value_index] = fact_index
	}
	return result
}

fn memory_agg_expected_scalar_storage(width int) !(u8, u8) {
	match width {
		1, 8 {
			return u8(1), u8(1)
		}
		16 {
			return u8(2), u8(2)
		}
		32 {
			return u8(4), u8(4)
		}
		64 {
			return u8(8), u8(8)
		}
		else {
			return memory_agg_upstream('scalar width ${width} is outside M1a')
		}
	}
}

fn memory_agg_snapshot_scalar_layouts(m &ssa.Module, facts &MemoryAggFunctionFacts) !(map[int]int, []MemoryAggScalarLayoutBinding, []int) {
	mut index_by_type := map[int]int{}
	mut snapshot := []MemoryAggScalarLayoutBinding{cap: facts.scalar_layouts.len}
	mut use_counts := []int{len: facts.scalar_layouts.len}
	for binding_index, binding in facts.scalar_layouts {
		if binding.profile != facts.profile {
			return memory_agg_stale('scalar layout ${binding_index} has wrong profile')
		}
		if binding.authority != .native_plain {
			return memory_agg_upstream('scalar layout ${binding_index} is not native_plain')
		}
		type_index := int(binding.type_id)
		if type_index in index_by_type {
			return memory_agg_duplicate('scalar layout ${binding_index} duplicates type ${type_index}')
		}
		typ := memory_agg_require_scalar_type(m, 'scalar layout ${binding_index}',
			binding.type_id)!
		expected_storage, expected_alignment := memory_agg_expected_scalar_storage(typ.width)!
		if binding.semantic_width_bits != typ.width
			|| binding.semantic_is_unsigned != typ.is_unsigned
			|| binding.storage_width_bytes != expected_storage
			|| binding.alignment_bytes != expected_alignment {
			return memory_agg_stale('scalar layout ${binding_index} disagrees with its native scalar type')
		}
		index_by_type[type_index] = binding_index
		snapshot << MemoryAggScalarLayoutBinding{
			profile:              binding.profile
			type_id:              binding.type_id
			authority:            binding.authority
			semantic_width_bits:  binding.semantic_width_bits
			semantic_is_unsigned: binding.semantic_is_unsigned
			storage_width_bytes:  binding.storage_width_bytes
			alignment_bytes:      binding.alignment_bytes
		}
	}
	snapshot.sort(a.type_id < b.type_id)
	return index_by_type, snapshot, use_counts
}

fn memory_agg_snapshot_constants(m &ssa.Module, facts &MemoryAggFunctionFacts) !MemoryAggConstantTable {
	mut index_by_value := map[int]int{}
	mut canonical_bits := []u64{len: facts.scalar_constants.len}
	mut use_counts := []int{len: facts.scalar_constants.len}
	for binding_index, binding in facts.scalar_constants {
		value_index := int(binding.value_id)
		if value_index in index_by_value {
			return memory_agg_duplicate('scalar constant ${binding_index} duplicates value ${value_index}')
		}
		value := memory_agg_get_value(m, binding.value_id)!
		if value.kind != .constant {
			return memory_agg_stale('scalar constant ${binding_index} value ${value_index} is not constant')
		}
		if binding.type_id != value.typ {
			return memory_agg_stale('scalar constant ${binding_index} type does not match value ${value_index}')
		}
		typ := memory_agg_require_scalar_type(m, 'scalar constant ${binding_index}',
			binding.type_id)!
		canonical_bits[binding_index] = canonical_scalar_constant_bits('memory agg scalar constant ${binding_index}',
			typ, binding.raw_bits) or { return memory_agg_malformed(err.msg()) }
		index_by_value[value_index] = binding_index
	}
	return MemoryAggConstantTable{
		index_by_value: index_by_value
		canonical_bits: canonical_bits
		use_counts:     use_counts
	}
}

struct MemoryAggSlotPlan {
	slots               []MemoryAggSlotBinding
	aggregate_slots     []MemoryAggAggregateSlotBinding
	aggregate_snapshots []MemoryAggAggregateSnapshot
	by_alloca           map[int]MemoryAggSlotBinding
	aggregate_by_alloca map[int]MemoryAggAggregateSlotBinding
	request_by_id       map[int]MemorySlotRequest
	snapshot_by_value   map[int]MemoryAggAggregateSnapshot
	total_bytes         u64
}

struct MemoryAggAggregateSlotCandidate {
	definition        MemoryAggInstructionAnchor
	owner_value_id    ssa.ValueID
	aggregate_type_id ssa.TypeID
	role              MemoryAggAggregateSlotRole
	purpose           MemoryAggTempPurpose
	size_bytes        u64
	alignment_bytes   u64
}

fn memory_agg_require_layout(type_id ssa.TypeID, facts &MemoryAggFunctionFacts, index_by_type map[int]int, mut use_counts []int, context string) !MemoryAggScalarLayoutBinding {
	binding_index := index_by_type[int(type_id)] or {
		return memory_agg_incomplete('${context} is missing scalar layout for type ${type_id}')
	}
	use_counts[binding_index]++
	return facts.scalar_layouts[binding_index]
}

fn memory_agg_is_power_of_two(value u64) bool {
	return value > 0 && value & (value - 1) == 0
}

fn memory_agg_snapshot_aggregate_layouts(m &ssa.Module, facts &MemoryAggFunctionFacts, scalar_layout_index map[int]int, mut scalar_layout_use_counts []int) !MemoryAggAggregateLayoutTable {
	mut index_by_type := map[int]int{}
	mut layouts := []MemoryAggAggregateLayoutBinding{cap: facts.aggregate_layouts.len}
	mut use_counts := []int{len: facts.aggregate_layouts.len}
	for layout_index, layout in facts.aggregate_layouts {
		if layout.profile != facts.profile {
			return memory_agg_stale('aggregate layout ${layout_index} has wrong profile')
		}
		if layout.authority != .native_plain {
			return memory_agg_upstream('aggregate layout ${layout_index} is not native_plain')
		}
		type_index := int(layout.type_id)
		if type_index in index_by_type {
			return memory_agg_duplicate('aggregate layout ${layout_index} duplicates type ${type_index}')
		}
		typ := memory_agg_get_type(m, layout.type_id)!
		if typ.kind != .struct_t {
			return memory_agg_upstream('aggregate layout ${layout_index} is not a struct type')
		}
		if typ.width != 0 || typ.is_unsigned || typ.elem_type != ssa.TypeID(0)
			|| typ.len != 0 || typ.params.len != 0 || typ.ret_type != ssa.TypeID(0) {
			return memory_agg_malformed('aggregate layout ${layout_index} has noncanonical struct payload')
		}
		if typ.is_c_struct || typ.is_union {
			return memory_agg_upstream('aggregate layout ${layout_index} is C or union storage')
		}
		if typ.fields.len == 0 {
			return memory_agg_upstream('aggregate layout ${layout_index} is empty')
		}
		if typ.field_names.len != 0 && typ.field_names.len != typ.fields.len {
			return memory_agg_malformed('aggregate layout ${layout_index} has inconsistent field names')
		}
		if layout.fields.len != typ.fields.len {
			return memory_agg_stale('aggregate layout ${layout_index} field count disagrees with its type')
		}
		if layout.size_bytes == 0
			|| layout.size_bytes > memory_agg_max_aggregate_object_bytes {
			return memory_agg_malformed('aggregate layout ${layout_index} size ${layout.size_bytes} is outside 1..${memory_agg_max_aggregate_object_bytes}')
		}
		if !memory_agg_is_power_of_two(layout.alignment_bytes)
			|| layout.alignment_bytes > 16 {
			return memory_agg_malformed('aggregate layout ${layout_index} alignment must be a power of two at most 16')
		}

		mut cursor := u64(0)
		mut maximum_alignment := u64(1)
		mut expected_padding := []MemoryAggByteRange{}
		mut fields_snapshot := []MemoryAggAggregateFieldLayout{cap: layout.fields.len}
		for field_index, field in layout.fields {
			if int(field.index) != field_index {
				return memory_agg_stale('aggregate layout ${layout_index} field index ${field.index} is not ${field_index}')
			}
			if field.type_id != typ.fields[field_index] {
				return memory_agg_stale('aggregate layout ${layout_index} field ${field_index} type disagrees')
			}
			if memory_agg_get_type(m, field.type_id)!.kind != .int_t {
				return memory_agg_upstream('aggregate layout ${layout_index} field ${field_index} is not a scalar integer')
			}
			scalar := memory_agg_require_layout(field.type_id, facts, scalar_layout_index,
				mut scalar_layout_use_counts, 'aggregate layout ${layout_index} field ${field_index}')!
			if field.size_bytes != u64(scalar.storage_width_bytes)
				|| field.alignment_bytes != u64(scalar.alignment_bytes) {
				return memory_agg_stale('aggregate layout ${layout_index} field ${field_index} size/alignment disagrees')
			}
			if field.offset_bytes < cursor {
				return memory_agg_malformed('aggregate layout ${layout_index} fields overlap')
			}
			if field.offset_bytes % field.alignment_bytes != 0 {
				return memory_agg_malformed('aggregate layout ${layout_index} field ${field_index} is misaligned')
			}
			if field.offset_bytes > cursor {
				expected_padding << MemoryAggByteRange{
					offset_bytes: cursor
					size_bytes:   field.offset_bytes - cursor
				}
			}
			field_end := memory_agg_checked_add(field.offset_bytes, field.size_bytes)!
			if field_end > layout.size_bytes {
				return memory_agg_malformed('aggregate layout ${layout_index} field ${field_index} exceeds object size')
			}
			cursor = field_end
			if field.alignment_bytes > maximum_alignment {
				maximum_alignment = field.alignment_bytes
			}
			fields_snapshot << MemoryAggAggregateFieldLayout{
				index:           field.index
				type_id:         field.type_id
				offset_bytes:    field.offset_bytes
				size_bytes:      field.size_bytes
				alignment_bytes: field.alignment_bytes
			}
		}
		if cursor < layout.size_bytes {
			expected_padding << MemoryAggByteRange{
				offset_bytes: cursor
				size_bytes:   layout.size_bytes - cursor
			}
		}
		if layout.alignment_bytes != maximum_alignment
			|| layout.size_bytes % layout.alignment_bytes != 0 {
			return memory_agg_stale('aggregate layout ${layout_index} aggregate size/alignment disagrees')
		}
		if layout.padding.len != expected_padding.len {
			return memory_agg_stale('aggregate layout ${layout_index} padding is not the maximal complement')
		}
		mut padding_snapshot := []MemoryAggByteRange{cap: layout.padding.len}
		for padding_index, padding in layout.padding {
			expected := expected_padding[padding_index]
			if padding.size_bytes == 0 || padding != expected {
				return memory_agg_stale('aggregate layout ${layout_index} padding ${padding_index} disagrees')
			}
			padding_snapshot << MemoryAggByteRange{
				offset_bytes: padding.offset_bytes
				size_bytes:   padding.size_bytes
			}
		}
		index_by_type[type_index] = layout_index
		layouts << MemoryAggAggregateLayoutBinding{
			profile:         layout.profile
			authority:       layout.authority
			type_id:         layout.type_id
			size_bytes:      layout.size_bytes
			alignment_bytes: layout.alignment_bytes
			fields:          fields_snapshot
			padding:         padding_snapshot
		}
	}
	layouts.sort(a.type_id < b.type_id)
	return MemoryAggAggregateLayoutTable{
		index_by_type: index_by_type
		layouts:       layouts
		use_counts:    use_counts
	}
}

fn memory_agg_require_aggregate_layout(type_id ssa.TypeID, facts &MemoryAggFunctionFacts, table &MemoryAggAggregateLayoutTable, mut use_counts []int, context string) !MemoryAggAggregateLayoutBinding {
	binding_index := table.index_by_type[int(type_id)] or {
		return memory_agg_incomplete('${context} is missing authoritative aggregate layout for type ${type_id}')
	}
	use_counts[binding_index]++
	return facts.aggregate_layouts[binding_index]
}

fn memory_agg_require_signed_i64_constant(m &ssa.Module, value_id ssa.ValueID, mut constants MemoryAggConstantTable, context string) !u64 {
	binding_index := constants.index_by_value[int(value_id)] or {
		return memory_agg_incomplete('${context} is missing ScalarConstantBinding for value ${value_id}')
	}
	value := memory_agg_get_value(m, value_id)!
	typ := memory_agg_require_scalar_type(m, context, value.typ)!
	if typ.width != 64 || typ.is_unsigned {
		return memory_agg_stale('${context} must be a signed i64 constant')
	}
	constants.use_counts[binding_index]++
	return constants.canonical_bits[binding_index]
}

fn memory_agg_snapshot_slots(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, locals_by_value []int, layout_index map[int]int, mut layout_use_counts []int, mut constants MemoryAggConstantTable) !MemoryAggSlotPlan {
	mut slots := []MemoryAggSlotBinding{}
	mut by_alloca := map[int]MemoryAggSlotBinding{}
	mut request_by_id := map[int]MemorySlotRequest{}
	mut total_bytes := u64(0)
	for value_id in structure.active_values {
		value := m.values[int(value_id)]
		instruction := m.instrs[value.index]
		if instruction.op != .alloca {
			continue
		}
		pointer_type := memory_agg_require_pointer_type(m, 'ALLOCA ${value_id}',
			instruction.typ)!
		if memory_agg_get_type(m, pointer_type.elem_type)!.kind == .struct_t {
			continue
		}
		fact_index := locals_by_value[int(value_id)]
		if fact_index < 0 {
			return memory_agg_incomplete('ALLOCA ${value_id} is missing its static-local fact')
		}
		fact := facts.static_locals[fact_index]
		if fact.profile != facts.profile {
			return memory_agg_stale('static local ${fact_index} has wrong profile')
		}
		if fact.authority != .native_plain {
			return memory_agg_upstream('static local ${fact_index} is not native_plain')
		}
		if fact.element_type != pointer_type.elem_type {
			return memory_agg_stale('static local ${fact_index} element type disagrees with ALLOCA ${value_id}')
		}
		element_layout := memory_agg_require_layout(fact.element_type, facts, layout_index,
			mut layout_use_counts, 'ALLOCA ${value_id} element')!
		mut element_count := u64(1)
		if instruction.operands.len == 0 {
			if fact.form != .one || fact.count_value_id != ssa.ValueID(0) {
				return memory_agg_stale('zero-operand ALLOCA ${value_id} requires form one and absent count')
			}
		} else {
			count_id := instruction.operands[0]
			if fact.form != .constant_count || fact.count_value_id != count_id {
				return memory_agg_stale('counted ALLOCA ${value_id} has inconsistent count fact')
			}
			count_value := memory_agg_get_value(m, count_id)!
			if count_value.kind != .constant {
				return memory_agg_upstream('ALLOCA ${value_id} count is dynamic')
			}
			_ = memory_agg_require_layout(count_value.typ, facts, layout_index,
				mut layout_use_counts, 'ALLOCA ${value_id} count')!
			count_bits := memory_agg_require_signed_i64_constant(m, count_id, mut constants,
				'ALLOCA ${value_id} count')!
			if count_bits == 0 || count_bits > u64(0x7fffffffffffffff) {
				return memory_agg_malformed('ALLOCA ${value_id} count must be positive signed i64')
			}
			element_count = count_bits
		}
		size_bytes := memory_agg_checked_mul(element_count,
			u64(element_layout.storage_width_bytes))!
		if size_bytes == 0 {
			return memory_agg_malformed('ALLOCA ${value_id} has zero occupied bytes')
		}
		total_bytes = memory_agg_checked_add(total_bytes, size_bytes)!
		if total_bytes > memory_agg_max_requested_bytes {
			return memory_agg_malformed('total slot bytes ${total_bytes} exceed ${memory_agg_max_requested_bytes}')
		}
		if int(value_id) <= 0 || u64(value_id) >= memory_agg_temp_id_base {
			return memory_agg_malformed('ALLOCA ${value_id} cannot form a stable u32 M0 request id')
		}
		request_id := u32(value_id)
		if int(request_id) in request_by_id {
			return memory_agg_duplicate('M0 request id ${request_id} collides')
		}
		slot := MemoryAggSlotBinding{
			definition:      structure.anchors[int(value_id)]
			alloca_value_id: value_id
			element_type:    fact.element_type
			element_count:   element_count
			request:         MemorySlotRequest{
				id:              request_id
				kind:            .fixed_alloca
				size_bytes:      size_bytes
				alignment_bytes: u64(element_layout.alignment_bytes)
			}
		}
		slots << slot
		by_alloca[int(value_id)] = slot
		request_by_id[int(request_id)] = slot.request
	}
	memory_agg_validate_count('slot count', slots.len, memory_agg_max_static_locals)!
	slots.sort(a.request.id < b.request.id)
	return MemoryAggSlotPlan{
		slots:         slots
		by_alloca:     by_alloca
		request_by_id: request_by_id
		total_bytes:   total_bytes
	}
}

fn memory_agg_aggregate_candidate_less(left MemoryAggAggregateSlotCandidate, right MemoryAggAggregateSlotCandidate) bool {
	if left.definition.function_index != right.definition.function_index {
		return left.definition.function_index < right.definition.function_index
	}
	if left.definition.block_ordinal != right.definition.block_ordinal {
		return left.definition.block_ordinal < right.definition.block_ordinal
	}
	if left.definition.instruction_ordinal != right.definition.instruction_ordinal {
		return left.definition.instruction_ordinal < right.definition.instruction_ordinal
	}
	if left.purpose != right.purpose {
		return int(left.purpose) < int(right.purpose)
	}
	return left.owner_value_id < right.owner_value_id
}

fn memory_agg_sort_aggregate_candidates(mut candidates []MemoryAggAggregateSlotCandidate) {
	for index := 1; index < candidates.len; index++ {
		mut cursor := index
		for cursor > 0
			&& memory_agg_aggregate_candidate_less(candidates[cursor], candidates[cursor - 1]) {
			previous := candidates[cursor - 1]
			candidates[cursor - 1] = candidates[cursor]
			candidates[cursor] = previous
			cursor--
		}
	}
}

fn memory_agg_assign_temp_ids(candidates []MemoryAggAggregateSlotCandidate) !(map[int]u32, map[int]u16) {
	memory_agg_validate_count('aggregate temp count', candidates.len,
		memory_agg_max_aggregate_temps)!
	mut seen_owners := map[int]bool{}
	for candidate in candidates {
		if candidate.role != .aggregate_temp {
			return memory_agg_malformed('non-temp candidate reached temp-id assignment')
		}
		owner := int(candidate.owner_value_id)
		if owner in seen_owners {
			return memory_agg_duplicate('aggregate temp owner ${owner} is duplicated')
		}
		seen_owners[owner] = true
	}
	mut ordered := candidates.clone()
	memory_agg_sort_aggregate_candidates(mut ordered)
	mut ids := map[int]u32{}
	mut ordinals := map[int]u16{}
	mut previous_anchor := MemoryAggInstructionAnchor{}
	mut have_previous_anchor := false
	mut anchor_ordinal := 0
	for rank, candidate in ordered {
		rank_value := u64(rank)
		if rank_value > u64(max_u32) - memory_agg_temp_id_base {
			return memory_agg_malformed('aggregate temp rank overflows u32 request id')
		}
		if have_previous_anchor && memory_agg_anchor_equal(previous_anchor,
			candidate.definition) {
			anchor_ordinal++
		} else {
			anchor_ordinal = 0
		}
		if anchor_ordinal > int(max_u16) {
			return memory_agg_malformed('aggregate temp anchor-instance ordinal overflows u16')
		}
		ids[int(candidate.owner_value_id)] = u32(memory_agg_temp_id_base + rank_value)
		ordinals[int(candidate.owner_value_id)] = u16(anchor_ordinal)
		previous_anchor = candidate.definition
		have_previous_anchor = true
	}
	return ids, ordinals
}

fn memory_agg_append_aggregate_candidate(mut candidates []MemoryAggAggregateSlotCandidate, mut owners map[int]string, candidate MemoryAggAggregateSlotCandidate, label string) ! {
	owner := int(candidate.owner_value_id)
	if previous := owners[owner] {
		return memory_agg_duplicate('${label} owner ${owner} duplicates ${previous}')
	}
	owners[owner] = label
	candidates << candidate
}

fn memory_agg_snapshot_aggregate_slots(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, aggregate_index &MemoryAggAggregateFactIndex, aggregate_layout_table &MemoryAggAggregateLayoutTable, mut aggregate_layout_use_counts []int, scalar_plan MemoryAggSlotPlan) !MemoryAggSlotPlan {
	mut candidate_count := facts.aggregate_allocas.len
	for count in [facts.aggregate_constructs.len, facts.aggregate_loads.len,
		facts.aggregate_inserts.len] {
		if candidate_count > memory_agg_max_static_locals - count {
			return memory_agg_malformed('merged slot count exceeds ${memory_agg_max_static_locals}')
		}
		candidate_count += count
	}
	if scalar_plan.slots.len > memory_agg_max_static_locals - candidate_count {
		return memory_agg_malformed('merged slot count exceeds ${memory_agg_max_static_locals}')
	}

	mut candidates := []MemoryAggAggregateSlotCandidate{cap: candidate_count}
	mut owners := map[int]string{}
	for scalar_slot in scalar_plan.slots {
		owners[int(scalar_slot.alloca_value_id)] = 'scalar fixed alloca'
	}
	for fact_index, fact in facts.aggregate_allocas {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate alloca ${fact_index} has wrong profile')
		}
		if fact.authority != .native_plain {
			return memory_agg_upstream('aggregate alloca ${fact_index} is not native_plain')
		}
		anchor := structure.anchors[int(fact.pointer_value_id)] or {
			return memory_agg_stale('aggregate alloca ${fact_index} owner is not active')
		}
		if aggregate_index.allocas[int(fact.pointer_value_id)] != fact_index {
			return memory_agg_stale('aggregate alloca ${fact_index} is not the indexed owner')
		}
		instruction := m.instrs[anchor.instruction_index]
		if instruction.operands.len != 0 {
			return memory_agg_upstream('aggregate ALLOCA ${fact.pointer_value_id} count form is outside M1b')
		}
		pointer_type := memory_agg_require_pointer_type(m,
			'aggregate ALLOCA ${fact.pointer_value_id}', instruction.typ)!
		if pointer_type.elem_type != fact.aggregate_type_id {
			return memory_agg_stale('aggregate alloca ${fact_index} element type disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate alloca ${fact.pointer_value_id}')!
		memory_agg_append_aggregate_candidate(mut candidates, mut owners,
			MemoryAggAggregateSlotCandidate{
				definition:        anchor
				owner_value_id:    fact.pointer_value_id
				aggregate_type_id: fact.aggregate_type_id
				role:              fact.role
				purpose:           .explicit_temp
				size_bytes:        layout.size_bytes
				alignment_bytes:   layout.alignment_bytes
			}, 'aggregate alloca ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_constructs {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate construct ${fact_index} has wrong profile')
		}
		value := memory_agg_get_value(m, fact.result_value_id)!
		if value.typ != fact.aggregate_type_id {
			return memory_agg_stale('aggregate construct ${fact_index} result type disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate construct ${fact.result_value_id}')!
		memory_agg_append_aggregate_candidate(mut candidates, mut owners,
			MemoryAggAggregateSlotCandidate{
				definition:        fact.anchor
				owner_value_id:    fact.result_value_id
				aggregate_type_id: fact.aggregate_type_id
				role:              .aggregate_temp
				purpose:           .construct_result
				size_bytes:        layout.size_bytes
				alignment_bytes:   layout.alignment_bytes
			}, 'aggregate construct ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_loads {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate load ${fact_index} has wrong profile')
		}
		value := memory_agg_get_value(m, fact.result_value_id)!
		if value.typ != fact.aggregate_type_id {
			return memory_agg_stale('aggregate load ${fact_index} result type disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate load ${fact.result_value_id}')!
		memory_agg_append_aggregate_candidate(mut candidates, mut owners,
			MemoryAggAggregateSlotCandidate{
				definition:        fact.anchor
				owner_value_id:    fact.result_value_id
				aggregate_type_id: fact.aggregate_type_id
				role:              .aggregate_temp
				purpose:           .load_result
				size_bytes:        layout.size_bytes
				alignment_bytes:   layout.alignment_bytes
			}, 'aggregate load ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_inserts {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate insert ${fact_index} has wrong profile')
		}
		value := memory_agg_get_value(m, fact.result_value_id)!
		if value.typ != fact.aggregate_type_id {
			return memory_agg_stale('aggregate insert ${fact_index} result type disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate insert ${fact.result_value_id}')!
		memory_agg_append_aggregate_candidate(mut candidates, mut owners,
			MemoryAggAggregateSlotCandidate{
				definition:        fact.anchor
				owner_value_id:    fact.result_value_id
				aggregate_type_id: fact.aggregate_type_id
				role:              .aggregate_temp
				purpose:           .insert_result
				size_bytes:        layout.size_bytes
				alignment_bytes:   layout.alignment_bytes
			}, 'aggregate insert ${fact_index}')!
	}
	if candidates.len != candidate_count {
		return memory_agg_malformed('internal aggregate slot candidate count failed')
	}

	mut total_bytes := scalar_plan.total_bytes
	mut temp_count := 0
	for candidate in candidates {
		total_bytes = memory_agg_checked_add(total_bytes, candidate.size_bytes)!
		if total_bytes > memory_agg_max_requested_bytes {
			return memory_agg_malformed('total slot bytes ${total_bytes} exceed ${memory_agg_max_requested_bytes}')
		}
		if candidate.role == .aggregate_temp {
			temp_count++
		}
	}
	memory_agg_validate_count('aggregate temp count', temp_count,
		memory_agg_max_aggregate_temps)!

	mut temp_candidates := []MemoryAggAggregateSlotCandidate{cap: temp_count}
	for candidate in candidates {
		if candidate.role == .aggregate_temp {
			temp_candidates << candidate
		}
	}
	temp_id_by_owner, temp_ordinal_by_owner := memory_agg_assign_temp_ids(temp_candidates)!

	mut aggregate_slots := []MemoryAggAggregateSlotBinding{cap: candidates.len}
	mut aggregate_snapshots := []MemoryAggAggregateSnapshot{cap: temp_count}
	mut aggregate_by_alloca := map[int]MemoryAggAggregateSlotBinding{}
	mut snapshot_by_value := map[int]MemoryAggAggregateSnapshot{}
	mut request_by_id := scalar_plan.request_by_id.clone()
	for candidate in candidates {
		mut request_id := u32(0)
		mut instance_ordinal := u16(0)
		if candidate.role == .fixed_alloca {
			if candidate.owner_value_id <= 0
				|| u64(candidate.owner_value_id) >= memory_agg_temp_id_base {
				return memory_agg_malformed('aggregate fixed ALLOCA ${candidate.owner_value_id} cannot form a stable request id')
			}
			request_id = u32(candidate.owner_value_id)
		} else {
			request_id = temp_id_by_owner[int(candidate.owner_value_id)] or {
				return memory_agg_malformed('internal aggregate temp id is missing')
			}
			instance_ordinal = temp_ordinal_by_owner[int(candidate.owner_value_id)] or {
				return memory_agg_malformed('internal aggregate temp ordinal is missing')
			}
		}
		if int(request_id) in request_by_id {
			return memory_agg_duplicate('M0 request id ${request_id} collides')
		}
		request := MemorySlotRequest{
			id:              request_id
			kind:            if candidate.role == .fixed_alloca {
				MemorySlotKind.fixed_alloca
			} else {
				.aggregate_temp
			}
			size_bytes:      candidate.size_bytes
			alignment_bytes: candidate.alignment_bytes
		}
		slot := MemoryAggAggregateSlotBinding{
			definition:              candidate.definition
			owner_value_id:          candidate.owner_value_id
			aggregate_type_id:       candidate.aggregate_type_id
			role:                    candidate.role
			purpose:                 candidate.purpose
			anchor_instance_ordinal: instance_ordinal
			request:                 request
		}
		aggregate_slots << slot
		request_by_id[int(request_id)] = request
		if candidate.purpose == .explicit_temp {
			aggregate_by_alloca[int(candidate.owner_value_id)] = slot
		} else {
			snapshot := MemoryAggAggregateSnapshot{
				definition:        candidate.definition
				value_id:          candidate.owner_value_id
				aggregate_type_id: candidate.aggregate_type_id
				root_slot_id:      request_id
				publish_phase:     4
			}
			aggregate_snapshots << snapshot
			snapshot_by_value[int(candidate.owner_value_id)] = snapshot
		}
	}
	aggregate_slots.sort(a.request.id < b.request.id)
	aggregate_snapshots.sort(a.value_id < b.value_id)
	return MemoryAggSlotPlan{
		slots:               scalar_plan.slots
		aggregate_slots:     aggregate_slots
		aggregate_snapshots: aggregate_snapshots
		by_alloca:           scalar_plan.by_alloca
		aggregate_by_alloca: aggregate_by_alloca
		request_by_id:       request_by_id
		snapshot_by_value:   snapshot_by_value
		total_bytes:         total_bytes
	}
}

fn memory_agg_anchor_dominates(structure &MemoryAggStructure, definition MemoryAggInstructionAnchor, use MemoryAggInstructionAnchor) bool {
	if definition.block_ordinal == use.block_ordinal {
		return definition.instruction_ordinal < use.instruction_ordinal
	}
	definition_block := int(definition.block_ordinal)
	use_block := int(use.block_ordinal)
	word := definition_block / 64
	bit := definition_block % 64
	row := use_block * structure.dominator_words
	return structure.dominators[row + word] & (u64(1) << bit) != 0
}

fn memory_agg_apply_byte_delta(base_offset u64, delta_bits u64, root_size u64, context string) !u64 {
	if delta_bits & (u64(1) << 63) != 0 {
		magnitude := (~delta_bits) + 1
		if magnitude > base_offset {
			return memory_agg_malformed('${context} points before its local root')
		}
		return base_offset - magnitude
	}
	result := memory_agg_checked_add(base_offset, delta_bits)!
	if result > root_size {
		return memory_agg_malformed('${context} points beyond one-past its local root')
	}
	return result
}

fn memory_agg_resolve_pointer(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, slot_plan &MemoryAggSlotPlan, aggregate_index &MemoryAggAggregateFactIndex, layout_index map[int]int, mut layout_use_counts []int, aggregate_layout_table &MemoryAggAggregateLayoutTable, mut aggregate_layout_use_counts []int, mut constants MemoryAggConstantTable, value_id ssa.ValueID, traversal_depth int, mut states []u8, mut absolute_depths []u8, mut cache []MemoryAggPointerSnapshot) !MemoryAggPointerSnapshot {
	value_index := int(value_id)
	if value_index <= 0 || value_index >= m.values.len {
		return memory_agg_malformed('pointer value ${value_index} is outside the value table')
	}
	if states[value_index] == 1 {
		return memory_agg_malformed('pointer provenance cycle at value ${value_index}')
	}
	if traversal_depth > memory_agg_max_provenance_depth {
		return memory_agg_malformed('pointer provenance depth exceeds ${memory_agg_max_provenance_depth}')
	}
	if states[value_index] == 2 {
		return cache[value_index]
	}
	value := memory_agg_get_value(m, value_id)!
	if value.kind != .instruction {
		return memory_agg_upstream('pointer value ${value_index} has unknown non-local provenance')
	}
	anchor := structure.anchors[value_index] or {
		return memory_agg_malformed('pointer instruction value ${value_index} is not active in function ${facts.function_index}')
	}
	instruction := m.instrs[value.index]
	states[value_index] = 1
	mut snapshot := MemoryAggPointerSnapshot{}
	mut absolute_depth := u8(0)
	match instruction.op {
		.alloca {
			pointer_type := memory_agg_require_pointer_type(m, 'ALLOCA ${value_index}',
				instruction.typ)!
			if slot := slot_plan.by_alloca[value_index] {
				snapshot = MemoryAggPointerSnapshot{
					definition:      anchor
					value_id:        value_id
					origin:          .fixed_alloca
					pointee_type:    pointer_type.elem_type
					root_slot_id:    slot.request.id
					root_size_bytes: slot.request.size_bytes
					byte_offset:     0
					remaining_bytes: slot.request.size_bytes
					is_one_past:     false
				}
			} else if slot := slot_plan.aggregate_by_alloca[value_index] {
				snapshot = MemoryAggPointerSnapshot{
					definition:      anchor
					value_id:        value_id
					origin:          .aggregate_storage
					pointee_type:    pointer_type.elem_type
					root_slot_id:    slot.request.id
					root_size_bytes: slot.request.size_bytes
					byte_offset:     0
					remaining_bytes: slot.request.size_bytes
					is_one_past:     false
				}
			} else {
				return memory_agg_incomplete('ALLOCA ${value_index} has no slot request')
			}
		}
		.get_element_ptr, .add {
			source_id := instruction.operands[0]
			source := memory_agg_resolve_pointer(m, facts, structure, slot_plan,
				aggregate_index, layout_index, mut layout_use_counts,
				aggregate_layout_table, mut aggregate_layout_use_counts, mut constants,
				source_id, traversal_depth + 1, mut states, mut absolute_depths,
				mut cache)!
			if source.origin == .aggregate_field {
				return memory_agg_upstream('aggregate field pointer ${source_id} further ${instruction.op} derivation is outside M1b')
			}
			if int(absolute_depths[int(source_id)]) >= memory_agg_max_provenance_depth {
				return memory_agg_malformed('pointer provenance depth exceeds ${memory_agg_max_provenance_depth}')
			}
			absolute_depth = absolute_depths[int(source_id)] + 1
			if !memory_agg_anchor_dominates(structure, source.definition, anchor) {
				return memory_agg_malformed('pointer definition ${source_id} does not dominate use ${value_id}')
			}
			source_type := memory_agg_require_pointer_type(m, 'pointer source ${source_id}',
				m.values[int(source_id)].typ)!
			result_type := memory_agg_require_pointer_type(m, 'pointer result ${value_id}',
				instruction.typ)!
			delta_id := instruction.operands[1]
			delta_value := memory_agg_get_value(m, delta_id)!
			if delta_value.kind != .constant {
				return memory_agg_upstream('pointer ${value_id} byte delta is not constant')
			}
			_ = memory_agg_require_layout(delta_value.typ, facts, layout_index,
				mut layout_use_counts, 'pointer ${value_id} byte delta')!
			delta_bits := memory_agg_require_signed_i64_constant(m, delta_id, mut constants,
				'pointer ${value_id} byte delta')!
			source_pointee := memory_agg_get_type(m, source_type.elem_type)!
			if source_pointee.kind == .struct_t {
				fact_index := aggregate_index.field_pointers[value_index]
				if fact_index < 0 {
					return memory_agg_upstream('pointer ${value_id} is missing aggregate field-pointer authority')
				}
				fact := facts.aggregate_field_pointers[fact_index]
				if fact.profile != facts.profile {
					return memory_agg_stale('aggregate field pointer ${fact_index} has wrong profile')
				}
				if fact.source_pointer_value_id != source_id
					|| fact.result_pointer_value_id != value_id
					|| fact.aggregate_type_id != source_type.elem_type {
					return memory_agg_stale('aggregate field pointer ${fact_index} disagrees with pointer ${value_id}')
				}
				layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id,
					facts, aggregate_layout_table, mut aggregate_layout_use_counts,
					'aggregate field pointer ${value_id}')!
				if int(fact.field_index) >= layout.fields.len {
					return memory_agg_stale('aggregate field pointer ${fact_index} field index is outside its layout')
				}
				field := layout.fields[int(fact.field_index)]
				if result_type.elem_type != field.type_id {
					return memory_agg_stale('aggregate field pointer ${fact_index} result pointee disagrees')
				}
				if delta_bits != field.offset_bytes {
					return memory_agg_stale('aggregate field pointer ${fact_index} byte delta disagrees')
				}
				if source.is_one_past {
					return memory_agg_malformed('aggregate field pointer ${value_id} starts one-past storage')
				}
				offset := memory_agg_checked_add(source.byte_offset, field.offset_bytes)!
				end := memory_agg_checked_add(offset, field.size_bytes)!
				if end > source.root_size_bytes {
					return memory_agg_malformed('aggregate field pointer ${value_id} exceeds its local root')
				}
				snapshot = MemoryAggPointerSnapshot{
					definition:      anchor
					value_id:        value_id
					origin:          .aggregate_field
					pointee_type:    result_type.elem_type
					root_slot_id:    source.root_slot_id
					root_size_bytes: source.root_size_bytes
					byte_offset:     offset
					remaining_bytes: source.root_size_bytes - offset
					is_one_past:     false
				}
			} else {
				if instruction.typ != m.values[int(source_id)].typ
					|| result_type.elem_type != source_type.elem_type {
					return memory_agg_malformed('byte-delta pointer ${value_id} must preserve pointer type')
				}
				offset := memory_agg_apply_byte_delta(source.byte_offset, delta_bits,
					source.root_size_bytes, 'pointer ${value_id}')!
				snapshot = MemoryAggPointerSnapshot{
					definition:      anchor
					value_id:        value_id
					origin:          .byte_delta
					pointee_type:    result_type.elem_type
					root_slot_id:    source.root_slot_id
					root_size_bytes: source.root_size_bytes
					byte_offset:     offset
					remaining_bytes: source.root_size_bytes - offset
					is_one_past:     offset == source.root_size_bytes
				}
			}
		}
		.bitcast {
			source_id := instruction.operands[0]
			source := memory_agg_resolve_pointer(m, facts, structure, slot_plan,
				aggregate_index, layout_index, mut layout_use_counts,
				aggregate_layout_table, mut aggregate_layout_use_counts, mut constants,
				source_id, traversal_depth + 1, mut states, mut absolute_depths,
				mut cache)!
			if source.origin == .aggregate_field {
				return memory_agg_upstream('aggregate field pointer ${source_id} further BITCAST derivation is outside M1b')
			}
			if int(absolute_depths[int(source_id)]) >= memory_agg_max_provenance_depth {
				return memory_agg_malformed('pointer provenance depth exceeds ${memory_agg_max_provenance_depth}')
			}
			absolute_depth = absolute_depths[int(source_id)] + 1
			if !memory_agg_anchor_dominates(structure, source.definition, anchor) {
				return memory_agg_malformed('pointer definition ${source_id} does not dominate use ${value_id}')
			}
			result_type := memory_agg_require_pointer_type(m, 'pointer BITCAST ${value_id}',
				instruction.typ)!
			source_type := memory_agg_require_pointer_type(m, 'pointer BITCAST ${value_id} source',
				m.values[int(source_id)].typ)!
			if memory_agg_get_type(m, source_type.elem_type)!.kind == .struct_t
				|| memory_agg_get_type(m, result_type.elem_type)!.kind == .struct_t {
				return memory_agg_upstream('aggregate pointer BITCAST ${value_id} is outside M1b provenance')
			}
			_ = memory_agg_require_scalar_type(m, 'pointer BITCAST ${value_id} pointee',
				result_type.elem_type)!
			snapshot = MemoryAggPointerSnapshot{
				definition:      anchor
				value_id:        value_id
				origin:          .bitcast
				pointee_type:    result_type.elem_type
				root_slot_id:    source.root_slot_id
				root_size_bytes: source.root_size_bytes
				byte_offset:     source.byte_offset
				remaining_bytes: source.remaining_bytes
				is_one_past:     source.is_one_past
			}
		}
		else {
			return memory_agg_upstream('pointer value ${value_index} is not a supported local provenance definition')
		}
	}
	pointee := memory_agg_get_type(m, snapshot.pointee_type)!
	if pointee.kind == .struct_t {
		_ = memory_agg_require_aggregate_layout(snapshot.pointee_type, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'pointer ${value_id} pointee')!
	} else {
		_ = memory_agg_require_layout(snapshot.pointee_type, facts, layout_index,
			mut layout_use_counts, 'pointer ${value_id} pointee')!
	}
	states[value_index] = 2
	absolute_depths[value_index] = absolute_depth
	cache[value_index] = snapshot
	return snapshot
}

fn memory_agg_validate_pointer_uses(m &ssa.Module, structure &MemoryAggStructure, module_proof &MemoryAggModuleProof, pointers []MemoryAggPointerSnapshot) ! {
	for pointer in pointers {
		for use in module_proof.uses[int(pointer.value_id)] {
			if use.anchor.function_index != pointer.definition.function_index {
				return memory_agg_upstream('pointer ${pointer.value_id} has cross-function reference in function ${use.anchor.function_index}')
			}
			use_instruction := m.instrs[use.anchor.instruction_index]
			allowed := match use_instruction.op {
				.get_element_ptr, .add, .bitcast, .load {
					use.operand_index == 0
				}
				.store {
					use.operand_index == 1
				}
				else {
					false
				}
			}
			if !allowed {
				return memory_agg_upstream('pointer ${pointer.value_id} escapes through ${use_instruction.op} operand ${use.operand_index}')
			}
			if !memory_agg_anchor_dominates(structure, pointer.definition, use.anchor) {
				return memory_agg_malformed('pointer definition ${pointer.value_id} does not dominate use ${use.anchor.instruction_value_id}')
			}
		}
	}
}

fn memory_agg_snapshot_pointers(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, module_proof &MemoryAggModuleProof, slot_plan &MemoryAggSlotPlan, aggregate_index &MemoryAggAggregateFactIndex, layout_index map[int]int, mut layout_use_counts []int, aggregate_layout_table &MemoryAggAggregateLayoutTable, mut aggregate_layout_use_counts []int, mut constants MemoryAggConstantTable) !([]MemoryAggPointerSnapshot, []MemoryAggPointerSnapshot, []u8, []u8, []u8) {
	mut pointer_count := 0
	for value_id in structure.active_values {
		value := m.values[int(value_id)]
		if memory_agg_type_is_pointer(m, value.typ)! {
			pointer_count++
		}
	}
	memory_agg_validate_count('pointer definition count', pointer_count,
		memory_agg_max_pointer_definitions)!
	mut states := []u8{len: m.values.len}
	mut absolute_depths := []u8{len: m.values.len}
	mut cache := []MemoryAggPointerSnapshot{len: m.values.len}
	mut pointers := []MemoryAggPointerSnapshot{cap: pointer_count}
	mut aggregate_field_pointer_use_counts := []u8{
		len: facts.aggregate_field_pointers.len
	}
	for value_id in structure.active_values {
		value := m.values[int(value_id)]
		if !memory_agg_type_is_pointer(m, value.typ)! {
			continue
		}
		pointer := memory_agg_resolve_pointer(m, facts, structure, slot_plan,
			aggregate_index, layout_index, mut layout_use_counts, aggregate_layout_table,
			mut aggregate_layout_use_counts, mut constants, value_id, 0, mut states,
			mut absolute_depths, mut cache)!
		field_pointer_index := aggregate_index.field_pointers[int(value_id)]
		if field_pointer_index >= 0 {
			if pointer.origin != .aggregate_field {
				return memory_agg_orphan('aggregate field pointer ${field_pointer_index} was not consumed as aggregate-field provenance')
			}
			if aggregate_field_pointer_use_counts[field_pointer_index] != 0 {
				return memory_agg_malformed('aggregate field pointer ${field_pointer_index} was consumed more than once')
			}
			aggregate_field_pointer_use_counts[field_pointer_index]++
		}
		pointers << pointer
	}
	memory_agg_validate_pointer_uses(m, structure, module_proof, pointers)!
	return pointers, cache, states, absolute_depths, aggregate_field_pointer_use_counts
}

fn memory_agg_validate_scalar_source_dominance(m &ssa.Module, function_index int, structure &MemoryAggStructure, value_id ssa.ValueID, use MemoryAggInstructionAnchor) ! {
	value := memory_agg_get_value(m, value_id)!
	match value.kind {
		.constant {
			return
		}
		.argument {
			function := m.funcs[function_index]
			if value_id !in function.params {
				return memory_agg_malformed('scalar argument ${value_id} does not belong to function ${function_index}')
			}
			return
		}
		.instruction {
			definition := structure.anchors[int(value_id)] or {
				return memory_agg_malformed('scalar instruction ${value_id} does not belong to function ${function_index}')
			}
			if !memory_agg_anchor_dominates(structure, definition, use) {
				return memory_agg_malformed('scalar definition ${value_id} does not dominate use ${use.instruction_value_id}')
			}
		}
		else {
			return memory_agg_upstream('scalar value ${value_id} has unsupported ownership ${value.kind}')
		}
	}
}

fn memory_agg_snapshot_accesses(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, slot_plan &MemoryAggSlotPlan, aggregate_index &MemoryAggAggregateFactIndex, accesses_by_value []int, layout_index map[int]int, mut layout_use_counts []int, aggregate_layout_table &MemoryAggAggregateLayoutTable, mut aggregate_layout_use_counts []int, mut constants MemoryAggConstantTable, mut pointer_cache []MemoryAggPointerSnapshot, mut pointer_states []u8, mut pointer_absolute_depths []u8) ![]MemoryAggScalarAccess {
	mut accesses := []MemoryAggScalarAccess{cap: facts.accesses.len}
	for value_id in structure.active_values {
		value := m.values[int(value_id)]
		instruction := m.instrs[value.index]
		if instruction.op !in [ssa.OpCode.load, .store] {
			continue
		}
		if instruction.op == .load
			&& memory_agg_get_type(m, instruction.typ)!.kind == .struct_t {
			continue
		}
		if instruction.op == .store {
			stored := memory_agg_get_value(m, instruction.operands[0])!
			if memory_agg_get_type(m, stored.typ)!.kind == .struct_t {
				continue
			}
		}
		fact_index := accesses_by_value[int(value_id)]
		if fact_index < 0 {
			return memory_agg_incomplete('${instruction.op} ${value_id} is missing its access fact')
		}
		fact := facts.accesses[fact_index]
		if fact.profile != facts.profile {
			return memory_agg_stale('access ${fact_index} has wrong profile')
		}
		if fact.semantics != .nonvolatile {
			return memory_agg_upstream('access ${fact_index} is not nonvolatile')
		}
		atomic_value := int(instruction.atomic_ord)
		if atomic_value < int(ssa.AtomicOrdering.not_atomic)
			|| atomic_value > int(ssa.AtomicOrdering.seq_cst) {
			return memory_agg_malformed('access ${fact_index} has unsupported atomic ordering ${atomic_value}')
		}
		if instruction.atomic_ord != .not_atomic {
			return memory_agg_upstream('access ${fact_index} is atomic')
		}

		mut expected_kind := MemoryAggScalarAccessKind.load
		mut pointer_id := ssa.ValueID(0)
		mut scalar_id := ssa.ValueID(0)
		mut scalar_type := ssa.TypeID(0)
		if instruction.op == .load {
			expected_kind = .load
			pointer_id = instruction.operands[0]
			scalar_id = value_id
			scalar_type = instruction.typ
		} else {
			expected_kind = .store
			scalar_id = instruction.operands[0]
			pointer_id = instruction.operands[1]
			scalar_type = m.values[int(scalar_id)].typ
			memory_agg_validate_scalar_source_dominance(m, facts.function_index, structure,
				scalar_id, structure.anchors[int(value_id)])!
		}
		if fact.kind != expected_kind || fact.pointer_value_id != pointer_id
			|| fact.scalar_value_id != scalar_id || fact.scalar_type != scalar_type {
			return memory_agg_stale('access ${fact_index} disagrees with ${instruction.op} ${value_id}')
		}
		layout := memory_agg_require_layout(scalar_type, facts, layout_index,
			mut layout_use_counts, 'access ${value_id}')!
		pointer_value := memory_agg_get_value(m, pointer_id)!
		pointer_type := memory_agg_require_pointer_type(m, 'access ${value_id} pointer',
			pointer_value.typ)!
		if pointer_type.elem_type != scalar_type {
			return memory_agg_malformed('access ${value_id} pointer pointee and scalar type disagree')
		}
		if pointer_states[int(pointer_id)] != 2 {
			_ = memory_agg_resolve_pointer(m, facts, structure, slot_plan,
				aggregate_index, layout_index, mut layout_use_counts,
				aggregate_layout_table, mut aggregate_layout_use_counts, mut constants,
				pointer_id, 0, mut pointer_states, mut pointer_absolute_depths,
				mut pointer_cache)!
		}
		pointer := pointer_cache[int(pointer_id)]
		if pointer.pointee_type != scalar_type {
			return memory_agg_malformed('access ${value_id} provenance pointee and scalar type disagree')
		}
		request := slot_plan.request_by_id[int(pointer.root_slot_id)] or {
			return memory_agg_malformed('access ${value_id} has unknown root slot ${pointer.root_slot_id}')
		}
		if request.alignment_bytes < u64(layout.alignment_bytes)
			|| pointer.byte_offset % u64(layout.alignment_bytes) != 0 {
			return memory_agg_malformed('access ${value_id} is not naturally aligned')
		}
		if pointer.is_one_past || pointer.byte_offset >= pointer.root_size_bytes {
			return memory_agg_malformed('access ${value_id} dereferences a one-past pointer')
		}
		end := memory_agg_checked_add(pointer.byte_offset, u64(layout.storage_width_bytes))!
		if end > pointer.root_size_bytes {
			return memory_agg_malformed('access ${value_id} exceeds its local root')
		}
		if expected_kind == .store && memory_agg_slot_plan_has_aggregate_request(slot_plan,
			pointer.root_slot_id) {
			memory_agg_consume_scalar_action_source(m, facts, structure, scalar_id,
				scalar_type, structure.anchors[int(value_id)], mut constants,
				'aggregate field store ${value_id}')!
		}
		accesses << MemoryAggScalarAccess{
			anchor:               structure.anchors[int(value_id)]
			kind:                 expected_kind
			pointer_value_id:     pointer_id
			scalar_value_id:      scalar_id
			scalar_type:          scalar_type
			root_slot_id:         pointer.root_slot_id
			byte_offset:          pointer.byte_offset
			semantic_width_bits:  layout.semantic_width_bits
			semantic_is_unsigned: layout.semantic_is_unsigned
			storage_width_bytes:  layout.storage_width_bytes
			alignment_bytes:      layout.alignment_bytes
			canonicalize_i1:      layout.semantic_width_bits == 1
		}
	}
	return accesses
}

fn memory_agg_slot_plan_has_aggregate_request(slot_plan &MemoryAggSlotPlan, request_id u32) bool {
	for slot in slot_plan.aggregate_slots {
		if slot.request.id == request_id {
			return true
		}
	}
	return false
}

struct MemoryAggLogicalRegion {
	slot_id         u32
	offset_bytes    u64
	size_bytes      u64
	alignment_bytes u64
}

fn memory_agg_aggregate_layout_without_use(type_id ssa.TypeID, facts &MemoryAggFunctionFacts, table &MemoryAggAggregateLayoutTable, context string) !MemoryAggAggregateLayoutBinding {
	index := table.index_by_type[int(type_id)] or {
		return memory_agg_incomplete('${context} is missing authoritative aggregate layout for type ${type_id}')
	}
	return facts.aggregate_layouts[index]
}

fn memory_agg_snapshot_region(snapshot MemoryAggAggregateSnapshot, layout MemoryAggAggregateLayoutBinding, slot_plan &MemoryAggSlotPlan, context string) !MemoryAggLogicalRegion {
	request := slot_plan.request_by_id[int(snapshot.root_slot_id)] or {
		return memory_agg_malformed('${context} snapshot has unknown slot ${snapshot.root_slot_id}')
	}
	if request.kind != .aggregate_temp || request.size_bytes != layout.size_bytes
		|| request.alignment_bytes != layout.alignment_bytes {
		return memory_agg_malformed('${context} snapshot request disagrees with its layout')
	}
	return MemoryAggLogicalRegion{
		slot_id:         request.id
		offset_bytes:    0
		size_bytes:      layout.size_bytes
		alignment_bytes: request.alignment_bytes
	}
}

fn memory_agg_pointer_region(pointer MemoryAggPointerSnapshot, layout MemoryAggAggregateLayoutBinding, slot_plan &MemoryAggSlotPlan, context string) !MemoryAggLogicalRegion {
	request := slot_plan.request_by_id[int(pointer.root_slot_id)] or {
		return memory_agg_malformed('${context} pointer has unknown slot ${pointer.root_slot_id}')
	}
	if pointer.pointee_type != layout.type_id || pointer.is_one_past {
		return memory_agg_malformed('${context} does not point at an aggregate object')
	}
	if request.alignment_bytes < layout.alignment_bytes
		|| pointer.byte_offset % layout.alignment_bytes != 0 {
		return memory_agg_malformed('${context} aggregate pointer is misaligned')
	}
	end := memory_agg_checked_add(pointer.byte_offset, layout.size_bytes)!
	if end > request.size_bytes || end > pointer.root_size_bytes {
		return memory_agg_malformed('${context} aggregate range exceeds its local root')
	}
	return MemoryAggLogicalRegion{
		slot_id:         request.id
		offset_bytes:    pointer.byte_offset
		size_bytes:      layout.size_bytes
		alignment_bytes: request.alignment_bytes
	}
}

fn memory_agg_known_pointer(pointer_id ssa.ValueID, mut pointer_states []u8, mut pointer_cache []MemoryAggPointerSnapshot, context string) !MemoryAggPointerSnapshot {
	index := int(pointer_id)
	if index <= 0 || index >= pointer_states.len || pointer_states[index] != 2 {
		return memory_agg_upstream('${context} has unknown local pointer provenance')
	}
	return pointer_cache[index]
}

fn memory_agg_snapshot_source(m &ssa.Module, structure &MemoryAggStructure, slot_plan &MemoryAggSlotPlan, value_id ssa.ValueID, use_anchor MemoryAggInstructionAnchor, aggregate_type ssa.TypeID, context string) !MemoryAggAggregateSnapshot {
	snapshot := slot_plan.snapshot_by_value[int(value_id)] or {
		return memory_agg_upstream('${context} source ${value_id} is not an immutable aggregate snapshot')
	}
	value := memory_agg_get_value(m, value_id)!
	if value.typ != aggregate_type || snapshot.aggregate_type_id != aggregate_type {
		return memory_agg_stale('${context} source aggregate type disagrees')
	}
	if !memory_agg_anchor_dominates(structure, snapshot.definition, use_anchor) {
		return memory_agg_malformed('${context} source snapshot ${value_id} does not dominate its use')
	}
	return snapshot
}

fn memory_agg_consume_scalar_action_source(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, value_id ssa.ValueID, scalar_type ssa.TypeID, use_anchor MemoryAggInstructionAnchor, mut constants MemoryAggConstantTable, context string) ! {
	value := memory_agg_get_value(m, value_id)!
	if value.typ != scalar_type {
		return memory_agg_stale('${context} scalar type disagrees')
	}
	_ = memory_agg_require_scalar_type(m, context, scalar_type)!
	if value.kind == .constant {
		binding_index := constants.index_by_value[int(value_id)] or {
			return memory_agg_incomplete('${context} constant ${value_id} is missing ScalarConstantBinding')
		}
		if facts.scalar_constants[binding_index].type_id != scalar_type {
			return memory_agg_stale('${context} constant type disagrees')
		}
		constants.use_counts[binding_index]++
		return
	}
	memory_agg_validate_scalar_source_dominance(m, facts.function_index, structure,
		value_id, use_anchor)!
}

fn memory_agg_validate_snapshot_uses(m &ssa.Module, structure &MemoryAggStructure, module_proof &MemoryAggModuleProof, snapshots []MemoryAggAggregateSnapshot) ! {
	for snapshot in snapshots {
		for use in module_proof.uses[int(snapshot.value_id)] {
			if use.anchor.function_index != snapshot.definition.function_index {
				return memory_agg_upstream('aggregate snapshot ${snapshot.value_id} has cross-function reference')
			}
			instruction := m.instrs[use.anchor.instruction_index]
			allowed := match instruction.op {
				.store {
					use.operand_index == 0
				}
				.extractvalue, .insertvalue {
					use.operand_index == 0
				}
				else {
					false
				}
			}
			if !allowed {
				return memory_agg_upstream('aggregate snapshot ${snapshot.value_id} escapes through ${instruction.op} operand ${use.operand_index}')
			}
			if !memory_agg_anchor_dominates(structure, snapshot.definition, use.anchor) {
				return memory_agg_malformed('aggregate snapshot ${snapshot.value_id} does not dominate use ${use.anchor.instruction_value_id}')
			}
		}
	}
}

fn memory_agg_validate_aggregate_operations(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, module_proof &MemoryAggModuleProof, aggregate_layout_table &MemoryAggAggregateLayoutTable, mut aggregate_layout_use_counts []int, slot_plan &MemoryAggSlotPlan, mut pointer_states []u8, mut pointer_cache []MemoryAggPointerSnapshot, mut constants MemoryAggConstantTable) ! {
	memory_agg_validate_snapshot_uses(m, structure, module_proof,
		slot_plan.aggregate_snapshots)!
	for fact_index, fact in facts.aggregate_constructs {
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate construct ${fact.result_value_id}')!
		instruction := m.instrs[fact.anchor.instruction_index]
		if instruction.typ != fact.aggregate_type_id
			|| instruction.operands.len != layout.fields.len {
			return memory_agg_stale('aggregate construct ${fact_index} shape disagrees')
		}
		snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
			return memory_agg_malformed('aggregate construct ${fact_index} has no result snapshot')
		}
		if snapshot.definition != fact.anchor {
			return memory_agg_stale('aggregate construct ${fact_index} snapshot anchor disagrees')
		}
		for field_index, field in layout.fields {
			memory_agg_consume_scalar_action_source(m, facts, structure,
				instruction.operands[field_index], field.type_id, fact.anchor, mut constants,
				'aggregate construct ${fact_index} field ${field_index}')!
		}
	}
	for fact_index, fact in facts.aggregate_loads {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate load ${fact_index} has wrong profile')
		}
		if fact.semantics != .nonvolatile {
			return memory_agg_upstream('aggregate load ${fact_index} is not nonvolatile')
		}
		instruction := m.instrs[fact.anchor.instruction_index]
		if instruction.atomic_ord != .not_atomic {
			return memory_agg_upstream('aggregate load ${fact_index} is atomic')
		}
		if instruction.operands.len != 1 || instruction.operands[0] != fact.pointer_value_id
			|| instruction.typ != fact.aggregate_type_id {
			return memory_agg_stale('aggregate load ${fact_index} shape disagrees')
		}
		pointer_value := memory_agg_get_value(m, fact.pointer_value_id)!
		pointer_type := memory_agg_require_pointer_type(m, 'aggregate load ${fact_index}',
			pointer_value.typ)!
		if pointer_type.elem_type != fact.aggregate_type_id {
			return memory_agg_stale('aggregate load ${fact_index} pointer type disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate load ${fact.result_value_id}')!
		pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
			mut pointer_cache, 'aggregate load ${fact_index}')!
		_ = memory_agg_pointer_region(pointer, layout, slot_plan,
			'aggregate load ${fact_index}')!
		snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
			return memory_agg_malformed('aggregate load ${fact_index} has no result snapshot')
		}
		_ = memory_agg_snapshot_region(snapshot, layout, slot_plan,
			'aggregate load ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_stores {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate store ${fact_index} has wrong profile')
		}
		if fact.semantics != .nonvolatile {
			return memory_agg_upstream('aggregate store ${fact_index} is not nonvolatile')
		}
		instruction := m.instrs[fact.anchor.instruction_index]
		if instruction.atomic_ord != .not_atomic {
			return memory_agg_upstream('aggregate store ${fact_index} is atomic')
		}
		if instruction.operands.len != 2 || instruction.operands[0] != fact.source_value_id
			|| instruction.operands[1] != fact.pointer_value_id
			|| instruction.typ != ssa.TypeID(0) {
			return memory_agg_stale('aggregate store ${fact_index} shape disagrees')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate store ${fact.anchor.instruction_value_id}')!
		source := memory_agg_snapshot_source(m, structure, slot_plan, fact.source_value_id,
			fact.anchor, fact.aggregate_type_id, 'aggregate store ${fact_index}')!
		_ = memory_agg_snapshot_region(source, layout, slot_plan,
			'aggregate store ${fact_index}')!
		pointer_value := memory_agg_get_value(m, fact.pointer_value_id)!
		pointer_type := memory_agg_require_pointer_type(m, 'aggregate store ${fact_index}',
			pointer_value.typ)!
		if pointer_type.elem_type != fact.aggregate_type_id {
			return memory_agg_stale('aggregate store ${fact_index} pointer type disagrees')
		}
		pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
			mut pointer_cache, 'aggregate store ${fact_index}')!
		_ = memory_agg_pointer_region(pointer, layout, slot_plan,
			'aggregate store ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_extracts {
		if fact.profile != facts.profile {
			return memory_agg_stale('aggregate extract ${fact_index} has wrong profile')
		}
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate extract ${fact.result_value_id}')!
		if int(fact.field_index) >= layout.fields.len {
			return memory_agg_stale('aggregate extract ${fact_index} field index is outside its layout')
		}
		field := layout.fields[int(fact.field_index)]
		instruction := m.instrs[fact.anchor.instruction_index]
		if instruction.operands != [fact.source_value_id] || instruction.typ != field.type_id {
			return memory_agg_stale('aggregate extract ${fact_index} shape disagrees')
		}
		source := memory_agg_snapshot_source(m, structure, slot_plan, fact.source_value_id,
			fact.anchor, fact.aggregate_type_id, 'aggregate extract ${fact_index}')!
		_ = memory_agg_snapshot_region(source, layout, slot_plan,
			'aggregate extract ${fact_index}')!
	}
	for fact_index, fact in facts.aggregate_inserts {
		layout := memory_agg_require_aggregate_layout(fact.aggregate_type_id, facts,
			aggregate_layout_table, mut aggregate_layout_use_counts,
			'aggregate insert ${fact.result_value_id}')!
		if int(fact.field_index) >= layout.fields.len {
			return memory_agg_stale('aggregate insert ${fact_index} field index is outside its layout')
		}
		field := layout.fields[int(fact.field_index)]
		instruction := m.instrs[fact.anchor.instruction_index]
		if instruction.operands != [fact.source_value_id, fact.field_value_id]
			|| instruction.typ != fact.aggregate_type_id {
			return memory_agg_stale('aggregate insert ${fact_index} shape disagrees')
		}
		source := memory_agg_snapshot_source(m, structure, slot_plan, fact.source_value_id,
			fact.anchor, fact.aggregate_type_id, 'aggregate insert ${fact_index}')!
		_ = memory_agg_snapshot_region(source, layout, slot_plan,
			'aggregate insert ${fact_index}')!
		result := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
			return memory_agg_malformed('aggregate insert ${fact_index} has no result snapshot')
		}
		_ = memory_agg_snapshot_region(result, layout, slot_plan,
			'aggregate insert ${fact_index}')!
		memory_agg_consume_scalar_action_source(m, facts, structure, fact.field_value_id,
			field.type_id, fact.anchor, mut constants,
			'aggregate insert ${fact_index} field')!
	}
}

fn memory_agg_region_end(region MemoryAggLogicalRegion) !u64 {
	return memory_agg_checked_add(region.offset_bytes, region.size_bytes)
}

fn memory_agg_copy_direction(source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion) !MemoryAggCopyDirection {
	if source.size_bytes != destination.size_bytes {
		return memory_agg_malformed('logical copy region sizes disagree')
	}
	source_end := memory_agg_region_end(source)!
	destination_end := memory_agg_region_end(destination)!
	if source.slot_id != destination.slot_id || source_end <= destination.offset_bytes
		|| destination_end <= source.offset_bytes
		|| destination.offset_bytes <= source.offset_bytes {
		return .low_to_high
	}
	return .high_to_low
}

fn memory_agg_regions_identical(source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion) bool {
	return source.slot_id == destination.slot_id
		&& source.offset_bytes == destination.offset_bytes
		&& source.size_bytes == destination.size_bytes
}

fn memory_agg_low_copy_width(source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion, cursor u64) u64 {
	remaining := source.size_bytes - cursor
	for width in [u64(8), 4, 2, 1] {
		if width <= remaining && source.alignment_bytes >= width
			&& destination.alignment_bytes >= width
			&& (source.offset_bytes + cursor) % width == 0
			&& (destination.offset_bytes + cursor) % width == 0 {
			return width
		}
	}
	return 1
}

fn memory_agg_high_copy_width(source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion, remaining u64) u64 {
	for width in [u64(8), 4, 2, 1] {
		if width <= remaining && source.alignment_bytes >= width
			&& destination.alignment_bytes >= width
			&& (source.offset_bytes + remaining - width) % width == 0
			&& (destination.offset_bytes + remaining - width) % width == 0 {
			return width
		}
	}
	return 1
}

fn memory_agg_zero_width(destination MemoryAggLogicalRegion, cursor u64) u64 {
	remaining := destination.size_bytes - cursor
	for width in [u64(8), 4, 2, 1] {
		if width <= remaining && destination.alignment_bytes >= width
			&& (destination.offset_bytes + cursor) % width == 0 {
			return width
		}
	}
	return 1
}

fn memory_agg_count_zero_chunks(destination MemoryAggLogicalRegion) !int {
	_ = memory_agg_region_end(destination)!
	mut cursor := u64(0)
	mut count := 0
	for cursor < destination.size_bytes {
		cursor += memory_agg_zero_width(destination, cursor)
		count++
	}
	return count
}

fn memory_agg_count_copy_chunks(source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion) !(int, MemoryAggCopyDirection) {
	_ = memory_agg_region_end(source)!
	_ = memory_agg_region_end(destination)!
	direction := memory_agg_copy_direction(source, destination)!
	if memory_agg_regions_identical(source, destination) {
		return 0, direction
	}
	mut count := 0
	if direction == .low_to_high {
		mut cursor := u64(0)
		for cursor < source.size_bytes {
			cursor += memory_agg_low_copy_width(source, destination, cursor)
			count++
		}
	} else {
		mut remaining := source.size_bytes
		for remaining > 0 {
			remaining -= memory_agg_high_copy_width(source, destination, remaining)
			count++
		}
	}
	return count, direction
}

fn memory_agg_checked_action_count(current int, additional int) !int {
	if additional < 0 || current > memory_agg_max_aggregate_actions - additional {
		return memory_agg_malformed('aggregate action count exceeds ${memory_agg_max_aggregate_actions}')
	}
	return current + additional
}

fn memory_agg_count_aggregate_actions(facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, aggregate_index &MemoryAggAggregateFactIndex, aggregate_layout_table &MemoryAggAggregateLayoutTable, slot_plan &MemoryAggSlotPlan, mut pointer_states []u8, mut pointer_cache []MemoryAggPointerSnapshot) !int {
	mut count := 0
	for value_id in structure.active_values {
		value_index := int(value_id)
		construct_index := aggregate_index.constructs[value_index]
		if construct_index >= 0 {
			fact := facts.aggregate_constructs[construct_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate construct ${value_id}')!
			snapshot := slot_plan.snapshot_by_value[value_index] or {
				return memory_agg_malformed('aggregate construct ${value_id} has no snapshot')
			}
			destination := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate construct ${value_id}')!
			count = memory_agg_checked_action_count(count,
				memory_agg_count_zero_chunks(destination)!)!
			count = memory_agg_checked_action_count(count, layout.fields.len)!
		}
		load_index := aggregate_index.loads[value_index]
		if load_index >= 0 {
			fact := facts.aggregate_loads[load_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate load ${value_id}')!
			pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
				mut pointer_cache, 'aggregate load ${load_index}')!
			source := memory_agg_pointer_region(pointer, layout, slot_plan,
				'aggregate load ${load_index}')!
			snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
				return memory_agg_malformed('aggregate load ${load_index} has no snapshot')
			}
			destination := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate load ${load_index}')!
			chunks, _ := memory_agg_count_copy_chunks(source, destination)!
			count = memory_agg_checked_action_count(count, chunks)!
		}
		store_index := aggregate_index.stores[value_index]
		if store_index >= 0 {
			fact := facts.aggregate_stores[store_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate store ${value_id}')!
			snapshot := slot_plan.snapshot_by_value[int(fact.source_value_id)] or {
				return memory_agg_malformed('aggregate store ${store_index} has no source snapshot')
			}
			source := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate store ${store_index}')!
			pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
				mut pointer_cache, 'aggregate store ${store_index}')!
			destination := memory_agg_pointer_region(pointer, layout, slot_plan,
				'aggregate store ${store_index}')!
			chunks, _ := memory_agg_count_copy_chunks(source, destination)!
			count = memory_agg_checked_action_count(count, chunks)!
		}
		if aggregate_index.extracts[value_index] >= 0 {
			count = memory_agg_checked_action_count(count, 1)!
		}
		insert_index := aggregate_index.inserts[value_index]
		if insert_index >= 0 {
			fact := facts.aggregate_inserts[insert_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate insert ${value_id}')!
			source_snapshot := slot_plan.snapshot_by_value[int(fact.source_value_id)] or {
				return memory_agg_malformed('aggregate insert ${insert_index} has no source snapshot')
			}
			source := memory_agg_snapshot_region(source_snapshot, layout, slot_plan,
				'aggregate insert ${insert_index}')!
			result_snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
				return memory_agg_malformed('aggregate insert ${insert_index} has no result snapshot')
			}
			destination := memory_agg_snapshot_region(result_snapshot, layout, slot_plan,
				'aggregate insert ${insert_index}')!
			chunks, _ := memory_agg_count_copy_chunks(source, destination)!
			count = memory_agg_checked_action_count(count, chunks)!
			count = memory_agg_checked_action_count(count, 1)!
		}
	}
	return count
}

fn memory_agg_append_zero_actions(mut actions []MemoryAggAggregateAction, anchor MemoryAggInstructionAnchor, destination MemoryAggLogicalRegion, start_ordinal u32) u32 {
	mut ordinal := start_ordinal
	mut cursor := u64(0)
	for cursor < destination.size_bytes {
		width := memory_agg_zero_width(destination, cursor)
		actions << MemoryAggAggregateAction{
			anchor:                   anchor
			phase:                    1
			ordinal:                  ordinal
			kind:                     .zero
			direction:                .low_to_high
			destination_slot_id:      destination.slot_id
			destination_offset_bytes: destination.offset_bytes + cursor
			width_bytes:              u8(width)
		}
		ordinal++
		cursor += width
	}
	return ordinal
}

fn memory_agg_append_copy_actions(mut actions []MemoryAggAggregateAction, anchor MemoryAggInstructionAnchor, source MemoryAggLogicalRegion, destination MemoryAggLogicalRegion, start_ordinal u32) !u32 {
	mut ordinal := start_ordinal
	direction := memory_agg_copy_direction(source, destination)!
	if memory_agg_regions_identical(source, destination) {
		return ordinal
	}
	if direction == .low_to_high {
		mut cursor := u64(0)
		for cursor < source.size_bytes {
			width := memory_agg_low_copy_width(source, destination, cursor)
			actions << MemoryAggAggregateAction{
				anchor:                   anchor
				phase:                    2
				ordinal:                  ordinal
				kind:                     .copy
				direction:                direction
				source_slot_id:           source.slot_id
				source_offset_bytes:      source.offset_bytes + cursor
				destination_slot_id:      destination.slot_id
				destination_offset_bytes: destination.offset_bytes + cursor
				width_bytes:              u8(width)
			}
			ordinal++
			cursor += width
		}
		return ordinal
	}
	mut remaining := source.size_bytes
	for remaining > 0 {
		width := memory_agg_high_copy_width(source, destination, remaining)
		remaining -= width
		actions << MemoryAggAggregateAction{
			anchor:                   anchor
			phase:                    2
			ordinal:                  ordinal
			kind:                     .copy
			direction:                direction
			source_slot_id:           source.slot_id
			source_offset_bytes:      source.offset_bytes + remaining
			destination_slot_id:      destination.slot_id
			destination_offset_bytes: destination.offset_bytes + remaining
			width_bytes:              u8(width)
		}
		ordinal++
	}
	return ordinal
}

fn memory_agg_append_scalar_write(mut actions []MemoryAggAggregateAction, anchor MemoryAggInstructionAnchor, value_id ssa.ValueID, field MemoryAggAggregateFieldLayout, destination MemoryAggLogicalRegion, ordinal u32, canonicalize_i1 bool) u32 {
	actions << MemoryAggAggregateAction{
		anchor:                   anchor
		phase:                    3
		ordinal:                  ordinal
		kind:                     .scalar_write
		direction:                .low_to_high
		destination_slot_id:      destination.slot_id
		destination_offset_bytes: destination.offset_bytes + field.offset_bytes
		width_bytes:              u8(field.size_bytes)
		scalar_value_id:          value_id
		scalar_type:              field.type_id
		canonicalize_i1:          canonicalize_i1
	}
	return ordinal + 1
}

fn memory_agg_append_scalar_read(mut actions []MemoryAggAggregateAction, anchor MemoryAggInstructionAnchor, result_id ssa.ValueID, field MemoryAggAggregateFieldLayout, source MemoryAggLogicalRegion, ordinal u32, canonicalize_i1 bool) u32 {
	actions << MemoryAggAggregateAction{
		anchor:                anchor
		phase:                 3
		ordinal:               ordinal
		kind:                  .scalar_read
		direction:             .low_to_high
		source_slot_id:        source.slot_id
		source_offset_bytes:   source.offset_bytes + field.offset_bytes
		width_bytes:           u8(field.size_bytes)
		scalar_value_id:       result_id
		scalar_type:           field.type_id
		canonicalize_i1:       canonicalize_i1
	}
	return ordinal + 1
}

fn memory_agg_emit_aggregate_actions(m &ssa.Module, facts &MemoryAggFunctionFacts, structure &MemoryAggStructure, aggregate_index &MemoryAggAggregateFactIndex, aggregate_layout_table &MemoryAggAggregateLayoutTable, slot_plan &MemoryAggSlotPlan, mut pointer_states []u8, mut pointer_cache []MemoryAggPointerSnapshot, action_count int) ![]MemoryAggAggregateAction {
	mut actions := []MemoryAggAggregateAction{cap: action_count}
	for value_id in structure.active_values {
		value_index := int(value_id)
		mut ordinal := u32(0)
		construct_index := aggregate_index.constructs[value_index]
		if construct_index >= 0 {
			fact := facts.aggregate_constructs[construct_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate construct ${value_id}')!
			snapshot := slot_plan.snapshot_by_value[value_index] or {
				return memory_agg_malformed('aggregate construct ${value_id} has no snapshot')
			}
			destination := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate construct ${value_id}')!
			ordinal = memory_agg_append_zero_actions(mut actions, fact.anchor,
				destination, ordinal)
			instruction := m.instrs[fact.anchor.instruction_index]
			for field_index, field in layout.fields {
				typ := memory_agg_get_type(m, field.type_id)!
				ordinal = memory_agg_append_scalar_write(mut actions, fact.anchor,
					instruction.operands[field_index], field, destination, ordinal,
					typ.width == 1)
			}
		}
		load_index := aggregate_index.loads[value_index]
		if load_index >= 0 {
			fact := facts.aggregate_loads[load_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate load ${value_id}')!
			pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
				mut pointer_cache, 'aggregate load ${load_index}')!
			source := memory_agg_pointer_region(pointer, layout, slot_plan,
				'aggregate load ${load_index}')!
			snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
				return memory_agg_malformed('aggregate load ${load_index} has no snapshot')
			}
			destination := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate load ${load_index}')!
			ordinal = memory_agg_append_copy_actions(mut actions, fact.anchor, source,
				destination, ordinal)!
		}
		store_index := aggregate_index.stores[value_index]
		if store_index >= 0 {
			fact := facts.aggregate_stores[store_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate store ${value_id}')!
			snapshot := slot_plan.snapshot_by_value[int(fact.source_value_id)] or {
				return memory_agg_malformed('aggregate store ${store_index} has no source snapshot')
			}
			source := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate store ${store_index}')!
			pointer := memory_agg_known_pointer(fact.pointer_value_id, mut pointer_states,
				mut pointer_cache, 'aggregate store ${store_index}')!
			destination := memory_agg_pointer_region(pointer, layout, slot_plan,
				'aggregate store ${store_index}')!
			ordinal = memory_agg_append_copy_actions(mut actions, fact.anchor, source,
				destination, ordinal)!
		}
		extract_index := aggregate_index.extracts[value_index]
		if extract_index >= 0 {
			fact := facts.aggregate_extracts[extract_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate extract ${value_id}')!
			field := layout.fields[int(fact.field_index)]
			snapshot := slot_plan.snapshot_by_value[int(fact.source_value_id)] or {
				return memory_agg_malformed('aggregate extract ${extract_index} has no source snapshot')
			}
			source := memory_agg_snapshot_region(snapshot, layout, slot_plan,
				'aggregate extract ${extract_index}')!
			typ := memory_agg_get_type(m, field.type_id)!
			ordinal = memory_agg_append_scalar_read(mut actions, fact.anchor,
				fact.result_value_id, field, source, ordinal, typ.width == 1)
		}
		insert_index := aggregate_index.inserts[value_index]
		if insert_index >= 0 {
			fact := facts.aggregate_inserts[insert_index]
			layout := memory_agg_aggregate_layout_without_use(fact.aggregate_type_id,
				facts, aggregate_layout_table, 'aggregate insert ${value_id}')!
			source_snapshot := slot_plan.snapshot_by_value[int(fact.source_value_id)] or {
				return memory_agg_malformed('aggregate insert ${insert_index} has no source snapshot')
			}
			source := memory_agg_snapshot_region(source_snapshot, layout, slot_plan,
				'aggregate insert ${insert_index}')!
			result_snapshot := slot_plan.snapshot_by_value[int(fact.result_value_id)] or {
				return memory_agg_malformed('aggregate insert ${insert_index} has no result snapshot')
			}
			destination := memory_agg_snapshot_region(result_snapshot, layout, slot_plan,
				'aggregate insert ${insert_index}')!
			ordinal = memory_agg_append_copy_actions(mut actions, fact.anchor, source,
				destination, ordinal)!
			field := layout.fields[int(fact.field_index)]
			typ := memory_agg_get_type(m, field.type_id)!
			ordinal = memory_agg_append_scalar_write(mut actions, fact.anchor,
				fact.field_value_id, field, destination, ordinal, typ.width == 1)
		}
	}
	if actions.len != action_count {
		return memory_agg_malformed('internal aggregate action count failed')
	}
	return actions
}

fn memory_agg_validate_fact_exhaustion(layout_use_counts []int, constants &MemoryAggConstantTable) ! {
	for binding_index, use_count in layout_use_counts {
		if use_count == 0 {
			return memory_agg_orphan('scalar layout ${binding_index} is unconsumed')
		}
	}
	for binding_index, use_count in constants.use_counts {
		if use_count == 0 {
			return memory_agg_orphan('scalar constant ${binding_index} is unconsumed')
		}
	}
}

fn memory_agg_validate_aggregate_fact_exhaustion(layout_use_counts []int, field_pointer_use_counts []u8) ! {
	for binding_index, use_count in layout_use_counts {
		if use_count == 0 {
			return memory_agg_orphan('aggregate layout ${binding_index} is unconsumed')
		}
	}
	for binding_index, use_count in field_pointer_use_counts {
		if use_count == 0 {
			return memory_agg_orphan('aggregate field pointer ${binding_index} is unconsumed')
		}
		if use_count != 1 {
			return memory_agg_malformed('aggregate field pointer ${binding_index} was consumed ${use_count} times')
		}
	}
}

fn memory_agg_validate_output(plan &MemoryAggPlan) ! {
	mut previous_request_id := u32(0)
	mut have_previous := false
	mut total := u64(0)
	mut request_ids := map[int]bool{}
	mut requests := map[int]MemorySlotRequest{}
	for slot in plan.slot_requests {
		if slot.request.kind != .fixed_alloca || slot.request.size_bytes == 0
			|| slot.request.alignment_bytes !in [u64(1), 2, 4, 8] {
			return memory_agg_malformed('internal slot invariant failed')
		}
		if have_previous && slot.request.id <= previous_request_id {
			return memory_agg_malformed('internal slot order invariant failed')
		}
		if int(slot.request.id) in request_ids {
			return memory_agg_malformed('internal slot id uniqueness invariant failed')
		}
		request_ids[int(slot.request.id)] = true
		requests[int(slot.request.id)] = slot.request
		previous_request_id = slot.request.id
		have_previous = true
		total = memory_agg_checked_add(total, slot.request.size_bytes)!
	}
	mut previous_aggregate_request_id := u32(0)
	mut have_previous_aggregate := false
	for slot in plan.aggregate_slots {
		expected_kind := if slot.role == .fixed_alloca {
			MemorySlotKind.fixed_alloca
		} else {
			.aggregate_temp
		}
		if slot.request.kind != expected_kind || slot.request.size_bytes == 0
			|| !memory_agg_is_power_of_two(slot.request.alignment_bytes)
			|| slot.request.alignment_bytes > 16 {
			return memory_agg_malformed('internal aggregate slot invariant failed')
		}
		if have_previous_aggregate && slot.request.id <= previous_aggregate_request_id {
			return memory_agg_malformed('internal aggregate slot order invariant failed')
		}
		if int(slot.request.id) in request_ids {
			return memory_agg_malformed('internal slot id uniqueness invariant failed')
		}
		request_ids[int(slot.request.id)] = true
		requests[int(slot.request.id)] = slot.request
		previous_aggregate_request_id = slot.request.id
		have_previous_aggregate = true
		total = memory_agg_checked_add(total, slot.request.size_bytes)!
	}
	if total != plan.total_requested_bytes || total > memory_agg_max_requested_bytes {
		return memory_agg_malformed('internal slot total invariant failed')
	}
	for pointer in plan.pointers {
		if int(pointer.root_slot_id) !in request_ids
			|| pointer.byte_offset > pointer.root_size_bytes
			|| pointer.remaining_bytes != pointer.root_size_bytes - pointer.byte_offset
			|| pointer.is_one_past != (pointer.byte_offset == pointer.root_size_bytes) {
			return memory_agg_malformed('internal pointer invariant failed')
		}
	}
	for access in plan.accesses {
		if int(access.root_slot_id) !in request_ids || access.storage_width_bytes == 0 {
			return memory_agg_malformed('internal access invariant failed')
		}
	}
	mut snapshot_values := map[int]bool{}
	for snapshot in plan.aggregate_snapshots {
		if snapshot.publish_phase != 4 || int(snapshot.root_slot_id) !in request_ids
			|| int(snapshot.value_id) in snapshot_values {
			return memory_agg_malformed('internal aggregate snapshot invariant failed')
		}
		snapshot_values[int(snapshot.value_id)] = true
	}
	if plan.aggregate_actions.len > memory_agg_max_aggregate_actions {
		return memory_agg_malformed('internal aggregate action cap invariant failed')
	}
	mut previous_action := MemoryAggAggregateAction{}
	mut have_previous_action := false
	for action in plan.aggregate_actions {
		if action.width_bytes !in [u8(1), 2, 4, 8] {
			return memory_agg_malformed('internal aggregate action width invariant failed')
		}
		if have_previous_action {
			if action.anchor.block_ordinal < previous_action.anchor.block_ordinal
				|| (action.anchor.block_ordinal == previous_action.anchor.block_ordinal
				&& action.anchor.instruction_ordinal < previous_action.anchor.instruction_ordinal) {
				return memory_agg_malformed('internal aggregate action order invariant failed')
			}
			if action.anchor.instruction_value_id == previous_action.anchor.instruction_value_id
				&& (action.phase < previous_action.phase
				|| action.ordinal != previous_action.ordinal + 1) {
				return memory_agg_malformed('internal aggregate action phase invariant failed')
			}
			if action.anchor.instruction_value_id != previous_action.anchor.instruction_value_id
				&& action.ordinal != 0 {
				return memory_agg_malformed('internal aggregate action ordinal invariant failed')
			}
		} else if action.ordinal != 0 {
			return memory_agg_malformed('internal aggregate action ordinal invariant failed')
		}
		match action.kind {
			.zero {
				if action.phase != 1 || int(action.destination_slot_id) !in request_ids
					|| action.source_slot_id != 0 || action.scalar_value_id != 0
					|| action.scalar_type != 0 || action.source_offset_bytes != 0
					|| action.direction != .low_to_high || action.canonicalize_i1 {
					return memory_agg_malformed('internal zero action invariant failed')
				}
				request := requests[int(action.destination_slot_id)]
				end := memory_agg_checked_add(action.destination_offset_bytes,
					u64(action.width_bytes))!
				if end > request.size_bytes {
					return memory_agg_malformed('internal zero action range invariant failed')
				}
			}
			.copy {
				if action.phase != 2 || int(action.source_slot_id) !in request_ids
					|| int(action.destination_slot_id) !in request_ids
					|| action.scalar_value_id != 0 || action.scalar_type != 0
					|| action.canonicalize_i1 {
					return memory_agg_malformed('internal copy action invariant failed')
				}
				source_request := requests[int(action.source_slot_id)]
				destination_request := requests[int(action.destination_slot_id)]
				source_end := memory_agg_checked_add(action.source_offset_bytes,
					u64(action.width_bytes))!
				destination_end := memory_agg_checked_add(action.destination_offset_bytes,
					u64(action.width_bytes))!
				if source_end > source_request.size_bytes
					|| destination_end > destination_request.size_bytes {
					return memory_agg_malformed('internal copy action range invariant failed')
				}
			}
			.scalar_read {
				if action.phase != 3 || int(action.source_slot_id) !in request_ids
					|| action.destination_slot_id != 0 || action.scalar_value_id <= 0
					|| action.scalar_type <= 0 || action.destination_offset_bytes != 0
					|| action.direction != .low_to_high {
					return memory_agg_malformed('internal scalar-read action invariant failed')
				}
				request := requests[int(action.source_slot_id)]
				end := memory_agg_checked_add(action.source_offset_bytes,
					u64(action.width_bytes))!
				if end > request.size_bytes {
					return memory_agg_malformed('internal scalar-read action range invariant failed')
				}
			}
			.scalar_write {
				if action.phase != 3 || action.source_slot_id != 0
					|| int(action.destination_slot_id) !in request_ids
					|| action.scalar_value_id <= 0 || action.scalar_type <= 0
					|| action.source_offset_bytes != 0
					|| action.direction != .low_to_high {
					return memory_agg_malformed('internal scalar-write action invariant failed')
				}
				request := requests[int(action.destination_slot_id)]
				end := memory_agg_checked_add(action.destination_offset_bytes,
					u64(action.width_bytes))!
				if end > request.size_bytes {
					return memory_agg_malformed('internal scalar-write action range invariant failed')
				}
			}
		}
		previous_action = action
		have_previous_action = true
	}
}

// plan_scalar_static_memory validates and snapshots the inert M1a scalar-local and M1b flat-aggregate subsets.
pub fn plan_scalar_static_memory(m &ssa.Module, facts &MemoryAggFunctionFacts) !MemoryAggPlan {
	memory_agg_validate_raw_domains(facts)!
	memory_agg_validate_input_caps(facts)!
	memory_agg_validate_module_preallocation(m)!
	memory_agg_validate_target_and_function(m, facts)!
	module_proof := memory_agg_snapshot_module_uses(m)!
	structure := memory_agg_snapshot_structure(m, facts.function_index)!
	locals_by_value, accesses_by_value := memory_agg_index_anchored_facts(m, facts,
		&structure)!
	aggregate_index := memory_agg_index_aggregate_facts(m, facts, &structure)!
	memory_agg_validate_relevant_op_table(m, &structure, &aggregate_index)!
	layout_index, scalar_layouts, mut layout_use_counts := memory_agg_snapshot_scalar_layouts(m,
		facts)!
	aggregate_layout_table := memory_agg_snapshot_aggregate_layouts(m, facts, layout_index,
		mut layout_use_counts)!
	mut aggregate_layout_use_counts := aggregate_layout_table.use_counts.clone()
	mut constants := memory_agg_snapshot_constants(m, facts)!
	scalar_slot_plan := memory_agg_snapshot_slots(m, facts, &structure, locals_by_value,
		layout_index,
		mut layout_use_counts, mut constants)!
	slot_plan := memory_agg_snapshot_aggregate_slots(m, facts, &structure, &aggregate_index,
		&aggregate_layout_table, mut aggregate_layout_use_counts, scalar_slot_plan)!
	pointers, mut pointer_cache, mut pointer_states, mut pointer_absolute_depths, aggregate_field_pointer_use_counts := memory_agg_snapshot_pointers(m,
		facts, &structure, &module_proof, &slot_plan, &aggregate_index, layout_index,
		mut layout_use_counts, &aggregate_layout_table, mut aggregate_layout_use_counts,
		mut constants)!
	accesses := memory_agg_snapshot_accesses(m, facts, &structure, &slot_plan,
		&aggregate_index, accesses_by_value, layout_index, mut layout_use_counts,
		&aggregate_layout_table, mut aggregate_layout_use_counts, mut constants,
		mut pointer_cache, mut pointer_states, mut pointer_absolute_depths)!
	memory_agg_validate_aggregate_operations(m, facts, &structure, &module_proof,
		&aggregate_layout_table, mut aggregate_layout_use_counts, &slot_plan,
		mut pointer_states, mut pointer_cache, mut constants)!
	aggregate_action_count := memory_agg_count_aggregate_actions(facts, &structure,
		&aggregate_index, &aggregate_layout_table, &slot_plan, mut pointer_states,
		mut pointer_cache)!
	aggregate_actions := memory_agg_emit_aggregate_actions(m, facts, &structure,
		&aggregate_index, &aggregate_layout_table, &slot_plan, mut pointer_states,
		mut pointer_cache, aggregate_action_count)!
	memory_agg_validate_fact_exhaustion(layout_use_counts, &constants)!
	memory_agg_validate_aggregate_fact_exhaustion(aggregate_layout_use_counts,
		aggregate_field_pointer_use_counts)!
	plan := MemoryAggPlan{
		profile:               facts.profile
		function_index:        facts.function_index
		function_id:           structure.function_id
		scalar_layouts:        scalar_layouts
		slot_requests:         slot_plan.slots
		pointers:              pointers
		accesses:              accesses
		aggregate_layouts:     aggregate_layout_table.layouts
		aggregate_slots:       slot_plan.aggregate_slots
		aggregate_snapshots:   slot_plan.aggregate_snapshots
		aggregate_actions:     aggregate_actions
		total_requested_bytes: slot_plan.total_bytes
	}
	memory_agg_validate_output(&plan)!
	return plan
}
