module amd64

import v3.ssa

pub enum AbiKind {
	sysv_amd64
	microsoft_x64
}

pub enum AbiCallKind {
	prototyped
	variadic
	unprototyped
}

pub enum AbiClass {
	no_class
	integer
	sse
	memory
}

pub enum AbiValueMode {
	no_value
	direct
	mixed
	memory_by_value
	indirect
}

pub enum AbiLocationKind {
	none
	gpr
	xmm
	stack
}

pub enum AbiRegister {
	none
	rax
	rdx
	rcx
	rdi
	rsi
	r8
	r9
	xmm0
	xmm1
	xmm2
	xmm3
	xmm4
	xmm5
	xmm6
	xmm7
}

pub enum AbiIntegralExtension {
	none
	sign_extend_to_32
	zero_extend_to_32
}

pub enum AbiAggregateLayoutForm {
	ordinary
	packed
	over_aligned
	bitfield
	explicit_custom
}

pub enum MicrosoftUdtEligibility {
	not_applicable
	eligible_plain_trivial
	unknown
	ineligible
}

pub struct AbiExternalCAggregateLayout {
pub:
	type_id             ssa.TypeID
	form                AbiAggregateLayoutForm
	size_bytes          int
	alignment_bytes     int
	field_offsets_bytes []int
}

pub struct AbiMicrosoftUdtProof {
pub:
	type_id     ssa.TypeID
	eligibility MicrosoftUdtEligibility
}

pub struct AbiLayoutSnapshot {
pub:
	entries []AbiExternalCAggregateLayout
}

pub struct AbiMicrosoftUdtEvidence {
pub:
	proofs []AbiMicrosoftUdtProof
}

struct AbiV3PhysicalLayout {
	type_id              ssa.TypeID
	size_bytes           int
	alignment_bytes      int
	field_offsets_bytes  []int
	element_stride_bytes int
}

fn derive_v3_physical_layouts(type_store &ssa.TypeStore, function_type ssa.TypeID) ![]AbiV3PhysicalLayout {
	graph := abi_validate_signature_graph(type_store, function_type)!
	abi_validate_layout_domains(type_store, &graph)!
	abi_preflight_layout_depths(type_store, &graph)!
	return abi_derive_private_layouts(type_store, &graph)!
}

pub struct AbiLocation {
pub:
	kind                      AbiLocationKind
	register                  AbiRegister
	class                     AbiClass
	value_offset_bytes        int
	width_bytes               int
	has_stack_address         bool
	caller_stack_offset_bytes int
	callee_stack_offset_bytes int
	has_home_address          bool
	caller_home_offset_bytes  int
	callee_home_offset_bytes  int
}

pub struct AbiValueDecision {
pub:
	type_id                            ssa.TypeID
	mode                               AbiValueMode
	size_bytes                         int
	alignment_bytes                    int
	semantic_width_bits                int
	abi_transfer_width_bits            int
	semantic_is_unsigned               bool
	integral_extension                 AbiIntegralExtension
	classes                            []AbiClass
	locations                          []AbiLocation
	has_indirect_temporary             bool
	indirect_temporary_offset_bytes    int
	indirect_temporary_size_bytes      int
	indirect_temporary_alignment_bytes int
}

pub struct AbiHiddenSretDecision {
pub:
	present bool
	input   AbiLocation
	echo    AbiLocation
}

pub struct AbiFunctionDecision {
pub:
	profile                               TargetProfile
	abi                                   AbiKind
	call_kind                             AbiCallKind
	function_type                         ssa.TypeID
	return_value                          AbiValueDecision
	parameters                            []AbiValueDecision
	hidden_sret                           AbiHiddenSretDecision
	shadow_space_bytes                    int
	red_zone_bytes                        int
	minimum_outgoing_area_bytes           int
	minimum_indirect_temporary_area_bytes int
	pre_call_stack_alignment_bytes        int
}

pub fn abi_kind_for_target_profile(profile TargetProfile) !AbiKind {
	profile_value := int(profile)
	if profile_value == int(TargetProfile.linux_x86_64_sysv_elf)
		|| profile_value == int(TargetProfile.macos_x86_64_sysv_macho) {
		return .sysv_amd64
	}
	if profile_value == int(TargetProfile.windows_x86_64_microsoft_abi_coff) {
		return .microsoft_x64
	}
	return abi_error('invalid_target_profile')
}

pub fn classify_abi_function(profile TargetProfile, call_kind AbiCallKind, type_store &ssa.TypeStore, external_layouts &AbiLayoutSnapshot, udt_evidence &AbiMicrosoftUdtEvidence, function_type ssa.TypeID) !AbiFunctionDecision {
	abi := abi_kind_for_target_profile(profile)!
	abi_validate_call_kind(call_kind)!
	graph := abi_validate_signature_graph(type_store, function_type)!
	evidence := abi_validate_evidence(type_store, &graph, external_layouts, udt_evidence)!
	abi_validate_layout_domains(type_store, &graph)!
	abi_preflight_layout_depths(type_store, &graph)!
	private_layouts := derive_v3_physical_layouts(type_store, function_type)!
	private_indices := abi_index_private_layouts(type_store.types.len, private_layouts)!
	abi_validate_external_layout_equations(type_store, &graph, external_layouts,
		&evidence)!
	return abi_classify_and_place(profile, abi, call_kind, type_store, external_layouts,
		udt_evidence, function_type, &graph, &evidence, private_layouts, private_indices)!
}

struct AbiTypeGraph {
	reachable_values     []bool
	parameter_roots      []ssa.TypeID
	return_root          ssa.TypeID
	top_level_aggregates []bool
}

struct AbiTypeWorkItem {
	type_id ssa.TypeID
}

struct AbiSupportWorkItem {
	type_id                    ssa.TypeID
	is_root                    bool
	allow_root_void            bool
	inside_private_v_aggregate bool
}

struct AbiCycleWorkItem {
	type_id ssa.TypeID
	exit    bool
}

struct AbiDepthWorkItem {
	type_id ssa.TypeID
	depth   int
}

struct AbiEvidenceIndex {
	external_by_type []int
	proof_by_type    []int
}

struct AbiPhysicalFacts {
	size_bytes           int
	alignment_bytes      int
	field_offsets_bytes  []int
	element_stride_bytes int
}

struct AbiScalarFacts {
	size_bytes              int
	alignment_bytes         int
	semantic_width_bits     int
	abi_transfer_width_bits int
	semantic_is_unsigned    bool
	integral_extension      AbiIntegralExtension
	class                   AbiClass
	location_width_bytes    int
}

struct AbiClassChunk {
	class              AbiClass
	value_offset_bytes int
	width_bytes        int
}

struct AbiUnplacedValue {
	type_id                 ssa.TypeID
	mode                    AbiValueMode
	size_bytes              int
	alignment_bytes         int
	semantic_width_bits     int
	abi_transfer_width_bits int
	semantic_is_unsigned    bool
	integral_extension      AbiIntegralExtension
	classes                 []AbiClass
	chunks                  []AbiClassChunk
}

struct AbiParameterPlacement {
	decisions             []AbiValueDecision
	outgoing_area_bytes    int
	indirect_area_bytes    int
}

@[cold]
fn abi_error(code string) IError {
	return error('amd64 ABI: ${code}')
}

fn abi_validate_call_kind(call_kind AbiCallKind) ! {
	if int(call_kind) != int(AbiCallKind.prototyped) {
		return abi_error('unsupported_call_kind')
	}
}

fn abi_type_kind_is_valid(kind ssa.TypeKind) bool {
	value := int(kind)
	return value >= int(ssa.TypeKind.void_t) && value <= int(ssa.TypeKind.metadata_t)
}

fn abi_layout_form_is_valid(form AbiAggregateLayoutForm) bool {
	value := int(form)
	return value >= int(AbiAggregateLayoutForm.ordinary)
		&& value <= int(AbiAggregateLayoutForm.explicit_custom)
}

fn abi_udt_eligibility_is_valid(eligibility MicrosoftUdtEligibility) bool {
	value := int(eligibility)
	return value >= int(MicrosoftUdtEligibility.not_applicable)
		&& value <= int(MicrosoftUdtEligibility.ineligible)
}

fn abi_type_id_in_range(type_id ssa.TypeID, type_count int) bool {
	index := int(type_id)
	return index >= 0 && index < type_count
}

fn abi_is_canonical_void(typ ssa.Type) bool {
	return abi_type_kind_is_valid(typ.kind) && typ.kind == .void_t && typ.width == 0
		&& !typ.is_unsigned && typ.elem_type == 0 && typ.len == 0 && typ.fields.len == 0
		&& typ.field_names.len == 0 && typ.params.len == 0 && typ.ret_type == 0
		&& !typ.is_c_struct && !typ.is_union
}

fn abi_validate_store_and_root(type_store &ssa.TypeStore, function_type ssa.TypeID) !ssa.Type {
	if type_store.types.len == 0 || !abi_is_canonical_void(type_store.types[0]) {
		return abi_error('invalid_type_graph')
	}
	if !abi_type_id_in_range(function_type, type_store.types.len) {
		return abi_error('invalid_function_type')
	}
	root := type_store.types[int(function_type)]
	if !abi_type_kind_is_valid(root.kind) {
		return abi_error('invalid_type_graph')
	}
	if root.kind != .func_t {
		return abi_error('invalid_function_type')
	}
	return root
}

fn abi_validate_zero_foreign_payload(typ ssa.Type) bool {
	return typ.width == 0 && !typ.is_unsigned && typ.elem_type == 0 && typ.len == 0
		&& typ.fields.len == 0 && typ.field_names.len == 0 && typ.params.len == 0
		&& typ.ret_type == 0 && !typ.is_c_struct && !typ.is_union
}

fn abi_validate_owned_type_id(type_id ssa.TypeID, type_count int, allow_zero bool) ! {
	index := int(type_id)
	if index < 0 || index >= type_count || (!allow_zero && index == 0) {
		return abi_error('invalid_type_graph')
	}
}

fn abi_validate_type_payload(type_store &ssa.TypeStore, type_id ssa.TypeID) ! {
	index := int(type_id)
	typ := type_store.types[index]
	if !abi_type_kind_is_valid(typ.kind) {
		return abi_error('invalid_type_graph')
	}
	match typ.kind {
		.void_t {
			if index != 0 || !abi_validate_zero_foreign_payload(typ) {
				return abi_error('invalid_type_graph')
			}
		}
		.int_t {
			if typ.elem_type != 0 || typ.len != 0 || typ.fields.len != 0
				|| typ.field_names.len != 0 || typ.params.len != 0 || typ.ret_type != 0
				|| typ.is_c_struct || typ.is_union || (typ.width == 1 && typ.is_unsigned) {
				return abi_error('invalid_type_graph')
			}
		}
		.float_t {
			if typ.is_unsigned || typ.elem_type != 0 || typ.len != 0 || typ.fields.len != 0
				|| typ.field_names.len != 0 || typ.params.len != 0 || typ.ret_type != 0
				|| typ.is_c_struct || typ.is_union {
				return abi_error('invalid_type_graph')
			}
		}
		.ptr_t {
			if typ.width != 0 || typ.is_unsigned || typ.len != 0 || typ.fields.len != 0
				|| typ.field_names.len != 0 || typ.params.len != 0 || typ.ret_type != 0
				|| typ.is_c_struct || typ.is_union {
				return abi_error('invalid_type_graph')
			}
			abi_validate_owned_type_id(typ.elem_type, type_store.types.len, true)!
		}
		.array_t {
			if typ.width != 0 || typ.is_unsigned || typ.fields.len != 0
				|| typ.field_names.len != 0 || typ.params.len != 0 || typ.ret_type != 0
				|| typ.is_c_struct || typ.is_union {
				return abi_error('invalid_type_graph')
			}
			abi_validate_owned_type_id(typ.elem_type, type_store.types.len, false)!
		}
		.struct_t {
			if typ.width != 0 || typ.is_unsigned || typ.elem_type != 0 || typ.len != 0
				|| typ.params.len != 0 || typ.ret_type != 0
				|| (typ.field_names.len != 0 && typ.field_names.len != typ.fields.len) {
				return abi_error('invalid_type_graph')
			}
			for field_type in typ.fields {
				abi_validate_owned_type_id(field_type, type_store.types.len, false)!
			}
		}
		.func_t {
			if typ.width != 0 || typ.is_unsigned || typ.elem_type != 0 || typ.len != 0
				|| typ.fields.len != 0 || typ.field_names.len != 0 || typ.is_c_struct
				|| typ.is_union {
				return abi_error('invalid_type_graph')
			}
			for parameter_type in typ.params {
				abi_validate_owned_type_id(parameter_type, type_store.types.len, true)!
			}
			abi_validate_owned_type_id(typ.ret_type, type_store.types.len, true)!
		}
		.label_t, .metadata_t {
			if !abi_validate_zero_foreign_payload(typ) {
				return abi_error('invalid_type_graph')
			}
		}
	}
}

fn abi_push_type_children(type_store &ssa.TypeStore, type_id ssa.TypeID, mut work []AbiTypeWorkItem) {
	typ := type_store.types[int(type_id)]
	match typ.kind {
		.array_t {
			work << AbiTypeWorkItem{
				type_id: typ.elem_type
			}
		}
		.struct_t {
			for field_offset in 0 .. typ.fields.len {
				field_index := typ.fields.len - 1 - field_offset
				work << AbiTypeWorkItem{
					type_id: typ.fields[field_index]
				}
			}
		}
		.func_t {
			work << AbiTypeWorkItem{
				type_id: typ.ret_type
			}
			for parameter_offset in 0 .. typ.params.len {
				parameter_index := typ.params.len - 1 - parameter_offset
				work << AbiTypeWorkItem{
					type_id: typ.params[parameter_index]
				}
			}
		}
		else {}
	}
}

fn abi_is_aggregate_kind(kind ssa.TypeKind) bool {
	return kind == .array_t || kind == .struct_t
}

fn abi_validate_signature_graph(type_store &ssa.TypeStore, function_type ssa.TypeID) !AbiTypeGraph {
	root := abi_validate_store_and_root(type_store, function_type)!
	abi_validate_type_payload(type_store, function_type)!
	mut reachable := []bool{len: type_store.types.len}
	mut seen := []bool{len: type_store.types.len}
	mut work := []AbiTypeWorkItem{}
	work << AbiTypeWorkItem{
		type_id: root.ret_type
	}
	for parameter_offset in 0 .. root.params.len {
		parameter_index := root.params.len - 1 - parameter_offset
		work << AbiTypeWorkItem{
			type_id: root.params[parameter_index]
		}
	}
	for work.len > 0 {
		item := work.pop()
		index := int(item.type_id)
		reachable[index] = true
		if seen[index] {
			continue
		}
		abi_validate_type_payload(type_store, item.type_id)!
		seen[index] = true
		abi_push_type_children(type_store, item.type_id, mut work)
	}
	mut top_level := []bool{len: type_store.types.len}
	for parameter_type in root.params {
		typ := type_store.types[int(parameter_type)]
		if abi_is_aggregate_kind(typ.kind) {
			top_level[int(parameter_type)] = true
		}
	}
	return_type := type_store.types[int(root.ret_type)]
	if abi_is_aggregate_kind(return_type.kind) {
		top_level[int(root.ret_type)] = true
	}
	graph := AbiTypeGraph{
		reachable_values:     reachable
		parameter_roots:      root.params.clone()
		return_root:          root.ret_type
		top_level_aggregates: top_level
	}
	abi_validate_supported_graph(type_store, &graph)!
	abi_validate_by_value_cycles(type_store, &graph)!
	return graph
}

fn abi_validate_supported_root(type_store &ssa.TypeStore, root_type ssa.TypeID, allow_void bool) ! {
	mut visited := []u8{len: type_store.types.len}
	mut work := [AbiSupportWorkItem{
		type_id:         root_type
		is_root:         true
		allow_root_void: allow_void
	}]
	for work.len > 0 {
		item := work.pop()
		index := int(item.type_id)
		state_bit := if item.inside_private_v_aggregate { u8(2) } else { u8(1) }
		if visited[index] & state_bit != 0 {
			continue
		}
		visited[index] |= state_bit
		typ := type_store.types[index]
		match typ.kind {
			.void_t {
				if !item.is_root || !item.allow_root_void {
					return abi_error('unsupported_abi_value')
				}
			}
			.int_t {
				if typ.width !in [1, 8, 16, 32, 64] {
					return abi_error('unsupported_integer_width')
				}
			}
			.float_t {
				if typ.width !in [32, 64] {
					return abi_error('unsupported_float_width')
				}
			}
			.ptr_t {
				if item.inside_private_v_aggregate && typ.elem_type == 0 {
					return abi_error('unsupported_v3_layout_shape')
				}
			}
			.array_t {
				if typ.len == 0 {
					return abi_error('zero_array_length')
				}
				if typ.len < 0 {
					return abi_error('negative_array_length')
				}
				work << AbiSupportWorkItem{
					type_id:                    typ.elem_type
					inside_private_v_aggregate: true
				}
			}
			.struct_t {
				if typ.fields.len == 0 {
					return abi_error('unsupported_abi_value')
				}
				if !typ.is_c_struct && typ.fields.len > 256 {
					return abi_error('unsupported_v3_layout_shape')
				}
				inside_private := item.inside_private_v_aggregate || !typ.is_c_struct
				for field_offset in 0 .. typ.fields.len {
					field_index := typ.fields.len - 1 - field_offset
					work << AbiSupportWorkItem{
						type_id:                    typ.fields[field_index]
						inside_private_v_aggregate: inside_private
					}
				}
			}
			.func_t, .label_t, .metadata_t {
				return abi_error('unsupported_abi_value')
			}
		}
	}
}

fn abi_validate_supported_graph(type_store &ssa.TypeStore, graph &AbiTypeGraph) ! {
	for parameter_type in graph.parameter_roots {
		abi_validate_supported_root(type_store, parameter_type, false)!
	}
	if graph.return_root != 0 {
		abi_validate_supported_root(type_store, graph.return_root, false)!
	} else {
		abi_validate_supported_root(type_store, graph.return_root, true)!
	}
}

fn abi_push_cycle_children(type_store &ssa.TypeStore, type_id ssa.TypeID, mut work []AbiCycleWorkItem) {
	typ := type_store.types[int(type_id)]
	if typ.kind == .array_t {
		work << AbiCycleWorkItem{
			type_id: typ.elem_type
		}
		return
	}
	if typ.kind == .struct_t {
		for field_offset in 0 .. typ.fields.len {
			field_index := typ.fields.len - 1 - field_offset
			work << AbiCycleWorkItem{
				type_id: typ.fields[field_index]
			}
		}
	}
}

fn abi_validate_cycle_root(type_store &ssa.TypeStore, root_type ssa.TypeID, mut colors []u8) ! {
	mut work := [AbiCycleWorkItem{
		type_id: root_type
	}]
	for work.len > 0 {
		item := work.pop()
		index := int(item.type_id)
		if item.exit {
			colors[index] = 2
			continue
		}
		if colors[index] == 1 {
			return abi_error('invalid_type_graph')
		}
		if colors[index] == 2 {
			continue
		}
		typ := type_store.types[index]
		if !abi_is_aggregate_kind(typ.kind) {
			colors[index] = 2
			continue
		}
		colors[index] = 1
		work << AbiCycleWorkItem{
			type_id: item.type_id
			exit:    true
		}
		abi_push_cycle_children(type_store, item.type_id, mut work)
	}
}

fn abi_validate_by_value_cycles(type_store &ssa.TypeStore, graph &AbiTypeGraph) ! {
	mut colors := []u8{len: type_store.types.len}
	for parameter_type in graph.parameter_roots {
		abi_validate_cycle_root(type_store, parameter_type, mut colors)!
	}
	if graph.return_root != 0 {
		abi_validate_cycle_root(type_store, graph.return_root, mut colors)!
	}
}

fn abi_aggregate_domain(typ ssa.Type) int {
	if typ.kind == .struct_t && typ.is_c_struct {
		return 1
	}
	return 0
}

fn abi_validate_layout_domains(type_store &ssa.TypeStore, graph &AbiTypeGraph) ! {
	for type_index, is_reachable in graph.reachable_values {
		if !is_reachable {
			continue
		}
		typ := type_store.types[type_index]
		if !abi_is_aggregate_kind(typ.kind) {
			continue
		}
		parent_domain := abi_aggregate_domain(typ)
		if typ.kind == .array_t {
			child := type_store.types[int(typ.elem_type)]
			if abi_is_aggregate_kind(child.kind) && abi_aggregate_domain(child) != parent_domain {
				return abi_error('mixed_aggregate_layout_domain')
			}
		} else {
			for field_type in typ.fields {
				child := type_store.types[int(field_type)]
				if abi_is_aggregate_kind(child.kind)
					&& abi_aggregate_domain(child) != parent_domain {
					return abi_error('mixed_aggregate_layout_domain')
				}
			}
		}
	}
}

fn abi_validate_evidence(type_store &ssa.TypeStore, graph &AbiTypeGraph, external_layouts &AbiLayoutSnapshot, udt_evidence &AbiMicrosoftUdtEvidence) !AbiEvidenceIndex {
	mut external_by_type := []int{len: type_store.types.len, init: -1}
	mut previous_type := -1
	for entry_index, entry in external_layouts.entries {
		type_index := int(entry.type_id)
		if type_index <= previous_type || type_index < 0 || type_index >= type_store.types.len {
			return abi_error('invalid_aggregate_layout')
		}
		previous_type = type_index
		typ := type_store.types[type_index]
		if !graph.reachable_values[type_index] || typ.kind != .struct_t || !typ.is_c_struct
			|| typ.fields.len == 0 {
			return abi_error('invalid_aggregate_layout')
		}
		if !abi_layout_form_is_valid(entry.form) {
			return abi_error('invalid_aggregate_layout')
		}
		if entry.form != .ordinary {
			return abi_error('unsupported_aggregate_layout')
		}
		if entry.size_bytes <= 0 || entry.alignment_bytes <= 0
			|| entry.alignment_bytes & (entry.alignment_bytes - 1) != 0
			|| entry.field_offsets_bytes.len != typ.fields.len {
			return abi_error('invalid_aggregate_layout')
		}
		for offset in entry.field_offsets_bytes {
			if offset < 0 {
				return abi_error('invalid_aggregate_layout')
			}
		}
		external_by_type[type_index] = entry_index
	}
	mut proof_by_type := []int{len: type_store.types.len, init: -1}
	previous_type = -1
	for proof_index, proof in udt_evidence.proofs {
		type_index := int(proof.type_id)
		if type_index <= previous_type || type_index < 0 || type_index >= type_store.types.len {
			return abi_error('invalid_aggregate_layout')
		}
		previous_type = type_index
		typ := type_store.types[type_index]
		if !graph.reachable_values[type_index] || !graph.top_level_aggregates[type_index]
			|| typ.kind != .struct_t || typ.fields.len == 0 {
			return abi_error('invalid_aggregate_layout')
		}
		if !abi_udt_eligibility_is_valid(proof.eligibility) {
			return abi_error('invalid_aggregate_layout')
		}
		proof_by_type[type_index] = proof_index
	}
	for type_index, is_reachable in graph.reachable_values {
		if !is_reachable {
			continue
		}
		typ := type_store.types[type_index]
		if typ.kind == .struct_t && typ.is_c_struct && external_by_type[type_index] < 0 {
			return abi_error('missing_external_c_layout')
		}
	}
	return AbiEvidenceIndex{
		external_by_type: external_by_type
		proof_by_type:    proof_by_type
	}
}

fn abi_preflight_depth_root(type_store &ssa.TypeStore, root_type ssa.TypeID) ! {
	mut maximum_depths := []int{len: type_store.types.len, init: -1}
	mut work := [AbiDepthWorkItem{
		type_id: root_type
	}]
	for work.len > 0 {
		item := work.pop()
		if item.depth > 16 {
			return abi_error('unsupported_v3_layout_depth')
		}
		index := int(item.type_id)
		if maximum_depths[index] >= item.depth {
			continue
		}
		maximum_depths[index] = item.depth
		typ := type_store.types[index]
		if typ.kind == .array_t {
			work << AbiDepthWorkItem{
				type_id: typ.elem_type
				depth:   item.depth + 1
			}
		} else if typ.kind == .struct_t {
			for field_type in typ.fields {
				work << AbiDepthWorkItem{
					type_id: field_type
					depth:   item.depth + 1
				}
			}
		}
	}
}

fn abi_preflight_layout_depths(type_store &ssa.TypeStore, graph &AbiTypeGraph) ! {
	for parameter_type in graph.parameter_roots {
		abi_preflight_depth_root(type_store, parameter_type)!
	}
	if graph.return_root != 0 {
		abi_preflight_depth_root(type_store, graph.return_root)!
	}
}

fn abi_checked_add(left int, right int) !int {
	if left < 0 || right < 0 || left > max_int - right {
		return abi_error('arithmetic_overflow')
	}
	return left + right
}

fn abi_checked_mul(left int, right int) !int {
	if left < 0 || right < 0 || (left != 0 && right > max_int / left) {
		return abi_error('arithmetic_overflow')
	}
	return left * right
}

fn abi_checked_align_up(value int, alignment int) !int {
	if value < 0 || alignment <= 0 {
		return abi_error('arithmetic_overflow')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return abi_checked_add(value, alignment - remainder)!
}

fn abi_scalar_physical_facts(typ ssa.Type) AbiPhysicalFacts {
	mut size := 8
	if typ.kind == .int_t || typ.kind == .float_t {
		size = (typ.width + 7) / 8
	}
	mut alignment := 1
	if size >= 8 {
		alignment = 8
	} else if size >= 4 {
		alignment = 4
	}
	return AbiPhysicalFacts{
		size_bytes:      size
		alignment_bytes: alignment
	}
}

fn abi_derive_type_physical_facts(type_store &ssa.TypeStore, type_id ssa.TypeID, mut cache []AbiPhysicalFacts, mut ready []bool) !AbiPhysicalFacts {
	index := int(type_id)
	if ready[index] {
		facts := cache[index]
		return AbiPhysicalFacts{
			size_bytes:           facts.size_bytes
			alignment_bytes:      facts.alignment_bytes
			field_offsets_bytes:  facts.field_offsets_bytes.clone()
			element_stride_bytes: facts.element_stride_bytes
		}
	}
	typ := type_store.types[index]
	mut facts := AbiPhysicalFacts{}
	match typ.kind {
		.int_t, .float_t, .ptr_t {
			facts = abi_scalar_physical_facts(typ)
		}
		.array_t {
			element := abi_derive_type_physical_facts(type_store, typ.elem_type, mut cache,
				mut ready)!
			stride := element.size_bytes
			size := abi_checked_mul(typ.len, stride)!
			facts = AbiPhysicalFacts{
				size_bytes:           size
				alignment_bytes:      element.alignment_bytes
				element_stride_bytes: stride
			}
		}
		.struct_t {
			if typ.is_c_struct {
				return abi_error('mixed_aggregate_layout_domain')
			}
			mut offsets := []int{cap: typ.fields.len}
			mut max_alignment := 1
			if typ.is_union {
				mut maximum_size := 0
				for field_type in typ.fields {
					child := abi_derive_type_physical_facts(type_store, field_type, mut cache,
						mut ready)!
					offsets << 0
					if child.size_bytes > maximum_size {
						maximum_size = child.size_bytes
					}
					if child.alignment_bytes > max_alignment {
						max_alignment = child.alignment_bytes
					}
				}
				facts = AbiPhysicalFacts{
					size_bytes:          abi_checked_align_up(maximum_size, max_alignment)!
					alignment_bytes:     8
					field_offsets_bytes: offsets
				}
			} else {
				mut cursor := 0
				for field_type in typ.fields {
					child := abi_derive_type_physical_facts(type_store, field_type, mut cache,
						mut ready)!
					cursor = abi_checked_align_up(cursor, child.alignment_bytes)!
					offsets << cursor
					cursor = abi_checked_add(cursor, child.size_bytes)!
					if child.alignment_bytes > max_alignment {
						max_alignment = child.alignment_bytes
					}
				}
				facts = AbiPhysicalFacts{
					size_bytes:          abi_checked_align_up(cursor, max_alignment)!
					alignment_bytes:     8
					field_offsets_bytes: offsets
				}
			}
		}
		else {
			return abi_error('unsupported_abi_value')
		}
	}
	cache[index] = AbiPhysicalFacts{
		size_bytes:           facts.size_bytes
		alignment_bytes:      facts.alignment_bytes
		field_offsets_bytes:  facts.field_offsets_bytes.clone()
		element_stride_bytes: facts.element_stride_bytes
	}
	ready[index] = true
	return facts
}

fn abi_derive_private_layouts(type_store &ssa.TypeStore, graph &AbiTypeGraph) ![]AbiV3PhysicalLayout {
	mut cache := []AbiPhysicalFacts{len: type_store.types.len}
	mut ready := []bool{len: type_store.types.len}
	mut layouts := []AbiV3PhysicalLayout{}
	for type_index, is_reachable in graph.reachable_values {
		if !is_reachable {
			continue
		}
		typ := type_store.types[type_index]
		if !abi_is_aggregate_kind(typ.kind) || (typ.kind == .struct_t && typ.is_c_struct) {
			continue
		}
		facts := abi_derive_type_physical_facts(type_store, ssa.TypeID(type_index), mut cache,
			mut ready)!
		layouts << AbiV3PhysicalLayout{
			type_id:              ssa.TypeID(type_index)
			size_bytes:           facts.size_bytes
			alignment_bytes:      facts.alignment_bytes
			field_offsets_bytes:  facts.field_offsets_bytes.clone()
			element_stride_bytes: facts.element_stride_bytes
		}
	}
	return layouts
}

fn abi_index_private_layouts(type_count int, layouts []AbiV3PhysicalLayout) ![]int {
	mut indices := []int{len: type_count, init: -1}
	mut previous_type := -1
	for layout_index, layout in layouts {
		type_index := int(layout.type_id)
		if type_index <= previous_type || type_index < 0 || type_index >= type_count
			|| indices[type_index] >= 0 {
			return abi_error('invalid_type_graph')
		}
		previous_type = type_index
		indices[type_index] = layout_index
	}
	return indices
}

fn abi_natural_alignment(type_store &ssa.TypeStore, type_id ssa.TypeID, mut cache []int, mut ready []bool) !int {
	index := int(type_id)
	if ready[index] {
		return cache[index]
	}
	typ := type_store.types[index]
	mut alignment := 1
	match typ.kind {
		.int_t {
			if typ.width == 1 || typ.width == 8 {
				alignment = 1
			} else if typ.width == 16 {
				alignment = 2
			} else if typ.width == 32 {
				alignment = 4
			} else if typ.width == 64 {
				alignment = 8
			} else {
				return abi_error('unsupported_integer_width')
			}
		}
		.float_t {
			if typ.width == 32 {
				alignment = 4
			} else if typ.width == 64 {
				alignment = 8
			} else {
				return abi_error('unsupported_float_width')
			}
		}
		.ptr_t {
			alignment = 8
		}
		.array_t {
			alignment = abi_natural_alignment(type_store, typ.elem_type, mut cache, mut ready)!
		}
		.struct_t {
			for field_type in typ.fields {
				field_alignment := abi_natural_alignment(type_store, field_type, mut cache,
					mut ready)!
				if field_alignment > alignment {
					alignment = field_alignment
				}
			}
		}
		else {
			return abi_error('unsupported_abi_value')
		}
	}
	cache[index] = alignment
	ready[index] = true
	return alignment
}

fn abi_external_child_size(type_store &ssa.TypeStore, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, child_type ssa.TypeID) !int {
	child := type_store.types[int(child_type)]
	if child.kind == .int_t || child.kind == .float_t || child.kind == .ptr_t {
		return abi_scalar_physical_facts(child).size_bytes
	}
	if child.kind == .struct_t && child.is_c_struct {
		entry_index := evidence.external_by_type[int(child_type)]
		if entry_index < 0 {
			return abi_error('missing_external_c_layout')
		}
		return external_layouts.entries[entry_index].size_bytes
	}
	return abi_error('mixed_aggregate_layout_domain')
}

fn abi_validate_external_type_equation(type_store &ssa.TypeStore, type_id ssa.TypeID, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, mut states []u8, mut natural_cache []int, mut natural_ready []bool) ! {
	index := int(type_id)
	if states[index] == 2 {
		return
	}
	if states[index] == 1 {
		return abi_error('invalid_type_graph')
	}
	states[index] = 1
	typ := type_store.types[index]
	entry_index := evidence.external_by_type[index]
	if entry_index < 0 {
		return abi_error('missing_external_c_layout')
	}
	entry := external_layouts.entries[entry_index]
	for field_type in typ.fields {
		field := type_store.types[int(field_type)]
		if field.kind == .struct_t && field.is_c_struct {
			abi_validate_external_type_equation(type_store, field_type, external_layouts,
				evidence, mut states, mut natural_cache, mut natural_ready)!
		}
	}
	mut maximum_alignment := 1
	if typ.is_union {
		mut maximum_size := 0
		for field_index, field_type in typ.fields {
			field_alignment := abi_natural_alignment(type_store, field_type, mut natural_cache,
				mut natural_ready)!
			field_size := abi_external_child_size(type_store, external_layouts, evidence,
				field_type)!
			if entry.field_offsets_bytes[field_index] != 0 {
				return abi_error('invalid_aggregate_layout')
			}
			if field_size > maximum_size {
				maximum_size = field_size
			}
			if field_alignment > maximum_alignment {
				maximum_alignment = field_alignment
			}
		}
		expected_size := abi_checked_align_up(maximum_size, maximum_alignment)!
		if entry.size_bytes != expected_size || entry.alignment_bytes != maximum_alignment {
			return abi_error('invalid_aggregate_layout')
		}
	} else {
		mut cursor := 0
		for field_index, field_type in typ.fields {
			field_alignment := abi_natural_alignment(type_store, field_type, mut natural_cache,
				mut natural_ready)!
			field_size := abi_external_child_size(type_store, external_layouts, evidence,
				field_type)!
			cursor = abi_checked_align_up(cursor, field_alignment)!
			if entry.field_offsets_bytes[field_index] != cursor {
				return abi_error('invalid_aggregate_layout')
			}
			cursor = abi_checked_add(cursor, field_size)!
			if field_alignment > maximum_alignment {
				maximum_alignment = field_alignment
			}
		}
		expected_size := abi_checked_align_up(cursor, maximum_alignment)!
		if entry.size_bytes != expected_size || entry.alignment_bytes != maximum_alignment {
			return abi_error('invalid_aggregate_layout')
		}
	}
	states[index] = 2
}

fn abi_validate_external_layout_equations(type_store &ssa.TypeStore, graph &AbiTypeGraph, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex) ! {
	mut states := []u8{len: type_store.types.len}
	mut natural_cache := []int{len: type_store.types.len}
	mut natural_ready := []bool{len: type_store.types.len}
	for type_index, is_reachable in graph.reachable_values {
		if !is_reachable {
			continue
		}
		typ := type_store.types[type_index]
		if typ.kind == .struct_t && typ.is_c_struct {
			abi_validate_external_type_equation(type_store, ssa.TypeID(type_index),
				external_layouts, evidence, mut states, mut natural_cache, mut natural_ready)!
		}
	}
}

fn abi_resolve_physical_facts(type_store &ssa.TypeStore, type_id ssa.TypeID, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int) !AbiPhysicalFacts {
	index := int(type_id)
	typ := type_store.types[index]
	if typ.kind == .int_t || typ.kind == .float_t || typ.kind == .ptr_t {
		return abi_scalar_physical_facts(typ)
	}
	if typ.kind == .struct_t && typ.is_c_struct {
		entry_index := evidence.external_by_type[index]
		if entry_index < 0 {
			return abi_error('missing_external_c_layout')
		}
		entry := external_layouts.entries[entry_index]
		return AbiPhysicalFacts{
			size_bytes:          entry.size_bytes
			alignment_bytes:     entry.alignment_bytes
			field_offsets_bytes: entry.field_offsets_bytes.clone()
		}
	}
	layout_index := private_indices[index]
	if layout_index < 0 {
		return abi_error('invalid_type_graph')
	}
	layout := private_layouts[layout_index]
	return AbiPhysicalFacts{
		size_bytes:           layout.size_bytes
		alignment_bytes:      layout.alignment_bytes
		field_offsets_bytes:  layout.field_offsets_bytes.clone()
		element_stride_bytes: layout.element_stride_bytes
	}
}

fn abi_is_physical_start_aligned(type_store &ssa.TypeStore, type_id ssa.TypeID, relative_offset int, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int, mut natural_cache []int, mut natural_ready []bool) !bool {
	// Accepted scalar leaves cap natural alignment at 8; aggregates take child maxima.
	state_count := abi_checked_mul(type_store.types.len, 8)!
	mut visited_congruences := []bool{len: state_count}
	return abi_is_physical_start_aligned_walk(type_store, type_id, relative_offset,
		external_layouts, evidence, private_layouts, private_indices, mut natural_cache,
		mut natural_ready, mut visited_congruences)!
}

fn abi_is_physical_start_aligned_walk(type_store &ssa.TypeStore, type_id ssa.TypeID, relative_offset int, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int, mut natural_cache []int, mut natural_ready []bool, mut visited_congruences []bool) !bool {
	if relative_offset < 0 {
		return abi_error('arithmetic_overflow')
	}
	natural_alignment := abi_natural_alignment(type_store, type_id, mut natural_cache,
		mut natural_ready)!
	if natural_alignment <= 0 || natural_alignment > 8 {
		return abi_error('arithmetic_overflow')
	}
	congruence := relative_offset % natural_alignment
	state_base := abi_checked_mul(int(type_id), 8)!
	state_index := abi_checked_add(state_base, congruence)!
	if state_index >= visited_congruences.len {
		return abi_error('arithmetic_overflow')
	}
	if visited_congruences[state_index] {
		return true
	}
	if congruence != 0 {
		return false
	}
	visited_congruences[state_index] = true
	typ := type_store.types[int(type_id)]
	if typ.kind == .array_t {
		facts := abi_resolve_physical_facts(type_store, type_id, external_layouts, evidence,
			private_layouts, private_indices)!
		element_alignment := abi_natural_alignment(type_store, typ.elem_type, mut natural_cache,
			mut natural_ready)!
		if typ.len > 1 && facts.element_stride_bytes % element_alignment != 0 {
			return false
		}
		return abi_is_physical_start_aligned_walk(type_store, typ.elem_type, relative_offset,
			external_layouts, evidence, private_layouts, private_indices, mut natural_cache,
			mut natural_ready, mut visited_congruences)!
	}
	if typ.kind == .struct_t {
		facts := abi_resolve_physical_facts(type_store, type_id, external_layouts, evidence,
			private_layouts, private_indices)!
		for field_index, field_type in typ.fields {
			field_start := abi_checked_add(relative_offset, facts.field_offsets_bytes[field_index])!
			if !abi_is_physical_start_aligned_walk(type_store, field_type, field_start,
				external_layouts, evidence, private_layouts, private_indices, mut natural_cache,
				mut natural_ready, mut visited_congruences)! {
				return false
			}
		}
	}
	return true
}

fn abi_scalar_facts(profile TargetProfile, typ ssa.Type) !AbiScalarFacts {
	physical := abi_scalar_physical_facts(typ)
	mut semantic_width := 64
	mut transfer_width := 64
	mut semantic_is_unsigned := false
	mut extension := AbiIntegralExtension.none
	mut class := AbiClass.integer
	if typ.kind == .int_t {
		semantic_width = typ.width
		transfer_width = if typ.width == 1 { 8 } else { typ.width }
		if typ.width != 1 {
			semantic_is_unsigned = typ.is_unsigned
		}
		if profile == .macos_x86_64_sysv_macho && typ.width < 32 {
			transfer_width = 32
			if typ.width == 1 || typ.is_unsigned {
				extension = .zero_extend_to_32
			} else {
				extension = .sign_extend_to_32
			}
		}
	} else if typ.kind == .float_t {
		semantic_width = typ.width
		transfer_width = typ.width
		class = .sse
	}
	return AbiScalarFacts{
		size_bytes:              physical.size_bytes
		alignment_bytes:         physical.alignment_bytes
		semantic_width_bits:     semantic_width
		abi_transfer_width_bits: transfer_width
		semantic_is_unsigned:    semantic_is_unsigned
		integral_extension:      extension
		class:                   class
		location_width_bytes:    transfer_width / 8
	}
}

fn abi_merge_class(left AbiClass, right AbiClass) AbiClass {
	if left == .memory || right == .memory {
		return .memory
	}
	if left == .no_class {
		return right
	}
	if right == .no_class {
		return left
	}
	if left == .integer || right == .integer {
		return .integer
	}
	return .sse
}

fn abi_mark_sysv_class(relative_offset int, size_bytes int, class AbiClass, mut slots []AbiClass) ! {
	end := abi_checked_add(relative_offset, size_bytes)!
	if size_bytes <= 0 || end <= relative_offset {
		return abi_error('arithmetic_overflow')
	}
	first_slot := relative_offset / 8
	last_slot := (end - 1) / 8
	if first_slot < 0 || last_slot >= slots.len {
		return abi_error('arithmetic_overflow')
	}
	for slot_index in first_slot .. last_slot + 1 {
		slots[slot_index] = abi_merge_class(slots[slot_index], class)
	}
}

fn abi_fill_sysv_classes(type_store &ssa.TypeStore, type_id ssa.TypeID, relative_offset int, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int, mut slots []AbiClass, mut visited_exact_offsets []bool) ! {
	if relative_offset < 0 || relative_offset >= 16 {
		return abi_error('arithmetic_overflow')
	}
	state_base := abi_checked_mul(int(type_id), 16)!
	state_index := abi_checked_add(state_base, relative_offset)!
	if state_index >= visited_exact_offsets.len {
		return abi_error('arithmetic_overflow')
	}
	if visited_exact_offsets[state_index] {
		return
	}
	visited_exact_offsets[state_index] = true
	typ := type_store.types[int(type_id)]
	if typ.kind == .int_t || typ.kind == .ptr_t {
		facts := abi_scalar_physical_facts(typ)
		abi_mark_sysv_class(relative_offset, facts.size_bytes, .integer, mut slots)!
		return
	}
	if typ.kind == .float_t {
		facts := abi_scalar_physical_facts(typ)
		abi_mark_sysv_class(relative_offset, facts.size_bytes, .sse, mut slots)!
		return
	}
	facts := abi_resolve_physical_facts(type_store, type_id, external_layouts, evidence,
		private_layouts, private_indices)!
	if typ.kind == .array_t {
		for element_index in 0 .. typ.len {
			element_delta := abi_checked_mul(element_index, facts.element_stride_bytes)!
			element_offset := abi_checked_add(relative_offset, element_delta)!
			abi_fill_sysv_classes(type_store, typ.elem_type, element_offset, external_layouts,
				evidence, private_layouts, private_indices, mut slots, mut visited_exact_offsets)!
		}
		return
	}
	for field_index, field_type in typ.fields {
		field_offset := abi_checked_add(relative_offset, facts.field_offsets_bytes[field_index])!
		abi_fill_sysv_classes(type_store, field_type, field_offset, external_layouts,
			evidence, private_layouts, private_indices, mut slots, mut visited_exact_offsets)!
	}
}

fn abi_sysv_aggregate_chunks(type_store &ssa.TypeStore, type_id ssa.TypeID, size_bytes int, external_layouts &AbiLayoutSnapshot, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int) ![]AbiClassChunk {
	mut slots := []AbiClass{len: 2, init: .no_class}
	// This walk is reached only for aggregates of at most 16 bytes.
	state_count := abi_checked_mul(type_store.types.len, 16)!
	mut visited_exact_offsets := []bool{len: state_count}
	abi_fill_sysv_classes(type_store, type_id, 0, external_layouts, evidence,
		private_layouts, private_indices, mut slots, mut visited_exact_offsets)!
	mut chunks := []AbiClassChunk{}
	for slot_index, class in slots {
		if class == .no_class {
			continue
		}
		offset := slot_index * 8
		mut width := size_bytes - offset
		if width > 8 {
			width = 8
		}
		chunks << AbiClassChunk{
			class:              class
			value_offset_bytes: offset
			width_bytes:        width
		}
	}
	return chunks
}

fn abi_microsoft_eligibility(type_id ssa.TypeID, udt_evidence &AbiMicrosoftUdtEvidence, evidence &AbiEvidenceIndex) ! {
	proof_index := evidence.proof_by_type[int(type_id)]
	if proof_index < 0 {
		return abi_error('unknown_microsoft_udt_eligibility')
	}
	eligibility := udt_evidence.proofs[proof_index].eligibility
	if eligibility == .not_applicable || eligibility == .unknown {
		return abi_error('unknown_microsoft_udt_eligibility')
	}
	if eligibility == .ineligible {
		return abi_error('unsupported_microsoft_udt')
	}
}

fn abi_classify_unplaced_value(profile TargetProfile, abi AbiKind, type_store &ssa.TypeStore, type_id ssa.TypeID, is_return bool, external_layouts &AbiLayoutSnapshot, udt_evidence &AbiMicrosoftUdtEvidence, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int, mut natural_cache []int, mut natural_ready []bool) !AbiUnplacedValue {
	typ := type_store.types[int(type_id)]
	if typ.kind == .void_t {
		return AbiUnplacedValue{
			type_id:         type_id
			mode:            .no_value
			alignment_bytes: 1
		}
	}
	if typ.kind == .int_t || typ.kind == .float_t || typ.kind == .ptr_t {
		scalar := abi_scalar_facts(profile, typ)!
		return AbiUnplacedValue{
			type_id:                 type_id
			mode:                    .direct
			size_bytes:              scalar.size_bytes
			alignment_bytes:         scalar.alignment_bytes
			semantic_width_bits:     scalar.semantic_width_bits
			abi_transfer_width_bits: scalar.abi_transfer_width_bits
			semantic_is_unsigned:    scalar.semantic_is_unsigned
			integral_extension:      scalar.integral_extension
			classes:                 [scalar.class]
			chunks:                  [AbiClassChunk{
				class:       scalar.class
				width_bytes: scalar.location_width_bytes
			}]
		}
	}
	facts := abi_resolve_physical_facts(type_store, type_id, external_layouts, evidence,
		private_layouts, private_indices)!
	aligned := abi_is_physical_start_aligned(type_store, type_id, 0, external_layouts,
		evidence, private_layouts, private_indices, mut natural_cache, mut natural_ready)!
	if abi == .sysv_amd64 {
		if !aligned || facts.size_bytes > 16 {
			return AbiUnplacedValue{
				type_id:         type_id
				mode:            if is_return { .indirect } else { .memory_by_value }
				size_bytes:      facts.size_bytes
				alignment_bytes: facts.alignment_bytes
				classes:         [.memory]
			}
		}
		chunks := abi_sysv_aggregate_chunks(type_store, type_id, facts.size_bytes,
			external_layouts, evidence, private_layouts, private_indices)!
		mut classes := []AbiClass{cap: chunks.len}
		mut has_integer := false
		mut has_sse := false
		for chunk in chunks {
			classes << chunk.class
			if chunk.class == .integer {
				has_integer = true
			} else if chunk.class == .sse {
				has_sse = true
			}
		}
		return AbiUnplacedValue{
			type_id:         type_id
			mode:            if has_integer && has_sse { .mixed } else { .direct }
			size_bytes:      facts.size_bytes
			alignment_bytes: facts.alignment_bytes
			classes:         classes
			chunks:          chunks
		}
	}
	if !aligned {
		return abi_error('unsupported_aggregate_layout')
	}
	if typ.kind == .array_t {
		if is_return {
			return abi_error('unsupported_microsoft_array_return')
		}
		return AbiUnplacedValue{
			type_id:         type_id
			mode:            .indirect
			size_bytes:      facts.size_bytes
			alignment_bytes: facts.alignment_bytes
			classes:         [.memory]
		}
	}
	abi_microsoft_eligibility(type_id, udt_evidence, evidence)!
	if facts.size_bytes in [1, 2, 4, 8] {
		return AbiUnplacedValue{
			type_id:         type_id
			mode:            .direct
			size_bytes:      facts.size_bytes
			alignment_bytes: facts.alignment_bytes
			classes:         [.integer]
			chunks:          [AbiClassChunk{
				class:       .integer
				width_bytes: facts.size_bytes
			}]
		}
	}
	return AbiUnplacedValue{
		type_id:         type_id
		mode:            .indirect
		size_bytes:      facts.size_bytes
		alignment_bytes: facts.alignment_bytes
		classes:         [.memory]
	}
}

fn abi_none_location() AbiLocation {
	return AbiLocation{
		kind:     .none
		register: .none
		class:    .no_class
	}
}

fn abi_gpr_location(register AbiRegister, value_offset_bytes int, width_bytes int, has_home bool, caller_home_offset_bytes int, callee_home_offset_bytes int) AbiLocation {
	return AbiLocation{
		kind:                     .gpr
		register:                 register
		class:                    .integer
		value_offset_bytes:       value_offset_bytes
		width_bytes:              width_bytes
		has_home_address:         has_home
		caller_home_offset_bytes: caller_home_offset_bytes
		callee_home_offset_bytes: callee_home_offset_bytes
	}
}

fn abi_xmm_location(register AbiRegister, value_offset_bytes int, width_bytes int, has_home bool, caller_home_offset_bytes int, callee_home_offset_bytes int) AbiLocation {
	return AbiLocation{
		kind:                     .xmm
		register:                 register
		class:                    .sse
		value_offset_bytes:       value_offset_bytes
		width_bytes:              width_bytes
		has_home_address:         has_home
		caller_home_offset_bytes: caller_home_offset_bytes
		callee_home_offset_bytes: callee_home_offset_bytes
	}
}

fn abi_stack_location(class AbiClass, value_offset_bytes int, width_bytes int, caller_offset_bytes int) !AbiLocation {
	callee_offset := abi_checked_add(caller_offset_bytes, 8)!
	return AbiLocation{
		kind:                      .stack
		register:                  .none
		class:                     class
		value_offset_bytes:        value_offset_bytes
		width_bytes:               width_bytes
		has_stack_address:         true
		caller_stack_offset_bytes: caller_offset_bytes
		callee_stack_offset_bytes: callee_offset
	}
}

fn abi_value_decision(value &AbiUnplacedValue, locations []AbiLocation, has_temporary bool, temporary_offset int, temporary_size int, temporary_alignment int) AbiValueDecision {
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
		locations:                          locations.clone()
		has_indirect_temporary:             has_temporary
		indirect_temporary_offset_bytes:    temporary_offset
		indirect_temporary_size_bytes:      temporary_size
		indirect_temporary_alignment_bytes: temporary_alignment
	}
}

fn abi_sysv_argument_gpr(index int) AbiRegister {
	return match index {
		0 { .rdi }
		1 { .rsi }
		2 { .rdx }
		3 { .rcx }
		4 { .r8 }
		5 { .r9 }
		else { .none }
	}
}

fn abi_sysv_argument_xmm(index int) AbiRegister {
	return match index {
		0 { .xmm0 }
		1 { .xmm1 }
		2 { .xmm2 }
		3 { .xmm3 }
		4 { .xmm4 }
		5 { .xmm5 }
		6 { .xmm6 }
		7 { .xmm7 }
		else { .none }
	}
}

fn abi_sysv_return_gpr(index int) AbiRegister {
	return match index {
		0 { .rax }
		1 { .rdx }
		else { .none }
	}
}

fn abi_sysv_return_xmm(index int) AbiRegister {
	return match index {
		0 { .xmm0 }
		1 { .xmm1 }
		else { .none }
	}
}

fn abi_microsoft_gpr(position int) AbiRegister {
	return match position {
		0 { .rcx }
		1 { .rdx }
		2 { .r8 }
		3 { .r9 }
		else { .none }
	}
}

fn abi_microsoft_xmm(position int) AbiRegister {
	return match position {
		0 { .xmm0 }
		1 { .xmm1 }
		2 { .xmm2 }
		3 { .xmm3 }
		else { .none }
	}
}

fn abi_place_return(abi AbiKind, value &AbiUnplacedValue) !AbiValueDecision {
	if value.mode == .no_value || value.mode == .indirect {
		return abi_value_decision(value, []AbiLocation{}, false, 0, 0, 0)
	}
	if value.mode != .direct && value.mode != .mixed {
		return abi_error('invalid_type_graph')
	}
	mut locations := []AbiLocation{cap: value.chunks.len}
	mut integer_index := 0
	mut sse_index := 0
	for chunk in value.chunks {
		if chunk.class == .integer {
			register := if abi == .sysv_amd64 {
				abi_sysv_return_gpr(integer_index)
			} else {
				AbiRegister.rax
			}
			if register == .none {
				return abi_error('arithmetic_overflow')
			}
			locations << abi_gpr_location(register, chunk.value_offset_bytes,
				chunk.width_bytes, false, 0, 0)
			integer_index++
		} else if chunk.class == .sse {
			register := if abi == .sysv_amd64 {
				abi_sysv_return_xmm(sse_index)
			} else {
				AbiRegister.xmm0
			}
			if register == .none {
				return abi_error('arithmetic_overflow')
			}
			locations << abi_xmm_location(register, chunk.value_offset_bytes,
				chunk.width_bytes, false, 0, 0)
			sse_index++
		} else {
			return abi_error('invalid_type_graph')
		}
	}
	return abi_value_decision(value, locations, false, 0, 0, 0)
}

fn abi_hidden_sret(abi AbiKind, present bool) AbiHiddenSretDecision {
	if !present {
		return AbiHiddenSretDecision{
			input: abi_none_location()
			echo:  abi_none_location()
		}
	}
	if abi == .sysv_amd64 {
		return AbiHiddenSretDecision{
			present: true
			input:   abi_gpr_location(.rdi, 0, 8, false, 0, 0)
			echo:    abi_gpr_location(.rax, 0, 8, false, 0, 0)
		}
	}
	return AbiHiddenSretDecision{
		present: true
		input:   abi_gpr_location(.rcx, 0, 8, true, 0, 8)
		echo:    abi_gpr_location(.rax, 0, 8, false, 0, 0)
	}
}

fn abi_count_chunk_banks(value &AbiUnplacedValue) !(int, int) {
	mut integer_count := 0
	mut sse_count := 0
	for chunk in value.chunks {
		if chunk.class == .integer {
			integer_count++
		} else if chunk.class == .sse {
			sse_count++
		} else {
			return abi_error('invalid_type_graph')
		}
	}
	return integer_count, sse_count
}

fn abi_place_sysv_parameters(values []AbiUnplacedValue, has_sret bool) !AbiParameterPlacement {
	mut decisions := []AbiValueDecision{cap: values.len}
	mut integer_index := if has_sret { 1 } else { 0 }
	mut sse_index := 0
	mut stack_cursor := 0
	for value in values {
		if value.mode == .memory_by_value {
			stack_cursor = abi_checked_align_up(stack_cursor, 8)!
			location := abi_stack_location(.memory, 0, value.size_bytes, stack_cursor)!
			decisions << abi_value_decision(&value, [location], false, 0, 0, 0)
			span := abi_checked_align_up(value.size_bytes, 8)!
			stack_cursor = abi_checked_add(stack_cursor, span)!
			continue
		}
		if value.mode != .direct && value.mode != .mixed {
			return abi_error('invalid_type_graph')
		}
		needed_integer, needed_sse := abi_count_chunk_banks(&value)!
		fits_registers := integer_index <= 6 - needed_integer && sse_index <= 8 - needed_sse
		mut locations := []AbiLocation{cap: value.chunks.len}
		if fits_registers {
			mut next_integer := integer_index
			mut next_sse := sse_index
			for chunk in value.chunks {
				if chunk.class == .integer {
					register := abi_sysv_argument_gpr(next_integer)
					if register == .none {
						return abi_error('arithmetic_overflow')
					}
					locations << abi_gpr_location(register, chunk.value_offset_bytes,
						chunk.width_bytes, false, 0, 0)
					next_integer++
				} else if chunk.class == .sse {
					register := abi_sysv_argument_xmm(next_sse)
					if register == .none {
						return abi_error('arithmetic_overflow')
					}
					locations << abi_xmm_location(register, chunk.value_offset_bytes,
						chunk.width_bytes, false, 0, 0)
					next_sse++
				} else {
					return abi_error('invalid_type_graph')
				}
			}
			integer_index = next_integer
			sse_index = next_sse
		} else {
			stack_cursor = abi_checked_align_up(stack_cursor, 8)!
			for chunk in value.chunks {
				if chunk.class != .integer && chunk.class != .sse {
					return abi_error('invalid_type_graph')
				}
				chunk_stack_offset := abi_checked_add(stack_cursor, chunk.value_offset_bytes)!
				locations << abi_stack_location(chunk.class, chunk.value_offset_bytes,
					chunk.width_bytes, chunk_stack_offset)!
			}
			span := abi_checked_align_up(value.size_bytes, 8)!
			stack_cursor = abi_checked_add(stack_cursor, span)!
		}
		decisions << abi_value_decision(&value, locations, false, 0, 0, 0)
	}
	return AbiParameterPlacement{
		decisions:          decisions
		outgoing_area_bytes: stack_cursor
	}
}

fn abi_microsoft_input_location(position int, class AbiClass, value_offset_bytes int, width_bytes int) !AbiLocation {
	position_offset := abi_checked_mul(position, 8)!
	if position < 4 {
		callee_home := abi_checked_add(position_offset, 8)!
		if class == .sse {
			return abi_xmm_location(abi_microsoft_xmm(position), value_offset_bytes,
				width_bytes, true, position_offset, callee_home)
		}
		return abi_gpr_location(abi_microsoft_gpr(position), value_offset_bytes,
			width_bytes, true, position_offset, callee_home)
	}
	return abi_stack_location(class, value_offset_bytes, width_bytes, position_offset)!
}

fn abi_place_microsoft_parameters(values []AbiUnplacedValue, has_sret bool) !AbiParameterPlacement {
	mut decisions := []AbiValueDecision{cap: values.len}
	mut position := if has_sret { 1 } else { 0 }
	mut outgoing_high_water := 32
	mut temporary_cursor := 0
	mut temporary_high_water := 0
	for value in values {
		position_offset := abi_checked_mul(position, 8)!
		position_end := abi_checked_add(position_offset, 8)!
		if position_end > outgoing_high_water {
			outgoing_high_water = position_end
		}
		if value.mode == .indirect {
			temporary_offset := abi_checked_align_up(temporary_cursor, 16)!
			temporary_end := abi_checked_add(temporary_offset, value.size_bytes)!
			temporary_cursor = temporary_end
			if temporary_end > temporary_high_water {
				temporary_high_water = temporary_end
			}
			location := abi_microsoft_input_location(position, .integer, 0, 8)!
			decisions << abi_value_decision(&value, [location], true, temporary_offset,
				value.size_bytes, 16)
		} else if value.mode == .direct {
			if value.chunks.len != 1 {
				return abi_error('invalid_type_graph')
			}
			chunk := value.chunks[0]
			location := abi_microsoft_input_location(position, chunk.class,
				chunk.value_offset_bytes, chunk.width_bytes)!
			decisions << abi_value_decision(&value, [location], false, 0, 0, 0)
		} else {
			return abi_error('invalid_type_graph')
		}
		position = abi_checked_add(position, 1)!
	}
	mut indirect_area := 0
	if temporary_high_water > 0 {
		indirect_area = abi_checked_align_up(temporary_high_water, 16)!
	}
	return AbiParameterPlacement{
		decisions:           decisions
		outgoing_area_bytes: outgoing_high_water
		indirect_area_bytes: indirect_area
	}
}

fn abi_classify_and_place(profile TargetProfile, abi AbiKind, call_kind AbiCallKind, type_store &ssa.TypeStore, external_layouts &AbiLayoutSnapshot, udt_evidence &AbiMicrosoftUdtEvidence, function_type ssa.TypeID, graph &AbiTypeGraph, evidence &AbiEvidenceIndex, private_layouts []AbiV3PhysicalLayout, private_indices []int) !AbiFunctionDecision {
	mut natural_cache := []int{len: type_store.types.len}
	mut natural_ready := []bool{len: type_store.types.len}
	return_unplaced := abi_classify_unplaced_value(profile, abi, type_store, graph.return_root,
		true, external_layouts, udt_evidence, evidence, private_layouts, private_indices,
		mut natural_cache, mut natural_ready)!
	mut parameter_unplaced := []AbiUnplacedValue{cap: graph.parameter_roots.len}
	for parameter_type in graph.parameter_roots {
		parameter_unplaced << abi_classify_unplaced_value(profile, abi, type_store,
			parameter_type, false, external_layouts, udt_evidence, evidence, private_layouts,
			private_indices, mut natural_cache, mut natural_ready)!
	}
	return_value := abi_place_return(abi, &return_unplaced)!
	has_sret := return_unplaced.mode == .indirect
	hidden_sret := abi_hidden_sret(abi, has_sret)
	placement := if abi == .sysv_amd64 {
		abi_place_sysv_parameters(parameter_unplaced, has_sret)!
	} else {
		abi_place_microsoft_parameters(parameter_unplaced, has_sret)!
	}
	return AbiFunctionDecision{
		profile:                               profile
		abi:                                   abi
		call_kind:                             call_kind
		function_type:                         function_type
		return_value:                          return_value
		parameters:                            placement.decisions.clone()
		hidden_sret:                           hidden_sret
		shadow_space_bytes:                    if abi == .microsoft_x64 { 32 } else { 0 }
		red_zone_bytes:                        if abi == .sysv_amd64 { 128 } else { 0 }
		minimum_outgoing_area_bytes:           placement.outgoing_area_bytes
		minimum_indirect_temporary_area_bytes: placement.indirect_area_bytes
		pre_call_stack_alignment_bytes:        16
	}
}
