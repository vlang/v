module amd64

const memory_frame_max_slots = 1024
const memory_frame_red_zone_bytes = u64(128)
const memory_frame_probe_threshold_bytes = u64(4096)
const memory_frame_max_stack_adjustment_bytes = u64(0x7ffffff8)

pub enum MemoryFrameExtentKind {
	fixed
	dynamic
}

pub enum MemoryRedZonePolicy {
	abi_default
	forbidden
}

pub enum MemorySlotKind {
	local
	spill
	fixed_alloca
	aggregate_temp
}

pub enum MemorySlotBasis {
	body_rsp
	entry_rsp
}

pub struct MemorySlotRequest {
pub:
	id              u32
	kind            MemorySlotKind
	size_bytes      u64
	alignment_bytes u64
}

pub struct MemoryFunctionFrameFacts {
pub:
	function_id       u32
	profile           TargetProfile
	extent_kind       MemoryFrameExtentKind
	call_extent_bytes u64
	has_call          bool
	slots             []MemorySlotRequest
}

pub struct MemorySlotPlacement {
pub:
	id                 u32
	kind               MemorySlotKind
	basis              MemorySlotBasis
	displacement_bytes i64
	size_bytes         u64
	alignment_bytes    u64
}

pub struct MemoryStackTranslations {
pub:
	entry_to_body_subtract_bytes u64
	incoming_from_body_add_bytes u64
	outgoing_from_body_add_bytes u64
}

pub struct MemoryFramePlan {
pub:
	function_id                  u32
	profile                      TargetProfile
	extent_kind                  MemoryFrameExtentKind
	call_extent_bytes            u64
	has_call                     bool
	uses_red_zone                bool
	red_zone_extent_bytes        u64
	non_red_zone_extent_bytes    u64
	stack_adjustment_bytes       u64
	probe_required               bool
	translations                 MemoryStackTranslations
	slots                        []MemorySlotPlacement
	red_zone_policy              MemoryRedZonePolicy
}

struct MemoryFrameUnsignedPlacement {
	request          MemorySlotRequest
	coordinate_bytes u64
}

fn memory_frame_error(message string) IError {
	return error('amd64 memory frame: ${message}')
}

fn memory_frame_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_frame_error('arithmetic overflow')
	}
	return left + right
}

fn memory_frame_checked_align_up(value u64, alignment u64) !u64 {
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return memory_frame_checked_add(value, alignment - remainder)
}

fn memory_frame_profile_is_sysv(profile TargetProfile) bool {
	value := int(profile)
	return value == int(TargetProfile.linux_x86_64_sysv_elf)
		|| value == int(TargetProfile.macos_x86_64_sysv_macho)
}

fn memory_frame_profile_is_windows(profile TargetProfile) bool {
	return int(profile) == int(TargetProfile.windows_x86_64_microsoft_abi_coff)
}

fn memory_frame_profile_is_valid(profile TargetProfile) bool {
	return memory_frame_profile_is_sysv(profile) || memory_frame_profile_is_windows(profile)
}

fn memory_frame_extent_kind_is_valid(kind MemoryFrameExtentKind) bool {
	value := int(kind)
	return value == int(MemoryFrameExtentKind.fixed)
		|| value == int(MemoryFrameExtentKind.dynamic)
}

fn memory_frame_slot_kind_is_valid(kind MemorySlotKind) bool {
	value := int(kind)
	return value >= int(MemorySlotKind.local)
		&& value <= int(MemorySlotKind.aggregate_temp)
}

fn memory_frame_red_zone_policy_is_valid(policy MemoryRedZonePolicy) bool {
	value := int(policy)
	return value == int(MemoryRedZonePolicy.abi_default)
		|| value == int(MemoryRedZonePolicy.forbidden)
}

fn memory_frame_alignment_is_valid(alignment u64) bool {
	return alignment > 0 && alignment <= 16 && alignment & (alignment - 1) == 0
}

fn memory_frame_validate_raw_domains(facts &MemoryFunctionFrameFacts) ! {
	if !memory_frame_profile_is_valid(facts.profile) {
		return memory_frame_error('unsupported target profile')
	}
	if !memory_frame_extent_kind_is_valid(facts.extent_kind) {
		return memory_frame_error('unsupported frame extent kind')
	}
	for index, request in facts.slots {
		if !memory_frame_slot_kind_is_valid(request.kind) {
			return memory_frame_error('slot ${index} has unsupported kind')
		}
	}
}

fn memory_frame_validate_call_facts(facts &MemoryFunctionFrameFacts) ! {
	if !facts.has_call {
		if facts.call_extent_bytes != 0 {
			return memory_frame_error('noncaller call extent must be zero')
		}
		return
	}
	if memory_frame_profile_is_windows(facts.profile) && facts.call_extent_bytes < 32 {
		return memory_frame_error('Microsoft call extent ${facts.call_extent_bytes} is below 32')
	}
	if facts.call_extent_bytes % 8 != 0 {
		return memory_frame_error('call extent ${facts.call_extent_bytes} is not a multiple of 8')
	}
}

fn memory_frame_validate_requests(facts &MemoryFunctionFrameFacts) ! {
	for index, request in facts.slots {
		if request.size_bytes == 0 {
			return memory_frame_error('slot ${index} id ${request.id} size must be positive')
		}
		if !memory_frame_alignment_is_valid(request.alignment_bytes) {
			return memory_frame_error('slot ${index} id ${request.id} alignment ${request.alignment_bytes} is invalid')
		}
	}
	for right := 1; right < facts.slots.len; right++ {
		for left in 0 .. right {
			if facts.slots[left].id == facts.slots[right].id {
				return memory_frame_error('duplicate slot id ${facts.slots[right].id}')
			}
		}
	}
}

fn memory_frame_request_less(left MemorySlotRequest, right MemorySlotRequest) bool {
	left_kind := int(left.kind)
	right_kind := int(right.kind)
	return left_kind < right_kind || (left_kind == right_kind && left.id < right.id)
}

fn memory_frame_sorted_requests(requests []MemorySlotRequest) []MemorySlotRequest {
	mut ordered := requests.clone()
	for index := 1; index < ordered.len; index++ {
		current := ordered[index]
		mut insertion := index
		for insertion > 0 && memory_frame_request_less(current, ordered[insertion - 1]) {
			ordered[insertion] = ordered[insertion - 1]
			insertion--
		}
		ordered[insertion] = current
	}
	return ordered
}

fn memory_frame_place_ordinary(requests []MemorySlotRequest, call_extent u64, mut placements []MemoryFrameUnsignedPlacement) !u64 {
	mut cursor := call_extent
	for request in requests {
		offset := memory_frame_checked_align_up(cursor, request.alignment_bytes)!
		end := memory_frame_checked_add(offset, request.size_bytes)!
		placements << MemoryFrameUnsignedPlacement{
			request:          request
			coordinate_bytes: offset
		}
		cursor = end
	}
	return cursor
}

fn memory_frame_red_depth(need u64, alignment u64) !u64 {
	residue := u64(8) % alignment
	remainder := need % alignment
	padding := if remainder <= residue {
		residue - remainder
	} else {
		alignment - (remainder - residue)
	}
	return memory_frame_checked_add(need, padding)
}

fn memory_frame_place_red_zone(requests []MemorySlotRequest, mut placements []MemoryFrameUnsignedPlacement) !u64 {
	mut previous_depth := u64(0)
	for request in requests {
		need := memory_frame_checked_add(previous_depth, request.size_bytes)!
		depth := memory_frame_red_depth(need, request.alignment_bytes)!
		if depth > memory_frame_red_zone_bytes {
			return memory_frame_error('red zone unavailable')
		}
		if depth % request.alignment_bytes != u64(8) % request.alignment_bytes
			|| depth < request.size_bytes
			|| depth - request.size_bytes < previous_depth {
			return memory_frame_error('internal invariant failed')
		}
		placements << MemoryFrameUnsignedPlacement{
			request:          request
			coordinate_bytes: depth
		}
		previous_depth = depth
	}
	return previous_depth
}

fn memory_frame_stack_adjustment(extent u64, has_call bool) !u64 {
	if extent == 0 && !has_call {
		return 0
	}
	with_entry_residue := memory_frame_checked_add(extent, 8)!
	aligned := memory_frame_checked_align_up(with_entry_residue, 16)!
	if aligned < 8 {
		return memory_frame_error('internal invariant failed')
	}
	adjustment := aligned - 8
	if adjustment > memory_frame_max_stack_adjustment_bytes {
		return memory_frame_error('stack adjustment ${adjustment} exceeds ${memory_frame_max_stack_adjustment_bytes}')
	}
	if adjustment < extent || adjustment % 16 != 8 {
		return memory_frame_error('internal invariant failed')
	}
	return adjustment
}

fn memory_frame_materialize(placements []MemoryFrameUnsignedPlacement, basis MemorySlotBasis, extent u64) ![]MemorySlotPlacement {
	mut result := []MemorySlotPlacement{cap: placements.len}
	for placement in placements {
		if placement.coordinate_bytes > memory_frame_max_stack_adjustment_bytes {
			return memory_frame_error('internal invariant failed')
		}
		request := placement.request
		mut displacement := i64(placement.coordinate_bytes)
		if basis == .entry_rsp {
			if placement.coordinate_bytes == 0 || placement.coordinate_bytes > memory_frame_red_zone_bytes {
				return memory_frame_error('internal invariant failed')
			}
			displacement = -displacement
		} else {
			end := memory_frame_checked_add(placement.coordinate_bytes, request.size_bytes)!
			if end > extent || placement.coordinate_bytes % request.alignment_bytes != 0 {
				return memory_frame_error('internal invariant failed')
			}
		}
		result << MemorySlotPlacement{
			id:                 request.id
			kind:               request.kind
			basis:              basis
			displacement_bytes: displacement
			size_bytes:         request.size_bytes
			alignment_bytes:    request.alignment_bytes
		}
	}
	return result
}

fn memory_frame_translations(adjustment u64) MemoryStackTranslations {
	return MemoryStackTranslations{
		entry_to_body_subtract_bytes: adjustment
		incoming_from_body_add_bytes: adjustment
		outgoing_from_body_add_bytes: 0
	}
}

fn memory_frame_build_plan(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy, uses_red_zone bool, red_extent u64, ordinary_extent u64, adjustment u64, placements []MemorySlotPlacement) MemoryFramePlan {
	return MemoryFramePlan{
		function_id:               facts.function_id
		profile:                   facts.profile
		extent_kind:               facts.extent_kind
		call_extent_bytes:         facts.call_extent_bytes
		has_call:                  facts.has_call
		uses_red_zone:             uses_red_zone
		red_zone_extent_bytes:     red_extent
		non_red_zone_extent_bytes: ordinary_extent
		stack_adjustment_bytes:    adjustment
		probe_required:            memory_frame_profile_is_windows(facts.profile)
			&& adjustment >= memory_frame_probe_threshold_bytes
		translations:              memory_frame_translations(adjustment)
		slots:                     placements.clone()
		red_zone_policy:           policy
	}
}

fn memory_frame_plan_with_red_zone_policy(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) !MemoryFramePlan {
	memory_frame_validate_raw_domains(facts)!
	if !memory_frame_red_zone_policy_is_valid(policy) {
		return memory_frame_error('unsupported red-zone policy')
	}
	if facts.slots.len > memory_frame_max_slots {
		return memory_frame_error('slot count ${facts.slots.len} exceeds ${memory_frame_max_slots}')
	}
	if facts.extent_kind != .fixed {
		return memory_frame_error('dynamic frame extent is unsupported')
	}
	memory_frame_validate_call_facts(facts)!
	memory_frame_validate_requests(facts)!

	ordered := memory_frame_sorted_requests(facts.slots)
	red_eligible := policy == .abi_default && memory_frame_profile_is_sysv(facts.profile)
		&& !facts.has_call && facts.call_extent_bytes == 0 && ordered.len != 0
	if red_eligible {
		mut red_unsigned := []MemoryFrameUnsignedPlacement{cap: ordered.len}
		if red_extent := memory_frame_place_red_zone(ordered, mut red_unsigned) {
			mut ordinary_for_record := []MemoryFrameUnsignedPlacement{cap: ordered.len}
			ordinary_extent := memory_frame_place_ordinary(ordered, 0, mut ordinary_for_record)!
			red_slots := memory_frame_materialize(red_unsigned, .entry_rsp, red_extent)!
			return memory_frame_build_plan(facts, policy, true, red_extent, ordinary_extent,
				0, red_slots)
		}
	}

	mut ordinary_unsigned := []MemoryFrameUnsignedPlacement{cap: ordered.len}
	ordinary_extent := memory_frame_place_ordinary(ordered, facts.call_extent_bytes,
		mut ordinary_unsigned)!
	adjustment := memory_frame_stack_adjustment(ordinary_extent, facts.has_call)!
	ordinary_slots := memory_frame_materialize(ordinary_unsigned, .body_rsp, ordinary_extent)!
	return memory_frame_build_plan(facts, policy, false, 0, ordinary_extent, adjustment,
		ordinary_slots)
}

// plan_memory_frame_with_red_zone_policy produces an inert policy-attested frame snapshot.
pub fn plan_memory_frame_with_red_zone_policy(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) !MemoryFramePlan {
	return memory_frame_plan_with_red_zone_policy(facts, policy)
}

// plan_memory_frame produces the legacy ABI-default frame-layout snapshot.
pub fn plan_memory_frame(facts &MemoryFunctionFrameFacts) !MemoryFramePlan {
	return plan_memory_frame_with_red_zone_policy(facts, .abi_default)
}
