module amd64

import v3.ssa

const memory_frame_compose_max_slots = 1024
const memory_frame_compose_max_requested_bytes = u64(0x7ffffff8)

pub struct MemoryFrameCallExtentFacts {
pub:
	present           bool
	function_id       u32
	profile           TargetProfile
	has_call          bool
	call_extent_bytes u64
}

pub enum MemoryFrameComposedSlotOrigin {
	scalar_fixed_alloca
	aggregate_fixed_alloca
	aggregate_temp
}

pub struct MemoryFrameComposedSlotBinding {
pub:
	origin           MemoryFrameComposedSlotOrigin
	source_index     u32
	frame_slot_index u32
}

pub struct MemoryFrameCompositionPlan {
pub:
	memory        MemoryAggPlan
	frame         MemoryFrameCfiInstructionFragmentPlan
	slot_bindings []MemoryFrameComposedSlotBinding
}

struct MemoryFrameComposePreflight {
	scalar_slot_count    u32
	aggregate_slot_count u32
	total_slot_count     u32
	total_requested_bytes u64
}

struct MemoryFrameComposeSource {
	origin       MemoryFrameComposedSlotOrigin
	source_index u32
	request      MemorySlotRequest
}

fn memory_frame_compose_error(message string) IError {
	return error('amd64 memory frame compose: ${message}')
}

fn memory_frame_compose_profile_is_valid(profile TargetProfile) bool {
	value := int(profile)
	return value == int(TargetProfile.linux_x86_64_sysv_elf)
		|| value == int(TargetProfile.macos_x86_64_sysv_macho)
		|| value == int(TargetProfile.windows_x86_64_microsoft_abi_coff)
}

fn memory_frame_compose_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_frame_compose_error('arithmetic overflow')
	}
	return left + right
}

fn memory_frame_compose_preflight(memory &MemoryAggPlan) !MemoryFrameComposePreflight {
	scalar_count := u64(memory.slot_requests.len)
	aggregate_count := u64(memory.aggregate_slots.len)
	total_count := memory_frame_compose_checked_add(scalar_count, aggregate_count)!
	if total_count > memory_frame_compose_max_slots {
		return memory_frame_compose_error('merged slot count ${total_count} exceeds ${memory_frame_compose_max_slots}')
	}

	mut total_bytes := u64(0)
	for slot in memory.slot_requests {
		total_bytes = memory_frame_compose_checked_add(total_bytes, slot.request.size_bytes)!
	}
	for slot in memory.aggregate_slots {
		total_bytes = memory_frame_compose_checked_add(total_bytes, slot.request.size_bytes)!
	}
	if total_bytes > memory_frame_compose_max_requested_bytes {
		return memory_frame_compose_error('requested bytes ${total_bytes} exceed ${memory_frame_compose_max_requested_bytes}')
	}
	if total_bytes != memory.total_requested_bytes {
		return memory_frame_compose_error('requested-byte invariant failed')
	}
	return MemoryFrameComposePreflight{
		scalar_slot_count:    u32(scalar_count)
		aggregate_slot_count: u32(aggregate_count)
		total_slot_count:     u32(total_count)
		total_requested_bytes: total_bytes
	}
}

fn memory_frame_compose_materialize_sources(memory &MemoryAggPlan, preflight MemoryFrameComposePreflight) ![]MemoryFrameComposeSource {
	mut sources := []MemoryFrameComposeSource{cap: int(preflight.total_slot_count)}
	mut ids := map[int]bool{}
	for source_index, slot in memory.slot_requests {
		if slot.request.kind != .fixed_alloca {
			return memory_frame_compose_error('scalar slot ${source_index} is not fixed_alloca')
		}
		if int(slot.request.id) in ids {
			return memory_frame_compose_error('duplicate logical slot id ${slot.request.id}')
		}
		ids[int(slot.request.id)] = true
		sources << MemoryFrameComposeSource{
			origin:       .scalar_fixed_alloca
			source_index: u32(source_index)
			request:      slot.request
		}
	}
	for source_index, slot in memory.aggregate_slots {
		role_value := int(slot.role)
		if role_value < int(MemoryAggAggregateSlotRole.fixed_alloca)
			|| role_value > int(MemoryAggAggregateSlotRole.aggregate_temp) {
			return memory_frame_compose_error('aggregate slot ${source_index} has unsupported role')
		}
		origin, expected_kind := match slot.role {
			.fixed_alloca {
				MemoryFrameComposedSlotOrigin.aggregate_fixed_alloca, MemorySlotKind.fixed_alloca
			}
			.aggregate_temp {
				MemoryFrameComposedSlotOrigin.aggregate_temp, MemorySlotKind.aggregate_temp
			}
		}
		if slot.request.kind != expected_kind {
			return memory_frame_compose_error('aggregate slot ${source_index} role/kind mismatch')
		}
		if int(slot.request.id) in ids {
			return memory_frame_compose_error('duplicate logical slot id ${slot.request.id}')
		}
		ids[int(slot.request.id)] = true
		sources << MemoryFrameComposeSource{
			origin:       origin
			source_index: u32(source_index)
			request:      slot.request
		}
	}
	if sources.len != int(preflight.total_slot_count)
		|| preflight.scalar_slot_count != u32(memory.slot_requests.len)
		|| preflight.aggregate_slot_count != u32(memory.aggregate_slots.len) {
		return memory_frame_compose_error('source count invariant failed')
	}
	return sources
}

fn memory_frame_compose_requests(sources []MemoryFrameComposeSource) []MemorySlotRequest {
	mut requests := []MemorySlotRequest{cap: sources.len}
	for source in sources {
		requests << source.request
	}
	return requests
}

fn memory_frame_compose_resolve_slot(slot_id u32, frame_indices map[int]u32, context string) ! {
	if int(slot_id) !in frame_indices {
		return memory_frame_compose_error('${context} references missing logical slot ${slot_id}')
	}
}

fn memory_frame_compose_validate_endpoints(memory &MemoryAggPlan, frame_indices map[int]u32) ! {
	for index, pointer in memory.pointers {
		memory_frame_compose_resolve_slot(pointer.root_slot_id, frame_indices,
			'pointer ${index}')!
	}
	for index, access in memory.accesses {
		memory_frame_compose_resolve_slot(access.root_slot_id, frame_indices,
			'access ${index}')!
	}
	for index, snapshot in memory.aggregate_snapshots {
		memory_frame_compose_resolve_slot(snapshot.root_slot_id, frame_indices,
			'aggregate snapshot ${index}')!
	}
	for index, action in memory.aggregate_actions {
		match action.kind {
			.zero {
				memory_frame_compose_resolve_slot(action.destination_slot_id, frame_indices,
					'zero action ${index} destination')!
			}
			.copy {
				memory_frame_compose_resolve_slot(action.source_slot_id, frame_indices,
					'copy action ${index} source')!
				memory_frame_compose_resolve_slot(action.destination_slot_id, frame_indices,
					'copy action ${index} destination')!
			}
			.scalar_read {
				memory_frame_compose_resolve_slot(action.source_slot_id, frame_indices,
					'scalar-read action ${index} source')!
			}
			.scalar_write {
				memory_frame_compose_resolve_slot(action.destination_slot_id, frame_indices,
					'scalar-write action ${index} destination')!
			}
		}
	}
}

fn memory_frame_compose_bind(memory &MemoryAggPlan, frame &MemoryFrameCfiInstructionFragmentPlan, sources []MemoryFrameComposeSource) ![]MemoryFrameComposedSlotBinding {
	encoded_slots := frame.cfi.frame.slots
	geometry_slots := frame.cfi.frame.layout_frame.slots
	if encoded_slots.len != sources.len || geometry_slots.len != sources.len {
		return memory_frame_compose_error('frame/source slot count mismatch')
	}

	mut source_indices := map[int]int{}
	for source_index, source in sources {
		if int(source.request.id) in source_indices {
			return memory_frame_compose_error('duplicate logical slot id ${source.request.id}')
		}
		source_indices[int(source.request.id)] = source_index
	}
	mut frame_indices := map[int]u32{}
	mut bindings := []MemoryFrameComposedSlotBinding{cap: sources.len}
	for frame_slot_index, encoded in encoded_slots {
		id := encoded.placement.id
		if int(id) in frame_indices {
			return memory_frame_compose_error('duplicate frame slot id ${id}')
		}
		source_index := source_indices[int(id)] or {
			return memory_frame_compose_error('frame slot ${id} has no logical source')
		}
		source := sources[source_index]
		if encoded.source_placement_index != u32(frame_slot_index)
			|| encoded.placement != geometry_slots[frame_slot_index]
			|| encoded.placement.kind != source.request.kind
			|| encoded.placement.size_bytes != source.request.size_bytes
			|| encoded.placement.alignment_bytes != source.request.alignment_bytes {
			return memory_frame_compose_error('frame slot ${id} does not match its logical source')
		}
		frame_indices[int(id)] = u32(frame_slot_index)
		bindings << MemoryFrameComposedSlotBinding{
			origin:           source.origin
			source_index:     source.source_index
			frame_slot_index: u32(frame_slot_index)
		}
	}
	if frame_indices.len != sources.len || bindings.len != sources.len {
		return memory_frame_compose_error('slot bijection invariant failed')
	}
	memory_frame_compose_validate_endpoints(memory, frame_indices)!
	return bindings
}

fn memory_frame_compose_validate_output(plan &MemoryFrameCompositionPlan, preflight MemoryFrameComposePreflight) ! {
	if plan.slot_bindings.len != int(preflight.total_slot_count)
		|| plan.memory.total_requested_bytes != preflight.total_requested_bytes
		|| plan.frame.cfi.frame.layout_frame.function_id != plan.memory.function_id
		|| plan.frame.cfi.frame.layout_frame.profile != plan.memory.profile {
		return memory_frame_compose_error('output invariant failed')
	}
	for index, binding in plan.slot_bindings {
		if binding.frame_slot_index != u32(index) {
			return memory_frame_compose_error('binding order invariant failed')
		}
		match binding.origin {
			.scalar_fixed_alloca {
				if int(binding.source_index) >= plan.memory.slot_requests.len {
					return memory_frame_compose_error('scalar binding source index is invalid')
				}
			}
			.aggregate_fixed_alloca, .aggregate_temp {
				if int(binding.source_index) >= plan.memory.aggregate_slots.len {
					return memory_frame_compose_error('aggregate binding source index is invalid')
				}
			}
		}
	}
}

// plan_scalar_static_memory_frame composes closed logical memory and frame records without activation.
pub fn plan_scalar_static_memory_frame(m &ssa.Module, memory_facts &MemoryAggFunctionFacts, call_facts &MemoryFrameCallExtentFacts, saves &MemoryCalleeSaveFacts) !MemoryFrameCompositionPlan {
	if !call_facts.present {
		return memory_frame_compose_error('call-extent facts are required')
	}
	if !memory_frame_compose_profile_is_valid(call_facts.profile) {
		return memory_frame_compose_error('unsupported call-extent target profile')
	}

	memory := plan_scalar_static_memory(m, memory_facts)!
	if call_facts.function_id != memory.function_id {
		return memory_frame_compose_error('call-extent function ${call_facts.function_id} does not match memory function ${memory.function_id}')
	}
	if call_facts.profile != memory.profile {
		return memory_frame_compose_error('call-extent profile does not match memory profile')
	}

	preflight := memory_frame_compose_preflight(&memory)!
	sources := memory_frame_compose_materialize_sources(&memory, preflight)!
	frame_facts := MemoryFunctionFrameFacts{
		function_id:       memory.function_id
		profile:           memory.profile
		extent_kind:       .fixed
		call_extent_bytes: call_facts.call_extent_bytes
		has_call:          call_facts.has_call
		slots:             memory_frame_compose_requests(sources)
	}
	frame := plan_memory_saved_frame_cfi_instruction_fragments(&frame_facts, saves)!
	bindings := memory_frame_compose_bind(&memory, &frame, sources)!
	result := MemoryFrameCompositionPlan{
		memory:        memory
		frame:         frame
		slot_bindings: bindings
	}
	memory_frame_compose_validate_output(&result, preflight)!
	return result
}
