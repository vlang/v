module amd64

const memory_saved_frame_max_saves = 8
const memory_saved_frame_max_prologue_bytes = 25
const memory_saved_frame_max_epilogue_bytes = 19
const memory_saved_frame_max_unwind_slots = 11
const memory_saved_frame_max_xdata_bytes = 28
const memory_saved_frame_max_fixups = 1
const memory_saved_frame_max_displacement_bytes = u64(4096)
const memory_saved_frame_max_byte_slice_payload = u64(4168)
const memory_saved_frame_max_allocation_bytes = u64(0x7ffffff8)
const memory_saved_frame_probe_threshold_bytes = u64(4096)

pub enum MemorySavedGpr {
	rbx
	rbp
	rsi
	rdi
	r12
	r13
	r14
	r15
}

pub struct MemoryCalleeSaveFacts {
pub:
	present     bool
	function_id u32
	registers   []MemorySavedGpr
}

pub struct MemorySavedRegisterPlan {
pub:
	source_request_index             u32
	register                         MemorySavedGpr
	register_encoding                u8
	push_offset_bytes                u8
	push_end_offset_bytes            u8
	push_width_bytes                 u8
	pop_offset_bytes                 u8
	pop_end_offset_bytes             u8
	pop_width_bytes                  u8
	windows_unwind_present           bool
	windows_unwind_code_offset_bytes u8
}

pub struct MemorySavedFrameWindowsUnwindPlan {
pub:
	present                      bool
	allocation_kind              MemoryWindowsUnwindKind
	allocation_code_present      bool
	allocation_bytes             u64
	size_of_prolog_bytes         u8
	allocation_code_offset_bytes u8
	count_of_codes               u8
	xdata_bytes                  []u8
}

pub struct MemorySavedFramePlan {
pub:
	layout_frame                 MemoryFramePlan
	save_facts                   MemoryCalleeSaveFacts
	saves                        []MemorySavedRegisterPlan
	save_push_count              u8
	base_allocation_bytes        u64
	padding_bytes                u8
	allocation_bytes             u64
	total_stack_extent_bytes     u64
	translations                 MemoryStackTranslations
	allocation_prologue_kind     MemoryFramePrologueKind
	prologue_bytes               []u8
	epilogue_bytes               []u8
	body_offset_bytes            u8
	entry_cfa_offset_bytes       u64
	body_cfa_offset_bytes        u64
	probe_required               bool
	probe_fixup                  MemoryFrameRel32Fixup
	chkstk                       MemoryChkstkContract
	windows_unwind               MemorySavedFrameWindowsUnwindPlan
	slots                        []MemoryFrameEncodedSlot
}

struct MemorySavedFrameIndexedRegister {
	source_request_index u32
	register             MemorySavedGpr
	register_encoding    u8
}

struct MemorySavedFrameRegisterWork {
	source_request_index u32
	register             MemorySavedGpr
	register_encoding    u8
	push_offset_bytes    u8
	push_end_offset_bytes u8
	push_width_bytes     u8
mut:
	pop_offset_bytes u8
	pop_end_offset_bytes u8
	pop_width_bytes u8
}

struct MemorySavedFrameAllocationEncoding {
	kind           MemoryFramePrologueKind
	prologue_bytes []u8
	epilogue_bytes []u8
	probe_required bool
	probe_fixup    MemoryFrameRel32Fixup
	chkstk         MemoryChkstkContract
}

struct MemorySavedFramePreflight {
	push_bytes         u8
	prologue_bytes     u8
	epilogue_bytes     u8
	unwind_slots       u8
	xdata_bytes        u8
	displacement_bytes u64
	byte_slice_payload u64
}

fn memory_saved_frame_error(message string) IError {
	return error('amd64 memory frame save: ${message}')
}

fn memory_saved_frame_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_saved_frame_error('arithmetic overflow')
	}
	return left + right
}

fn memory_saved_frame_checked_mul(left u64, right u64) !u64 {
	if left != 0 && right > max_u64 / left {
		return memory_saved_frame_error('arithmetic overflow')
	}
	return left * right
}

fn memory_saved_frame_append_u16_le(mut bytes []u8, value u16) {
	bytes << u8(value)
	bytes << u8(value >> 8)
}

fn memory_saved_frame_append_u32_le(mut bytes []u8, value u32) {
	bytes << u8(value)
	bytes << u8(value >> 8)
	bytes << u8(value >> 16)
	bytes << u8(value >> 24)
}

fn memory_saved_frame_register_is_valid(register MemorySavedGpr) bool {
	value := int(register)
	return value >= int(MemorySavedGpr.rbx) && value <= int(MemorySavedGpr.r15)
}

fn memory_saved_frame_register_encoding(register MemorySavedGpr) u8 {
	return match register {
		.rbx { 3 }
		.rbp { 5 }
		.rsi { 6 }
		.rdi { 7 }
		.r12 { 12 }
		.r13 { 13 }
		.r14 { 14 }
		.r15 { 15 }
	}
}

fn memory_saved_frame_register_is_nonvolatile(profile TargetProfile, register MemorySavedGpr) bool {
	return match profile {
		.linux_x86_64_sysv_elf, .macos_x86_64_sysv_macho {
			register in [.rbx, .rbp, .r12, .r13, .r14, .r15]
		}
		.windows_x86_64_microsoft_abi_coff {
			register in [.rbx, .rbp, .rsi, .rdi, .r12, .r13, .r14, .r15]
		}
	}
}

fn memory_saved_frame_sorted_registers(registers []MemorySavedFrameIndexedRegister) []MemorySavedFrameIndexedRegister {
	mut ordered := registers.clone()
	for index := 1; index < ordered.len; index++ {
		current := ordered[index]
		mut insertion := index
		for insertion > 0
			&& current.register_encoding < ordered[insertion - 1].register_encoding {
			ordered[insertion] = ordered[insertion - 1]
			insertion--
		}
		ordered[insertion] = current
	}
	return ordered
}

fn memory_saved_frame_validate_registers(profile TargetProfile, registers []MemorySavedGpr) ![]MemorySavedFrameIndexedRegister {
	for index, register in registers {
		if !memory_saved_frame_register_is_valid(register) {
			return memory_saved_frame_error('save ${index} has unsupported register')
		}
	}
	for index, register in registers {
		if !memory_saved_frame_register_is_nonvolatile(profile, register) {
			return memory_saved_frame_error('save ${index} register ${register} is not nonvolatile for target profile')
		}
	}
	for right := 1; right < registers.len; right++ {
		for left in 0 .. right {
			if registers[left] == registers[right] {
				return memory_saved_frame_error('duplicate saved register ${registers[right]}')
			}
		}
	}
	mut indexed := []MemorySavedFrameIndexedRegister{cap: registers.len}
	for index, register in registers {
		indexed << MemorySavedFrameIndexedRegister{
			source_request_index: u32(index)
			register:             register
			register_encoding:    memory_saved_frame_register_encoding(register)
		}
	}
	return memory_saved_frame_sorted_registers(indexed)
}

fn memory_saved_frame_push_width(encoding u8) u8 {
	return if encoding < 8 { u8(1) } else { u8(2) }
}

fn memory_saved_frame_append_push(mut bytes []u8, encoding u8) {
	if encoding < 8 {
		bytes << u8(0x50) + encoding
		return
	}
	bytes << u8(0x41)
	bytes << u8(0x50) + (encoding & 7)
}

fn memory_saved_frame_append_pop(mut bytes []u8, encoding u8) {
	if encoding < 8 {
		bytes << u8(0x58) + encoding
		return
	}
	bytes << u8(0x41)
	bytes << u8(0x58) + (encoding & 7)
}

fn memory_saved_frame_geometry(base_allocation u64, save_count int) !(u8, u64, u64) {
	if save_count == 0 {
		if base_allocation != 0 && base_allocation % 16 != 8 {
			return memory_saved_frame_error('internal geometry invariant failed')
		}
		return 0, base_allocation, base_allocation
	}
	push_extent := memory_saved_frame_checked_mul(u64(save_count), 8)!
	without_padding := memory_saved_frame_checked_add(push_extent, base_allocation)!
	padding := match without_padding % 16 {
		8 { u8(0) }
		0 { u8(8) }
		else { return memory_saved_frame_error('internal geometry invariant failed') }
	}
	allocation := memory_saved_frame_checked_add(base_allocation, u64(padding))!
	if allocation > memory_saved_frame_max_allocation_bytes {
		return memory_saved_frame_error('allocation ${allocation} exceeds ${memory_saved_frame_max_allocation_bytes}')
	}
	total := memory_saved_frame_checked_add(push_extent, allocation)!
	if total % 16 != 8 {
		return memory_saved_frame_error('internal geometry invariant failed')
	}
	return padding, allocation, total
}

fn memory_saved_frame_build_allocation(profile TargetProfile, allocation u64, save_count int, push_bytes u8) !MemorySavedFrameAllocationEncoding {
	if allocation > memory_saved_frame_max_allocation_bytes {
		return memory_saved_frame_error('allocation ${allocation} exceeds ${memory_saved_frame_max_allocation_bytes}')
	}
	if allocation != 0 && allocation % 8 != 0 {
		return memory_saved_frame_error('internal allocation invariant failed')
	}
	if allocation == 0 {
		return MemorySavedFrameAllocationEncoding{
			prologue_bytes: []u8{}
			epilogue_bytes: []u8{}
		}
	}
	mut epilogue := []u8{}
	if allocation <= 127 {
		epilogue = [u8(0x48), 0x83, 0xc4, u8(allocation)]
	} else {
		epilogue = [u8(0x48), 0x81, 0xc4]
		memory_saved_frame_append_u32_le(mut epilogue, u32(allocation))
	}
	probe_required := profile == .windows_x86_64_microsoft_abi_coff
		&& allocation >= memory_saved_frame_probe_threshold_bytes
	if probe_required {
		mut prologue := [u8(0xb8)]
		memory_saved_frame_append_u32_le(mut prologue, u32(allocation))
		prologue << [u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
		opcode_offset := int(push_bytes) + 5
		field_offset := int(push_bytes) + 6
		if field_offset + 4 > memory_saved_frame_max_prologue_bytes {
			return memory_saved_frame_error('probe fixup offset invariant failed')
		}
		even_push_count := save_count % 2 == 0
		return MemorySavedFrameAllocationEncoding{
			kind:           .windows_chkstk
			prologue_bytes: prologue
			epilogue_bytes: epilogue
			probe_required: true
			probe_fixup:    MemoryFrameRel32Fixup{
				present:             true
				kind:                .call_rel32_next_instruction
				target_name:         '__chkstk'.clone()
				opcode_offset_bytes: u8(opcode_offset)
				field_offset_bytes:  u8(field_offset)
				width_bytes:         4
				addend:              0
			}
			chkstk:         MemoryChkstkContract{
				present:                      true
				allocation_bytes:             allocation
				save_push_count:              u8(save_count)
				shadow_space_bytes:           0
				pre_call_rsp_mod_16:          if even_push_count { u8(8) } else { u8(0) }
				helper_entry_rsp_mod_16:      if even_push_count { u8(0) } else { u8(8) }
				eax_zero_extends_rax:         true
				helper_preserves_rax:         true
				rax_after_prologue:           allocation
				clobbers_r10:                 true
				clobbers_r11:                 true
				clobbers_eflags:              true
				preserves_other_integer_gprs: true
			}
		}
	}
	if allocation <= 127 {
		return MemorySavedFrameAllocationEncoding{
			kind:           .sub_imm8
			prologue_bytes: [u8(0x48), 0x83, 0xec, u8(allocation)]
			epilogue_bytes: epilogue
		}
	}
	mut prologue := [u8(0x48), 0x81, 0xec]
	memory_saved_frame_append_u32_le(mut prologue, u32(allocation))
	return MemorySavedFrameAllocationEncoding{
		kind:           .sub_imm32
		prologue_bytes: prologue
		epilogue_bytes: epilogue
	}
}

fn memory_saved_frame_allocation_unwind_kind(allocation u64) MemoryWindowsUnwindKind {
	if allocation == 0 {
		return .none
	}
	if allocation <= 128 {
		return .alloc_small
	}
	if allocation <= 524280 {
		return .alloc_large_info0
	}
	return .alloc_large_info1
}

fn memory_saved_frame_allocation_unwind_slots(allocation u64) u8 {
	return match memory_saved_frame_allocation_unwind_kind(allocation) {
		.none { 0 }
		.alloc_small { 1 }
		.alloc_large_info0 { 2 }
		.alloc_large_info1 { 3 }
	}
}

fn memory_saved_frame_preflight(base &MemoryFrameEncodingPlan, ordered []MemorySavedFrameIndexedRegister, allocation u64, probe_required bool) !MemorySavedFramePreflight {
	mut push_bytes := u64(0)
	for indexed in ordered {
		push_bytes = memory_saved_frame_checked_add(push_bytes,
			u64(memory_saved_frame_push_width(indexed.register_encoding)))!
	}
	allocation_prologue_bytes := if allocation == 0 {
		u64(0)
	} else if probe_required {
		u64(13)
	} else if allocation <= 127 {
		u64(4)
	} else {
		u64(7)
	}
	allocation_epilogue_bytes := if allocation == 0 {
		u64(0)
	} else if allocation <= 127 {
		u64(4)
	} else {
		u64(7)
	}
	prologue_bytes := memory_saved_frame_checked_add(push_bytes,
		allocation_prologue_bytes)!
	epilogue_bytes := memory_saved_frame_checked_add(allocation_epilogue_bytes,
		push_bytes)!
	if prologue_bytes > memory_saved_frame_max_prologue_bytes
		|| epilogue_bytes > memory_saved_frame_max_epilogue_bytes {
		return memory_saved_frame_error('output byte cap exceeded')
	}
	is_windows := base.frame.profile == .windows_x86_64_microsoft_abi_coff
	mut unwind_slots := u64(0)
	mut xdata_bytes := u64(0)
	if is_windows && prologue_bytes != 0 {
		unwind_slots = memory_saved_frame_checked_add(u64(ordered.len),
			u64(memory_saved_frame_allocation_unwind_slots(allocation)))!
		if unwind_slots > memory_saved_frame_max_unwind_slots {
			return memory_saved_frame_error('unwind code cap exceeded')
		}
		padded_slots := if unwind_slots % 2 == 0 {
			unwind_slots
		} else {
			memory_saved_frame_checked_add(unwind_slots, 1)!
		}
		xdata_bytes = memory_saved_frame_checked_add(4,
			memory_saved_frame_checked_mul(padded_slots, 2)!)!
		if xdata_bytes > memory_saved_frame_max_xdata_bytes {
			return memory_saved_frame_error('xdata byte cap exceeded')
		}
	}
	mut displacement_bytes := u64(0)
	for encoded in base.slots {
		displacement_bytes = memory_saved_frame_checked_add(displacement_bytes,
			u64(encoded.address.displacement_le.len))!
	}
	if displacement_bytes > memory_saved_frame_max_displacement_bytes {
		return memory_saved_frame_error('displacement byte cap exceeded')
	}
	mut payload := memory_saved_frame_checked_add(displacement_bytes, prologue_bytes)!
	payload = memory_saved_frame_checked_add(payload, epilogue_bytes)!
	payload = memory_saved_frame_checked_add(payload, xdata_bytes)!
	if payload > memory_saved_frame_max_byte_slice_payload {
		return memory_saved_frame_error('byte-slice payload cap exceeded')
	}
	return MemorySavedFramePreflight{
		push_bytes:         u8(push_bytes)
		prologue_bytes:     u8(prologue_bytes)
		epilogue_bytes:     u8(epilogue_bytes)
		unwind_slots:       u8(unwind_slots)
		xdata_bytes:        u8(xdata_bytes)
		displacement_bytes: displacement_bytes
		byte_slice_payload: payload
	}
}

fn memory_saved_frame_build_unwind(profile TargetProfile, allocation u64, prologue []u8, works []MemorySavedFrameRegisterWork, expected_slots u8, expected_xdata_bytes u8) !MemorySavedFrameWindowsUnwindPlan {
	if profile != .windows_x86_64_microsoft_abi_coff || prologue.len == 0 {
		if expected_slots != 0 || expected_xdata_bytes != 0 {
			return memory_saved_frame_error('unwind preflight invariant failed')
		}
		return MemorySavedFrameWindowsUnwindPlan{
			xdata_bytes: []u8{}
		}
	}
	allocation_kind := memory_saved_frame_allocation_unwind_kind(allocation)
	mut count := u8(0)
	mut codes := []u8{cap: memory_saved_frame_max_unwind_slots * 2 + 2}
	if allocation != 0 {
		allocation_offset := u8(prologue.len)
		match allocation_kind {
			.alloc_small {
				count = 1
				codes << allocation_offset
				codes << (u8((allocation - 8) / 8) << 4) | u8(2)
			}
			.alloc_large_info0 {
				count = 2
				codes << allocation_offset
				codes << u8(1)
				memory_saved_frame_append_u16_le(mut codes, u16(allocation / 8))
			}
			.alloc_large_info1 {
				count = 3
				codes << allocation_offset
				codes << u8(0x11)
				memory_saved_frame_append_u32_le(mut codes, u32(allocation))
			}
			.none {
				return memory_saved_frame_error('unwind allocation invariant failed')
			}
		}
	}
	for reverse_index in 0 .. works.len {
		index := works.len - 1 - reverse_index
		work := works[index]
		count++
		codes << work.push_end_offset_bytes
		codes << work.register_encoding << 4
	}
	if count != expected_slots {
		return memory_saved_frame_error('unwind code count invariant failed')
	}
	if count % 2 != 0 {
		codes << [u8(0), 0]
	}
	mut xdata := [u8(0x01), u8(prologue.len), count, 0x00]
	xdata << codes
	if xdata.len != int(expected_xdata_bytes)
		|| xdata.len > memory_saved_frame_max_xdata_bytes {
		return memory_saved_frame_error('xdata byte length invariant failed')
	}
	return MemorySavedFrameWindowsUnwindPlan{
		present:                      true
		allocation_kind:              allocation_kind
		allocation_code_present:      allocation != 0
		allocation_bytes:             allocation
		size_of_prolog_bytes:         u8(prologue.len)
		allocation_code_offset_bytes: if allocation == 0 { u8(0) } else { u8(prologue.len) }
		count_of_codes:               count
		xdata_bytes:                  xdata
	}
}

fn memory_saved_frame_clone_slots(slots []MemoryFrameEncodedSlot) []MemoryFrameEncodedSlot {
	mut cloned := []MemoryFrameEncodedSlot{cap: slots.len}
	for encoded in slots {
		cloned << MemoryFrameEncodedSlot{
			source_placement_index: encoded.source_placement_index
			placement:              encoded.placement
			address:                MemoryRspAddressEncoding{
				basis:              encoded.address.basis
				displacement_bytes: encoded.address.displacement_bytes
				kind:               encoded.address.kind
				mod_bits:           encoded.address.mod_bits
				rm_bits:            encoded.address.rm_bits
				sib_scale_bits:     encoded.address.sib_scale_bits
				sib_index_bits:     encoded.address.sib_index_bits
				sib_base_bits:      encoded.address.sib_base_bits
				displacement_le:    encoded.address.displacement_le.clone()
			}
		}
	}
	return cloned
}

fn memory_saved_frame_validate_output(plan &MemorySavedFramePlan, preflight &MemorySavedFramePreflight) ! {
	if plan.saves.len != int(plan.save_push_count)
		|| plan.prologue_bytes.len != int(preflight.prologue_bytes)
		|| plan.epilogue_bytes.len != int(preflight.epilogue_bytes)
		|| plan.windows_unwind.count_of_codes != preflight.unwind_slots
		|| plan.windows_unwind.xdata_bytes.len != int(preflight.xdata_bytes) {
		return memory_saved_frame_error('output count invariant failed')
	}
	if plan.save_push_count != 0 {
		if plan.layout_frame.red_zone_policy != .forbidden || plan.layout_frame.uses_red_zone
			|| plan.total_stack_extent_bytes % 16 != 8 {
			return memory_saved_frame_error('policy geometry invariant failed')
		}
	}
	fixup_count := if plan.probe_fixup.present { 1 } else { 0 }
	if fixup_count > memory_saved_frame_max_fixups {
		return memory_saved_frame_error('fixup cap exceeded')
	}
	if preflight.byte_slice_payload > memory_saved_frame_max_byte_slice_payload
		|| preflight.displacement_bytes > memory_saved_frame_max_displacement_bytes {
		return memory_saved_frame_error('output payload invariant failed')
	}
}

// plan_memory_saved_frame produces an inert save-aware frame encoding snapshot.
pub fn plan_memory_saved_frame(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts) !MemorySavedFramePlan {
	if !saves.present {
		return memory_saved_frame_error('callee-save facts are required')
	}
	if saves.registers.len > memory_saved_frame_max_saves {
		return memory_saved_frame_error('save count ${saves.registers.len} exceeds ${memory_saved_frame_max_saves}')
	}
	policy := if saves.registers.len == 0 {
		MemoryRedZonePolicy.abi_default
	} else {
		MemoryRedZonePolicy.forbidden
	}
	base := plan_memory_frame_encoding_with_red_zone_policy(facts, policy)!
	if saves.function_id != facts.function_id {
		return memory_saved_frame_error('callee-save function ${saves.function_id} does not match frame function ${facts.function_id}')
	}
	ordered := memory_saved_frame_validate_registers(base.frame.profile, saves.registers)!
	padding, allocation, total := memory_saved_frame_geometry(base.frame.stack_adjustment_bytes,
		ordered.len)!
	probe_required := base.frame.profile == .windows_x86_64_microsoft_abi_coff
		&& allocation >= memory_saved_frame_probe_threshold_bytes
	preflight := memory_saved_frame_preflight(&base, ordered, allocation, probe_required)!

	mut prologue := []u8{cap: int(preflight.prologue_bytes)}
	mut works := []MemorySavedFrameRegisterWork{cap: ordered.len}
	for indexed in ordered {
		push_offset := u8(prologue.len)
		memory_saved_frame_append_push(mut prologue, indexed.register_encoding)
		push_width := memory_saved_frame_push_width(indexed.register_encoding)
		works << MemorySavedFrameRegisterWork{
			source_request_index: indexed.source_request_index
			register:             indexed.register
			register_encoding:    indexed.register_encoding
			push_offset_bytes:    push_offset
			push_end_offset_bytes: u8(prologue.len)
			push_width_bytes:     push_width
		}
	}
	if prologue.len != int(preflight.push_bytes) {
		return memory_saved_frame_error('push byte count invariant failed')
	}
	allocation_encoding := memory_saved_frame_build_allocation(base.frame.profile, allocation,
		ordered.len, preflight.push_bytes)!
	if allocation_encoding.probe_required != probe_required {
		return memory_saved_frame_error('probe preflight invariant failed')
	}
	prologue << allocation_encoding.prologue_bytes

	mut epilogue := allocation_encoding.epilogue_bytes.clone()
	for reverse_index in 0 .. works.len {
		index := works.len - 1 - reverse_index
		works[index].pop_offset_bytes = u8(epilogue.len)
		memory_saved_frame_append_pop(mut epilogue, works[index].register_encoding)
		works[index].pop_end_offset_bytes = u8(epilogue.len)
		works[index].pop_width_bytes = works[index].push_width_bytes
	}
	unwind := memory_saved_frame_build_unwind(base.frame.profile, allocation, prologue,
		works, preflight.unwind_slots, preflight.xdata_bytes)!

	is_windows := base.frame.profile == .windows_x86_64_microsoft_abi_coff
	mut save_plans := []MemorySavedRegisterPlan{cap: works.len}
	for work in works {
		save_plans << MemorySavedRegisterPlan{
			source_request_index:             work.source_request_index
			register:                         work.register
			register_encoding:                work.register_encoding
			push_offset_bytes:                work.push_offset_bytes
			push_end_offset_bytes:            work.push_end_offset_bytes
			push_width_bytes:                 work.push_width_bytes
			pop_offset_bytes:                 work.pop_offset_bytes
			pop_end_offset_bytes:             work.pop_end_offset_bytes
			pop_width_bytes:                  work.pop_width_bytes
			windows_unwind_present:           is_windows
			windows_unwind_code_offset_bytes: if is_windows {
				work.push_end_offset_bytes
			} else {
				u8(0)
			}
		}
	}
	body_cfa := memory_saved_frame_checked_add(total, 8)!
	mut result := MemorySavedFramePlan{
		layout_frame:             memory_frame_encoding_clone_frame(&base.frame)
		save_facts:               MemoryCalleeSaveFacts{
			present:     saves.present
			function_id: saves.function_id
			registers:   saves.registers.clone()
		}
		saves:                    save_plans
		save_push_count:          u8(works.len)
		base_allocation_bytes:    base.frame.stack_adjustment_bytes
		padding_bytes:            padding
		allocation_bytes:         allocation
		total_stack_extent_bytes: total
		translations:             MemoryStackTranslations{
			entry_to_body_subtract_bytes: total
			incoming_from_body_add_bytes: total
			outgoing_from_body_add_bytes: 0
		}
		allocation_prologue_kind: allocation_encoding.kind
		prologue_bytes:           prologue.clone()
		epilogue_bytes:           epilogue.clone()
		body_offset_bytes:        u8(prologue.len)
		entry_cfa_offset_bytes:   8
		body_cfa_offset_bytes:    body_cfa
		probe_required:           probe_required
		probe_fixup:              MemoryFrameRel32Fixup{
			present:             allocation_encoding.probe_fixup.present
			kind:                allocation_encoding.probe_fixup.kind
			target_name:         allocation_encoding.probe_fixup.target_name.clone()
			opcode_offset_bytes: allocation_encoding.probe_fixup.opcode_offset_bytes
			field_offset_bytes:  allocation_encoding.probe_fixup.field_offset_bytes
			width_bytes:         allocation_encoding.probe_fixup.width_bytes
			addend:              allocation_encoding.probe_fixup.addend
		}
		chkstk:                  allocation_encoding.chkstk
		windows_unwind:          MemorySavedFrameWindowsUnwindPlan{
			present:                      unwind.present
			allocation_kind:              unwind.allocation_kind
			allocation_code_present:      unwind.allocation_code_present
			allocation_bytes:             unwind.allocation_bytes
			size_of_prolog_bytes:         unwind.size_of_prolog_bytes
			allocation_code_offset_bytes: unwind.allocation_code_offset_bytes
			count_of_codes:               unwind.count_of_codes
			xdata_bytes:                  unwind.xdata_bytes.clone()
		}
		slots:                    memory_saved_frame_clone_slots(base.slots)
	}
	memory_saved_frame_validate_output(&result, &preflight)!
	return result
}
