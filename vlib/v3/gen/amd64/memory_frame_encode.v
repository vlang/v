module amd64

const memory_frame_encoding_max_slots = 1024
const memory_frame_encoding_max_fixups = 1
const memory_frame_encoding_max_prologue_bytes = 13
const memory_frame_encoding_max_epilogue_bytes = 7
const memory_frame_encoding_max_xdata_bytes = 12
const memory_frame_encoding_max_displacement_bytes_per_slot = 4
const memory_frame_encoding_max_total_displacement_bytes = u64(4096)
const memory_frame_encoding_max_byte_slice_payload = u64(4128)
const memory_frame_encoding_max_stack_adjustment_bytes = u64(0x7ffffff8)

pub enum MemoryFramePrologueKind {
	none
	sub_imm8
	sub_imm32
	windows_chkstk
}

pub enum MemoryFrameFixupKind {
	none
	call_rel32_next_instruction
}

pub enum MemoryRspDisplacementKind {
	zero
	disp8
	disp32
}

pub enum MemoryWindowsUnwindKind {
	none
	alloc_small
	alloc_large_info0
	alloc_large_info1
}

pub struct MemoryFrameRel32Fixup {
pub:
	present             bool
	kind                MemoryFrameFixupKind
	target_name         string
	opcode_offset_bytes u8
	field_offset_bytes  u8
	width_bytes         u8
	addend              i64
}

pub struct MemoryChkstkContract {
pub:
	present                      bool
	allocation_bytes             u64
	save_push_count              u8
	shadow_space_bytes           u8
	pre_call_rsp_mod_16          u8
	helper_entry_rsp_mod_16      u8
	eax_zero_extends_rax         bool
	helper_preserves_rax         bool
	rax_after_prologue           u64
	clobbers_r10                 bool
	clobbers_r11                 bool
	clobbers_eflags              bool
	preserves_other_integer_gprs bool
}

pub struct MemoryRspAddressEncoding {
pub:
	basis              MemorySlotBasis
	displacement_bytes i32
	kind               MemoryRspDisplacementKind
	mod_bits           u8
	rm_bits            u8
	sib_scale_bits     u8
	sib_index_bits     u8
	sib_base_bits      u8
	displacement_le    []u8
}

pub struct MemoryFrameEncodedSlot {
pub:
	source_placement_index u32
	placement              MemorySlotPlacement
	address                MemoryRspAddressEncoding
}

pub struct MemoryWindowsUnwindPlan {
pub:
	present                      bool
	kind                         MemoryWindowsUnwindKind
	allocation_bytes             u64
	size_of_prolog_bytes         u8
	allocation_code_offset_bytes u8
	count_of_codes               u8
	xdata_bytes                  []u8
}

pub struct MemoryFrameEncodingPlan {
pub:
	frame                  MemoryFramePlan
	prologue_kind          MemoryFramePrologueKind
	prologue_bytes         []u8
	epilogue_bytes         []u8
	body_offset_bytes      u8
	entry_cfa_offset_bytes u64
	body_cfa_offset_bytes  u64
	probe_fixup            MemoryFrameRel32Fixup
	chkstk                 MemoryChkstkContract
	windows_unwind         MemoryWindowsUnwindPlan
	slots                  []MemoryFrameEncodedSlot
}

struct MemoryFrameStackEncoding {
	kind           MemoryFramePrologueKind
	prologue_bytes []u8
	epilogue_bytes []u8
	probe_fixup    MemoryFrameRel32Fixup
	chkstk         MemoryChkstkContract
}

fn memory_frame_encoding_error(message string) IError {
	return error('amd64 memory frame encoding: ${message}')
}

fn memory_frame_encoding_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_frame_encoding_error('arithmetic overflow')
	}
	return left + right
}

fn memory_frame_encoding_checked_mul(left u64, right u64) !u64 {
	if left != 0 && right > max_u64 / left {
		return memory_frame_encoding_error('arithmetic overflow')
	}
	return left * right
}

fn memory_frame_encoding_append_u16_le(mut bytes []u8, value u16) {
	bytes << u8(value)
	bytes << u8(value >> 8)
}

fn memory_frame_encoding_append_u32_le(mut bytes []u8, value u32) {
	bytes << u8(value)
	bytes << u8(value >> 8)
	bytes << u8(value >> 16)
	bytes << u8(value >> 24)
}

fn memory_frame_encoding_clone_frame(frame &MemoryFramePlan) MemoryFramePlan {
	return MemoryFramePlan{
		function_id:               frame.function_id
		profile:                   frame.profile
		extent_kind:               frame.extent_kind
		call_extent_bytes:         frame.call_extent_bytes
		has_call:                  frame.has_call
		uses_red_zone:             frame.uses_red_zone
		red_zone_extent_bytes:     frame.red_zone_extent_bytes
		non_red_zone_extent_bytes: frame.non_red_zone_extent_bytes
		stack_adjustment_bytes:    frame.stack_adjustment_bytes
		probe_required:            frame.probe_required
		translations:              frame.translations
		slots:                     frame.slots.clone()
		red_zone_policy:           frame.red_zone_policy
	}
}

fn memory_frame_encoding_encode_rsp_address(basis MemorySlotBasis, displacement i64) !MemoryRspAddressEncoding {
	if displacement < i64(min_i32) || displacement > i64(max_i32) {
		return memory_frame_encoding_error('RSP displacement ${displacement} is outside signed i32')
	}
	narrowed := i32(displacement)
	bits := u32(narrowed)
	if narrowed == 0 {
		return MemoryRspAddressEncoding{
			basis:              basis
			displacement_bytes: 0
			kind:               .zero
			mod_bits:           0
			rm_bits:            4
			sib_scale_bits:     0
			sib_index_bits:     4
			sib_base_bits:      4
			displacement_le:    []u8{}
		}
	}
	if narrowed >= -128 && narrowed <= 127 {
		return MemoryRspAddressEncoding{
			basis:              basis
			displacement_bytes: narrowed
			kind:               .disp8
			mod_bits:           1
			rm_bits:            4
			sib_scale_bits:     0
			sib_index_bits:     4
			sib_base_bits:      4
			displacement_le:    [u8(bits)]
		}
	}
	mut bytes := []u8{cap: memory_frame_encoding_max_displacement_bytes_per_slot}
	memory_frame_encoding_append_u32_le(mut bytes, bits)
	return MemoryRspAddressEncoding{
		basis:              basis
		displacement_bytes: narrowed
		kind:               .disp32
		mod_bits:           2
		rm_bits:            4
		sib_scale_bits:     0
		sib_index_bits:     4
		sib_base_bits:      4
		displacement_le:    bytes
	}
}

fn memory_frame_encoding_build_stack(adjustment u64, probe_required bool) !MemoryFrameStackEncoding {
	if adjustment > memory_frame_encoding_max_stack_adjustment_bytes {
		return memory_frame_encoding_error('stack adjustment ${adjustment} exceeds ${memory_frame_encoding_max_stack_adjustment_bytes}')
	}
	if adjustment == 0 {
		if probe_required {
			return memory_frame_encoding_error('zero stack adjustment requires a probe')
		}
		return MemoryFrameStackEncoding{
			prologue_bytes: []u8{}
			epilogue_bytes: []u8{}
		}
	}
	if adjustment % 16 != 8 {
		return memory_frame_encoding_error('stack adjustment ${adjustment} is not an M0-aligned extent')
	}
	mut epilogue := []u8{}
	if adjustment <= 127 {
		epilogue = [u8(0x48), 0x83, 0xc4, u8(adjustment)]
	} else {
		epilogue = [u8(0x48), 0x81, 0xc4]
		memory_frame_encoding_append_u32_le(mut epilogue, u32(adjustment))
	}
	if probe_required {
		mut prologue := [u8(0xb8)]
		memory_frame_encoding_append_u32_le(mut prologue, u32(adjustment))
		prologue << [u8(0xe8), 0x00, 0x00, 0x00, 0x00, 0x48, 0x29, 0xc4]
		return MemoryFrameStackEncoding{
			kind:           .windows_chkstk
			prologue_bytes: prologue
			epilogue_bytes: epilogue
			probe_fixup:    MemoryFrameRel32Fixup{
				present:             true
				kind:                .call_rel32_next_instruction
				target_name:         '__chkstk'.clone()
				opcode_offset_bytes: 5
				field_offset_bytes:  6
				width_bytes:         4
				addend:              0
			}
			chkstk:         MemoryChkstkContract{
				present:                      true
				allocation_bytes:             adjustment
				save_push_count:              0
				shadow_space_bytes:           0
				pre_call_rsp_mod_16:          8
				helper_entry_rsp_mod_16:      0
				eax_zero_extends_rax:         true
				helper_preserves_rax:         true
				rax_after_prologue:           adjustment
				clobbers_r10:                 true
				clobbers_r11:                 true
				clobbers_eflags:              true
				preserves_other_integer_gprs: true
			}
		}
	}
	if adjustment <= 127 {
		return MemoryFrameStackEncoding{
			kind:           .sub_imm8
			prologue_bytes: [u8(0x48), 0x83, 0xec, u8(adjustment)]
			epilogue_bytes: epilogue
		}
	}
	mut prologue := [u8(0x48), 0x81, 0xec]
	memory_frame_encoding_append_u32_le(mut prologue, u32(adjustment))
	return MemoryFrameStackEncoding{
		kind:           .sub_imm32
		prologue_bytes: prologue
		epilogue_bytes: epilogue
	}
}

fn memory_frame_encoding_build_windows_unwind(profile TargetProfile, adjustment u64, prologue []u8) !MemoryWindowsUnwindPlan {
	if profile != .windows_x86_64_microsoft_abi_coff || adjustment == 0 {
		return MemoryWindowsUnwindPlan{
			xdata_bytes: []u8{}
		}
	}
	if adjustment % 8 != 0 || adjustment > memory_frame_encoding_max_stack_adjustment_bytes {
		return memory_frame_encoding_error('Windows unwind allocation ${adjustment} is unsupported')
	}
	if prologue.len == 0 || prologue.len > memory_frame_encoding_max_prologue_bytes {
		return memory_frame_encoding_error('Windows unwind prologue length ${prologue.len} is invalid')
	}
	size_of_prolog := u8(prologue.len)
	mut kind := MemoryWindowsUnwindKind.none
	mut count := u8(0)
	mut unwind_codes := []u8{cap: 8}
	if adjustment <= 128 {
		operation_info := u8(adjustment / 8 - 1)
		kind = .alloc_small
		count = 1
		unwind_codes << size_of_prolog
		unwind_codes << (operation_info << 4) | u8(2)
	} else if adjustment <= 524280 {
		kind = .alloc_large_info0
		count = 2
		unwind_codes << size_of_prolog
		unwind_codes << u8(1)
		memory_frame_encoding_append_u16_le(mut unwind_codes, u16(adjustment / 8))
	} else {
		kind = .alloc_large_info1
		count = 3
		unwind_codes << size_of_prolog
		unwind_codes << u8(0x11)
		memory_frame_encoding_append_u32_le(mut unwind_codes, u32(adjustment))
	}
	if count % 2 != 0 {
		unwind_codes << [u8(0x00), 0x00]
	}
	mut xdata := [u8(0x01), size_of_prolog, count, 0x00]
	xdata << unwind_codes
	expected_length := if adjustment <= 524280 { 8 } else { 12 }
	if xdata.len != expected_length || xdata.len > memory_frame_encoding_max_xdata_bytes {
		return memory_frame_encoding_error('Windows unwind byte length invariant failed')
	}
	return MemoryWindowsUnwindPlan{
		present:                      true
		kind:                         kind
		allocation_bytes:             adjustment
		size_of_prolog_bytes:         size_of_prolog
		allocation_code_offset_bytes: size_of_prolog
		count_of_codes:               count
		xdata_bytes:                  xdata
	}
}

fn memory_frame_encoding_validate_plan_basis(frame &MemoryFramePlan, index int, placement &MemorySlotPlacement) ! {
	basis_value := int(placement.basis)
	if basis_value < int(MemorySlotBasis.body_rsp)
		|| basis_value > int(MemorySlotBasis.entry_rsp) {
		return memory_frame_encoding_error('slot ${index} has unsupported M0 basis')
	}
	match placement.basis {
		.entry_rsp {
			if !frame.uses_red_zone || frame.stack_adjustment_bytes != 0
				|| frame.probe_required || placement.displacement_bytes >= 0 {
				return memory_frame_encoding_error('slot ${index} entry-RSP basis is inconsistent with M0 frame')
			}
		}
		.body_rsp {
			if frame.uses_red_zone || placement.displacement_bytes < 0 {
				return memory_frame_encoding_error('slot ${index} body-RSP basis is inconsistent with M0 frame')
			}
		}
	}
}

fn memory_frame_encoding_encode_slots(frame &MemoryFramePlan) ![]MemoryFrameEncodedSlot {
	mut result := []MemoryFrameEncodedSlot{cap: frame.slots.len}
	for index, placement in frame.slots {
		memory_frame_encoding_validate_plan_basis(frame, index, &placement)!
		address := memory_frame_encoding_encode_rsp_address(placement.basis,
			placement.displacement_bytes)!
		result << MemoryFrameEncodedSlot{
			source_placement_index: u32(index)
			placement:              placement
			address:                address
		}
	}
	if result.len != frame.slots.len {
		return memory_frame_encoding_error('M0 placement consumption invariant failed')
	}
	for index, encoded in result {
		if encoded.source_placement_index != u32(index)
			|| encoded.placement != frame.slots[index]
			|| encoded.address.basis != encoded.placement.basis
			|| i64(encoded.address.displacement_bytes) != encoded.placement.displacement_bytes {
			return memory_frame_encoding_error('M0 placement consumption invariant failed')
		}
	}
	return result
}

fn memory_frame_encoding_validate_output(plan &MemoryFrameEncodingPlan) ! {
	if plan.prologue_bytes.len > memory_frame_encoding_max_prologue_bytes
		|| plan.epilogue_bytes.len > memory_frame_encoding_max_epilogue_bytes
		|| plan.windows_unwind.xdata_bytes.len > memory_frame_encoding_max_xdata_bytes {
		return memory_frame_encoding_error('output byte cap invariant failed')
	}
	fixup_count := if plan.probe_fixup.present { 1 } else { 0 }
	if fixup_count > memory_frame_encoding_max_fixups {
		return memory_frame_encoding_error('fixup cap invariant failed')
	}
	mut displacement_bytes := u64(0)
	for encoded in plan.slots {
		displacement_bytes = memory_frame_encoding_checked_add(displacement_bytes,
			u64(encoded.address.displacement_le.len))!
	}
	if displacement_bytes > memory_frame_encoding_max_total_displacement_bytes {
		return memory_frame_encoding_error('displacement byte cap invariant failed')
	}
	mut byte_payload := displacement_bytes
	byte_payload = memory_frame_encoding_checked_add(byte_payload,
		u64(plan.prologue_bytes.len))!
	byte_payload = memory_frame_encoding_checked_add(byte_payload,
		u64(plan.epilogue_bytes.len))!
	byte_payload = memory_frame_encoding_checked_add(byte_payload,
		u64(plan.windows_unwind.xdata_bytes.len))!
	if byte_payload > memory_frame_encoding_max_byte_slice_payload {
		return memory_frame_encoding_error('byte-slice payload cap invariant failed')
	}
}

fn memory_frame_encoding_plan_with_red_zone_policy(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) !MemoryFrameEncodingPlan {
	frame := plan_memory_frame_with_red_zone_policy(facts, policy)!
	if frame.slots.len > memory_frame_encoding_max_slots {
		return memory_frame_encoding_error('slot count ${frame.slots.len} exceeds ${memory_frame_encoding_max_slots}')
	}
	max_displacement_bytes := memory_frame_encoding_checked_mul(u64(frame.slots.len),
		memory_frame_encoding_max_displacement_bytes_per_slot)!
	if max_displacement_bytes > memory_frame_encoding_max_total_displacement_bytes {
		return memory_frame_encoding_error('displacement byte cap exceeded')
	}
	max_byte_payload := memory_frame_encoding_checked_add(max_displacement_bytes,
		u64(memory_frame_encoding_max_prologue_bytes + memory_frame_encoding_max_epilogue_bytes +
		memory_frame_encoding_max_xdata_bytes))!
	if max_byte_payload > memory_frame_encoding_max_byte_slice_payload {
		return memory_frame_encoding_error('byte-slice payload cap exceeded')
	}
	adjustment := frame.stack_adjustment_bytes
	if adjustment > memory_frame_encoding_max_stack_adjustment_bytes {
		return memory_frame_encoding_error('stack adjustment ${adjustment} exceeds ${memory_frame_encoding_max_stack_adjustment_bytes}')
	}
	body_cfa := memory_frame_encoding_checked_add(adjustment, 8)!
	if frame.probe_required && frame.profile != .windows_x86_64_microsoft_abi_coff {
		return memory_frame_encoding_error('M0 probe/profile invariant failed')
	}
	if frame.uses_red_zone
		&& (adjustment != 0 || frame.probe_required
		|| (frame.profile != .linux_x86_64_sysv_elf
		&& frame.profile != .macos_x86_64_sysv_macho)) {
		return memory_frame_encoding_error('M0 red-zone invariant failed')
	}

	stack := memory_frame_encoding_build_stack(adjustment, frame.probe_required)!
	unwind := memory_frame_encoding_build_windows_unwind(frame.profile, adjustment,
		stack.prologue_bytes)!
	encoded_slots := memory_frame_encoding_encode_slots(&frame)!
	mut result := MemoryFrameEncodingPlan{
		frame:                  memory_frame_encoding_clone_frame(&frame)
		prologue_kind:          stack.kind
		prologue_bytes:         stack.prologue_bytes.clone()
		epilogue_bytes:         stack.epilogue_bytes.clone()
		body_offset_bytes:      u8(stack.prologue_bytes.len)
		entry_cfa_offset_bytes: 8
		body_cfa_offset_bytes:  body_cfa
		probe_fixup:            stack.probe_fixup
		chkstk:                 stack.chkstk
		windows_unwind:         MemoryWindowsUnwindPlan{
			present:                      unwind.present
			kind:                         unwind.kind
			allocation_bytes:             unwind.allocation_bytes
			size_of_prolog_bytes:         unwind.size_of_prolog_bytes
			allocation_code_offset_bytes: unwind.allocation_code_offset_bytes
			count_of_codes:               unwind.count_of_codes
			xdata_bytes:                  unwind.xdata_bytes.clone()
		}
		slots:                  encoded_slots
	}
	memory_frame_encoding_validate_output(&result)!
	return result
}

// plan_memory_frame_encoding_with_red_zone_policy preserves the M0 policy attestation.
pub fn plan_memory_frame_encoding_with_red_zone_policy(facts &MemoryFunctionFrameFacts, policy MemoryRedZonePolicy) !MemoryFrameEncodingPlan {
	return memory_frame_encoding_plan_with_red_zone_policy(facts, policy)
}

// plan_memory_frame_encoding produces the legacy ABI-default encoding snapshot.
pub fn plan_memory_frame_encoding(facts &MemoryFunctionFrameFacts) !MemoryFrameEncodingPlan {
	return plan_memory_frame_encoding_with_red_zone_policy(facts, .abi_default)
}
