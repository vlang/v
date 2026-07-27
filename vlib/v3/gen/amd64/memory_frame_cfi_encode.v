module amd64

const memory_frame_cfi_encode_cie_initial_instruction_bytes = [u8(0x0c), 0x07, 0x08,
	0x90, 0x01]
const memory_frame_cfi_encode_max_cfa_offset_bytes = u64(0x80000030)
const memory_frame_cfi_encode_max_region_code_offset_bytes = u64(17)
const memory_frame_cfi_encode_max_uleb128_bytes = u64(5)
const memory_frame_cfi_encode_max_prologue_fragment_bytes = u64(37)
const memory_frame_cfi_encode_max_epilogue_fragment_bytes = u64(21)
const memory_frame_cfi_encode_max_total_instruction_fragment_bytes = u64(63)

pub struct MemoryFrameDwarfFdeInstructionFragment {
pub:
	present                                        bool
	region                                         MemoryFrameCfiRegion
	region_relative_initial_code_offset_bytes      u8
	region_relative_final_code_offset_bytes        u8
	initial_cfa_offset_bytes                       u64
	final_cfa_offset_bytes                         u64
	semantic_operation_count                       u8
	region_relative_fde_instruction_fragment_bytes []u8
}

pub struct MemoryFrameCfiInstructionFragmentPlan {
pub:
	cfi                                      MemoryFrameCfiPlan
	cie_initial_instruction_bytes            []u8
	prologue_fde_instruction_fragment        MemoryFrameDwarfFdeInstructionFragment
	epilogue_fde_instruction_template        MemoryFrameDwarfFdeInstructionFragment
	total_instruction_fragment_bytes         u8
}

struct MemoryFrameCfiEncodeRegionPreflight {
	operation_count       u64
	fragment_bytes        u64
	final_code_offset     u64
	final_cfa_offset      u64
	max_cfa_offset        u64
	max_uleb128_bytes     u64
}

struct MemoryFrameCfiEncodePreflight {
	prologue MemoryFrameCfiEncodeRegionPreflight
	epilogue MemoryFrameCfiEncodeRegionPreflight
	total    u64
}

struct MemoryFrameCfiDecodedOp {
	instruction_end_offset_bytes u8
	same_pc_ordinal              u8
	kind                         MemoryFrameCfiOpKind
	cfa_offset_bytes             u64
	register_present             bool
	dwarf_register_number        u8
	saved_cfa_displacement_bytes i64
}

struct MemoryFrameCfiDecodedFragment {
	final_code_offset_bytes u8
	final_cfa_offset_bytes  u64
	ops                     []MemoryFrameCfiDecodedOp
}

fn memory_frame_cfi_encode_error(message string) IError {
	return error('amd64 memory frame cfi encode: ${message}')
}

fn memory_frame_cfi_encode_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_frame_cfi_encode_error('arithmetic overflow')
	}
	return left + right
}

fn memory_frame_cfi_encode_uleb128_size(value u64) u64 {
	mut remaining := value
	mut count := u64(1)
	for remaining >= 0x80 {
		remaining >>= 7
		count++
	}
	return count
}

fn memory_frame_cfi_encode_append_uleb128(mut bytes []u8, value u64) {
	mut remaining := value
	for {
		mut byte := u8(remaining & 0x7f)
		remaining >>= 7
		if remaining != 0 {
			byte |= 0x80
		}
		bytes << byte
		if remaining == 0 {
			return
		}
	}
}

fn memory_frame_cfi_encode_read_uleb128(bytes []u8, cursor_start int) !(u64, u64, int) {
	mut cursor := cursor_start
	start := cursor
	mut value := u64(0)
	mut shift := u32(0)
	for cursor < bytes.len {
		if cursor - start >= int(memory_frame_cfi_encode_max_uleb128_bytes) {
			return memory_frame_cfi_encode_error('ULEB128 exceeds five bytes')
		}
		byte := bytes[cursor]
		cursor++
		payload := u64(byte & 0x7f)
		if shift >= 64 || (payload << shift) >> shift != payload {
			return memory_frame_cfi_encode_error('ULEB128 overflow')
		}
		value |= payload << shift
		if byte & 0x80 == 0 {
			count := u64(cursor - start)
			if count != memory_frame_cfi_encode_uleb128_size(value) {
				return memory_frame_cfi_encode_error('non-shortest ULEB128')
			}
			return value, count, cursor
		}
		shift += 7
	}
	return memory_frame_cfi_encode_error('truncated ULEB128')
}

fn memory_frame_cfi_encode_validate_active_caps(max_cfa u64, max_code_offset u64, max_uleb128_bytes u64, prologue_bytes u64, epilogue_bytes u64, total_bytes u64) ! {
	if max_cfa > memory_frame_cfi_encode_max_cfa_offset_bytes {
		return memory_frame_cfi_encode_error('CFA offset cap exceeded')
	}
	if max_code_offset > memory_frame_cfi_encode_max_region_code_offset_bytes {
		return memory_frame_cfi_encode_error('region code offset cap exceeded')
	}
	if max_uleb128_bytes > memory_frame_cfi_encode_max_uleb128_bytes {
		return memory_frame_cfi_encode_error('ULEB128 byte cap exceeded')
	}
	if prologue_bytes > memory_frame_cfi_encode_max_prologue_fragment_bytes {
		return memory_frame_cfi_encode_error('prologue fragment byte cap exceeded')
	}
	if epilogue_bytes > memory_frame_cfi_encode_max_epilogue_fragment_bytes {
		return memory_frame_cfi_encode_error('epilogue fragment byte cap exceeded')
	}
	if total_bytes > memory_frame_cfi_encode_max_total_instruction_fragment_bytes {
		return memory_frame_cfi_encode_error('total instruction fragment byte cap exceeded')
	}
}

fn memory_frame_cfi_encode_validate_baseline(cfi &MemoryFrameCfiPlan) ! {
	expected := MemoryFrameCfiBaseline{
		present:                               true
		code_alignment_factor:                 1
		data_alignment_factor:                 -8
		cfa_register_number:                   7
		cfa_offset_bytes:                      8
		return_address_register_number:        16
		return_address_cfa_displacement_bytes: -8
	}
	if cfi.baseline != expected {
		return memory_frame_cfi_encode_error('DWARF baseline invariant failed')
	}
}

fn memory_frame_cfi_encode_saved_factor(op &MemoryFrameCfiOp) !u64 {
	if !op.register_present || op.saved_cfa_displacement_bytes >= 0
		|| op.saved_cfa_displacement_bytes % 8 != 0 {
		return memory_frame_cfi_encode_error('saved-register rule invariant failed')
	}
	if op.cfa_offset_bytes > u64(max_i64)
		|| op.saved_cfa_displacement_bytes != -i64(op.cfa_offset_bytes) {
		return memory_frame_cfi_encode_error('saved-register displacement invariant failed')
	}
	if op.dwarf_register_number > 63 {
		return memory_frame_cfi_encode_error('DWARF register number exceeds primary opcode')
	}
	return u64(-op.saved_cfa_displacement_bytes) / 8
}

fn memory_frame_cfi_encode_region_preflight(ops []MemoryFrameCfiOp, region MemoryFrameCfiRegion, initial_cfa u64) !MemoryFrameCfiEncodeRegionPreflight {
	max_ops := if region == .prologue {
		memory_frame_cfi_max_prologue_ops
	} else {
		memory_frame_cfi_max_epilogue_ops
	}
	if ops.len > max_ops {
		return memory_frame_cfi_encode_error('semantic operation cap exceeded')
	}
	mut previous_code_offset := u64(0)
	mut previous_ordinal := u8(0)
	mut has_previous_op := false
	mut cfa := initial_cfa
	mut max_cfa := initial_cfa
	mut max_uleb := u64(0)
	mut byte_count := u64(0)
	for op in ops {
		if op.region != region {
			return memory_frame_cfi_encode_error('operation region invariant failed')
		}
		code_offset := u64(op.instruction_end_offset_bytes)
		if code_offset < previous_code_offset {
			return memory_frame_cfi_encode_error('operation code offsets are not ordered')
		}
		if !has_previous_op || code_offset != previous_code_offset {
			if op.same_pc_ordinal != 0 {
				return memory_frame_cfi_encode_error('first same-PC ordinal is not zero')
			}
			delta := code_offset - previous_code_offset
			if delta > 63 {
				return memory_frame_cfi_encode_error('advance delta exceeds primary opcode')
			}
			if delta != 0 {
				byte_count = memory_frame_cfi_encode_checked_add(byte_count, 1)!
			}
		} else {
			if previous_ordinal == max_u8 || op.same_pc_ordinal != previous_ordinal + 1 {
				return memory_frame_cfi_encode_error('same-PC ordinal invariant failed')
			}
		}
		if int(op.kind) < int(MemoryFrameCfiOpKind.def_cfa_offset)
			|| int(op.kind) > int(MemoryFrameCfiOpKind.offset_register) {
			return memory_frame_cfi_encode_error('unsupported CFI operation kind')
		}
		match op.kind {
			.def_cfa_offset {
				if op.register_present {
					return memory_frame_cfi_encode_error('CFA rule unexpectedly names a register')
				}
				cfa = op.cfa_offset_bytes
				uleb_size := memory_frame_cfi_encode_uleb128_size(cfa)
				if uleb_size > max_uleb {
					max_uleb = uleb_size
				}
				byte_count = memory_frame_cfi_encode_checked_add(byte_count,
					memory_frame_cfi_encode_checked_add(1, uleb_size)!)!
			}
			.offset_register {
				if op.cfa_offset_bytes != cfa {
					return memory_frame_cfi_encode_error('saved-register CFA invariant failed')
				}
				factor := memory_frame_cfi_encode_saved_factor(&op)!
				uleb_size := memory_frame_cfi_encode_uleb128_size(factor)
				if uleb_size > max_uleb {
					max_uleb = uleb_size
				}
				byte_count = memory_frame_cfi_encode_checked_add(byte_count,
					memory_frame_cfi_encode_checked_add(1, uleb_size)!)!
			}
		}
		if cfa > max_cfa {
			max_cfa = cfa
		}
		has_previous_op = true
		previous_code_offset = code_offset
		previous_ordinal = op.same_pc_ordinal
	}
	return MemoryFrameCfiEncodeRegionPreflight{
		operation_count:   u64(ops.len)
		fragment_bytes:    byte_count
		final_code_offset: previous_code_offset
		final_cfa_offset:  cfa
		max_cfa_offset:    max_cfa
		max_uleb128_bytes: max_uleb
	}
}

fn memory_frame_cfi_encode_preflight(cfi &MemoryFrameCfiPlan) !MemoryFrameCfiEncodePreflight {
	if cfi.disposition != .dwarf_zero_delta && cfi.disposition != .dwarf_transitions {
		return memory_frame_cfi_encode_error('unsupported CFI disposition')
	}
	memory_frame_cfi_encode_validate_baseline(cfi)!
	prologue := memory_frame_cfi_encode_region_preflight(cfi.prologue_ops, .prologue,
		cfi.baseline.cfa_offset_bytes)!
	epilogue := memory_frame_cfi_encode_region_preflight(cfi.epilogue_template_ops,
		.epilogue_template, cfi.frame.body_cfa_offset_bytes)!
	if prologue.final_cfa_offset != cfi.frame.body_cfa_offset_bytes
		|| epilogue.final_cfa_offset != cfi.baseline.cfa_offset_bytes {
		return memory_frame_cfi_encode_error('region CFA endpoint invariant failed')
	}
	expected_prologue_code := if cfi.disposition == .dwarf_zero_delta {
		u64(0)
	} else {
		u64(cfi.frame.body_offset_bytes)
	}
	expected_epilogue_code := if cfi.disposition == .dwarf_zero_delta {
		u64(0)
	} else {
		u64(cfi.frame.epilogue_bytes.len)
	}
	if prologue.final_code_offset != expected_prologue_code
		|| epilogue.final_code_offset != expected_epilogue_code {
		return memory_frame_cfi_encode_error('region code endpoint invariant failed')
	}
	total := memory_frame_cfi_encode_checked_add(u64(memory_frame_cfi_encode_cie_initial_instruction_bytes.len),
		memory_frame_cfi_encode_checked_add(prologue.fragment_bytes,
			epilogue.fragment_bytes)!)!
	max_cfa := if prologue.max_cfa_offset > epilogue.max_cfa_offset {
		prologue.max_cfa_offset
	} else {
		epilogue.max_cfa_offset
	}
	max_code := if prologue.final_code_offset > epilogue.final_code_offset {
		prologue.final_code_offset
	} else {
		epilogue.final_code_offset
	}
	max_uleb := if prologue.max_uleb128_bytes > epilogue.max_uleb128_bytes {
		prologue.max_uleb128_bytes
	} else {
		epilogue.max_uleb128_bytes
	}
	memory_frame_cfi_encode_validate_active_caps(max_cfa, max_code, max_uleb,
		prologue.fragment_bytes, epilogue.fragment_bytes, total)!
	return MemoryFrameCfiEncodePreflight{
		prologue: prologue
		epilogue: epilogue
		total:    total
	}
}

fn memory_frame_cfi_encode_region(ops []MemoryFrameCfiOp, preflight MemoryFrameCfiEncodeRegionPreflight) ![]u8 {
	mut bytes := []u8{cap: int(preflight.fragment_bytes)}
	mut previous_code_offset := u8(0)
	for op in ops {
		delta := op.instruction_end_offset_bytes - previous_code_offset
		if delta != 0 {
			bytes << u8(0x40) | delta
		}
		if int(op.kind) < int(MemoryFrameCfiOpKind.def_cfa_offset)
			|| int(op.kind) > int(MemoryFrameCfiOpKind.offset_register) {
			return memory_frame_cfi_encode_error('unsupported CFI operation kind')
		}
		match op.kind {
			.def_cfa_offset {
				bytes << u8(0x0e)
				memory_frame_cfi_encode_append_uleb128(mut bytes, op.cfa_offset_bytes)
			}
			.offset_register {
				bytes << u8(0x80) | op.dwarf_register_number
				factor := memory_frame_cfi_encode_saved_factor(&op)!
				memory_frame_cfi_encode_append_uleb128(mut bytes, factor)
			}
		}
		previous_code_offset = op.instruction_end_offset_bytes
	}
	if bytes.len != int(preflight.fragment_bytes) {
		return memory_frame_cfi_encode_error('encoded fragment length invariant failed')
	}
	return bytes
}

fn memory_frame_cfi_encode_decode_fragment(bytes []u8, region MemoryFrameCfiRegion, initial_cfa u64, expected_operation_count int) !MemoryFrameCfiDecodedFragment {
	max_ops := if region == .prologue {
		memory_frame_cfi_max_prologue_ops
	} else {
		memory_frame_cfi_max_epilogue_ops
	}
	max_bytes := if region == .prologue {
		memory_frame_cfi_encode_max_prologue_fragment_bytes
	} else {
		memory_frame_cfi_encode_max_epilogue_fragment_bytes
	}
	if expected_operation_count < 0 || expected_operation_count > max_ops {
		return memory_frame_cfi_encode_error('semantic operation cap exceeded')
	}
	if u64(bytes.len) > max_bytes {
		return memory_frame_cfi_encode_error('${region} fragment byte cap exceeded')
	}
	mut decoded := []MemoryFrameCfiDecodedOp{cap: expected_operation_count}
	mut cursor := 0
	mut code_offset := u64(0)
	mut cfa := initial_cfa
	mut advance_pending := false
	mut previous_op_code := u64(0)
	mut previous_ordinal := u8(0)
	mut has_previous_op := false
	for cursor < bytes.len {
		opcode := bytes[cursor]
		cursor++
		if opcode >= 0x40 && opcode <= 0x7f {
			delta := u64(opcode & 0x3f)
			if delta == 0 || advance_pending {
				return memory_frame_cfi_encode_error('non-shortest code advance')
			}
			code_offset = memory_frame_cfi_encode_checked_add(code_offset, delta)!
			if code_offset > memory_frame_cfi_encode_max_region_code_offset_bytes {
				return memory_frame_cfi_encode_error('region code offset cap exceeded')
			}
			advance_pending = true
			continue
		}
		mut decoded_op := MemoryFrameCfiDecodedOp{
			instruction_end_offset_bytes: u8(code_offset)
		}
		if !has_previous_op || code_offset != previous_op_code {
			decoded_op = MemoryFrameCfiDecodedOp{
				...decoded_op
				same_pc_ordinal: 0
			}
		} else {
			if previous_ordinal == max_u8 {
				return memory_frame_cfi_encode_error('same-PC ordinal overflow')
			}
			decoded_op = MemoryFrameCfiDecodedOp{
				...decoded_op
				same_pc_ordinal: previous_ordinal + 1
			}
		}
		if opcode == 0x0e {
			value, _, next_cursor := memory_frame_cfi_encode_read_uleb128(bytes,
				cursor)!
			cursor = next_cursor
			if value > memory_frame_cfi_encode_max_cfa_offset_bytes {
				return memory_frame_cfi_encode_error('CFA offset cap exceeded')
			}
			cfa = value
			decoded_op = MemoryFrameCfiDecodedOp{
				...decoded_op
				kind:             .def_cfa_offset
				cfa_offset_bytes: cfa
			}
		} else if opcode >= 0x80 && opcode <= 0xbf {
			factor, _, next_cursor := memory_frame_cfi_encode_read_uleb128(bytes,
				cursor)!
			cursor = next_cursor
			if factor > u64(max_i64) / 8 {
				return memory_frame_cfi_encode_error('saved-register displacement overflow')
			}
			decoded_op = MemoryFrameCfiDecodedOp{
				...decoded_op
				kind:                         .offset_register
				cfa_offset_bytes:             cfa
				register_present:             true
				dwarf_register_number:        opcode & 0x3f
				saved_cfa_displacement_bytes: -i64(factor * 8)
			}
		} else {
			return memory_frame_cfi_encode_error('unsupported CFI opcode 0x${opcode:02x}')
		}
		if decoded.len >= expected_operation_count {
			return memory_frame_cfi_encode_error('decoded operation count exceeds preflight')
		}
		decoded << decoded_op
		advance_pending = false
		has_previous_op = true
		previous_op_code = code_offset
		previous_ordinal = decoded_op.same_pc_ordinal
	}
	if advance_pending {
		return memory_frame_cfi_encode_error('trailing code advance')
	}
	if decoded.len != expected_operation_count {
		return memory_frame_cfi_encode_error('decoded operation count invariant failed')
	}
	return MemoryFrameCfiDecodedFragment{
		final_code_offset_bytes: u8(code_offset)
		final_cfa_offset_bytes:  cfa
		ops:                     decoded
	}
}

fn memory_frame_cfi_encode_validate_decoded(source []MemoryFrameCfiOp, decoded &MemoryFrameCfiDecodedFragment, expected_final_code u64, expected_final_cfa u64) ! {
	if decoded.ops.len != source.len || u64(decoded.final_code_offset_bytes) != expected_final_code
		|| decoded.final_cfa_offset_bytes != expected_final_cfa {
		return memory_frame_cfi_encode_error('decoded fragment endpoint invariant failed')
	}
	for index, op in source {
		actual := decoded.ops[index]
		if actual.instruction_end_offset_bytes != op.instruction_end_offset_bytes
			|| actual.same_pc_ordinal != op.same_pc_ordinal || actual.kind != op.kind
			|| actual.cfa_offset_bytes != op.cfa_offset_bytes
			|| actual.register_present != op.register_present
			|| actual.dwarf_register_number != op.dwarf_register_number
			|| actual.saved_cfa_displacement_bytes != op.saved_cfa_displacement_bytes {
			return memory_frame_cfi_encode_error('decoded semantic operation invariant failed')
		}
	}
}

fn memory_frame_cfi_encode_fragment(region MemoryFrameCfiRegion, initial_cfa u64, preflight MemoryFrameCfiEncodeRegionPreflight, bytes []u8) MemoryFrameDwarfFdeInstructionFragment {
	return MemoryFrameDwarfFdeInstructionFragment{
		present:                                        true
		region:                                         region
		region_relative_initial_code_offset_bytes:      0
		region_relative_final_code_offset_bytes:        u8(preflight.final_code_offset)
		initial_cfa_offset_bytes:                       initial_cfa
		final_cfa_offset_bytes:                         preflight.final_cfa_offset
		semantic_operation_count:                       u8(preflight.operation_count)
		region_relative_fde_instruction_fragment_bytes: bytes.clone()
	}
}

fn memory_frame_cfi_encode_absent_fragment(region MemoryFrameCfiRegion) MemoryFrameDwarfFdeInstructionFragment {
	return MemoryFrameDwarfFdeInstructionFragment{
		region:                                         region
		region_relative_fde_instruction_fragment_bytes: []u8{}
	}
}

fn memory_frame_cfi_encode_validate_windows_none(plan &MemoryFrameCfiInstructionFragmentPlan) ! {
	if plan.cfi.disposition != .windows_none || plan.cie_initial_instruction_bytes.len != 0
		|| plan.prologue_fde_instruction_fragment !=
		memory_frame_cfi_encode_absent_fragment(.prologue)
		|| plan.epilogue_fde_instruction_template !=
		memory_frame_cfi_encode_absent_fragment(.epilogue_template)
		|| plan.total_instruction_fragment_bytes != 0 {
		return memory_frame_cfi_encode_error('Windows-none output invariant failed')
	}
}

// plan_memory_saved_frame_cfi_instruction_fragments produces inert instruction subsequences.
pub fn plan_memory_saved_frame_cfi_instruction_fragments(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts) !MemoryFrameCfiInstructionFragmentPlan {
	cfi := plan_memory_saved_frame_cfi(facts, saves)!
	if cfi.disposition == .windows_none {
		result := MemoryFrameCfiInstructionFragmentPlan{
			cfi:                               cfi
			cie_initial_instruction_bytes:     []u8{}
			prologue_fde_instruction_fragment: memory_frame_cfi_encode_absent_fragment(.prologue)
			epilogue_fde_instruction_template: memory_frame_cfi_encode_absent_fragment(.epilogue_template)
		}
		memory_frame_cfi_encode_validate_windows_none(&result)!
		return result
	}

	preflight := memory_frame_cfi_encode_preflight(&cfi)!
	prologue_bytes := memory_frame_cfi_encode_region(cfi.prologue_ops, preflight.prologue)!
	epilogue_bytes := memory_frame_cfi_encode_region(cfi.epilogue_template_ops,
		preflight.epilogue)!
	prologue_decoded := memory_frame_cfi_encode_decode_fragment(prologue_bytes, .prologue,
		cfi.baseline.cfa_offset_bytes, cfi.prologue_ops.len)!
	epilogue_decoded := memory_frame_cfi_encode_decode_fragment(epilogue_bytes,
		.epilogue_template, cfi.frame.body_cfa_offset_bytes,
		cfi.epilogue_template_ops.len)!
	memory_frame_cfi_encode_validate_decoded(cfi.prologue_ops, &prologue_decoded,
		preflight.prologue.final_code_offset, preflight.prologue.final_cfa_offset)!
	memory_frame_cfi_encode_validate_decoded(cfi.epilogue_template_ops, &epilogue_decoded,
		preflight.epilogue.final_code_offset, preflight.epilogue.final_cfa_offset)!

	result := MemoryFrameCfiInstructionFragmentPlan{
		cfi:                                   cfi
		cie_initial_instruction_bytes:         memory_frame_cfi_encode_cie_initial_instruction_bytes.clone()
		prologue_fde_instruction_fragment:     memory_frame_cfi_encode_fragment(.prologue,
			cfi.baseline.cfa_offset_bytes, preflight.prologue, prologue_bytes)
		epilogue_fde_instruction_template:     memory_frame_cfi_encode_fragment(.epilogue_template,
			cfi.frame.body_cfa_offset_bytes, preflight.epilogue, epilogue_bytes)
		total_instruction_fragment_bytes:      u8(preflight.total)
	}
	return result
}
