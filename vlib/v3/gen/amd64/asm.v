// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

struct CallRel32Site {
	field_offset u64
}

struct JmpRel32Site {
	field_offset int
}

fn emit_sub_rsp_8(mut text []u8) {
	text << [u8(0x48), 0x83, 0xec, 0x08]
}

fn emit_sub_rsp_40(mut text []u8) {
	text << [u8(0x48), 0x83, 0xec, 0x28]
}

fn emit_call_rel32_placeholder(mut text []u8) CallRel32Site {
	site := CallRel32Site{
		field_offset: u64(text.len) + 1
	}
	text << [u8(0xe8), 0x00, 0x00, 0x00, 0x00]
	return site
}

fn checked_jmp_rel32_displacement(next_instruction_offset i64, target_offset i64) !i32 {
	displacement := target_offset - next_instruction_offset
	if displacement < -2_147_483_648 || displacement > 2_147_483_647 {
		return lowering_error('JMP rel32',
			'displacement ${displacement} is outside signed 32-bit range')
	}
	return i32(displacement)
}

fn emit_jmp_rel32_placeholder(mut text []u8) JmpRel32Site {
	site := JmpRel32Site{
		field_offset: text.len + 1
	}
	text << [u8(0xe9), 0x00, 0x00, 0x00, 0x00]
	return site
}

fn patch_jmp_rel32(mut text []u8, site JmpRel32Site, target_offset int) ! {
	if site.field_offset <= 0 || site.field_offset > text.len - 4 {
		return lowering_error('JMP rel32',
			'field offset ${site.field_offset} is outside emitted text length ${text.len}')
	}
	if text[site.field_offset - 1] != 0xe9 {
		return lowering_error('JMP rel32',
			'field ${site.field_offset} is not preceded by opcode E9')
	}
	for byte_offset in 0 .. 4 {
		if text[site.field_offset + byte_offset] != 0 {
			return lowering_error('JMP rel32',
				'field ${site.field_offset} is not a zero placeholder')
		}
	}
	next_instruction_offset := site.field_offset + 4
	displacement :=
		checked_jmp_rel32_displacement(i64(next_instruction_offset), i64(target_offset))!
	bits := u32(i64(displacement) & i64(0xffff_ffff))
	for byte_offset in 0 .. 4 {
		text[site.field_offset + byte_offset] = u8(bits >> (byte_offset * 8))
	}
}

fn emit_xor_eax_eax(mut text []u8) {
	text << [u8(0x31), 0xc0]
}

fn emit_mov_rax_imm64(mut text []u8, value u64) {
	text << [u8(0x48), 0xb8]
	for byte_offset in 0 .. 8 {
		text << u8(value >> (byte_offset * 8))
	}
}

fn emit_mov_rdi_imm64(mut text []u8, value u64) {
	text << [u8(0x48), 0xbf]
	for byte_offset in 0 .. 8 {
		text << u8(value >> (byte_offset * 8))
	}
}

fn emit_mov_rcx_imm64(mut text []u8, value u64) {
	text << [u8(0x48), 0xb9]
	for byte_offset in 0 .. 8 {
		text << u8(value >> (byte_offset * 8))
	}
}

fn emit_mov_rax_rdi(mut text []u8) {
	text << [u8(0x48), 0x89, 0xf8]
}

fn emit_mov_rax_rcx(mut text []u8) {
	text << [u8(0x48), 0x89, 0xc8]
}

fn abi_integer_gpr_code(register AbiRegister) !int {
	return match register {
		.rax { 0 }
		.rcx { 1 }
		.rdx { 2 }
		.rsi { 6 }
		.rdi { 7 }
		.r8 { 8 }
		.r9 { 9 }
		else {
			return lowering_error('scalar ABI emission',
				'register ${int(register)} is not an integer GPR')
		}
	}
}

fn abi_emit_integer_prefix(mut text []u8, width_bytes int, rex_bits u8, force_byte_rex bool) ! {
	if width_bytes !in [1, 2, 4, 8] {
		return lowering_error('scalar ABI emission',
			'integer transfer width must be 1, 2, 4, or 8 bytes, got ${width_bytes}')
	}
	if width_bytes == 2 {
		text << u8(0x66)
	}
	mut rex := u8(0x40) | rex_bits
	if width_bytes == 8 {
		rex |= u8(0x08)
	}
	if width_bytes == 8 || rex_bits != 0 || (width_bytes == 1 && force_byte_rex) {
		text << rex
	}
}

fn abi_emit_little_endian(mut text []u8, value u64, width_bytes int) {
	for byte_offset in 0 .. width_bytes {
		text << u8(value >> (byte_offset * 8))
	}
}

fn emit_mov_gpr_imm(mut text []u8, register AbiRegister, width_bytes int, value u64) ! {
	code := abi_integer_gpr_code(register)!
	force_byte_rex := code & 7 in [6, 7]
	abi_emit_integer_prefix(mut text, width_bytes, if code >= 8 { u8(0x01) } else { u8(0) },
		force_byte_rex)!
	opcode_base := if width_bytes == 1 { u8(0xb0) } else { u8(0xb8) }
	text << opcode_base + u8(code & 7)
	abi_emit_little_endian(mut text, value, width_bytes)
}

fn emit_mov_rax_gpr(mut text []u8, source AbiRegister, width_bytes int) ! {
	code := abi_integer_gpr_code(source)!
	force_byte_rex := code & 7 in [6, 7]
	abi_emit_integer_prefix(mut text, width_bytes, if code >= 8 { u8(0x04) } else { u8(0) },
		force_byte_rex)!
	text << if width_bytes == 1 { u8(0x88) } else { u8(0x89) }
	text << u8(0xc0) | (u8(code & 7) << 3)
}

fn abi_emit_rsp_modrm(mut text []u8, stack_offset_bytes int) ! {
	if stack_offset_bytes < 0 || stack_offset_bytes > 127 {
		return lowering_error('scalar ABI emission',
			'RSP offset must be in 0..127, got ${stack_offset_bytes}')
	}
	text << if stack_offset_bytes == 0 { u8(0x04) } else { u8(0x44) }
	text << u8(0x24)
	if stack_offset_bytes != 0 {
		text << u8(stack_offset_bytes)
	}
}

fn emit_mov_rax_rsp_offset(mut text []u8, stack_offset_bytes int, width_bytes int) ! {
	abi_emit_integer_prefix(mut text, width_bytes, 0, false)!
	text << if width_bytes == 1 { u8(0x8a) } else { u8(0x8b) }
	abi_emit_rsp_modrm(mut text, stack_offset_bytes)!
}

fn emit_mov_rsp_offset_rax(mut text []u8, stack_offset_bytes int, width_bytes int) ! {
	abi_emit_integer_prefix(mut text, width_bytes, 0, false)!
	text << if width_bytes == 1 { u8(0x88) } else { u8(0x89) }
	abi_emit_rsp_modrm(mut text, stack_offset_bytes)!
}

fn emit_add_rsp_8(mut text []u8) {
	text << [u8(0x48), 0x83, 0xc4, 0x08]
}

fn emit_add_rsp_40(mut text []u8) {
	text << [u8(0x48), 0x83, 0xc4, 0x28]
}

fn emit_ret(mut text []u8) {
	text << u8(0xc3)
}
