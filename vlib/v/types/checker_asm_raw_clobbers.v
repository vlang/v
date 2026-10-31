module types

import v.flat
import v.token

// inline_asm_raw_gp_registers are the 64-bit general-purpose registers that a GNU x86 raw
// template can name with `%%`. `rsp` is left out because no template should write it.
const inline_asm_raw_gp_registers = ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'r8', 'r9',
	'r10', 'r11', 'r12', 'r13', 'r14', 'r15']!

// inline_asm_raw_writes_dst_families are the GNU x86 mnemonics whose last operand, the
// destination, is written. `cmp` and `test` only read their operands.
const inline_asm_raw_writes_dst_families = ['mov', 'add', 'sub', 'and', 'or', 'xor', 'inc', 'dec',
	'neg', 'not', 'shl', 'shr', 'sar', 'lea', 'pop', 'imul']!

// inline_asm_raw_string_families are the GNU x86 string instructions. Only these use `%%rcx`
// as the repeat count under a `rep` prefix; `repz ret` does not touch it.
const inline_asm_raw_string_families = ['movs', 'stos', 'lods', 'cmps', 'scas', 'ins', 'outs']!

// inline_asm_raw_flag_families are the GNU x86 mnemonics that set the condition flags.
const inline_asm_raw_flag_families = ['add', 'sub', 'and', 'or', 'xor', 'cmp', 'test', 'inc', 'dec',
	'neg', 'shl', 'shr', 'sar', 'imul', 'mul', 'div', 'idiv']!

// inline_asm_raw_x86_family maps a register name to the 64-bit general-purpose register that
// contains it, so `%%eax` and `%%al` both name `rax`. Other names are returned unchanged.
fn inline_asm_raw_x86_family(name string) string {
	match name {
		'rax', 'eax', 'ax', 'al', 'ah' { return 'rax' }
		'rbx', 'ebx', 'bx', 'bl', 'bh' { return 'rbx' }
		'rcx', 'ecx', 'cx', 'cl', 'ch' { return 'rcx' }
		'rdx', 'edx', 'dx', 'dl', 'dh' { return 'rdx' }
		'rsi', 'esi', 'si', 'sil' { return 'rsi' }
		'rdi', 'edi', 'di', 'dil' { return 'rdi' }
		'rbp', 'ebp', 'bp', 'bpl' { return 'rbp' }
		else {}
	}
	if name.len >= 2 && name[0] == `r` && name[1].is_digit() {
		// `r8d`, `r8w` and `r8b` are the narrower views of `r8`.
		mut end := name.len
		if name.len >= 3 && name[end - 1] in [`d`, `w`, `b`] {
			end--
		}
		return name[..end]
	}
	return name
}

// inline_asm_raw_base_mnemonic returns the family of a GNU x86 mnemonic, with or without its
// size suffix, or an empty string when the family is not in families. `addq` and `add` share
// the family `add`, while `movsb` is not a `mov` family member.
fn inline_asm_raw_base_mnemonic(name string, families []string) string {
	if name in families {
		return name
	}
	if name.len > 1 && name[name.len - 1] in [`b`, `w`, `l`, `q`]
		&& name[..name.len - 1] in families {
		return name[..name.len - 1]
	}
	return ''
}

// inline_asm_raw_fixed_output_registers returns the general-purpose registers that output
// operands bind through their constraints, such as `"=a"` for `rax`, and through `{rax}`.
fn inline_asm_raw_fixed_output_registers(block string, section InlineAsmRange) []string {
	mut registers := []string{}
	for io in inline_asm_ios(block, section) {
		if io.constraint.contains('@') {
			continue
		}
		if io.constraint.contains('{') {
			name := io.constraint.all_after('{').all_before('}')
			registers << inline_asm_raw_x86_family(name)
			continue
		}
		for c in io.constraint {
			match c {
				`a` { registers << 'rax' }
				`b` { registers << 'rbx' }
				`c` { registers << 'rcx' }
				`d` { registers << 'rdx' }
				`S` { registers << 'rsi' }
				`D` { registers << 'rdi' }
				else {}
			}
		}
	}
	return registers
}

// inline_asm_raw_mul_writes_dx reports whether a mul, imul, div or idiv is proven to write DX.
// A `b` suffix never does. A `w`, `l` or `q` suffix does, and so does a resolved operand width of
// 16, 32 or 64 bits. An unresolved width proves nothing, so the check stays silent.
fn inline_asm_raw_mul_writes_dx(name string, width int) bool {
	if name.len > 1 && name[name.len - 1] == `b` {
		return false
	}
	if name.len > 1 && name[name.len - 1] in [`w`, `l`, `q`]
		&& name[..name.len - 1] in ['mul', 'imul', 'div', 'idiv'] {
		return true
	}
	return width == 16 || width == 32 || width == 64
}

// check_inline_asm_raw_clobbers reports a GNU x86 raw template that writes a general-purpose
// register, or sets the flags, without declaring it. It only reports the cases it can prove:
// a destination register operand, the registers that `mul`, `div`, `cqo` and `rep` write
// implicitly, and a flag-setting instruction without `cc`. Other mnemonics are not reported.
fn (mut tc TypeChecker) check_inline_asm_raw_clobbers(id flat.NodeId, node flat.Node, block string, base int, sections []InlineAsmRange, arch string, types map[string]Type) {
	template := inline_asm_raw_template(block, sections[0], arch)
	instructions := inline_asm_raw_parse_instructions(&template, [])
	mut clobbers := map[string]bool{}
	mut has_cc := false
	if sections.len > 3 {
		for word in inline_asm_words(block, sections[3]) {
			if word.text == 'cc' {
				has_cc = true
			}
			clobbers[inline_asm_raw_x86_family(word.text)] = true
		}
	}
	if sections.len > 1 {
		for io in inline_asm_ios(block, sections[1]) {
			// A flag output such as `"=@ccz"` declares the flags without a `cc` clobber.
			if io.constraint.contains('@cc') {
				has_cc = true
			}
		}
		for register in inline_asm_raw_fixed_output_registers(block, sections[1]) {
			clobbers[register] = true
		}
	}
	arch_bits := inline_asm_att_arch_bits(arch)
	for instruction in instructions {
		if instruction.is_directive || instruction.mnemonic.len == 0 {
			continue
		}
		words := instruction.mnemonic.split(' ')
		name := words[words.len - 1].to_lower_ascii()
		mnemonic_start, mnemonic_end := template.span(instruction.mnemonic_start, instruction.mnemonic_end)
		mut written := map[string]bool{}
		// A destination operand is reported at its own span, implicit writes at the mnemonic.
		mut written_at := map[string]token.Pos{}
		family := inline_asm_raw_base_mnemonic(name, inline_asm_raw_writes_dst_families)
		mul_family := inline_asm_raw_base_mnemonic(name, ['mul', 'imul', 'div', 'idiv'])
		if mul_family.len > 0 && instruction.operands.len > 0 && (mul_family == 'div'
			|| mul_family == 'idiv' || instruction.operands.len == 1) {
			written['rax'] = true
			last := instruction.operands[instruction.operands.len - 1]
			width := tc.inline_asm_att_operand_width(last, types, arch_bits)
			if inline_asm_raw_mul_writes_dx(name, width) {
				written['rdx'] = true
			}
		}
		if name in ['cqo', 'cqto', 'cdq', 'cltd'] {
			written['rdx'] = true
		}
		if inline_asm_raw_base_mnemonic(name, inline_asm_raw_string_families).len > 0 {
			for word in words {
				if word in ['rep', 'repe', 'repz', 'repne', 'repnz'] {
					written['rcx'] = true
				}
			}
		}
		is_movx := name.len == 6 && (name.starts_with('movz') || name.starts_with('movs'))
		writes_dst := (family.len > 0 && family != 'imul') || (family == 'imul' && instruction.operands.len > 1)
		if (writes_dst || is_movx) && instruction.operands.len > 0 {
			dst := instruction.operands[instruction.operands.len - 1]
			if dst.kind == .register {
				register := inline_asm_raw_x86_family(dst.name)
				start, end := template.span(dst.start, dst.end)
				written[register] = true
				written_at[register] = token.new_span(node.pos.id, base + start, base + end)
			}
		}
		for register in inline_asm_raw_gp_registers {
			if written[register] && !clobbers[register] {
				span := if register in written_at {
					written_at[register]
				} else {
					token.new_span(node.pos.id, base + mnemonic_start, base + mnemonic_end)
				}
				tc.record_error_at(.compile_error, '`${instruction.mnemonic}` writes `${register}` in this raw assembly template, but `${register}` is neither an output operand nor in the clobber list; add `${register}` to the clobbers',
					id, span)
			}
		}
		if inline_asm_raw_base_mnemonic(name, inline_asm_raw_flag_families).len > 0 && !has_cc {
			tc.record_error_at(.compile_error, '`${instruction.mnemonic}` sets the condition flags in this raw assembly template, but the clobbers do not list `cc`; add `cc` to the clobbers',
				id, token.new_span(node.pos.id, base + mnemonic_start, base + mnemonic_end))
		}
	}
}
