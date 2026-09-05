module util

import strings

// InlineAsmHeader describes the words a V assembly block writes before its `{`,
// for example `asm amd64 raw intel {`.
pub struct InlineAsmHeader {
pub:
	arch        string
	is_volatile bool
	is_raw      bool
	is_intel    bool
}

// parse_inline_asm_header reads the instruction set and template modifiers from the
// text preceding an assembly block's opening brace.
pub fn parse_inline_asm_header(source string) InlineAsmHeader {
	mut arch := ''
	mut is_volatile := false
	mut is_raw := false
	mut is_intel := false
	for word in source.fields() {
		if word == 'asm' {
			continue
		}
		if word == 'volatile' {
			is_volatile = true
			continue
		}
		// The instruction set always comes first, so a leading `raw` or `intel` would
		// name the architecture rather than a modifier.
		if arch.len > 0 && word == 'raw' {
			is_raw = true
			continue
		}
		if arch.len > 0 && word == 'intel' {
			is_intel = true
			continue
		}
		if arch.len == 0 {
			arch = word
		}
	}
	return InlineAsmHeader{
		arch: arch
		is_volatile: is_volatile
		is_raw: is_raw
		is_intel: is_intel
	}
}

// asm_register_names returns every register name an assembly block for arch can use.
// An empty result means the architecture has no table here, so callers must not treat
// unknown names as errors.
pub fn asm_register_names(arch string) []string {
	return match arch {
		'amd64', 'i386', 'x86' { x86_asm_register_names() }
		'arm64', 'aarch64' { arm64_asm_register_names() }
		else { []string{} }
	}
}

// x86_asm_register_names mirrors the i386 and amd64 register set V's assembly blocks
// accept, including the numbered general purpose, vector, mask, control and debug ones.
pub fn x86_asm_register_names() []string {
	mut names := ['al', 'ah', 'bl', 'bh', 'cl', 'ch', 'dl', 'dh', 'bpl', 'sil', 'dil', 'spl', 'ax',
		'bx', 'cx', 'dx', 'bp', 'si', 'di', 'sp', 'cs', 'ss', 'ds', 'es', 'fs', 'gs', 'flags', 'ip',
		'gdtr', 'idtr', 'tr', 'ldtr', 'cw', 'sw', 'tw', 'fp_ip', 'fp_dp', 'fp_cs', 'fp_ds', 'fp_opc',
		'eax', 'ebx', 'ecx', 'edx', 'ebp', 'esi', 'edi', 'esp', 'eflags', 'eip', 'mxcsr', 'rax',
		'rbx', 'rcx', 'rdx', 'rbp', 'rsi', 'rdi', 'rsp', 'rflags', 'rip']
	for i in 0 .. 8 {
		names << 'k${i}'
	}
	for i in 0 .. 16 {
		names << 'r${i}'
		names << 'r${i}b'
		names << 'r${i}w'
		names << 'r${i}d'
		names << 'mm${i}'
		names << 'cr${i}'
		names << 'dr${i}'
		names << 'st${i}'
	}
	for i in 0 .. 32 {
		names << 'xmm${i}'
		names << 'ymm${i}'
		names << 'zmm${i}'
	}
	return names
}

// arm64_asm_register_names returns the register names an arm64 assembly block can use.
pub fn arm64_asm_register_names() []string {
	mut names := ['sp', 'wsp', 'lr', 'fp', 'pc', 'xzr', 'wzr', 'nzcv', 'fpcr']
	for i in 0 .. 31 {
		names << 'x${i}'
		names << 'w${i}'
	}
	for i in 0 .. 16 {
		names << 'p${i}'
	}
	for i in 0 .. 32 {
		names << 'v${i}'
		names << 'q${i}'
		names << 'd${i}'
		names << 's${i}'
		names << 'h${i}'
		names << 'b${i}'
		names << 'z${i}'
	}
	return names
}

// asm_clobber_is_special reports whether name is one of the GNU pseudo clobbers that
// never name a register.
pub fn asm_clobber_is_special(name string) bool {
	return name in ['cc', 'memory', 'dirflag', 'fpsr', 'flags']
}

// closest_asm_register returns the register in registers that name was most likely
// meant to be, or none when nothing is close enough to be worth suggesting.
pub fn closest_asm_register(name string, registers []string) ?string {
	if name.len == 0 || registers.len == 0 {
		return none
	}
	mut digit_start := -1
	for i, character in name {
		if character.is_digit() {
			digit_start = i
			break
		}
	}
	// `xmm01` and `xmm1` name the same register, so drop redundant leading zeroes first.
	if digit_start > 0 && name[digit_start..].bytes().all(it.is_digit()) {
		normalized := name[..digit_start] + name[digit_start..].int().str()
		if normalized != name && normalized in registers {
			return normalized
		}
	}
	mut candidates := registers.clone()
	candidates.sort()
	mut closest := ''
	mut closest_distance := 3
	for candidate in candidates {
		if candidate == name {
			continue
		}
		distance := strings.levenshtein_distance(name, candidate)
		if distance < closest_distance {
			closest = candidate
			closest_distance = distance
		}
	}
	limit := if name.len <= 4 { 1 } else { 2 }
	if closest != '' && closest_distance <= limit {
		return closest
	}
	return none
}
