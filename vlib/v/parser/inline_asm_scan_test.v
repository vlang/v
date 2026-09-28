module parser

fn test_inline_asm_guard_scan_skips_template_modifiers() {
	assert source_contains_target_inline_asm('asm amd64 raw intel { "nop" }', 'amd64')
	assert source_contains_target_inline_asm('asm ia-32 intel { nop }', 'x86')

	tokens := [
		InlineAsmScanToken{ kind: .key_asm, lit: 'asm' },
		InlineAsmScanToken{ kind: .name, lit: 'amd64' },
		InlineAsmScanToken{ kind: .name, lit: 'raw' },
		InlineAsmScanToken{ kind: .name, lit: 'intel' },
		InlineAsmScanToken{ kind: .lcbr, lit: '{' },
		InlineAsmScanToken{ kind: .rcbr, lit: '}' },
	]
	assert inline_asm_tokens_match_target(tokens, 0, tokens.len, 'amd64')

	ia32_tokens := [
		InlineAsmScanToken{ kind: .key_asm, lit: 'asm' },
		InlineAsmScanToken{ kind: .name, lit: 'ia' },
		InlineAsmScanToken{ kind: .minus, lit: '-' },
		InlineAsmScanToken{ kind: .number, lit: '32' },
		InlineAsmScanToken{ kind: .name, lit: 'intel' },
		InlineAsmScanToken{ kind: .lcbr, lit: '{' },
		InlineAsmScanToken{ kind: .rcbr, lit: '}' },
	]
	assert inline_asm_tokens_match_target(ia32_tokens, 0, ia32_tokens.len, 'x86')
}
