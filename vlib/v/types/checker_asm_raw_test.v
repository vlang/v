module types

import os
import v.parser
import v.pref
import v.util

fn raw_label_checker(name string, source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'raw_asm_labels_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	// Check retained templates for each ISA without requiring a matching host backend.
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	return tc
}

fn test_raw_labels_accept_quoted_exported_symbols() {
	tc := raw_label_checker('quoted_exports', 'module main
fn main() {
 asm arm64 raw {
  ".globl \\"first_symbol\\"\\n"
  ".global \\"second_symbol\\"\\n"
  ".weak \\"third_symbol\\"\\n"
  "first_symbol: second_symbol: third_symbol: nop"
 }
}
')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 0, tc.notices.str()
}

fn test_raw_labels_skip_unknown_assembler_comment_grammars() {
	for arch, comment in {
		'arm':     '@'
		'riscv64': '#'
	} {
		tc := raw_label_checker(arch, 'module main
fn main() {
 asm ${arch} raw {
  "${comment} ignored; fake_label:\\n"
  "nop"
 }
}
')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.notices.len == 0, tc.notices.str()
	}
}

fn test_raw_labels_still_warn_for_other_unexported_symbols() {
	tc := raw_label_checker('unexported', 'module main
fn main() {
 asm arm64 raw {
  ".globl \\"exported_symbol\\"\\n"
  ".globl \\"private\\\\_symbol\\"\\n"
  "private_symbol: nop"
 }
}
')
	assert tc.errors.len == 0, tc.errors.str()
	// The quoted backslash is part of a different assembler symbol, not an escape.
	assert tc.notices.len == 1, tc.notices.str()
	assert tc.notices[0].severity == 'warning:'
	assert tc.notices[0].msg.starts_with('label `private_symbol` in a raw assembly template has no `%=` suffix')
}

fn test_raw_labels_warn_for_quoted_definitions() {
	for arch in ['amd64', 'arm64'] {
		source := 'module main
fn main() {
 asm ${arch} raw {
  "\\"quoted_label\\": \\"1\\": nop\\n"
  "\\"escaped\\\\\\"quote\\": nop\\n"
  ;
 }
}
'
		tc := raw_label_checker('quoted_${arch}', source)
		assert tc.errors.len == 0, tc.errors.str()
		// A quoted digit names an ordinary symbol, unlike the numeric local label `1:`.
		assert tc.notices.len == 3, tc.notices.str()
		for name in ['quoted_label', '1', 'escaped\\"quote'] {
			assert tc.notices.any(it.msg.starts_with('label `${name}` in a raw assembly template'))
		}
		escaped_notices := tc.notices.filter(it.msg.starts_with('label `escaped'))
		escaped := escaped_notices[0].pos
		assert source[int(escaped.offset)..int(escaped.end)] == 'escaped\\\\\\"quote'
	}
}

fn test_raw_labels_accept_safe_quoted_definitions() {
	for arch in ['amd64', 'arm64'] {
		tc := raw_label_checker('safe_quoted_${arch}', 'module main
fn main() {
 asm ${arch} raw {
  ".globl exported\\n"
  ".global \\"quoted export\\"\\n"
  "\\"exported\\": \\"quoted export\\": \\"unique%=\\": 1: nop\\n"
  ".ascii \\"text: inside a string\\"\\n"
  ".weak \\"escaped\\\\\\"quote\\"\\n"
  "\\"escaped\\\\\\"quote\\": nop\\n"
  ;
 }
}
')
		assert tc.errors.len == 0, tc.errors.str()
		assert tc.notices.len == 0, tc.notices.str()
	}
}

// raw_template_of decodes the template section of a complete raw asm block, so the
// tests exercise the same block offsets a checker diagnostic would use.
fn raw_template_of(source string) InlineAsmRawTemplate {
	block := inline_asm_mask_comments(source)
	open := inline_asm_index_of(block, 0, block.len, `{`) or { panic('no `{`') }
	close := inline_asm_last_index_of(block, open, block.len, `}`) or { panic('no `}`') }
	sections := inline_asm_section_ranges(block, open + 1, close)
	return inline_asm_raw_template(block, sections[0], util.parse_inline_asm_header(block[..open]).arch)
}

fn first_label(instruction InlineAsmRawInstruction) string {
	return if instruction.labels.len > 0 { instruction.labels[0].name } else { '' }
}

// raw_instructions_of tokenizes a block the way the checker will: bare register names
// only exist outside GNU x86 syntax, where every register carries `%%`.
fn raw_instructions_of(source string, arch string) []InlineAsmRawInstruction {
	template := raw_template_of(source)
	registers := if arch in ['amd64', 'i386'] { []string{} } else { util.asm_register_names(arch) }
	return inline_asm_raw_parse_instructions(&template, registers)
}

const raw_loop_block = 'asm amd64 raw {
	"testq %[len], %[len]\\n\\t"
	"jz .Ldone%=\\n\\t" // skip empty input
	".Lloop%=: \\n\\t"
	"movzbl ( %[ptr] ), %%eax\\n\\t"
	"addq %%rax, %[sum]; incq %[ptr]\\n\\t"
	"decq %[len]\\n\\t"
	"jnz .Lloop%=\\n\\t"
	".Ldone%=:"
	; [sum] "+r" (sum)
	  [ptr] "+r" (ptr)
	  [len] "+r" (len)
	;
	; rax
	  memory
	  cc
}'

fn test_raw_template_decodes_literals_into_assembler_text() {
	template := raw_template_of(raw_loop_block)
	assert template.text == 'testq %[len], %[len]\n\tjz .Ldone%=\n\t.Lloop%=: \n\tmovzbl ( %[ptr] ), %%eax\n\taddq %%rax, %[sum]; incq %[ptr]\n\tdecq %[len]\n\tjnz .Lloop%=\n\t.Ldone%=:'
	assert template.starts.len == template.text.len
	assert template.ends.len == template.text.len
}

fn test_raw_template_maps_decoded_bytes_back_to_source() {
	template := raw_template_of(raw_loop_block)
	// A plain byte maps onto itself.
	jz := template.text.index('jz .Ldone') or { -1 }
	start, end := template.span(jz, jz + 2)
	assert raw_loop_block[start..end] == 'jz'
	// An escape maps one decoded byte onto its two source bytes.
	newline := template.text.index_u8(`\n`)
	nl_start, nl_end := template.span(newline, newline + 1)
	assert raw_loop_block[nl_start..nl_end] == '\\n'
	// A range that crosses literals covers everything between them in the source.
	first_label := template.text.index('.Lloop%=') or { -1 }
	label_start, label_end := template.span(first_label, first_label + '.Lloop%='.len)
	assert raw_loop_block[label_start..label_end] == '.Lloop%='
	tail := template.text.index('decq') or { -1 }
	cross_start, cross_end := template.span(first_label, tail + 'decq'.len)
	assert raw_loop_block[cross_start..cross_end].starts_with('.Lloop%=')
	assert raw_loop_block[cross_start..cross_end].ends_with('decq')
	assert raw_loop_block[cross_start..cross_end].contains('"\n\t"')
}

fn test_raw_template_span_handles_empty_and_out_of_range() {
	template := raw_template_of(raw_loop_block)
	start, end := template.span(3, 3)
	assert start == end
	too_far_start, too_far_end := template.span(0, template.text.len + 10)
	_, last_end := template.span(template.text.len - 1, template.text.len)
	assert too_far_end == last_end
	assert too_far_start == template.starts[0]
	empty := inline_asm_raw_template('nothing quoted here', InlineAsmRange{ start: 0, end: 19 }, 'amd64')
	assert empty.text == ''
	empty_start, empty_end := empty.span(0, 1)
	assert empty_start == 0 && empty_end == 0
}

fn test_raw_template_decodes_c_escapes() {
	block := '"a\\\\b\\"c\\x41\\101\\q"'
	template := inline_asm_raw_template(block, InlineAsmRange{ start: 0, end: block.len }, 'amd64')
	assert template.text == 'a\\b"cAAq'
	// `\x41` spans four source bytes.
	x := template.text.index('A') or { -1 }
	start, end := template.span(x, x + 1)
	assert block[start..end] == '\\x41'
	// Like C, `\x` consumes every following hex digit, and `\x` alone is the letter.
	hex := '"\\x001\\x7g\\xz\\0017"'
	decoded := inline_asm_raw_template(hex, InlineAsmRange{ start: 0, end: hex.len }, 'amd64')
	assert decoded.text.bytes() == [u8(1), 7, `g`, `x`, `z`, 1, `7`]
	// Octal escapes stop after three digits; `\0017` is `\001` then `7`.
	seven := decoded.text.len - 1
	seven_start, seven_end := decoded.span(seven, seven + 1)
	assert hex[seven_start..seven_end] == '7'
	one_start, one_end := decoded.span(seven - 1, seven)
	assert hex[one_start..one_end] == '\\001'
}

fn test_raw_template_span_at_end_of_text_is_past_the_last_byte() {
	block := '"nop\\n"'
	template := inline_asm_raw_template(block, InlineAsmRange{ start: 0, end: block.len }, 'amd64')
	assert template.text == 'nop\n'
	// Position 4 is one past the decoded text, which is the closing quote in the source.
	start, end := template.span(template.text.len, template.text.len)
	assert start == end
	assert block[start] == `"`
	also_start, also_end := template.span(template.text.len, template.text.len + 1)
	assert also_start == start && also_end == end
	// A lone trailing backslash is kept as a literal byte.
	lone := '"a\\'
	lone_template := inline_asm_raw_template(lone, InlineAsmRange{ start: 0, end: lone.len }, 'amd64')
	assert lone_template.text == 'a\\'
}

fn test_raw_lines_split_on_newline_and_semicolon() {
	template := raw_template_of(raw_loop_block)
	lines := inline_asm_raw_lines(&template)
	texts := lines.map(template.text[it.start..it.end].trim_space())
	assert texts == ['testq %[len], %[len]', 'jz .Ldone%=', '.Lloop%=:', 'movzbl ( %[ptr] ), %%eax',
		'addq %%rax, %[sum]', 'incq %[ptr]', 'decq %[len]', 'jnz .Lloop%=', '.Ldone%=:']
}

fn test_raw_lines_keep_semicolons_inside_quoted_text() {
	block := '".ascii \\"a;b\\"\\n\\t"'
	template := inline_asm_raw_template(block, InlineAsmRange{ start: 0, end: block.len }, 'amd64')
	lines := inline_asm_raw_lines(&template)
	assert lines.map(template.text[it.start..it.end]) == ['.ascii "a;b"', '\t']
}

fn test_raw_lines_honour_escaped_quotes_and_character_constants() {
	// The assembler sees `.ascii "a\";b"` then `nop`: the `\"` does not close the string.
	block := 'asm amd64 raw { ".ascii \\"a\\\\\\";b\\"\\n" "nop; ret" ; }'
	instructions := raw_instructions_of(block, 'amd64')
	assert instructions.map(it.mnemonic) == ['.ascii', 'nop', 'ret']
	assert instructions[0].operands.map(it.text) == ['"a\\";b"']
	// A `"` inside a GNU character constant, with or without a closing `\'`, is not a
	// string delimiter, and a `,` inside a string does not split operands.
	chars := 'asm amd64 raw { "movb $\'\\"\', %%al\\n" "movb $\'\\", %%al\\n" ".ascii \\"a,b\\", \\"c\\"" ; }'
	chars_instructions := raw_instructions_of(chars, 'amd64')
	assert chars_instructions.map(it.mnemonic) == ['movb', 'movb', '.ascii']
	assert chars_instructions[0].operands.map(it.text) == ['$\'"\'', '%%al']
	assert chars_instructions[1].operands.map(it.text) == ['$\'"', '%%al']
	assert chars_instructions[2].operands.map(it.text) == ['"a,b"', '"c"']
}

fn test_raw_instructions_labels_mnemonics_and_operands() {
	instructions := raw_instructions_of(raw_loop_block, 'amd64')
	assert instructions.map(it.mnemonic) == ['testq', 'jz', '', 'movzbl', 'addq', 'incq', 'decq',
		'jnz', '']
	assert instructions.map(first_label(it)) == ['', '', '.Lloop%=', '', '', '', '', '', '.Ldone%=']
	assert instructions.map(it.is_directive) == [false, false, false, false, false, false, false,
		false, false]

	testq := instructions[0]
	assert testq.operands.len == 2
	assert testq.operands[0].kind == .operand_ref
	assert testq.operands[0].name == 'len'
	assert testq.operands[0].modifier == 0

	jz := instructions[1]
	assert jz.operands.len == 1
	assert jz.operands[0].kind == .symbol
	assert jz.operands[0].text == '.Ldone%='

	movzbl := instructions[3]
	assert movzbl.operands.len == 2
	assert movzbl.operands[0].kind == .memory
	assert movzbl.operands[0].text == '( %[ptr] )'
	assert movzbl.operands[1].kind == .register
	assert movzbl.operands[1].name == 'eax'

	// The label span points into the string literal in the source.
	template := raw_template_of(raw_loop_block)
	loop := instructions[2]
	start, end := template.span(loop.labels[0].start, loop.labels[0].end)
	assert raw_loop_block[start..end] == '.Lloop%='
}

fn test_raw_operand_modifiers_immediates_and_prefixes() {
	block := 'asm amd64 raw {
	"movl $0x1f, %k[value]\\n\\t"
	"lock addq %q0, 8(%%rax,%%rcx,4)\\n\\t"
	"rep movsb\\n\\t"
	"movq %%fs:0x28, %%rdx\\n\\t"
	"jmp *%%rax\\n\\t"
	"1:\\n\\t"
	"jnz 1b\\n\\t"
	"call memcpy@PLT\\n\\t"
	".byte 0x0f, 0x1f\\n\\t"
	"vpxord %%zmm0, %%zmm0, %%zmm0%{%%k1%}%{z%}\\n\\t"
	"{vex} vpxor %%xmm0, %%xmm1, %%xmm2\\n\\t"
	"rex.w addl %%eax, %%ecx\\n\\t"
	"vextracti128 $1, %%ymm0, %%xmm1\\n\\t"
	"fxch %%st(1), %%st\\n\\t"
	"jmp rax\\n\\t"
	"movq fs:0x28, %%rdx"
	; [value] "+r" (value)
}'
	instructions := raw_instructions_of(block, 'amd64')
	assert instructions.map(it.mnemonic) == ['movl', 'lock addq', 'rep movsb', 'movq', 'jmp', '',
		'jnz', 'call', '.byte', 'vpxord', '{vex} vpxor', 'rex.w addl', 'vextracti128', 'fxch',
		'jmp', 'movq']
	assert instructions[10].operands.map(it.name) == ['xmm0', 'xmm1', 'xmm2']
	assert instructions[11].operands.map(it.name) == ['eax', 'ecx']
	// `vex...` is a mnemonic start, not a prefix word.
	assert instructions[12].operands.map(it.text) == ['$1', '%%ymm0', '%%xmm1']
	// x87 stack registers keep their `(n)` and are registers, not addresses.
	assert instructions[13].operands.map(it.kind) == [.register, .register]
	assert instructions[13].operands.map(it.name) == ['st(1)', 'st']
	// In GNU x86 syntax a bare `rax` is a symbol; only `%%rax` is the register.
	assert instructions[14].operands[0].kind == .symbol
	assert instructions[15].operands[0].kind == .memory

	movl := instructions[0]
	assert movl.operands[0].kind == .immediate
	assert movl.operands[0].name == '0x1f'
	assert movl.operands[1].kind == .operand_ref
	assert movl.operands[1].modifier == `k`
	assert movl.operands[1].name == 'value'

	addq := instructions[1]
	assert addq.operands[0].kind == .operand_ref
	assert addq.operands[0].modifier == `q`
	assert addq.operands[0].name == '0'
	assert addq.operands[1].kind == .memory
	assert addq.operands[1].text == '8(%%rax,%%rcx,4)'

	assert instructions[2].operands.len == 0

	movq := instructions[3]
	assert movq.operands[0].kind == .memory
	assert movq.operands[1].kind == .register
	assert movq.operands[1].name == 'rdx'

	jmp := instructions[4]
	assert jmp.operands[0].kind == .register
	assert jmp.operands[0].name == 'rax'
	assert jmp.operands[0].text == '*%%rax'

	assert first_label(instructions[5]) == '1'
	assert instructions[6].operands[0].kind == .symbol
	assert instructions[6].operands[0].text == '1b'
	assert instructions[7].operands[0].kind == .symbol
	assert instructions[7].operands[0].text == 'memcpy@PLT'

	byte_directive := instructions[8]
	assert byte_directive.is_directive
	assert byte_directive.operands.map(it.text) == ['0x0f', '0x1f']

	vpxord := instructions[9]
	assert vpxord.operands.len == 3
	assert vpxord.operands[2].kind == .register
	assert vpxord.operands[2].text == '%%zmm0%{%%k1%}%{z%}'
	assert vpxord.operands[2].name == 'zmm0%{%%k1%}%{z%}'
}

fn test_raw_operands_on_arm64_use_bare_registers() {
	block := 'asm arm64 raw {
	"ldr q1, [%[state], #16]\\n\\t"
	"mov v8.16b, v0.16b\\n\\t"
	"shl v3.4s, v3.4s, #16\\n\\t"
	".rept 10\\n\\t"
	"add x0, x1, x2\\n\\t"
	"add x0, x0, :lo12:table\\n\\t"
	"ins v6.d[1], v4.d[0]\\n\\t"
	"ldp x0, x1, [sp], #16"
	; [state] "r" (state)
}'
	instructions := raw_instructions_of(block, 'arm64')
	assert instructions.map(it.mnemonic) == ['ldr', 'mov', 'shl', '.rept', 'add', 'add', 'ins',
		'ldp']
	assert instructions[5].operands[2].kind == .symbol
	assert instructions[5].operands[2].text == ':lo12:table'
	// A vector lane index is part of the register, not an address.
	assert instructions[6].operands.map(it.kind) == [.register, .register]
	assert instructions[6].operands.map(it.name) == ['v6', 'v4']
	assert instructions[7].operands.map(it.kind) == [.register, .register, .memory, .immediate]

	ldr := instructions[0]
	assert ldr.operands[0].kind == .register
	assert ldr.operands[0].name == 'q1'
	assert ldr.operands[1].kind == .memory
	assert ldr.operands[1].text == '[%[state], #16]'

	mov := instructions[1]
	assert mov.operands[0].kind == .register
	assert mov.operands[0].name == 'v8'
	assert mov.operands[0].text == 'v8.16b'

	shl := instructions[2]
	assert shl.operands[2].kind == .immediate
	assert shl.operands[2].name == '16'

	assert instructions[3].is_directive
	assert instructions[4].operands.map(it.name) == ['x0', 'x1', 'x2']
}

fn test_raw_intel_templates_use_bare_registers() {
	template := raw_template_of('asm amd64 raw intel { "mov rax, qword ptr [rbx+8]\\n\\t" "add rax, rcx" ; }')
	instructions := inline_asm_raw_parse_instructions(&template, util.asm_register_names('amd64'))
	assert instructions.map(it.mnemonic) == ['mov', 'add']
	assert instructions[0].operands.map(it.kind) == [.register, .memory]
	assert instructions[1].operands.map(it.name) == ['rax', 'rcx']
}

fn test_raw_label_detection_rejects_operand_colons() {
	template := raw_template_of('asm amd64 raw { "%%fs:0x28\\n\\t" "a::\\n\\t" "x: nop" ; }')
	instructions := inline_asm_raw_parse_instructions(&template, util.asm_register_names('amd64'))
	assert instructions.map(first_label(it)) == ['', '', 'x']
	assert instructions.map(it.mnemonic) == ['%%fs:0x28', 'a::', 'nop']
}

fn test_raw_template_removes_c_line_splices() {
	for line_end in ['\n', '\r\n'] {
		block := '"mov' + '\\' + line_end + 'q %%rax, %%rbx"'
		template := inline_asm_raw_template(block, InlineAsmRange{ start: 0, end: block.len }, 'amd64')
		assert template.text == 'movq %%rax, %%rbx'
		start, end := template.span(3, 4)
		assert block[start..end] == 'q'
		// A splice can also join the source bytes within an escape sequence.
		escape := '"' + '\\\\' + line_end + 'n' + '\\x4' + '\\' + line_end + '1"'
		decoded := inline_asm_raw_template(escape, InlineAsmRange{ start: 0, end: escape.len }, 'amd64')
		assert decoded.text == '\nA'
		escape_start, escape_end := decoded.span(1, 2)
		assert escape[escape_start..escape_end] == '\\x4' + '\\' + line_end + '1'
	}
}

fn test_raw_instructions_fold_multiple_prefixes() {
	instructions := raw_instructions_of('asm amd64 raw { "lock rex.w addl %%eax, (%%rcx)" ; }',
		'amd64')
	assert instructions.len == 1
	assert instructions[0].mnemonic == 'lock rex.w addl'
	assert instructions[0].operands.map(it.text) == ['%%eax', '(%%rcx)']
}

fn test_raw_statement_can_define_several_labels_with_blanks_before_colons() {
	instructions := raw_instructions_of('asm amd64 raw { ".La%=: .Lb: nop\\n\\t" "spaced\\t: ret" ; }',
		'amd64')
	assert instructions.map(it.mnemonic) == ['nop', 'ret']
	assert instructions[0].labels.map(it.name) == ['.La%=', '.Lb']
	assert instructions[1].labels.map(it.name) == ['spaced']
	assert instructions[1].labels[0].end - instructions[1].labels[0].start == 'spaced'.len
}

fn test_raw_quoted_label_names_and_spans() {
	block := 'asm amd64 raw { "\\"quoted;: label\\" \\t: \\"1\\": nop\\n" ; }'
	template := raw_template_of(block)
	instructions := inline_asm_raw_parse_instructions(&template, [])
	assert instructions.len == 1
	assert instructions[0].mnemonic == 'nop'
	assert instructions[0].labels.map(it.name) == ['quoted;: label', '1']
	for label in instructions[0].labels {
		assert label.is_quoted
		start, end := template.span(label.start, label.end)
		assert block[start..end] == label.name
	}
}

fn test_raw_template_masks_assembler_comments_per_arch() {
	x86 := raw_template_of('asm amd64 raw { "/* a\\n" " note: b */ nop # c: d \\"e\\"\\n" ".ascii \\"#x\\"\\n" "jmp .L1 // not: comment" ; }')
	// Masking keeps the length and the newlines, so offsets still map back to the source.
	assert x86.text.len == x86.starts.len
	assert x86.text.count('\n') == 3
	assert !x86.text.contains('note')
	assert x86.text.contains('"#x"')
	assert x86.text.contains('// not: comment')
	x86_instructions := inline_asm_raw_parse_instructions(&x86, [])
	assert x86_instructions.map(it.mnemonic) == ['nop', '.ascii', 'jmp']
	assert x86_instructions.map(it.labels.len) == [0, 0, 0]
	assert x86_instructions[1].operands.map(it.text) == ['"#x"']
	// On x86 `//` is not a comment, and operands split only on commas.
	assert x86_instructions[2].operands.map(it.text) == ['.L1 // not: comment']
	// A `//` comment hides a `'` that would otherwise start a character constant.
	arm := raw_template_of('asm arm64 raw { "add x0, x0, #16 // don\'t: stop\\n" "1: ret" ; }')
	arm_instructions := inline_asm_raw_parse_instructions(&arm, util.asm_register_names('arm64'))
	assert arm_instructions.map(it.mnemonic) == ['add', 'ret']
	assert arm_instructions[0].operands.map(it.kind) == [.register, .register, .immediate]
	assert arm_instructions[1].labels.map(it.name) == ['1']
}

fn test_att_mnemonic_widths_read_the_size_suffix() {
	src, dst, is_shift := inline_asm_att_mnemonic_widths('movl')
	assert src == 32 && dst == 32 && !is_shift
	src2, dst2, shift2 := inline_asm_att_mnemonic_widths('shlq')
	assert src2 == 64 && dst2 == 64 && shift2
	zbl_src, zbl_dst, _ := inline_asm_att_mnemonic_widths('movzbl')
	assert zbl_src == 8 && zbl_dst == 32
	slq_src, slq_dst, _ := inline_asm_att_mnemonic_widths('movslq')
	assert slq_src == 32 && slq_dst == 64
	sbw_src, sbw_dst, _ := inline_asm_att_mnemonic_widths('movsbw')
	assert sbw_src == 8 && sbw_dst == 16
}

fn test_att_mnemonic_widths_ignore_unsuffixed_and_unrelated_names() {
	for name in ['mov', 'movq_extra', 'movsb', 'movsl', 'movsd', 'movzx', 'movbe', 'sarx', 'shrd',
		'andn', 'movzlq', 'movswb', 'lea_x'] {
		src, dst, _ := inline_asm_att_mnemonic_widths(name)
		assert src == 0 && dst == 0, 'expected no suffix width for `${name}`'
	}
}

fn test_att_gpr_constraints_exclude_memory_and_immediate() {
	assert inline_asm_raw_gpr_constraint('+r')
	assert inline_asm_raw_gpr_constraint('=&q')
	assert inline_asm_raw_gpr_constraint('a')
	assert !inline_asm_raw_gpr_constraint('rm')
	assert !inline_asm_raw_gpr_constraint('i')
	assert !inline_asm_raw_gpr_constraint('g')
	assert !inline_asm_raw_gpr_constraint('x')
	assert !inline_asm_raw_gpr_constraint('+')
	// clang prints a narrow `A` operand as `%eax` on i386 and `%rax` on amd64: not checked.
	assert !inline_asm_raw_gpr_constraint('=A')
	assert !inline_asm_raw_gpr_constraint('+A')
}

fn att_width_errors(name string, source string) []string {
	tc := raw_label_checker('att_width_${name}', source)
	return tc.errors.map(it.msg)
}

fn test_att_or_family_is_checked() {
	src, dst, is_shift := inline_asm_att_mnemonic_widths('orq')
	assert src == 64 && dst == 64 && !is_shift
	b_src, _, _ := inline_asm_att_mnemonic_widths('orb')
	assert b_src == 8
}

fn test_att_register_widths_follow_the_instruction_set() {
	for segment in ['cs', 'ss', 'ds', 'es', 'fs', 'gs'] {
		assert inline_asm_att_register_width(segment, 64) == 0
		assert inline_asm_att_register_width(segment, 32) == 0
	}
	assert inline_asm_att_register_width('cr0', 64) == 64
	assert inline_asm_att_register_width('cr0', 32) == 32
	assert inline_asm_att_register_width('dr7', 32) == 32
	assert inline_asm_att_register_width('rax', 64) == 64
	assert inline_asm_att_register_width('ecx', 64) == 32
	assert inline_asm_att_arch_bits('x86_64') == 64
	assert inline_asm_att_arch_bits('i686') == 32
	assert inline_asm_att_arch_bits('x86') == 32
}

fn test_att_int_operands_have_the_native_width() {
	ok := att_width_errors('int_ok', 'module main
enum Wide as int {
 a
}
fn main() {
 mut value := 40
 mut tag := Wide.a
 asm amd64 raw {
  "addq %[value], %[value]\\n\\t"
  "incq %[tag]"
  ; [value] "+r" (value)
    [tag] "+r" (tag)
  ; ; cc
 }
 mut narrow := 40
 asm i386 raw {
  "incl %[narrow]"
  ; [narrow] "+r" (narrow)
  ; ; cc
 }
}
')
	assert ok.len == 0, ok.str()
	bad := att_width_errors('int_bad', 'module main
fn main() {
 mut value := 40
 asm amd64 raw {
  "addl $1, %[value]"
  ; [value] "+r" (value)
  ; ; cc
 }
}
')
	assert bad.len == 1, bad.str()
	assert bad[0].starts_with('`addl` operates on 32-bit operands here, but `%[value]` is 64-bit; use a size suffix that matches the operand, or an operand modifier'), bad[0]
}

fn test_att_segment_control_and_debug_registers_are_accepted() {
	errors := att_width_errors('special_registers', 'module main
fn main() {
 asm amd64 raw {
  "pushq %%fs\\n\\t"
  "popq %%fs\\n\\t"
  "movl %%ds, %%eax\\n\\t"
  "movq %%fs, %%rax\\n\\t"
  "movl %%eax, %%ds\\n\\t"
  "movq %%rax, %%fs\\n\\t"
  "movq %%cr0, %%rax"
  ; ; ; rax memory
 }
 asm i386 raw {
  "pushl %%ds\\n\\t"
  "popl %%fs\\n\\t"
  "movl %%cr0, %%eax\\n\\t"
  "movl %%eax, %%cr0\\n\\t"
  "movl %%dr7, %%eax\\n\\t"
  "movl %%ds, %%eax"
  ; ; ; eax memory
 }
}
')
	assert errors.len == 0, errors.str()
	bad := att_width_errors('amd64_control_register', 'module main
fn main() {
 asm amd64 raw {
  "movl %%cr0, %%eax"
  ; ; ; rax
 }
}
')
	assert bad.len == 1, bad.str()
	// A literal register is renamed rather than given an operand modifier.
	assert bad[0] == '`movl` operates on 32-bit operands here, but `%%cr0` is 64-bit; use a size suffix that matches the operand, or a 32-bit register'
}

fn test_att_i386_named_operands_print_at_most_32_bits() {
	errors := att_width_errors('i386_pair', 'module main
fn main() {
 x := u64(5)
 mut out := u32(0)
 asm i386 raw {
  "movl %[x], %[out]"
  ; [out] "=r" (out)
  ; [x] "r" (x)
 }
}
')
	assert errors.len == 0, errors.str()
	bad := att_width_errors('i386_pair_bad', 'module main
fn main() {
 x := u64(5)
 asm i386 raw {
  "movw %[x], %%ax"
  ; ; [x] "r" (x)
  ; eax
 }
}
')
	assert bad.len == 1, bad.str()
	assert bad[0].starts_with('`movw` operates on 16-bit operands here, but `%[x]` is 32-bit'), bad[0]
}

fn test_att_modifiers_require_a_register_only_constraint() {
	for constraint in ['m', 'rm', 'g'] {
		errors := att_width_errors('modified_memory_${constraint}', 'module main
fn main() {
 x := u64(42)
 mut out := u64(0)
 asm amd64 raw {
  "movq %k[x], %[out]"
  ; [out] "=r" (out)
  ; [x] "${constraint}" (x)
 }
}
')
		assert errors.len == 0, errors.str()
	}
	errors := att_width_errors('modified_register', 'module main
fn main() {
 x := u64(42)
 mut out := u64(0)
 asm amd64 raw {
  "movq %k[x], %[out]"
  ; [out] "=r" (out)
  ; [x] "r" (x)
 }
}
')
	assert errors.len == 1, errors.str()
	assert errors[0].starts_with('`movq` operates on 64-bit operands here, but `%k[x]` is 32-bit'), errors[0]
}

fn test_x86_family_maps_register_views_and_keeps_short_names() {
	assert inline_asm_raw_x86_family('AL') == 'rax'
	assert inline_asm_raw_x86_family('r8d') == 'r8'
	assert inline_asm_raw_x86_family('R9W') == 'r9'
	assert inline_asm_raw_x86_family('r15') == 'r15'
	assert inline_asm_raw_x86_family('r') == 'r'
	assert inline_asm_raw_x86_family('') == ''
}

// raw_all_declared holds every register a save may use, as a block that declares them all would.
const raw_all_declared = InlineAsmRawSavePlaces{
	changeable: {
		'rax': true
		'rbx': true
		'rcx': true
		'rdx': true
		'rsi': true
		'rdi': true
		'rbp': true
		'r8':  true
		'r9':  true
		'r10': true
		'r11': true
		'r12': true
	}
}

// raw_mov_save_restored models a template that saves `rbx` (`ebx` on i386) with `save`, writes
// it, and copies it back with `back`, and reports whether the copy back counts as a restore.
fn raw_mov_save_restored(arch string, save string, back string) bool {
	bits := if arch == 'i386' { 32 } else { 64 }
	write := if arch == 'i386' { 'movl $5, %%ebx' } else { 'movq $5, %%rbx' }
	source := 'asm ${arch} raw {\n\t"${save}\\n\\t"\n\t"${write}\\n\\t"\n\t"${back}"\n}'
	return inline_asm_raw_mov_restored(raw_instructions_of(source, arch), bits, &raw_all_declared)['rbx']
}

// raw_rbx_is_saved reports whether the check of a whole amd64 block counts its write to `rbx` as saved.
// `lines` are the lines of the template and `sections` the source after it.
fn raw_rbx_is_saved(name string, lines []string, sections string) bool {
	return !raw_block_errors(name, lines, sections).any(it.contains('writes `rbx`'))
}

// raw_block_errors returns the errors of a whole amd64 block; see raw_rbx_is_saved.
fn raw_block_errors(name string, lines []string, sections string) []string {
	template := lines.map('"' + it + '\\n\\t"').join('\n')
	return att_width_errors(name, 'module main

fn main() {
	mut o := u64(0)
	s := u64(1)
	t := u64(2)
	asm amd64 raw {
		${template}
		${sections}
	}
	println(o + s + t)
}
')
}

// The rows below are generated from the spellings of each part of a save and its copy back:
// the mnemonic, the saved register and the place, each in several letter cases. The lists are
// written out here rather than taken from the checker's own tables, so a register or spelling
// dropped from those tables fails a row.
const raw_mov_amd64_mnemonics = ['movq', 'MOVQ', 'Movq', 'mov', 'wait movq']
const raw_mov_i386_mnemonics = ['movl', 'MOVL', 'mov', 'wait movl']

fn test_mov_restored_counts_every_general_purpose_register_place() {
	for family in ['rax', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13',
		'r14', 'r15'] {
		for place in [family, family.to_upper()] {
			for back_place in [family, family.to_upper()] {
				for saved in ['rbx', 'RBX'] {
					for back_saved in ['rbx', 'RBX'] {
						for m in raw_mov_amd64_mnemonics {
							for back_m in raw_mov_amd64_mnemonics {
								save := '${m} %%${saved}, %%${place}'
								back := '${back_m} %%${back_place}, %%${back_saved}'
								assert raw_mov_save_restored('amd64', save, back), '${save} / ${back}'
							}
						}
					}
				}
			}
		}
	}
	for place in ['eax', 'ecx', 'edx', 'esi', 'edi', 'ebp', 'ESI'] {
		for m in raw_mov_i386_mnemonics {
			for back_m in raw_mov_i386_mnemonics {
				save := '${m} %%ebx, %%${place}'
				back := '${back_m} %%${place.to_lower()}, %%EBX'
				assert raw_mov_save_restored('i386', save, back), '${save} / ${back}'
			}
		}
	}
}

fn test_mov_restored_ignores_registers_that_are_no_place() {
	// A narrower register, the stack pointer, an `mm`, `xmm`, control, debug or segment
	// register, the instruction pointer, and the saved register itself in any letter case.
	for place in ['r11d', 'R11D', 'r11w', 'r11b', 'rsp', 'RSP', 'esp', 'mm0', 'MM7', 'xmm0', 'cr0',
		'cr8', 'dr0', 'dr7', 'rip', 'fs', 'r16', 'r31', 'rbx', 'RBX', 'Rbx', 'ebx', 'bl'] {
		for saved in ['rbx', 'RBX'] {
			save := 'movq %%${saved}, %%${place}'
			back := 'movq %%${place}, %%${saved}'
			assert !raw_mov_save_restored('amd64', save, back), '${save} / ${back}'
		}
	}
	for place in ['esp', 'ebx', 'EBX', 'si', 'mm0', 'xmm0', 'cr0', 'dr0', 'r11'] {
		save := 'movl %%ebx, %%${place}'
		back := 'movl %%${place}, %%ebx'
		assert !raw_mov_save_restored('i386', save, back), '${save} / ${back}'
	}
	// A bare name such as `esi` is a symbol in GNU x86 syntax, on i386 as on amd64.
	assert !raw_mov_save_restored('i386', 'movl %%ebx, esi', 'movl esi, %%ebx')
	assert !raw_mov_save_restored('amd64', 'movq %%rbx, r11', 'movq r11, %%rbx')
}

// raw_suffix_shapes pairs a mnemonic with operands for zero, one and two operand statements, so
// the suffix gate is tried on every statement shape.
const raw_suffix_shapes = {
	'nop':   ''
	'incq':  ' %%rax'
	'pushq': ' %%rax'
	'movq':  ' %%rax, %%rbx'
}

fn raw_first_has_encoding_suffix(statement string) bool {
	source := 'asm amd64 raw {\n\t"${statement}"\n}'
	return inline_asm_raw_has_encoding_suffix(raw_instructions_of(source, 'amd64')[0])
}

fn test_encoding_suffix_gate_over_generated_shapes() {
	for base, operands in raw_suffix_shapes {
		for prefix in ['', 'lock ', 'wait '] {
			for suffix in ['s', 'd8', 'd32'] {
				for spelled in [suffix, suffix.to_upper(), suffix.capitalize()] {
					for mnemonic in ['${base}.${spelled}', '${base.to_upper()}.${spelled}'] {
						statement := '${prefix}${mnemonic}${operands}'
						assert raw_first_has_encoding_suffix(statement), statement
					}
				}
			}
			// No suffix, or a dotted name such as a macro that is not one of the three suffixes.
			for mnemonic in [base, base.to_upper(), '${base}.x', '${base}.d16', '${base}.ss',
				'${base}.s8', 'my.${base}', 'set.rbx', 'a.b'] {
				statement := '${prefix}${mnemonic}${operands}'
				assert !raw_first_has_encoding_suffix(statement), statement
			}
		}
	}
	for statement in ['.p2align 4', '.balign 8', 'movq 1.5, %%rbx', 'movq $0x1.s, %%rbx'] {
		assert !raw_first_has_encoding_suffix(statement), statement
	}
}

fn test_mov_restored_needs_a_whole_width_copy_both_ways() {
	for place in ['%%r11', '%[slot]', '%0'] {
		for narrow in ['ebx', 'EBX', 'bx', 'bl'] {
			// A narrower copy, either way, keeps only part of the register.
			for save, back in {
				'mov %%${narrow}, ${place}': 'movq ${place}, %%rbx'
				'movq %%rbx, ${place}':      'mov ${place}, %%${narrow}'
			} {
				assert !raw_mov_save_restored('amd64', save, back), '${save} / ${back}'
			}
		}
	}
}

fn test_mov_restored_matches_operand_places_as_written() {
	places := ['%[slot]', '%[Slot]', '%[SLOT]', '%[rbx]', '%[RBX]', '%0', '%1', '%q[slot]', '%k[slot]',
		'%Q[slot]', '%q0']
	for place in places {
		for back_place in places {
			save := 'movq %%rbx, ${place}'
			back := 'movq ${back_place}, %%rbx'
			assert raw_mov_save_restored('amd64', save, back) == (place == back_place), '${save} / ${back}'
		}
	}
	assert raw_mov_save_restored('i386', 'movl %%ebx, %[slot]', 'movl %[slot], %%ebx')
	assert !raw_mov_save_restored('i386', 'movl %%ebx, %[slot]', 'movl %[Slot], %%ebx')
}

fn test_mov_restored_ignores_other_places_and_mnemonics() {
	// Another register for the copy back, and memory written in the template.
	assert !raw_mov_save_restored('amd64', 'movq %%rbx, %%r11', 'movq %%r10, %%rbx')
	for place in ['(%[p])', '8(%[p])', '(%%rdi)', '8(%%RDI)', 'slot(%%rip)', 'slot', '0x1000',
		'%%fs:8'] {
		assert !raw_mov_save_restored('amd64', 'movq %%rbx, ${place}', 'movq ${place}, %%rbx'), place
	}
	// An operand named like the register is not the register, whether it is read for the save
	// or written by the copy back.
	for operand in ['%[rbx]', '%[RBX]', '%[ebx]'] {
		assert !raw_mov_save_restored('amd64', 'movq ${operand}, %%r11', 'movq %%r11, %%rbx'), operand
		assert !raw_mov_save_restored('amd64', 'movq %%rbx, %[slot]', 'movq %[slot], ${operand}'), operand
	}
	// A save or a copy back made with anything but `mov` or `xchg`.
	for other in ['cmovzq', 'cmovneq', 'CMOVZQ', 'addq', 'movabsq', 'movsxq', 'xaddq', 'cmpxchgq'] {
		assert !raw_mov_save_restored('amd64', 'movq %%rbx, %%r11', '${other} %%r11, %%rbx'), other
		assert !raw_mov_save_restored('amd64', '${other} %%rbx, %%r11', 'movq %%r11, %%rbx'), other
	}
}

// raw_pair returns the two operand orders of an `xchg` between `a` and `b`, which mean the same.
fn raw_pair(m string, a string, b string) []string {
	return ['${m} ${a}, ${b}', '${m} ${b}, ${a}']
}

fn test_xchg_counts_as_a_copy_both_ways() {
	for place in ['%%rax', '%%r11', '%%R11', '%%rbp', '%[slot]', '%0'] {
		for m in ['xchgq', 'XCHGQ', 'xchg', 'wait xchgq'] {
			for saved in ['%%rbx', '%%RBX'] {
				for save in raw_pair(m, saved, place) {
					// Saved and restored with `xchg`, or one of the two with `mov`.
					for back in raw_pair(m, '%%rbx', place) {
						assert raw_mov_save_restored('amd64', save, back), '${save} / ${back}'
						assert raw_mov_save_restored('amd64', 'movq ${saved}, ${place}', back), back
					}
					assert raw_mov_save_restored('amd64', save, 'movq ${place}, %%rbx'), save
				}
			}
		}
	}
	for place in ['%%esi', '%[slot]'] {
		for m in ['xchgl', 'XCHGL', 'xchg'] {
			for save in raw_pair(m, '%%ebx', place) {
				for back in raw_pair(m, '%%ebx', place) {
					assert raw_mov_save_restored('i386', save, back), '${save} / ${back}'
				}
			}
		}
	}
}

// Two `xchg` give back one of the two registers, so a register counts only when the other one is
// declared.
fn test_xchg_needs_the_other_register_declared() {
	for orders in [['%%rbx, %%r12', '%%r12, %%rbx'], ['%%r12, %%rbx', '%%rbx, %%r12'],
		['%%rbx, %%r12', '%%rbx, %%r12']] {
		source := 'asm amd64 raw {\n\t"xchgq ${orders[0]}\\n\\txchgq ${orders[1]}"\n}'
		instructions := raw_instructions_of(source, 'amd64')
		assert inline_asm_raw_mov_restored(instructions, 64, &InlineAsmRawSavePlaces{}).len == 0, source
		assert inline_asm_raw_mov_restored(instructions, 64, &InlineAsmRawSavePlaces{
			changeable: {
				'r12': true
			}
		}) == {
			'rbx': true
		}, source
		assert inline_asm_raw_mov_restored(instructions, 64, &InlineAsmRawSavePlaces{
			changeable: {
				'rbx': true
			}
		}) == {
			'r12': true
		}, source
	}
	// A `mov` on the other half of the pair changes nothing about the `xchg` half.
	for lines in ['xchgq %%rbx, %%r12\\n\\tmovq %%r12, %%rbx', 'movq %%rbx, %%r12\\n\\txchgq %%rbx, %%r12'] {
		instructions := raw_instructions_of('asm amd64 raw {\n\t"${lines}"\n}', 'amd64')
		assert !inline_asm_raw_mov_restored(instructions, 64, &InlineAsmRawSavePlaces{})['rbx'], lines
	}
}

// A template keeps its inputs, so an input is no place for a save, unless it shares the register of
// an output or a clobber. Each row is the sections after the template, and whether a save of `rbx`
// in the place counts.
fn test_input_is_no_place_for_a_save() {
	for index, row in [
		['%[s]', ';\n; [s] "c" (s)', 'no'],
		['%q[s]', ';\n; [s] "c" (s)', 'no'],
		['%[s]', ';\n; "c" (s)', 'no'],
		['%[s]', ';\n; "c" (t) as s', 'no'],
		['%0', ';\n; [s] "c" (s)', 'no'],
		['%1', '; [o] "=d" (o)\n; [s] "c" (s)', 'no'],
		['%[s]', ';\n; [s] "x" (s)', 'no'],
		['%[s]', ';\n; [s] "c" (s)\n  [t] "d" (t)', 'no'],
		['%[t]', ';\n; [s] "c" (s)\n  [t] "d" (t)', 'no'],
		['%1', ';\n; [s] "c" (s)\n  [t] "d" (t)', 'no'],
		['%[s]', '; [o] "=d" (o)\n; (s)', 'no'],
		['%[o]', '; (o)\n; [s] "c" (s)', 'no'],
		['%[s]', ';\n; [s] "cS" (s)\n; rcx', 'no'],
		['%[s]', ';\n; [s] "cx" (s)\n; rcx', 'no'],
		['%[s]', ';\n; [s] "{rcx}" (s)', 'no'],
		['%[s]', '; [o] "=@ccz" (o)\n; [s] "c" (s)', 'no'],
		['%%rcx', ';\n; [s] "c" (s)', 'no'],
		['%%RCX', ';\n; [s] "{ecx}" (s)', 'no'],
		['%%rax', '; [o] "=d" (o)\n; [s] "A" (s)', 'no'],
		['%%rdx', '; [o] "=d" (o)\n; [s] "A" (s)', 'yes'],
		['%[o]', '; [o] "=d" (o)\n; [s] "c" (s)', 'yes'],
		['%0', '; [o] "=d" (o)\n; [s] "c" (s)', 'yes'],
		['%[o]', '; [o] "+x" (o)\n; [s] "c" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "0" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "%0" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "c" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "{rcx}" (s)', 'yes'],
		['%%rcx', '; [o] "=c" (o)\n; [s] "c" (s)', 'yes'],
		['%%rcx', ';\n; [s] "d" (s)\n; rcx', 'yes'],
		['%%rax', '; [o] "=A" (o)\n; [s] "d" (s)', 'yes'],
		// Alternatives, modifiers and names.
		['%[s]', '; [o] "=c,c" (o)\n; [s] "c,c" (s)', 'yes'],
		['%[s]', '; [o] "=c,c" (o)\n; [s] "0,0" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "?0" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "?c" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "0c" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "c#r" (s)', 'yes'],
		['%[s]', '; [o] "=c" (o)\n; [s] "[o]" (s)', 'yes'],
		['%[s]', '; [o] "=c,c" (o)\n; [s] "[o],[o]" (s)', 'yes'],
		['%[s]', '; [o] "=A" (o)\n; [s] "A" (s)', 'yes'],
		['%[s]', '; [o] "=A" (o)\n; [s] "d" (s)', 'yes'],
		['%[s]', '; [o] "=a" (o)\n; [s] "A" (s)', 'no'],
		['%[s]', ';\n; [s] "c,c" (s)', 'no'],
		['%[s]', '; [o] "=d" (o)\n; [s] "A" (s)', 'no'],
		['%[s]', '; [o] "=a" (o)\n; [s] "Q" (s)', 'no'],
		['%[s]', '; [o] "=c" (o)\n; [s] "0S" (s)', 'no'],
		['%[s]', '; [o] "=c" (o)\n; [s] "0x" (s)', 'no'],
		['%[s]', '; [o] "=c" (o)\n; [s] "ci" (s)', 'no'],
		['%[s]', '; [o] "=c" (o)\n; [s] "0r" (s)', 'no'],
		// Each other fixed register, with and without an output that shares it.
		['%[s]', '; [o] "=a" (o)\n; [s] "a" (s)', 'yes'],
		['%[s]', '; [o] "=d" (o)\n; [s] "d" (s)', 'yes'],
		['%[s]', '; [o] "=S" (o)\n; [s] "S" (s)', 'yes'],
		['%[s]', '; [o] "=D" (o)\n; [s] "D" (s)', 'yes'],
		['%[s]', ';\n; [s] "a" (s)', 'no'],
		['%[s]', ';\n; [s] "d" (s)', 'no'],
		['%[s]', ';\n; [s] "S" (s)', 'no'],
		['%[s]', ';\n; [s] "D" (s)', 'no'],
	] {
		for m in ['movq', 'xchgq'] {
			back := if m == 'movq' { '${row[0]}, %%rbx' } else { '%%rbx, ${row[0]}' }
			lines := ['${m} %%rbx, ${row[0]}', 'movq $5, %%rbx', '${m} ${back}']
			assert raw_rbx_is_saved('place_${index}_${m}', lines, row[1]) == (row[2] == 'yes'), '${lines} ${row[1]}'
		}
	}
	// A register that nothing declares is a place for `mov`, which is reported on its own, but not
	// for `xchg`.
	for m in ['movq', 'xchgq'] {
		lines := ['${m} %%rbx, %%rcx', 'movq $5, %%rbx', '${m} %%rcx, %%rbx']
		assert raw_rbx_is_saved('undeclared_${m}', lines, ';\n;') == (m == 'movq'), m
	}
}

// As above for `rbx` as the place, which needs another register to save.
fn test_rbx_input_is_no_place_for_a_save() {
	for index, sections in ['; [o] "=b" (o)\n; [s] "b" (s)', '; [o] "=d" (o)\n; [s] "b" (s)'] {
		lines := ['movq %%rcx, %[s]', 'movq $5, %%rcx', 'movq %[s], %%rcx']
		errors := raw_block_errors('rbx_place_${index}', lines, sections)
		assert errors.any(it.contains('writes `rcx`')) == (index == 1), sections
	}
}

// The name in a matching constraint is not read as letters: `[Acc]` is no `A` operand, and `[b]`
// binds no `rbx`.
fn test_matching_name_is_not_read_as_letters() {
	lines := ['movq %%rax, %%rcx', 'movq $5, %%rax', 'movq %%rcx, %%rax']
	sections := '; [Acc] "=S" (o)\n; [s] "[Acc]" (s)\n; rcx'
	assert raw_block_errors('name_acc', lines, sections) == []
	assert !raw_rbx_is_saved('name_b', ['movq $5, %%rbx'], '; [b] "=d" (o)\n; [s] "[b]" (s)')
	errors := raw_block_errors('name_a', ['cqto'], '; [d] "=c" (o)\n; [s] "[d]" (s)')
	assert errors.any(it.contains('writes `rdx`')), errors.str()
}

// An operand keeps its own width where a register has the same name, and the size suffix gives the
// width where its type is not known. Either way the push is followed and the write is reported.
fn test_pushed_operand_width_is_its_own() {
	same_name := ['push %[di]', 'movq $5, %%rbx', 'pop %[di]', 'movw %%di, %%di']
	assert !raw_rbx_is_saved('push_name', same_name, '; [di] "=d" (o)\n;\n; rdi')
	literal := ['pushq %[c]', 'movq $5, %%rbx', 'popq %%rcx']
	assert !raw_rbx_is_saved('push_literal', literal, ';\n; [c] "c" (5)')
}

// A `lock` waits for the next instruction only on a line of prefixes.
fn test_lock_of_an_instruction_does_not_reach_the_next() {
	errors := raw_block_errors('lock_next', ['lock movsb', 'rep movsb'], ';\n;\n; rsi rdi memory')
	assert errors.any(it.contains('writes `rcx`')), errors.str()
}

// A save of a register counts beside an operand whose constraint names a class without it.
fn test_save_beside_a_register_class() {
	registers := ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp', 'r8', 'r9', 'r10', 'r11', 'r12']
	for class, members in {
		'Q': ['rax', 'rbx', 'rcx', 'rdx']
		'R': ['rax', 'rbx', 'rcx', 'rdx', 'rsi', 'rdi', 'rbp']
		'U': ['rax', 'rcx', 'rdx', 'rsi', 'rdi', 'r8', 'r9', 'r10', 'r11']
		'A': ['rax', 'rdx']
	} {
		for register in registers {
			lines := ['movq %%${register}, %%r13', 'movq $5, %%${register}',
				'movq %%r13, %%${register}']
			// `rax` and `rdx` beside an `A` operand are declared by it.
			if class == 'A' && register in members {
				continue
			}
			errors := raw_block_errors('class_${class}_${register}', lines, '; [o] "=${class}" (o)\n;\n; r13')
			assert (errors.len > 0) == (register in members), '${class} ${register} ${errors}'
		}
	}
	// A `Q` operand declares none of its registers.
	errors := raw_block_errors('class_write', ['movq $5, %%rdx'], '; [o] "=Q" (o)')
	assert errors.len == 1 && errors[0].contains('add `rdx` to the clobbers'), errors.str()
	// A second operand that the compiler places anywhere ends that.
	lines := ['movq %%rbp, %%r13', 'movq $5, %%rbp', 'movq %%r13, %%rbp']
	assert raw_block_errors('class_free', lines, '; [o] "=Q" (o)\n; [s] "r" (s)\n; r13').len > 0
	assert raw_block_errors('class_two', lines, '; [o] "=Q" (o)\n; [s] "R" (s)\n; r13').len > 0
}

// A save may go through several places, `push` and `pop` included.
fn test_save_through_several_places() {
	for index, row in [
		['yes', 'movq %%rbx, %%r11', 'movq $5, %%rbx', 'pushq %%r11', 'popq %%rbx'],
		['yes', 'pushq %%rbx', 'popq %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'movq %%r11, %%r10', 'movq $5, %%rbx', 'movq %%r10, %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'popq %[o]', 'movq $5, %%rbx', 'movq %[o], %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%r10', 'movq $5, %%rbx', 'popq %%r10',
			'popq %%rbx'],
		['yes', 'movq %%rbx, %[o]', 'movq %[o], %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx'],
		['yes', 'xchgq %%rbx, %%r11', 'movq %%r11, %%r10', 'movq $5, %%rbx', 'movq %%r10, %%rbx'],
		['no', 'movq %%rbx, %%r11', 'movq $5, %%rbx', 'pushq %%r10', 'popq %%rbx'],
		['no', 'pushq %%rbx', 'popq %%r11', 'movq $5, %%rbx', 'movq %%r10, %%rbx'],
		['no', 'pushq %%r10', 'popq %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx'],
		['no', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%r10', 'movq $5, %%rbx', 'popq %%rbx',
			'popq %%r11'],
		// A push and a pop of the register itself cover what is between them only.
		['no', 'pushq %%rbx', 'popq %%rbx', 'movq $5, %%rbx'],
		// An undeclared place that only `xchg` uses is none.
		['no', 'xchgq %%rbx, %%r9', 'pushq %%r9', 'movq $5, %%rbx', 'popq %%rbx'],
	] {
		assert raw_rbx_is_saved('several_${index}', row[1..], '; [o] "=d" (o)\n;\n; r10 r11') == (row[0] == 'yes'), row.str()
	}
	for index, row in [
		// Frames, `leave` and the saved flags move the stack by their own slots.
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%r10',
			'leave', 'movq $5, %%rbx', 'popq %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%r10',
			'movq %%rbp, %%rsp', 'popq %%rbp', 'movq $5, %%rbx', 'popq %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%rbp',
			'movq %%rsp, %%rbp', 'leave', 'leave', 'movq $5, %%rbx', 'popq %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%rbp',
			'movq %%rsp, %%rbp', 'popq %%rbp', 'pushq %%r10', 'leave', 'movq $5, %%rbx', 'popq %%rbx'],
		['yes', 'movq %%rbx, %%r11', 'pushq %%r11', 'pushfq', 'movq $5, %%rbx', 'popfq', 'popq %%rbx'],
		['no', 'movq %%rbx, %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%r11', 'leave',
			'movq $5, %%rbx', 'popq %%rbx'],
		['no', 'movq %%rbx, %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'pushq %%r11', 'pushq %%r11',
			'movq %%rbp, %%rsp', 'popq %%rbp', 'movq $5, %%rbx', 'popq %%rbx'],
		['no', 'movq %%rbx, %%r11', 'pushq %%rbp', 'movq %%rsp, %%rbp', 'popq %%rbp', 'pushq %%r11',
			'pushq %%rbp', 'movq %%rsp, %%rbp', 'leave', 'popq %%r10', 'movq $5, %%rbx', 'popq %%rbx'],
	] {
		assert raw_rbx_is_saved('framed_${index}', row[1..], '; [o] "=d" (o)\n;\n; r10 r11') == (row[0] == 'yes'), row.str()
	}
	// Upper-case names, and an instruction with one operand between the push and the pop.
	for index, lines in [
		['pushq %%RBX', 'popq %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx'],
		['movq %%rbx, %%r11', 'pushq %%r11', 'notq %%r10', 'movq $5, %%rbx', 'popq %%rbx'],
	] {
		assert raw_rbx_is_saved('between_${index}', lines, ';\n;\n; r10 r11'), lines.str()
	}
	// A pop of the flags that nothing pushed, and a pop without an operand, are read past.
	assert raw_rbx_is_saved('lone_popf', ['popfq'], ';\n;')
	assert raw_rbx_is_saved('bare_pop', ['pushq %%rcx', 'popq'], ';\n;')
	assert inline_asm_raw_joined(['a'], ['a', 'b', 'c'], ['c', 'd', 'e']) == ['a', 'b', 'c', 'd',
		'e']
	// An operand named as a register is not that register.
	for index, lines in [
		['pushq %[rbx]', 'popq %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx'],
		['movq %%rbx, %%r11', 'pushq %%r11', 'popq %[rbx]', 'movq $5, %%rbx'],
	] {
		assert !raw_rbx_is_saved('named_${index}', lines, '; [rbx] "=d" (o)\n;\n; r11'), lines.str()
	}
	// An input is no place here either.
	input := ['pushq %%rbx', 'popq %[s]', 'movq $5, %%rbx', 'pushq %[s]', 'popq %%rbx']
	assert !raw_rbx_is_saved('pop_input', input, ';\n; [s] "c" (s)')
	// LLVM's i386 `pushfd` and `popfd` use a slot too.
	for index, pair in [['pushfd', 'popfl'], ['pushfl', 'popfd']] {
		errors := att_width_errors('pushfd_${index}', 'module main

fn main() {
	mut o := u32(0)
	asm i386 raw {
		"movl %%ebx, %%esi\\n\\t"
		"pushl %%esi\\n\\t"
		"${pair[0]}\\n\\t"
		"movl $5, %%ebx\\n\\t"
		"${pair[1]}\\n\\t"
		"popl %%ebx"
		; [o] "=d" (o)
		;
		; esi
	}
	println(o)
}
')
		assert errors == []
	}
}

// An instruction that cannot change its destination writes nothing, though it may set the flags.
fn test_instruction_that_changes_nothing() {
	for index, row in [
		['ok', 'amd64', 'leaq 0(%%rsi), %%rsi'],
		['ok', 'amd64', 'leaq (%%rsi), %%rsi'],
		['ok', 'amd64', 'leaq 0x0(%%rsi,%%riz,1), %%rsi'],
		['ok', 'i386', 'leal 0(%%esi,%%eiz,1), %%esi'],
		['ok', 'i386', 'leal 0(%%esi), %%esi'],
		['ok', 'amd64', 'movq %%rbx, %%rbx'],
		['ok', 'amd64', 'movq %%RBX, %%rbx'],
		['ok', 'amd64', 'movb %%bl, %%bl'],
		['ok', 'amd64', 'movw %%bx, %%bx'],
		['ok', 'i386', 'movl %%ebx, %%ebx'],
		['ok', 'amd64', 'orq %%rbx, %%rbx'],
		['ok', 'amd64', 'andq %%rbx, %%rbx'],
		['ok', 'amd64', 'addq $0, %%rbx'],
		['ok', 'amd64', 'subq $0x0, %%rbx'],
		['ok', 'amd64', 'orq $0, %%rbx'],
		['ok', 'amd64', 'xorq $0, %%rbx'],
		['ok', 'amd64', 'shlq $0, %%rbx'],
		['ok', 'amd64', 'shrq $0, %%rbx'],
		['ok', 'amd64', 'sarq $0, %%rbx'],
		['ok', 'amd64', 'andq $-1, %%rbx'],
		// Other spellings of the same values, as GNU as and clang read them (amendment 5).
		['ok', 'amd64', 'addq $00, %%rbx'],
		['ok', 'amd64', 'addq $0X0, %%rbx'],
		['ok', 'amd64', 'addq $0b0, %%rbx'],
		['ok', 'amd64', 'addq $0B0, %%rbx'],
		['ok', 'amd64', 'addq $-0, %%rbx'],
		['ok', 'amd64', 'addq $+0, %%rbx'],
		['ok', 'amd64', 'addq $(0), %%rbx'],
		['ok', 'amd64', 'addq $((0)), %%rbx'],
		['ok', 'amd64', 'addq $- -0, %%rbx'],
		['ok', 'amd64', 'addq $~-1, %%rbx'],
		['ok', 'amd64', 'addq $!1, %%rbx'],
		['ok', 'amd64', 'addq $0x00000000, %%rbx'],
		['ok', 'amd64', 'subq $-0x0, %%rbx'],
		['ok', 'amd64', 'orq $0b00, %%rbx'],
		['ok', 'amd64', 'xorq $(0), %%rbx'],
		['ok', 'amd64', 'addb $256, %%bl'],
		['ok', 'amd64', 'addw $0x10000, %%bx'],
		['ok', 'amd64', 'andq $~0, %%rbx'],
		['ok', 'amd64', 'andq $-0x1, %%rbx'],
		['ok', 'amd64', 'andq $0xffffffffffffffff, %%rbx'],
		['ok', 'amd64', 'andq $18446744073709551615, %%rbx'],
		['ok', 'amd64', 'andq $01777777777777777777777, %%rbx'],
		['ok', 'amd64', 'andq $(-1), %%rbx'],
		['ok', 'amd64', 'andq $- 1, %%rbx'],
		['ok', 'amd64', 'andb $255, %%bl'],
		['ok', 'amd64', 'andb $0xff, %%bl'],
		['ok', 'amd64', 'andb $-1, %%bl'],
		['ok', 'amd64', 'andw $0xffff, %%bx'],
		['ok', 'amd64', 'andw $65535, %%bx'],
		['ok', 'i386', 'andl $0xffffffff, %%esi'],
		['ok', 'i386', 'addl $0x0, %%esi'],
		['ok', 'amd64', 'shlq $64, %%rbx'],
		['ok', 'amd64', 'shlq $0x40, %%rbx'],
		['ok', 'amd64', 'shrq $128, %%rbx'],
		['ok', 'amd64', 'sarq $0xc0, %%rbx'],
		['ok', 'amd64', 'shlb $32, %%bl'],
		['ok', 'amd64', 'shlw $32, %%bx'],
		['ok', 'i386', 'shll $32, %%esi'],
		['ok', 'amd64', 'leaq 0x0(,%%rbx,1), %%rbx'],
		['ok', 'amd64', 'leaq (,%%rbx), %%rbx'],
		['ok', 'amd64', 'leaq 00(%%rbx), %%rbx'],
		['ok', 'amd64', 'leaq 0b0(%%rbx), %%rbx'],
		['ok', 'amd64', 'leaq -0(%%rbx), %%rbx'],
		['ok', 'amd64', 'leaq (0)(%%rbx), %%rbx'],
		['ok', 'amd64', 'leaq 0(%%rbx,%%riz,2), %%rbx'],
		['ok', 'amd64', 'leaw (%%rbx), %%bx'],
		['ok', 'amd64', 'leaw (%%ebx), %%bx'],
		['ok', 'amd64', 'leaw (,%%rbx,1), %%bx'],
		['ok', 'amd64', 'leaq (,%%rbx,01), %%rbx'],
		['ok', 'amd64', 'leaq (,%%rbx,0x1), %%rbx'],
		['ok', 'amd64', 'leaq (,%%rbx,(1)), %%rbx'],
		['ok', 'amd64', 'leaq (,%%rbx, +1), %%rbx'],
		['ok', 'amd64', 'leaq (0)(,%%rbx,(1)), %%rbx'],
		['no', 'amd64', 'leaq (,%%rbx,2-1), %%rbx'],
		['no', 'amd64', 'leaq (%%rcx,%%rbx,1), %%rbx'],
		['no', 'amd64', 'addq 00, %%rbx'],
		['no', 'amd64', 'andq 0, %%rbx'],
		['no', 'amd64', 'leaq (,%%rbx,(2)), %%rbx'],
		['ok', 'i386', 'leal 0x0(,%%esi,1), %%esi'],
		['ok', 'i386', 'leal 00(%%esi), %%esi'],
		['ok', 'i386', 'leaw (%%si), %%si'],
		['no', 'amd64', 'shlb $8, %%bl'],
		['no', 'amd64', 'shlq $63, %%rbx'],
		['no', 'i386', 'shll $31, %%esi'],
		['no', 'amd64', 'addb $255, %%bl'],
		['no', 'amd64', 'andb $254, %%bl'],
		['no', 'amd64', 'andw $0xff, %%bx'],
		['no', 'amd64', 'andq $0x7fffffffffffffff, %%rbx'],
		['no', 'amd64', 'addq $1-1, %%rbx'],
		['no', 'amd64', 'addq $~0, %%rbx'],
		['no', 'amd64', 'orq $!0, %%rbx'],
		['no', 'amd64', 'andq $--1, %%rbx'],
		['no', 'amd64', 'addq $18446744073709551616, %%rbx'],
		['no', 'amd64', 'leaq (%%ebx), %%rbx'],
		['no', 'i386', 'leal (%%si), %%esi'],
		['no', 'amd64', 'leaq (,%%rbx,2), %%rbx'],
		['no', 'amd64', 'leaq 1(,%%rbx,1), %%rbx'],
		['no', 'amd64', 'leaq (,%%rcx,1), %%rbx'],
		['no', 'amd64', 'leaq 8(%%rbx,%%riz,2), %%rbx'],
		['no', 'amd64', 'leaq 0(%%rbx,%%rcx,1), %%rbx'],
		['no', 'amd64', 'leaq 0(%%rcx,%%riz,1), %%rbx'],
		['no', 'amd64', 'leaq 8(%%rsi), %%rsi'],
		['no', 'amd64', 'leaq 0(%%rdi), %%rsi'],
		['no', 'amd64', 'leaq 0(%%rsi,%%rdi,1), %%rsi'],
		['no', 'amd64', 'leaq foo(%%rsi), %%rsi'],
		['no', 'amd64', 'leaq %%fs:0(%%rsi), %%rsi'],
		['no', 'amd64', 'leal 0(%%rsi), %%esi'],
		['no', 'amd64', 'leal 0(%%esi), %%esi'],
		['no', 'amd64', 'movl %%ebx, %%ebx'],
		['no', 'amd64', 'movb %%bh, %%bl'],
		['no', 'amd64', 'movq %%rcx, %%rbx'],
		['no', 'amd64', 'orl %%ebx, %%ebx'],
		['no', 'amd64', 'xorq %%rbx, %%rbx'],
		['no', 'amd64', 'subq %%rbx, %%rbx'],
		['no', 'amd64', 'addq %%rbx, %%rbx'],
		['no', 'amd64', 'addq $1, %%rbx'],
		['no', 'amd64', 'addl $0, %%ebx'],
		['no', 'amd64', 'andq $0, %%rbx'],
		['no', 'amd64', 'movq $0, %%rbx'],
		['no', 'amd64', 'orq $-1, %%rbx'],
		['no', 'amd64', 'leaw 0(%%si), %%si'],
		['no', 'amd64', 'orq $zero, %%rbx'],
		['no', 'amd64', 'movsbq %%bl, %%rbx'],
		['no', 'amd64', 'notq %%rbx'],
	] {
		errors := att_width_errors('identity_${index}', 'module main

fn main() {
	mut o := u64(0)
	asm ${row[1]} raw {
		"${row[2]}"
		; [o] "=d" (o)
		;
		; cc
	}
	println(o)
}
')
		assert (errors.len == 0) == (row[0] == 'ok'), '${row} ${errors}'
	}
	assert raw_block_errors('identity_flags', ['orq %%rbx, %%rbx'], ';\n;').any(it.contains('condition flags'))
}

// One place may save two registers, one after the other or one inside the other.
fn test_one_place_saves_two_registers() {
	for index, lines in [
		['movq %%rbx, %%r11', 'movq $5, %%rbx', 'movq %%r11, %%rbx', 'movq %%rcx, %%r11',
			'movq $7, %%rcx', 'movq %%r11, %%rcx'],
		['xchgq %%rbx, %%r11', 'xchgq %%rcx, %%r11', 'movq $5, %%rbx', 'movq $7, %%rcx',
			'xchgq %%rcx, %%r11', 'xchgq %%rbx, %%r11'],
	] {
		assert raw_block_errors('two_${index}', lines, ';\n;\n; r11') == []
	}
}

// Both the save and the copy back must use a place that may change.
fn test_save_and_copy_back_each_need_a_place_that_may_change() {
	for index, pair in [
		['xchgq %%rbx, %%rcx', 'movq %%rcx, %%rbx'],
		['movq %%rbx, %%rcx', 'xchgq %%rcx, %%rbx'],
		['movq %%rbx, %[s]', 'xchgq %[s], %%rbx'],
		['xchgq %%rbx, %[s]', 'movq %[s], %%rbx'],
	] {
		lines := [pair[0], 'movq $5, %%rbx', pair[1]]
		assert !raw_rbx_is_saved('mixed_input_${index}', lines, ';\n; [s] "c" (s)'), lines.str()
		assert raw_rbx_is_saved('mixed_output_${index}', lines, '; [s] "+c" (o)'), lines.str()
	}
}

// A clobber that is only a `%` names no register, and the check reads past it.
fn test_bare_percent_clobber_does_not_crash_the_check() {
	for index, clobber in ['%', '#', '%%'] {
		assert !raw_rbx_is_saved('bare_clobber_${index}', ['movq $5, %%rbx'], ';\n;\n; ${clobber}'), clobber
	}
}

fn test_xchg_that_restores_nothing() {
	// One `xchg` alone, in both operand orders, does not restore itself.
	for single in raw_pair('xchgq', '%%rbx', '%%r11') {
		source := 'asm amd64 raw {\n\t"${single}"\n}'
		assert inline_asm_raw_mov_restored(raw_instructions_of(source, 'amd64'), 64, &raw_all_declared).len == 0, single
	}
	for pair in [
		// Another place, a narrower view, the register itself, the stack pointer, memory.
		['xchgq %%rbx, %%r11', 'xchgq %%rbx, %%r10'],
		['xchgq %%rbx, %[slot]', 'xchgq %%rbx, %[Slot]'],
		['xchgl %%ebx, %%r11d', 'xchgl %%ebx, %%r11d'],
		['xchgb %%bl, %%cl', 'xchgb %%bl, %%cl'],
		['xchgq %%rbx, %%rbx', 'xchgq %%rbx, %%rbx'],
		['xchgq %%rbx, %%rsp', 'xchgq %%rbx, %%rsp'],
		['xchgq %%rbx, (%%rdi)', 'xchgq %%rbx, (%%rdi)'],
		['xchgq (%%rdi), %%rbx', 'xchgq (%%rdi), %%rbx'],
		['xchgq %[rbx], %%r11', 'xchgq %[rbx], %%r11'],
		['cmpxchgq %%rbx, %%r11', 'cmpxchgq %%rbx, %%r11'],
		['xaddq %%rbx, %%r11', 'xaddq %%rbx, %%r11'],
	] {
		assert !raw_mov_save_restored('amd64', pair[0], pair[1]), pair.str()
	}
}

fn test_rex_prefixes_are_the_spellings_of_the_binutils_table() {
	for base in ['rex', 'rex64'] {
		for suffix in ['', 'z', 'y', 'yz', 'x', 'xz', 'xy', 'xyz'] {
			assert inline_asm_raw_is_rex_prefix(base + suffix), base + suffix
		}
	}
	for suffix in ['b', 'x', 'xb', 'r', 'rb', 'rx', 'rxb', 'w', 'wb', 'wx', 'wxb', 'wr', 'wrb',
		'wrx', 'wrxb'] {
		assert inline_asm_raw_is_rex_prefix('rex.' + suffix), suffix
	}
	for word in ['rex.', 'rex.bw', 'rex.ww', 'rex.q', 'rexzy', 'rexzz', 'rex64q', 'rexx64', 'rexw',
		're'] {
		assert !inline_asm_raw_is_rex_prefix(word), word
	}
}

// raw_asm_docs returns the raw-asm check section of doc/docs.md as one line of text.
fn raw_asm_docs() string {
	docs := os.read_file(os.join_path(@VEXEROOT, 'doc', 'docs.md')) or { panic(err) }
	start := docs.index('In x86 raw blocks that use AT&T syntax') or { panic('no width-check text') }
	end := docs.index_after('For a loop over a V buffer', start) or { panic('no section end') }
	return docs[start..end].replace('\n', ' ')
}

// raw_asm_docs_words returns the backquoted words of the docs text from `from` up to `to`.
fn raw_asm_docs_words(docs string, from string, to string) []string {
	start := docs.index(from) or { panic('docs no longer say `${from}`') }
	end := docs.index_after(to, start + from.len) or { panic('docs no longer say `${to}`') }
	mut words := []string{}
	parts := docs[start..end].split('`')
	for i := 1; i < parts.len; i += 2 {
		words << parts[i]
	}
	return words
}

// The docs list the prefixes the clobber check reads past and the directives both checks read;
// each list must be the code's table exactly (vlang/v PR 31407: docs sentences about the checks
// were false in two review rounds in a row).
fn test_raw_asm_docs_lists_match_the_tables() {
	docs := raw_asm_docs()
	read := raw_asm_docs_words(docs, 'The check reads past only the', 'prefixes.')
	assert read.sorted() == inline_asm_raw_read_prefixes[..].sorted()
	inert := raw_asm_docs_words(docs, 'the alignment and symbol directives', 'Any other directive')
	assert inert.sorted() == inline_asm_raw_inert_directives[..].sorted()
	assert docs.contains('the `.cfi_` directives')
}

// Every example the docs give for a group must be in the code's table for that group, and an
// example of what stops a check must not be in the table of what the check reads.
fn test_raw_asm_docs_examples_match_the_tables() {
	docs := raw_asm_docs()
	unread := raw_asm_docs_words(docs, 'A template with any other prefix', 'is not checked either')
	assert unread.len > 0
	for word in unread {
		assert inline_asm_raw_is_prefix(word), word
		assert word !in inline_asm_raw_read_prefixes, word
	}
	stops := raw_asm_docs_words(docs, 'Any other directive, such as', 'stop both checks')
	assert stops.len > 0
	for directive in stops {
		assert directive !in inline_asm_raw_inert_directives, directive
		assert !directive.starts_with('.cfi_'), directive
	}
	dst := raw_asm_docs_words(docs, 'destination of common instructions such as', 'and the registers')
	assert dst.len > 0
	for family in dst {
		assert family in inline_asm_raw_writes_dst_families, family
	}
	flags := raw_asm_docs_words(docs, 'common flag-setting instruction such as', 'unless')
	for word in flags {
		assert word.trim_right('bwlq') in inline_asm_raw_flag_families, word
	}
	string_example := raw_asm_docs_words(docs, 'a string instruction such as', '. It reports')
	for word in string_example {
		assert word.trim_right('bwlq') in inline_asm_raw_string_families, word
	}
	// Each far transfer the table holds is named, the `l` forms as "a far jump, call or return".
	far := raw_asm_docs_words(docs, 'any form of', 'a 16-bit `callw`')
	for mnemonic in inline_asm_raw_far_transfers {
		if mnemonic in ['ljmp', 'lcall', 'lret'] {
			continue
		}
		assert mnemonic in far, mnemonic
	}
	assert docs.contains('a far jump, call or return')
}

// A statement without the operands its mnemonic needs is refused by the assembler; the checker
// must not read an operand that is not there.
fn test_operandless_statements_do_not_crash_the_check() {
	for statement in ['div', 'idiv', 'mul', 'imul', 'mov', 'push', 'pop', 'call', 'xchg'] {
		errors := att_width_errors('operandless_${statement}', 'module main

fn main() {
	mut x := u64(1)
	asm amd64 raw {
		"${statement}"
		; "+c" (x)
		;
		; cc
	}
	println(x)
}
')
		assert errors.len <= 1, statement + errors.str()
	}
}

// raw_is_free reports whether the compiler may choose a register for an operand.
fn raw_is_free(constraint string) bool {
	return inline_asm_raw_read_constraint(constraint).is_free
}

fn test_operand_is_pinned_only_without_a_register_the_compiler_chooses() {
	// Written out rather than taken from the checker's list, so a letter dropped from it fails.
	pinned := ['a', 'b', 'c', 'd', 'S', 'D', 'A', 'i', 'n', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
		'e', 'Z', 'E', 'F', 'G', 'H', 's', 'x', 't', 'u', 'f', '0', '9', '{rdx}', '{r11}', '{RBX}',
		'v', 'y', 'k', 'C', 'Yz', 'Yk', 'Yi', 'Ym', 'Yt', 'Y2', 'YW', 'Yd', 'Yr', 'Yv', 'Yw', 'We',
		'Wz', 'Wd', 'Ws', 'Wb', 'Wf', 'Ww', 'Bz', 'Q', 'R', 'U', '[o]', '[mr]', 'd^', 'd$', 'd#r',
		'd#rm', 'd ', 'd, d']
	free := ['r', 'q', 'l', 'g', 'm', 'o', 'V', '<', '>', 'p', 'X', 'Y', 'Yp', 'YC', 'Ya', 'Yb',
		'T', 'Tv', 'Ts', 'W', 'Wc', 'B', 'Bm', 'P', 'w', 'h', 'j', 'jc', 'z', 'd#a,r', 'd#r,m']
	for modifier in ['', '=', '+', '=&', '+&', '%', '?', '*', '!', '^', '$'] {
		for letter in pinned {
			constraint := '${modifier}${letter}'
			assert !raw_is_free(constraint), constraint
		}
		for letter in free {
			constraint := '${modifier}${letter}'
			assert raw_is_free(constraint), constraint
			// One free letter among pinned ones, or in another alternative, frees the operand.
			assert raw_is_free('${modifier}a${letter}'), constraint
			assert raw_is_free('${modifier}d,${letter}'), constraint
			assert raw_is_free('${modifier}{rdx}${letter}'), constraint
		}
	}
	// A name or a brace that is not closed ends with the constraint.
	assert inline_asm_raw_read_constraint('=[a').registers == []
	assert inline_asm_raw_read_constraint('{rdx').registers == ['rdx']
	assert raw_is_free('=Y')
	// A letter after `Y`, `W`, `T`, `B` or `j` names no register, and one after `#` neither.
	for constraint in ['Yc', 'Wd', 'Tb', 'Ba', 'jc', 'YA', 'i#a', 'i#A'] {
		read := inline_asm_raw_read_constraint(constraint)
		assert read.registers == [] && !read.has_pair, constraint
	}
	assert inline_asm_raw_read_constraint('=abcdSD').registers == ['rax', 'rbx', 'rcx', 'rdx',
		'rsi', 'rdi']
	assert inline_asm_raw_read_constraint('d#a,c').registers == ['rdx', 'rcx']
	assert inline_asm_raw_read_constraint('AQ').has_pair
	// A free alternative beside a matching one may hold the input in any register.
	assert inline_asm_raw_read_constraint('0').is_in_changeable(map[string]bool{})
	assert !inline_asm_raw_read_constraint('0r').is_in_changeable(map[string]bool{})
	// V binds an operand without a constraint with `r` or `+r`, and one that names nothing may sit
	// anywhere.
	for constraint in ['', '=', '#d', '=#r'] {
		assert raw_is_free(constraint), constraint
	}
	for constraint in ['=@ccz', '=@ccnz', '+@cca', 'a,d', '=a,=d', '0,1'] {
		assert !raw_is_free(constraint), constraint
	}
}

fn test_integer_value_reads_what_the_assemblers_read() {
	for text, want in {
		'0':                    u64(0)
		'00':                   0
		'010':                  8
		'0x1F':                 31
		'0B101':                5
		'18446744073709551615': max_u64
		'-1':                   max_u64
		'~0':                   max_u64
		'!0':                   1
		'!7':                   0
		'+-(2)':                max_u64 - 1
		'((3))':                3
	} {
		assert inline_asm_raw_integer_value(text) or { panic('${text}: none') } == want, text
	}
	for text in ['', '-', '()', '(0', '(0+1)', '(0]', '0)', '1-1', '08', '0b2', '0x', '0xg', '1x5',
		'1b1', '18446744073709551616', 'x', '0h', '1_0'] {
		assert inline_asm_raw_integer_value(text) == none, text
	}
	// Deep nesting must not exhaust the stack.
	n := 1_000_000
	assert inline_asm_raw_integer_value('('.repeat(n) + '-(~(0))' + ')'.repeat(n)) or { 0 } == 1
	assert inline_asm_raw_integer_value('-'.repeat(n) + '1') or { 0 } == 1
	assert inline_asm_raw_integer_value('('.repeat(n) + '1' + ')'.repeat(n - 1)) == none
}

fn test_lea_keeps_reads_the_address() {
	for address in ['(rbx)', '0(rbx)', '(,rbx)', '(,rbx,1)', '(0)(,rbx,(1))', '(rbx,riz,4)'] {
		assert inline_asm_raw_lea_keeps(address, 'rbx', 64, 64), address
	}
	for address in ['rbx', '(rbx', 'rbx)', '(rbx)x', '(,rbx,1,2)', '(rbx,,2)', '(,riz)', '()',
		'(,,1)', '(rcx,rbx,1)', '(,rbx,2)', '1(rbx)', 'x(rbx)', '(ebx)'] {
		assert !inline_asm_raw_lea_keeps(address, 'rbx', 64, 64), address
	}
	assert inline_asm_raw_lea_keeps('(ebx)', 'bx', 16, 64)
	assert !inline_asm_raw_lea_keeps('(si)', 'si', 16, 64)
	assert inline_asm_raw_lea_keeps('(si)', 'si', 16, 32)
}
