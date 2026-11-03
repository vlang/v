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
	registers := if arch == 'amd64' { []string{} } else { util.asm_register_names(arch) }
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
