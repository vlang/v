module types

import v.flat
import v.token

// Raw assembly templates (`asm amd64 raw { "..." "..." ; ... }`) are a sequence of
// double-quoted string literals that the C backend passes verbatim to the C compiler.
// The helpers in checker_asm.v skip quoted text, so they see nothing of a raw template.
// This file decodes the literals into the text the assembler receives, keeps a map from
// every decoded byte back to its block offset, and tokenizes that text into GNU assembler
// statements. Diagnostics built on it can therefore point into the string literal that a
// label, mnemonic or operand came from.

// InlineAsmRawTemplate is the decoded text of a raw template section. `starts[i]` and
// `ends[i]` are the block offsets of the source bytes that produced `text[i]`, so an
// escape such as `\n` maps one decoded byte onto its two source bytes.
struct InlineAsmRawTemplate {
	text   string
	starts []int
	ends   []int
}

// InlineAsmRawLine is one assembler statement of a decoded template: a half open
// [start, end) span into `InlineAsmRawTemplate.text`.
struct InlineAsmRawLine {
	start int
	end   int
}

// InlineAsmRawOperandKind classifies an operand by its leading syntax only; the
// instruction-specific meaning is left to the checks built on top of the tokenizer.
enum InlineAsmRawOperandKind {
	// `%%rax` in GNU x86 syntax, or a bare register name on instruction sets that use them.
	register
	// `%[name]`, `%0`, or `%k[name]` with an operand modifier: a bound V operand.
	operand_ref
	// `$16`, `#16`: an immediate.
	immediate
	// `8(%%rax,%%rcx,4)`, `[%[ptr], #16]`, `%%fs:0x28`: an addressed operand.
	memory
	// Anything else: a label reference such as `1b` or `.Lloop%=`, a symbol, a qualifier.
	symbol
}

// InlineAsmRawOperand is one comma-separated operand with its offsets into the decoded
// text. `name` is the register without `%%`, the alias or number of an operand reference
// without `%` and modifier, or the full text otherwise. `modifier` is the GNU operand
// modifier letter of an operand reference, or 0 when there is none.
struct InlineAsmRawOperand {
mut:
	kind     InlineAsmRawOperandKind
	text     string
	name     string
	modifier u8
	start    int
	end      int
}

// InlineAsmRawLabel is one `name:` label definition, without its colon or surrounding
// quotes and with `.L` prefixes and `%=` kept verbatim, at its offsets into the decoded text.
struct InlineAsmRawLabel {
	name      string
	start     int
	end       int
	is_quoted bool
}

// InlineAsmRawInstruction is one tokenized statement. `labels` holds its leading label
// definitions (`a: b: nop` defines two). A statement that is only labels has an empty
// mnemonic. `is_directive` reports a mnemonic that starts with `.`.
struct InlineAsmRawInstruction {
mut:
	labels         []InlineAsmRawLabel
	mnemonic       string
	mnemonic_start int
	mnemonic_end   int
	is_directive   bool
	operands       []InlineAsmRawOperand
	start          int
	end            int
}

// Instruction prefixes that the GNU assembler accepts as a separate leading word; they are
// folded into the mnemonic so `lock addq` and `rep movsb` keep a single instruction name.
const inline_asm_raw_prefixes = ['lock', 'rep', 'repe', 'repz', 'repne', 'repnz', 'data16', 'addr32',
	'notrack', 'bnd', 'xacquire', 'xrelease']!

// inline_asm_raw_template decodes the double-quoted string literals of a raw template
// section into the text the assembler receives, with assembler comments blanked out for
// `arch` (see inline_asm_raw_mask_comments). Unknown escapes keep their character; `%%`
// and `%=` are assembler syntax, not escapes, and stay as written.
fn inline_asm_raw_template(block string, section InlineAsmRange, arch string) InlineAsmRawTemplate {
	// C removes line splices before interpreting string literals and their escapes.
	mut source := []u8{cap: section.end - section.start}
	mut offsets := []int{cap: section.end - section.start}
	mut pos := section.start
	for pos < section.end {
		if block[pos] == `\\` && pos + 1 < section.end {
			if block[pos + 1] == `\n` {
				pos += 2
				continue
			}
			if block[pos + 1] == `\r` && pos + 2 < section.end && block[pos + 2] == `\n` {
				pos += 3
				continue
			}
		}
		source << block[pos]
		offsets << pos
		pos++
	}
	literal := source.bytestr()
	mut text := []u8{cap: literal.len}
	mut starts := []int{cap: literal.len}
	mut ends := []int{cap: literal.len}
	mut i := 0
	for i < literal.len {
		if literal[i] != `"` {
			i++
			continue
		}
		i++
		for i < literal.len && literal[i] != `"` {
			if literal[i] != `\\` || i + 1 >= literal.len {
				text << literal[i]
				starts << offsets[i]
				ends << offsets[i] + 1
				i++
				continue
			}
			decoded, consumed := inline_asm_raw_decode_escape(literal, i, literal.len)
			text << decoded
			starts << offsets[i]
			ends << offsets[i + consumed - 1] + 1
			i += consumed
		}
		// Skip the closing quote; an unterminated literal simply ends the section.
		i++
	}
	return InlineAsmRawTemplate{
		text:   inline_asm_raw_mask_comments(text.bytestr(), arch)
		starts: starts
		ends:   ends
	}
}

// The x86 instruction set names a raw block can carry; `#` starts a comment on these,
// while on arm64 it prefixes an immediate and `//` starts the comment.
const inline_asm_raw_x86_arches = ['amd64', 'x64', 'x86_64', 'i386', 'i486', 'i586', 'i686', 'x86',
	'x86_32', 'ia-32', 'ia32']!

// inline_asm_raw_mask_comments replaces every GNU assembler comment in decoded template
// text with spaces, keeping newlines and the text length, so offsets into the result still
// map back to the source: `/* ... */` on every instruction set, `# ...` to the end of the
// line on x86, and `// ...` to the end of the line elsewhere. Comment characters inside a
// string or character constant are kept.
fn inline_asm_raw_mask_comments(text string, arch string) string {
	if !text.contains('/') && !text.contains('#') {
		return text
	}
	is_x86 := arch in inline_asm_raw_x86_arches
	mut out := text.bytes()
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `"` || c == `'` {
			i = inline_asm_raw_skip_literal(text, i, text.len)
			continue
		}
		mut comment_end := -1
		if c == `/` && i + 1 < text.len && text[i + 1] == `*` {
			close := text.index_after('*/', i + 2) or { text.len }
			comment_end = int_min(close + 2, text.len)
		} else if (is_x86 && c == `#`)
			|| (!is_x86 && c == `/` && i + 1 < text.len && text[i + 1] == `/`) {
			comment_end = text.index_after('\n', i) or { text.len }
		}
		if comment_end < 0 {
			i++
			continue
		}
		for masked in i .. comment_end {
			if out[masked] != `\n` {
				out[masked] = ` `
			}
		}
		i = comment_end
	}
	return out.bytestr()
}

// inline_asm_raw_decode_escape decodes the C escape sequence at block[i] (a backslash)
// and returns the byte it stands for and the number of source bytes it spans.
fn inline_asm_raw_decode_escape(block string, i int, end int) (u8, int) {
	c := block[i + 1]
	match c {
		`n` { return `\n`, 2 }
		`t` { return `\t`, 2 }
		`r` { return `\r`, 2 }
		`a` { return 7, 2 }
		`b` { return 8, 2 }
		`f` { return 12, 2 }
		`v` { return 11, 2 }
		`e` { return 27, 2 }
		`x` {
			mut value := 0
			mut j := i + 2
			// C consumes every following hex digit; an out-of-range value is a C error, so
			// keeping the low byte is as good a recovery as any.
			for j < end && block[j].is_hex_digit() {
				value = value * 16 + inline_asm_raw_hex_value(block[j])
				j++
			}
			if j == i + 2 {
				return `x`, 2
			}
			return u8(value), j - i
		}
		else {}
	}
	if c >= `0` && c <= `7` {
		mut value := 0
		mut j := i + 1
		for j < end && j < i + 4 && block[j] >= `0` && block[j] <= `7` {
			value = value * 8 + int(block[j] - `0`)
			j++
		}
		return u8(value), j - i
	}
	// `\\`, `\"`, `\'`, `\$` and anything unknown stand for the character itself.
	return c, 2
}

fn inline_asm_raw_hex_value(c u8) int {
	if c.is_digit() {
		return int(c - `0`)
	}
	if c >= `a` {
		return int(c - `a`) + 10
	}
	return int(c - `A`) + 10
}

// span returns the block offsets [start, end) of the source bytes that produced the
// decoded text range [start, end). An empty range maps to an empty span at its start; a
// range at or past the end of the text maps to the end of the last literal's content.
fn (t &InlineAsmRawTemplate) span(start int, end int) (int, int) {
	if t.starts.len == 0 {
		return 0, 0
	}
	if start >= t.starts.len {
		past := t.ends[t.ends.len - 1]
		return past, past
	}
	first := int_max(start, 0)
	if end <= start {
		return t.starts[first], t.starts[first]
	}
	last := int_min(int_max(end - 1, 0), t.ends.len - 1)
	return t.starts[first], t.ends[last]
}

// inline_asm_raw_skip_literal returns the offset just past the assembler-level string or
// character constant that starts at text[i], or i when none starts there. A `"..."` string
// honours backslash escapes; a `'c` character constant takes one (possibly escaped)
// character and an optional closing `'`, as the GNU assembler accepts both `'A` and `'A'`.
fn inline_asm_raw_skip_literal(text string, i int, end int) int {
	if text[i] == `"` {
		mut j := i + 1
		for j < end && text[j] != `"` {
			if text[j] == `\\` && j + 1 < end {
				j++
			}
			j++
		}
		return int_min(j + 1, end)
	}
	if text[i] == `'` {
		mut j := i + 1
		if j < end && text[j] == `\\` {
			j++
		}
		j++
		if j < end && text[j] == `'` {
			j++
		}
		return int_min(j, end)
	}
	return i
}

// inline_asm_raw_lines splits the decoded text into assembler statements on newlines and
// on the `;` statement separator, outside string and character constants such as the
// operand of `.ascii` or `$'"'`.
fn inline_asm_raw_lines(t &InlineAsmRawTemplate) []InlineAsmRawLine {
	mut lines := []InlineAsmRawLine{}
	mut line_start := 0
	mut i := 0
	for i < t.text.len {
		c := t.text[i]
		if c == `"` || c == `'` {
			i = inline_asm_raw_skip_literal(t.text, i, t.text.len)
			continue
		}
		if c == `\n` || c == `;` {
			lines << InlineAsmRawLine{
				start: line_start
				end:   i
			}
			line_start = i + 1
		}
		i++
	}
	if line_start < t.text.len {
		lines << InlineAsmRawLine{
			start: line_start
			end:   t.text.len
		}
	}
	return lines
}

// inline_asm_raw_parse_instructions tokenizes every non-blank statement of a decoded
// template. `registers` names the words that are registers without any prefix: pass
// `util.asm_register_names(arch)` for arm64 and friends or for `raw intel` templates, and
// an empty list for GNU x86 syntax, where registers carry `%%` and a bare `rax` is a
// symbol.
fn inline_asm_raw_parse_instructions(t &InlineAsmRawTemplate, registers []string) []InlineAsmRawInstruction {
	mut instructions := []InlineAsmRawInstruction{}
	for line in inline_asm_raw_lines(t) {
		mut start := line.start
		for start < line.end && t.text[start].is_space() {
			start++
		}
		mut end := line.end
		for end > start && t.text[end - 1].is_space() {
			end--
		}
		if start >= end {
			continue
		}
		mut instruction := InlineAsmRawInstruction{
			start: start
			end:   end
		}
		mut i := start
		for {
			name_end, colon := inline_asm_raw_label_end(t.text, i, end)
			if colon <= i {
				break
			}
			is_quoted := t.text[i] == `"`
			label_start := if is_quoted { i + 1 } else { i }
			label_end := if is_quoted { name_end - 1 } else { name_end }
			instruction.labels << InlineAsmRawLabel{
				name:      t.text[label_start..label_end]
				start:     label_start
				end:       label_end
				is_quoted: is_quoted
			}
			i = inline_asm_raw_skip_blanks(t.text, colon + 1, end)
		}
		if i >= end {
			instructions << instruction
			continue
		}
		mut mnemonic_end := i
		for mnemonic_end < end && !t.text[mnemonic_end].is_space() {
			mnemonic_end++
		}
		mut mnemonic := t.text[i..mnemonic_end]
		mut args_start := inline_asm_raw_skip_blanks(t.text, mnemonic_end, end)
		mut prefix := mnemonic
		for inline_asm_raw_is_prefix(prefix) && args_start < end {
			mut second_end := args_start
			for second_end < end && !t.text[second_end].is_space() {
				second_end++
			}
			prefix = t.text[args_start..second_end]
			mnemonic += ' ' + prefix
			mnemonic_end = second_end
			args_start = inline_asm_raw_skip_blanks(t.text, second_end, end)
		}
		instruction.mnemonic = mnemonic
		instruction.mnemonic_start = i
		instruction.mnemonic_end = mnemonic_end
		instruction.is_directive = mnemonic.starts_with('.')
		instruction.operands = inline_asm_raw_parse_operands(t.text, args_start, end, registers)
		instructions << instruction
	}
	return instructions
}

// inline_asm_raw_is_prefix reports a leading word that modifies the instruction after
// it: the `lock`/`rep` family, the `rex`/`rex.w`/`rex64` encoding prefixes, and the
// `{vex}`/`{evex}`/`{disp32}` pseudo prefixes of GNU syntax. `vex` and `xop` are not
// bare prefix words in GNU syntax, and `vextracti128`-style mnemonics start with them.
fn inline_asm_raw_is_prefix(word string) bool {
	lower := word.to_lower_ascii()
	if lower in inline_asm_raw_prefixes {
		return true
	}
	if lower.starts_with('{') && lower.ends_with('}') {
		return true
	}
	return lower == 'rex' || lower.starts_with('rex.') || lower.starts_with('rex64')
}

// inline_asm_raw_label_end recognizes a `name:` label definition at `start` and returns
// the end of the name and the offset of the colon, or `start` twice when there is none.
// A label token may be quoted or contain `.`, `$` and the `%=` uniqueness marker, and
// the assembler allows blanks before the colon (`name :`); a `::` or a `:` followed
// by an operand character (`%%fs:0x28`) is not a label.
fn inline_asm_raw_label_end(text string, start int, end int) (int, int) {
	mut i := start
	if i < end && text[i] == `"` {
		i = inline_asm_raw_skip_literal(text, i, end)
	} else {
		for i < end {
			c := text[i]
			if inline_asm_is_ident_char(c) || c == `.` || c == `$` {
				i++
				continue
			}
			if c == `%` && i + 1 < end && text[i + 1] == `=` {
				i += 2
				continue
			}
			break
		}
	}
	name_end := i
	for i < end && text[i] in [` `, `\t`] {
		i++
	}
	if name_end == start || i >= end || text[i] != `:` {
		return start, start
	}
	if text[start] == `%` || (i + 1 < end && text[i + 1] == `:`) {
		return start, start
	}
	return name_end, i
}

fn inline_asm_raw_skip_blanks(text string, start int, end int) int {
	mut i := start
	for i < end && text[i].is_space() {
		i++
	}
	return i
}

// inline_asm_raw_parse_operands splits the operand list on commas outside `()`, `[]`,
// `{}`, the GNU `%{ %}` brace escapes and string or character constants, then
// classifies each operand.
fn inline_asm_raw_parse_operands(text string, start int, end int, registers []string) []InlineAsmRawOperand {
	mut operands := []InlineAsmRawOperand{}
	mut operand_start := start
	mut depth := 0
	mut i := start
	for i <= end {
		at_end := i == end
		if !at_end {
			c := text[i]
			if c == `"` || c == `'` {
				i = inline_asm_raw_skip_literal(text, i, end)
				continue
			}
			if c == `%` && i + 1 < end && text[i + 1] in [`{`, `}`, `|`] {
				match text[i + 1] {
					`{` { depth++ }
					`}` { depth-- }
					else {}
				}
				i += 2
				continue
			}
			match c {
				`(`, `[`, `{` { depth++ }
				`)`, `]`, `}` { depth-- }
				else {}
			}
		}
		if at_end || (text[i] == `,` && depth <= 0) {
			left := inline_asm_raw_skip_blanks(text, operand_start, i)
			mut right := i
			for right > left && text[right - 1].is_space() {
				right--
			}
			if left < right {
				operands << inline_asm_raw_classify_operand(text, left, right, registers)
			}
			operand_start = i + 1
		}
		i++
	}
	return operands
}

// inline_asm_raw_classify_operand builds the operand for text[start..end].
fn inline_asm_raw_classify_operand(text string, start int, end int, registers []string) InlineAsmRawOperand {
	operand_text := text[start..end]
	mut operand := InlineAsmRawOperand{
		kind:  .symbol
		text:  operand_text
		name:  operand_text
		start: start
		end:   end
	}
	// `*%%rax` is an indirect branch target; the register is what matters for checks.
	body := operand_text.trim_left('*')
	if body.len == 0 {
		return operand
	}
	if body[0] == `$` || body[0] == `#` {
		operand.kind = .immediate
		operand.name = body[1..]
		return operand
	}
	if body.starts_with('%%') {
		// A segment override such as `%%fs:0x28` addresses memory; `%%st(0)` is the x87
		// stack register, so `(` alone does not make a `%%` operand an address.
		if body.contains(':') {
			operand.kind = .memory
			return operand
		}
		operand.kind = .register
		operand.name = body[2..]
		return operand
	}
	// `(%%rax)`, `8(%%rax,%%rcx,4)` and `%[sym](%%rip)` are GNU addresses; `[x0, #16]`
	// is an arm64 address and `qword ptr [rax]` an Intel one. A `[` directly after a
	// bare word is an arm64 vector lane such as `v6.d[1]`, and `%[name]` is an operand
	// reference, so neither of those addresses memory.
	before_bracket := body.all_before('[')
	if body[0] == `[` || body.contains('(') || (body.contains('[') && before_bracket.contains(' ')) {
		operand.kind = .memory
		return operand
	}
	if body.contains(':') && body[0] != `.` && body[0] != `:` && body[0] != `%` {
		// A bare segment such as `fs:0x28` addresses memory; a leading `:` is an arm64
		// relocation specifier such as `:lo12:sym`, which stays a symbol.
		operand.kind = .memory
		return operand
	}
	if body[0] == `%` && body.len > 1 && body[1] != `=` {
		mut name_start := 1
		if body.len > 2 && body[1].is_letter() && (body[2] == `[` || body[2].is_digit()) {
			operand.modifier = body[1]
			name_start = 2
		}
		if name_start < body.len && body[name_start] == `[` {
			close := body.index_u8(`]`)
			if close > name_start {
				operand.kind = .operand_ref
				operand.name = body[name_start + 1..close]
				return operand
			}
		} else if name_start < body.len && body[name_start].is_digit() {
			operand.kind = .operand_ref
			operand.name = body[name_start..]
			return operand
		}
		return operand
	}
	// Bare register names: `x0`, `v0.16b`, `w1` on arm64, or `rax` in Intel syntax. The
	// caller passes no bare names for GNU x86 syntax, where `rax` is a symbol.
	bare := body.all_before('.').all_before('[').to_lower_ascii()
	if bare in registers {
		operand.kind = .register
		operand.name = bare
	}
	return operand
}

// check_inline_asm_raw_labels warns about a label that a raw template inside a function
// body defines without the GNU `%=` uniqueness marker. The C compiler emits one copy of
// the template per instantiation of the function, so inlining or a generic specialization
// defines the same assembler symbol twice. Numeric local labels (`1:`) are always local,
// and a label declared with `.globl`, `.global` or `.weak` is a deliberate global symbol.
fn (mut tc TypeChecker) check_inline_asm_raw_labels(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, arch string) {
	template := inline_asm_raw_template(block, section, arch)
	instructions := inline_asm_raw_parse_instructions(&template, [])
	mut exported := map[string]bool{}
	for instruction in instructions {
		directive := instruction.mnemonic.to_lower_ascii()
		if instruction.is_directive && directive in ['.globl', '.global', '.weak'] {
			for operand in instruction.operands {
				// Quotes delimit a symbol name; assembler backslashes remain literal bytes.
				name := if operand.text.len >= 2 && operand.text[0] == `"`
					&& operand.text[operand.text.len - 1] == `"` {
					operand.text[1..operand.text.len - 1]
				} else {
					operand.text
				}
				exported[name] = true
			}
		}
	}
	for instruction in instructions {
		for label in instruction.labels {
			name := label.name
			if name.contains('%=') || (!label.is_quoted && name.bytes().all(it.is_digit()))
				|| name in exported {
				continue
			}
			start, end := template.span(label.start, label.end)
			tc.record_warning_at(.compile_error, 'label `${name}` in a raw assembly template has no `%=` suffix, so a second instantiation of this function (inlining, or a generic specialization) defines the same assembler symbol again; use `${name}%=`, a numeric local label such as `1:`, or export the label with `.globl`',
				id, token.new_span(node.pos.id, base + start, base + end))
		}
	}
}
