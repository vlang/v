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

// inline_asm_att_suffix_families are the mnemonics whose one size suffix sizes every operand.
const inline_asm_att_suffix_families = ['mov', 'add', 'sub', 'and', 'or', 'xor', 'cmp', 'test',
	'inc', 'dec', 'neg', 'not', 'shl', 'shr', 'sar', 'lea', 'push', 'pop']!

// inline_asm_att_shift_families take their count in `%cl` or an immediate, so only the
// destination of a shift has the suffix width.
const inline_asm_att_shift_families = ['shl', 'shr', 'sar']!

// inline_asm_att_mnemonic_widths returns the source and destination operand widths that a
// GNU x86 mnemonic states through its size suffix, or zeros when it states none. `movzbl`
// and `movslq` carry two suffixes: the first sizes the source and the second the destination.
// The shift family takes its count in `%cl`, so `is_shift` limits the check to the destination.
fn inline_asm_att_mnemonic_widths(name string) (int, int, bool) {
	if name.len == 6 && (name.starts_with('movz') || name.starts_with('movs')) {
		src := inline_asm_att_suffix_width(name[4])
		dst := inline_asm_att_suffix_width(name[5])
		is_movz := name.starts_with('movz')
		if src > 0 && dst > src && (!is_movz || name[4] != `l`) {
			return src, dst, false
		}
		return 0, 0, false
	}
	if name.len < 3 {
		return 0, 0, false
	}
	base := name[..name.len - 1]
	if base !in inline_asm_att_suffix_families {
		return 0, 0, false
	}
	width := inline_asm_att_suffix_width(name[name.len - 1])
	return width, width, base in inline_asm_att_shift_families
}

// inline_asm_att_suffix_width maps a GNU size suffix letter to its operand width in bits.
fn inline_asm_att_suffix_width(c u8) int {
	return match c {
		`b` { 8 }
		`w` { 16 }
		`l` { 32 }
		`q` { 64 }
		else { 0 }
	}
}

// inline_asm_att_modifier_width maps a GNU x86 operand modifier letter to the register width
// it selects: `%k` is the 32-bit view, `%w` the 16-bit one, and `%b` and `%h` the 8-bit ones.
fn inline_asm_att_modifier_width(modifier u8) int {
	return match modifier {
		`k` { 32 }
		`w` { 16 }
		`b`, `h` { 8 }
		`q` { 64 }
		else { 0 }
	}
}

// inline_asm_raw_gpr_constraint reports whether a constraint can only place its operand in a
// general-purpose register. A `g`, `m` or `i` constraint may print a memory address or an
// immediate, which carries no register width.
fn inline_asm_raw_gpr_constraint(constraint string) bool {
	mut letters := 0
	for c in constraint {
		if c in [`+`, `=`, `&`] {
			continue
		}
		if c !in [`r`, `q`, `Q`, `R`, `a`, `b`, `c`, `d`, `S`, `D`] {
			return false
		}
		letters++
	}
	return letters > 0
}

// inline_asm_att_arch_bits returns the general register width of an x86 instruction set:
// 64 for amd64 and 32 for the i386 family.
fn inline_asm_att_arch_bits(arch string) int {
	return if arch in ['amd64', 'x64', 'x86_64'] { 64 } else { 32 }
}

// inline_asm_att_register_width returns the width that a literal register fixes in an AT&T
// instruction of a block whose general registers are `arch_bits` wide, or 0 when it fixes
// none. Segment registers move to and from general registers of every width, and control
// and debug registers have the general register width of the instruction set.
fn inline_asm_att_register_width(name string, arch_bits int) int {
	register := name.to_lower_ascii()
	if register in ['cs', 'ss', 'ds', 'es', 'fs', 'gs'] {
		return 0
	}
	if register.starts_with('cr') || register.starts_with('dr') {
		return arch_bits
	}
	return inline_asm_intel_register_width(register)
}

// inline_asm_att_enum_explicit_backing_type returns the `as` backing type of an enum, or none
// when the enum is declared without one.
fn (tc &TypeChecker) inline_asm_att_enum_explicit_backing_type(name_id u32) ?Type {
	name_syntax := flat.source_type_syntax(name_id)
	short_name_id := name_syntax.unqualified()
	is_qualified := flat.source_type_shape(name_id).has(.dotted)
	for index in tc.top_level_idx {
		decl := tc.a.nodes[index]
		if decl.kind != .enum_decl || !inline_asm_enum_decl_is_named(decl.value, short_name_id) {
			continue
		}
		file := tc.a.source_files[decl.pos.id] or { continue }
		module_name := tc.file_modules[file.name] or { '' }
		if is_qualified && !inline_asm_enum_module_is_qualifier(module_name, name_syntax.qualifier()) {
			continue
		}
		if decl.enum_backing_type_id() != 0 {
			return *tc.parse_type(decl.enum_backing_type_id())
		}
		break
	}
	return none
}

// inline_asm_att_type_width returns the width of the register that an unmodified `%[name]`
// prints for an operand of type `typ`, or 0 when it prints no general register. V's `int`,
// `isize`, `usize` and pointers have the target's pointer width, which is `arch_bits` on any
// target that assembles the block. On the i386 family GCC keeps a 64-bit value in a register
// pair and `%[name]` prints its low 32-bit register.
fn (tc &TypeChecker) inline_asm_att_type_width(typ Type, arch_bits int) int {
	clean := unalias_type(typ)
	mut width := 0
	match clean {
		Primitive {
			if clean.props.has(.float) {
				return 0
			}
			width = if clean.props.has(.integer) && clean.size == 0 {
				arch_bits
			} else {
				tc.inline_asm_intel_operand_width(clean) or { 0 }
			}
		}
		ISize, USize, Pointer {
			width = arch_bits
		}
		Enum {
			// An enum without an `as` backing type is a C `int`, which is 32 bits wide.
			backing := tc.inline_asm_att_enum_explicit_backing_type(clean.source_type_id()) or {
				return 32
			}
			width = tc.inline_asm_att_type_width(backing, arch_bits)
		}
		else {
			width = tc.inline_asm_intel_operand_width(clean) or { 0 }
		}
	}
	return int_min(width, arch_bits)
}

// inline_asm_att_operand_width returns the register width that a raw AT&T operand fixes, or 0
// when it fixes none. Immediates, memory operands, floats and unbound names fix no width.
fn (tc &TypeChecker) inline_asm_att_operand_width(operand InlineAsmRawOperand, types map[string]Type, arch_bits int) int {
	match operand.kind {
		.register {
			return inline_asm_att_register_width(operand.name, arch_bits)
		}
		.operand_ref {
			// Modifiers do not establish a register width for memory or mixed constraints.
			typ := types[operand.name] or { return 0 }
			if operand.modifier != 0 {
				return inline_asm_att_modifier_width(operand.modifier)
			}
			return tc.inline_asm_att_type_width(typ, arch_bits)
		}
		else {
			return 0
		}
	}
}

// inline_asm_raw_att_operand_types maps the names of general-register operands in a raw
// template to their V types. Operands with other constraints are left out.
fn (mut tc TypeChecker) inline_asm_raw_att_operand_types(node flat.Node, block string, sections []InlineAsmRange) map[string]Type {
	mut types := map[string]Type{}
	mut child_offset := 0
	for index in 1 .. 3 {
		if index >= sections.len {
			break
		}
		ios := inline_asm_ios(block, sections[index])
		for io_index, io in ios {
			child_index := child_offset + io_index
			if io.alias.len > 0 && inline_asm_is_ident(io.expr)
				&& inline_asm_raw_gpr_constraint(io.constraint)
				&& child_index < int(node.children_count) {
				types[io.alias] = tc.resolve_type(tc.a.child(&node, child_index))
			}
		}
		child_offset += ios.len
	}
	return types
}

// check_inline_asm_raw_att_widths reports a GNU x86 mnemonic whose size suffix disagrees with
// the register width of an operand it acts on, such as `movl` applied to a 64-bit register.
// Mnemonics without a suffix, and instructions with vector or other non-general operands,
// are not checked. The GNU assembler rejects the same mismatches.
fn (mut tc TypeChecker) check_inline_asm_raw_att_widths(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, arch string, types map[string]Type) {
	template := inline_asm_raw_template(block, section, arch)
	instructions := inline_asm_raw_parse_instructions(&template, [])
	arch_bits := inline_asm_att_arch_bits(arch)
	for instruction in instructions {
		if instruction.is_directive || instruction.operands.len == 0 {
			continue
		}
		src_width, dst_width, is_shift := inline_asm_att_mnemonic_widths(instruction.mnemonic)
		if src_width == 0 {
			continue
		}
		mut widths := []int{len: instruction.operands.len}
		mut has_vector := false
		for index, operand in instruction.operands {
			widths[index] = tc.inline_asm_att_operand_width(operand, types, arch_bits)
			if widths[index] > 64 {
				has_vector = true
			}
		}
		if has_vector {
			continue
		}
		last := instruction.operands.len - 1
		for index, operand in instruction.operands {
			if is_shift && index != last {
				continue
			}
			expected := if index == last {
				dst_width
			} else if index == 0 {
				src_width
			} else {
				0
			}
			if expected == 0 || widths[index] == 0 || widths[index] == expected {
				continue
			}
			start, end := template.span(operand.start, operand.end)
			// Operand modifiers only apply to `%[name]` operands; a literal register is renamed.
			alternative := if operand.kind == .register {
				'a ${expected}-bit register'
			} else {
				'an operand modifier that selects a ${expected}-bit register'
			}
			tc.record_error_at(.compile_error, '`${instruction.mnemonic}` operates on ${expected}-bit operands here, but `${operand.text}` is ${widths[index]}-bit; use a size suffix that matches the operand, or ${alternative}',
				id, token.new_span(node.pos.id, base + start, base + end))
		}
	}
}
