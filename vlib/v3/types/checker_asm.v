module types

import v3.flat
import v3.token
import v3.util

// InlineAsmRange is a half open [start, end) span into a masked assembly block.
struct InlineAsmRange {
	start int
	end   int
}

// InlineAsmWord is an identifier found in an assembly block, with its block offsets.
struct InlineAsmWord {
	text  string
	start int
	end   int
}

// InlineAsmIOSpan is one `constraint (expression)` operand, with its block offsets.
struct InlineAsmIOSpan {
	constraint string
	alias      string
	start      int
	end        int
}

// x86 instruction prefixes are written as a separate word, so the mnemonic that can be
// mistaken for an operand is the one after them.
const inline_asm_instruction_prefixes = ['lock', 'rep', 'repe', 'repz', 'repne', 'repnz']

// check_inline_asm_block reports the assembly diagnostics that only need the block's
// preserved source: unsupported operand constraints in structured `intel` blocks, and
// register names that were probably misspelled.
fn (mut tc TypeChecker) check_inline_asm_block(id flat.NodeId, node flat.Node, source string, start int, end int) {
	if start >= end {
		return
	}
	// Comments become spaces so that block offsets keep matching file offsets.
	block := inline_asm_mask_comments(source[start..end])
	open := inline_asm_index_of(block, 0, block.len, `{`) or { return }
	close := inline_asm_last_index_of(block, open, block.len, `}`) or { return }
	header := util.parse_inline_asm_header(block[..open])
	registers := util.asm_register_names(header.arch)
	sections := inline_asm_section_ranges(block, open + 1, close)
	if header.is_intel && !header.is_raw {
		for index in 1 .. 3 {
			if index < sections.len {
				tc.check_inline_asm_intel_ios(id, node, block, start, sections[index])
			}
		}
	}
	if registers.len == 0 {
		// Without a register table for this instruction set every name looks unknown.
		return
	}
	if sections.len > 3 {
		tc.check_inline_asm_clobbers(id, node, block, start, sections[3], registers)
	}
	if !header.is_raw && sections.len > 0 {
		mut aliases := map[string]bool{}
		for index in 1 .. 3 {
			if index < sections.len {
				for io in inline_asm_ios(block, sections[index]) {
					if io.alias.len > 0 {
						aliases[io.alias] = true
					}
				}
			}
		}
		tc.check_inline_asm_templates(id, node, block, start, sections[0], registers, aliases)
	}
}

// check_inline_asm_intel_ios rejects the operand constraints that a structured `intel`
// block cannot express, because compilers still format those placeholders as AT&T.
fn (mut tc TypeChecker) check_inline_asm_intel_ios(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange) {
	for io in inline_asm_ios(block, section) {
		if io.constraint.trim_left('=+&%*') == 'r' {
			continue
		}
		tc.record_error_at(.compile_error, 'constraint `${io.constraint}` is not supported for operands in structured `intel` assembly; use a register-only `r` constraint or a `raw` template with explicit operand modifiers', id, token.new_span(node.pos.id, base + io.start, base + io.end))
	}
}

// check_inline_asm_clobbers reports clobber list entries that name no known register.
fn (mut tc TypeChecker) check_inline_asm_clobbers(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, registers []string) {
	for word in inline_asm_words(block, section) {
		if util.asm_clobber_is_special(word.text) || word.text in registers {
			continue
		}
		mut message := 'unknown clobbered register `${word.text}`'
		if suggestion := util.closest_asm_register(word.text, registers) {
			message += '; did you mean `${suggestion}`?'
		}
		tc.record_error_at(.unknown_ident, message, id, token.new_span(node.pos.id, base + word.start, base + word.end))
	}
}

// check_inline_asm_templates reports operand names that are close enough to a register
// name to be a typo. Anything else can legitimately be a symbol or a label.
fn (mut tc TypeChecker) check_inline_asm_templates(id flat.NodeId, node flat.Node, block string, base int, section InlineAsmRange, registers []string, aliases map[string]bool) {
	labels := inline_asm_template_labels(block, section)
	for line in inline_asm_lines(block, section) {
		trimmed := block[line.start..line.end].trim_space()
		if trimmed.len == 0 || trimmed.ends_with(':') || trimmed.starts_with('.') {
			continue
		}
		// Only operands can name a register; the mnemonic itself never does.
		mut operand_start := inline_asm_skip_mnemonic(block, line)
		if block[line.start..operand_start].trim_space() in inline_asm_instruction_prefixes {
			operand_start = inline_asm_skip_mnemonic(block, InlineAsmRange{
				start: operand_start
				end: line.end
			})
		}
		for word in inline_asm_words(block, InlineAsmRange{ start: operand_start, end: line.end }) {
			if aliases[word.text] || word.text in labels || word.text in registers {
				continue
			}
			suggestion := util.closest_asm_register(word.text, registers) or { continue }
			tc.record_error_at(.unknown_ident, 'unknown register `${word.text}`; did you mean `${suggestion}`?', id, token.new_span(node.pos.id, base + word.start, base + word.end))
		}
	}
}

// inline_asm_mask_comments replaces every comment with spaces, keeping newlines and the
// length of source so that offsets into the result stay usable as source offsets.
fn inline_asm_mask_comments(source string) string {
	if !source.contains('/') {
		return source
	}
	mut out := []u8{cap: source.len}
	mut i := 0
	for i < source.len {
		c := source[i]
		if c in [`'`, `"`, `\``] {
			next := inline_asm_skip_quoted_text(source, i, source.len)
			out << source[i..next].bytes()
			i = next
			continue
		}
		comment_end := inline_asm_comment_end(source, i, source.len) or {
			out << c
			i++
			continue
		}
		for masked in i .. comment_end {
			out << if source[masked] == `\n` { `\n` } else { ` ` }
		}
		i = comment_end
	}
	return out.bytestr()
}

// inline_asm_section_ranges splits an assembly block body on the `;` separators that are
// not inside a string or character literal.
fn inline_asm_section_ranges(block string, start int, end int) []InlineAsmRange {
	mut ranges := []InlineAsmRange{}
	mut section_start := start
	mut i := start
	for i < end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, end)
			continue
		}
		if c == `;` {
			ranges << InlineAsmRange{
				start: section_start
				end: i
			}
			section_start = i + 1
		}
		i++
	}
	ranges << InlineAsmRange{
		start: section_start
		end: end
	}
	return ranges
}

// inline_asm_lines splits a section into lines.
fn inline_asm_lines(block string, section InlineAsmRange) []InlineAsmRange {
	mut lines := []InlineAsmRange{}
	mut line_start := section.start
	mut i := section.start
	for i < section.end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, section.end)
			continue
		}
		if c == `\n` {
			lines << InlineAsmRange{
				start: line_start
				end: i
			}
			line_start = i + 1
		}
		i++
	}
	lines << InlineAsmRange{
		start: line_start
		end: section.end
	}
	return lines
}

// inline_asm_skip_mnemonic returns the offset just past the first word of a line.
fn inline_asm_skip_mnemonic(block string, line InlineAsmRange) int {
	mut i := line.start
	for i < line.end && block[i].is_space() {
		i++
	}
	for i < line.end && !block[i].is_space() {
		i++
	}
	return i
}

// inline_asm_template_labels collects the `name:` labels a template section declares.
fn inline_asm_template_labels(block string, section InlineAsmRange) map[string]bool {
	mut labels := map[string]bool{}
	for line in inline_asm_lines(block, section) {
		trimmed := block[line.start..line.end].trim_space()
		if !trimmed.ends_with(':') {
			continue
		}
		name := trimmed#[..-1].trim_space()
		if inline_asm_is_ident(name) {
			labels[name] = true
		}
	}
	return labels
}

// inline_asm_words returns the identifiers in a range, skipping quoted text and anything
// that is part of a number.
fn inline_asm_words(block string, section InlineAsmRange) []InlineAsmWord {
	mut words := []InlineAsmWord{}
	mut i := section.start
	for i < section.end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, section.end)
			continue
		}
		if c.is_digit() {
			// Skip the whole literal so `0x1f` never looks like the identifier `x1f`.
			for i < section.end && (block[i].is_alnum() || block[i] == `_`) {
				i++
			}
			continue
		}
		if !inline_asm_is_ident_start(c) {
			i++
			continue
		}
		start := i
		for i < section.end && inline_asm_is_ident_char(block[i]) {
			i++
		}
		words << InlineAsmWord{
			text: block[start..i]
			start: start
			end: i
		}
	}
	return words
}

// inline_asm_ios parses the `[alias] "constraint" (expression) as alias` operands of an
// input or output section.
fn inline_asm_ios(block string, section InlineAsmRange) []InlineAsmIOSpan {
	mut ios := []InlineAsmIOSpan{}
	mut i := section.start
	for i < section.end {
		i = inline_asm_skip_blanks(block, i, section.end)
		if i >= section.end {
			break
		}
		start := i
		mut alias := ''
		if block[i] == `[` {
			close := inline_asm_index_of(block, i + 1, section.end, `]`) or { break }
			alias = block[i + 1..close].trim_space()
			i = inline_asm_skip_blanks(block, close + 1, section.end)
			if i >= section.end {
				break
			}
		}
		mut constraint := ''
		if block[i] != `(` {
			constraint_start := i
			for i < section.end && !block[i].is_space() && block[i] != `(` {
				i++
			}
			constraint = block[constraint_start..i].trim('"\'')
			i = inline_asm_skip_blanks(block, i, section.end)
		}
		if i >= section.end || block[i] != `(` {
			break
		}
		expr_start := i + 1
		expr_end := inline_asm_matching_paren(block, i, section.end) or { break }
		expr := block[expr_start..expr_end].trim_space()
		i = inline_asm_skip_blanks(block, expr_end + 1, section.end)
		mut end := expr_end + 1
		if i + 2 <= section.end && block[i..i + 2] == 'as'
			&& (i + 2 == section.end || block[i + 2].is_space()) {
			i = inline_asm_skip_blanks(block, i + 2, section.end)
			alias_start := i
			for i < section.end && inline_asm_is_ident_char(block[i]) {
				i++
			}
			if alias.len == 0 {
				alias = block[alias_start..i]
			}
			end = i
		} else if alias.len == 0 && inline_asm_is_ident(expr) {
			alias = expr
		}
		ios << InlineAsmIOSpan{
			constraint: constraint
			alias: alias
			start: start
			end: end
		}
	}
	return ios
}

fn inline_asm_skip_blanks(block string, start int, end int) int {
	mut i := start
	for i < end && block[i].is_space() {
		i++
	}
	return i
}

fn inline_asm_skip_quoted_text(source string, start int, end int) int {
	quote := source[start]
	mut i := start + 1
	for i < end {
		if source[i] == `\\` && i + 1 < end {
			i += 2
			continue
		}
		if source[i] == quote {
			return i + 1
		}
		i++
	}
	return end
}

// inline_asm_comment_end returns the offset just past a comment starting at start, or
// none when there is no comment there.
fn inline_asm_comment_end(source string, start int, end int) ?int {
	if start + 1 >= end || source[start] != `/` {
		return none
	}
	if source[start + 1] == `/` {
		mut i := start + 2
		for i < end && source[i] != `\n` {
			i++
		}
		return i
	}
	if source[start + 1] != `*` {
		return none
	}
	mut i := start + 2
	mut depth := 1
	for i < end && depth > 0 {
		if source[i] == `/` && i + 1 < end && source[i + 1] == `*` {
			depth++
			i += 2
			continue
		}
		if source[i] == `*` && i + 1 < end && source[i + 1] == `/` {
			depth--
			i += 2
			continue
		}
		i++
	}
	return i
}

fn inline_asm_matching_paren(block string, open int, end int) ?int {
	mut depth := 0
	mut i := open
	for i < end {
		c := block[i]
		if c in [`'`, `"`, `\``] {
			i = inline_asm_skip_quoted_text(block, i, end)
			continue
		}
		if c == `(` {
			depth++
		} else if c == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
		i++
	}
	return none
}

fn inline_asm_index_of(block string, start int, end int, needle u8) ?int {
	for i in start .. end {
		if block[i] == needle {
			return i
		}
	}
	return none
}

fn inline_asm_last_index_of(block string, start int, end int, needle u8) ?int {
	for i := end - 1; i >= start; i-- {
		if block[i] == needle {
			return i
		}
	}
	return none
}

fn inline_asm_is_ident_start(c u8) bool {
	return c == `_` || c.is_letter()
}

fn inline_asm_is_ident_char(c u8) bool {
	return inline_asm_is_ident_start(c) || c.is_digit()
}

fn inline_asm_is_ident(source string) bool {
	if source.len == 0 || !inline_asm_is_ident_start(source[0]) {
		return false
	}
	return source[1..].bytes().all(inline_asm_is_ident_char(it))
}
