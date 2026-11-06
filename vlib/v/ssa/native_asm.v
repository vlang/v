// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module ssa

import v.pref

// Constaints the SSA native emitters can lower, plus the instruction sets they
// accept. The builder installs asm blocks for these filters only; everything
// else falls through to the parser's diagnostic so the failure is a compile
// error with source position, never a silently dropped statement.
pub const native_asm_arches = ['amd64', 'arm64']

pub const native_asm_backends = ['x64', 'arm64', 'native']

// parse_native_inline_asm lowers the preserved raw source of an `asm` block
// into an InlineAsmSpec. The grammar mirrors the C backend's parser in
// v.gen.c.stmt (sections split on unquoted `;`, `[alias] constraint (expr)`
// operand entries): it stays text-based, because V assembly is line-sensitive
// and cannot be reconstructed from the token stream alone.
pub fn parse_native_inline_asm(source string) ?InlineAsmSpec {
	open := source.index_u8(`{`)
	close := source.last_index('}') or { return none }
	if open < 0 || close <= open {
		return none
	}
	header := strip_inline_asm_comments(source[..open]).fields()
	mut arch := ''
	mut is_volatile := false
	mut is_raw := false
	mut is_intel := false
	for word in header {
		if word == 'asm' {
			continue
		}
		if word == 'volatile' {
			is_volatile = true
			continue
		}
		if word == 'goto' {
			return none // asm goto stays C-backend-only
		}
		if arch.len > 0 && word == 'raw' {
			return none // verbatim templates stay C-backend-only
		}
		if arch.len > 0 && word == 'intel' {
			return none // intel-syntax templates stay C-backend-only
		}
		if arch == '' {
			arch = word
		}
	}
	if pref.normalized_arch(arch) != arch {
		arch = pref.normalized_arch(arch)
	}
	if arch !in native_asm_arches {
		return none
	}
	body := strip_inline_asm_comments(source[open + 1..close])
	sections := split_inline_asm_sections(body)
	mut insts := []string{}
	if sections.len > 0 {
		for line in sections[0].split_into_lines() {
			trimmed := line.trim_space()
			if trimmed.len > 0 {
				insts << trimmed
			}
		}
	}
	mut clobbered := []string{}
	if sections.len > 3 {
		for name in sections[3].replace(',', ' ').fields() {
			clobbered << name
		}
	}
	// The parser's .asm_stmt children are the parenthesized operand expressions
	// in source order. Template operand entries walk the same order: outputs
	// first, then inputs, so a running child index pairs each io entry with
	// its expression child.
	mut ios := []InlineAsmIO{}
	mut next_child := 0
	if sections.len > 1 {
		section_ios := parse_inline_asm_ios(sections[1])
		for io in section_ios {
			ios << InlineAsmIO{
				alias:       io.alias
				constraint:  io.constraint
				child_index: next_child
			}
			next_child++
		}
	}
	if sections.len > 2 {
		section_ios := parse_inline_asm_ios(sections[2])
		for io in section_ios {
			ios << InlineAsmIO{
				alias:       io.alias
				constraint:  io.constraint
				child_index: next_child
			}
			next_child++
		}
	}
	return InlineAsmSpec{
		arch:        arch
		is_volatile: is_volatile
		is_intel:    is_intel
		is_raw:      is_raw
		insts:       insts
		ios:         ios
		clobbered:   clobbered
	}
}

// asm_output_alias finds the declared alias for the first output operand, the
// value an inline_asm instruction defines.
pub fn (spec &InlineAsmSpec) asm_output_alias() string {
	if spec.ios.len > 0 {
		return spec.ios[0].alias
	}
	return ''
}

// asm_input_start returns the index of the first input operand in spec.ios.
pub fn (spec &InlineAsmSpec) asm_input_start() int {
	for i, io in spec.ios {
		if !io.constraint.starts_with('=') {
			return i
		}
	}
	return spec.ios.len
}

// asm_input_count returns how many input operands spec.ios declares. The builder
// stores that many input values before the secondary-output pointers, so it is
// the offset of the first pointer operand; asm_input_start is the output count.
pub fn (spec &InlineAsmSpec) asm_input_count() int {
	mut n := 0
	for io in spec.ios {
		if !io.constraint.starts_with('=') {
			n++
		}
	}
	return n
}

// split_inline_asm_sections splits the block body on unquoted `;` separators,
// the section structure `instructions ; outputs ; inputs ; clobbers` follows.
// The comment-stripped body keeps quotes balanced, so a single quote state
// machine is enough here.
fn split_inline_asm_sections(source string) []string {
	mut sections := []string{}
	mut start := 0
	mut i := 0
	mut quote := u8(0)
	for i < source.len {
		c := source[i]
		if quote != 0 {
			if c == `\\` && i + 1 < source.len {
				i += 2
				continue
			}
			if c == quote {
				quote = 0
			}
			i++
			continue
		}
		if c in [`'`, `"`] {
			quote = c
			i++
			continue
		}
		if c == `;` {
			sections << source[start..i]
			start = i + 1
		}
		i++
	}
	sections << source[start..]
	return sections
}

// parse_inline_asm_ios walks `constraint (expr)` and `[alias] (expr)` operand
// entries in one section. Only the constraint text and the alias name matter
// for the native lowering; the expression body stays with the .asm_stmtChild
// node at the entry's ordinal position.
fn parse_inline_asm_ios(source string) []InlineAsmIO {
	mut ios := []InlineAsmIO{}
	mut i := 0
	for i < source.len {
		for i < source.len && source[i].is_space() {
			i++
		}
		if i >= source.len {
			break
		}
		// GNU's passthrough form: `[alias] "constraint" (expression)`. Only the
		// constraint text and the alias name matter for the native lowering; the
		// expression body stays on the .asm_stmt child node at the entry's
		// ordinal position.
		mut named_alias := ''
		if source[i] == `[` {
			close := source.index_after(']', i + 1) or { break }
			named_alias = source[i + 1..close].trim_space()
			i = close + 1
			for i < source.len && source[i].is_space() {
				i++
			}
			if i >= source.len {
				break
			}
		}
		mut constraint := 'r'
		if source[i] != `(` {
			constraint_start := i
			for i < source.len && !source[i].is_space() && source[i] != `(` {
				i++
			}
			constraint = strip_inline_asm_quotes(source[constraint_start..i])
			for i < source.len && source[i].is_space() {
				i++
			}
		}
		if i >= source.len || source[i] != `(` {
			break
		}
		i++
		expr_start := i
		mut depth := 1
		mut quote := u8(0)
		for i < source.len && depth > 0 {
			c := source[i]
			if quote != 0 {
				if c == `\\` && i + 1 < source.len {
					i += 2
					continue
				}
				if c == quote {
					quote = 0
				}
				i++
				continue
			}
			if c in [`'`, `"`] {
				quote = c
				i++
				continue
			}
			if c == `(` {
				depth++
			} else if c == `)` {
				depth--
			}
			i++
		}
		if expr_start >= i {
			break
		}
		// i sits after the closing ')'; trim spaces off the expr body.
		mut expr_end := i - 2
		for expr_end >= expr_start && source[expr_end].is_space() {
			expr_end--
		}
		expr_text := if expr_end >= expr_start { source[expr_start..expr_end + 1] } else { '' }
		mut alias := if named_alias.len > 0 {
			named_alias
		} else if inline_asm_is_ident(expr_text) {
			expr_text
		} else {
			''
		}
		if named_alias.len == 0 && i + 2 <= source.len && source[i..i + 2] == 'as'
			&& (i + 2 == source.len || source[i + 2].is_space()) {
			i += 2
			for i < source.len && source[i].is_space() {
				i++
			}
			alias_start := i
			for i < source.len && inline_asm_ident_char(source[i]) {
				i++
			}
			alias = source[alias_start..i].trim_space()
		}
		ios << InlineAsmIO{
			alias:      alias
			constraint: constraint
		}
	}
	return ios
}

// inline_asm_is_ident reports whether s is a bare V identifier, so an operand
// entry `(scn)` aliases itself while `(a + b)` stays nameless.
fn inline_asm_is_ident(s string) bool {
	if s.len == 0 {
		return false
	}
	if s[0].is_digit() {
		return false
	}
	for c in s.bytes() {
		if !c.is_alnum() && c != `_` {
			return false
		}
	}
	return true
}

fn inline_asm_ident_char(c u8) bool {
	return c.is_alnum() || c == `_`
}

// strip_inline_asm_comments removes `//` line comments and `/* */` block
// comments from the preserved assembly source, keeping string literals intact.
fn strip_inline_asm_comments(source string) string {
	mut out := []u8{cap: source.len}
	mut i := 0
	mut quote := u8(0)
	mut line_comment := false
	mut block_comment := false
	for i < source.len {
		c := source[i]
		if line_comment {
			if c == `\n` {
				line_comment = false
				out << c
			}
			i++
			continue
		}
		if block_comment {
			if c == `*` && i + 1 < source.len && source[i + 1] == `/` {
				block_comment = false
				i += 2
				continue
			}
			i++
			continue
		}
		if quote != 0 {
			if c == `\\` && i + 1 < source.len {
				out << c
				out << source[i + 1]
				i += 2
				continue
			}
			if c == quote {
				quote = 0
			}
			out << c
			i++
			continue
		}
		if c in [`'`, `"`] {
			quote = c
			out << c
			i++
			continue
		}
		if c == `/` && i + 1 < source.len && source[i + 1] == `/` {
			line_comment = true
			i += 2
			continue
		}
		if c == `/` && i + 1 < source.len && source[i + 1] == `*` {
			block_comment = true
			i += 2
			continue
		}
		out << c
		i++
	}
	return out.bytestr()
}

// strip_inline_asm_quotes removes surrounding double/single quote pairs from
// a token, the way written-out constraints like `"a"` reach the parser.
fn strip_inline_asm_quotes(s string) string {
	t := s.trim_space()
	if t.len >= 2 {
		if t[0] == `"` && t[t.len - 1] == `"` {
			return t[1..t.len - 1]
		}
		if t[0] == `'` && t[t.len - 1] == `'` {
			return t[1..t.len - 1]
		}
	}
	return t
}
