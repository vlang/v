module types

import v.pref
import v.scanner
import v.token

// A question of the mini-VLS protocol is about code that is being written, and
// that may not parse yet: `os.` with nothing after the dot, a call whose `(` is
// not closed yet. V1 parsed such code in its VLS mode, and answered from what it
// made of it. V3 completes the file that is asked about instead, before it
// parses it (see vls_complete_source). What it writes comes right after a token,
// at the end of a line or before a `)`, a `]`, a `,` or a `}` on it: every
// position keeps its line and its column, but those after it on its line.

// vls_member_placeholder is the name written after a dot that has no name after
// it: the member being written, which a completion request asks about.
const vls_member_placeholder = 'vlsmember'

// VlsSourceToken is a token of a file being completed: its kind, its byte span
// and its text.
struct VlsSourceToken {
	kind token.Token
	pos  int
	end  int
	lit  string
}

// VlsInsert is text that vls_complete_source writes at the byte `offset` of the
// file.
struct VlsInsert {
	offset int
	text   string
}

// vls_complete_source returns `source` completed where it is being written,
// as V1 parsed it in its VLS mode:
// - a dot that a name does not follow, as `x.` before `)` or before a keyword,
//   or that ends a line before a declaration, `x.` before `y := 1`, gets a name;
// - so does a dot that ends a line after a module name, `os.`, when the next
//   line starts with a module: the names of `os` are asked for;
// - a `(` or a `[` left open when a statement starts on a line of its own, as
//   `y := 1` after `f(`, or when a `}` ends the block around it, is closed at
//   the end of the line before.
// `source` itself is returned when it needs none of that.
pub fn vls_complete_source(source string) string {
	tokens := vls_source_tokens(source)
	modules := vls_module_names(tokens)
	mut inserts := []VlsInsert{}
	mut open := []int{} // the indexes of the open brackets
	for i, tok in tokens {
		if i > 0 && open.len > 0 && vls_on_new_line(source, tokens[i - 1], tok)
			&& vls_starts_statement(tokens, i) {
			vls_close_open_calls(tokens, mut open, tokens[i - 1].end, mut inserts)
		}
		match tok.kind {
			.lpar, .lsbr, .lcbr {
				open << i
			}
			.rpar, .rsbr {
				if open.len > 0 && vls_closes(tokens[open.last()].kind, tok.kind) {
					open.pop()
				}
			}
			.rcbr {
				// A `}` ends the block around the calls that are still open.
				if open.len > 0 && tokens[open.last()].kind != .lcbr && i > 0 {
					vls_close_open_calls(tokens, mut open, tokens[i - 1].end, mut inserts)
				}
				if open.len > 0 && tokens[open.last()].kind == .lcbr {
					open.pop()
				}
			}
			.dot {
				if vls_dot_lacks_name(source, tokens, i, modules) {
					inserts << VlsInsert{tok.end, vls_member_placeholder}
				}
			}
			else {}
		}
	}
	if inserts.len == 0 {
		return source
	}
	mut parts := []string{cap: 2 * inserts.len + 1}
	mut from := 0
	for insert in inserts {
		parts << source[from..insert.offset]
		parts << insert.text
		from = insert.offset
	}
	parts << source[from..]
	return parts.join('')
}

// vls_source_tokens returns the tokens of the V code of `source`: without its
// comments, the semicolons that end its lines, its `#` directives and its
// `asm` blocks, whose brackets need not pair.
fn vls_source_tokens(source string) []VlsSourceToken {
	mut s := scanner.new_scanner(&pref.Preferences{}, .normal)
	s.init(unsafe { nil }, source)
	mut tokens := []VlsSourceToken{}
	mut directive_end := -1
	mut in_asm := false
	mut asm_depth := 0
	// A token takes at least a byte, or ends a line; the bound only guards
	// against a scanner that would stop moving on.
	for _ in 0 .. 2 * source.len + 2 {
		tok := s.scan()
		if tok == .eof {
			break
		}
		if in_asm {
			if tok == .lcbr {
				asm_depth++
			} else if tok == .rcbr {
				asm_depth--
				in_asm = asm_depth > 0
			}
			continue
		}
		if tok == .semicolon || s.pos < directive_end {
			continue
		}
		if tok == .hash {
			directive_end = source.index_after('\n', s.pos) or { source.len }
			continue
		}
		if tok == .key_asm && (tokens.len == 0 || tokens.last().kind != .dot) {
			in_asm = true
			asm_depth = 0
			continue
		}
		tokens << VlsSourceToken{
			kind: tok
			pos:  s.pos
			end:  s.offset
			lit:  s.lit
		}
	}
	return tokens
}

// vls_module_names returns the names that stand for a module in the file of
// `tokens`: the alias of each import, or the last part of its path, and the
// last part of the name of the file's module.
fn vls_module_names(tokens []VlsSourceToken) []string {
	mut names := []string{}
	mut i := 0
	for i < tokens.len {
		kind := tokens[i].kind
		if kind !in [.key_import, .key_module] {
			i++
			continue
		}
		i++
		for {
			mut last := ''
			for i < tokens.len && tokens[i].kind == .name {
				last = tokens[i].lit
				i++
				if i < tokens.len && tokens[i].kind == .dot {
					i++
					continue
				}
				break
			}
			if kind == .key_import && i + 1 < tokens.len && tokens[i].kind == .key_as
				&& tokens[i + 1].kind == .name {
				last = tokens[i + 1].lit
				i += 2
			}
			if last != '' {
				names << last
			}
			// `import os, strings`
			if kind == .key_import && i < tokens.len && tokens[i].kind == .comma {
				i++
				continue
			}
			break
		}
	}
	return names
}

// vls_on_new_line reports whether `tok` is on a line after the one of `prev`.
fn vls_on_new_line(source string, prev VlsSourceToken, tok VlsSourceToken) bool {
	for i in prev.end .. tok.pos {
		if source[i] == `\n` {
			return true
		}
	}
	return false
}

// vls_dot_lacks_name reports whether the dot `tokens[i]` is one that code being
// written left without the name that comes after it (see vls_complete_source).
fn vls_dot_lacks_name(source string, tokens []VlsSourceToken, i int, modules []string) bool {
	if i == 0 || tokens[i - 1].kind !in [.name, .rpar, .rsbr, .rcbr, .string] {
		return false
	}
	if i + 1 >= tokens.len {
		return true
	}
	next := tokens[i + 1]
	if !vls_on_new_line(source, tokens[i], next) {
		// `println(x.)`, `[x.]`, `f(x., y)`, `x. }`
		return next.kind in [.rpar, .rsbr, .rcbr, .comma]
	}
	if next.kind != .name || vls_starts_statement(tokens, i + 1) {
		return true
	}
	// `os.` on the line before one that starts with a module, `os.\n\ttime.now()`:
	// the names of `os` are asked for, not a member of it named `time`.
	prev := tokens[i - 1]
	return prev.kind == .name && prev.lit in modules && next.lit in modules
		&& (i < 2 || tokens[i - 2].kind != .dot)
}

// vls_starts_statement reports whether `tokens[i]`, first on its line, starts a
// statement that cannot be part of an expression begun on a line before: a
// declaration, `a, mut b := f()`, a keyword that only starts a statement, or a
// `}` that ends a block.
fn vls_starts_statement(tokens []VlsSourceToken, i int) bool {
	tok := tokens[i]
	if tok.kind in [.rcbr, .key_return, .key_for, .key_defer, .key_break, .key_continue, .key_assert,
		.key_goto, .key_const, .key_enum, .key_interface, .key_import, .key_module, .key_pub,
		.key_global] {
		return true
	}
	mut j := i
	for j < tokens.len {
		if tokens[j].kind == .key_mut {
			j++
		}
		if j >= tokens.len || tokens[j].kind != .name {
			return false
		}
		j++
		if j < tokens.len && tokens[j].kind == .comma {
			j++
			continue
		}
		break
	}
	return j < tokens.len && tokens[j].kind == .decl_assign
}

// vls_closes reports whether the bracket `close` closes the bracket `open`.
fn vls_closes(open token.Token, close token.Token) bool {
	return (open == .lpar && close == .rpar) || (open == .lsbr && close == .rsbr)
		|| (open == .lcbr && close == .rcbr)
}

// vls_close_open_calls closes the `(` and the `[` that are open inside the
// innermost open `{`, at the byte `offset`.
fn vls_close_open_calls(tokens []VlsSourceToken, mut open []int, offset int, mut inserts []VlsInsert) {
	mut closers := ''
	for open.len > 0 {
		kind := tokens[open.last()].kind
		if kind == .lpar {
			closers += ')'
		} else if kind == .lsbr {
			closers += ']'
		} else {
			break
		}
		open.pop()
	}
	if closers != '' {
		inserts << VlsInsert{offset, closers}
	}
}
