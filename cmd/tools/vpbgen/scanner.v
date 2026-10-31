module main

import os

// TokenKind classifies a .proto token.
pub enum TokenKind {
	ident
	number
	str
	punct
	eof
}

// Token is one lexical unit of a .proto file, with the offset it started at so
// a diagnostic can point at a line.
pub struct Token {
pub mut:
	kind TokenKind = .eof
	text string
	pos  Pos
}

// Pos is a position in a .proto file.
pub struct Pos {
pub mut:
	line int
	col  int
}

// Scanner turns .proto text into tokens.
//
// It is hand-written rather than reusing the V compiler's own scanner: a .proto
// file is not V, its punctuation set is small, and a tool under `cmd/tools` must
// not depend on compiler internals.
pub struct Scanner {
pub mut:
	text  string
	pos   int
	line  int = 1
	col   int = 1
	start int
	// comments accumulates the text of every comment seen since it was last
	// drained, so the parser can attach a doc comment to the statement that
	// follows it.
	comments []string
}

// new_scanner returns a Scanner over `text`.
pub fn new_scanner(text string) &Scanner {
	return &Scanner{
		text: text
	}
}

// next returns the next token, skipping whitespace and both comment forms.
pub fn (mut s Scanner) next() !Token {
	s.skip_space()!
	s.start = s.pos
	// The position is recorded before the token is consumed, so a diagnostic
	// points at the token's first character rather than the one after it.
	here := s.here()
	if s.pos >= s.text.len {
		return Token{
			kind: .eof
			text: ''
			pos:  here
		}
	}
	b := s.text[s.pos]
	// An identifier may start with a letter or an underscore, and may continue
	// with digits. Proto keywords are identifiers as far as the lexer is
	// concerned; the parser gives them meaning.
	if b == `_` || b.is_letter() {
		return s.scan_ident(here)
	}
	if b.is_digit() || (b == `-` && s.peek(1).is_digit()) || (b == `-` && s.peek(1) == `.`) {
		return s.scan_number(here)
	}
	if b == `'` || b == `"` {
		return s.scan_string(here, b)
	}
	return s.scan_punct(here)
}

// here returns the current position.
pub fn (s &Scanner) here() Pos {
	return Pos{
		line: s.line
		col:  s.col
	}
}

// peek returns the byte `n` positions ahead, or 0 past the end.
pub fn (s &Scanner) peek(n int) u8 {
	i := s.pos + n
	if i < 0 || i >= s.text.len {
		return 0
	}
	return s.text[i]
}

// advance moves one byte forward, keeping the line and column counters honest.
fn (mut s Scanner) advance() u8 {
	b := s.text[s.pos]
	s.pos++
	if b == `\n` {
		s.line++
		s.col = 1
	} else {
		s.col++
	}
	return b
}

// scan_ident reads an identifier or keyword.
fn (mut s Scanner) scan_ident(here Pos) !Token {
	mut out := []u8{}
	for s.pos < s.text.len {
		b := s.text[s.pos]
		if b != `_` && !b.is_letter() && !b.is_digit() {
			break
		}
		out << s.advance()
	}
	return Token{
		kind: .ident
		text: out.bytestr()
		pos:  here
	}
}

// is_hex_digit reports whether `b` is a hexadecimal digit, which is what lets a
// number literal continue past `0x`.
pub fn is_hex_digit(b u8) bool {
	return b.is_digit() || (b >= `a` && b <= `f`) || (b >= `A` && b <= `F`)
}

// scan_number reads an integer or float literal. A leading `-` is part of the
// literal so a negative field number reaches the parser intact and can be
// diagnosed there rather than here.
fn (mut s Scanner) scan_number(here Pos) !Token {
	mut out := []u8{}
	if s.text[s.pos] == `-` {
		out << s.advance()
	}
	for s.pos < s.text.len {
		b := s.text[s.pos]
		if !b.is_digit() && b != `.` && b != `x` && b != `X` && !is_hex_digit(b) && b != `_` {
			break
		}
		// A `.` only continues the number when a digit follows, so `1.5.2` in a
		// qualified name still splits into a number and two dots.
		if b == `.` && !s.peek(1).is_digit() {
			break
		}
		out << s.advance()
	}
	return Token{
		kind: .number
		text: out.bytestr()
		pos:  here
	}
}

// scan_string reads a quoted string literal, keeping the quotes out of the text
// but honouring the usual backslash escapes.
fn (mut s Scanner) scan_string(here Pos, quote u8) !Token {
	s.advance() // opening quote
	mut out := []u8{}
	for s.pos < s.text.len {
		b := s.advance()
		if b == quote {
			return Token{
				kind: .str
				text: out.bytestr()
				pos:  here
			}
		}
		if b == `\\` && s.pos < s.text.len {
			next := s.advance()
			out << match next {
				`n` { u8(10) }
				`t` { u8(9) }
				`r` { u8(13) }
				`0` { u8(0) }
				else { next }
			}
			continue
		}
		out << b
	}
	return error('unterminated string starting at line ${s.line}')
}

// scan_punct reads a single punctuation character. Multi-character operators
// are not needed: .proto only ever uses these one at a time, and `[` `]` are
// always separate.
fn (mut s Scanner) scan_punct(here Pos) !Token {
	b := s.advance()
	if b in [`{`, `}`, `(`, `)`, `[`, `]`, `<`, `>`, `=`, `;`, `,`, `.`, `/`, `-`, `:`] {
		return Token{
			kind: .punct
			// A single byte becomes a one-character string through its array
			// form; `bytestr` is only for slices.
			text: [b].bytestr()
			pos:  here
		}
	}
	return error('unexpected character `${[b].bytestr()}` at line ${s.line}')
}

// skip_space consumes whitespace, `//` line comments, and `/* */` block
// comments. A block comment that is never closed is an error rather than a
// silent end of file, since that would surface as a baffling parse failure much
// later.
//
// Comments are collected rather than dropped: they become the doc comments on
// the generated declarations, which is the difference between generated code
// that explains itself and a wall of field names.
fn (mut s Scanner) skip_space() ! {
	for s.pos < s.text.len {
		b := s.text[s.pos]
		if b == ` ` || b == `\t` || b == `\r` || b == `\n` {
			s.advance()
			continue
		}
		if b == `/` && s.peek(1) == `/` {
			s.advance()
			s.advance()
			mut out := []u8{}
			for s.pos < s.text.len && s.text[s.pos] != `\n` {
				out << s.advance()
			}
			s.comments << out.bytestr().trim_space()
			continue
		}
		if b == `/` && s.peek(1) == `*` {
			s.advance()
			s.advance()
			mut out := []u8{}
			mut closed := false
			for s.pos < s.text.len {
				if s.text[s.pos] == `*` && s.peek(1) == `/` {
					s.advance()
					s.advance()
					closed = true
					break
				}
				out << s.advance()
			}
			if !closed {
				return error('unterminated block comment starting before line ${s.line}')
			}
			// Each line of a block comment becomes a line of its own, since the
			// emitter prefixes every comment line with `//` and a line it never
			// sees would land in the generated source as bare text.
			s.comments << block_comment_lines(out.bytestr())
			continue
		}
		return
	}
}

// block_comment_lines splits the text between `/*` and `*/` into trimmed lines.
//
// The leading `*` of the `/** ... */` style is dropped from each line, and so
// are the blank lines that style leaves at either end, so
//
//     /**
//      * Two lines.
//      * Of text.
//      */
//
// gives `Two lines.` and `Of text.`. A blank line in the middle is kept, since it
// separates paragraphs.
fn block_comment_lines(text string) []string {
	mut lines := []string{}
	for raw in text.split_into_lines() {
		mut line := raw.trim_space()
		if line.starts_with('*') {
			line = line[1..].trim_space()
		}
		lines << line
	}
	mut first := 0
	for first < lines.len && lines[first] == '' {
		first++
	}
	mut last := lines.len
	for last > first && lines[last - 1] == '' {
		last--
	}
	return lines[first..last]
}

// take_comments returns the comments collected since the last call and clears
// the buffer.
pub fn (mut s Scanner) take_comments() []string {
	out := s.comments
	s.comments = []string{}
	return out
}

// read_file_scanner returns a Scanner over the contents of `path`.
pub fn read_file_scanner(path string) !&Scanner {
	text := os.read_file(path)!
	return new_scanner(text)
}
