module wit

pub enum TokKind {
	eof
	ident
	punct
	string
}

pub struct Token {
pub:
	kind TokKind
	val  string
	pos  int
}

pub struct Lexer {
	src         string
	mut     idx int
}

pub fn new_lexer(src string) Lexer {
	return Lexer{ src: src, idx: 0 }
}

pub fn (mut l Lexer) tokenize() ![]Token {
	mut toks := []Token{}
	for l.idx < l.src.len {
		c := l.src[l.idx]
		// whitespace
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			l.idx++
			continue
		}
		// line comment
		if c == `/` && l.idx + 1 < l.src.len && l.src[l.idx + 1] == `/` {
			for l.idx < l.src.len && l.src[l.idx] != `\n` { l.idx++ }
			continue
		}
		// block comment
		if c == `/` && l.idx + 1 < l.src.len && l.src[l.idx + 1] == `*` {
			l.idx += 2
			for l.idx + 1 < l.src.len && !(l.src[l.idx] == `*` && l.src[l.idx + 1] == `/`) {
				l.idx++
			}
			l.idx += 2
			continue
		}
		// string
		if c == `"` {
			start := l.idx
			l.idx++
			mut s := ''
			for l.idx < l.src.len && l.src[l.idx] != `"` {
				if l.src[l.idx] == `\\` {
					l.idx++
					if l.idx < l.src.len {
						s += l.src[l.idx..l.idx + 1]
						l.idx++
					}
					continue
				}
				s += l.src[l.idx..l.idx + 1]
				l.idx++
			}
			if l.idx >= l.src.len { return error('unterminated string at ${start}') }
			l.idx++ // closing
			toks << Token{ kind: .string, val: s, pos: start }
			continue
		}
		// punctuation / multi
		if c == `-` && l.idx + 1 < l.src.len && l.src[l.idx + 1] == `>` {
			toks << Token{ kind: .punct, val: '->', pos: l.idx }
			l.idx += 2
			continue
		}
		if c == `{` || c == `}` || c == `(` || c == `)` || c == `[` || c == `]`
			|| c == `,` || c == `:` || c == `;` || c == `=` || c == `<` || c == `>`
			|| c == `.` || c == `/` || c == `*` || c == `@` {
			toks << Token{ kind: .punct, val: rune(c).str(), pos: l.idx }
			l.idx++
			continue
		}
		// ident (letters, digits, _, -)
		if is_ident_start(c) {
			start := l.idx
			for l.idx < l.src.len {
				ch := l.src[l.idx]
				if ch == `-` && l.idx + 1 < l.src.len && l.src[l.idx + 1] == `>` {
					break
				}
				if is_ident_char(ch) {
					l.idx++
				} else {
					break
				}
			}
			toks << Token{ kind: .ident, val: l.src[start..l.idx], pos: start }
			continue
		}
		return error('unexpected char ${c} at ${l.idx}')
	}
	toks << Token{ kind: .eof, val: '', pos: l.src.len }
	return toks
}

fn is_ident_start(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
		|| (c >= `0` && c <= `9`)
}

fn is_ident_char(c u8) bool {
	return is_ident_start(c) || (c >= `0` && c <= `9`) || c == `-`
}
