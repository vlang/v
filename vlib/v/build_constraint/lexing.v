module build_constraint

// lexing:
enum BTokenKind {
	tfact   // linux, tinyc, prod etc
	tdefine // abc, gcboehm
	tor     // ||
	tand    // &&
	tnot    // !
	tparen_open
	tparen_close
	teof
}

struct Token {
	kind  BTokenKind
	value string
}

fn unexpected(c u8) IError {
	return error('unexpected character `${rune(c)}`')
}

fn new_token(kind BTokenKind, value string) Token {
	return Token{
		kind:  kind
		value: value
	}
}

fn new_op(kind BTokenKind) Token {
	return new_token(kind, '')
}

fn new_span(kind BTokenKind, mut span []u8) Token {
	t := new_token(kind, span.bytestr())
	span.clear()
	return t
}

fn lex(original string) ![]Token {
	mut res := []Token{}
	mut span := []u8{cap: original.len}
	mut op := []u8{}
	s := original.all_before('//')
	for c in s {
		match c {
			` `, `\t`, `\n` {}
			`(` {
				if span.len > 0 {
					res << new_span(.tfact, mut span)
				}
				res << new_op(.tparen_open)
			}
			`)` {
				if span.len > 0 {
					res << new_span(.tfact, mut span)
				}
				res << new_op(.tparen_close)
			}
			`&`, `|` {
				if span.len > 0 {
					res << new_span(.tfact, mut span)
				}
				op << c
				if op == [c, c] {
					op.clear()
					if c == `&` {
						res << new_op(.tand)
					} else if c == `|` {
						res << new_op(.tor)
					} else {
						return unexpected(c)
					}
				}
				if op.len == 2 {
					return unexpected(c)
				}
			}
			`?` {
				res << new_span(.tdefine, mut span)
			}
			`!` {
				res << new_op(.tnot)
				if span.len > 0 {
					return unexpected(c)
				}
			}
			else {
				if u8(c).is_alnum() || c in [`_`, `-`] {
					span << c
				} else {
					return unexpected(c)
				}
			}
		}
	}
	if span.len > 0 {
		res << new_span(.tfact, mut span)
	}
	res << new_op(.teof)
	return res
}
