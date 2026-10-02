module json5

// Parser turns a stream of JSON5 tokens into an `Any` tree.
pub struct Parser {
mut:
	scanner &Scanner
	tok     Token
	started bool
}

// new_parser creates a Parser over `text`.
pub fn new_parser(text string) &Parser {
	return &Parser{
		scanner: new_scanner(text)
	}
}

// advance reads the next token into the parser.
fn (mut p Parser) advance() ! {
	p.tok = p.scanner.next()!
	p.started = true
}

// current returns the token the parser is positioned on.
fn (p &Parser) current() Token {
	return p.tok
}

// expect consumes the current token when it has kind `kind`, and returns an
// error naming `what` otherwise.
fn (mut p Parser) expect(kind TokenKind, what string) ! {
	if p.tok.kind != kind {
		return p.unexpected(what)
	}
	p.advance()!
}

// unexpected builds a ParseError describing the token the parser stopped on.
fn (p &Parser) unexpected(what string) &ParseError {
	tok := p.tok
	if !p.started || tok.kind == .eof {
		return syntax_error('unexpected end of input, expected ${what}', tok.pos.line,
			tok.pos.col)
	}
	mut desc := tok.lit
	if desc == '' {
		desc = tok.kind.str()
	}
	return syntax_error('unexpected `${desc}`, expected ${what}', tok.pos.line, tok.pos.col)
}

// parse reads a single JSON5 value and checks that nothing but trivia follows.
pub fn (mut p Parser) parse() !Any {
	p.advance()!
	value := p.parse_value()!
	if p.tok.kind != .eof {
		return p.unexpected('end of input')
	}
	return value
}

// parse_value reads any JSON5 value.
fn (mut p Parser) parse_value() !Any {
	match p.tok.kind {
		.lcbr {
			return p.parse_object()
		}
		.lsbr {
			return p.parse_array()
		}
		.str {
			value := p.tok.lit
			p.advance()!
			return value
		}
		.number {
			value := Number{
				text: p.tok.lit
			}
			p.advance()!
			return value
		}
		.bool {
			value := p.tok.lit == 'true'
			p.advance()!
			return value
		}
		.null {
			p.advance()!
			return Null{}
		}
		.infinity, .nan {
			value := Number{
				text: p.tok.lit
			}
			p.advance()!
			return value
		}
		else {
			return p.unexpected('a value')
		}
	}
}

// parse_object reads `{ key: value, ... }`, allowing a trailing comma and
// unquoted keys.
fn (mut p Parser) parse_object() !Any {
	p.advance()! // consume `{`
	mut members := map[string]Any{}
	if p.tok.kind == .rcbr {
		p.advance()!
		return members
	}
	for {
		key := p.parse_key()!
		p.expect(.colon, '`:` after the key `${key}`')!
		members[key] = p.parse_value()!
		match p.tok.kind {
			.comma {
				p.advance()!
				// A comma may be followed by the closing brace; JSON5 allows the
				// trailing form.
				if p.tok.kind == .rcbr {
					p.advance()!
					return members
				}
			}
			.rcbr {
				p.advance()!
				return members
			}
			else {
				return p.unexpected('`,` or `}`')
			}
		}
	}
}

// parse_key reads an object key, quoted or bare.
fn (mut p Parser) parse_key() !string {
	match p.tok.kind {
		.str {
			key := p.tok.lit
			p.advance()!
			return key
		}
		.ident, .number, .bool, .null, .infinity, .nan {
			// `Infinity`, `NaN`, `true`, `false` and `null` are valid bare keys in
			// JSON5, as is any number-looking identifier such as `0x10`.
			key := p.tok.lit
			p.advance()!
			return key
		}
		else {
			return p.unexpected('an object key')
		}
	}
}

// parse_array reads `[ value, ... ]`, allowing a trailing comma.
fn (mut p Parser) parse_array() !Any {
	p.advance()! // consume `[`
	mut items := []Any{}
	if p.tok.kind == .rsbr {
		p.advance()!
		return items
	}
	for {
		items << p.parse_value()!
		match p.tok.kind {
			.comma {
				p.advance()!
				// A comma may be followed by the closing bracket; JSON5 allows the
				// trailing form.
				if p.tok.kind == .rsbr {
					p.advance()!
					return items
				}
			}
			.rsbr {
				p.advance()!
				return items
			}
			else {
				return p.unexpected('`,` or `]`')
			}
		}
	}
}
