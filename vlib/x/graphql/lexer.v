module graphql

pub struct Lexer {
pub:
	source Source
mut:
	line_start int
	line       int
	last_token &Token
	token      &Token
}

fn Lexer.new(source Source) Lexer {
	start_of_file_token := &Token{
		kind: TokenKind.sof
		start: 0
		end: 0
		line: 0
		column: 0
	}

	return Lexer{
		source: source
		last_token: start_of_file_token
		token: start_of_file_token
		line: 1
		line_start: 0
	}
}

// Advances the token stream to the next non-ignored token.
fn (mut lexer Lexer) advance() &Token {
	lexer.last_token = lexer.token
	token := lexer.lookahead()
	// println('..advancing from ${lexer.token.kind} to ${token.kind}')

	lexer.token = token

	return token
}

// Looks ahead and returns the next non-ignored token,
// but does not change the state of Lexer.
fn (mut lexer Lexer) lookahead() &Token {
	mut token := lexer.token

	if token.kind != TokenKind.eof {
		for {
			if mut next := token.next {
				token = next
			} else {
				// println('char ends at ${token.end}')
				mut next_token := read_next_token(mut lexer, token.end) or { panic(err) }

				// println('...<< prev token ${token.kind}')
				// println('...>> next token ${next_token.kind}')
				// println('...>> next token VALUE ${next_token.value}')
				token.next = next_token
				next_token.prev = token
				token = next_token
			}

			// Exits loop
			if token.kind != TokenKind.comment {
				break
			}
		}
	}

	return token
}

fn read_next_token(mut lexer Lexer, start int) !&Token {
	body := lexer.source.body

	mut body_length := body.len
	mut position := start

	// println('==== BODY_LENGTH ${body_length}')

	for {
		// println('==== POSITION ${position}')
		// println('==== LINE_START ${lexer.line_start}, COLUMN ${lexer.token.column}')
		if position >= body_length {
			// println('EOF reached')
			break
		}

		code := body[position]

		// println('code : "${code.ascii_str()}"')

		match code {
			// Ignored ::
			//   - UnicodeBOM
			//   - WhiteSpace
			//   - LineTerminator
			//   - Comment
			//   - Comma
			//
			// UnicodeBOM :: "Byte Order Mark (U+FEFF)"
			//
			// WhiteSpace ::
			//   - "Horizontal Tab (U+0009)"
			//   - "Space (U+0020)"
			//
			// Comma :: ,
			0xfeff, 0x0009, 0x0020, 0x002c {
				position += 1
				continue
			}
			// LineTerminator ::
			//   - "New Line (U+000A)"
			//   - "Carriage Return (U+000D)" [lookahead != "New Line (U+000A)"]
			//   - "Carriage Return (U+000D)" "New Line (U+000A)"
			// \n
			0x000a {
				position += 1
				lexer.line += 1
				lexer.line_start = position
				continue
			}
			// \r
			0x000d {
				if body[position + 1] == 0x000a {
					position += 2
				} else {
					position += 1
				}

				lexer.line += 1
				lexer.line_start = position
				continue
			}
			// # (comment)
			0x0023 {
				return lexer.read_comment(position)
			}
			// Token ::
			//   - Punctuator
			//   - Name
			//   - IntValue
			//   - FloatValue
			//   - StringValue
			//
			// Punctuator :: one of ! $ & ( ) ... : = @ [ ] { | }
			0x0021 { // !
				return create_token(lexer, TokenKind.bang, position, position + 1, none)
			}
			0x0024 { // $
				return create_token(lexer, TokenKind.dollar, position, position + 1, none)
			}
			0x0026 { // &
				return create_token(lexer, TokenKind.amp, position, position + 1, none)
			}
			0x0028 { // (
				return create_token(lexer, TokenKind.paren_l, position, position + 1,
					none)
			}
			0x0029 { // )
				return create_token(lexer, TokenKind.paren_r, position, position + 1,
					none)
			}
			0x002e { // .
				if body[position + 1] == 0x002e && body[position + 2] == 0x002e {
					return create_token(lexer, TokenKind.spread, position, position + 3,
						none)
				}
				break
			}
			0x003a { // :
				return create_token(lexer, TokenKind.colon, position, position + 1, none)
			}
			0x003d { // =
				return create_token(lexer, TokenKind.equals, position, position + 1, none)
			}
			0x0040 { // @
				return create_token(lexer, TokenKind.at, position, position + 1, none)
			}
			0x005b { // [
				return create_token(lexer, TokenKind.bracket_l, position, position + 1,
					none)
			}
			0x005d { // ]
				return create_token(lexer, TokenKind.bracket_r, position, position + 1,
					none)
			}
			0x007b { // {
				return create_token(lexer, TokenKind.brace_l, position, position + 1,
					none)
			}
			0x007c { // |
				return create_token(lexer, TokenKind.pipe, position, position + 1, none)
			}
			0x007d { // }
				return create_token(lexer, TokenKind.brace_r, position, position + 1,
					none)
			}
			0x003f { // ?
				return create_token(lexer, TokenKind.question_mark, position, position + 1,
					none)
			}
			0x0022 { // "

				if body[position + 1] == 0x0022 && body[position + 2] == 0x0022 {
					// println('matched comment')

					return lexer.read_block_string(position)
				}
				return lexer.read_string(position)
			}
			else {
				// Do nothing
			}
		}

		// IntValue | FloatValue (Digit | -)
		if is_digit(code) || code == 0x002d {
			return lexer.read_number(position, code) or { panic(err) }
		}

		// Name
		if is_name_start(code) {
			name_token := lexer.read_name(position)
			// println('code (NAME) : ${name_token.value}')
			return name_token
		}

		return error('unexpected character')
	}

	return create_token(lexer, TokenKind.eof, body_length, body_length, none)
}

fn (lexer Lexer) read_name(start int) &Token {
	body := lexer.source.body
	body_length := body.len

	mut position := start + 1

	for {
		if position >= body_length {
			break
		}

		code := body[position]
		if is_name_continue(code) {
			// println('name continuing ${body[start..position]}')
			position += 1
		} else {
			break
		}
	}

	return create_token(lexer, TokenKind.name, start, position, body[start..position])
}

fn (lexer Lexer) read_number(start int, first_code u8) !&Token {
	body := lexer.source.body

	mut code := first_code
	mut position := start
	mut is_float := false

	// NegativeSign (-)
	if code == 0x002d {
		position += 1
		code = body[position]
	}

	// Zero(0)
	if code == 0x0030 {
		position += 1

		code = body[position]

		if is_digit(code) {
			return error('Invalid number, unexpected digit after 0')
		}
	} else {
		position = lexer.read_digits(position, code)
		code = body[position]
	}

	// Full stop (.)
	if code == 0x002e {
		is_float = true

		position += 1
		code = body[position]
		position = lexer.read_digits(position, code)
		code = body[position]
	}

	if code == 0x0045 || code == 0x0065 {
		is_float = true
		position += 1
		code = body[position]

		// + -
		if code == 0x002b || code == 0x002d {
			position += 1
			code = body[position]
		}

		position = lexer.read_digits(position, code)
		code = body[position]
	}

	// Numbers cannot be followed by . or NameStart
	if code == 0x002e || is_name_start(code) {
		return error('INvalid number')
	}

	kind := if is_float {
		TokenKind.float
	} else {
		TokenKind.integer
	}
	return create_token(lexer, kind, start, position, body[start..position])
}

fn (lexer Lexer) read_digits(start int, first_code u8) int {
	if !is_digit(first_code) {
		error('Invalid number')
	}

	body := lexer.source.body
	mut position := start + 1 // +1 to skip first first_code

	for {
		if !is_digit(body[position]) {
			break
		}
		position += 1
	}

	return position
}

fn (lexer Lexer) read_string(start int) &Token {
	body := lexer.source.body
	body_length := body.len

	mut position := start + 1
	mut chunk_start := position

	mut value := ''

	for {
		if position >= body_length {
			break
		}

		code := body[position]

		// Closing Quote (")
		if code == 0x0022 {
			value += body[chunk_start..position]
			return create_token(lexer, TokenKind.string_value, start, position + 1, value)
		}

		// Escape Sequence (\)
		if code == 0x005c {
			value += body[chunk_start..position]

			escape := if body[position + 1] == 0x0075 {
				if body[position + 2] == 0x007b {
					read_escaped_unicode_variable_width(lexer, position)
				} else {
					read_escaped_unicode_fixed_width(lexer, position)
				}
			} else {
				read_escaped_character(lexer, position)
			}
			value += escape.value
			position += escape.size
			chunk_start += position
			continue
		}

		// LineTerminator (\n | \r)
		if code == 0x000a || code == 0x000d {
			break
		}

		// SourceCharacter
		if is_unicode_scalar_value(code) {
			position++
		} else if is_supplementary_code_point(body, position) {
			position += 2
		} else {
			panic('invalid char')
		}
	}

	panic('unterminated string')
}

fn (mut lexer Lexer) read_comment(start int) &Token {
	body := lexer.source.body
	body_length := body.len

	mut position := start + 1

	for {
		if position >= body_length {
			break
		}

		code := body[position]

		// LineTerminator (\n | \r)
		if code == 0x000a || code == 0x000d {
			break
		}

		// SourceCharacter
		if is_unicode_scalar_value(code) {
			position++
		} else if is_supplementary_code_point(body, position) {
			position += 2
		} else {
			break
		}
	}

	return create_token(lexer, TokenKind.comment, start, position, body[start + 1..position])
}

fn (mut lexer Lexer) read_block_string(start int) &Token {
	body := lexer.source.body
	body_length := body.len

	mut line_start := lexer.line_start

	mut position := start + 3
	mut chunk_start := position
	mut current_line := ''

	mut block_lines := []string{}

	for {
		if position >= body_length {
			break
		}

		code := body[position]

		// Closing Triple-Quote (""")
		if code == 0x0022 && body[position + 1] == 0x0022 && body[position + 2] == 0x0022 {
			current_line += body[chunk_start..position]
			block_lines << current_line

			// println('dedented')
			// println(dedent_block_string_lines(block_lines).join('\n'))
			token := create_token(lexer, TokenKind.block_string, start, position + 3,
				dedent_block_string_lines(block_lines).join('\n'))

			// println('LINE ${block_lines.len - 1}')
			lexer.line += block_lines.len - 1
			lexer.line_start = line_start

			return token
		}

		// Escaped Triple-Quote (\""")
		if code == 0x005c && body[position + 1] == 0x0022 && body[position + 2] == 0x0022
			&& body[position + 3] == 0x0022 {
			current_line += body[chunk_start..position]
			// skip only slash
			chunk_start = position + 1

			position += 4
			continue
		}

		// LineTerminator
		if code == 0x000a || code == 0x000d {
			current_line += body[chunk_start..position]
			block_lines << current_line

			if code == 0x000d && body[position + 1] == 0x000a {
				position += 2
			} else {
				position++
			}

			current_line = ''
			chunk_start = position
			line_start = position

			continue
		}

		if is_unicode_scalar_value(code) {
			position += 1
		} else if is_supplementary_code_point(body, position) {
			position += 2
		} else {
			error('Invalid character within string')
		}
	}

	panic('unterminated string')
}

fn create_token(lexer Lexer, kind TokenKind, start int, end int, value ?string) &Token {
	line := lexer.line
	col := 1 + start - lexer.line_start

	return &Token{
		kind: kind
		start: start
		end: end
		line: line
		column: col
		value: value
	}
}

fn is_punctuator_token_kind(kind TokenKind) bool {
	return match kind {
		.bang, .question_mark, .dollar, .amp, .paren_l, .paren_r, .spread, .colon, .equals, .at,
		.bracket_l, .bracket_r, .brace_l, .pipe, .brace_r {
			true
		}
		else {
			false
		}
	}
}

struct EscapeSequence {
	value string
	size  int
}

fn read_escaped_unicode_variable_width(lexer Lexer, position int) EscapeSequence {
	body := lexer.source.body
	mut point := 0
	mut size := 3

	for {
		// Cannot be larger than 12 chars (\u{00000000}).
		if size > 12 {
			break
		}

		size++
		code := body[position + size]

		if code == 0x007d {
			if size < 5 || !is_unicode_scalar_value(point) {
				break
			}

			return EscapeSequence{
				value: u8(point).ascii_str()
				size: size
			}
		}

		point = int((u32(point) << 4) | u32(read_hex_digit(code)))
		if point < 0 {
			break
		}
	}

	panic('Invalid unicode escape sequence')
}

fn read_escaped_unicode_fixed_width(lexer Lexer, position int) EscapeSequence {
	body := lexer.source.body
	code := read16_bit_hex_code(body, position + 2)

	if is_unicode_scalar_value(code) {
		return EscapeSequence{
			value: u8(code).ascii_str()
			size: 6
		}
	}

	// GraphQL allows JSON-style surrogate pair escape sequences, but only when
	// a valid pair is formed.
	if is_leading_surrogate(code) {
		// \u
		if body[position + 6] == 0x005c && body[position + 7] == 0x0075 {
			trailing_code := read16_bit_hex_code(body, position + 8)

			if is_trailing_surrogate(trailing_code) {
				return EscapeSequence{
					value: [u8(code), u8(trailing_code)].bytestr()
					size: 12
				}
			}
		}
	}

	panic('Invalid Unicode escape sequence:')
}

// | Escaped Character | Code Point | Character Name               |
// | ----------------- | ---------- | ---------------------------- |
// | `"`               | U+0022     | double quote                 |
// | `\`               | U+005C     | reverse solidus (back slash) |
// | `/`               | U+002F     | solidus (forward slash)      |
// | `b`               | U+0008     | backspace                    |
// | `f`               | U+000C     | form feed                    |
// | `n`               | U+000A     | line feed (new line)         |
// | `r`               | U+000D     | carriage return              |
// | `t`               | U+0009     | horizontal tab               |
fn read_escaped_character(lexer Lexer, position int) EscapeSequence {
	body := lexer.source.body
	code := body[position + 1]

	match code {
		0x0022 { // ""
			return EscapeSequence{
				value: `"`.str()
				size: 2
			}
		}
		0x005c { // \
			return EscapeSequence{
				value: `\\`.str()
				size: 2
			}
		}
		0x002f { // /
			return EscapeSequence{
				value: `/`.str()
				size: 2
			}
		}
		0x0062 { // b
			return EscapeSequence{
				value: `b`.str()
				size: 2
			}
		}
		0x0066 { // f
			return EscapeSequence{
				value: `f`.str()
				size: 2
			}
		}
		0x006e { // n
			return EscapeSequence{
				value: `n`.str()
				size: 2
			}
		}
		0x0072 { // r
			return EscapeSequence{
				value: `r`.str()
				size: 2
			}
		}
		0x0074 { // t
			return EscapeSequence{
				value: `t`.str()
				size: 2
			}
		}
		else {
			panic('Invalid character escape sequence')
		}
	}
}

// Reads four hexadecimal characters and returns the positive integer that 16bit
// hexadecimal string represents. For example, "000f" will return 15, and "dead"
// will return 57005.
//
// Returns a negative number if any char was not a valid hexadecimal digit.
fn read16_bit_hex_code(body string, position int) int {
	d0 := read_hex_digit(body[position])
	d1 := read_hex_digit(body[position + 1])
	d2 := read_hex_digit(body[position + 2])
	d3 := read_hex_digit(body[position + 3])
	if d0 < 0 || d1 < 0 || d2 < 0 || d3 < 0 {
		return -1
	}
	return int((u32(d0) << 12) | (u32(d1) << 8) | (u32(d2) << 4) | u32(d3))
}

fn read_hex_digit(code u8) int {
	// 0-9
	if code >= 0x0030 && code <= 0x0039 {
		return code - 0x0030
	}

	// A-F
	if code >= 0x0041 && code <= 0x0046 {
		return code - 0x0037
	}

	// a-f
	if code >= 0x0061 && code <= 0x0066 {
		return code - 0x0057
	}

	return -1
}
