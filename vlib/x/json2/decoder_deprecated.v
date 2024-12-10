module json2

pub struct DecodeError {
	line    int
	column  int
	message string
}

// code returns the error code of DecodeError
@[deprecated_after: '2025-03-18']
pub fn (err DecodeError) code() int {
	return 3
}

// msg returns the message of the DecodeError
@[deprecated_after: '2025-03-18']
pub fn (err DecodeError) msg() string {
	return format_message(err.message, err.line, err.column)
}

pub struct InvalidTokenError {
	DecodeError
	token    Token
	expected TokenKind
}

// code returns the error code of the InvalidTokenError
@[deprecated_after: '2025-03-18']
pub fn (err InvalidTokenError) code() int {
	return 2
}

// msg returns the message of the InvalidTokenError
@[deprecated_after: '2025-03-18']
pub fn (err InvalidTokenError) msg() string {
	footer_text := if err.expected != .none { ', expecting `${err.expected}`' } else { '' }
	return format_message('invalid token `${err.token.kind}`${footer_text}', err.token.line,
		err.token.full_col())
}

pub struct UnknownTokenError {
	DecodeError
	token Token
	kind  ValueKind = .unknown
}

// code returns the error code of the UnknownTokenError
@[deprecated_after: '2025-03-18']
pub fn (err UnknownTokenError) code() int {
	return 1
}

// msg returns the error message of the UnknownTokenError
@[deprecated_after: '2025-03-18']
pub fn (err UnknownTokenError) msg() string {
	return format_message("unknown token '${err.token.lit}' when decoding ${err.kind}.",
		err.token.line, err.token.full_col())
}

struct Parser {
pub mut:
	scanner      &Scanner = unsafe { nil }
	prev_tok     Token
	tok          Token
	next_tok     Token
	n_level      int
	convert_type bool = true
}

// Decodes a JSON string into an `Any` type. Returns an option.
pub fn raw_decode(src string) !Any {
	mut p := new_parser(src, true)
	return p.decode()
}

// Same with `raw_decode`, but skips the type conversion for certain types when decoding a certain value.
@[deprecated: 'use `decode[json.Any]` instead']
@[deprecated_after: '2025-03-18']
pub fn fast_raw_decode(src string) !Any {
	mut p := new_parser(src, false)
	return p.decode()
}

// decode - decodes provided JSON
@[deprecated_after: '2025-03-18']
pub fn (mut p Parser) decode() !Any {
	p.next()
	p.next_with_err()!
	fi := p.decode_value()!
	if p.tok.kind != .eof {
		return InvalidTokenError{
			token: p.tok
		}
	}
	return fi
}

// decode_array is a generic function that decodes a JSON string into the array target type.
@[deprecated: 'use `decode` instead']
@[deprecated_after: '2025-03-18']
pub fn decode_array[T](src string) ![]T {
	res := raw_decode(src)!.as_map()
	return decode_struct_array(T{}, res)
}
