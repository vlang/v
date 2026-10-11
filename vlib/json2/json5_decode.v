module json2

import math
import os
import strconv

// Json5Parser writes decoded strings and bare keys as JSON strings, while keeping
// number literals intact for custom decoders. It builds the same flat token list
// as the JSON checker, so both dialects use the existing typed decoder.
struct Json5Parser {
mut:
	scanner     &Json5Scanner
	tok         Json5Token
	output      Encoder
	values_info []ValueInfo
}

fn json5_syntax_error(message string, line int, col int) &JsonScanError {
	return &JsonScanError{
		message:   message
		line:      line
		character: col
	}
}

fn (mut p Json5Parser) advance() ! {
	p.tok = p.scanner.next()!
}

fn (p &Json5Parser) unexpected(expected string) &JsonScanError {
	message := if p.tok.kind == .eof {
		'unexpected end of input, expected ${expected}'
	} else {
		'unexpected `${p.tok.lit}`, expected ${expected}'
	}
	return json5_syntax_error(message, p.tok.pos.line, p.tok.pos.col)
}

fn (mut p Json5Parser) parse() ! {
	p.advance()!
	p.parse_value()!
	if p.tok.kind != .eof {
		return p.unexpected('end of input')
	}
}

fn (mut p Json5Parser) write_string_value(text string) {
	start := p.output.output.len
	p.output.encode_string(text)
	end := p.output.output.len
	p.values_info << ValueInfo{
		position:   start
		length:     end - start
		value_kind: .string
		has_escape: p.output.output[start..end].contains(`\\`)
	}
}

fn (mut p Json5Parser) parse_value() ! {
	start := p.output.output.len
	index := p.values_info.len
	match p.tok.kind {
		.lcbr, .lsbr {
			is_object := p.tok.kind == .lcbr
			p.values_info << ValueInfo{
				position:   start
				value_kind: if is_object { .object } else { .array }
			}
			p.parse_container(is_object)!
			p.values_info[index].length = p.output.output.len - start
		}
		.str {
			p.write_string_value(p.tok.lit)
			p.advance()!
		}
		.number, .infinity, .nan, .bool, .null {
			kind := match p.tok.kind {
				.bool { ValueKind.boolean }
				.null { ValueKind.null }
				else { ValueKind.number }
			}
			p.output.output << p.tok.lit.bytes()
			p.values_info << ValueInfo{
				position:   start
				length:     p.tok.lit.len
				value_kind: kind
			}
			p.advance()!
		}
		else {
			return p.unexpected('a value')
		}
	}
}

fn (mut p Json5Parser) parse_container(is_object bool) ! {
	closing := if is_object { Json5TokenKind.rcbr } else { Json5TokenKind.rsbr }
	p.output.output << if is_object { `{` } else { `[` }
	p.advance()!
	for p.tok.kind != closing {
		if is_object {
			match p.tok.kind {
				.str, .ident, .number, .bool, .null, .infinity, .nan {
					p.write_string_value(p.tok.lit)
					p.advance()!
				}
				else { return p.unexpected('an object key') }
			}
			if p.tok.kind != .colon {
				return p.unexpected('`:` after the key')
			}
			p.output.output << `:`
			p.advance()!
		}
		p.parse_value()!
		if p.tok.kind == closing {
			break
		}
		if p.tok.kind != .comma {
			return p.unexpected(if is_object { '`,` or `}`' } else { '`,` or `]`' })
		}
		p.advance()!
		if p.tok.kind != closing {
			p.output.output << `,`
		}
	}
	p.output.output << if is_object { `}` } else { `]` }
	p.advance()!
}

@[manualfree]
fn decode_json5[T](text string, mut buffer DecodeBuffer, params DecoderOptions) !T {
	mut parser := Json5Parser{
		scanner:     new_json5_scanner(text)
		values_info: unsafe { buffer.values_info }
	}
	parser.values_info.clear()
	defer {
		buffer.values_info = unsafe { parser.values_info }
		// The parser owns the normalized output and its private rune scanner.
		unsafe {
			parser.output.output.free()
			parser.scanner.text.free()
			free(parser.scanner)
		}
	}
	parser.parse()!
	mut decoder := Decoder{
		json:        parser.output.output.bytestr()
		json5:       true
		strict:      params.strict
		values_info: unsafe { parser.values_info }
		values_len:  parser.values_info.len
	}
	return decoder.decode_root[T]()
}

// normalize_json5_number converts hexadecimal integers and leading/trailing dots
// without going through a float, preserving the full range of 64-bit integers.
fn normalize_json5_number(text string) !string {
	negative := text.starts_with('-')
	mut digits := text.trim_left('+-')
	if digits.starts_with('0x') || digits.starts_with('0X') {
		value := strconv.parse_uint(digits[2..], 16, 64)!
		return if negative { '-' + value.str() } else { value.str() }
	}
	if digits.starts_with('.') {
		digits = '0' + digits
	}
	exponent := digits.index_any('eE')
	mantissa_end := if exponent >= 0 { exponent } else { digits.len }
	if mantissa_end > 0 && digits[mantissa_end - 1] == `.` {
		digits = digits[..mantissa_end] + '0' + digits[mantissa_end..]
	}
	return if negative { '-' + digits } else { digits }
}

fn parse_json5_float(text string) !f64 {
	digits := text.trim_left('+-')
	if digits == 'Infinity' {
		return math.inf(if text.starts_with('-') { -1 } else { 1 })
	}
	if digits == 'NaN' {
		return math.nan()
	}
	return strconv.atof64(normalize_json5_number(text)!,
		allow_extra_chars: false
		allow_overflow:    true
	)
}

// parse parses JSON into a dynamic Any value. Set `json5: true` for JSON5 syntax.
pub fn parse(text string, params DecoderOptions) !Any {
	return decode[Any](text, params)
}

// parse_text parses a JSON or JSON5 document with path lookup.
pub fn parse_text(text string, params DecoderOptions) !Doc {
	return new_doc(parse(text, params)!, params)
}

// parse_file reads a JSON or JSON5 document with path lookup.
pub fn parse_file(path string, params DecoderOptions) !Doc {
	return parse_text(os.read_file(path)!, params)
}

// decode_file reads and decodes a JSON or JSON5 document into T.
pub fn decode_file[T](path string, params DecoderOptions) !T {
	return decode[T](os.read_file(path)!, params)
}

// decode_any converts a dynamic value into T using the existing JSON decoder.
// Decoded strings can borrow the encoded text, so autofree must retain it.
@[manualfree]
pub fn decode_any[T](value Any, params DecoderOptions) !T {
	return decode[T](encode(value, json5: params.json5), params)
}
