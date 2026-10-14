module json2

import math
import os
import strconv
import strings

struct Json5SourceSpan {
mut:
	position int
	length   int
}

// Json5Parser writes decoded strings and bare keys as JSON strings, while keeping
// number literals intact for custom decoders. It builds the same flat token list
// as the JSON checker, so both dialects use the existing typed decoder.
struct Json5Parser {
mut:
	scanner            &Json5Scanner
	tok                Json5Token
	output             Encoder
	values_info        []ValueInfo
	source_values_info []Json5SourceSpan
}

fn json5_syntax_error(message string, line int, col int) &JsonScanError {
	return &JsonScanError{
		message:   message
		line:      line
		character: col
	}
}

@[manualfree]
fn (mut p Json5Parser) advance() ! {
	next := p.scanner.next()!
	// Token literals are private scanner allocations; normalization has copied them.
	unsafe { p.tok.lit.free() }
	p.tok = next
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
	mut has_escape := false
	for i in start .. end {
		if p.output.output[i] == `\\` {
			has_escape = true
			break
		}
	}
	p.values_info << ValueInfo{
		position:   start
		length:     end - start
		value_kind: .string
		has_escape: has_escape
	}
	p.source_values_info << Json5SourceSpan{
		position: p.tok.pos.offset
		length:   p.scanner.offset - p.tok.pos.offset
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
			p.source_values_info << Json5SourceSpan{ position: p.tok.pos.offset }
			source_end := p.parse_container(is_object)!
			p.values_info[index].length = p.output.output.len - start
			p.source_values_info[index].length = source_end - p.source_values_info[index].position
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
			for ch in p.tok.lit {
				p.output.output << ch
			}
			p.values_info << ValueInfo{
				position:   start
				length:     p.tok.lit.len
				value_kind: kind
			}
			p.source_values_info << Json5SourceSpan{
				position: p.tok.pos.offset
				length:   p.scanner.offset - p.tok.pos.offset
			}
			p.advance()!
		}
		else {
			return p.unexpected('a value')
		}
	}
}

fn (mut p Json5Parser) parse_container(is_object bool) !int {
	closing := if is_object { Json5TokenKind.rcbr } else { Json5TokenKind.rsbr }
	p.output.output << if is_object { `{` } else { `[` }
	p.advance()!
	for p.tok.kind != closing {
		if is_object {
			match p.tok.kind {
				.str, .ident, .bool, .null, .infinity, .nan {
					// Signed literals share keyword token kinds but are not IdentifierNames.
					if p.tok.kind != .str
						&& (p.tok.lit.starts_with('+') || p.tok.lit.starts_with('-')) {
						return p.unexpected('an object key')
					}
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
	source_end := p.scanner.offset
	p.advance()!
	return source_end
}

@[manualfree]
fn decode_json5[T](text string, mut buffer DecodeBuffer, params DecoderOptions) !T {
	mut parser := Json5Parser{
		scanner:     new_json5_scanner(text)
		values_info: unsafe { buffer.values_info }
	}
	parser.values_info.clear()
	// Parsing keeps no array slices; growth can release each previous allocation.
	unsafe {
		parser.values_info.flags |= .noslices
		parser.source_values_info.flags |= .noslices
		parser.output.output.flags |= .noslices
	}
	defer {
		buffer.values_info = unsafe { parser.values_info }
		// Only cloned strings escape the decoder; all parser storage is scratch.
		unsafe {
			parser.tok.lit.free()
			parser.output.output.free()
			parser.source_values_info.free()
			parser.scanner.text.free()
			free(parser.scanner)
		}
	}
	parser.parse()!
	// JSON5 decoding clones text that escapes, so this document can always be freed.
	normalized := parser.output.output.bytestr()
	defer {
		unsafe { normalized.free() }
	}
	mut decoder := Decoder{
		json:               normalized
		json5:              true
		source_json:        text
		source_values_info: unsafe { parser.source_values_info }
		strict:             params.strict
		values_info:        unsafe { parser.values_info }
		values_len:         parser.values_info.len
	}
	return decoder.decode_root[T]()
}

// json5_line_terminator_length treats CRLF as one terminator.
fn json5_line_terminator_length(text string, position int) int {
	ch, width := utf8_rune_at(text, position)
	if !json5_is_line_terminator(int(ch)) {
		return 0
	}
	if int(ch) == r_cr && position + 1 < text.len && text[position + 1] == `\n` {
		return 2
	}
	return width
}

// json5_source_location returns the zero based line, display column and line start.
fn json5_source_location(text string, position int) (int, int, int) {
	mut line := 0
	mut column := 0
	mut line_start := 0
	// The scanner skips an initial BOM without advancing its column.
	mut i := if text.starts_with('\uFEFF') { 3 } else { 0 }
	for i < position {
		width := json5_line_terminator_length(text, i)
		if width > 0 {
			i += width
			line++
			column = 0
			line_start = i
		} else {
			ch, rune_width := utf8_rune_at(text, i)
			column += if int(ch) == r_tab { tab_width } else { 1 }
			i += rune_width
		}
	}
	return line, column, line_start
}

// json5_context_end limits a source span by display columns without splitting runes.
fn json5_context_end(text string, start int, end int, max_columns int) (int, int) {
	mut position := start
	mut columns := 0
	for position < end {
		ch, width := utf8_rune_at(text, position)
		display_width := if int(ch) == r_tab { tab_width } else { 1 }
		if json5_is_line_terminator(int(ch)) || columns + display_width > max_columns {
			break
		}
		position += width
		columns += display_width
	}
	return position, columns
}

@[manualfree]
fn json5_write_context(mut output strings.Builder, text string, start int, end int) {
	mut position := start
	for position < end {
		ch, width := utf8_rune_at(text, position)
		if int(ch) == r_tab {
			for _ in 0 .. tab_width {
				output.write_u8(` `)
			}
		} else {
			output.write_string(text.substr_unsafe(position, position + width))
		}
		position += width
	}
}

// json5_decode_context keeps the preview and underline in the same display columns.
@[manualfree]
fn json5_decode_context(text string, start int, length int, line_start int, column int) string {
	mut context_start := if line_start == 0 && text.starts_with('\uFEFF') { 3 } else { line_start }
	mut removed_columns := 0
	for column - removed_columns > max_context_length {
		ch, width := utf8_rune_at(text, context_start)
		removed_columns += if int(ch) == r_tab { tab_width } else { 1 }
		context_start += width
	}
	end, highlighted_columns := json5_context_end(text, start, start + length, max_context_length)
	context_end, _ := json5_context_end(text, end, text.len, max_extra_characters)
	cutoff := removed_columns > 0
	mut output := strings.new_builder(512)
	defer { unsafe { output.free() } }
	if cutoff {
		output.write_string('...')
	}
	json5_write_context(mut output, text, context_start, start)
	output.write_string('\\e[31m')
	json5_write_context(mut output, text, start, end)
	output.write_string('\\e[0m')
	json5_write_context(mut output, text, end, context_end)
	output.write_u8(`\n`)
	for _ in 0 .. column - removed_columns + if cutoff { 3 } else { 0 } {
		output.write_u8(` `)
	}
	output.write_string('\\e[31m')
	for _ in 0 .. highlighted_columns {
		output.write_u8(`~`)
	}
	output.write_string('\\e[0m')
	return output.str()
}

// normalize_json5_number converts hexadecimal integers and leading/trailing dots
// without going through a float, preserving the full range of 64-bit integers.
// Its result owns its storage and must be released after number conversion.
@[manualfree]
fn normalize_json5_number(text string) !string {
	negative := text.starts_with('-')
	digits := text.substr_unsafe(if text.starts_with('-') || text.starts_with('+') { 1 } else { 0 }, text.len)
	if digits.starts_with('0x') || digits.starts_with('0X') {
		value := strconv.parse_uint(digits.substr_unsafe(2, digits.len), 16, 64)!
		decimal := value.str()
		if negative {
			defer { unsafe { decimal.free() } }
			return '-' + decimal
		}
		return decimal
	}
	mut bytes := []u8{cap: text.len + 2}
	defer { unsafe { bytes.free() } }
	if negative {
		bytes << `-`
	}
	if digits.starts_with('.') {
		bytes << `0`
	}
	for i, ch in digits {
		bytes << ch
		if ch == `.` && (i + 1 == digits.len || digits[i + 1] in [`e`, `E`]) {
			bytes << `0`
		}
	}
	return bytes.bytestr()
}

// parse_json5_hex_float converts a scanner-validated hexadecimal integer without
// imposing the integer decoder's u64 limit or repeatedly rounding each digit.
fn parse_json5_hex_float(digits string) f64 {
	mut start := 2
	for start < digits.len && digits[start] == `0` {
		start++
	}
	if digits.len - start > 256 {
		return math.inf(1)
	}
	end := start + int_min(digits.len - start, 15)
	mut mantissa := u64(0)
	for i in start .. end {
		mantissa = (mantissa << 4) | u64(json5_hex_digit_value(int(digits[i])))
	}
	// Fifteen significant digits retain at least four bits beyond f64 precision.
	// Fold the remaining digits into a sticky bit so halfway cases round correctly.
	for i in end .. digits.len {
		if digits[i] != `0` {
			mantissa |= 1
			break
		}
	}
	return math.ldexp(f64(mantissa), 4 * (digits.len - end))
}

@[manualfree]
fn parse_json5_float(text string) !f64 {
	digits := text.substr_unsafe(if text.starts_with('-') || text.starts_with('+') { 1 } else { 0 }, text.len)
	if digits == 'Infinity' {
		return math.inf(if text.starts_with('-') { -1 } else { 1 })
	}
	if digits == 'NaN' {
		return math.nan()
	}
	if digits.starts_with('0x') || digits.starts_with('0X') {
		value := parse_json5_hex_float(digits)
		return if text.starts_with('-') { -value } else { value }
	}
	normalized := normalize_json5_number(text)!
	defer { unsafe { normalized.free() } }
	return strconv.atof64(normalized,
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
@[manualfree]
pub fn parse_file(path string, params DecoderOptions) !Doc {
	text := os.read_file(path)!
	defer { unsafe { text.free() } }
	return parse_text(text, params)
}

// decode_file reads and decodes a JSON or JSON5 document into T.
@[manualfree]
pub fn decode_file[T](path string, params DecoderOptions) !T {
	text := os.read_file(path)!
	defer { unsafe { text.free() } }
	return decode[T](text, params)
}

// decode_any converts a dynamic value into T using the existing JSON decoder.
@[manualfree]
pub fn decode_any[T](value Any, params DecoderOptions) !T {
	mut bytes := []u8{cap: 2048}
	// No slices escape this scratch buffer, so growth can release its old storage.
	unsafe { bytes.flags |= .noslices }
	defer { unsafe { bytes.free() } }
	encode_append(value, mut bytes, json5: params.json5)
	text := bytes.bytestr()
	defer { unsafe { text.free() } }
	return decode[T](text, params)
}
