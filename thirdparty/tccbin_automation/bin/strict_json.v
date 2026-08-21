module bin

import crypto.sha256
import encoding.utf8
import regex
import strings

pub const max_safe_integer = i64(9_007_199_254_740_991)

// JsonKind identifies the closed JSON value kinds accepted by the contract.
pub enum JsonKind {
	null_value
	boolean
	integer
	string_value
	array
	object
}

// JsonValue preserves object member order while rejecting duplicate keys.
pub struct JsonValue {
pub:
	kind          JsonKind
	bool_value    bool
	int_value     i64
	string_value  string
	array_value   []JsonValue
	object_keys   []string
	object_values []JsonValue
}

struct StrictJsonParser {
	source string
mut:
	pos int
}

// parse_strict_json parses the contract JSON profile without a permissive map decoder.
pub fn parse_strict_json(source string) !JsonValue {
	if source.len >= 3 && source[0] == 0xef && source[1] == 0xbb && source[2] == 0xbf {
		return error('UTF-8 BOM is not permitted')
	}
	if !utf8.validate_str(source) {
		return error('input is not valid UTF-8')
	}
	mut parser := StrictJsonParser{
		source: source
	}
	parser.skip_whitespace()
	value := parser.parse_value()!
	parser.skip_whitespace()
	if parser.pos != source.len {
		return error('trailing data at byte ${parser.pos}')
	}
	return value
}

fn (mut parser StrictJsonParser) parse_value() !JsonValue {
	if parser.pos >= parser.source.len {
		return error('unexpected end of JSON input')
	}
	return match parser.source[parser.pos] {
		`n` {
			parser.parse_literal('null', JsonValue{ kind: .null_value })!
		}
		`t` {
			parser.parse_literal('true', JsonValue{ kind: .boolean, bool_value: true })!
		}
		`f` {
			parser.parse_literal('false', JsonValue{ kind: .boolean })!
		}
		`"` {
			JsonValue{
				kind:         .string_value
				string_value: parser.parse_string()!
			}
		}
		`[` {
			parser.parse_array()!
		}
		`{` {
			parser.parse_object()!
		}
		`-`, `0`...`9` {
			parser.parse_integer()!
		}
		else {
			return error('unexpected JSON token at byte ${parser.pos}')
		}
	}
}

fn (mut parser StrictJsonParser) parse_literal(literal string, value JsonValue) !JsonValue {
	if parser.pos + literal.len > parser.source.len
		|| parser.source[parser.pos..parser.pos + literal.len] != literal {
		return error('invalid literal at byte ${parser.pos}')
	}
	parser.pos += literal.len
	return value
}

fn (mut parser StrictJsonParser) parse_array() !JsonValue {
	parser.pos++
	parser.skip_whitespace()
	mut values := []JsonValue{}
	if parser.consume(`]`) {
		return JsonValue{
			kind:        .array
			array_value: values
		}
	}
	for {
		values << parser.parse_value()!
		parser.skip_whitespace()
		if parser.consume(`]`) {
			break
		}
		if !parser.consume(`,`) {
			return error('expected comma or closing bracket at byte ${parser.pos}')
		}
		parser.skip_whitespace()
	}
	return JsonValue{
		kind:        .array
		array_value: values
	}
}

fn (mut parser StrictJsonParser) parse_object() !JsonValue {
	parser.pos++
	parser.skip_whitespace()
	mut keys := []string{}
	mut values := []JsonValue{}
	if parser.consume(`}`) {
		return JsonValue{
			kind:          .object
			object_keys:   keys
			object_values: values
		}
	}
	for {
		if parser.pos >= parser.source.len || parser.source[parser.pos] != `"` {
			return error('expected object key at byte ${parser.pos}')
		}
		key := parser.parse_string()!
		if key in keys {
			return error('duplicate object key at byte ${parser.pos}')
		}
		parser.skip_whitespace()
		if !parser.consume(`:`) {
			return error('expected colon after object key at byte ${parser.pos}')
		}
		parser.skip_whitespace()
		keys << key
		values << parser.parse_value()!
		parser.skip_whitespace()
		if parser.consume(`}`) {
			break
		}
		if !parser.consume(`,`) {
			return error('expected comma or closing brace at byte ${parser.pos}')
		}
		parser.skip_whitespace()
	}
	return JsonValue{
		kind:          .object
		object_keys:   keys
		object_values: values
	}
}

fn (mut parser StrictJsonParser) parse_integer() !JsonValue {
	start := parser.pos
	mut negative := false
	if parser.consume(`-`) {
		negative = true
		if parser.pos >= parser.source.len {
			return error('incomplete number at byte ${start}')
		}
	}
	if parser.source[parser.pos] == `0` {
		parser.pos++
		if negative {
			return error('lexical -0 is not permitted')
		}
		if parser.pos < parser.source.len && parser.source[parser.pos] >= `0`
			&& parser.source[parser.pos] <= `9` {
			return error('leading zero at byte ${start}')
		}
	} else {
		if parser.source[parser.pos] < `1` || parser.source[parser.pos] > `9` {
			return error('invalid integer at byte ${start}')
		}
		for parser.pos < parser.source.len && parser.source[parser.pos] >= `0`
			&& parser.source[parser.pos] <= `9` {
			parser.pos++
		}
	}
	if parser.pos < parser.source.len && parser.source[parser.pos] in [`.`, `e`, `E`] {
		return error('floating-point numbers are not permitted')
	}
	digits_start := if negative { start + 1 } else { start }
	mut magnitude := i64(0)
	for digit in parser.source[digits_start..parser.pos].bytes() {
		magnitude = magnitude * 10 + i64(digit - `0`)
		if magnitude > max_safe_integer {
			return error('integer is outside the interoperable safe range')
		}
	}
	return JsonValue{
		kind:      .integer
		int_value: if negative { -magnitude } else { magnitude }
	}
}

fn (mut parser StrictJsonParser) parse_string() !string {
	parser.pos++
	mut builder := strings.new_builder(32)
	for parser.pos < parser.source.len {
		byte := parser.source[parser.pos]
		if byte == `"` {
			parser.pos++
			return builder.str()
		}
		if byte < 0x20 {
			return error('unescaped control character at byte ${parser.pos}')
		}
		if byte != `\\` {
			builder.write_u8(byte)
			parser.pos++
			continue
		}
		parser.pos++
		if parser.pos >= parser.source.len {
			return error('unterminated escape sequence')
		}
		escape := parser.source[parser.pos]
		parser.pos++
		match escape {
			`"`, `\\`, `/` {
				builder.write_u8(escape)
			}
			`b` {
				builder.write_u8(8)
			}
			`f` {
				builder.write_u8(12)
			}
			`n` {
				builder.write_u8(10)
			}
			`r` {
				builder.write_u8(13)
			}
			`t` {
				builder.write_u8(9)
			}
			`u` {
				mut codepoint := parser.parse_hex_quad()!
				if codepoint >= 0xd800 && codepoint <= 0xdbff {
					if parser.pos + 2 > parser.source.len || parser.source[parser.pos] != `\\`
						|| parser.source[parser.pos + 1] != `u` {
						return error('isolated high surrogate in string escape')
					}
					parser.pos += 2
					low := parser.parse_hex_quad()!
					if low < 0xdc00 || low > 0xdfff {
						return error('invalid low surrogate in string escape')
					}
					codepoint = 0x10000 + (codepoint - 0xd800) * 0x400 + (low - 0xdc00)
				} else if codepoint >= 0xdc00 && codepoint <= 0xdfff {
					return error('isolated low surrogate in string escape')
				}
				builder.write_string(rune(codepoint).str())
			}
			else {
				return error('invalid string escape at byte ${parser.pos - 1}')
			}
		}
	}
	return error('unterminated JSON string')
}

fn (mut parser StrictJsonParser) parse_hex_quad() !int {
	if parser.pos + 4 > parser.source.len {
		return error('short Unicode escape')
	}
	mut value := 0
	for _ in 0 .. 4 {
		byte := parser.source[parser.pos]
		value <<= 4
		value += match byte {
			`0`...`9` { int(byte - `0`) }
			`a`...`f` { int(byte - `a`) + 10 }
			`A`...`F` { int(byte - `A`) + 10 }
			else { return error('invalid Unicode escape at byte ${parser.pos}') }
		}
		parser.pos++
	}
	return value
}

fn (mut parser StrictJsonParser) skip_whitespace() {
	for parser.pos < parser.source.len && parser.source[parser.pos] in [` `, `\t`, `\n`, `\r`] {
		parser.pos++
	}
}

fn (mut parser StrictJsonParser) consume(expected u8) bool {
	if parser.pos < parser.source.len && parser.source[parser.pos] == expected {
		parser.pos++
		return true
	}
	return false
}

// object_value returns an object member without converting the object to a map.
pub fn (value JsonValue) object_value(key string) ?JsonValue {
	if value.kind != .object {
		return none
	}
	for index, candidate in value.object_keys {
		if candidate == key {
			return value.object_values[index]
		}
	}
	return none
}

// has_object_key reports whether an object contains a named member.
pub fn (value JsonValue) has_object_key(key string) bool {
	return value.object_value(key) != none
}

// canonical_json returns the RFC 8785 representation of the supported JSON profile.
pub fn canonical_json(value JsonValue) string {
	return match value.kind {
		.null_value {
			'null'
		}
		.boolean {
			if value.bool_value {
				'true'
			} else {
				'false'
			}
		}
		.integer {
			value.int_value.str()
		}
		.string_value {
			canonical_string(value.string_value)
		}
		.array {
			'[' + value.array_value.map(canonical_json(it)).join(',') + ']'
		}
		.object {
			canonical_object(value)
		}
	}
}

fn canonical_object(value JsonValue) string {
	mut indices := []int{len: value.object_keys.len, init: index}
	for cursor in 1 .. indices.len {
		current := indices[cursor]
		mut insert_at := cursor
		for insert_at > 0
			&& utf16_compare(value.object_keys[current], value.object_keys[indices[insert_at - 1]]) < 0 {
			indices[insert_at] = indices[insert_at - 1]
			insert_at--
		}
		indices[insert_at] = current
	}
	mut parts := []string{cap: indices.len}
	for index in indices {
		parts << '${canonical_string(value.object_keys[index])}:${canonical_json(value.object_values[index])}'
	}
	return '{' + parts.join(',') + '}'
}

fn canonical_string(value string) string {
	mut builder := strings.new_builder(value.len + 2)
	builder.write_u8(`"`)
	for byte in value.bytes() {
		match byte {
			`"` {
				builder.write_string('\\"')
			}
			`\\` {
				builder.write_string('\\\\')
			}
			8 {
				builder.write_string('\\b')
			}
			9 {
				builder.write_string('\\t')
			}
			10 {
				builder.write_string('\\n')
			}
			12 {
				builder.write_string('\\f')
			}
			13 {
				builder.write_string('\\r')
			}
			else {
				if byte < 0x20 {
					builder.write_string('\\u00${byte:02x}')
				} else {
					builder.write_u8(byte)
				}
			}
		}
	}
	builder.write_u8(`"`)
	return builder.str()
}

fn utf16_compare(left string, right string) int {
	left_units := utf16_units(left)
	right_units := utf16_units(right)
	limit := if left_units.len < right_units.len { left_units.len } else { right_units.len }
	for index in 0 .. limit {
		if left_units[index] < right_units[index] {
			return -1
		}
		if left_units[index] > right_units[index] {
			return 1
		}
	}
	if left_units.len < right_units.len {
		return -1
	}
	if left_units.len > right_units.len {
		return 1
	}
	return 0
}

fn utf16_units(value string) []u16 {
	mut units := []u16{}
	for character in value.runes() {
		codepoint := u32(character)
		if codepoint <= 0xffff {
			units << u16(codepoint)
		} else {
			adjusted := codepoint - 0x10000
			units << u16(0xd800 + (adjusted >> 10))
			units << u16(0xdc00 + (adjusted & 0x3ff))
		}
	}
	return units
}

// json_equal compares JSON values structurally without map-order sensitivity.
pub fn json_equal(left JsonValue, right JsonValue) bool {
	if left.kind != right.kind {
		return false
	}
	return canonical_json(left) == canonical_json(right)
}

// json_sha256 returns the lowercase SHA-256 of the canonical JSON bytes.
pub fn json_sha256(value JsonValue) string {
	return sha256.sum256(canonical_json(value).bytes()).hex()
}

// matches_json_pattern matches a schema pattern against the entire candidate string.
pub fn matches_json_pattern(pattern string, candidate string) !bool {
	mut expression := regex.regex_opt(pattern)!
	return expression.matches_string(candidate)
}
