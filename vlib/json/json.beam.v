// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend JSON module
// On BEAM, V JSON operations map to Erlang's json module (OTP 27+)
// or fall back to a pure Erlang implementation
module json

// Any represents a dynamically typed JSON value
// On BEAM: represented as Erlang term (maps, lists, binaries, numbers, atoms)
pub type Any = Null
	| []Any
	| bool
	| f64
	| i64
	| int
	| map[string]Any
	| string

// Null represents JSON null value
// On BEAM: represented as atom 'null'
pub struct Null {}

// null is a constant null value for comparisons
pub const null = Null{}

// decode decodes a JSON string into a V type T
// On BEAM: Uses Erlang json:decode/1 (OTP 27+) or custom decoder
// Codegen: vbeam_json:decode(TypeMeta, JsonBinary)
pub fn decode[T](s string) !T {
	// Compiler implementation - the actual codegen generates Erlang code
	// that decodes JSON into the target V struct/type
	//
	// For BEAM, this becomes:
	//   case vbeam_json:decode(TypeInfo, Binary) of
	//       {ok, Value} -> Value;
	//       {error, Reason} -> error(Reason)
	//   end
	return error('json.decode: not implemented at runtime (compiler builtin)')
}

// decode tries to decode the provided JSON string, into a V structure.
// This is the two-argument form that takes type info and string.
// On BEAM: vbeam_json:decode(TypeMeta, Binary)
pub fn decode_with_type(typ voidptr, s string) !voidptr {
	// Compiler implementation - this signature matches what the compiler expects
	return error('json.decode: not implemented at runtime (compiler builtin)')
}

// encode serializes the provided V value as a JSON string
// On BEAM: Uses Erlang json:encode/1 (OTP 27+) or custom encoder
// Codegen: vbeam_json:encode(Value)
pub fn encode[T](val T) string {
	// Compiler implementation - generates Erlang code to encode V types to JSON
	//
	// For BEAM, this becomes:
	//   vbeam_json:encode(VValue)
	//
	// Which converts V structs (Erlang maps) to JSON strings
	return ''
}

// encode_pretty serializes the provided V value as a formatted JSON string
// On BEAM: vbeam_json:encode_pretty(Value)
pub fn encode_pretty[T](val T) string {
	// Compiler implementation - generates formatted JSON output
	return ''
}

// raw_decode decodes JSON string to a dynamic Any type
// Pure V implementation that parses JSON into the Any sum type
pub fn raw_decode(s string) !Any {
	if s.len == 0 {
		return error('json.raw_decode: empty input')
	}
	val, end_pos := parse_value(s, 0)!
	// Check for trailing non-whitespace
	final_pos := skip_whitespace(s, end_pos)
	if final_pos < s.len {
		return error('json.raw_decode: unexpected trailing content')
	}
	return val
}

// fast_raw_decode is like raw_decode but optimized for performance
pub fn fast_raw_decode(s string) !Any {
	return raw_decode(s)
}

// ============================================================================
// Internal decode helpers (used by compiler-generated code)
// ============================================================================

// decode_int extracts an int from a JSON value
// On BEAM: Erlang integer from decoded JSON
@[markused]
fn decode_int(root Any) int {
	if root is int {
		return root
	}
	if root is i64 {
		return int(root)
	}
	if root is f64 {
		return int(root)
	}
	if root is string {
		return root.int()
	}
	return 0
}

@[markused]
fn decode_i8(root Any) i8 {
	return i8(decode_int(root))
}

@[markused]
fn decode_i16(root Any) i16 {
	return i16(decode_int(root))
}

@[markused]
fn decode_i64(root Any) i64 {
	if root is i64 {
		return root
	}
	if root is int {
		return i64(root)
	}
	if root is f64 {
		return i64(root)
	}
	if root is string {
		return root.i64()
	}
	return 0
}

@[markused]
fn decode_u8(root Any) u8 {
	return u8(decode_int(root))
}

@[markused]
fn decode_u16(root Any) u16 {
	return u16(decode_int(root))
}

@[markused]
fn decode_u32(root Any) u32 {
	return u32(decode_int(root))
}

@[markused]
fn decode_u64(root Any) u64 {
	if root is i64 {
		return u64(root)
	}
	if root is int {
		return u64(root)
	}
	if root is f64 {
		return u64(root)
	}
	return 0
}

@[markused]
fn decode_f32(root Any) f32 {
	if root is f64 {
		return f32(root)
	}
	if root is int {
		return f32(root)
	}
	if root is i64 {
		return f32(root)
	}
	if root is string {
		return root.f32()
	}
	return 0.0
}

@[markused]
fn decode_f64(root Any) f64 {
	if root is f64 {
		return root
	}
	if root is int {
		return f64(root)
	}
	if root is i64 {
		return f64(root)
	}
	if root is string {
		return root.f64()
	}
	return 0.0
}

@[markused]
fn decode_string(root Any) string {
	if root is string {
		return root
	}
	return ''
}

@[markused]
fn decode_bool(root Any) bool {
	if root is bool {
		return root
	}
	if root is string {
		return root == 'true'
	}
	return false
}

// ============================================================================
// Any type methods (dynamic JSON access)
// ============================================================================

// str returns the string representation of an Any value
pub fn (a Any) str() string {
	match a {
		Null {
			return 'null'
		}
		bool {
			return if a { 'true' } else { 'false' }
		}
		int {
			// Convert int to string without string interpolation to avoid recursion
			mut val := a
			if val == 0 {
				return '0'
			}
			mut s := ''
			mut neg := false
			if val < 0 {
				neg = true
				val = -val
			}
			for val > 0 {
				s = '${u8(val % 10 + 48)}' + s
				val = val / 10
			}
			return if neg { '-' + s } else { s }
		}
		i64 {
			// Convert i64 to string without recursion
			mut val := a
			if val == 0 {
				return '0'
			}
			mut s := ''
			mut neg := false
			if val < 0 {
				neg = true
				val = -val
			}
			for val > 0 {
				s = '${u8(val % 10 + 48)}' + s
				val = val / 10
			}
			return if neg { '-' + s } else { s }
		}
		f64 {
			// Float-to-string: handle integer part and up to 6 decimal places
			return f64_to_str(a)
		}
		string {
			return a
		}
		[]Any {
			return encode_array(a)
		}
		map[string]Any {
			return encode_map(a)
		}
	}
}

// int returns the Any value as int
pub fn (a Any) int() int {
	return decode_int(a)
}

// i64 returns the Any value as i64
pub fn (a Any) i64() i64 {
	return decode_i64(a)
}

// f64 returns the Any value as f64
pub fn (a Any) f64() f64 {
	return decode_f64(a)
}

// bool returns the Any value as bool
pub fn (a Any) bool() bool {
	return decode_bool(a)
}

// as_array returns the Any value as array of Any
pub fn (a Any) as_array() []Any {
	if a is []Any {
		return a
	}
	return []
}

// as_map returns the Any value as map
pub fn (a Any) as_map() map[string]Any {
	if a is map[string]Any {
		return a
	}
	return {}
}

// ============================================================================
// JSON encoding helpers (pure V implementations for BEAM)
// ============================================================================

// f64_to_str converts a float64 to its string representation
// Handles integer values (prints without decimal point), and fractional with up to 6 digits
fn f64_to_str(val f64) string {
	if val == 0.0 {
		return '0.0'
	}
	mut neg := false
	mut v := val
	if v < 0 {
		neg = true
		v = -v
	}
	// Integer part
	int_part := i64(v)
	frac := v - f64(int_part)
	// Build integer part string
	mut int_str := ''
	if int_part == 0 {
		int_str = '0'
	} else {
		mut ip := int_part
		for ip > 0 {
			int_str = u8(ip % 10 + 48).ascii_str() + int_str
			ip = ip / 10
		}
	}
	// Build fractional part (up to 6 digits, trim trailing zeros)
	mut frac_str := ''
	mut f := frac
	mut digits := []u8{}
	for _ in 0 .. 6 {
		f = f * 10.0
		d := int(f) % 10
		digits << u8(d + 48)
		f = f - f64(int(f))
	}
	// Trim trailing zeros
	mut last_nonzero := -1
	for di := 0; di < digits.len; di++ {
		if digits[di] != u8(48) {
			last_nonzero = di
		}
	}
	if last_nonzero >= 0 {
		for di := 0; di <= last_nonzero; di++ {
			frac_str += digits[di].ascii_str()
		}
	} else {
		frac_str = '0'
	}
	mut result := int_str + '.' + frac_str
	if neg {
		result = '-' + result
	}
	return result
}

// escape_json_string escapes a string for use in JSON output
fn escape_json_string(s string) string {
	mut result := '"'
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `\\` {
			result += '\\\\'
		} else if c == `"` {
			result += '\\"'
		} else if c == `\n` {
			result += '\\n'
		} else if c == `\r` {
			result += '\\r'
		} else if c == `\t` {
			result += '\\t'
		} else if c < 0x20 {
			// Control characters - encode as \u00XX
			hex_chars := '0123456789abcdef'
			result += '\\u00'
			result += hex_chars[(c >> 4) & 0x0F].ascii_str()
			result += hex_chars[c & 0x0F].ascii_str()
		} else {
			result += c.ascii_str()
		}
	}
	result += '"'
	return result
}

// encode_any converts an Any value to its JSON string representation
fn encode_any(val Any) string {
	match val {
		Null {
			return 'null'
		}
		bool {
			return if val { 'true' } else { 'false' }
		}
		int {
			return Any(val).str()
		}
		i64 {
			return Any(val).str()
		}
		f64 {
			return f64_to_str(val)
		}
		string {
			return escape_json_string(val)
		}
		[]Any {
			return encode_array(val)
		}
		map[string]Any {
			return encode_map(val)
		}
	}
}

// encode_array converts an array of Any values to a JSON array string
fn encode_array(arr []Any) string {
	if arr.len == 0 {
		return '[]'
	}
	mut result := '['
	for i, item in arr {
		if i > 0 {
			result += ','
		}
		result += encode_any(item)
	}
	result += ']'
	return result
}

// encode_map converts a map of string->Any to a JSON object string
fn encode_map(m map[string]Any) string {
	keys := m.keys()
	if keys.len == 0 {
		return '{}'
	}
	mut result := '{'
	for i, key in keys {
		if i > 0 {
			result += ','
		}
		result += escape_json_string(key)
		result += ':'
		result += encode_any(m[key])
	}
	result += '}'
	return result
}

// encode_any_pretty converts an Any value to a formatted JSON string
fn encode_any_pretty(val Any, indent int) string {
	pad := '  '.repeat(indent)
	pad_inner := '  '.repeat(indent + 1)
	match val {
		Null {
			return 'null'
		}
		bool {
			return if val { 'true' } else { 'false' }
		}
		int {
			return Any(val).str()
		}
		i64 {
			return Any(val).str()
		}
		f64 {
			return f64_to_str(val)
		}
		string {
			return escape_json_string(val)
		}
		[]Any {
			if val.len == 0 {
				return '[]'
			}
			mut result := '[\n'
			for i, item in val {
				result += pad_inner + encode_any_pretty(item, indent + 1)
				if i < val.len - 1 {
					result += ','
				}
				result += '\n'
			}
			result += pad + ']'
			return result
		}
		map[string]Any {
			keys := val.keys()
			if keys.len == 0 {
				return '{}'
			}
			mut result := '{\n'
			for i, key in keys {
				result += pad_inner + escape_json_string(key) + ': ' + encode_any_pretty(val[key],
					indent + 1)
				if i < keys.len - 1 {
					result += ','
				}
				result += '\n'
			}
			result += pad + '}'
			return result
		}
	}
}

// ============================================================================
// JSON raw_decode parser (pure V implementation for BEAM)
// ============================================================================

// skip_whitespace advances the position past any whitespace characters
fn skip_whitespace(s string, start int) int {
	mut pos := start
	for pos < s.len {
		c := s[pos]
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			pos++
		} else {
			break
		}
	}
	return pos
}

// parse_value parses a JSON value starting at position pos
// Returns the parsed Any value and the position after the value
fn parse_value(s string, start int) !(Any, int) {
	pos := skip_whitespace(s, start)
	if pos >= s.len {
		return error('unexpected end of JSON input')
	}
	c := s[pos]
	if c == `"` {
		return parse_string(s, pos)
	} else if c == `{` {
		return parse_object(s, pos)
	} else if c == `[` {
		return parse_array_value(s, pos)
	} else if c == `t` {
		// true
		if pos + 4 <= s.len && s[pos + 1] == `r` && s[pos + 2] == `u` && s[pos + 3] == `e` {
			return Any(true), pos + 4
		}
		return error('invalid JSON: expected true')
	} else if c == `f` {
		// false
		if pos + 5 <= s.len && s[pos + 1] == `a` && s[pos + 2] == `l` && s[pos + 3] == `s`
			&& s[pos + 4] == `e` {
			return Any(false), pos + 5
		}
		return error('invalid JSON: expected false')
	} else if c == `n` {
		// null
		if pos + 4 <= s.len && s[pos + 1] == `u` && s[pos + 2] == `l` && s[pos + 3] == `l` {
			return Any(Null{}), pos + 4
		}
		return error('invalid JSON: expected null')
	} else if c == `-` || (c >= `0` && c <= `9`) {
		return parse_number(s, pos)
	}
	return error('invalid JSON: unexpected character')
}

// parse_string parses a JSON string starting at pos (which should be a '"')
fn parse_string(s string, start int) !(Any, int) {
	if s[start] != `"` {
		return error('expected string')
	}
	mut pos := start + 1
	mut result := ''
	for pos < s.len {
		c := s[pos]
		if c == `"` {
			return Any(result), pos + 1
		} else if c == `\\` {
			pos++
			if pos >= s.len {
				return error('unexpected end of string')
			}
			esc := s[pos]
			if esc == `"` {
				result += '"'
			} else if esc == `\\` {
				result += '\\'
			} else if esc == `/` {
				result += '/'
			} else if esc == `n` {
				result += '\n'
			} else if esc == `r` {
				result += '\r'
			} else if esc == `t` {
				result += '\t'
			} else if esc == `b` {
				result += u8(0x08).ascii_str()
			} else if esc == `f` {
				result += u8(0x0C).ascii_str()
			} else if esc == `u` {
				// Unicode escape \uXXXX - parse 4 hex digits
				if pos + 4 >= s.len {
					return error('unexpected end of unicode escape')
				}
				mut code_point := 0
				for hi := 1; hi <= 4; hi++ {
					h := s[pos + hi]
					if h >= `0` && h <= `9` {
						code_point = code_point * 16 + int(h - `0`)
					} else if h >= `a` && h <= `f` {
						code_point = code_point * 16 + int(h - `a`) + 10
					} else if h >= `A` && h <= `F` {
						code_point = code_point * 16 + int(h - `A`) + 10
					} else {
						return error('invalid hex digit in unicode escape')
					}
				}
				pos += 4
				// Encode code point as UTF-8
				if code_point < 0x80 {
					result += u8(code_point).ascii_str()
				} else if code_point < 0x800 {
					result += u8(0xC0 | (code_point >> 6)).ascii_str()
					result += u8(0x80 | (code_point & 0x3F)).ascii_str()
				} else {
					result += u8(0xE0 | (code_point >> 12)).ascii_str()
					result += u8(0x80 | ((code_point >> 6) & 0x3F)).ascii_str()
					result += u8(0x80 | (code_point & 0x3F)).ascii_str()
				}
			} else {
				result += esc.ascii_str()
			}
			pos++
		} else {
			result += c.ascii_str()
			pos++
		}
	}
	return error('unterminated string')
}

// parse_number parses a JSON number starting at pos
fn parse_number(s string, start int) !(Any, int) {
	mut pos := start
	mut is_float := false
	// Optional negative sign
	if pos < s.len && s[pos] == `-` {
		pos++
	}
	// Integer part
	if pos >= s.len {
		return error('unexpected end of number')
	}
	if s[pos] == `0` {
		pos++
	} else if s[pos] >= `1` && s[pos] <= `9` {
		for pos < s.len && s[pos] >= `0` && s[pos] <= `9` {
			pos++
		}
	} else {
		return error('invalid number')
	}
	// Fractional part
	if pos < s.len && s[pos] == `.` {
		is_float = true
		pos++
		if pos >= s.len || s[pos] < `0` || s[pos] > `9` {
			return error('invalid number: expected digit after decimal point')
		}
		for pos < s.len && s[pos] >= `0` && s[pos] <= `9` {
			pos++
		}
	}
	// Exponent
	if pos < s.len && (s[pos] == `e` || s[pos] == `E`) {
		is_float = true
		pos++
		if pos < s.len && (s[pos] == `+` || s[pos] == `-`) {
			pos++
		}
		if pos >= s.len || s[pos] < `0` || s[pos] > `9` {
			return error('invalid number: expected digit in exponent')
		}
		for pos < s.len && s[pos] >= `0` && s[pos] <= `9` {
			pos++
		}
	}
	num_str := s.substr(start, pos)
	if is_float {
		return Any(num_str.f64()), pos
	} else {
		return Any(num_str.i64()), pos
	}
}

// parse_array_value parses a JSON array starting at pos (which should be '[')
fn parse_array_value(s string, start int) !(Any, int) {
	mut pos := start + 1 // skip '['
	mut arr := []Any{}
	pos = skip_whitespace(s, pos)
	if pos < s.len && s[pos] == `]` {
		return Any(arr), pos + 1
	}
	for {
		val, next_pos := parse_value(s, pos)!
		arr << val
		pos = skip_whitespace(s, next_pos)
		if pos >= s.len {
			return error('unterminated array')
		}
		if s[pos] == `]` {
			return Any(arr), pos + 1
		}
		if s[pos] != `,` {
			return error('expected comma or ] in array')
		}
		pos++ // skip ','
	}
	return error('unterminated array')
}

// parse_object parses a JSON object starting at pos (which should be '{')
fn parse_object(s string, start int) !(Any, int) {
	mut pos := start + 1 // skip '{'
	mut obj := map[string]Any{}
	pos = skip_whitespace(s, pos)
	if pos < s.len && s[pos] == `}` {
		return Any(obj), pos + 1
	}
	for {
		pos = skip_whitespace(s, pos)
		if pos >= s.len || s[pos] != `"` {
			return error('expected string key in object')
		}
		key_any, key_end := parse_string(s, pos)!
		key := if key_any is string { key_any } else { '' }
		pos = skip_whitespace(s, key_end)
		if pos >= s.len || s[pos] != `:` {
			return error('expected colon after key in object')
		}
		pos++ // skip ':'
		val, val_end := parse_value(s, pos)!
		obj[key] = val
		pos = skip_whitespace(s, val_end)
		if pos >= s.len {
			return error('unterminated object')
		}
		if s[pos] == `}` {
			return Any(obj), pos + 1
		}
		if s[pos] != `,` {
			return error('expected comma or } in object')
		}
		pos++ // skip ','
	}
	return error('unterminated object')
}

// ============================================================================
// Null type methods
// ============================================================================

// str returns "null"
pub fn (n Null) str() string {
	return 'null'
}
